/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib,
 * a free-software/open-source library for financial quantitative analysts and developers
 * (http://quantlib.org/) to Scala. The basic structure and design of the library has been
 * preserved, but thebnaming conventions, basic types, collection classes and implementation
 * have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.instruments

import java.time.LocalDate

import com.soliscode.finance.quant.EvaluationContext
import com.soliscode.finance.quant.patterns.LazyObject
import com.soliscode.finance.quant.pricingengines.PricingEngine

import scala.collection.mutable

object Instrument {

  trait Results extends PricingEngine.Results {
    var value: Option[Double]
    var valuationDate: Option[LocalDate]
    var errorEstimate: Option[Double]
    val additionalResults : mutable.Map[String,Any] = mutable.Map()

    def reset(): Unit = {
      value = None
      errorEstimate = None
      valuationDate = None
      additionalResults.clear()
  }
}

abstract class Instrument(val evaluationContext: EvaluationContext) extends LazyObject {

  private var npv: Option[Double] = None
  private var error: Option[Double] = None
  private var additionalResults: mutable.Map[String,Any] = mutable.HashMap()
  private var engine: Option[PricingEngine] = None

  def isExpired: Boolean

  def valuationDate: LocalDate = evaluationContext.valuationDate

  def setPricingEngine(engine: PricingEngine): Unit = {
    this.engine match {
      case Some(e) => unregisterWith(e)
      case None =>
    }
    this.engine = Some(engine)
    registerWith(engine)

    // trigger (lazy) recalculation and notify observers
    update()
  }

  def setupArguments(arguments: PricingEngine.Arguments): Unit = {
    throw new NotImplementedError("Instrument::setupArguments() not implemented")
  }

  override def calculate(): Unit = {
    if (isExpired) {
      setupExpired()
      calculated = true
    } else {
      super.calculate()
    }
  }

  def setupExpired(): Unit = {
    npv = Some(0.0)
    error = Some(0.0)
    additionalResults.clear()
  }

  def performCalculations(): Unit = {
    engine match {
      case Some(e) =>
        e.reset()
        setupArguments(e.getArguments)
        e.getArguments.validate()
        e.calculate()
        fetchResults(e.getResults)
      case None =>
        throw new IllegalStateException("null pricing engine")
    }
  }

  def fetchResults(results: PricingEngine.Results): Unit = {
    results match {
      case r: Instrument.Results =>
        npv = r.value
        error = r.errorEstimate
        additionalResults = r.additionalResults

      case _ =>
        throw new RuntimeException("no results returned from pricing engine")
    }
  }

  def netPresentValue(): Double = {
    calculate()
    assert(npv.isDefined, "NPV not provided")
    npv.get
  }

  def errorEstimate(): Double = {
    calculate()
    assert(error.isDefined, "error estimate not provided")
    error.get
  }
}}
