/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a free-software/open-source library for financial quantitative analysts and developers (http://quantlib.org/) to Scala. The basic structure and design of the library has been preserved, but thebnaming conventions, basic types, collection classes and implementation have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant

import java.time.LocalDate

import com.soliscode.finance.quant.patterns.Observable

object EvaluationContext {
  def apply(): EvaluationContext =
    new MutableEvaluationContext(LocalDate.now(), false)

  def apply(valutationDate: LocalDate): EvaluationContext =
    new MutableEvaluationContext(valutationDate, false)

  def apply(valutationDate: LocalDate, includeReferenceDateEvents: Boolean): EvaluationContext =
    new MutableEvaluationContext(valutationDate, includeReferenceDateEvents)
}

class EvaluationContext(valDate: LocalDate, includeRefDateEvents: Boolean) extends Observable {

  var _valuationDate : LocalDate = valDate
  var _includeReferenceDateEvents : Boolean = includeRefDateEvents

  def valuationDate : LocalDate = _valuationDate
  def includeReferenceDateEvents : Boolean = _includeReferenceDateEvents
}

class MutableEvaluationContext(valuationDate: LocalDate, includeReferenceDateEvents: Boolean)
  extends EvaluationContext(valuationDate, includeReferenceDateEvents) {

  def valuationDate_(date: LocalDate): Unit = {
    _valuationDate = date
    notifyObservers()
  }

  def includeReferenceDateEvents_(include: Boolean): Unit = {
    _includeReferenceDateEvents = include
    notifyObservers()
  }
}

object PrivateEvaluationContext {
  def apply(): PrivateEvaluationContext = new PrivateEvaluationContext(LocalDate.now(), false)
}

class PrivateEvaluationContext(valuationDate: LocalDate, includeReferenceDateEvents: Boolean)
  extends MutableEvaluationContext(valuationDate, includeReferenceDateEvents) {
}
