/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a free-software/open-source library for financial quantitative analysts and developers (http://quantlib.org/) to Scala. The basic structure and design of the library has been preserved, but thebnaming conventions, basic types, collection classes and implementation have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.pricingengines

import com.soliscode.finance.quant.patterns.{Observable, Observer}

object PricingEngine {
  trait Arguments {
    def validate(): Unit
  }

  trait Results {
    def reset(): Unit
  }
}

trait PricingEngine extends Observable {

  def getArguments: PricingEngine.Arguments
  def getResults: PricingEngine.Results
  def reset(): Unit
  def calculate(): Unit
}

abstract class GenericEngine[A <: PricingEngine.Arguments, R <: PricingEngine.Results]
  extends PricingEngine with Observer {

  protected var arguments: A = _
  protected var results: R = _

  override def getArguments: PricingEngine.Arguments = arguments
  override def getResults: PricingEngine.Results = results
  override def reset(): Unit = results.reset()
  override def update(): Unit = notifyObservers()
}
