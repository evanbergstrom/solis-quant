/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a free-software/open-source library for financial quantitative analysts and developers (http://quantlib.org/) to Scala. The basic structure and design of the library has been preserved, but thebnaming conventions, basic types, collection classes and implementation have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.pricingengines.bond

import java.time.LocalDate

import com.soliscode.finance.quant.cashflows.CashFlow.Leg
import com.soliscode.finance.quant.instruments.Instrument
import com.soliscode.finance.quant.pricingengines.{GenericEngine, PricingEngine}
import com.soliscode.finance.quant.time.Calendar

object BondEngine {
  trait Arguments extends PricingEngine.Arguments {
    var settlementDate: LocalDate
    var cashflows: Leg
    var calendar: Calendar

    def validate(): Unit
  }

  trait Results extends Instrument.Results {
    var settlementValue: Option[Double]
    override def reset(): Unit = {
      settlementValue = None
      super.reset()
    }
  }
}

abstract class BondEngine extends GenericEngine[BondEngine.Arguments, BondEngine.Results]
