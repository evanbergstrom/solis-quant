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

import com.soliscode.finance.quant.cashflows.CashFlows
import com.soliscode.finance.quant.termstructures.YieldTermStructure

class DiscountingBondEngine(discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean)
  extends BondEngine {

  registerWith(discountCurve)

  override def calculate(): Unit = {

    results.valuationDate = Some(discountCurve.referenceDate)

    val includeRefDateFlows = includeSettlementDateFlows
    val npv = CashFlows.netPresentValue(arguments.cashflows, discountCurve, includeRefDateFlows,
       results.valuationDate.get, results.valuationDate.get)
    results.value = Some(npv)

    // a bond's cashflow on settlement date is never taken into
    // account, so we might have to play it safe and recalculate
    if (!includeRefDateFlows && results.valuationDate.contains(arguments.settlementDate)) {
      // same parameters as above, we can avoid another call
      results.settlementValue = results.value
    } else {
      // no such luck
      val npv = CashFlows.netPresentValue(arguments.cashflows, discountCurve, includeSettlementDateFlows = false,
         arguments.settlementDate, arguments.settlementDate)
      results.settlementValue = Some(npv)
    }
  }
}
