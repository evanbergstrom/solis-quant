/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 Copyright (C) 2018 Evan Bergstrom

 This file is provided under the BSD open software license. This is a port of QuantLib,
 a free-software/open-source library for financial quantitative analysts and developers
 (http://quantlib.org/) to Scala. The basic structure and design of the library has been
 preserved, but the naming conventions, types, collection classes and implementation
 have been modified to support common Scala idioms.

 See the full license in the license file (LICENSE.txt)
*/

package com.soliscode.finance.quant.termstructures.yields

import java.lang.Math._
import java.time.LocalDate

import com.soliscode.finance.quant.Quote
import com.soliscode.finance.quant.termstructures.YieldTermStructure
import com.soliscode.finance.quant.time.{Calendar, DayCounter}


abstract class ZeroYieldStructure(referenceDate: LocalDate, dayCounter: DayCounter, calendar: Calendar,
                                  jumps: List[Quote], jumpDates: List[LocalDate])
  extends YieldTermStructure(referenceDate, calendar, dayCounter, jumps, jumpDates) {

    protected def zeroYieldImpl(time: Double): Double

    def discountImpl(time: Double): Double =
      if (time == 0.0) 1.0 else exp(-zeroYieldImpl(time) * time)
}
