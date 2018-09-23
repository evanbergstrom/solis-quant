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

import java.time.LocalDate

import com.soliscode.finance.quant.termstructures.YieldTermStructure
import com.soliscode.finance.quant.time.{Frequency, NoFrequency}
import com.soliscode.finance.quant.{Compounding, Continuous, InterestRate, Quote}


class ZeroSpreadedTermStructure(curve: YieldTermStructure, spread: Quote, compounding: Compounding, frequency: Frequency)
  extends ZeroYieldStructure(curve.referenceDate, curve.dayCounter, curve.calendar, curve.jumps, curve.jumpDates) {

  override protected def zeroYieldImpl(time: Double): Double = {
    val zeroRate: InterestRate = curve.zeroRate(time, compounding, frequency, extrapolate = true)
    val spreadedRate = InterestRate(zeroRate.rate + spread.value, zeroRate.dayCounter, zeroRate.compounding,
                                    zeroRate.frequency)
    spreadedRate.equivalentRate(Continuous, NoFrequency, time).rate
  }

  override def maxDate: LocalDate = curve.maxDate

  override def maxTime: Double = curve.maxTime
}
