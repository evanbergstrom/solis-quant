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

package com.soliscode.finance.quant.time.daycounters

import java.time.LocalDate

import com.soliscode.finance.quant.time.DayCounter

object SimpleDayCounter {
  val fallback : DayCounter = Thirty360(Thirty360.BondBasis)
}

class SimpleDayCounter extends DayCounter {

  override def name: String = "Simple"

  override def dayCount(start: LocalDate, end: LocalDate): Long = {
    SimpleDayCounter.fallback.dayCount(start,end)
  }

  override def yearFraction(start: LocalDate, end: LocalDate,
                            refPeriodStart: LocalDate, refPeriodEnd: LocalDate): Double = {
    val (startD, endD) = (start.getDayOfMonth, end.getDayOfMonth)

    if (startD == endD || (startD > endD && endD == end.lengthOfMonth()) ||
      (startD < endD && startD == start.lengthOfMonth())) {
      (end.getMonthValue - start.getMonthValue) / 12
    } else {
      SimpleDayCounter.fallback.yearFraction(start, end, refPeriodStart, refPeriodEnd)
    }
  }
}

