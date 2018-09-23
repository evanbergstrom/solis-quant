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

import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.time.DayCounter

object Actual360 {
  def apply(includeLastDay: Boolean = false): Actual360 =
    new Actual360(includeLastDay)
}

class Actual360(includeLastDay: Boolean) extends DayCounter {
  override def name: String = "Actual/360"

  override def dayCount(start: LocalDate, end: LocalDate): Long = {
    start.dayCountUntil(end) + (if (includeLastDay) 1 else  0)
  }

  override def yearFraction(start: LocalDate, end: LocalDate): Double =
    dayCount(start, end)/360.0

  override def yearFraction(start: LocalDate, end: LocalDate, refPeriodStart: LocalDate, refPeriodEnd: LocalDate): Double =
    yearFraction(start, end)
}
