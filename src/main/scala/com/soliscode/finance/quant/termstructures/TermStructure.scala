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

package com.soliscode.finance.quant.termstructures

import java.time.LocalDate

import com.soliscode.finance.quant.math.interpolations.Extrapolator
import com.soliscode.finance.quant.patterns.{Observable, Observer}
import com.soliscode.finance.quant.time.{Calendar, DayCounter}
import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.math.Doubles._

abstract class TermStructure(val referenceDate: LocalDate, val calendar: Calendar, val dayCounter: DayCounter)
  extends Extrapolator with Observer with Observable {

  def maxDate: LocalDate

  def maxTime: Double = timeFromReference(maxDate)

  def timeFromReference(date: LocalDate): Double = dayCounter.yearFraction(referenceDate, date)

  def checkRange(date: LocalDate, extrapolate: Boolean): Unit = {
    assert(date >= referenceDate, s"date ($date) before reference date ($referenceDate)")
    assert(extrapolate || allowsExtrapolation || date <= maxDate, s"date ($date) is past max curve date ($maxDate)")
  }

  def checkRange(time: Double, extrapolate: Boolean): Unit = {
    assert(time >= 0.0, s"negative time ($time) given")
    assert(extrapolate || allowsExtrapolation || time <= maxTime || (time almost maxTime),
      s"time ($time) is past max curve time ($maxTime)")
  }

  def update(): Unit = {
    notifyObservers()
  }
}
