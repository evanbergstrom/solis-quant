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
import com.soliscode.finance.quant.time.{Calendar, DayCounter}

abstract class TermStructure(val referenceDate: LocalDate,
                             val calendar: Calendar,
                             val dayCounter: DayCounter) extends Extrapolator {

  def maxDate: LocalDate
  def maxTime: Double
  def timeFromReference(date: LocalDate): Double
}
