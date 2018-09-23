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
package com.soliscode.finance.quant.time

import java.time.LocalDate

trait DayCounter {

  def name: String

  def dayCount(start: LocalDate, end: LocalDate) : Long =
    start.until(end).getDays

  def yearFraction(start: LocalDate, end: LocalDate) : Double = {
    yearFraction(start, end, start, end)
  }

//  def yearFraction(start: LocalDate, end: LocalDate, refStart: Option[LocalDate], refEnd: Option[LocalDate]) =
//    (refStart, refEnd) match {
//      case (Some(s),Some(e)) => yearFraction(start, end, s, e)
//      case _ => yearFraction(start, end)
//    }

  def yearFraction(start: LocalDate, end: LocalDate, refPeriodStart: LocalDate, refPeriodEnd: LocalDate) : Double
}
