/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a free-software/open-source library for financial quantitative analysts and developers (http://quantlib.org/) to Scala. The basic structure and design of the library has been preserved, but thebnaming conventions, basic types, collection classes and implementation have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.time.calendars

import java.time.LocalDate
import java.time.Month._

object Target extends WesternCalendar {
  override def name: String = "Target"

  override def isNormalBusinessDay(date: LocalDate): Boolean = {
    val w = date.getDayOfWeek
    val d = date.getDayOfMonth
    val dd = date.getDayOfYear
    val m = date.getMonth
    val y = date.getYear
    val em = easterMondayDayOfYear(y)

    !(isWeekend(w) || // New Year's Day
      ((d == 1) && (m eq JANUARY)) || // Good Friday
      ((dd == em - 3) && y >= 2000) || // Easter Monday
      ((dd == em) && y >= 2000) || // Labour Day
      ((d == 1) && (m == MAY) && y >= 2000) || // Christmas
      ((d == 25) && (m == DECEMBER)) || // Day of Goodwill
      ((d == 26) && (m == DECEMBER) && y >= 2000) || // December 31st, 1998, 1999, and 2001 only
      ((d == 31) && (m == DECEMBER) && ((y == 1998) || (y == 1999) || (y == 2001))))
  }
}
