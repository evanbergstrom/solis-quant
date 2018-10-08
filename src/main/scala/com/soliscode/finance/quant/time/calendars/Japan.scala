/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib,
 * a free-software/open-source library for financial quantitative analysts and developers (http://quantlib.org/)
 * to Scala. The basic structure and design of the library has been preserved, but thebnaming conventions,
 * basic types, collection classes and implementation have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.time.calendars

import java.time.{DayOfWeek, LocalDate}
import java.time.Month._
import java.time.DayOfWeek._

import com.soliscode.finance.quant.time.Calendar

object Japan extends Calendar {
  override def name: String = "Japan"

  override def isNormalBusinessDay(date: LocalDate): Boolean = {
    val w = date.getDayOfWeek
    val d = date.getDayOfMonth
    val m = date.getMonth
    val y = date.getYear

    // equinox calculation
    val exact_vernal_equinox_time = 20.69115
    val exact_autumnal_equinox_time = 23.09
    val diff_per_year = 0.242194
    val moving_amount = (y-2000)*diff_per_year
    val number_of_leap_years = (y-2000)/4+(y-2000)/100-(y-2000)/400
    var ve = (exact_vernal_equinox_time + moving_amount - number_of_leap_years).toInt  // vernal equinox day
    var ae = (exact_autumnal_equinox_time + moving_amount - number_of_leap_years).toInt  // autumnal equinox day

    // checks
    !(isWeekend(w)
      // New Year's Day
      || (d == 1  && m == JANUARY)
      // Bank Holiday
      || (d == 2  && m == JANUARY)
      // Bank Holiday
      || (d == 3  && m == JANUARY)
      // Coming of Age Day (2nd Monday in January),
      // was January 15th until 2000
      || (w == MONDAY && (d >= 8 && d <= 14) && m == JANUARY
      && y >= 2000)
      || ((d == 15 || (d == 16 && w == MONDAY)) && m == JANUARY
      && y < 2000)
      // National Foundation Day
      || ((d == 11 || (d == 12 && w == MONDAY)) && m == FEBRUARY)
      // Vernal Equinox
      || ((d == ve || (d == ve+1 && w == MONDAY)) && m == MARCH)
      // Greenery Day
      || ((d == 29 || (d == 30 && w == MONDAY)) && m == APRIL)
      // Constitution Memorial Day
      || (d == 3  && m == MAY)
      // Holiday for a Nation
      || (d == 4  && m == MAY)
      // Children's Day
      || (d == 5  && m == MAY)
      // any of the three above observed later if on Saturday or Sunday
      || (d == 6 && m == MAY
      && (w == MONDAY || w == TUESDAY || w == WEDNESDAY))
      // Marine Day (3rd Monday in July),
      // was July 20th until 2003, not a holiday before 1996
      || (w == MONDAY && (d >= 15 && d <= 21) && m == JULY
      && y >= 2003)
      || ((d == 20 || (d == 21 && w == MONDAY)) && m == JULY
      && y >= 1996 && y < 2003)
      // Mountain Day (from 2016)
      || ((d == 11 || (d == 12 && w == MONDAY)) && m == AUGUST
      && y >= 2016)
      // Respect for the Aged Day (3rd Monday in September),
      // was September 15th until 2003
      || (w == MONDAY && (d >= 15 && d <= 21) && m == SEPTEMBER
      && y >= 2003)
      || ((d == 15 || (d == 16 && w == MONDAY)) && m == SEPTEMBER
      && y < 2003)
      // If a single day falls between Respect for the Aged Day
      // and the Autumnal Equinox, it is holiday
      || (w == TUESDAY && d+1 == ae && d >= 16 && d <= 22
      && m == SEPTEMBER && y >= 2003)
      // Autumnal Equinox
      || ((d == ae || (d == ae+1 && w == MONDAY)) && m == SEPTEMBER)
      // Health and Sports Day (2nd Monday in October),
      // was October 10th until 2000
      || (w == MONDAY && (d >= 8 && d <= 14) && m == OCTOBER
      && y >= 2000)
      || ((d == 10 || (d == 11 && w == MONDAY)) && m == OCTOBER
      && y < 2000)
      // National Culture Day
      || ((d == 3  || (d == 4 && w == MONDAY)) && m == NOVEMBER)
      // Labor Thanksgiving Day
      || ((d == 23 || (d == 24 && w == MONDAY)) && m == NOVEMBER)
      // Emperor's Birthday
      || ((d == 23 || (d == 24 && w == MONDAY)) && m == DECEMBER
      && y >= 1989)
      // Bank Holiday
      || (d == 31 && m == DECEMBER)
      // one-shot holidays
      // Marriage of Prince Akihito
      || (d == 10 && m == APRIL && y == 1959)
      // Rites of Imperial Funeral
      || (d == 24 && m == FEBRUARY && y == 1989)
      // Enthronement Ceremony
      || (d == 12 && m == NOVEMBER && y == 1990)
      // Marriage of Prince Naruhito
      || (d == 9 && m == JUNE && y == 1993))
  }

  override def isWeekend(dayOfWeek: DayOfWeek): Boolean =
    (dayOfWeek == SATURDAY) || (dayOfWeek == SUNDAY)
}
