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

package com.soliscode.finance.quant.time.calendars

import java.time.DayOfWeek._
import java.time.LocalDate
import java.time.Month._

import com.soliscode.finance.quant.time.Calendar

object Argentina {
  sealed trait Market
  case object Merval extends Market

  def withMarket(market: Market) : Calendar =
      market match {
        case Merval => new Argentina
      }
}

class Argentina extends WesternCalendar {

  override def name: String = "Buenos Aires stock exchange"

  override def isNormalBusinessDay(date: LocalDate): Boolean = {

    val w = date.getDayOfWeek
    val d = date.getDayOfMonth
    val dd = date.getDayOfYear
    val m = date.getMonth
    val y = date.getYear
    val em = easterMondayDayOfYear(y)

    !(isWeekend(w) ||
      // New Year's Day
      ((d == 1) && (m == JANUARY)) ||
      // Holy Thursday
      dd == em - 4 ||
      // Good Friday
      dd == em - 3 ||
      // Labour Day
      ((d == 1) && (m == MAY)) ||
      // May Revolution
      ((d == 25) && (m == MAY)) ||
      // Death of General Manuel Belgrano
      (d >= 15 && d <= 21 && (w == MONDAY) && (m == JUNE)) ||
      // Independence Day
      ((d == 9) && (m == JULY)) ||
      // Death of General Jose de San Marten
      (d >= 15 && d <= 21 && (w == MONDAY) && (m == AUGUST)) ||
      // Columbus Day
      (((d == 10) || (d == 11) || (d == 12) || (d == 15) || (d == 16)) && (w == MONDAY) && (m == OCTOBER)) ||
      // Immaculate Conception
      ((d == 8) && (m == DECEMBER)) ||
      // Christmas Eve
      ((d == 24) && (m == DECEMBER)) ||
      // New Year's Eve
      (((d == 31) || ((d == 30) && (w == FRIDAY))) && (m == DECEMBER)))
  }
}
