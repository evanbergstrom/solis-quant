package com.soliscode.finance.quant.time.calendars

import java.time.LocalDate
import java.time.DayOfWeek._
import java.time.Month._

import com.soliscode.finance.quant.time.Calendar

object Australia {
  sealed trait Market
  case object Government extends Market

  def withMarket(market: Market) : Calendar =
    market match {
      case Government => new Australia
    }
}

class Australia extends WesternCalendar {

  override def name: String = "Australia"

  override def isNormalBusinessDay(date: LocalDate): Boolean = {
    val w = date.getDayOfWeek
    val d = date.getDayOfMonth
    val dd = date.getDayOfYear
    val m = date.getMonth
    val y = date.getYear
    val em = easterMondayDayOfYear(y)

    !(isWeekend(w) ||
      // New Year's Day (possibly moved to Monday)
      (((d == 1) || (w == MONDAY && (d == 2 || d == 3)) ) && (m == JANUARY)) ||
      // Australia Day, January 26th (possibly moved to Monday)
      (((d == 26) || (((d == 27) || (d == 28)) && (w == MONDAY))) && (m == JANUARY)) ||
      // Good Friday
      dd == em - 3 ||
      // Easter Monday
      dd == em ||
      // ANZAC Day, April 25th (possibly moved to Monday)
      (((d == 25) || ((d == 26) && (w == MONDAY))) && (m == APRIL)) ||
      // Queen's Birthday, second Monday in June
      ((d > 7 && d <= 14) && (w == MONDAY) && (m == JUNE)) ||
      // Bank Holiday, first Monday in August
      (d <= 7 && (w == MONDAY) && (m == AUGUST)) ||
      // Labour Day, first Monday in October
      (d <= 7 && (w == MONDAY) && (m == OCTOBER)) ||
      // Christmas, December 25th (possibly Monday or Tuesday)
      (((d == 25) || ((d == 27) && ((w == MONDAY) || (w == TUESDAY)))) && (m == DECEMBER)) ||
      // Boxing Day, December 26th (possibly Monday or Tuesday)
      (((d == 26) || ((d == 28) && ((w == MONDAY) || (w == TUESDAY)))) && (m == DECEMBER)))
  }
}
