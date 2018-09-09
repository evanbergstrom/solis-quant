package com.soliscode.finance.quant.time.calendars

import java.time.{DayOfWeek, LocalDate, Month}
import java.time.DayOfWeek._
import java.time.Month._

import com.soliscode.finance.quant.time.Calendar

object UnitedStates {
  sealed trait Market
  case object Settlement extends Market
  case object GovernmentBond extends Market

  def withMarket(market: Market) : Calendar =
    market match {
      case Settlement => new USSettlement()
      case GovernmentBond => new USGovernmentBond()
    }

  private def isWeekend(dayOfWeek: DayOfWeek): Boolean =
    dayOfWeek == DayOfWeek.SATURDAY || dayOfWeek == DayOfWeek.SUNDAY

  def isNewYearsDayNoSunday(day: Int, month: Month, weekday: DayOfWeek) : Boolean = {
    // New Year's Day (possibly moved to Monday if on Sunday)
    (day == 1 || (day == 2 && weekday == MONDAY)) && month == JANUARY
  }

  def isNewYearsDay(day: Int, month: Month, weekday: DayOfWeek) : Boolean = {
    // New Year's Day (January 1st if on a weekday)
    ((day == 1  && !isWeekend(weekday)) ||
      // possibly moved to Monday if on a Sunday
      (day == 2 && weekday == MONDAY)) && month == JANUARY ||
      // possibly moved to Friday if on a Saturday
      (day == 31 && weekday == FRIDAY && month == DECEMBER)
  }

  def isMartinLutherKingDay(d: Int, m: Month, y: Int, w: DayOfWeek): Boolean =
  // Martin Luther King's birthday (third Monday in January)
    (d >= 15 && d <= 21) && w == MONDAY && m == JANUARY && y >= 1983

  def isWashingtonBirthday(day: Int, month: Month, year: Int, weekday: DayOfWeek) : Boolean = {
    if (year >= 1971) {
      // third Monday in February
      (day >= 15 && day <= 21) && weekday == MONDAY && month == FEBRUARY
    } else {
      // February 22nd, possily adjusted
      (day == 22 || (day == 23 && weekday == MONDAY) || (day == 21 && weekday == FRIDAY)) && month == FEBRUARY
    }
  }

  def isMemorialDay(day: Int, month: Month, year: Int, weekday: DayOfWeek): Boolean = {
    if (year >= 1971) {
      // last Monday in May
      day >= 25 && weekday == MONDAY && month == MAY
    } else {
      // May 30th, possibly adjusted
      (day == 30 || (day == 31 && weekday == MONDAY) || (day == 29 && weekday == FRIDAY)) && month == MAY
    }
  }

  def isIndependenceDay(d: Int, m: Month, w: DayOfWeek): Boolean =
    (d == 4 || (d == 5 && w == MONDAY) || (d == 3 && w == FRIDAY)) && m == JULY

  def isLaborDay(day: Int, month: Month, year: Int, weekday: DayOfWeek): Boolean = {
    // first Monday in September
    day <= 7 && weekday == MONDAY && month == SEPTEMBER
  }

  def isColumbusDay(day: Int, month: Month, year: Int, weekday: DayOfWeek): Boolean = {
    // second Monday in October
    (day >= 8 && day <= 14) && weekday == MONDAY && month == OCTOBER && year >= 1971
  }

  def isVeteransDay(day: Int, month: Month, year: Int, weekday: DayOfWeek): Boolean = {
    if (year <= 1970 || year >= 1978) {
      // November 11th, adjusted
      (day == 11 || (day == 12 && weekday == MONDAY) || (day == 10 && weekday == FRIDAY)) && month == NOVEMBER
    } else {
      // fourth Monday in October
      (day >= 22 && day <= 28) && weekday == MONDAY && month == OCTOBER
    }
  }

  def isVeteransDayNoSaturday(day: Int, month: Month, year: Int, weekday: DayOfWeek): Boolean = {
    if (year <= 1970 || year >= 1978) {
      // November 11th, adjusted, but no Saturday to Friday
      (day == 11 || (day == 12 && weekday == MONDAY)) && month == NOVEMBER
    } else {
      // fourth Monday in October
      (day >= 22 && day <= 28) && weekday == MONDAY && month == OCTOBER
    }
  }

  def isThanksgivingDay(d: Int, m: Month, w: DayOfWeek): Boolean =
    (d >= 22 && d <= 28) && w == THURSDAY && m == NOVEMBER

  def isChristmasDay(d: Int, m: Month, w: DayOfWeek): Boolean =
    (d == 25 || (d == 26 && w == MONDAY) || (d == 24 && w == FRIDAY)) && m == DECEMBER
}

abstract class UnitedStates(val name: String) extends WesternCalendar {
}

class USSettlement extends UnitedStates("US settlement") {
  override def isNormalBusinessDay(date: LocalDate): Boolean = {
    import com.soliscode.finance.quant.time.calendars.UnitedStates._

    val w: DayOfWeek = date.getDayOfWeek
    val d = date.getDayOfMonth
    val m = date.getMonth
    val y = date.getYear

    !(isWeekend(w)
      || isNewYearsDay(d,m,w)
      || isMartinLutherKingDay(d, m, y, w)
      || isWashingtonBirthday(d, m, y, w)
      || isMemorialDay(d, m, y, w)
      || isIndependenceDay(d,m,w)          // Independence Day (Monday if Sunday or Friday if Saturday)
      || isLaborDay(d, m, y, w)            // Labor Day (first Monday in September)
      || isColumbusDay(d, m, y, w)         // Columbus Day (second Monday in October)
      || isVeteransDay(d, m, y, w)         // Veteran's Day (Monday if Sunday or Friday if Saturday)
      || isThanksgivingDay(d, m, w)        // Thanksgiving Day (fourth Thursday in November)
      || isChristmasDay(d, m, w))
  }
}

class USGovernmentBond extends UnitedStates("US government bond market") {
  override def isNormalBusinessDay(date: LocalDate): Boolean = {
    import com.soliscode.finance.quant.time.calendars.UnitedStates._

    val w = date.getDayOfWeek
    val d = date.getDayOfMonth
    val dd = date.getDayOfYear
    val m = date.getMonth
    val y = date.getYear
    val em = easterMondayDayOfYear(y)

    !(isWeekend(w)
      || isNewYearsDayNoSunday(d, m, w)
      || isMartinLutherKingDay(d, m, y, w)
      || isWashingtonBirthday(d, m, y, w)
      || (dd == em-3) // Good Friday
      || isMemorialDay(d, m, y, w)
      || isIndependenceDay(d,m,w)
      || isLaborDay(d, m, y, w)
      || isColumbusDay(d, m, y, w)
      || isVeteransDayNoSaturday(d, m, y, w)
      || isThanksgivingDay(d, m, w)
      || isChristmasDay(d, m, w))
  }
}
