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

/**
  * These conventions specify the algorithm used to adjust a date in case
  * it is not a valid business day.
  */
sealed trait BusinessDayConvention {
  def adjust(date: LocalDate, calendar: Calendar) : LocalDate
}


object BusinessDayConvention {

  /**
    * Choose the first business day after the given holiday.
    *
    */
  case object Following extends BusinessDayConvention {
    override def adjust(date: LocalDate, calendar: Calendar): LocalDate = {
      var result = date

      // TODO: This method creates a lot of temporary LocalDate objects
      while (calendar.isHoliday(result))
        result = result.plusDays(1)

      result
    }
  }

  /**
    * Choose the first business day after the given holiday unless it belongs to a different month, in which case
    * choose the first business day before the holiday.
    */
  case object ModifiedFollowing extends BusinessDayConvention {
    override def adjust(date: LocalDate, calendar: Calendar): LocalDate = {
      var result = Following.adjust(date, calendar)
      if (result.getMonth != date.getMonth) {
        result = Preceding.adjust(date, calendar)
      }
      result
    }
  }

  /**
    * Choose the first business day before the given holiday.
    */
  case object Preceding extends BusinessDayConvention {
    override def adjust(date: LocalDate, calendar: Calendar): LocalDate = {
      var result = date

      // TODO: This method creates a lot of temporary LocalDate objects
      while (calendar.isHoliday(result))
        result = result.minusDays(1)

      result
    }
  }

  /**
    * Choose the first business day before the given holiday unless it belongs to a different month, in which case
    * choose the first business day after the holiday.
    */
  case object ModifiedPreceding extends BusinessDayConvention {
    override def adjust(date: LocalDate, calendar: Calendar): LocalDate = {
      var result = Preceding.adjust(date,calendar)
      if (result.getMonth != date.getMonth) {
        result = Following.adjust(date, calendar)
      }
      result
    }
  }

  /**
    * Do not adjust.
    */
  case object Unadjusted extends BusinessDayConvention {
    override def adjust(date: LocalDate, calendar: Calendar): LocalDate = date
  }

  /**
    * Choose the first business day after the given holiday unless that day crosses the mid-month (15th) or the
    * end of month, in which case choose the first business day before the holiday.
    */
  case object HalfMonthModifiedFollowing extends BusinessDayConvention {
    override def adjust(date: LocalDate, calendar: Calendar): LocalDate = {
      var result = ModifiedFollowing.adjust(date,calendar)
      if (date.getDayOfMonth <= 15 && result.getDayOfMonth > 15) {
        result = Preceding.adjust(date, calendar)
      }
      result
    }
  }

  /**
    * Choose the nearest business day to the given holiday. If both the preceding and following business days are
    * equally far away, default to following business day.
    */
  case object Nearest extends BusinessDayConvention {
    override def adjust(date: LocalDate, calendar: Calendar): LocalDate = {
      var d1 = date
      var d2 = date
      while (calendar.isHoliday(d1) && calendar.isHoliday(d2)) {
        d1 = d1.plusDays(1)
        d2 = d2.minusDays(1)
      }
      if (calendar.isHoliday(d1)) d2 else d1
    }
  }
}


