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

import java.time.temporal.{ChronoUnit, TemporalAdjusters, TemporalUnit}
import java.time.{DayOfWeek, LocalDate, Period}

import com.soliscode.finance.quant.time.BusinessDayConvention.{Following, Preceding}

import scala.collection.JavaConverters._
import scala.collection.mutable

object Calendar {
  def holidayList(cal: Calendar, from: LocalDate, to: LocalDate, includeWeekEnds: Boolean): List[LocalDate] = {
    from.datesUntil(to).iterator().asScala.filter(d => cal.isHoliday(d) && (includeWeekEnds || !cal.isWeekend(d))).toList
  }
}

abstract class Calendar {

  private val addedHolidays = mutable.Set[LocalDate]()
  private val removedHolidays = mutable.Set[LocalDate]()

  def name : String
  def isNormalBusinessDay(date: LocalDate) : Boolean
  def isWeekend(dayOfWeek: DayOfWeek) : Boolean

  def isWeekend(date: LocalDate): Boolean = {
    isWeekend(date.getDayOfWeek)
  }

  def addHoliday(date: LocalDate) : Unit = {
    // If the holiday was previously removed, revert the change
    removedHolidays -= date
    // If it's already a holiday, leave the calendar alone, otherwise add it
    if (isBusinessDay(date)) addedHolidays += date
  }

  def removeHoliday(date: LocalDate) : Unit = {
    // If the holiday was previously added, revert the change
    addedHolidays -= date
    // If it's already a business day, leave the calendar alone, otherwise, add it.
    if (!isBusinessDay(date)) removedHolidays += date
  }

  def advance(date: LocalDate, num: Long, unit: TemporalUnit, convention: BusinessDayConvention = Following,
              endOfMonth: Boolean = false) : LocalDate = {
    if (num == 0) {
      convention.adjust(date, this)
    } else {
      unit match {
        case ChronoUnit.DAYS => advanceBusinessDays(date, num)
        case ChronoUnit.WEEKS => convention.adjust(date.plusWeeks(num), this)
        case ChronoUnit.MONTHS =>
          val d = date.plusMonths(num)
          if (endOfMonth && isEndOfMonth(date)) this.endOfMonth(d) else convention.adjust(d, this)
        case ChronoUnit.YEARS =>
          val d = date.plusYears(num)
          if (endOfMonth && isEndOfMonth(date)) this.endOfMonth(d) else convention.adjust(d, this)
        case _ =>
          throw new IllegalArgumentException("Time unit " + unit + " not supported")
      }
    }
  }

  def advance(date: LocalDate, period: Period, convention: BusinessDayConvention, endOfMonth: Boolean): LocalDate = {

    var result = date
    period.getUnits.asScala.foreach(unit => result = advance(result, period.get(unit), unit, convention, endOfMonth))
    result
  }

  private def advanceBusinessDays(date: LocalDate, numberOfBusinessDays: Long): LocalDate = {
    var n = numberOfBusinessDays
    var result = date
    if (n > 0) {
      while (n > 0) {
        result = result.plusDays(1)
        while (isHoliday(result))
          result = result.plusDays(1)
        n -= 1
      }
    } else {
      while (n < 0) {
        result = result.minusDays(1)
        while (isHoliday(result))
          result = result.minusDays(1)
        n += 1
      }
    }
    result
  }

  def isHoliday(date: LocalDate) : Boolean = !isBusinessDay(date)

  def isBusinessDay(date: LocalDate) : Boolean =
    !addedHolidays.contains(date) && (removedHolidays.contains(date) || isNormalBusinessDay(date))

  def isEndOfMonth(date: LocalDate): Boolean =
    date.getMonth != Following.adjust(date.plusDays(1), this).getMonth

  def endOfMonth(date: LocalDate): LocalDate =
    Preceding.adjust(date.`with`(TemporalAdjusters.lastDayOfMonth()), this)
}

object NullCalendar extends Calendar {
  override def name: String = "None"
  override def isNormalBusinessDay(date: LocalDate): Boolean = true
  override def isWeekend(dayOfWeek: DayOfWeek): Boolean = false
}

