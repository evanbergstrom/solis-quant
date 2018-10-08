/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a
 * free-software/open-source library for financial quantitative analysts and developers
 * (http://quantlib.org/) to Scala. The basic structure and design of the library has been
 * preserved, but thebnaming conventions, basic types, collection classes and implementation
 * have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.time.dategenerators

import java.time.{LocalDate, Period}

import com.soliscode.finance.quant.time.BusinessDayConvention.Unadjusted
import com.soliscode.finance.quant.time.{BusinessDayConvention, Calendar}
import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.util.Collections._

import scala.collection.mutable.ArrayBuffer

trait DateGenerator {
  def generate(): (Seq[LocalDate],Seq[Boolean])

  def nextTwentieth(d: LocalDate): LocalDate = {

    var result = LocalDate.of(d.getYear, d.getMonth, 20)
    if (result < d)
      result.plusMonths(1)
/*
    if (rule == DateGeneration::TwentiethIMM || rule == DateGeneration::OldCDS || rule == DateGeneration::CDS ||
        rule == DateGeneration::CDS2015) {

      val m = result.getMonthValue
      if (m % 3 != 0) { // not a main IMM nmonth
        result.plusMonths(3 - m % 3)
      }
    }
    */
    result
  }

  def previousTwentieth(d: LocalDate): LocalDate = {
    val result = LocalDate.of(d.getYear, d.getMonth, 20)
    if (result > d)
      result.minusMonths(1)
/*
    if (rule == DateGeneration::TwentiethIMM || rule == DateGeneration::OldCDS || rule == DateGeneration::CDS ||
        rule == DateGeneration::CDS2015) {

      val m = result.getMonthValue
      if (m % 3 != 0) { // not a main IMM nmonth
        result.minusMonths(m % 3)
      }
    }
    */
    result
  }

  def adjustDates(dates: ArrayBuffer[LocalDate], regular: ArrayBuffer[Boolean], seed: LocalDate,
                  convention: BusinessDayConvention, terminationConvention: BusinessDayConvention,
                  calendar: Calendar, endOfMonth: Boolean): Unit = {

    if (endOfMonth && calendar.isEndOfMonth(seed)) {
      // Adjust to end of month
      dates.mapInPlace(0, dates.length-1, a => convention.adjustToEndOfMonth(a, calendar))
      if (terminationConvention != Unadjusted) {
        dates(dates.length - 1) = terminationConvention.adjustToEndOfMonth(dates.last, calendar)
      }
    } else {
      // Adjust based upon the calendar
      dates.mapInPlace(0, dates.length-1, a => convention.adjust(a, calendar))

      if (terminationConvention != Unadjusted) {
        dates(dates.length - 1) = terminationConvention.adjust(dates.last, calendar)
      }
    }
  }

  def fixFirstAndLastDates(dates: ArrayBuffer[LocalDate], regular: ArrayBuffer[Boolean]): Unit = {
    if (dates.length >= 2 && dates(dates.length - 2) >= dates.last) {
      regular.update(regular.length - 2, dates(dates.length - 2) == dates.last)
      dates.update(dates.length - 2, dates.last)
      dates.remove(dates.length-1)
      regular.remove(dates.length-1)
    }
    if (dates.length >= 2 && dates(1) <= dates(0)) {
      regular.update(1, dates(1) == dates(0))
      dates.update(1, dates(0))
      dates.remove(0)
      regular.remove(0)
    }
  }


}
