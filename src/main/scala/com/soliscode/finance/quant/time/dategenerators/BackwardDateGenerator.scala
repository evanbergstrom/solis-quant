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

import com.soliscode.finance.quant.time.{BusinessDayConvention, Calendar, NullCalendar}
import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.util.Collections._

import scala.collection.mutable

class BackwardDateGenerator(effectiveDate: LocalDate,
                            terminationDate: LocalDate,
                            tenor: Period,
                            calendar: Calendar,
                            convention: BusinessDayConvention,
                            endOfMonth: Boolean,
                            terminationConventionOption: Option[BusinessDayConvention],
                            firstDateOption: Option[LocalDate],
                            nextToLastDateOption: Option[LocalDate]) extends DateGenerator {

  override def generate(): (Seq[LocalDate], Seq[Boolean]) = {

    var exitDate = firstDateOption.getOrElse(effectiveDate)
    var terminationConvention = terminationConventionOption.getOrElse(convention)

    val dates = mutable.ArrayBuffer[LocalDate]()
    val regular = mutable.ArrayBuffer[Boolean]()

    dates += terminationDate

    var periods = 1
    var seed = terminationDate

    nextToLastDateOption match {
      case Some(d) =>
        dates += d
        val lastRegularCoupon = NullCalendar.advance(seed, tenor.multipliedBy(-periods), convention, endOfMonth)
        regular += (lastRegularCoupon == d)
        seed = d
      case None =>
    }

    var continue = true
    while(continue) {
      val temp = NullCalendar.advance(seed, tenor.multipliedBy(-periods), convention, endOfMonth)
      if (temp < exitDate) {
        if (firstDateOption.isDefined && !convention.equalWhenAdjusted(dates.last, firstDateOption.get, calendar)) {
          dates += firstDateOption.get
          regular += false
        }
        continue = false
      } else {
        // skip dates that would result in duplicates after adjustment
        if (!convention.equalWhenAdjusted(dates.last, temp, calendar)) {
          dates += temp
          regular += true
        }
        periods += 1
      }
    }

    if (!convention.equalWhenAdjusted(dates.last, effectiveDate, calendar)) {
      dates += effectiveDate
      regular += false
    }

    // Adjust dates for endOfMonth and business date where necessary
    adjustDates(dates, regular, seed, convention, terminationConvention, calendar, endOfMonth)

    // They were generated backward, so reverse them unto increasing temporal order
    dates.reverseInPlace()
    regular.reverseInPlace()

    // Final safety checks to remove extra next-to-last date, if necessary.  It can happen to be equal or later than
    // the end date due to EOM adjustments (see the Schedule test suite for an example).
    fixFirstAndLastDates(dates, regular)

    (dates,regular)
  }
}
