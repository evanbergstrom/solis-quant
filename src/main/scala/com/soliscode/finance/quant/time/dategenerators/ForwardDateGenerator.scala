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

import scala.collection.mutable

class ForwardDateGenerator(effectiveDate: LocalDate,
                           terminationDate: LocalDate,
                           tenor: Period,
                           calendar: Calendar,
                           convention: BusinessDayConvention,
                           endOfMonth: Boolean,
                           terminationConventionOption: Option[BusinessDayConvention],
                           firstDate: Option[LocalDate],
                           nextToLastDate: Option[LocalDate])
  extends DateGenerator {

  override def generate(): (Seq[LocalDate], Seq[Boolean]) = {

    val terminationConvention = terminationConventionOption.getOrElse(convention)

    // The exit date is the termination date unless a next=-to-last date is specified
    val exitDate = nextToLastDate.getOrElse(terminationDate)


    val dates = mutable.ArrayBuffer[LocalDate]()
    val regular = mutable.ArrayBuffer[Boolean]()

    dates += effectiveDate

    var seed = effectiveDate
    var periods = 1

    // Check for optional, possibly irregular first date
    firstDate match {
      case Some(d) =>
        dates += d
        val firstRegularDate = NullCalendar.advance(seed, tenor.multipliedBy(periods), convention, endOfMonth)
        regular += (d == firstRegularDate)  // check if the first date is regular
        seed = d
      case None =>
    }


    var continue = true
    while (continue) {
      val nextDate = NullCalendar.advance(seed, tenor.multipliedBy(periods), convention, endOfMonth)
      if (nextDate > exitDate) {

        if (nextToLastDate.isDefined && (dates.last equal nextToLastDate.get using(convention, calendar))) {
          dates += nextToLastDate.get
          regular += false
        }
        continue = false
      } else {
        // skip dates that would result in duplicates after adjustment
        if (!convention.equalWhenAdjusted(dates.last, nextDate, calendar)) {
          dates += nextDate
          regular += true
        }
        periods += 1
      }
    }

    if (dates.last notEqual terminationDate using(convention, calendar)) {
        dates += terminationDate
        regular += false
    }

    adjustDates(dates, regular, seed, convention, terminationConvention, calendar, endOfMonth)
    fixFirstAndLastDates(dates, regular)

    (dates,regular)
  }
}
