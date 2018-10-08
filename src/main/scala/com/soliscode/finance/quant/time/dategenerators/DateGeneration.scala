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

import com.soliscode.finance.quant.time.{BusinessDayConvention, Calendar}
import com.soliscode.finance.quant.time.Dates._


sealed trait DateGenerationRule {
  def apply(effectiveDate: LocalDate,
            terminationDate: LocalDate,
            tenor: Period,
            calendar: Calendar,
            convention: BusinessDayConvention,
            endOfMonth: Boolean,
            terminationConventionOption: Option[BusinessDayConvention],
            firstDateOption: Option[LocalDate],
            nextToLastDateOption: Option[LocalDate]): DateGenerator

  def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit
}

object DateGeneration {

  /** // No intermediate dates between effective date and termination date. */
  object Zero extends DateGenerationRule {
    def apply(effectiveDate: LocalDate, terminationDate: LocalDate, tenor: Period, calendar: Calendar,
              convention: BusinessDayConvention, endOfMonth: Boolean,
              terminationConventionOption: Option[BusinessDayConvention],
              firstDateOption: Option[LocalDate], nextToLastDateOption: Option[LocalDate]): DateGenerator =

      new ZeroDateGenerator(effectiveDate, terminationDate)

    def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit =
      if(date != effectiveDate && date != terminationDate) {
        throw new IllegalArgumentException(s"date ($date) out of effective-termination date range [$effectiveDate,$terminationDate]")
      }
  }

  /** Backward from termination date to effective date. */
  object Backward extends DateGenerationRule {
    def apply(effectiveDate: LocalDate, terminationDate: LocalDate, tenor: Period, calendar: Calendar,
              convention: BusinessDayConvention, endOfMonth: Boolean,
              terminationConventionOption: Option[BusinessDayConvention],
              firstDateOption: Option[LocalDate], nextToLastDateOption: Option[LocalDate]): DateGenerator =

      new BackwardDateGenerator(effectiveDate, terminationDate, tenor, calendar, convention, endOfMonth,
        terminationConventionOption, firstDateOption, nextToLastDateOption)


    def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit = {
      if (date <= effectiveDate || date >= terminationDate) {
        throw new IllegalArgumentException(s"date ($date) out of effective-termination date range [$effectiveDate,$terminationDate]")
      }
    }
  }

  /** // Forward from effective date to termination date. */
  object Forward extends DateGenerationRule {
    def apply(effectiveDate: LocalDate, terminationDate: LocalDate, tenor: Period, calendar: Calendar,
              convention: BusinessDayConvention, endOfMonth: Boolean,
              terminationConventionOption: Option[BusinessDayConvention],
              firstDateOption: Option[LocalDate], nextToLastDateOption: Option[LocalDate]): DateGenerator =

      new ForwardDateGenerator(effectiveDate, terminationDate, tenor, calendar, convention, endOfMonth,
        terminationConventionOption, firstDateOption, nextToLastDateOption)

    def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit = {
      if (date <= effectiveDate || date >= terminationDate) {
        throw new IllegalArgumentException(s"date ($date) out of effective-termination date range [$effectiveDate,$terminationDate]")
      }
    }
  }

/*
  /** All dates but effective date and termination date are taken to be on the third  wednesday */
  object ThirdWednesday extends DateGenerationRule {
    def apply(effectiveDate: LocalDate, terminationDate: LocalDate, calendar: Calendar,
              convention: BusinessDayConvention, endOfMonth: Boolean, firstDate: Option[LocalDate],
              nextToLastDate: Option[LocalDate], tenor: Period): DateGenerator = {
      require(!endOfMonth, s"endOfMonth convention incompatible with ${this.getClass.getSimpleName} date generation rule")
    }

    def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit = {
      //        if (!IMM::isIMMdate(firstDate_, false)) {
      //          throw new IllegalArgumentException(s"date ($date) is not an IMM date")
      //        }
    }
  }

  /** All dates but the effective date are taken to be the twentieth of their month (used for CDS schedules in
    * emerging markets.)  The termination date is also modified.  */
  object Twentieth extends DateGenerationRule {
    def apply(effectiveDate: LocalDate, terminationDate: LocalDate, calendar: Calendar,
              convention: BusinessDayConvention, endOfMonth: Boolean, firstDate: Option[LocalDate],
              nextToLastDate: Option[LocalDate], tenor: Period): DateGenerator = {
      require(!endOfMonth, s"endOfMonth convention incompatible with ${this.getClass.getSimpleName} date generation rule")
    }

    def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit = {
      throw new IllegalArgumentException(s"date incompatible with Twentieth date generation rule")
    }
  }

  object TwentiethIMM extends DateGenerationRule {
    def apply(effectiveDate: LocalDate, terminationDate: LocalDate, calendar: Calendar,
              convention: BusinessDayConvention, endOfMonth: Boolean, firstDate: Option[LocalDate],
              nextToLastDate: Option[LocalDate], tenor: Period): DateGenerator = {
      require(!endOfMonth, s"endOfMonth convention incompatible with TwentiethIMM date generation rule")

    }

    def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit = {
      throw new IllegalArgumentException(s"date incompatible with TwentiethIMM date generation rule")
    }
  }

  /** Same as TwentiethIMM with unrestricted date ends and log/short stub coupon period (old CDS convention). */
  object OldCDS extends DateGenerationRule {
    def apply(effectiveDate: LocalDate, terminationDate: LocalDate, calendar: Calendar,
              convention: BusinessDayConvention, endOfMonth: Boolean, firstDate: Option[LocalDate],
              nextToLastDate: Option[LocalDate], tenor: Period): DateGenerator = {
      require(!endOfMonth, s"endOfMonth convention incompatible with ${this.getClass.getSimpleName} date generation rule")

    }

    def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit = {
      throw new IllegalArgumentException(s"date incompatible with OldCDS date generation rule")
    }
  }

  /** Credit derivatives standard rule since 'Big Bang' changes in 2009. */
  object CDS extends DateGenerationRule {
    def apply(effectiveDate: LocalDate, terminationDate: LocalDate, calendar: Calendar,
              convention: BusinessDayConvention, endOfMonth: Boolean, firstDate: Option[LocalDate],
              nextToLastDate: Option[LocalDate], tenor: Period): DateGenerator = {
      require(!endOfMonth, s"endOfMonth convention incompatible with ${this.getClass.getSimpleName} date generation rule")
    }

    def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit = {
      throw new IllegalArgumentException(s"date incompatible with CDS date generation rule")
    }
  }

  /** Credit derivatives standard rule since December 20th, 2015. */
  object CDS2015 extends DateGenerationRule {
    def apply(effectiveDate: LocalDate, terminationDate: LocalDate, calendar: Calendar,
              convention: BusinessDayConvention, endOfMonth: Boolean, firstDate: Option[LocalDate],
              nextToLastDate: Option[LocalDate], tenor: Period): DateGenerator = {
      require(!endOfMonth, s"endOfMonth convention incompatible with ${this.getClass.getSimpleName} date generation rule")
    }

    def checkValidDate(date: LocalDate, effectiveDate: LocalDate, terminationDate: LocalDate): Unit = {
      throw new IllegalArgumentException(s"date incompatible with CDS2015 date generation rule")
    }
  }
  */
}

