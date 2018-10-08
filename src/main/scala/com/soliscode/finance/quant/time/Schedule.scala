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

import java.time.{LocalDate, Period}

import com.soliscode.finance.quant.time.BusinessDayConvention.{Following, Unadjusted}
import com.soliscode.finance.quant.time.dategenerators.DateGeneration.{Backward, Forward}
import com.soliscode.finance.quant.time.calendars.UnitedStates
import com.soliscode.finance.quant.time.calendars.UnitedStates.GovernmentBond
import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.time.dategenerators.{DateGeneration, DateGenerationRule}

import scala.collection.Searching._

object Schedule {
  def apply(): Schedule =
    new Schedule(Seq[LocalDate](), None, UnitedStates.withMarket(GovernmentBond), Following,
      None, None, None, None)

  def apply(dates: Seq[LocalDate]): Schedule = {
    new Schedule(dates, None, UnitedStates.withMarket(GovernmentBond), Following,
      None, None, None, None)
  }

  def apply(dates: Seq[LocalDate],
            isRegular: Seq[Boolean],
            calendar: Calendar,
            convention: BusinessDayConvention): Schedule =
    new Schedule(dates, Some(isRegular), calendar, convention, None, None, None, None)


  def apply(effectiveDate: LocalDate,
            terminationDate: LocalDate,
            tenor: Period,
            calendar: Calendar,
            convention: BusinessDayConvention,
            terminationConvention: BusinessDayConvention,
            rule: DateGenerationRule,
            endOfMonth: Boolean,
            firstDateOption: Option[LocalDate] = None,
            nextToLastDateOption: Option[LocalDate] = None) : Schedule = {

    require(effectiveDate < terminationDate, s"effective date ($effectiveDate) less than termination nate ($terminationDate)")

    val firstDate = firstDateOption match {
      case Some(d) => if (d == effectiveDate) None else Some(d)
      case None => None
    }

    val nextToLast = nextToLastDateOption match {
      case Some(d) => if (d == terminationDate) None else Some(d)
      case None => None
    }

    val t = rule match {
      case DateGeneration.Zero => Period.ofYears(0)
      case _ =>
    }

    val eom = if (allowsEndOfMonth(tenor)) endOfMonth else false

    val r = if (tenor.getDays == 0)
      DateGeneration.Zero
    else {
      assert(tenor.getDays > 0, s"non positive tenor ($tenor) not allowed")
      rule
    }

    firstDate match {
      case Some(d) => rule.checkValidDate(d, effectiveDate, terminationDate)
      case None =>
    }

    nextToLast match {
      case Some(d) => rule.checkValidDate(d, effectiveDate, terminationDate)
      case None =>
    }

    null
  }

  def allowsEndOfMonth(tenor: Period): Boolean =
    tenor.toTotalMonths != 0 && tenor.getDays == 0 && tenor.getDays >= Period.ofMonths(1).getDays
}

class Schedule(val dates: Seq[LocalDate],
               val isRegular: Option[Seq[Boolean]],
               val calendar: Calendar,
               val convention: BusinessDayConvention,
               val terminationConvention: Option[BusinessDayConvention],
               val tenor: Option[Period],
               val rule: Option[DateGenerationRule],
               val endOfMonth: Option[Boolean]) extends Iterable[LocalDate] {


  def apply(n: Int): LocalDate = dates(n)

  override def size: Int = dates.size

  def startDate: LocalDate = dates.head
  def endDate: LocalDate = dates.last

  def previousDate(date: LocalDate): Option[LocalDate] = {
    dates.search(date) match {
      case Found(i) => if (i == 0) None else Some(dates(i-1))
      case InsertionPoint(i) => if (i == 0) None else Some(dates(i-1))
    }
  }

  def nextDate(date: LocalDate): Option[LocalDate] = {
    dates.search(date) match {
      case Found(i) => if (i == dates.size) None else Some(dates(i+1))
      case InsertionPoint(i) => if (i == dates.size) None else Some(dates(i))
    }
  }

  def hasTenor: Boolean = tenor.isDefined
  def hasIsRegular: Boolean = isRegular.isDefined

  override def iterator: Iterator[LocalDate] = dates.iterator
}

object ScheduleBuilder {
  def apply(): ScheduleBuilder = new ScheduleBuilder
}

class ScheduleBuilder {

  var effectiveDate: Option[LocalDate] = None
  var terminationDate: Option[LocalDate] = None
  var tenor: Option[Period] = None

  var calendarOption: Option[Calendar] = None
  var conventionOption: Option[BusinessDayConvention] = None
  var terminationConventionOption: Option[BusinessDayConvention] = None
  var firstDateOption: Option[LocalDate] = None
  var nextToLastDateOption: Option[LocalDate] = None

  var rule: DateGenerationRule = Backward
  var endOfMonth: Boolean = false

  def from(effectiveDate: LocalDate): ScheduleBuilder = {
    this.effectiveDate = Some(effectiveDate)
    this
  }

  def to(terminationDate: LocalDate): ScheduleBuilder = {
    this.terminationDate = Some(terminationDate)
    this
  }

  def withTenor(tenor: Period): ScheduleBuilder = {
    this.tenor = Some(tenor)
    this
  }

  def withFrequency(frequency: Frequency): ScheduleBuilder = {
    tenor = Some(frequency.toPeriod)
    this
  }

  def withCalendar(calendar: Calendar): ScheduleBuilder = {
    this.calendarOption = Some(calendar)
    this
  }

  def withConvention(convention: BusinessDayConvention): ScheduleBuilder = {
    this.conventionOption = Some(convention)
    this
  }

  def withTerminationDateConvention(convention: BusinessDayConvention): ScheduleBuilder = {
    terminationConventionOption = Some(convention)
    this
  }

  def withRule(rule: DateGenerationRule): ScheduleBuilder = {
    this.rule = rule
    this
  }

  def forwards(): ScheduleBuilder = {
    rule = Forward
    this
  }

  def backwards(): ScheduleBuilder = {
    rule = Backward
    this
  }

  def endOfMonth(flag: Boolean = true): ScheduleBuilder = {
    endOfMonth = flag
    this
  }

  def withFirstDate(date: LocalDate): ScheduleBuilder = {
    firstDateOption = Some(date)
    this
  }

  def withNextToLastDate(date: LocalDate): ScheduleBuilder = {
    nextToLastDateOption = Some(date)
    this
  }

  def build(): Schedule = {
    // check for mandatory arguments
    require(effectiveDate.isDefined, "effective date not provided")
    require(terminationDate.isDefined, "termination date not provided")
    require(tenor.isDefined, "tenor/frequency not provided")

    // set dynamic defaults:
    val convention: BusinessDayConvention = conventionOption match {
      case Some(c) => c
      case None if calendarOption.isDefined => Following
      case None => Unadjusted
    }

    val terminationConvention: BusinessDayConvention = terminationConventionOption match {
      case Some(c) => c
      case None => convention
    }

    val calendar: Calendar = calendarOption match {
      case Some(c) => c
      case None => NullCalendar
    }

    Schedule(effectiveDate.get, terminationDate.get, tenor.get, calendar, convention, terminationConvention, rule,
      endOfMonth, firstDateOption, nextToLastDateOption)
  }
}