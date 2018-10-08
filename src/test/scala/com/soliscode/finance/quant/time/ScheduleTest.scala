/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a free-software/open-source library for financial quantitative analysts and developers (http://quantlib.org/) to Scala. The basic structure and design of the library has been preserved, but thebnaming conventions, basic types, collection classes and implementation have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.time

import java.time.{LocalDate, Month, Period}
import java.time.Month._

import com.soliscode.finance.quant.time.BusinessDayConvention._
import com.soliscode.finance.quant.time.calendars.{Japan, Target, UnitedStates, WeekendsOnly}
import Dates._
import com.soliscode.finance.quant.time.dategenerators.DateGeneration
import org.scalatest.FlatSpec

class ScheduleTest extends FlatSpec {

  "ScheduleBuilder" should "create a daily schedule" in {

    val startDate = LocalDate.of(17, Month.JANUARY, 2012)

    val s = ScheduleBuilder().from(startDate)
                             .to(startDate.plusDays(7))
                             .withCalendar(Target)
                             .withFrequency(Daily)
                             .withConvention(Preceding)
                             .build()

    assert(s.size == 6)
    assert(s(0) == LocalDate.of(2012, JANUARY, 17))
    assert(s(1) == LocalDate.of(2012, JANUARY, 18))
    assert(s(2) == LocalDate.of(2012, JANUARY, 19))
    assert(s(3) == LocalDate.of(2012, JANUARY, 20))
    assert(s(4) == LocalDate.of(2012, JANUARY, 23))
    assert(s(5) == LocalDate.of(2012, JANUARY, 24))
  }

  it should "create a schedule with a termination convention and EOM edjustment" in {

    var s = ScheduleBuilder().from(LocalDate.of(2009,SEPTEMBER, 30)).to(LocalDate.of(2012,JUNE,15))
                             .withCalendar(Japan)
                             .withTenor(Period.ofMonths(6))
                             .withConvention(Following)
                             .withTerminationDateConvention(Following)
                             .forwards()
                             .endOfMonth()
                             .build()

    assert(s.size == 7)

    // The end date is adjusted, so it should also be moved to the end of the month.
    assert(s(0) == LocalDate.of(2009, SEPTEMBER, 30))
    assert(s(1) == LocalDate.of(2010, MARCH, 31))
    assert(s(2) == LocalDate.of(2010, SEPTEMBER, 30))
    assert(s(3) == LocalDate.of(2011, MARCH, 31))
    assert(s(4) == LocalDate.of(2011, SEPTEMBER, 30))
    assert(s(5) == LocalDate.of(2012, MARCH, 30))
    assert(s(6) == LocalDate.of(2012, JUNE, 29))

    // now with unadjusted termination date...
    s = ScheduleBuilder().from(LocalDate.of(2009, SEPTEMBER, 30)).to(LocalDate.of(2112, JUNE, 15))
                         .withCalendar(Japan)
                         .withTenor(Period.ofMonths(6))
                         .withConvention(Following)
                         .withTerminationDateConvention(Unadjusted)
                         .forwards()
                         .endOfMonth()
                         .build()

    // ...which should leave it alone.
    assert(s(0) == LocalDate.of(2009, SEPTEMBER, 30))
    assert(s(1) == LocalDate.of(2010, MARCH, 31))
    assert(s(2) == LocalDate.of(2010, SEPTEMBER, 30))
    assert(s(3) == LocalDate.of(2011, MARCH, 31))
    assert(s(4) == LocalDate.of(2011, SEPTEMBER, 30))
    assert(s(5) == LocalDate.of(2012, MARCH, 30))
    assert(s(6) == LocalDate.of(2012, JUNE, 15))
  }

  it should "build no dates past the end date (even with EOM adjustment)" in {
    val s = ScheduleBuilder().from(LocalDate.of(2013,MARCH,28)).to(LocalDate.of(2015,MARCH,30))
                             .withCalendar(Target)
                             .withTenor(Period.ofYears(1))
                             .withConvention(Unadjusted)
                             .withTerminationDateConvention(Unadjusted)
                             .forwards()
                             .endOfMonth()
                             .build()

    assert(s(0) == LocalDate.of(2013, MARCH, 31))
    assert(s(1) == LocalDate.of(2014, MARCH, 31))

    // March 31st 2015, coming from the EOM adjustment of March 28th, should be discarded as past the end date.
    assert(s(2) == LocalDate.of(2015, MARCH, 30))

    // also, the last period should not be regular.
    assert(!s.isRegular.get.apply(2))
  }

  it should "remove next-to-last date that is the same as the end date" in {

    val s = ScheduleBuilder().from(LocalDate.of(2013, MARCH, 28)).to(LocalDate.of(2015,MARCH,31))
                             .withCalendar(Target)
                             .withTenor(Period.ofYears(1))
                             .withConvention(Unadjusted)
                             .withTerminationDateConvention(Unadjusted)
                             .forwards()
                             .endOfMonth()
                             .build()

    assert(s(0) == LocalDate.of(2013, MARCH, 31))
    assert(s(1) == LocalDate.of(2014, MARCH, 31))

    // March 31st 2015, coming from the EOM adjustment of March 28th, should be discarded as the same as the end date.
    assert(s(2) == LocalDate.of(2015, MARCH, 31))

    // also, the last period should be regular.
    assert(s.isRegular.get.apply(2))
  }

  it should "not adjust the last date for EOM when termination date is unadjusted" in {

    val s = ScheduleBuilder().from(LocalDate.of(1996, AUGUST, 31)).to(LocalDate.of(1997, SEPTEMBER, 15))
                             .withCalendar(UnitedStates.withMarket(UnitedStates.GovernmentBond))
                             .withTenor(Period.ofMonths(6))
                             .withConvention(Unadjusted)
                             .withTerminationDateConvention(Unadjusted)
                             .forwards()
                             .endOfMonth()
                             .build()

    assert(s.size == 4)
    assert(s(0) == LocalDate.of(196, AUGUST, 31))
    assert(s(1) == LocalDate.of(1997, FEBRUARY, 28))
    assert(s(2) == LocalDate.of(1997, AUGUST, 31))
    assert(s(3) == LocalDate.of(1997, SEPTEMBER, 15))
  }

  it should "not adjust the first date for EOM going backwards when termination date is unadjusted" in {

    val s = ScheduleBuilder().from(LocalDate.of(1996, AUGUST, 22)).to(LocalDate.of(1997, AUGUST, 31))
        .withCalendar(UnitedStates.withMarket(UnitedStates.GovernmentBond))
        .withTenor(Period.ofMonths(6))
        .withConvention(Unadjusted)
        .withTerminationDateConvention(Unadjusted)
        .backwards()
        .endOfMonth()
        .build()

    assert(s.size == 4)
    s(0) == LocalDate.of(1996, AUGUST, 22)
    s(1) == LocalDate.of(1996, AUGUST, 31)
    s(2) == LocalDate.of(1997, FEBRUARY, 28)
    s(3) == LocalDate.of(1997, AUGUST, 31)
  }

  it should "not duplicate the first date due to EOM convention when going backwards" in {

    val s = ScheduleBuilder().from(LocalDate.of(1996, AUGUST, 22)).to(LocalDate.of(1997, AUGUST, 31))
        .withCalendar(UnitedStates.withMarket(UnitedStates.GovernmentBond))
        .withTenor(Period.ofMonths(6))
        .withConvention(Following)
        .withTerminationDateConvention(Following)
        .backwards()
        .endOfMonth()
        .build()

    assert(s.size == 3)
    s(0) == LocalDate.of(1996, AUGUST, 30)
    s(1) == LocalDate.of(1997, FEBRUARY, 28)
    s(2) == LocalDate.of(1997, AUGUST, 29)
  }

  /*
it should "support CDS2015 semi-annual rolling convention" in {

  //From September 20th 2016 to March 19th 2017 of the next Year, end date is December 20th 2021 for a 5 year Swap
  val s1 = ScheduleBuilder().from(LocalDate.of(2016,DECEMBER,12))
      .to(LocalDate.of(2016,DECEMBER,12).plus(Period.ofYears(5)))
      .withCalendar(WeekendsOnly)
      .withTenor(Period.ofMonths(3))
      .withConvention(ModifiedFollowing)
      .withTerminationDateConvention(Unadjusted)
      .withRule(DateGeneration.CDS2015)
      .build()

  assert(s1.startDate == LocalDate.of(2016, SEPTEMBER, 20))
  assert(s1.endDate == LocalDate.of(2021, DECEMBER, 20))

  val s2 = ScheduleBuilder().from(LocalDate.of(2017, MARCH, 1))
      .to(LocalDate.of(2017, MARCH, 1) + Period.ofYears(5))
      .withCalendar(WeekendsOnly())
      .withTenor(Period.ofMonths(3))
      .withConvention(ModifiedFollowing)
      .withTerminationDateConvention(Unadjusted)
      .withRule(DateGeneration.CDS2015)
      .build()

  assert(s2.startDate == LocalDate.of(2016, DECEMBER, 20))
  assert(s2.endDate == LocalDate.of(2021, DECEMBER, 20))

  // From March 20th 2017 to September 19th 2017 end date is June 20th 2022 for a 5 year Swap
  val s3 = ScheduleBuilder().from(LocalDate.of(2017, MARCH, 20))
      .to(LocalDate.of(2017, MARCH, 20) + Period.ofYears(5))
      .withCalendar(WeekendsOnly())
      .withTenor(Period.ofMonths(3))
      .withConvention(ModifiedFollowing)
      .withTerminationDateConvention(Unadjusted)
      .withRule(DateGeneration.CDS2015)
      .build()

  assert(s3.startDate == LocalDate.of(2017, MARCH, 20))
  assert(s3.endDate == LocalDate.of(2022, JUNE, 20))
}

it should "take a vector of dates and possibly additional meta information" in {

  val dates = List(LocalDate.of(2015,MAY,16), LocalDate.of(2015,MAY,18), LocalDate.of(2016,MAY,18),
                   LocalDate.of(2017, DECEMBER, 31))

  // schedule without any additional information
  val schedule1 = Schedule(dates)

  assert(schedule1.size == dates.size, s"schedule1 has size ${schedule1.size}, expected ${dates.size}")

  for (i <- dates.indices) {
    assert(schedule1(i) == dates(i), s"schedule1 has ${schedule1(i)} at position $i, expected $dates(i)")
  }

  assert(schedule1.calendar.name == NullCalendar.name, s"schedule1 has calendar ${schedule1.calendar.name}, expected null calendar")
  assert(schedule1.convention == Unadjusted, s"schedule1 has convention ${schedule1.convention}, expected Unadjusted")

  // schedule with metadata
  val regular = List(false, true, false)

  val schedule2: Schedule = Schedule(dates, Target, Following, ModifiedPreceding, Period.ofYears(1),
    DateGeneration.Backward, true, regular)

  for (i <= regular.indices) {
    assert(schedule2.isRegular(i) == regular(i - 1))
  }

  assert(schedule2.calendar.name == Target.name, s"schedule1 has calendar ${schedule2.calendar.name}, expected TARGET")
  assert(schedule2.convention == Following, s"schedule2 has convention ${schedule2.convention}, expected Following")
  assert(schedule2.terminationConvention == Some(ModifiedPreceding), s"schedule2 has convention ${schedule2.terminationConvention}, expected Modified Preceding")
  assert(schedule2.tenor == Period.ofYears(1), s"schedule2 has tenor ${schedule2.tenor}, expected 1Y")
  assert(schedule2.rule == DateGeneration.Backward, s"schedule2 has rule ${schedule2.rule}, expected Backward")
  assert(schedule2.endOfMonth == Some(true), s"schedule2 has end of month flag false, expected true")
}
*/

  it should "support a four-week tenor" in {
    try {
      ScheduleBuilder().from(LocalDate.of(2016,JANUARY,13))
          .to(LocalDate.of(2016,MAY,4))
          .withCalendar(Target)
          .withTenor(Period.ofWeeks(4))
          .withConvention(Following)
          .forwards()
          .build()
    } catch{
      case e: Error => throw new AssertionError("A four-weeks tenor caused an exception: ", e)
    }
  }
}
