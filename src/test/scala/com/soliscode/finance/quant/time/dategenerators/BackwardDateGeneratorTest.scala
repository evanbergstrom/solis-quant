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

import java.time.Month._
import java.time.{LocalDate, Month, Period}

import com.soliscode.finance.quant.time.BusinessDayConvention.{Following, Preceding, Unadjusted}
import com.soliscode.finance.quant.time.calendars.UnitedStates.GovernmentBond
import com.soliscode.finance.quant.time.calendars.{Japan, Target, UnitedStates}
import org.scalatest.FlatSpec

class BackwardDateGeneratorTest extends FlatSpec {

  "Generate" should "create a daily schedule" in {

    val startDate = LocalDate.of(2012, Month.JANUARY, 17)

    val dg = new BackwardDateGenerator(startDate, startDate.plusDays(7), Period.ofDays(1), Target, Preceding,
      false, None, None, None)

    val (dates, regular) = dg.generate()

    assert(dates.size == 6, s"$dates")
    assert(dates(0) == LocalDate.of(2012, JANUARY, 17))
    assert(dates(1) == LocalDate.of(2012, JANUARY, 18))
    assert(dates(2) == LocalDate.of(2012, JANUARY, 19))
    assert(dates(3) == LocalDate.of(2012, JANUARY, 20))
    assert(dates(4) == LocalDate.of(2012, JANUARY, 23))
    assert(dates(5) == LocalDate.of(2012, JANUARY, 24))
  }

  it should "generate a schedule with an end of month (EOM) adjustment" in {

    // Testing that the first date is not adjusted for EOM going backward when termination
    // date convention is unadjusted...

    val dg = new BackwardDateGenerator(LocalDate.of(1996,Month.AUGUST,22),
      LocalDate.of(1997, AUGUST, 31), Period.ofMonths(6), UnitedStates.withMarket(GovernmentBond), Unadjusted,
      true, Some(Unadjusted), None, None)

    val (dates, regular) = dg.generate()

    assert(dates.size == 4, s", dates=$dates")
    assert(dates(0) == LocalDate.of(1996, AUGUST, 22))
    assert(dates(1) == LocalDate.of(1996, AUGUST, 31))
    assert(dates(2) == LocalDate.of(1997, FEBRUARY, 28))
    assert(dates(3) == LocalDate.of(1997, AUGUST, 31))

  }

  it should "not duplicate the first date due to end of month adjustemnt" in {

    val dg = new BackwardDateGenerator(LocalDate.of(1996,Month.AUGUST,22),
      LocalDate.of(1997, AUGUST, 31), Period.ofMonths(6), UnitedStates.withMarket(GovernmentBond), Following,
      true, Some(Following), None, None)

    val (dates, regular) = dg.generate()

    assert(dates.size == 3)
    assert(dates(0) == LocalDate.of(1996, AUGUST, 30))
    assert(dates(1) == LocalDate.of(1997, FEBRUARY, 28))
    assert(dates(2) == LocalDate.of(1997, AUGUST, 29))
  }
}
