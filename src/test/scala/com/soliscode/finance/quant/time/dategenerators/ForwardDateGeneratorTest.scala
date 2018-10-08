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
import java.time.{LocalDate, Period}

import com.soliscode.finance.quant.time.BusinessDayConvention.{Following, Unadjusted}
import com.soliscode.finance.quant.time.calendars.UnitedStates.GovernmentBond
import com.soliscode.finance.quant.time.calendars.{Japan, Target, UnitedStates}
import org.scalatest.FlatSpec

class ForwardDateGeneratorTest extends FlatSpec {

  "Generate" should "create a schedule with a termination convention and EOM edjustment" in {

    val dg1 = new ForwardDateGenerator(LocalDate.of(2009,SEPTEMBER, 30), LocalDate.of(2012,JUNE,15),
      Period.ofMonths(6), Japan, Following, true, Some(Following), None, None)

    val (dates, _) = dg1.generate()

    assert(dates.size == 7)

    // The end date is adjusted, so it should also be moved to the end of the month.
    assert(dates(0) == LocalDate.of(2009, SEPTEMBER, 30), s" dates=$dates")
    assert(dates(1) == LocalDate.of(2010, MARCH, 31), s" dates=$dates")
    assert(dates(2) == LocalDate.of(2010, SEPTEMBER, 30), s" dates=$dates")
    assert(dates(3) == LocalDate.of(2011, MARCH, 31), s" dates=$dates")
    assert(dates(4) == LocalDate.of(2011, SEPTEMBER, 30), s" dates=$dates")
    assert(dates(5) == LocalDate.of(2012, MARCH, 30), s" dates=$dates")
    assert(dates(6) == LocalDate.of(2012, JUNE, 29), s" dates=$dates")

    val dg2 = new ForwardDateGenerator(LocalDate.of(2009, SEPTEMBER, 30), LocalDate.of(2012, JUNE, 15),
      Period.ofMonths(6), Japan, Following, true, Some(Unadjusted), None, None)

    val (dates2, _) = dg2.generate()

    // ...which should leave it alone.
    assert(dates2(0) == LocalDate.of(2009, SEPTEMBER, 30))
    assert(dates2(1) == LocalDate.of(2010, MARCH, 31))
    assert(dates2(2) == LocalDate.of(2010, SEPTEMBER, 30))
    assert(dates2(3) == LocalDate.of(2011, MARCH, 31))
    assert(dates2(4) == LocalDate.of(2011, SEPTEMBER, 30))
    assert(dates2(5) == LocalDate.of(2012, MARCH, 30))
    assert(dates2(6) == LocalDate.of(2012, JUNE, 15))
  }

  it should "not generate dates past the end due to the end of month adjustment" in {

    val dg = new ForwardDateGenerator(LocalDate.of(2013, MARCH, 28), LocalDate.of(2015, MARCH, 30),
      Period.ofYears(1), Target, Unadjusted, true, Some(Unadjusted), None, None)

    val (dates, regular) = dg.generate()

    assert(dates.size == 3)

    assert(dates(0) == LocalDate.of(2013, MARCH, 31))
    assert(dates(1) == LocalDate.of(2014, MARCH, 31))
    // March 31st 2015, coming from the EOM adjustment of March 28th, should be discarded as past the end date.
    assert(dates(2) == LocalDate.of(2015, MARCH, 30))

    // also, the last period should not be regular.
    assert(!regular(1), s" regular=$regular")
  }

  it should "not duplicate the end date when EOM adjustment is used" in {

    val dg = new ForwardDateGenerator(LocalDate.of(2013, MARCH, 28), LocalDate.of(2015, MARCH, 31),
      Period.ofYears(1), Target, Unadjusted, true, Some(Unadjusted), None, None)

    val (dates, regular) = dg.generate()

    assert(dates.size == 3)

    assert(dates(0) == LocalDate.of(2013, MARCH, 31))
    assert(dates(1) == LocalDate.of(2014, MARCH, 31))
    // March 31st 2015, coming from the EOM adjustment of March 28th, should be discarded as past the end date.
    assert(dates(2) == LocalDate.of(2015, MARCH, 31))

    // also, the last period should be regular.
    assert(regular(1), s" regular=$regular")
  }

  it should "not adjust the last date for EOM when teermination date convention is unadjusted" in {

    val dg = new ForwardDateGenerator(LocalDate.of(1996, AUGUST, 31), LocalDate.of(1997, SEPTEMBER, 15),
      Period.ofMonths(6), UnitedStates.withMarket(GovernmentBond), Unadjusted, true, Some(Unadjusted), None, None)

    val (dates, _) = dg.generate()

    assert(dates.size == 4)
    assert(dates(0) == LocalDate.of(1996, AUGUST, 31))
    assert(dates(1) == LocalDate.of(1997, FEBRUARY, 28))
    assert(dates(2) == LocalDate.of(1997, AUGUST, 31))
    assert(dates(3) == LocalDate.of(1997, SEPTEMBER, 15))
  }
}
