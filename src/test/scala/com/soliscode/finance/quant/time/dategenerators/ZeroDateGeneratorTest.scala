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

import java.time.{LocalDate, Month}
import java.time.Month._

import org.scalatest.FlatSpec

class ZeroDateGeneratorTest extends FlatSpec {

  "generate" should "generate a schedule with two dates" in {

    val dg = new ZeroDateGenerator(LocalDate.of(2018, OCTOBER, 7), LocalDate.of(2020, OCTOBER, 1))

    val (dates, regular) = dg.generate()

    assert(dates.size == 2)
    assert(dates(0) == LocalDate.of(2018, OCTOBER, 7))
    assert(dates(1) == LocalDate.of(2020, OCTOBER, 1))

    assert(regular(0))
  }
}
