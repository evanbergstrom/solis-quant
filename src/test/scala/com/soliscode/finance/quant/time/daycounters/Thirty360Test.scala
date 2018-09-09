package com.soliscode.finance.quant.time.daycounters

import java.time.LocalDate

import com.soliscode.finance.quant.time.daycounters.Thirty360.BondBasis
import org.scalatest.FlatSpec
import java.time.Month._


class Thirty360Test extends FlatSpec{

  "Thirty360(BondBasis)" should "handle end dates that are not the last day of february" in {
    val dc = Thirty360(BondBasis)

    assert(dc.dayCount(LocalDate.of(2006, AUGUST, 20), LocalDate.of(2007, FEBRUARY, 20)) == 180)
    assert(dc.dayCount(LocalDate.of(2007, FEBRUARY, 20), LocalDate.of(2007, AUGUST, 20)) == 180)
    assert(dc.dayCount(LocalDate.of(2007, AUGUST, 20), LocalDate.of(2008, FEBRUARY, 20)) == 180)
    assert(dc.dayCount(LocalDate.of(2008, FEBRUARY, 20), LocalDate.of(2008, AUGUST, 20)) == 180)
    assert(dc.dayCount(LocalDate.of(2008, AUGUST, 20), LocalDate.of(2009, FEBRUARY, 20)) == 180)
    assert(dc.dayCount(LocalDate.of(2009, FEBRUARY, 20), LocalDate.of(2009, AUGUST, 20)) == 180)
  }

  it should "handle end dates include some end-February dates" in {
    val dc = Thirty360(BondBasis)

    assert(dc.dayCount(LocalDate.of(2006, AUGUST, 31), LocalDate.of(2007, FEBRUARY, 28)) == 178)
    assert(dc.dayCount(LocalDate.of(2007, FEBRUARY, 28), LocalDate.of(2007, AUGUST, 31)) == 183)
    assert(dc.dayCount(LocalDate.of(2007, AUGUST, 31), LocalDate.of(2008, FEBRUARY, 29)) == 179)
    assert(dc.dayCount(LocalDate.of(2008, FEBRUARY, 29), LocalDate.of(2008, AUGUST, 31)) == 182)
    assert(dc.dayCount(LocalDate.of(2008, AUGUST, 31), LocalDate.of(2009, FEBRUARY, 28)) == 178)
    assert(dc.dayCount(LocalDate.of(2009, FEBRUARY, 28), LocalDate.of(2009, AUGUST, 31)) == 183)
  }

  it should "handle miscellaneous calculations" in {
    val dc = Thirty360(BondBasis)

    assert(dc.dayCount(LocalDate.of(2006, JANUARY, 31), LocalDate.of(2006, FEBRUARY, 28)) == 28)
    assert(dc.dayCount(LocalDate.of(2006, JANUARY, 30), LocalDate.of(2006, FEBRUARY, 28)) == 28)
    assert(dc.dayCount(LocalDate.of(2006, FEBRUARY, 28), LocalDate.of(2006, MARCH, 3)) == 5)
    assert(dc.dayCount(LocalDate.of(2006, FEBRUARY, 14), LocalDate.of(2006, FEBRUARY, 28)) == 14)
    assert(dc.dayCount(LocalDate.of(2006, SEPTEMBER, 30), LocalDate.of(2006, OCTOBER, 31)) == 30)
    assert(dc.dayCount(LocalDate.of(2006, OCTOBER, 31), LocalDate.of(2006, NOVEMBER, 28)) == 28)
    assert(dc.dayCount(LocalDate.of(2007, AUGUST, 31), LocalDate.of(2008, FEBRUARY, 28)) == 178)
    assert(dc.dayCount(LocalDate.of(2008, FEBRUARY, 28), LocalDate.of(2008, AUGUST, 28)) == 180)
    assert(dc.dayCount(LocalDate.of(2008, FEBRUARY, 28), LocalDate.of(2008, AUGUST, 30)) == 182)
    assert(dc.dayCount(LocalDate.of(2008, FEBRUARY, 28), LocalDate.of(2008, AUGUST, 31)) == 183)
    assert(dc.dayCount(LocalDate.of(2007, FEBRUARY, 26), LocalDate.of(2008, FEBRUARY, 28)) == 362)
    assert(dc.dayCount(LocalDate.of(2007, FEBRUARY, 26), LocalDate.of(2008, FEBRUARY, 29)) == 363)
    assert(dc.dayCount(LocalDate.of(2008, FEBRUARY, 29), LocalDate.of(2009, FEBRUARY, 28)) == 359)
    assert(dc.dayCount(LocalDate.of(2008, FEBRUARY, 28), LocalDate.of(2008, MARCH, 30)) == 32)
    assert(dc.dayCount(LocalDate.of(2008, FEBRUARY, 28), LocalDate.of(2008, MARCH, 31)) == 33)
  }
}
