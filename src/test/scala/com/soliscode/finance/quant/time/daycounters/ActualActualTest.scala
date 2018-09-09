package com.soliscode.finance.quant.time.daycounters

import java.time.LocalDate

import com.soliscode.finance.quant.time.Schedule
import com.soliscode.finance.quant.time.daycounters.ActualActual.{Isda, Isma}
import org.scalatest.FlatSpec
import java.time.Month._

class ActualActualTest extends FlatSpec {

  "ActualActual(ISDA)" should "handle normal calculation periods" in {
    val dc = ActualActual(Isda, Schedule())
    assert(dc.yearFraction(LocalDate.of(2003, NOVEMBER, 1), LocalDate.of(2004, MAY, 1)) == 0.49772438056740775)
  }

  it should "handle short first calculation period (first period)" in {
    val dc = ActualActual(Isda, Schedule())
    assert(dc.yearFraction(LocalDate.of(1999, FEBRUARY, 1), LocalDate.of(1999, JULY, 1)) == 0.41095890410958910)
  }

  it  should "handle short first calculation period (second period)" in {
    val dc = ActualActual(Isda, Schedule())
    assert(dc.yearFraction(LocalDate.of(1999, JULY, 1), LocalDate.of(2000, JULY, 1)) == 1.00137734860393750)
  }

  it should "handle long first calculation period (first period)" in {
    val dc = ActualActual(Isda, Schedule())
    assert(dc.yearFraction(LocalDate.of(2002, AUGUST, 15), LocalDate.of(2003, JULY, 15)) == 0.91506849315068500)
  }

  it should "handle long first calculation period (second period)" in {
    val dc = ActualActual(Isda, Schedule())
    assert(dc.yearFraction(LocalDate.of(2003, JULY, 15), LocalDate.of(2004, JANUARY, 15)) == 0.50400479077775280)
  }

  it should "handle short final calculation period (penultimate period)" in {
    val dc = ActualActual(Isda, Schedule())
    assert(dc.yearFraction(LocalDate.of(1999, JULY, 30), LocalDate.of(2000, JANUARY, 30)) == 0.50389250692417100)
  }

  it should "handle short final calculation period (final period)" in {
    val dc = ActualActual(Isda, Schedule())
    assert(dc.yearFraction(LocalDate.of(2000, JANUARY, 30), LocalDate.of(2000, JUNE, 30)) == 0.41530054644808745)
  }



  "ActualActual(ISMA)" should "handle normal calculation periods" in {
    val dc = ActualActual(Isma, Schedule())
    assert(dc.yearFraction(LocalDate.of(2003, NOVEMBER, 1), LocalDate.of(2004, MAY, 1)) == 0.500000000000)
  }

  it should "handle short first calculation period (first period)" in {
    val dc = ActualActual(Isma, Schedule())
    assert(dc.yearFraction(LocalDate.of(1999, FEBRUARY, 1), LocalDate.of(1999, JULY, 1),
      LocalDate.of(1998, JULY, 1), LocalDate.of(1999, JULY, 1)) == 0.410958904109589)
  }

  it  should "handle short first calculation period (second period)" in {
    val dc = ActualActual(Isma, Schedule())
    assert(dc.yearFraction(LocalDate.of(1999, JULY, 1), LocalDate.of(2000, JULY, 1)) == 1.000000000000)
  }

  it should "handle long first calculation period (first period)" in {
    val dc = ActualActual(Isma, Schedule())
    assert(dc.yearFraction(LocalDate.of(2002, AUGUST, 15), LocalDate.of(2003, JULY, 15),
      LocalDate.of(2003, JANUARY, 15), LocalDate.of(2003, JULY, 15)) == 0.9157608695652174)
  }

  it should "handle long first calculation period (second period)" in {
    val dc = ActualActual(Isma, Schedule())
    assert(dc.yearFraction(LocalDate.of(2003, JULY, 15), LocalDate.of(2004, JANUARY, 15)) == 0.500000000000)
  }

  it should "handle short final calculation period (penultimate period)" in {
    val dc = ActualActual(Isma, Schedule())
    assert(dc.yearFraction(LocalDate.of(1999, JULY, 30), LocalDate.of(2000, JANUARY, 30)) == 0.500000000000)
  }

  it should "handle short final calculation period (final period)" in {
    val dc = ActualActual(Isma, Schedule())
    assert(dc.yearFraction(LocalDate.of(2000, JANUARY, 30), LocalDate.of(2000, JUNE, 30),
      LocalDate.of(2000, JANUARY, 30), LocalDate.of(2000, JULY, 30)) == 0.4175824175824176)
  }
}

