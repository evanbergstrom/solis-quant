package com.soliscode.finance.quant.time.calendars

import java.time.{LocalDate, Month}
import java.time.Month._

import org.scalatest.FlatSpec

class AustraliaTest extends FlatSpec {

  "isNormalBusinessDay" should "return false for observed New Year's Day holiday" in {
    val australia = new Australia
    assert(!australia.isNormalBusinessDay(LocalDate.of(2018, JANUARY, 1)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2019, JANUARY, 1)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2020, JANUARY, 1)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2021, JANUARY, 1)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2022, JANUARY, 3)))
  }

  it should "return false for observed Autralia Day holiday" in {
    val australia = new Australia
    assert(!australia.isNormalBusinessDay(LocalDate.of(2018, JANUARY, 26)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2019, JANUARY, 28)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2020, JANUARY, 27)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2021, JANUARY, 26)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2022, JANUARY, 26)))
  }

  it should "return false for Good Friday holiday" in {
    val australia = new Australia
    assert(!australia.isNormalBusinessDay(LocalDate.of(2018, MARCH, 30)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2019, APRIL, 19)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2020, APRIL, 10)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2021, APRIL, 2)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2022, APRIL, 15)))
  }

  it should "return false for the Easter Monday holiday" in {
    val australia = new Australia
    assert(!australia.isNormalBusinessDay(LocalDate.of(2018, APRIL, 2)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2019, APRIL, 22)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2020, APRIL, 13)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2021, APRIL, 5)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2022, APRIL, 18)))
  }

  it should "return false for the ANZAC Day holiday" in {
    val australia = new Australia
    assert(!australia.isNormalBusinessDay(LocalDate.of(2018, APRIL, 25)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2019, APRIL, 25)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2021, APRIL, 26)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2022, APRIL, 25)))
  }

  it should "return false for the Queens Birthday holiday" in {
    val australia = new Australia
    assert(!australia.isNormalBusinessDay(LocalDate.of(2018, JUNE, 11)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2019, JUNE, 10)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2020, JUNE, 8)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2021, JUNE, 14)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2022, JUNE, 13)))
  }

  it should "return false for the Christmas holiday" in {
    val australia = new Australia
    assert(!australia.isNormalBusinessDay(LocalDate.of(2018, DECEMBER, 25)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2019, DECEMBER, 25)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2020, DECEMBER, 25)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2021, DECEMBER, 27)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2022, DECEMBER, 26)))
  }

  it should "return false for the Boxing Day holiday" in {
    val australia = new Australia
    assert(!australia.isNormalBusinessDay(LocalDate.of(2018, DECEMBER, 26)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2019, DECEMBER, 26)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2020, DECEMBER, 28)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2021, DECEMBER, 28)))
    assert(!australia.isNormalBusinessDay(LocalDate.of(2022, DECEMBER, 27)))
  }

}
