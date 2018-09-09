package com.soliscode.finance.quant.time.calendars

import java.time.Month._
import java.time.{DayOfWeek, LocalDate}

import com.soliscode.finance.quant.time.DateIterator
import com.soliscode.finance.quant.time.calendars.UnitedStates.{GovernmentBond, Settlement}
import org.scalatest.FlatSpec

class UnitedStatesTest extends FlatSpec {

  private def checkHolidays(expected: List[LocalDate], isHoliday: LocalDate => Boolean) : Unit = {
    expected.foreach(d => {
      assert(!isHoliday(d.minusDays(1)), s"Failed for date $d")
      assert(isHoliday(d), s"Failed for date $d")
      assert(!isHoliday(d.plusDays(1)), s"Failed for date $d")
    })
  }

  "isNewYearsDay" should "return true for observed New Year's Day holiday" in {
    val expectedHolidays = List(
      LocalDate.of(2010, JANUARY, 1),    // 2010
      LocalDate.of(2010, DECEMBER, 31),  // 2011
      LocalDate.of(2012, JANUARY, 2),    // 2012
      LocalDate.of(2013, JANUARY, 1),    // 2013
      LocalDate.of(2014, JANUARY, 1),    // 2014
      LocalDate.of(2015, JANUARY, 1),    // 2015
      LocalDate.of(2016, JANUARY, 1),    // 2016
      LocalDate.of(2017, JANUARY, 2)     // 2017
    )
    checkHolidays(expectedHolidays, d=> UnitedStates.isNewYearsDay(d.getDayOfMonth,d.getMonth, d.getDayOfWeek))
  }


  "isMartinLutherKingDay" should "return true for Martin Luther King Jr. Day" in {
    val expectedHolidays = List(
      LocalDate.of(2010, JANUARY, 18),
      LocalDate.of(2011, JANUARY, 17),
      LocalDate.of(2012, JANUARY, 16),
      LocalDate.of(2013, JANUARY, 21),
      LocalDate.of(2014, JANUARY, 20),
      LocalDate.of(2015, JANUARY, 19),
      LocalDate.of(2016, JANUARY, 18),
      LocalDate.of(2017, JANUARY, 16),
      LocalDate.of(2018, JANUARY, 15),
      LocalDate.of(2019, JANUARY, 21),
      LocalDate.of(2020, JANUARY, 20)
    )
    checkHolidays(expectedHolidays, d=> UnitedStates.isMartinLutherKingDay(d.getDayOfMonth,d.getMonth, d.getYear, d.getDayOfWeek))

    // Not observed before 1983
    assert(!UnitedStates.isMartinLutherKingDay(18, JANUARY, 1982, DayOfWeek.MONDAY))
  }


  "Settlement" should "identify business days for U.S. settlement" in {
    val cal = UnitedStates.withMarket(Settlement)

    val expectedHolidays = Set(
      LocalDate.of(2004, JANUARY,1),      // New Years Day
      LocalDate.of(2004, JANUARY, 19),   // Marting Liuther King Jr. Day
      LocalDate.of(2004, FEBRUARY, 16),  // President's Day
      LocalDate.of(2004, MAY, 31),       // Memorial Day
      LocalDate.of(2004, JULY, 5),       // Independence Day (Observed)
      LocalDate.of(2004, SEPTEMBER, 6),  // Labor Day
      LocalDate.of(2004, OCTOBER, 11),   // Columbus Day
      LocalDate.of(2004, NOVEMBER, 11),  // Veterans Day
      LocalDate.of(2004, NOVEMBER, 25),  // Thanksgiving
      LocalDate.of(2004, DECEMBER, 24),  // Christmas (Observed)
      LocalDate.of(2004, DECEMBER, 31),  // New Year's Day (Observed)
      LocalDate.of(2005, JANUARY, 17),   // Marting Liuther King Jr. Day
      LocalDate.of(2005, FEBRUARY, 21),  // President's Day
      LocalDate.of(2005, MAY, 30),       // Memorial Day
      LocalDate.of(2005, JULY, 4),       // Independence Day (Observed)
      LocalDate.of(2005, SEPTEMBER, 5),  // Labor Day
      LocalDate.of(2005, OCTOBER, 10),   // Columbus Day
      LocalDate.of(2005, NOVEMBER, 11),  // Veterans Day
      LocalDate.of(2005, NOVEMBER, 24),  // Thanksgiving
      LocalDate.of(2005, DECEMBER, 26)   // Christmas (Observed)
    )

    DateIterator(LocalDate.of(2004,JANUARY,1), LocalDate.of(2006,JANUARY,1)).foreach(d => {
      val isBusDay = d.getDayOfWeek != DayOfWeek.SATURDAY && d.getDayOfWeek != DayOfWeek.SUNDAY && !expectedHolidays.contains(d)
      assert(cal.isNormalBusinessDay(d) == isBusDay, s"Failed for date $d")
    })
  }

  "GovernmentBond" should "identify business days for U.S. settlement" in {
    val cal = UnitedStates.withMarket(GovernmentBond)

    val expectedHolidays = Set(
      LocalDate.of(2004, JANUARY,1),      // New Years Day
      LocalDate.of(2004, JANUARY, 19),   // Marting Liuther King Jr. Day
      LocalDate.of(2004, FEBRUARY, 16),  // President's Day
      LocalDate.of(2004, APRIL, 9),      // Good Friday
      LocalDate.of(2004, MAY, 31),       // Memorial Day
      LocalDate.of(2004, JULY, 5),       // Independence Day (Observed)
      LocalDate.of(2004, SEPTEMBER, 6),  // Labor Day
      LocalDate.of(2004, OCTOBER, 11),   // Columbus Day
      LocalDate.of(2004, NOVEMBER, 11),  // Veterans Day
      LocalDate.of(2004, NOVEMBER, 25),  // Thanksgiving
      LocalDate.of(2004, DECEMBER, 24)   // Christmas (Observed)
    )

    DateIterator(LocalDate.of(2004,JANUARY,1), LocalDate.of(2005,JANUARY,1)).foreach(d => {
      val isBusDay = d.getDayOfWeek != DayOfWeek.SATURDAY && d.getDayOfWeek != DayOfWeek.SUNDAY && !expectedHolidays.contains(d)
      assert(cal.isNormalBusinessDay(d) == isBusDay, s"Failed for date $d")
    })
  }

}

