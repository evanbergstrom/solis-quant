package com.soliscode.finance.quant.time.calendars

import com.soliscode.finance.quant.time.Calendar
import org.scalatest.FlatSpec

class CalendarTest extends FlatSpec {
  import java.time.{DayOfWeek, LocalDate, Month}

  import com.soliscode.finance.quant.time.calendars.UnitedStates.Settlement
  import org.scalatest.FlatSpec

  import scala.collection.JavaConverters._

  class CalendarTest extends FlatSpec {

    val testCal : Calendar = UnitedStates.withMarket(Settlement)
    val testRangeFrom : LocalDate = LocalDate.of(2018, Month.JANUARY, 1)
    val testRangeTo : LocalDate = LocalDate.of(2018, Month.DECEMBER, 31)

    "isWeekend" should "return true only for weekend days" in {
      testRangeFrom.datesUntil(testRangeTo).iterator().asScala.foreach(d => {
        if (d.getDayOfWeek == DayOfWeek.SATURDAY || d.getDayOfWeek == DayOfWeek.SUNDAY)
          assert(testCal.isWeekend(d))
        else
          assert(!testCal.isWeekend(d))
      })
    }
  }

}
