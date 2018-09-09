package com.soliscode.finance.quant.time


import java.time.temporal.ChronoUnit
import java.time.{LocalDate, Month, Period}

import com.soliscode.finance.quant.time.BusinessDayConvention._
import com.soliscode.finance.quant.time.calendars.UnitedStates
import com.soliscode.finance.quant.time.calendars.UnitedStates.Settlement
import org.scalatest.FlatSpec

import scala.collection.JavaConverters._

class BusinessDayConventionTest extends FlatSpec {

  val testCal = UnitedStates.withMarket(Settlement)
  val testRangeFrom = LocalDate.of(2018, Month.JANUARY, 1)
  val testRangeTo = LocalDate.of(2018, Month.DECEMBER, 31)

  def nextBusinessDay(d: LocalDate): LocalDate = {
    d.datesUntil(d.plusDays(7)).filter(b => testCal.isBusinessDay(b)).findFirst.get
  }

  def prevBusinessDay(d: LocalDate): LocalDate = {
    // TODO: Figure out how to do this with stanard methods, like for next
    DateIterator.withPeriod(d,d.minusDays(7),Period.ofDays(-1)).find(b => testCal.isBusinessDay(b)).get
  }

  "Unadusted" should " always return the input date" in {
    testRangeFrom.datesUntil(testRangeTo).iterator().asScala.foreach(d =>{
      assert(Unadjusted.adjust(d, testCal) == d)
    })
  }

  "Following" should " always return the next business day after a holiday" in {
    DateIterator(testRangeFrom, testRangeTo).foreach(d => {
      val adjusted = Following.adjust(d, testCal)
      if (testCal.isBusinessDay(d)) {
        assert(d == adjusted)
      } else {
        assert(adjusted == nextBusinessDay(d))
      }
    })
  }

  "ModifiedFollowing" should " return the next business day, unless it is in the next month, then return previous" in {
    DateIterator(testRangeFrom, testRangeTo).foreach(d => {
      val adjusted = ModifiedFollowing.adjust(d, testCal)
      if (testCal.isBusinessDay(d)) {
        assert(d == adjusted)
      } else {
        val n = nextBusinessDay(d)
        if (n.getMonth == d.getMonth) {
          assert(adjusted == n)
        } else {
          assert(adjusted == prevBusinessDay(d))
        }
      }
    })
  }

  "Preceding" should " always return the previous business day before a holiday" in {
    DateIterator(testRangeFrom, testRangeTo).foreach(d => {
      val adjusted = Preceding.adjust(d, testCal)
      if (testCal.isBusinessDay(d)) {
        assert(d == adjusted)
      } else {
        assert(adjusted == prevBusinessDay(d))
      }
    })
  }

  "ModifiedPreceding" should " return the previous business day, unless it is in the previous month, then return next" in {
    DateIterator(testRangeFrom, testRangeTo).foreach(d => {
      val adjusted = ModifiedPreceding.adjust(d, testCal)
      if (testCal.isBusinessDay(d)) {
        assert(d == adjusted)
      } else {
        val p = prevBusinessDay(d)
        if (p.getMonth == d.getMonth) {
          assert(adjusted == p)
        } else {
          assert(adjusted == nextBusinessDay(d))
        }
      }
    })
  }

  "Nearest" should " always return the nearest business day to a holiday" in {
    DateIterator(testRangeFrom, testRangeTo).foreach(d => {
      val adjusted = Nearest.adjust(d, testCal)
      if (testCal.isBusinessDay(d)) {
        assert(d == adjusted)
      } else {
        val prev = prevBusinessDay(d)
        val next = nextBusinessDay(d)
        val nearest = if (ChronoUnit.DAYS.between(prev,d) < ChronoUnit.DAYS.between(d,next)) prev else next
        assert(adjusted == nearest)
      }
    })
  }

}

