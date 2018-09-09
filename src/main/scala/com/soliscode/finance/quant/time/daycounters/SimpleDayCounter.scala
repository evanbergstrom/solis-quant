package com.soliscode.finance.quant.time.daycounters

import java.time.LocalDate

import com.soliscode.finance.quant.time.DayCounter

object SimpleDayCounter {
  val fallback : DayCounter = Thirty360(Thirty360.BondBasis)
}

class SimpleDayCounter extends DayCounter {

  override def name: String = "Simple"

  override def dayCount(start: LocalDate, end: LocalDate): Int = {
    SimpleDayCounter.fallback.dayCount(start,end)
  }

  override def yearFraction(start: LocalDate, end: LocalDate,
                            refPeriodStart: LocalDate, refPeriodEnd: LocalDate): Double = {
    val (startD, endD) = (start.getDayOfMonth, end.getDayOfMonth)

    if (startD == endD || (startD > endD && endD == end.lengthOfMonth()) ||
      (startD < endD && startD == start.lengthOfMonth())) {
      (end.getMonthValue - start.getMonthValue) / 12
    } else {
      SimpleDayCounter.fallback.yearFraction(start, end, refPeriodStart, refPeriodEnd)
    }
  }
}

