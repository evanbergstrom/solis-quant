package com.soliscode.finance.quant.time

import java.time.LocalDate

trait DayCounter {

  def name: String

  def dayCount(start: LocalDate, end: LocalDate) : Int =
    start.until(end).getDays

  def yearFraction(start: LocalDate, end: LocalDate) : Double = {
    yearFraction(start, end, start, end)
  }

  def yearFraction(start: LocalDate, end: LocalDate, refPeriodStart: LocalDate, refPeriodEnd: LocalDate) : Double
}
