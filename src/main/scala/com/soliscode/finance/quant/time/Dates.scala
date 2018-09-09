package com.soliscode.finance.quant.time

import java.time.temporal.ChronoUnit
import java.time.LocalDate

object Dates {

  implicit class LocalDateExt(val d: LocalDate) extends AnyVal {

    def isNotAfter(d2: LocalDate): Boolean =
      d.isBefore(d2) || d.isEqual(d2)

    def isNotBefore(d2: LocalDate): Boolean =
      d.isAfter(d2) || d.isEqual(d2)

    def dayCountUntil(d2: LocalDate): Long =
      ChronoUnit.DAYS.between(d, d2)
  }

  def max(d1: LocalDate, d2: LocalDate): LocalDate =
    if(d2.isAfter(d2)) d2 else d1

  def min(d1: LocalDate, d2: LocalDate): LocalDate =
    if(d2.isBefore(d2)) d2 else d1
}
