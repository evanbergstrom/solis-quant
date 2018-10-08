/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 Copyright (C) 2018 Evan Bergstrom

 This file is provided under the BSD open software license. This is a port of QuantLib,
 a free-software/open-source library for financial quantitative analysts and developers
 (http://quantlib.org/) to Scala. The basic structure and design of the library has been
 preserved, but the naming conventions, types, collection classes and implementation
 have been modified to support common Scala idioms.

 See the full license in the license file (LICENSE.txt)
*/

package com.soliscode.finance.quant.time

import java.time.temporal.ChronoUnit
import java.time.{LocalDate, Month, Period, YearMonth}

trait DateOp[R] {
  def using(convention: BusinessDayConvention, calendar: Calendar): R
}

trait DateLogicalOp extends DateOp[Boolean] {
  implicit def toBoolean: Boolean
}

class DateEqualOp(d1: LocalDate, d2: LocalDate) extends DateLogicalOp {

  override def using(convention: BusinessDayConvention, calendar: Calendar): Boolean =
    convention.adjust(d1, calendar).isEqual(convention.adjust(d2, calendar))

  implicit def toBoolean: Boolean =
    d1.isEqual(d2)
}

class DateNotEqualOp(d1: LocalDate, d2: LocalDate) extends DateLogicalOp {

  override def using(convention: BusinessDayConvention, calendar: Calendar): Boolean =
    !convention.adjust(d1, calendar).isEqual(convention.adjust(d2, calendar))

  implicit def toBoolean: Boolean =
    !d1.isEqual(d2)
}

object Dates {

  implicit class LocalDateExt(val d: LocalDate) extends AnyVal {

    def isNotAfter(d2: LocalDate): Boolean =
      d.isBefore(d2) || d.isEqual(d2)

    def isNotBefore(d2: LocalDate): Boolean =
      d.isAfter(d2) || d.isEqual(d2)

    def < (d2: LocalDate): Boolean = d.isBefore(d2)
    def > (d2: LocalDate): Boolean = d.isAfter(d2)
    def <= (d2: LocalDate): Boolean = d.isNotAfter(d2)
    def >= (d2: LocalDate): Boolean = d.isNotBefore(d2)
    def != (d2: LocalDate): Boolean = !d.isEqual(d2)
    def == (d2: LocalDate): Boolean = d.isEqual(d2)

    def + (p: Period): LocalDate = d.plus(p)
    def - (p: Period): LocalDate = d.minus(p)

    def dayCountUntil(d2: LocalDate): Long =
      ChronoUnit.DAYS.between(d, d2)

    def endOfYear(i: Int = 0): LocalDate =
      LocalDate.of(d.getYear + i, Month.DECEMBER, 31)

    def endOfMonth(i: Int = 0): LocalDate =
      LocalDate.of(d.getYear, d.getMonth, YearMonth.of(d.getYear, d.getMonth).lengthOfMonth())

    def equal(d2: LocalDate): DateLogicalOp = new DateEqualOp(d,d2)
    def notEqual(d2: LocalDate): DateLogicalOp = new DateNotEqualOp(d,d2)
  }

  def max(d1: LocalDate, d2: LocalDate): LocalDate =
    if(d2.isAfter(d2)) d2 else d1

  def min(d1: LocalDate, d2: LocalDate): LocalDate =
    if(d2.isBefore(d2)) d2 else d1

  implicit val localDateOrdering: Ordering[LocalDate] = _ compareTo _
}
