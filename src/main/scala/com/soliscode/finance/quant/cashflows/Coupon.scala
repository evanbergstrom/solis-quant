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

package com.soliscode.finance.quant.cashflows

import java.time.LocalDate

import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.time.DayCounter


abstract class Coupon(paymentDate: LocalDate,
                      val nominal: Double,
                      val accrualStartDate: LocalDate,
                      val accrualEndDate: LocalDate,
                      refStartDate: Option[LocalDate],
                      refEndDate: Option[LocalDate],
                      val exCouponDate: Option[LocalDate]) extends CashFlow {

  val dayCounter: DayCounter

  val date: LocalDate = paymentDate

  val refPeriodStart: LocalDate = refStartDate match {
    case Some(startDate) => startDate
    case None => accrualStartDate
  }

  val refPeriodEnd: LocalDate = refEndDate match {
    case Some(endDate) => endDate
    case None => accrualEndDate
  }

  val accrualPeriod: Double = dayCounter.yearFraction(accrualStartDate, accrualEndDate, refPeriodStart, refPeriodEnd)
  val accrualDays: Long = dayCounter.dayCount(accrualStartDate, accrualEndDate)

  def rate() : Double
  def accruedAmount(date: LocalDate): Double

  def compatibleWith(other: Coupon): Boolean =
    nominal == other.nominal && accrualPeriod == other.accrualPeriod && dayCounter == other.dayCounter

  def accruedPeriod(d: LocalDate): Double =
    if (d.isNotAfter(accrualStartDate) || d.isAfter(date))
      0.0
    else
      dayCounter.yearFraction(accrualStartDate, min(d,accrualEndDate), refPeriodStart, refPeriodEnd)

  def accruedDays(d: LocalDate): Long =
    if (d.isNotAfter(accrualStartDate) || d.isAfter(date))
      0
    else
      dayCounter.dayCount(accrualStartDate, min(d, accrualEndDate))

}
