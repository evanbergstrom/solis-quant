/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a free-software/open-source library for financial quantitative analysts and developers (http://quantlib.org/) to Scala. The basic structure and design of the library has been preserved, but thebnaming conventions, basic types, collection classes and implementation have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.instruments

import java.time.{LocalDate, Period}

import com.soliscode.finance.quant.{EvaluationContext, PrivateEvaluationContext}
import com.soliscode.finance.quant.cashflows.CashFlow.Leg
import com.soliscode.finance.quant.time.BusinessDayConvention.{Following, Unadjusted}
import com.soliscode.finance.quant.time._

object FixedRateBond {
  def apply(settlementDays: Long,
            faceAmount: Double,
            schedule: Schedule,
            coupon: List[Double],
            accrualDayCounter: DayCounter,
            paymentConvention: BusinessDayConvention = Following,
            redemption: Double = 100.0,
            issueDate: LocalDate,
            paymentCalendar: Calendar,
            exCouponPeriod: Period,
            exCouponCalendar: Calendar,
            exCouponConvention: BusinessDayConvention  = Unadjusted,
            exCouponEndOfMonth: Boolean) : FixedRateBond = ???
}

class FixedRateBond(evaluationContext: EvaluationContext,
                    settlementDays: Int,
                    issueDate: LocalDate,
                    cashflows: Leg,
                    paymentCalendar: Calendar,
                    frequency: Frequency,
                    accrualDayCounter: DayCounter,
                    maturityDate: LocalDate,
                    redemption: Double)
  extends Bond(evaluationContext, settlementDays, paymentCalendar, 100 /*faceAmount*/, issueDate, maturityDate, cashflows) {

}

object FixedRateBondBuilder {
  def apply() : FixedRateBondBuilder =
    new FixedRateBondBuilder(PrivateEvaluationContext())

  def apply(evaluationContext: EvaluationContext) =
    new FixedRateBondBuilder(evaluationContext)
}

class FixedRateBondBuilder(evaluationContext: EvaluationContext) {


  var settlementDays: Option[Long] = None
  var schedule: Option[Schedule] = None
  var coupons: Option[List[Double]] = None
  var faceAmount: Option[Long] = None
  var redemption: Option[Double] = None
  var paymentCalendar: Option[Calendar] = None
  var paymentConvention: Option[BusinessDayConvention] = None
  var accrualDayCounter: Option[DayCounter] = None

  var exCouponPeriod: Option[Period] = None
  var exCouponCalendar: Option[Calendar] = None
  var exCouponConvention: Option[BusinessDayConvention] = None
  var exCouponEndOfMOnth: Option[Boolean] = None

  def settlementDays(days: Long): FixedRateBondBuilder = {
    settlementDays = Some(days)
    this
  }

  def schedule(sched: Schedule): FixedRateBondBuilder = {
    schedule = Some(sched)
    this
  }

  def coupons(coupons: List[Double]): FixedRateBondBuilder = {
    this.coupons = Some(coupons)
    this
  }

  def faceAmount(amount: Long): FixedRateBondBuilder = {
    faceAmount = Some(amount)
    this
  }

  def redemption(value: Double): FixedRateBondBuilder = {
    redemption = Some(value)
    this
  }

  def paymentCalendar(calendar: Calendar): FixedRateBondBuilder = {
    paymentCalendar = Some(calendar)
    this
  }

  def paymentConvention(convention: BusinessDayConvention): FixedRateBondBuilder = {
    paymentConvention = Some(convention)
    this
  }

  def accrualDayCounter(dayCounter: DayCounter): FixedRateBondBuilder = {
    accrualDayCounter = Some(dayCounter)
    this
  }

  def exCouponCalendar(calendar: Calendar): FixedRateBondBuilder = {
    exCouponCalendar = Some(calendar)
    this
  }

  def exCouponConvention(convention: BusinessDayConvention): FixedRateBondBuilder = {
    exCouponConvention = Some(convention)
    this
  }

  def exCouponPeriod(period: Period): FixedRateBondBuilder = {
    exCouponPeriod = Some(period)
    this
  }

  def exCouponEndOfMonth(endOfMonth: Boolean): FixedRateBondBuilder = {
    exCouponEndOfMOnth = Some(endOfMonth)
    this
  }

  def build(): FixedRateBond = {
    null
    /*new FixedRateBond(evaluationContext,
                      settlementDays,
                      faceAmount,
                     )*/

  }
}
