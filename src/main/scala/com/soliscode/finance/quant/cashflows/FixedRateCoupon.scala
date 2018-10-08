/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a free-software/open-source library for financial quantitative analysts and developers (http://quantlib.org/) to Scala. The basic structure and design of the library has been preserved, but thebnaming conventions, basic types, collection classes and implementation have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.cashflows

import java.time.temporal.ChronoUnit
import java.time.{LocalDate, Period}

import com.soliscode.finance.quant
import com.soliscode.finance.quant.InterestRate
import com.soliscode.finance.quant.Compounding
import com.soliscode.finance.quant.cashflows.CashFlow.Leg
import com.soliscode.finance.quant.time.BusinessDayConvention.Following
import com.soliscode.finance.quant.time._
import com.soliscode.finance.quant.time.Dates._

import scala.collection.mutable

object FixedRateCoupon {
  def apply(paymentDate: LocalDate,
            nominal: Double,
            rate: Double,
            dayCounter: DayCounter,
            accrualStartDate: LocalDate,
            accrualEndDate: LocalDate,
            refPeriodStart: LocalDate,
            refPeriodEnd: LocalDate,
            exCouponDate: LocalDate) : FixedRateCoupon = {
    new FixedRateCoupon(paymentDate, nominal, InterestRate(rate, dayCounter, quant.Simple, Annual), accrualStartDate,
      accrualEndDate, Some(refPeriodStart), Some(refPeriodEnd), Some(exCouponDate))
  }

  def apply(paymentDate: LocalDate,
            nominal: Double,
            interestRate: InterestRate,
            accrualStartDate: LocalDate,
            accrualEndDate: LocalDate,
            refPeriodStart: LocalDate,
            refPeriodEnd: LocalDate,
            exCouponDate: LocalDate) : FixedRateCoupon = {
    new FixedRateCoupon(paymentDate, nominal, interestRate, accrualStartDate, accrualEndDate, Some(refPeriodStart),
      Some(refPeriodEnd), Some(exCouponDate))
  }

}

class FixedRateCoupon(paymentDate: LocalDate,
                      nominal: Double,
                      val interestRate: InterestRate,
                      accrualStartDate: LocalDate,
                      accrualEndDate: LocalDate,
                      refStartDate: Option[LocalDate],
                      refEndDate: Option[LocalDate],
                      exCouponDate: Option[LocalDate])
  extends Coupon(paymentDate, nominal, accrualStartDate, accrualEndDate, refStartDate, refEndDate, exCouponDate) {

  override val dayCounter: DayCounter = interestRate.dayCounter

  override def rate(): Double =
    interestRate.rate

  override def accruedAmount(date: LocalDate): Double =
    if (date <= accrualStartDate || date > paymentDate)
      0.0
    else if (tradingExCoupon(date))
      -nominal * (interestRate.compoundFactor(date, accrualEndDate, refPeriodStart, refPeriodEnd) - 1.0)
    else
      nominal * (interestRate.compoundFactor(accrualStartDate, min(date,accrualEndDate), refPeriodStart, refPeriodEnd) - 1.0)

  override def amount: Double =
    nominal * (interestRate.compoundFactor(accrualStartDate, accrualEndDate, refPeriodStart, refPeriodEnd) - 1.0)

}

class FixedRateLegBuilder(val schedule: Schedule) {
  var notionals: Seq[Double] = Seq()
  var couponRates: Seq[InterestRate] = Seq()

  var paymentAdjustment: BusinessDayConvention = Following
  var paymentLag: Int = 0

  var paymentCalendarOption: Option[Calendar] = None
  var firstPeriodDCOption: Option[DayCounter] = None
  var lastPeriodDCOption: Option[DayCounter] = None
  var exCouponPeriod: Option[ExCouponPeriod] = None

  def withNotional(notional: Double): FixedRateLegBuilder = {
    notionals = List(notional)
    this
  }

  def withNotionals(notionals: Seq[Double]): FixedRateLegBuilder = {
    this.notionals = notionals
    this
  }

  def withCouponRate(rate: Double,
                     dayCounter: DayCounter): FixedRateLegBuilder = {
    couponRates = List(InterestRate(rate, dayCounter, quant.Simple, Annual))
    this
  }

  def withCouponRate(rate: Double,
                     dayCounter: DayCounter,
                     compounding: Compounding,
                     frequency: Frequency): FixedRateLegBuilder = {
    couponRates = List(InterestRate(rate, dayCounter, compounding, frequency))
    this
  }

  def withCouponRates(rates: Seq[Double],
                      dayCounter: DayCounter): FixedRateLegBuilder = {
    couponRates = rates.map(r => InterestRate(r, dayCounter, quant.Simple, Annual))
    this
  }

  def withCouponRates(rates: Seq[Double],
                      dayCounter: DayCounter,
                      compounding: Compounding,
                      frequency: Frequency): FixedRateLegBuilder = {
    couponRates = rates.map(r => InterestRate(r, dayCounter, compounding, frequency))
    this
  }

  def withCouponRate(rate: InterestRate): FixedRateLegBuilder = {
    couponRates = List(rate)
    this
  }

  def withCouponRates(rates: Seq[InterestRate]): FixedRateLegBuilder = {
    couponRates = rates
    this
  }

  def withPaymentAdjustemnt(convention: BusinessDayConvention): FixedRateLegBuilder = {
    paymentAdjustment = convention
    this
  }

  def withFirstPeriodDayCounter(dayCounter: DayCounter): FixedRateLegBuilder = {
    firstPeriodDCOption = Some(dayCounter)
    this
  }

  def withLastPeriodDayCounter(dayCounter: DayCounter): FixedRateLegBuilder = {
    lastPeriodDCOption = Some(dayCounter)
    this
  }

  def withPaymentCalendar(calendar: Calendar): FixedRateLegBuilder = {
    paymentCalendarOption = Some(calendar)
    this
  }

  def withPaymentLag(lag: Int): FixedRateLegBuilder = {
    paymentLag = lag
    this
  }

  def withExCouponPeriod(period: Period,
                         calendar: Calendar,
                         convention: BusinessDayConvention,
                         endOfMonth: Boolean): FixedRateLegBuilder = {
    exCouponPeriod = Some(ExCouponPeriod(period, calendar, convention, endOfMonth))
    this
  }

  def build(): Leg = {
    require(couponRates.nonEmpty, "no coupon rates given")
    require(notionals.nonEmpty, "no notional given")

    val leg = new mutable.ArrayBuffer[CashFlow](schedule.size)

    val paymentCalendar = paymentCalendarOption match {
      case Some(calendar) => calendar
      case None => schedule.calendar
    }

    // first period might be short or long
    leg += buildFirstCoupon(paymentCalendar)

    // regular periods
    for( i <- 2 until schedule.size) {
      leg += buildRegularCoupon(i, paymentCalendar)
    }

    if (schedule.size > 2) {
      // last period might be short or long
      leg += buildLastCoupon(paymentCalendar)
    }
    leg
  }

  private def buildFirstCoupon(paymentCalendar: Calendar): FixedRateCoupon = {
    val start = schedule(0)
    val end = schedule(1)
    val paymentDate = paymentCalendar.advance(end, paymentLag, ChronoUnit.DAYS, paymentAdjustment)
    val rate: InterestRate = couponRates.head
    val nominal = notionals.head

    val exCouponDate = exCouponPeriod.map(ex => ex.adjust(paymentDate))

    val ref = if (schedule.hasTenor && schedule.hasIsRegular && !schedule.isRegular.get.apply(1))
      schedule.calendar.advance(end, schedule.tenor.get.negated, schedule.convention, schedule.endOfMonth.get)
    else
      start

    val firstPeriodDC = firstPeriodDCOption.getOrElse(rate.dayCounter)
    val r: InterestRate = new InterestRate(rate.rate, firstPeriodDC, rate.compounding, rate.frequency)
    new FixedRateCoupon(paymentDate, nominal, r, start, end, Some(ref), Some(end), exCouponDate)
  }

  def buildRegularCoupon(i: Int, paymentCalendar: Calendar): FixedRateCoupon = {
    val start = schedule(i-1)
    val end = schedule(i)

    val paymentDate = paymentCalendar.advance(end, paymentLag, ChronoUnit.DAYS, paymentAdjustment)
    val exCouponDate = exCouponPeriod.map(ex => ex.adjust(paymentDate))

    val rate = if ((i-1) < couponRates.size)
      couponRates(i-1)
    else
      couponRates.last

    val nominal = if ((i-1) < notionals.size)
      notionals(i-1)
    else
      notionals.last

    new FixedRateCoupon(paymentDate, nominal, rate, start, end, Some(start), Some(end), exCouponDate)
  }

  def buildLastCoupon(paymentCalendar: Calendar): FixedRateCoupon = {
    // last period might be short or long
    val N = schedule.size
    val start = schedule(N-2)
    val end = schedule(N-1)
    val paymentDate = paymentCalendar.advance(end, paymentLag, ChronoUnit.DAYS, paymentAdjustment)

    val exCouponDate = exCouponPeriod.map(ex => ex.adjust(paymentDate))

    var rate = if ((N-2) < couponRates.size)
      couponRates(N-2)
    else
      couponRates.last

    val nominal = if ((N-2) < notionals.size)
      notionals(N-2)
    else
      notionals.last

    val lastPeriodDC = lastPeriodDCOption.getOrElse(rate.dayCounter)
    val r = InterestRate (rate.rate, lastPeriodDC, rate.compounding, rate.frequency)

    val ref = if ((schedule.hasIsRegular && schedule.isRegular.get.apply(N - 1)) || !schedule.hasTenor) {
      end
    } else {
      schedule.calendar.advance(start, schedule.tenor.get, schedule.convention, schedule.endOfMonth.get)
    }

    FixedRateCoupon(paymentDate, nominal, r, start, end, start, ref, exCouponDate.get)
  }

}