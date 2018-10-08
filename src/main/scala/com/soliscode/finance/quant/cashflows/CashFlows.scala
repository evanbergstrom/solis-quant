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

import com.soliscode.finance.quant._
import com.soliscode.finance.quant.math.{FunctionD0, FunctionD1, Solver1D}
import com.soliscode.finance.quant.math.solvers1d.{Brent, NewtonSafe}
import com.soliscode.finance.quant.patterns.{Visitable, Visitor}
import com.soliscode.finance.quant.quotes.SimpleQuote
import com.soliscode.finance.quant.termstructures.YieldTermStructure
import com.soliscode.finance.quant.termstructures.yields.{FlatForward, ZeroSpreadedTermStructure}
import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.time.{DayCounter, Frequency}

object CashFlows {
  private val BASIS_POINT: Double = 1.0e-4

  import CashFlow._

  def startDate(leg: Leg): LocalDate = {
    require(leg.nonEmpty, "empty leg")
    leg.foldLeft(LocalDate.MAX)((d, cf) => {
      cf match {
        case cp: Coupon => min(d, cp.accrualStartDate)
        case _ => min(d, cf.date)
      }
    })
  }

  def maturityDate(leg: Leg): LocalDate = {
    require(leg.nonEmpty, "empty leg")
    leg.foldLeft(LocalDate.MIN)((d, cf) => {
      cf match {
        case cp: Coupon => max(d, cp.accrualEndDate)
        case _ => max(d, cf.date)
      }
    })
  }

  def isExpired(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Boolean =
    leg.isEmpty || !leg.reverse.exists(cf => !cf.hasOccurred(settlementDate, includeSettlementDateFlows))

  def previousCashFlow(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[CashFlow] =
    leg.reverse.find(cf => cf.hasOccurred(settlementDate, includeSettlementDateFlows))

  def previousCashFlows(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Seq[CashFlow] = {
    val flows = leg.reverse.dropWhile(cf => !cf.hasOccurred(settlementDate, includeSettlementDateFlows))
    flows.headOption match {
      case Some(head) => flows.takeWhile(cf => cf.date.isEqual(head.date))
      case None => Seq.empty[CashFlow]
    }
  }

  def previousCoupon(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[Coupon] =
    previousCashFlows(leg, includeSettlementDateFlows, settlementDate).collectFirst { case cp: Coupon => cp }

  def nextCashFlow(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[CashFlow] =
    leg.find(cf => !cf.hasOccurred(settlementDate, includeSettlementDateFlows))

  def nextCashFlows(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Seq[CashFlow] = {
    val flows = leg.dropWhile(cf => cf.hasOccurred(settlementDate, includeSettlementDateFlows))
    flows.headOption match {
      case Some(head) => flows.takeWhile(cf => cf.date.isEqual(head.date))
      case None => Seq.empty[CashFlow]
    }
  }

  def nextCoupon(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[Coupon] =
    nextCashFlows(leg, includeSettlementDateFlows, settlementDate).collectFirst { case cp: Coupon => cp }

  def previousCashFlowDate(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[LocalDate] =
    previousCashFlow(leg, includeSettlementDateFlows, settlementDate).map(cf => cf.date)

  def nextCashFlowDate(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[LocalDate] =
    nextCashFlow(leg, includeSettlementDateFlows, settlementDate).map(cf => cf.date)

  def previousCashFlowAmount(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Double =
    previousCashFlows(leg, includeSettlementDateFlows, settlementDate).map(cf => cf.amount).sum

  def nextCashFlowAmount(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Double =
    nextCashFlows(leg, includeSettlementDateFlows, settlementDate).map(cf => cf.amount).sum

  def aggregateRate(cashflows: Seq[CashFlow]): Double = {
    var firstOption: Option[Coupon] = None
    cashflows.collect { case cp: Coupon => cp }.map(cp => {
      firstOption match {
        case Some(first) => assert(cp.compatibleWith(first), s"cannot aggregate two different coupons on ${first.date}")
        case None => firstOption = Some(cp)
      }
      cp.rate()
    }).sum
  }

  def previousCouponRate(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Double =
    aggregateRate(previousCashFlows(leg, includeSettlementDateFlows, settlementDate))

  def nextCouponRate(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Double =
    aggregateRate(nextCashFlows(leg, includeSettlementDateFlows, settlementDate))

  def nominal(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Double =
    nextCoupon(leg, includeSettlementDateFlows, settlementDate).map(cp => cp.nominal).getOrElse[Double](0.0)

  def accrualStartDate(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[LocalDate] =
    nextCoupon(leg, includeSettlementDateFlows, settlementDate).map(cp => cp.accrualStartDate)

  def accrualEndDate(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[LocalDate] =
    nextCoupon(leg, includeSettlementDateFlows, settlementDate).map(cp => cp.accrualEndDate)

  def referencePeriodStart(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[LocalDate] =
    nextCoupon(leg, includeSettlementDateFlows, settlementDate).map(cp => cp.refPeriodStart)

  def referencePeriodEnd(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Option[LocalDate] =
    nextCoupon(leg, includeSettlementDateFlows, settlementDate).map(cp => cp.refPeriodEnd)

  def accrualPeriod(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Double =
    nextCoupon(leg, includeSettlementDateFlows, settlementDate).map(cp => cp.accrualPeriod).getOrElse[Double](0.0)

  def accrualDays(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Long =
    nextCoupon(leg, includeSettlementDateFlows, settlementDate).map(cp => cp.accrualDays).getOrElse[Long](0)

  def accruedPeriod(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Double =
    nextCoupon(leg, includeSettlementDateFlows, settlementDate).map(cp => cp.accruedPeriod(settlementDate)).getOrElse[Double](0.0)

  def accruedDays(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Long =
    nextCoupon(leg, includeSettlementDateFlows, settlementDate).map(cp => cp.accruedDays(settlementDate)).getOrElse[Long](0)

  def accruedAmount(leg: Leg, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Double =
    nextCashFlows(leg, includeSettlementDateFlows, settlementDate)
      .collect { case cp: Coupon => cp.accruedAmount(settlementDate) }.sum

  def netPresentValue(leg: Leg, discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean, settlementDate: LocalDate): Double =
    netPresentValue(leg, discountCurve, includeSettlementDateFlows, settlementDate, settlementDate)

  def netPresentValue(leg: Leg, discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean, settlementDate: LocalDate,
          npvDate: LocalDate): Double = {

    val totalNPV = leg.filter(cf => cf.willReceive(settlementDate, includeSettlementDateFlows))
      .map(cf => cf.amount * discountCurve.discount(cf.date)).sum

    totalNPV / discountCurve.discount(npvDate)
  }

  private class BPSCalculator(discountCurve: YieldTermStructure) extends Visitor {
    var bps: Double = 0.0
    var nonSensNPV: Double = 0.0

    override def visit(v: Visitable): Unit = v match {
      case cp: Coupon => bps += (cp.nominal * cp.accrualPeriod * discountCurve.discount(cp.date))
      case cf: CashFlow => nonSensNPV += (cf.amount * discountCurve.discount(cf.date))
      case _ =>
    }
  }

  def basisPointSensitivity(leg: Leg, discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean,
                            settlementDate: LocalDate): Double =
    basisPointSensitivity(leg, discountCurve, includeSettlementDateFlows, settlementDate, settlementDate)

  def basisPointSensitivity(leg: Leg, discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean,
                            settlementDate: LocalDate, npvDate: LocalDate): Double = {
    if (leg.isEmpty) return 0.0
    val calc = new BPSCalculator(discountCurve)
    leg.filter(cf => cf.willReceive(settlementDate, includeSettlementDateFlows)).foreach(cf => cf.accept(calc))
    BASIS_POINT * calc.bps / discountCurve.discount(npvDate)
  }

  def npvbps(leg: Leg, discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean,
             settlementDate: LocalDate, npvDate: LocalDate): (Double, Double) = {

    if (leg.isEmpty) return (0.0, 0.0)

    var npv, bps = 0.0
    leg.filter(cf => cf.willReceive(settlementDate, includeSettlementDateFlows))
      .foreach(cf => {
        val df = discountCurve.discount(settlementDate)
        npv += cf.amount * df
        cf match {
          case cp: Coupon => bps += cp.nominal * cp.accrualPeriod * df
        }
      })

    val df = discountCurve.discount(npvDate)
    (npv / df, BASIS_POINT * bps / df)
  }

  def atmRate(leg: Leg, discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean,
              settlementDate: LocalDate): Double =
    atmRate(leg, discountCurve, includeSettlementDateFlows, settlementDate, settlementDate)

  def atmRate(leg: Leg, discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean,
              settlementDate: LocalDate, npvDate: LocalDate): Double = {

    var npv = 0.0
    val calc = new BPSCalculator(discountCurve)
    leg.filter(cf => cf.willReceive(settlementDate, includeSettlementDateFlows)).foreach(cf => {
      npv += cf.amount * discountCurve.discount(cf.date)
      cf.accept(calc)
    })

    val targetNpv = npv - calc.nonSensNPV
    if (targetNpv == 0.0)
      0.0
    else {
      assert(calc.bps != 0.0, "null bps: impossible atm rate")
      targetNpv / calc.bps
    }
  }

  def atmRate(leg: Leg, discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean,
              settlementDate: LocalDate, targetNpv: Double): Double =
    atmRate(leg, discountCurve, includeSettlementDateFlows, settlementDate, settlementDate, targetNpv)

  def atmRate(leg: Leg, discountCurve: YieldTermStructure, includeSettlementDateFlows: Boolean,
              settlementDate: LocalDate, npvDate: LocalDate, targetNpv: Double): Double = {

    val calc = new BPSCalculator(discountCurve)
    leg.filter(cf => cf.willReceive(settlementDate, includeSettlementDateFlows)).foreach(cf => cf.accept(calc))

    val npv = targetNpv * discountCurve.discount(npvDate) - calc.nonSensNPV
    if (npv == 0.0)
      0.0
    else {
      assert(calc.bps != 0.0, "null bps: impossible atm rate")
      npv / calc.bps
    }
  }

  private def sign(x: Double): Int = x match {
    case 0 => 0
    case i if i < 0 => -1
    case i if i > 0 => 1
  }

  // helper function used to calculate Time-To-Discount for each stage when calculating discount factor stepwisely
  private def getStepwiseDiscountTime(cf: CashFlow, dayCounter: DayCounter, npvDate: LocalDate, lastDate: LocalDate): Double = {
    val (refStartDate, refEndDate) = cf match {
      case cp: Coupon => (cp.refPeriodStart, cp.refPeriodEnd)
      case _ if lastDate == npvDate => (cf.date.minusYears(1), cf.date)
      case _ => (lastDate, cf.date)
    }

    cf match {
      case cp: Coupon if lastDate != cp.accrualStartDate =>
        val couponPeriod = dayCounter.yearFraction(cp.accrualStartDate, cp.date, refStartDate, refEndDate)
        val accruedPeriod = dayCounter.yearFraction(cp.accrualStartDate, lastDate, refStartDate, refEndDate)
        couponPeriod - accruedPeriod;
      case _ =>
        dayCounter.yearFraction(lastDate, cf.date, refStartDate, refEndDate)
    }
  }

  private def simpleDuration(leg: Leg, y: InterestRate, includeSettlementDateFlows: Boolean,
                             settlementDate: LocalDate, npvDate: LocalDate): Double = {
    if (leg.isEmpty)
      return 0.0

    var P = 0.0
    var dPdy = 0.0
    var t = 0.0
    var lastDate = npvDate

    for (cf <- leg) {
      if (!cf.hasOccurred(settlementDate, includeSettlementDateFlows)) {
        t += getStepwiseDiscountTime(cf, y.dayCounter, npvDate, lastDate)
        val B: Double = y.discountFactor(t)

        if (!cf.tradingExCoupon(settlementDate)) {
          P += cf.amount * B
          dPdy += t * cf.amount * B
        }

        lastDate = cf.date
      }
    }

    if (P == 0.0) 0.0 else dPdy / P
  }

  private def modifiedDuration(leg: Leg, y: InterestRate, includeSettlementDateFlows: Boolean,
                               settlementDate: LocalDate, npvDate: LocalDate): Double = {
    if (leg.isEmpty) return 0.0

    var P, t, dPdy = 0.0
    var lastDate = npvDate

    for (cf <- leg) {
      if (!cf.hasOccurred(settlementDate, includeSettlementDateFlows)) {
        t += getStepwiseDiscountTime(cf, y.dayCounter, npvDate, lastDate)
        if (!cf.tradingExCoupon(settlementDate)) {
          P += cf.amount * y.discountFactor(t)
          dPdy -= y.compounding.dPdy(cf.amount, y, t)
        }
        lastDate = cf.date
      }
    }
    if (P == 0.0) 0.0 else -dPdy / P // reverse derivative sign
  }

  private def macaulayDuration(leg: Leg, y: InterestRate, includeSettlementDateFlows: Boolean,
                               settlementDate: LocalDate, npvDate: LocalDate): Double = {
    if (leg.isEmpty) return 0.0
    require(y.compounding == Compounded, "compounded rate required")
    (1.0 + y.rate / y.frequency.toInt) * modifiedDuration(leg, y, includeSettlementDateFlows, settlementDate, npvDate)
  }

  private class IrrFinder(leg: Leg, npv: Double, dayCounter: DayCounter, comp: Compounding, freq: Frequency,
                          includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate)
    extends FunctionD1 {

    override def apply(x: Double): Double = {
      val yieldX = InterestRate(x, dayCounter, comp, freq)
      npv - CashFlows.netPresentValue(leg, yieldX, includeSettlementDateFlows, settlementDate, npvDate)
    }

    override def derivative(x: Double): Double = {
      val yieldX = InterestRate(x, dayCounter, comp, freq)
      modifiedDuration(leg, yieldX, includeSettlementDateFlows, settlementDate, npvDate)
    }

    def checkSign(): Unit = {
      // TODO: A simple search of the cashflows should be enough to check that IRR will make sense.

      // depending on the sign of the market price, check that cash flows of the opposite sign have been specified
      // (otherwise IRR is nonsensical.)
      var lastSign = sign(-npv)
      var signChanges = 0

      for (cf <- leg) {
        if (!cf.hasOccurred(settlementDate, includeSettlementDateFlows) && !cf.tradingExCoupon(settlementDate)) {
          val thisSign = sign(cf.amount)
          if (lastSign * thisSign < 0) // sign changes
            signChanges += 1

          if (thisSign != 0)
            lastSign = thisSign
        }
      }

      assert(signChanges > 0, "the given cash flows cannot result in the given market price due to their sign")
    }
  }

  def netPresentValue(leg: Leg, y: InterestRate, includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate): Double = {
    var discount = 1.0
    var lastDate = npvDate

    leg.filter(cf => !cf.hasOccurred(settlementDate, includeSettlementDateFlows))
      .foldLeft(0.0)((npv, cf) => {
        val amount = if (cf.tradingExCoupon(settlementDate)) 0.0 else cf.amount
        val factor = y.discountFactor(getStepwiseDiscountTime(cf, y.dayCounter, npvDate, lastDate))
        discount *= factor
        lastDate = cf.date
        npv + (amount * discount)
      })
  }

  def netPresentValue(leg: Leg, y: Double, dayCount: DayCounter, comp: Compounding, freq: Frequency,
          includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate): Unit =
    netPresentValue(leg, InterestRate(y, dayCount, comp, freq), includeSettlementDateFlows, settlementDate, npvDate)

  def basisPointSensitivity(leg: Leg, y: InterestRate, includeSettlementDateFlows: Boolean,
          settlementDate: LocalDate, npvDate: LocalDate): Double = {

    if (leg.isEmpty) return 0.0

    val flatRate = FlatForward(settlementDate, y.rate, y.dayCounter, y.compounding, y.frequency)
    basisPointSensitivity(leg, flatRate, includeSettlementDateFlows, settlementDate, npvDate)
  }

  def basisPointSensitivity(leg: Leg, y: Double, dc: DayCounter, comp: Compounding, freq: Frequency,
          includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate): Double = {
    basisPointSensitivity(leg, InterestRate(y, dc, comp, freq), includeSettlementDateFlows, settlementDate, npvDate)
  }

  def yieldRate(solver: Solver1D, leg: Leg, npv: Double, dayCounter: DayCounter, compounding: Compounding,
                frequency: Frequency, includeSettlementDateFlows: Boolean, settlementDate: LocalDate,
                npvDate: LocalDate): Double =
     yieldRate(solver, leg, npv, dayCounter, compounding, frequency, includeSettlementDateFlows, settlementDate,
               npvDate, 1.0e-10, 0.05)

  def yieldRate(solver: Solver1D, leg: Leg, npv: Double, dayCounter: DayCounter, compounding: Compounding,
                frequency: Frequency, includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate,
                accuracy: Double, guess: Double): Double = {
    val objFunction = new IrrFinder(leg, npv, dayCounter, compounding, frequency, includeSettlementDateFlows,
                                    settlementDate, npvDate)
    solver.solve(objFunction, accuracy, guess, guess / 10.0)
  }

  def yieldRate(leg: Leg, npv: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate): Double =
    yieldRate(leg, npv, dayCounter, compounding, frequency, includeSettlementDateFlows, settlementDate, npvDate,
      1.0e-10, 100, 0.05)

  def yieldRate(leg: Leg, npv: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate,
                accuracy: Double, maxIterations: Long, guess: Double): Double = {
    val solver = NewtonSafe(maxIterations)
    yieldRate(solver, leg, npv, dayCounter, compounding, frequency, includeSettlementDateFlows, settlementDate,
              npvDate, accuracy, guess)
  }

  def duration(leg: Leg, rate: InterestRate, durType: Duration, includeSettlementDateFlows: Boolean,
               settlementDate: LocalDate, npvDate: LocalDate): Double =
    durType match {
      case Simple => simpleDuration(leg, rate, includeSettlementDateFlows, settlementDate, npvDate)
      case Modified => modifiedDuration(leg, rate, includeSettlementDateFlows, settlementDate, npvDate)
      case Macaulay => macaulayDuration(leg, rate, includeSettlementDateFlows, settlementDate, npvDate)
      case _ => throw new IllegalArgumentException("unknown duration type")
    }

  def duration(leg: Leg, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
               durType: Duration, includeSettlementDateFlows: Boolean, settlementDate: LocalDate,
               npvDate: LocalDate): Double =
    duration(leg, InterestRate(rate, dayCounter, compounding, frequency), durType, includeSettlementDateFlows,
             settlementDate, npvDate)

  def convexity(leg: Leg, rate: InterestRate, includeSettlementDateFlows: Boolean, settlementDate: LocalDate,
                npvDate: LocalDate): Double = {

    if (leg.isEmpty) return 0.0

    var t, P, d2Pdy2 = 0.0
    var lastDate = npvDate

    for(cf <- leg) {
      if (!cf.hasOccurred(settlementDate, includeSettlementDateFlows)) {
        t += getStepwiseDiscountTime(cf, rate.dayCounter, npvDate, lastDate)

        if (!cf.tradingExCoupon(settlementDate)) {
          P += cf.amount * rate.discountFactor(t)
          d2Pdy2 += rate.compounding.d2Pdy2(cf.amount, rate, t)
        }
        lastDate = cf.date
      }
    }

    if (P == 0.0) 0.0 else d2Pdy2/P
  }

  def convexity(leg: Leg, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
               includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate): Double =
    convexity(leg, InterestRate(rate, dayCounter, compounding, frequency), includeSettlementDateFlows, settlementDate,
              npvDate)

  def basisPointValue(leg: Leg, rate: InterestRate, includeSettlementDateFlows: Boolean, settlementDate: LocalDate,
                      npvDate: LocalDate): Double = {
    if (leg.isEmpty) return 0.0

    val npv = netPresentValue(leg, rate, includeSettlementDateFlows, settlementDate, npvDate)
    val d = duration(leg, rate, Modified, includeSettlementDateFlows, settlementDate, npvDate)
    val c = convexity(leg, rate, includeSettlementDateFlows, settlementDate, npvDate)
    val shift = 0.0001

    // bpv = delta + 0.5 * gamma   delta = -d * pv * shift   gamma = (c / 100.0) * pv * shift^2
    (-d * npv * shift) + (0.5 * (c / 100.0) * npv * shift * shift)
  }

  def basisPointValue(leg: Leg, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                      includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate): Double =
    basisPointValue(leg, InterestRate(rate, dayCounter, compounding, frequency), includeSettlementDateFlows,
                    settlementDate, npvDate)

  def yieldValueBasisPoint(leg: Leg, rate: InterestRate, includeSettlementDateFlows: Boolean, settlementDate: LocalDate,
                           npvDate: LocalDate): Double = {
    if (leg.isEmpty) return 0.0

    val pv = netPresentValue(leg, rate, includeSettlementDateFlows, settlementDate, npvDate)
    val d = duration(leg, rate, Modified, includeSettlementDateFlows, settlementDate, npvDate)
    val shift = 0.01

    (1.0 / (-pv * d)) * shift
  }

  def yieldValueBasisPoint(leg: Leg, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                           includeSettlementDateFlows: Boolean, settlementDate: LocalDate, npvDate: LocalDate): Double =
    yieldValueBasisPoint(leg, InterestRate(rate, dayCounter, compounding, frequency), includeSettlementDateFlows,
                         settlementDate, npvDate)


  private class ZSpreadFinder(leg: Leg, discountCurve: YieldTermStructure, npv: Double, dayCounter: DayCounter,
                              compounding: Compounding, frequency: Frequency, includeSettlementDateFlows: Boolean,
                              settlementDate: LocalDate, npvDate: LocalDate)
    extends FunctionD0 {

    var zSpread = new SimpleQuote(0.0)
    val curve = new ZeroSpreadedTermStructure(discountCurve, zSpread, compounding, frequency)
     if (discountCurve.allowsExtrapolation) curve.enableExtrapolation() else curve.disableExtrapolation()

    def apply(x: Double): Double = {
      zSpread.value = x
      npv - netPresentValue(leg, curve, includeSettlementDateFlows, settlementDate, npvDate)
    }
  }

  def netPresentValue(leg: Leg, discount: YieldTermStructure, zSpread: Double, dayCounter: DayCounter, compounding: Compounding,
          frequency: Frequency, includeSettlementDateFlows: Boolean, settlementDate: LocalDate,
          npvDate: LocalDate): Double = {

    if (leg.isEmpty) return 0.0

    val zSpreadQuote = SimpleQuote(zSpread)
    val spreadedCurve = new ZeroSpreadedTermStructure(discount, zSpreadQuote, compounding, frequency)
    if (discount.allowsExtrapolation) spreadedCurve.enableExtrapolation() else spreadedCurve.disableExtrapolation()

    netPresentValue(leg, spreadedCurve, includeSettlementDateFlows, settlementDate, npvDate)
  }

  def zSpread(leg: Leg, npv: Double, discount: YieldTermStructure, dayCounter: DayCounter, compounding: Compounding,
              frequency: Frequency, includeSettlementDateFlows: Boolean, settlementDate: LocalDate,
              npvDate: LocalDate, accuracy: Double = 1.0e-10, maxIterations: Long = 100,
              guess: Double = 0.0): Double = {

    // TODO: Figure out why the object apply() methods aren't working properly for Brent solver
    val solver = new Brent(maxIterations)

    val objFunction = new ZSpreadFinder(leg, discount, npv, dayCounter, compounding, frequency,
                                        includeSettlementDateFlows, settlementDate, npvDate)

    solver.solve(objFunction, accuracy, guess, 0.01)
  }
}