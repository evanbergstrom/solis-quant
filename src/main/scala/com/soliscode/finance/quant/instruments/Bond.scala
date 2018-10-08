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

package com.soliscode.finance.quant.instruments

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import com.soliscode.finance.quant.{Compounding, EvaluationContext, InterestRate, PrivateEvaluationContext}
import com.soliscode.finance.quant.cashflows.CashFlow.Leg
import com.soliscode.finance.quant.cashflows.{CashFlows, Duration}
import com.soliscode.finance.quant.instruments.Instrument.Instrument
import com.soliscode.finance.quant.math.Solver1D
import com.soliscode.finance.quant.math.solvers1d.NewtonSafe
import com.soliscode.finance.quant.termstructures.YieldTermStructure
import com.soliscode.finance.quant.time.{Calendar, DayCounter, Frequency}
import com.soliscode.finance.quant.time.Dates._

object Bond {
  def apply(settlementDays: Int,
            calendar: Calendar,
            issueDate: LocalDate,
            maturityDate: LocalDate,
            coupons: Leg): Bond = {
    new Bond(PrivateEvaluationContext(), settlementDays, calendar, 100.0, issueDate, maturityDate, coupons)
  }

  def apply(context: EvaluationContext,
            settlementDays: Int,
            calendar: Calendar,
            issueDate: LocalDate,
            maturityDate: LocalDate,
            coupons: Leg): Bond = {
    new Bond(context, settlementDays, calendar, 100.0, issueDate, maturityDate, coupons)
  }

  def apply(settlementDays: Int,
            calendar: Calendar,
            faceAmount: Double,
            issueDate: LocalDate,
            maturityDate: LocalDate,
            coupons: Leg): Bond = {
    new Bond(PrivateEvaluationContext(), settlementDays, calendar, faceAmount, issueDate, maturityDate, coupons)
  }

  def apply(context: EvaluationContext,
            settlementDays: Int,
            calendar: Calendar,
            faceAmount: Double,
            issueDate: LocalDate,
            maturityDate: LocalDate,
            coupons: Leg): Bond = {
    new Bond(context, settlementDays, calendar, faceAmount, issueDate, maturityDate, coupons)
  }

  def startDate(bond: Bond): LocalDate =
    CashFlows.startDate(bond.cashflows)

  def maturityDate(bond: Bond): LocalDate =
    CashFlows.maturityDate(bond.cashflows)

  def isTradable(bond: Bond, settlementDate: LocalDate): Boolean =
    bond.notional(settlementDate) != 0.0

  def previousCashFlowDate(bond: Bond, settlement: LocalDate): Option[LocalDate] =
    CashFlows.previousCashFlowDate(bond.cashflows, includeSettlementDateFlows = false, settlement)

  def nextCashFlowDate(bond: Bond, settlement: LocalDate): Option[LocalDate] =
    CashFlows.previousCashFlowDate(bond.cashflows, includeSettlementDateFlows = false, settlement)

  def previousCashFlowAmount(bond: Bond, settlement: LocalDate): Double =
    CashFlows.previousCashFlowAmount(bond.cashflows, includeSettlementDateFlows = false, settlement)

  def nextCashFlowAmount(bond: Bond, settlement: LocalDate): Double =
    CashFlows.nextCashFlowAmount(bond.cashflows, includeSettlementDateFlows = false, settlement)

  def previousCouponRate(bond: Bond, settlement: LocalDate): Double =
    CashFlows.previousCouponRate(bond.cashflows, includeSettlementDateFlows = false, settlement)

  def nextCouponRate(bond: Bond, settlement: LocalDate): Double =
    CashFlows.nextCouponRate(bond.cashflows, includeSettlementDateFlows = false, settlement)

  def yieldRate(bond: Bond, cleanPrice: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                settlementDate: LocalDate, accuracy: Double, maxIterations: Long, guess: Double): Double = {
    val solver = NewtonSafe(maxIterations)
    yieldRate(solver, bond, cleanPrice, dayCounter, compounding, frequency, settlementDate, accuracy, guess)
  }

  def yieldRate(solver: Solver1D, bond: Bond, cleanPrice: Double, dayCounter: DayCounter, compounding: Compounding,
                frequency: Frequency, settlementDate: LocalDate, accuracy: Double, guess: Double): Double = {
    require(isTradable(bond, settlementDate), s"non tradable at $settlementDate (maturity being ${bond.maturityDate}")
    val dirtyPrice = (cleanPrice + bond.accruedAmount(settlementDate)) / 100.0 / bond.notional(settlementDate)
    CashFlows.yieldRate(solver, bond.cashflows, dirtyPrice, dayCounter, compounding, frequency,
      includeSettlementDateFlows = false, settlementDate, settlementDate, accuracy, guess)
  }

  def accruedAmount(bond: Bond, settlement: LocalDate): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate} )")
    val npv = CashFlows.accruedAmount(bond.cashflows, includeSettlementDateFlows = false, settlement)
    npv * 100.0 / bond.notional(settlement)
  }

  def cleanPrice(bond: Bond, discountCurve: YieldTermStructure, settlement: LocalDate): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate} )")
    val npv = CashFlows.netPresentValue(bond.cashflows, discountCurve, includeSettlementDateFlows = false,
      settlement)
    val dirtyPrice = npv *  100.0 / bond.notional(settlement)
    dirtyPrice - bond.accruedAmount(settlement)
  }

  def cleanPrice(bond: Bond, rate: InterestRate, settlement: LocalDate): Double =
    dirtyPrice(bond, rate, settlement) - bond.accruedAmount(settlement)

  def cleanPrice(bond: Bond, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                 settlement: LocalDate): Double =
    cleanPrice(bond, InterestRate(rate, dayCounter, compounding, frequency), settlement)

  def cleanPrice(bond: Bond, discountCurve: YieldTermStructure, zSpread: Double, dayCounter: DayCounter,
                 compounding: Compounding, frequency: Frequency, settlement: LocalDate): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate} )")

    val npv = CashFlows.netPresentValue(bond.cashflows, discountCurve, zSpread, dayCounter, compounding, frequency,
      includeSettlementDateFlows = false, settlement, settlement)

    val dirtyPrice = npv * 100.0 / bond.notional(settlement)
    dirtyPrice - bond.accruedAmount(settlement)
  }

  def dirtyPrice(bond: Bond, rate: InterestRate, settlement: LocalDate): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate} )")
    val npv = CashFlows.netPresentValue(bond.cashflows, rate, includeSettlementDateFlows = false, settlement, settlement)
    npv * 100.0/ bond.notional(settlement)
  }

  def dirtyPrice(bond: Bond, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                 settlement: LocalDate): Double =
    dirtyPrice(bond, InterestRate(rate, dayCounter, compounding, frequency), settlement)

  def basisPointSensitivity(bond: Bond, discountCurve: YieldTermStructure, settlement: LocalDate): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate} )")
    val npv = CashFlows.basisPointSensitivity(bond.cashflows, discountCurve, includeSettlementDateFlows = false, settlement)
    npv * 100.0 / bond.notional(settlement)
  }

  def basisPointSensitivity(bond: Bond, rate: InterestRate, settlement: LocalDate): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate} )")
    val bps = CashFlows.basisPointSensitivity(bond.cashflows, rate, includeSettlementDateFlows = false, settlement, settlement)
    bps * 100.0 / bond.notional(settlement)
  }

  def basisPointSensitivity(bond: Bond, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
           settlement: LocalDate): Double =
    basisPointSensitivity(bond, InterestRate(rate, dayCounter, compounding, frequency), settlement)

  def atmRate(bond: Bond, discountCurve: YieldTermStructure, settlement: LocalDate, cleanPrice: Double): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate})")

    def dirtyPrice = cleanPrice + bond.accruedAmount(settlement)
    def currentNotional = bond.notional(settlement)
    def npv = dirtyPrice/100.0 * currentNotional

    CashFlows.atmRate(bond.cashflows, discountCurve, includeSettlementDateFlows = false, settlement, settlement, npv)
  }

  def duration(bond: Bond, rate: InterestRate, durType: Duration, settlement: LocalDate): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate})")
    CashFlows.duration(bond.cashflows, rate, durType, includeSettlementDateFlows = false, settlement, settlement)
  }

  def duration(bond: Bond, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
               durType: Duration, settlement: LocalDate): Double =
    duration(bond, InterestRate(rate, dayCounter, compounding, frequency), durType, settlement)

  def convexity(bond: Bond, rate: InterestRate, settlement: LocalDate): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate})")
    CashFlows.convexity(bond.cashflows, rate, includeSettlementDateFlows = false, settlement, settlement)
  }

  def convexity(bond: Bond, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                settlement: LocalDate): Double =
    convexity(bond, InterestRate(rate, dayCounter, compounding, frequency), settlement)

  def basisPointValue(bond: Bond, rate: InterestRate, settlement: LocalDate) {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate})")
    CashFlows.basisPointValue(bond.cashflows, rate, includeSettlementDateFlows = false, settlement, settlement)
  }

  def basisPointValue(bond: Bond, rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                      settlement: LocalDate): Double =
    CashFlows.basisPointValue(bond.cashflows, InterestRate(rate, dayCounter, compounding, frequency),
      includeSettlementDateFlows = false, settlement, settlement)

  def yieldValueBasisPoint(bond: Bond, rate: InterestRate, settlement: LocalDate): Double = {
    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate})")
    CashFlows.yieldValueBasisPoint(bond.cashflows, rate, includeSettlementDateFlows = false, settlement, settlement)
  }

  def yieldValueBasisPoint(bond: Bond, rate: Double, dayCounter: DayCounter, compounding: Compounding,
                           frequency: Frequency, settlement: LocalDate): Double =
    CashFlows.yieldValueBasisPoint(bond.cashflows, InterestRate(rate, dayCounter, compounding, frequency),
      includeSettlementDateFlows = false, settlement, settlement)

  def zSpread(bond: Bond, cleanPrice: Double, discountCurve: YieldTermStructure, dayCounter: DayCounter,
              compounding: Compounding, frequency: Frequency, settlement: LocalDate, accuracy: Double,
              maxIterations: Int, guess: Double): Double = {

    require(isTradable(bond, settlement), s"non tradable at $settlement (maturity being ${bond.maturityDate})")

    val dirtyPrice = (cleanPrice + bond.accruedAmount(settlement)) / 100.0 / bond.notional(settlement)

    CashFlows.zSpread(bond.cashflows, dirtyPrice, discountCurve, dayCounter, compounding, frequency,
      includeSettlementDateFlows = false, settlement, settlement, accuracy, maxIterations, guess)
  }
}

class Bond(evaluationContext: EvaluationContext,
           val settlementDays: Int,
           val calendar: Calendar,
           faceAmount: Double,
           val issueDate: LocalDate,
           val maturityDate: LocalDate,
           coupons: Leg) extends Instrument(evaluationContext) {

  private val notionalSchedule: List[LocalDate] = List()
  private val notionals : List[Double] = List()
  private val cashflows: Leg = coupons   // TODO: Add principal cashflows

  private var settleValue: Option[Double] = None

  def isExpired: Boolean = {
    // this is the Instrument interface, so it doesn't use BondFunctions, and includeSettlementDateFlows is true
    // (unless QL_TODAY_PAYMENTS will set it to false later on)
    CashFlows.isExpired(cashflows, includeSettlementDateFlows = true, valuationDate)
  }

  def notional(date: LocalDate): Double =
    if (date > notionalSchedule.last) {
      0.0  // after maturity
    } else {
      // After the check above, d is between the schedule boundaries.  We search starting from the second notional
      // date, since the first is null.  i is the earliest date which is greater or equal than d.  Its index is
      // greater or equal to 1.
      val i = notionalSchedule.slice(1, notionalSchedule.length).indexWhere(n => date.isNotBefore(n))

      if (date < notionalSchedule(i)) {
        // no doubt about what to return
        notionals(i - 1)
      } else {
        // d is equal to a redemption date. As per bond conventions, the payment has occurred;
        // the bond already changed notional.
        notionals(i)
      }
    }

  def startDate: LocalDate = Bond.startDate(this)

  def isTradable(date: LocalDate): Boolean = Bond.isTradable(this, date)

  def settlementDate(): LocalDate =
    settlementDate(valuationDate)

  def settlementDate(date: LocalDate): LocalDate = {
    // usually, the settlement is at T+n...
    val settlement = calendar.advance(date, settlementDays, ChronoUnit.DAYS)
    // ...but the bond won't be traded until the issue date (if given.)
    max(settlement, issueDate)
  }

  def cleanPrice(): Double =
    dirtyPrice() - accruedAmount(settlementDate())

  def dirtyPrice(): Double = {
    val currentNotional = notional(settlementDate())
    if (notional(settlementDate()) == 0.0) 0.0 else settlementValue()*100.0/currentNotional
  }

  def settlementValue(): Double = {
    calculate()
    settleValue match {
      case Some(v) => v
      case None => throw new RuntimeException("settlement value not provided")
    }
  }

  def settlementValue(cleanPrice: Double): Double = {
    val dirtyPrice = cleanPrice + accruedAmount(settlementDate())
    dirtyPrice / 100.0 * notional(settlementDate())
  }

  def yieldRate(dayCounter: DayCounter, compounding: Compounding, frequency: Frequency, accuracy: Double,
    maxEvaluations: Int): Double = {
    val currentNotional = notional(settlementDate())
    if (currentNotional == 0.0)
      0.0
    else
      Bond.yieldRate(this, cleanPrice(), dayCounter, compounding, frequency, settlementDate(), accuracy,
        maxEvaluations, 0.05)
  }

  def yieldRate(cleanPrice: Double, dc: DayCounter, comp: Compounding, freq: Frequency, settlementDate: LocalDate,
                accuracy: Double = 1.0e-8, maxEvaluations: Int = 100): Double =
    if (notional(settlementDate) == 0.0)
      0.0
    else
      Bond.yieldRate(this, cleanPrice, dc, comp, freq, settlementDate, accuracy, maxEvaluations, 0.05)

  def cleanPrice(rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
    settlement: LocalDate): Double =
    Bond.cleanPrice(this, rate, dayCounter, compounding, frequency, settlement)

  def dirtyPrice(rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                 settlement: LocalDate): Double = {
    val currentNotional = notional(settlement)
    if (currentNotional == 0.0)
      0.0
    else
      Bond.cleanPrice(this, rate, dayCounter, compounding, frequency, settlement) + accruedAmount(settlement)
  }

  def accruedAmount(settlement: LocalDate): Double =
    if (notional(settlement) == 0.0)  0.0  else  Bond.accruedAmount(this, settlement)

  def nextCouponRate(settlement: LocalDate): Double =
    Bond.nextCouponRate(this, settlement)

  def previousCouponRate(settlement: LocalDate): Double =
    Bond.previousCouponRate(this, settlement)

  def nextCashFlowDate(settlement: LocalDate): Option[LocalDate] =
    Bond.nextCashFlowDate(this, settlement)

  def previousCashFlowDate(settlement: LocalDate): Option[LocalDate] =
    Bond.previousCashFlowDate(this, settlement)

  override def setupExpired(): Unit = {
    super.setupExpired()
    settleValue = Some(0.0)
  }
}

