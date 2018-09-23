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

import com.soliscode.finance.quant.Instrument
import com.soliscode.finance.quant.cashflows.CashFlow.Leg
import com.soliscode.finance.quant.time.Calendar
import com.soliscode.finance.quant.time.Dates._

object Bond {
  def apply(settlementDays: Int,
            calendar: Calendar,
            issueDate: LocalDate,
            maturityDate: LocalDate,
            coupons: Leg): Bond = {
    new Bond(settlementDays, calendar, 100.0, issueDate, maturityDate, coupons)
  }

  def apply(settlementDays: Int,
            calendar: Calendar,
            faceAmount: Double,
            issueDate: LocalDate,
            maturityDate: LocalDate,
            coupons: Leg): Bond = {
    new Bond(settlementDays, calendar, faceAmount, issueDate, maturityDate, coupons)
  }

  def isTradable(bond: Bond, settlementDate: LocalDate): Boolean =
    bond.notional(settlementDate) != 0.0

//  def startDate(bond: Bond): LocalDate =
//    CashFlows.startDate(bond.cashflows())
//
//  def maturityDate(bond: Bond): LocalDate =
//    CashFlows.maturityDate(bond.cashflows())
//
//  def previousCashflowDate(bond: Bond, settlement: LocalDate): LocalDate = {
//    CashFlows.previousCashFlowDate(bond.cashflows(), false, settlement)
//  }
//
//  def nextCashflowDate(bond: Bond, settlement: LocalDate): LocalDate = {
//    CashFlows.previousCashFlowDate(bond.cashflows(), false, settlement)
//  }

//  def yieldRate(bond: Bond, cleanPrice: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
//                settlementDate: LocalDate, accuracy: Double, maxIterations: Long, guess: Double): Double = {
//    val solver = NewtonSafe(maxIterations)
//    yieldRate(solver, bond, cleanPrice, dayCounter, compounding, frequency, settlementDate, accuracy, guess)
//  }

//  def yieldRate(solver: Solver1D, bond: Bond, cleanPrice: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
//                settlementDate: LocalDate, accuracy: Double, guess: Double): Double = {
//
//    require(isTradable(bond, settlementDate), s"non tradable at $settlementDate (maturity being ${bond.maturityDate()}")
//
//    val dirtyPrice = (cleanPrice + bond.accruedAmount(settlementDate)) / 100.0 / bond.notional(settlementDate)
//
//    CashFlows.yieldRate(solver, bond.cashflows(), dirtyPrice, dayCounter, compounding, frequency, false,
//      settlementDate, settlementDate, accuracy, guess)
//  }
}

class Bond(val settlementDays: Int,
           val calendar: Calendar,
           faceAmount: Double,
           val issueDate: LocalDate,
           val maturityDate: LocalDate,
           coupons: Leg) extends Instrument {

  private val notionalSchedule: List[LocalDate] = List()
  private val notionals : List[Double] = List()

//  def yieldRate(cleanPrice: Double,
//                dc: DayCounter,
//                comp: Compounding,
//                freq: Frequency,
//                settlementDate: LocalDate,
//                accuracy: Double = 1.0e-8,
//                maxEvaluations: Int = 100): Double = {
//    val currentNotional = notional(settlementDate);
//    if (currentNotional == 0.0)
//      0.0
//    else
//      yieldRate(this, cleanPrice, dc, comp, freq, settlementDate, accuracy, maxEvaluations)
//  }

  def notional(date: LocalDate): Double = {

    if (date.isAfter(notionalSchedule.last)) {
      // after maturity
      0.0
    }

    // After the check above, d is between the schedule boundaries.  We search starting from the second notional
    // date, since the first is null.  i is the earliest date which is greater or
    // equal than d.  Its index is greater or equal to 1.
    val i = notionalSchedule.slice(1,notionalSchedule.length).indexWhere(n => date.isNotBefore(n))

    if (date.isBefore(notionalSchedule(i))) {
      // no doubt about what to return
      notionals(i-1)
    } else {
      // d is equal to a redemption date.
      // As per bond conventions, the payment has occurred;
      // the bond already changed notional.
      notionals(i)
    }
  }
}

