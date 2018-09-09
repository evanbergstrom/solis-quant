package com.soliscode.finance.quant.instruments

import java.time.LocalDate

import com.soliscode.finance.quant.Compounding._
import com.soliscode.finance.quant.Instrument
import com.soliscode.finance.quant.cashflows.Cashflow.Leg
import com.soliscode.finance.quant.math.solvers1d.NewtonSafe
import com.soliscode.finance.quant.time.Frequency.Frequency
import com.soliscode.finance.quant.time.{Calendar, DayCounter}
import com.soliscode.finance.quant.time.Dates._

import scala.collection.mutable.ListBuffer

object Bond {
  def apply(settlementDays: Int,
            calendar: Calendar,
            issueDate: LocalDate,
            maturityDate: LocalDate,
            coupons: Leg) : Bond = {
    new Bond(settlementDays, calendar, 100.0, issueDate, maturityDate, coupons)
  }

  def apply(settlementDays: Int,
            calendar: Calendar,
            faceAmount: Double,
            issueDate: LocalDate,
            maturityDate: LocalDate,
            coupons: Leg) : Bond = {
    new Bond(settlementDays, calendar, faceAmount, issueDate, maturityDate, coupons)
  }

  def yieldRate(bond: Bond, cleanPrice: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
                settlement: LocalDate, accuracy: Double, maxIterations: Long, guess: Double): Double = {
    val solver = NewtonSafe(maxIterations)
    0.0 // yieldRate[NewtonSafe](solver, bond, cleanPrice, dayCounter, compounding, frequency, settlement, accuracy, guess)
  }
}

class Bond(val settlementDays: Int,
           val calendar: Calendar,
           faceAmount: Double,
           val issueDate: LocalDate,
           val maturityDate: LocalDate,
           coupons: Leg) extends Instrument {

  private val notionalSchedule: List[LocalDate] = List()
  private val notionals : List[Double] = List()

  def yieldRate(cleanPrice: Double,
                dc: DayCounter,
                comp: Compounding,
                freq: Frequency,
                settlementDate: LocalDate,
                accuracy: Double = 1.0e-8,
                maxEvaluations: Int = 100): Double = {
    val currentNotional = notional(settlementDate);
    if (currentNotional == 0.0)
      0.0
    else
      0.0 // BondFunctions::yield(*this, cleanPrice, dc, comp, freq, settlement, accuracy, maxEvaluations)
  }

  def notional(date: LocalDate): Double = {

    if (date.isAfter(notionalSchedule.last)) {
      // after maturity
      return 0.0
    }

    // After the check above, d is between the schedule boundaries.  We search starting from the second notional
    // date, since the first is null.  i is the earliest date which is greater or
    // equal than d.  Its index is greater or equal to 1.
    val i = notionalSchedule.slice(1,notionalSchedule.length).indexWhere(n => date.isNotBefore(n))

    if (date.isBefore(notionalSchedule(i))) {
      // no doubt about what to return
      return notionals(i-1);
    } else {
      // d is equal to a redemption date.
      // As per bond conventions, the payment has occurred;
      // the bond already changed notional.
      return notionals(i);
    }
  }
}

