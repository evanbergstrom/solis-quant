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

package com.soliscode.finance.quant.termstructures

import java.time.LocalDate

import com.soliscode.finance.quant.math.interpolations.Extrapolator
import com.soliscode.finance.quant.quotes.Quote
import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.time.{Annual, Calendar, DayCounter, Frequency}
import com.soliscode.finance.quant.{Compounding, InterestRate}

object YieldTermStructure {
  def annualJumpDates(referenceDate: LocalDate, nJumps: Int): List[LocalDate] =
    Range(0,nJumps).map(i => referenceDate.endOfYear(i)).toList
}

abstract class YieldTermStructure(override val referenceDate: LocalDate,
                                  override val calendar: Calendar,
                                  override val dayCounter: DayCounter,
                                  val jumps: List[Quote],
                                  val jumpDates: List[LocalDate])
  extends TermStructure(referenceDate, calendar, dayCounter) with Extrapolator {

  private val dt = 0.0001

  override var allowsExtrapolation: Boolean = false

  val jumpTimes: List[Double] = jumpDates.map(d => timeFromReference(d))

  /**
    * This method must be implemented in derived classes to perform the actual calculations. When it is called, range
    * check has already been performed; therefore, it must assume that extrapolation is required.
    */
  protected def discountImpl(time: Double): Double

  def discount(date: LocalDate): Double =
    discount(timeFromReference(date))

  def discount(date: LocalDate, extrapolate: Boolean): Double =
    discount(timeFromReference(date), extrapolate)

  def discount(time: Double): Double =
    discount(time, extrapolate=false)

  def discount(time: Double, extrapolate: Boolean): Double = {
    checkRange(referenceDate, extrapolate)

      val jumpEffect = jumps.foldLeft(1.0)((effect,q) => {
      assert(q.valid, "invalid jump quote")
      assert(q.value > 0.0, "invalid jump value")
      effect * q.value
    })

    jumpEffect * discountImpl(time)
  }

  def zeroRate(date: LocalDate, resultDayCounter: DayCounter, compounding: Compounding): InterestRate =
    zeroRate(date, resultDayCounter, compounding, Annual, extrapolate = false)

  def zeroRate(date: LocalDate, resultDayCounter: DayCounter, compounding: Compounding, frequency: Frequency,
               extrapolate: Boolean): InterestRate =
    if (date == referenceDate) {
      val compound = 1.0/discount(dt, extrapolate)
      // t has been calculated with a possibly different daycounter but the difference should
      // not matter for very small times
      InterestRate.impliedRate(compound, dayCounter, compounding, frequency, dt)
    } else {
      val compound = 1.0 / discount(date, extrapolate)
      InterestRate.impliedRate(compound, dayCounter, compounding, frequency, referenceDate, date)
    }

  // The resulting interest rate has the same day-counting rule used by the term structure. The same rule should be
  // used for calculating the passed time t.

  def zeroRate(time: Double, compounding: Compounding): InterestRate =
    zeroRate(time, compounding, Annual, extrapolate = false)

  def zeroRate(time: Double, compounding: Compounding, frequency: Frequency, extrapolate: Boolean): InterestRate = {
    val t = if (time == 0.0) dt else time
    val compound = 1.0 / discount(t, extrapolate)
    InterestRate.impliedRate(compound, dayCounter, compounding, frequency, t)
  }

  override def checkRange(d: LocalDate, extrapolate: Boolean): Unit = {
    assert(d.isNotBefore(referenceDate), s"date ($d) before reference date ($referenceDate")
    assert(extrapolate || allowsExtrapolation || d.isNotAfter(maxDate), s"date ($d) is past max curve date ($maxDate)")
  }
}
