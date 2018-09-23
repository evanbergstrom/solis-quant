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

package com.soliscode.finance.quant

import java.time.LocalDate

import com.soliscode.finance.quant.time.Dates._
import com.soliscode.finance.quant.time.{DayCounter, Frequency}

object InterestRate {

  def apply(rate: Double, dayCounter: DayCounter, compounding: Compounding, frequency: Frequency) =
    new InterestRate(rate, dayCounter, compounding, frequency)

  def impliedRate(compound: Double, dayCounter: DayCounter, compounding: Compounding, freq: Frequency,
                  time: Double): InterestRate = {
    require(compound > 0.0, "positive compound factor required")
    val rate = if (compound == 1.0) {
      require(time >= 0.0, s"non negative time ($time) required")
      0.0
    } else {
      require(time > 0.0, s"positive time ($time) required")
      compounding.implied(compound, time, freq)
    }
    new InterestRate(rate,dayCounter, compounding, freq)
  }

  def impliedRate(compound: Double, dayCounter: DayCounter, compounding: Compounding, freq: Frequency, d1: LocalDate,
                  d2: LocalDate): InterestRate = {
    require(d2 >= d1, "d1 ($d1) later than d2 ($d2)")
    val time = dayCounter.yearFraction(d1, d2)
    impliedRate(compound, dayCounter, compounding, freq, time)
  }

  def impliedRate(compound: Double, dayCounter: DayCounter, compounding: Compounding, freq: Frequency, d1: LocalDate,
                  d2: LocalDate, refStart: LocalDate, refEnd: LocalDate): InterestRate = {
    require(d2 >= d1, "d1 ($d1) later than d2 ($d2)")
    val time = dayCounter.yearFraction(d1, d2, refStart, refEnd)
    impliedRate(compound, dayCounter, compounding, freq, time)
  }
}

class InterestRate(val rate: Double,
                   val dayCounter: DayCounter,
                   val compounding: Compounding,
                   val frequency: Frequency) {

  def discountFactor(time: Double): Double = 1.0 / compoundFactor(time)

  def discountFactor(d1: LocalDate, d2: LocalDate): Double = {
    assert(d2.isNotBefore(d1), s"d1 ($d1) later than d2 ($d2)")
    val t = dayCounter.yearFraction(d1, d2)
    discountFactor(t)
  }

  def discountFactor(d1: LocalDate, d2: LocalDate, refStart: LocalDate, refEnd: LocalDate): Double = {
    assert(d2.isNotBefore(d1), s"d1 ($d1) later than d2 ($d2)")
    val t = dayCounter.yearFraction(d1, d2, refStart, refEnd)
    discountFactor(t)
  }

  /**
    * compound factor implied by the rate compounded at time t.
    *
    * @note Time must be measured using InterestRate's own day counter.
    *
    * @param  time The time to period for the factor
    * @return the compound (a.k.a capitalization) factor implied by the rate compounded at time t.
    */
  def compoundFactor(time: Double): Double = {
    require(time >= 0.0, "negative time ($time) not allowed")
    compounding.factor(rate, time, frequency)
   }

  /**
    * compound factor implied by the rate compounded between two dates
    * @return the compound (a.k.a capitalization) factor implied by the rate compounded between two dates.
   */

  def compoundFactor(d1: LocalDate, d2: LocalDate): Double = {
    require(d2.isNotBefore(d1), s"d1 ($d1) later than d2 ($d2)")
    compounding.factor(rate, dayCounter.yearFraction(d1, d2), frequency)
  }

  def compoundFactor(d1: LocalDate, d2: LocalDate, refStart: LocalDate, refEnd: LocalDate): Double = {
    require(d2.isNotBefore(d1), s"d1 ($d1) later than d2 ($d2)")
    compounding.factor(rate, dayCounter.yearFraction(d1, d2, refStart, refEnd), frequency)
  }

  def equivalentRate(compounding: Compounding, freq: Frequency, time: Double): InterestRate =
    InterestRate.impliedRate(compoundFactor(time), dayCounter, compounding, freq, time)

  def equivalentRate(dayCounter: DayCounter, compounding: Compounding, freq: Frequency,
                     d1: LocalDate, d2: LocalDate): InterestRate =
    equivalentRate(dayCounter, compounding, freq, d1, d2, d1, d2)

    def equivalentRate(dayCounter: DayCounter, compounding: Compounding, freq: Frequency,
                     d1: LocalDate, d2: LocalDate, refStart: LocalDate, refEnd: LocalDate): InterestRate = {
    require(d2 >= d1, s"d1 ($d1) later than d2 ($d2)")
    val t1 = dayCounter.yearFraction(d1, d2, refStart, refEnd)
    val t2 = dayCounter.yearFraction(d1, d2, refStart, refEnd)
    InterestRate.impliedRate(compoundFactor(t1), dayCounter, compounding, freq, t2)
  }
}
