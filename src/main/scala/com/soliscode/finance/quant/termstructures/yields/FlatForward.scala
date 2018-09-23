/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 Copyright (C) 2018 Evan Bergstrom

This file is a port of QuantLib, a free-software/open-source library for financial quantitative analysts and
developers (http://quantlib.org/) to Scala. The basic structure and design of the library has been preserved, but the
naming conventions, basic types, collection classes and implementation have been modified to support common Scala
idioms.

See the full license in the license file (LICENSE.txt)
*/

package com.soliscode.finance.quant.termstructures.yields

import java.time.LocalDate

import com.soliscode.finance.quant.quotes.SimpleQuote
import com.soliscode.finance.quant.termstructures.YieldTermStructure
import com.soliscode.finance.quant.time._
import com.soliscode.finance.quant.{Compounding, Continuous, InterestRate, Quote}

object FlatForward {

  // Quote as the forward rate
  def apply(referenceDate: LocalDate, forward: Quote, dayCounter: DayCounter) : FlatForward =
    new FlatForward(referenceDate, forward, dayCounter, NullCalendar, Continuous, Annual)

  def apply(referenceDate: LocalDate, forward: Quote, dayCounter: DayCounter, compounding: Compounding,
            frequency: Frequency) : FlatForward =
    new FlatForward(referenceDate, forward, dayCounter, NullCalendar, compounding, frequency)

  // Double as the forward rate
  def apply(referenceDate: LocalDate, forward: Double, dayCounter: DayCounter): FlatForward =
    new FlatForward(referenceDate, SimpleQuote(forward), dayCounter, NullCalendar, Continuous, Annual)

  def apply(referenceDate: LocalDate, forward: Double, dayCounter: DayCounter, compounding: Compounding,
            frequency: Frequency): FlatForward =
    new FlatForward(referenceDate, SimpleQuote(forward), dayCounter, NullCalendar, compounding, frequency)

//  // Days from today as the reference date, Quote as the forward rate
//  def apply(settlementDays: Integer, calendar: Calendar, forward: Quote, dayCounter:  DayCounter): FlatForward =
//    new FlatForward(calendar.advance(LocalDate.now(), settlementDays, ChronoUnit.DAYS), forward, dayCounter, calendar,
//      Continuous, Annual)
//
//  def apply(settlementDays: Integer, calendar: Calendar, forward: Quote, dayCounter:  DayCounter,
//            compounding: Compounding  = Continuous, frequency: Frequency = Annual): FlatForward =
//    new FlatForward(calendar.advance(LocalDate.now(), settlementDays, ChronoUnit.DAYS), forward, dayCounter, calendar,
//      compounding, frequency)

//  // Days from today as the reference date, Double as the forward rate
//  def apply(settlementDays: Integer, calendar: Calendar, forward: Double, dayCounter: DayCounter): FlatForward =
//    new FlatForward(calendar.advance(LocalDate.now(), settlementDays, ChronoUnit.DAYS), SimpleQuote(forward),
//                    dayCounter, calendar, Continuous, Annual)
//
//  def apply(settlementDays: Integer, calendar: Calendar, forward: Double, dayCounter: DayCounter,
//            compounding: Compounding = Continuous, frequency: Frequency = Annual): FlatForward =
//    new FlatForward(calendar.advance(LocalDate.now(), settlementDays, ChronoUnit.DAYS), SimpleQuote(forward),
//      dayCounter, calendar, compounding, frequency)

}


class FlatForward(referenceDate: LocalDate, forward: Quote, dayCounter: DayCounter,
                  calendar: Calendar, comp: Compounding, freq: Frequency)
  extends YieldTermStructure(referenceDate, calendar, dayCounter, List(), List()) {

  val rate = InterestRate(forward.value, dayCounter, comp, freq)

  override protected def discountImpl(time: Double): Double = rate.discountFactor(time)

  override def maxDate: LocalDate = LocalDate.MAX

  override def maxTime: Double = timeFromReference(LocalDate.MAX)
}
