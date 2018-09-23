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

import java.lang.Math._

import com.soliscode.finance.quant.time.Frequency

/**
  * Interest rate compounding rules
  */
sealed trait Compounding {

  def factor(rate: Double, time: Double, frequency: Frequency): Double

  def implied(compound: Double, time: Double, frequency: Frequency): Double

  def dPdy(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double

  def d2Pdy2(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double

  def dPdy(amount: Double, rate: InterestRate, time: Double): Double =
    dPdy(amount, rate.rate, time, rate.discountFactor(time), rate.frequency.toInt)

  def d2Pdy2(amount: Double, rate: InterestRate, time: Double): Double =
    d2Pdy2(amount, rate.rate, time, rate.discountFactor(time), rate.frequency.toInt)
}

/* 1+rt */
case object Simple extends Compounding {

  override def factor(rate: Double, time: Double, frequency: Frequency): Double =
    1.0 + rate * time

  override def implied(compound: Double, time: Double, frequency: Frequency): Double =
    (compound - 1.0) / time

  override def dPdy(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    amount * discountFactor * discountFactor * time

  override def d2Pdy2(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    amount * 2.0 * discountFactor * discountFactor * discountFactor * time * time
}

/* (1+r)^t */
case object Compounded extends Compounding {
  override def factor(rate: Double, time: Double, frequency: Frequency): Double =
    pow(1.0 + rate / frequency, frequency * time)

  override def implied(compound: Double, time: Double, frequency: Frequency): Double =
    (pow(compound, 1.0/(frequency * time)) - 1.0) * frequency

  def dPdy(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    amount * time * discountFactor / (1 + rate / frequency)

  def d2Pdy2(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    amount * discountFactor * time * (frequency * time + 1) /(frequency * (1 + rate / frequency) * (1 + rate / frequency))
}

/* e^{rt} */
case object Continuous extends Compounding {
  override def factor(rate: Double, time: Double, frequency: Frequency): Double =
    exp(rate * time)

  override def implied(compound: Double, time: Double, frequency: Frequency): Double =
    log(compound)/time

  def dPdy(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    amount * discountFactor * time

  def d2Pdy2(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    amount * discountFactor * time * time
}

/* Simple up to the first period then Compounded */
case object SimpleThenCompounded extends Compounding {
  override def factor(rate: Double, time: Double, frequency: Frequency): Double =
    if (time <= 1.0 / frequency)
      1.0 + rate * time
    else
      pow(1.0 + rate / frequency, frequency * time)

  override def implied(compound: Double, time: Double, frequency: Frequency): Double =
    if (time <= 1.0 / frequency)
      (compound - 1.0) / time
    else
      (pow(compound, 1.0/(frequency * time))-1.0) * frequency

  def dPdy(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    if (time <= 1.0 / frequency)
      amount * discountFactor * discountFactor * time
    else
      amount * time * discountFactor / (1 + rate / frequency)

  def d2Pdy2(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    if (time <= 1.0 / frequency)
      amount * 2.0 * discountFactor * discountFactor * discountFactor * time * time
    else
      amount * discountFactor * time * (frequency * time + 1) / (frequency * (1 + rate / frequency) * (1 + rate / frequency))
}

/* Compounded up to the first period then Simple */
case object CompoundedThenSimple extends Compounding {
  override def factor(rate: Double, time: Double, frequency: Frequency): Double =
    if (time > 1.0/ frequency)
      1.0 + rate * time
  else
      pow(1.0 + rate /frequency, frequency * time)

  override def implied(compound: Double, time: Double, frequency: Frequency): Double =
    if (time > 1.0/frequency)
      (compound - 1.0) / time
    else
      (pow(compound, 1.0 / (frequency * time)) - 1.0) * frequency

  def dPdy(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    if (time > 1.0 / frequency)
      amount * discountFactor * discountFactor * time
    else
      amount * time * discountFactor / (1 + rate / frequency)

  def d2Pdy2(amount: Double, rate: Double, time: Double, discountFactor: Double, frequency: Int): Double =
    if (time > 1.0 / frequency)
      amount * 2.0 * discountFactor * discountFactor * discountFactor * time * time
    else
      amount * discountFactor * time * (frequency * time + 1) / (frequency * (1 + rate / frequency) * (1 + rate / frequency))
}

