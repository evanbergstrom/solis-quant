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

package com.soliscode.finance.quant.time

import java.time.Period

import scala.language.implicitConversions

object Frequency {
  implicit def freqeuncyToInt(freq: Frequency): Int = freq.toInt

}

sealed trait Frequency {
  def toInt: Int
  def toPeriod: Period
}

case object NoFrequency extends Frequency {
  private val ZERO_DAYS: Period = Period.ofDays(0)

  def toInt: Int = -1
  def toPeriod: Period = ZERO_DAYS
}

case object Once extends Frequency {
  private val ZERO_YEARS: Period = Period.ofYears(0)

  def toInt: Int = 0
  def toPeriod: Period = ZERO_YEARS
}

case object Annual extends Frequency {
  private val ONE_YEAR: Period = Period.ofYears(1)

  def toInt: Int = 1
  def toPeriod: Period = ONE_YEAR
}

case object Semiannual extends Frequency {
  private val SIX_MONTHS: Period = Period.ofMonths(6)

  def toInt: Int = 2
  def toPeriod: Period = SIX_MONTHS
}

case object EveryFourthMonth extends Frequency {
  private val FOUR_MONTHS: Period = Period.ofMonths(4)

  def toInt: Int = 3
  def toPeriod: Period = FOUR_MONTHS
}

case object Quarterly extends Frequency {
  private val THREE_MONTHS: Period = Period.ofMonths(3)

  def toInt: Int = 4
  def toPeriod: Period = THREE_MONTHS
}

case object Bimonthly extends Frequency {
  private val TWO_MONTHS: Period = Period.ofMonths(3)

  def toInt: Int = 6
  def toPeriod: Period = TWO_MONTHS
}

case object Monthly extends Frequency {
  private val ONE_MONTH: Period = Period.ofMonths(1)

  def toInt: Int = 12
  def toPeriod: Period = ONE_MONTH
}

case object EveryFourthWeek extends Frequency {
  private val FOUR_WEEKS: Period = Period.ofWeeks(4)

  def toInt: Int = 13
  def toPeriod: Period = FOUR_WEEKS
}

case object Biweekly extends Frequency {
  private val TWO_WEEKS: Period = Period.ofWeeks(2)

  def toInt: Int = 26
  def toPeriod: Period = TWO_WEEKS
}

case object Weekly extends Frequency {
  private val ONE_WEEK: Period = Period.ofWeeks(1)

  def toInt: Int = 52
  def toPeriod: Period = ONE_WEEK
}

case object Daily extends Frequency {
  private val ONE_DAY: Period = Period.ofDays(1)

  def toInt: Int = 365
  def toPeriod: Period = ONE_DAY
}

case object OtherFrequency extends Frequency {
  def toInt: Int = 999
  def toPeriod: Period = Period.ZERO
}
