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

import com.soliscode.finance.quant.Event

object CashFlow {
  type Leg = Seq[CashFlow]
}

trait CashFlow extends Event {

  val date: LocalDate
  def amount : Double
  val exCouponDate : Option[LocalDate]

  override def hasOccurred(date: LocalDate, includeRefDate: Boolean = true): Boolean = {
    val now = LocalDate.now()
    if (!date.equals(now))
      date.isBefore(now)
    else
      includeRefDate
  }

  def tradingExCoupon(refDate: LocalDate): Boolean = {
    exCouponDate match {
      case Some(d) => d.isBefore(refDate) || d.isEqual(refDate)
      case None => false
    }
  }

  def willReceive(refDate: LocalDate, includeRefDate: Boolean = true): Boolean =
    !hasOccurred(refDate, includeRefDate) && !tradingExCoupon(refDate)

  def earlierThan(c: CashFlow): Boolean =
    date.isBefore(c.date)
}

