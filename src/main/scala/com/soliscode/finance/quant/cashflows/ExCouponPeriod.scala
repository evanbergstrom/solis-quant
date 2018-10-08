/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a
 * free-software/open-source library for financial quantitative analysts and developers
 * (http://quantlib.org/) to Scala. The basic structure and design of the library has been
 * preserved, but thebnaming conventions, basic types, collection classes and implementation
 * have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.cashflows

import java.time.{LocalDate, Period}

import com.soliscode.finance.quant.time.{BusinessDayConvention, Calendar}

object ExCouponPeriod {
  def apply(period: Period, calendar: Calendar, adjustment: BusinessDayConvention, endOfMonth: Boolean): ExCouponPeriod =
    new ExCouponPeriod(period, calendar, adjustment, endOfMonth)
}

class ExCouponPeriod(val period: Period,
                     val calendar: Calendar,
                     val adjustment: BusinessDayConvention,
                     val endOfMonth: Boolean) {

  def adjust(paymentDate: LocalDate): LocalDate =
    calendar.advance(paymentDate, period.negated(), adjustment, endOfMonth)
}
