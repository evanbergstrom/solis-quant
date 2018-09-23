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

import java.time.{LocalDate, Period}

// TODO: Eliminate or redesign this class

object DateIterator {

  def apply(from: LocalDate, to: LocalDate): Iterator[LocalDate] = {
    Iterator.iterate(from)(_ plusDays 1).takeWhile(d => d.isBefore(to))
  }

  def withPeriod(from: LocalDate, to: LocalDate, per: Period): Iterator[LocalDate] = {
    Iterator.iterate(from)(_.plus(per)).takeWhile(d => if (per.isNegative) d.isAfter(to) else d.isBefore(to))
  }
}

