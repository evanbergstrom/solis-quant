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

package com.soliscode.finance.quant.time.daycounters

import java.time.LocalDate

import com.soliscode.finance.quant.time.DayCounter

object Thirty360 {

  sealed trait Convention
  object Usa extends Convention
  object BondBasis extends Convention
  object European extends Convention
  object EuroBondBasis extends Convention
  object Italian extends Convention

  def apply(conv: Convention = BondBasis): DayCounter = conv match {
    case Usa | BondBasis => new Thirty360Us
    case European | EuroBondBasis => new Thirty360Eu
    case Italian => new Thirty360It
  }
}

abstract class Thirty360 extends DayCounter {
  override def yearFraction(start: LocalDate, end: LocalDate,
                            refPeriodStart: LocalDate, refPeriodEnd: LocalDate): Double = {
    dayCount(start, end) / 360.0
  }
}

class Thirty360Us extends Thirty360 {
  override def name: String = "30/360 (Bond Basis)"

  override def dayCount(start: LocalDate, end: LocalDate): Long = {
    var (startDOM, endDOM) = (start.getDayOfMonth, end.getDayOfMonth)
    var (startM, endM) = (start.getMonthValue, end.getMonthValue)
    val (startY, endY) = (start.getYear, end.getYear)

    if (endDOM == 31 && startDOM < 30) {
      endDOM = 1
      endM += 1
    }

    360*(endY-startY) + 30*(endM-startM-1) + Math.max(0, 30-startDOM) + Math.min(30,endDOM)
  }
}


class Thirty360Eu extends Thirty360 {
  override def name: String = "30/360 (Eurobond Basis)"

  override def dayCount(start: LocalDate, end: LocalDate): Long = {
    val (startDOM, endDOM) = (start.getDayOfMonth, end.getDayOfMonth)
    val (startM, endM) = (start.getMonthValue, end.getMonthValue)
    val (startY, endY) = (start.getYear, end.getYear)

    360*(endY-startY) + 30*(endM-startM-1) + Math.max(0,30-startDOM) + Math.max(30,endDOM)
  }
}

class Thirty360It extends Thirty360 {
  override def name: String = "30/360 (Italian)"

  override def dayCount(start: LocalDate, end: LocalDate): Long = {
    var (startDOM, endDOM) = (start.getDayOfMonth, end.getDayOfMonth)
    val (startM, endM) = (start.getMonthValue, end.getMonthValue)
    val (startY, endY) = (start.getYear, end.getYear)

    if (startM == 2 && startDOM > 27) startDOM = 30
    if (endM == 2 && endDOM > 27) endDOM = 30

    360*(endY-startY) + 30*(endM-startM-1) + Math.max(0,30-startDOM) + Math.max(30,endDOM)
  }
}

