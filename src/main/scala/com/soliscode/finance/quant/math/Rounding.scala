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

package com.soliscode.finance.quant.math

import java.lang.Math._

import com.soliscode.finance.quant.math.Doubles._
import com.soliscode.finance.quant.math.Rounding._

object Rounding {

  def apply(precision: Int, rounding: RoundingType = Closest, digit: Integer = 5): Rounding =
    new Rounding(precision, rounding, digit)

  sealed trait RoundingType

  case object None extends RoundingType
  case object Up extends RoundingType
  case object Down extends RoundingType
  case object Closest extends RoundingType
  case object Floor extends RoundingType
  case object Ceiling extends RoundingType
}

class Rounding(val precision: Int, val rounding: RoundingType = None, val digit: Integer = 5) {
  def apply(value: Double): Double = {

      if (rounding == None)
        return value

      val mult = pow(10.0, precision)
      var lvalue = abs(value) * mult

      val (modVal, integral) = modf(lvalue)
      lvalue -= modVal

      rounding match  {
        case Down =>
        case Up => if (modVal != 0.0) lvalue += 1.0
        case Closest => if (modVal >= (digit/10.0)) lvalue += 1.0
        case Floor => if (value >= 0 && modVal >= (digit / 10.0)) lvalue += 1.0
        case Ceiling => if (value < 0 && modVal >= (digit / 10.0)) lvalue += 1.0
        case _ => new RuntimeException("unknown rounding method")
      }

      if (value < 0) -(lvalue/mult) else lvalue/mult
  }
}

class DownRounding(precision: Int) extends Rounding(precision, Down)

class UpRounding(precision: Int) extends Rounding(precision, Up)

class ClosestRounding(precision: Int) extends Rounding(precision, Closest)

class FloorRounding(precision: Int) extends Rounding(precision, Floor)

class CeilingRounding(precision: Int) extends Rounding(precision, Ceiling)
