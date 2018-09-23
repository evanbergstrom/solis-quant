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

trait Differentiable {

  def apply(x: Double): Double
  def d(p: Int)(x: Double): Double
  def derivatives(): Int
}

object FunctionD0 {
  def apply(f: Double => Double): Differentiable =
    new FunctionD0 { def apply(x: Double): Double = f(x) }
}

abstract class FunctionD0 extends Differentiable {

  override def d(p: Int)(x: Double): Double = p match {
    case 0 => apply(x)
    case _ => throw new NotImplementedError()
  }

  override def derivatives(): Int = 0
}

object FunctionD1 {
  def apply(f: Double => Double, d1: Double => Double): Differentiable =
    new FunctionD1 {
      def apply(x: Double): Double = f(x)
      def derivative(x: Double): Double = d1(x)
    }
}

abstract class FunctionD1 extends Differentiable {

  def derivative(x: Double): Double

  override def d(p: Int)(x: Double): Double = p match {
    case 0 => apply(x)
    case 1 => derivative(x)
    case _ => throw new NotImplementedError()
  }

  override def derivatives(): Int = 1
}

object FunctionD2 {
  def apply(f: Double => Double, d1: Double => Double, d2: Double => Double): Differentiable =
    new FunctionD2 {
      def apply(x: Double): Double = f(x)
      def derivative(x: Double): Double = d1(x)
      def derivative2(x: Double): Double = d2(x)
    }
}

abstract class FunctionD2 extends Differentiable {

  def derivative(x: Double): Double
  def derivative2(x: Double): Double

  override def d(p: Int)(x: Double): Double = p match {
    case 0 => apply(x)
    case 1 => derivative(x)
    case 2 => derivative2(x)
    case _ => throw new NotImplementedError()
  }

  override def derivatives(): Int = 2
}
