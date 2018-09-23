package com.soliscode.finance.quant.math

import com.soliscode.finance.quant.math.Doubles._
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

import com.soliscode.finance.quant.math.errors.ConvergenceFailureException

import scala.math._

abstract class Solver1D(val maxEvaluations: Long,
                        val lowerBound: Double,
                        val upperBound: Double,
                        val lowerBoundEnforced: Boolean = false,
                        val upperBoundEnforced: Boolean = false) {

  protected var xMax, xMin, fxMax, fxMin, root = 0.0
  protected var evaluationNumber = 0

  private def enforceBounds(x: Double): Double =
    if (lowerBoundEnforced && x < lowerBound)
      lowerBound
    else if (upperBoundEnforced && x > upperBound)
      upperBound
    else
      x

  def solve(f: Differentiable, accuracy: Double, guess: Double, step: Double): Double = {
    require(accuracy > 0.0, s"accuracy ($accuracy) must be positive")

    val eff_accuracy = math.max(accuracy, Double.MinPositiveValue)

    val growthFactor = 1.6
    var flipflop = -1

    root = guess
    fxMax = f(root)

    // monotonically crescent bias, as in optionValue(volatility)
    if (fxMax almost 0.0)
      return root
    else if (fxMax > 0.0) {
      xMin = enforceBounds(root - step)
      fxMin = f(xMin)
      xMax = root
    }
    else {
      xMin = root
      fxMin = fxMax
      xMax = enforceBounds(root + step)
      fxMax = f(xMax)
    }

    evaluationNumber = 2
    while (evaluationNumber <= maxEvaluations) {
      if (fxMin * fxMax <= 0.0) {
        if (fxMin almost 0.0)
          return xMin
        if (fxMax almost 0.0)
          return xMax
        root = (xMax + xMin) / 2.0
        return solveForZero(f, eff_accuracy)
      }
      if (abs(fxMin) < abs(fxMax)) {
        xMin = enforceBounds(xMin + growthFactor * (xMin - xMax))
        fxMin = f(xMin)
      } else if (abs(fxMin) > abs(fxMax)) {
        xMax = enforceBounds(xMax + growthFactor * (xMax - xMin))
        fxMax = f(xMax)
      } else if (flipflop == -1) {
        xMin = enforceBounds(xMin + growthFactor*(xMin - xMax))
        fxMin = f(xMin)
        evaluationNumber += 1
        flipflop = 1
      } else if (flipflop == 1) {
        xMax = enforceBounds(xMax + growthFactor * (xMax - xMin))
        fxMax= f(xMax)
        flipflop = -1
      }
      evaluationNumber += 1
    }
    throw new ConvergenceFailureException(s"unable to bracket root in $maxEvaluations function evaluations. Last bracket attempt: f[$xMin,$xMax] -> [$fxMin,$fxMax])")
  }

  def solve(f: Differentiable, accuracy: Double, guess: Double, xMin: Double, xMax: Double): Double = {
    require(accuracy > 0.0, s"accuracy ($accuracy) must be positive")

    val effAccuracy = math.max(accuracy, Double.MinPositiveValue)

    require(xMin < xMax, s"invalid range: xMin ($xMin) >= xMax ($xMax)")
    require(!lowerBoundEnforced || xMin >= lowerBound, s"xMin_ ($xMin) < enforced low bound ($lowerBound)")
    require(!upperBoundEnforced || xMax <= upperBound, s"xMax_ ($xMax) > enforced hi bound ($upperBound)")

    var fxMin, fxMax = 0.0

    fxMin = f(xMin)
    if (fxMin almost 0.0)
      return xMin

    fxMax = f(xMax)
    if (fxMax almost 0.0)
      return xMax

    val evaluationNumber = 2

    assert(fxMin * fxMax < 0.0, s"root not bracketed: f[$xMin,$xMax] -> [$fxMin,$fxMax]")
    assert(guess > xMin, s"guess ($guess) < xMin ($xMin)")
    assert(guess < xMax, s"guess ($guess) > xMax ($xMax)")

    val root = guess
    solveForZero(f, effAccuracy)
  }

  protected def solveForZero(f: Differentiable, accuracy: Double): Double
}
