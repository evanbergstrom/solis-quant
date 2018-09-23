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

package com.soliscode.finance.quant.math.solvers1d

import com.soliscode.finance.quant.math.Doubles._
import com.soliscode.finance.quant.math.errors.ConvergenceFailureException
import com.soliscode.finance.quant.math.{Differentiable, Solver1D}

import scala.math._

object Brent {

  def qpply(): Brent = new Brent(100)

  def qpply(maxEvaluations: Long): Brent = new Brent(maxEvaluations)
}

class Brent(maxEvaluations: Long)
  extends Solver1D(maxEvaluations, Double.MinValue, Double.MaxValue, false, false) {

  override protected def solveForZero(f: Differentiable, accuracy: Double): Double = {

    // The implementation of the algorithm was inspired by Press, Teukolsky, Vetterling, and Flannery
    // "Numerical Recipes in C", 2nd edition, Cambridge University Press

    var min1, min2 = 0.0
    var froot, p, q, r, s, xAcc1, xMid = 0.0

    // we want to start with root_ (which equals the guess) on one side of the bracket and both xMin_ and xMax_ on the
    // other.
    froot = f(root)
    evaluationNumber += 1
    if (froot * fxMin < 0) {
      xMax = xMin
      fxMax = fxMin
    } else {
      xMin = xMax
      fxMin = fxMax
    }

    var d = root - xMax
    var e = d

    while (evaluationNumber <= maxEvaluations) {
      if ((froot > 0.0 && fxMax > 0.0) || (froot < 0.0 && fxMax < 0.0)) {

        // Rename xMin_, root_, xMax_ and adjust bounds
        xMax = xMin
        fxMax = fxMin
        e = root - xMin
        d = e
      }

      if (abs(fxMax) < abs(froot)) {
        xMin = root
        root = xMax
        xMax = xMin
        fxMin = froot
        froot = fxMax
        fxMax = fxMin
      }

      // Convergence check
      xAcc1 = 2.0 * Math.ulp(root) * abs(root) + 0.5 * accuracy
      xMid = (xMax - root) / 2.0
      if (abs(xMid) <= xAcc1 || (froot almost 0.0)) {
        f(root)
        evaluationNumber += 1
        return root
      }

      if (abs(e) >= xAcc1 && abs(fxMin) > abs(froot)) {

        // Attempt inverse quadratic interpolation
        s = froot / fxMin
        if (xMin almost xMax) {
          p = 2.0 * xMid * s
          q = 1.0 - s
        } else {
          q = fxMin / fxMax
          r = froot / fxMax
          p = s * (2.0 * xMid * q * (q-r) - (root - xMin) * (r-1.0))
          q = (q-1.0) * (r-1.0) * (s-1.0)
        }
        if (p > 0.0) q = -q  // Check whether in bounds
        p = abs(p)
        min1 = 3.0 * xMid * q - abs(xAcc1 * q)
        min2 = abs(e * q)


        if (2.0*p < min(min1, min2)) {
          e = d    // Accept interpolation
          d = p / q
        } else {
          d=xMid  // Interpolation failed, use bisection
          e=d
        }
      } else {
        // Bounds decreasing too slowly, use bisection
        d = xMid
        e = d
      }
      xMin = root
      fxMin = froot
      if (abs(d) > xAcc1)
        root += d
      else
        root += sign(xAcc1, xMid)

      froot = f(root)
      evaluationNumber += 1
    }
    throw new ConvergenceFailureException(s"maximum number of function evaluations ($maxEvaluations) exceeded")
  }

  private def sign(a: Double, b: Double): Double =
    if (b >= 0.0) abs(a) else  -abs(a)

}
