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

import com.soliscode.finance.quant.math.errors.ConvergenceFailureException
import com.soliscode.finance.quant.math.{Differentiable, Solver1D}

import scala.math._

object NewtonSafe {
  def apply(): NewtonSafe = new NewtonSafe(100, 0.0, 0.0, false, false)

  def apply(maxEvaluations: Long): NewtonSafe = new NewtonSafe(maxEvaluations, 0.0, 0.0, false, false)

  def apply(maxEvaluations: Long, lowerBound: Double, upperBound: Double): NewtonSafe =
    new NewtonSafe(maxEvaluations, lowerBound, upperBound, true, true)
}

class NewtonSafe(maxEvaluations: Long,
                 lowerBound: Double,
                 upperBound: Double,
                 lowerBoundEnforced: Boolean = false,
                 upperBoundEnforced: Boolean = false)
  extends Solver1D(maxEvaluations, lowerBound, upperBound, lowerBoundEnforced, upperBoundEnforced) {

  override protected def solveForZero(f: Differentiable, accuracy: Double): Double = {

    require(f.derivatives() >= 1, "function must have at least one derivative")

    // The implementation of the algorithm was inspired by Press, Teukolsky, Vetterling, and Flannery,
    // "Numerical Recipes in C", 2nd edition, Cambridge University Press

    // Orient the search so that f(xl) < 0
    var (xh, xl) = if (fxMin < 0.0) (xMax, xMin) else (xMin, xMax)

    // the "stepsize before last"
    // it was dxold=std::fabs(xMax_-xMin_); in Numerical Recipes
    // here (xMax_-xMin_ > 0) is verified in the constructor
    var dxold = xMax - xMin

    // and the last step
    var dx = dxold

    var froot = f(root)
    var dfroot = f.d(1)(root)

    evaluationNumber += 1
    while (evaluationNumber <= maxEvaluations) {

      // Bisect if (out of range || not decreasing fast enough)
      if ((((root - xh) * dfroot - froot) * ((root - xl) * dfroot - froot) > 0.0)
          || (abs(2.0 * froot) > abs(dxold * dfroot))) {
        dxold = dx
        dx = (xh - xl) / 2.0
        root = xl + dx
      } else {
        dxold = dx
        dx = froot / dfroot
        root -= dx
      }

      // Convergence criterion
      if (abs(dx) < accuracy) {
        f(root) // TODO Figure out why this function call is here (side-effects?)
        evaluationNumber += 1
        return root
      }

      froot = f(root)
      dfroot = f.d(1)(root)
      evaluationNumber += 1
      if (froot < 0.0)
        xl=root
      else
        xh=root
      root
    }

    throw new ConvergenceFailureException(s"maximum number of function evaluations ($maxEvaluations) exceeded")
  }
}
