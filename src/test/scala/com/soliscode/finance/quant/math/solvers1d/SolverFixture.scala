package com.soliscode.finance.quant.math.solvers1d

import com.soliscode.finance.quant.functions.Function1D
import com.soliscode.finance.quant.math.{Differentiable, Solver1D}

object SolverFixture {

  def testNotBracketed(solver: Solver1D, f: Differentiable, guess: Double): Unit = {
    val name = solver.getClass.getSimpleName
    val accuracy = Array(1.0e-4, 1.0e-6, 1.0e-8)
    val expected = 1.0
    for (a <- accuracy) {
      val root = solver.solve(f, a, guess, 0.1)
      assert(math.abs(root - expected) <= a, s"$name solver (not bracketed). expected: $expected, calculated: $root, accuracy: $a")
    }
  }

  def testBracketed(solver: Solver1D, f: Differentiable, guess: Double): Unit = {
    val name = solver.getClass.getSimpleName
    val accuracy = Array(1.0e-4, 1.0e-6, 1.0e-8 )
    val expected = 1.0
    for (a <- accuracy) {
      // guess on the left side of the root, increasing function
      val root = solver.solve(f, a, guess, 0.0, 2.0)
      assert(math.abs(root-expected) <= a, s"$name solver (bracketed). expected: $expected, calculated: $root, accuracy: $a")
    }
  }

  val f1 = Function1D(x => x * x - 1.0, x => 2.0 * x)
  val f2 = Function1D(x => 1.0 - x * x, x => -2.0 * x)
  val f3 = Function1D(x => math.atan(x-1), x => 1.0 / (1.0+(x-1.0)*(x-1.0)))

}
