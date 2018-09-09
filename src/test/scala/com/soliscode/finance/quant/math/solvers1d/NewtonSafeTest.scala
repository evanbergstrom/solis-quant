package com.soliscode.finance.quant.math.solvers1d

import org.scalatest.FlatSpec

class NewtonSafeTest extends FlatSpec {
  import SolverFixture._

  "NewtonSafe" should "solve with the guess on the left side of the root for an increasing function" in {
    val solver = NewtonSafe()
    testNotBracketed(solver, f1, 0.5)
    testBracketed(solver, f1, 0.5)
  }

  it should "solve with a guess on the right side of the root for an increasing function" in {
    val solver = NewtonSafe()
    testNotBracketed(solver, f1, 1.5)
    testBracketed(solver, f1, 1.5)
  }

  it should "solve with a guess on the left side of the root for a decreasing function" in {
    val solver = NewtonSafe()
    testNotBracketed(solver, f2, 0.5)
    testBracketed(solver, f2, 0.5)
  }

  it should "solve with a guess on the right side of the root for a decreasing function" in {
    val solver = NewtonSafe()
    testNotBracketed(solver, f2, 1.5)
    testBracketed(solver, f2, 1.5)
  }

  it should "handle the case with an infinite derivative" in {
    // situation where bisection is used in the finite difference newton solver as the first step and where the initial
    // guess is equal to the next estimate (which causes an infinite derivative if we do not handle this case with
    // special care)
    testNotBracketed(NewtonSafe(), f3, 1.00001)
  }


}
