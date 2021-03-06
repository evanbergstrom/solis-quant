package com.soliscode.finance.quant.math.solvers1d

import com.soliscode.finance.quant.QuantSpec

class BrentTest extends QuantSpec {
  import SolverFixture._

  "Brent" should "solve with the guess on the left side of the root for an increasing function" in {
    val solver = new Brent(100)
    testNotBracketed(solver, f1, 0.5)
    testBracketed(solver, f1, 0.5)
  }

  it should "solve with a guess on the right side of the root for an increasing function" in {
    val solver = new Brent(100)
    testNotBracketed(solver, f1, 1.5)
    testBracketed(solver, f1, 1.5)
  }

  it should "solve with a guess on the left side of the root for a decreasing function" in {
    val solver = new Brent(100)
    testNotBracketed(solver, f2, 0.5)
    testBracketed(solver, f2, 0.5)
  }

  it should "solve with a guess on the right side of the root for a decreasing function" in {
    val solver = new Brent(100)
    testNotBracketed(solver, f2, 1.5)
    testBracketed(solver, f2, 1.5)
  }

  it should "handle the case with an infinite derivative" in {
    // situation where bisection is used in the finite difference newton solver as the first step and where the initial
    // guess is equal to the next estimate (which causes an infinite derivative if we do not handle this case with
    // special care)
    testNotBracketed(new Brent(100), f3, 1.00001)
  }
}
