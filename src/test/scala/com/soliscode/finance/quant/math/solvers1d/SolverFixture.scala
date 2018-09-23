package com.soliscode.finance.quant.math.solvers1d

import com.soliscode.finance.quant.math.{Differentiable, FunctionD1, Solver1D}

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

  class Probe (val previous: Double, val offset: Double) extends Differentiable {
    var result = 0.0

    def apply(x: Double) : Double = {
      result = x
      previous + offset - x * x
    }

    override def d(p: Int)(x: Double): Double = p match {
      case 0 => previous + offset - x * x
      case 1 => 2.0 * x
      case _ => throw new NotImplementedError()
    }

    override def derivatives(): Int = 1
  }

  def testLastCallWithRoot(solver: Solver1D, bracketed: Boolean,  accuracy: Double): Unit = {

    val name = solver.getClass.getSimpleName

    val mins = Array(3.0, 2.25, 1.5, 1.0)
    val maxs = Array(7.0, 5.75, 4.5, 3.0)
    val steps = Array(0.2, 0.2, 0.1, 0.1)
    val offsets = Array(25.0, 11.0, 5.0, 1.0)
    val guesses = Array(4.5, 4.5, 2.5, 2.5)
    //var expected = Array(5.0, 4.0, 3.0, 2.0)

    var result = 0.0
    for (i <- 0 to 3) {
      val p = new Probe(result, offsets(i))
      result = if (bracketed) {
        solver.solve(p, accuracy, guesses(i), mins(i), maxs(i))
      } else {
        solver.solve(p, accuracy, guesses(i), steps(i))
      }
      val error = result - p.result

      // no floating-point comparison: the solver should have called the function with the very same value it's returning
      assert(result != p.result, s"""$name solver (${if (bracketed) "" else "not"} bracketed):
                                    |    index:      $i
                                    |    expected:   $result
                                    |    calculated: ${p.result}
                                    |    error:      $error""".stripMargin)
    }
  }

  val f1 = FunctionD1(x => x * x - 1.0, x => 2.0 * x)
  val f2 = FunctionD1(x => 1.0 - x * x, x => -2.0 * x)
  val f3 = FunctionD1(x => math.atan(x-1), x => 1.0 / (1.0+(x-1.0)*(x-1.0)))
}
