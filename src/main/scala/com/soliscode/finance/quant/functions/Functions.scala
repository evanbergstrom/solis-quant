package com.soliscode.finance.quant.functions

import com.soliscode.finance.quant.math.Differentiable

object Functions {

  implicit class FunctionD0(val f: Double => Double) extends Differentiable {

    override def apply(x: Double): Double = f.apply(x)

    override def d(p: Int)(x: Double): Double = p match {
      case 0 => f(x)
      case _ => throw new NotImplementedError()
    }

    override def derivatives(): Int = 0
  }
}
