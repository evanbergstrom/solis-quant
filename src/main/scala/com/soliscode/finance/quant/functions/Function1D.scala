package com.soliscode.finance.quant.functions

import com.soliscode.finance.quant.math.Differentiable

object Function1D {
  def apply(f: Double => Double, d: Double => Double): Function1D = new Function1D(f,d)
}

class Function1D(f: Double => Double, d: Double => Double) extends Differentiable {

  override def apply(x: Double): Double = f(x)

  override def d(p: Int)(x: Double): Double = p match {
    case 0 =>  f(x)
    case 1 =>  d(x)
    case _ =>  throw new NotImplementedError()
  }

  override def derivatives(): Int = 1
}