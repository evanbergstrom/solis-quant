package com.soliscode.finance.quant.math

trait Differentiable {

  def apply(x: Double): Double
  def d(p: Int)(x: Double): Double
  def derivatives(): Int
}

