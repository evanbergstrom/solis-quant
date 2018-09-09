package com.soliscode.finance.quant.math

import math._

object Doubles {
  implicit class DoubleExt(val x: Double) extends AnyVal {

    def almost(y: Double): Boolean = almost(42)(y)

    def almost(n:Int)(y: Double): Boolean =
      // Deals with +infinity and -infinity representations etc.
      if (x == y)
        true
      else {
        val diff = abs(x - y)
        val tolerance = n * Double.MinPositiveValue

        if (x * y == 0.0) // x or y = 0.0
          diff < (tolerance * tolerance)
        else
          diff <= tolerance * abs(x) && diff <= tolerance * abs(y);
      }

    def isCloseEnough(y: Double): Boolean = isCloseEnough(y, 42)

    def isCloseEnough(y: Double, n: Int): Boolean = {
      // Deals with +infinity and -infinity representations etc.
      if (x == y)
        true
      else {
        val diff = abs(x - y)
        val tolerance = n * Double.MinPositiveValue

        if (x * y == 0.0) // x or y = 0.0
          diff < (tolerance * tolerance);
        else
          diff <= tolerance * abs(x) || diff <= tolerance * abs(y);
      }
    }
  }
}
