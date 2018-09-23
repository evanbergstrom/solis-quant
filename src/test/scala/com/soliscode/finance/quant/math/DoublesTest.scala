package com.soliscode.finance.quant.math

import com.soliscode.finance.quant.QuantSpec
import com.soliscode.finance.quant.math.Doubles._

class DoublesTest extends QuantSpec {

  "modf" should "return the integer and fractional part of a decimal number" in {

    val (f1, i1) = modf(5.134)
    assert(f1 almost 0.134, s"actual: $f1, expected: 0.134")
    assert(i1 == 5.0)
  }
}
