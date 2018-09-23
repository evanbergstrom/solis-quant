package com.soliscode.finance.quant.math

import com.soliscode.finance.quant.QuantSpec
import com.soliscode.finance.quant.math.Doubles._
import com.soliscode.finance.quant.math.Rounding._

class RoudingTest extends QuantSpec {

  def checkRounding(x: Double, precision: Int, round: RoundingType, expected: Double): Unit = {
    val rounding = Rounding(precision, round)
    val actual = rounding.apply(x)
    assert(actual almost expected, s"original: $x, expected: $expected, actual: $actual")
  }

  "Rounding" should "create a default rounding to the closest digit (5 rounds up)" in {
    val rounding = Rounding(4)
    assert(rounding.digit == 5)
    assert(rounding.rounding == Closest)
  }

  "Closest" should "Round to the nearest number" in {
    checkRounding(0.86313513, 5, Closest, 0.86314)
    checkRounding(0.86313, 5, Closest, 0.86313)
    checkRounding(-7.64555346, 1, Closest, -7.6)
    checkRounding(0.13961605, 2, Closest, 0.14)
    checkRounding(0.14344179, 4, Closest, 0.1434)
    checkRounding(-4.74315016, 2, Closest, -4.74)
    checkRounding(-7.82772074, 5, Closest, -7.82772)
    checkRounding(2.74137947, 3, Closest, 2.741)
    checkRounding(2.13056714, 1, Closest, 2.1)
    checkRounding(-1.06228670, 1, Closest, -1.1)
    checkRounding(8.29234094, 4, Closest, 8.2923)
    checkRounding(7.90185598, 2, Closest, 7.90)
    checkRounding(-0.26738058, 1, Closest, -0.3)
    checkRounding(1.78128713, 1, Closest, 1.8)
    checkRounding(4.23537260, 1, Closest, 4.2)
    checkRounding(3.64369953, 4, Closest, 3.6437)
    checkRounding(6.34542470, 2, Closest, 6.35)
    checkRounding(-0.84754962, 4, Closest, -0.8475)
    checkRounding(4.60998652, 1, Closest, 4.6)
    checkRounding(6.28794223, 3, Closest, 6.288)
    checkRounding(7.89428221, 2, Closest, 7.89)
  }

  "Up" should "Round to the next higher number" in {
    checkRounding(0.86313513, 5, Up, 0.86314)
    checkRounding(0.86313, 5, Up, 0.86313)
    checkRounding(-7.64555346, 1, Up, -7.7)
    checkRounding(0.13961605, 2, Up, 0.14)
    checkRounding(0.14344179, 4, Up, 0.1435)
    checkRounding(-4.74315016, 2, Up, -4.75)
    checkRounding(-7.82772074, 5, Up, -7.82773)
    checkRounding(2.74137947, 3, Up, 2.742)
    checkRounding(2.13056714, 1, Up, 2.2)
    checkRounding(-1.06228670, 1, Up, -1.1)
    checkRounding(8.29234094, 4, Up, 8.2924)
    checkRounding(7.90185598, 2, Up, 7.91)
    checkRounding(-0.26738058, 1, Up, -0.3)
    checkRounding(1.78128713, 1, Up, 1.8)
    checkRounding(4.23537260, 1, Up, 4.3)
    checkRounding(3.64369953, 4, Up, 3.6437)
    checkRounding(6.34542470, 2, Up, 6.35)
    checkRounding(-0.84754962, 4, Up, -0.8476)
    checkRounding(4.60998652, 1, Up, 4.7)
    checkRounding(6.28794223, 3, Up, 6.288)
    checkRounding(7.89428221, 2, Up, 7.90)
  }

  "Down" should "Round to the next lower number" in {
    checkRounding(0.86313513, 5, Down, 0.86313)
    checkRounding(0.86313, 5, Down, 0.86313)
    checkRounding(-7.64555346, 1, Down, -7.6)
    checkRounding(0.13961605, 2, Down, 0.13)
    checkRounding(0.14344179, 4, Down, 0.1434)
    checkRounding(-4.74315016, 2, Down, -4.74)
    checkRounding(-7.82772074, 5, Down, -7.82772)
    checkRounding(2.74137947, 3, Down, 2.741)
    checkRounding(2.13056714, 1, Down, 2.1)
    checkRounding(-1.06228670, 1, Down, -1.0)
    checkRounding(8.29234094, 4, Down, 8.2923)
    checkRounding(7.90185598, 2, Down, 7.90)
    checkRounding(-0.26738058, 1, Down, -0.2)
    checkRounding(1.78128713, 1, Down, 1.7)
    checkRounding(4.23537260, 1, Down, 4.2)
    checkRounding(3.64369953, 4, Down, 3.6436)
    checkRounding(6.34542470, 2, Down, 6.34)
    checkRounding(-0.84754962, 4, Down, -0.8475)
    checkRounding(4.60998652, 1, Down, 4.6)
    checkRounding(6.28794223, 3, Down, 6.287)
    checkRounding(7.89428221, 2, Down, 7.89)

  }

  "Floor" should "Round to the next floor decimal" in {
    checkRounding(0.86313513, 5, Floor, 0.86314)
    checkRounding(0.86313, 5, Floor, 0.86313)
    checkRounding(-7.64555346, 1, Floor, -7.6)
    checkRounding(0.13961605, 2, Floor, 0.14)
    checkRounding(0.14344179, 4, Floor, 0.1434)
    checkRounding(-4.74315016, 2, Floor, -4.74)
    checkRounding(-7.82772074, 5, Floor, -7.82772)
    checkRounding(2.74137947, 3, Floor, 2.741)
    checkRounding(2.13056714, 1, Floor, 2.1)
    checkRounding(-1.06228670, 1, Floor, -1.0)
    checkRounding(8.29234094, 4, Floor, 8.2923)
    checkRounding(7.90185598, 2, Floor, 7.90)
    checkRounding(-0.26738058, 1, Floor, -0.2)
    checkRounding(1.78128713, 1, Floor, 1.8)
    checkRounding(4.23537260, 1, Floor, 4.2)
    checkRounding(3.64369953, 4, Floor, 3.6437)
    checkRounding(6.34542470, 2, Floor, 6.35)
    checkRounding(-0.84754962, 4, Floor, -0.8475)
    checkRounding(4.60998652, 1, Floor, 4.6)
    checkRounding(6.28794223, 3, Floor, 6.288)
    checkRounding(7.89428221, 2, Floor, 7.89)
  }

  "Ceil" should "Round to the next ceiling decimal" in {
    checkRounding(0.86313513, 5, Ceiling, 0.86313)
    checkRounding(0.86313, 5, Ceiling, 0.86313)
    checkRounding(-7.64555346, 1, Ceiling, -7.6)
    checkRounding(0.13961605, 2, Ceiling, 0.13)
    checkRounding(0.14344179, 4, Ceiling, 0.1434)
    checkRounding(-4.74315016, 2, Ceiling, -4.74)
    checkRounding(-7.82772074, 5, Ceiling, -7.82772)
    checkRounding(2.74137947, 3, Ceiling, 2.741)
    checkRounding(2.13056714, 1, Ceiling, 2.1)
    checkRounding(-1.06228670, 1, Ceiling, -1.1)
    checkRounding(8.29234094, 4, Ceiling, 8.2923)
    checkRounding(7.90185598, 2, Ceiling, 7.90)
    checkRounding(-0.26738058, 1, Ceiling, -0.3)
    checkRounding(1.78128713, 1, Ceiling, 1.7)
    checkRounding(4.23537260, 1, Ceiling, 4.2)
    checkRounding(3.64369953, 4, Ceiling, 3.6436)
    checkRounding(6.34542470, 2, Ceiling, 6.34)
    checkRounding(-0.84754962, 4, Ceiling, -0.8475)
    checkRounding(4.60998652, 1, Ceiling, 4.6)
    checkRounding(6.28794223, 3, Ceiling, 6.287)
    checkRounding(7.89428221, 2, Ceiling, 7.89)
  }

}