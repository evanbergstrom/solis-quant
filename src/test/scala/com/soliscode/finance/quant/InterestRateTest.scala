package com.soliscode.finance.quant

import java.lang.Math._
import java.time.LocalDate

import com.soliscode.finance.quant.math.Rounding
import com.soliscode.finance.quant.time._
import com.soliscode.finance.quant.time.daycounters.Actual360

class InterestRateTest extends QuantSpec {

  def checkRateConversion(r: Double, comp: Compounding, freq: Frequency, t: Double, comp2: Compounding,
                          freq2: Frequency, expected: Double, precision: Int): Unit = {

    val ir = InterestRate(r, Actual360(), comp, freq)
    val d1 = LocalDate.now()
    val d2 = d1.plusDays((360 * t + 0.5).toInt)
    val roundingPrecision = Rounding(precision)

    // check that the compound factor is the inverse of the discount factor
    val compoundf = ir.compoundFactor(d1, d2)
    val disc = ir.discountFactor(d1, d2)

    val error = abs(disc - 1.0 / compoundf)
    val errmsg = s"""\n original interest rate ${ir.rate}
                    |  1.0/compound_factor: ${1.0 / compoundf}
                    |  discount_factor: $disc
                    |  error: $error""".stripMargin

    assert(error <= 1e-15, errmsg)

    // check that the equivalent InterestRate with *same* daycounter, compounding, and frequency is the *same* InterestRate
    val ir2 = ir.equivalentRate(ir.dayCounter, ir.compounding, ir.frequency, d1, d2)

    val error2 = abs(ir.rate - ir2.rate)
    val errmsg2 = s"""\noriginal interest rate: ${ir.rate}
                     |  equivalent interest rate: ${ir2.rate}
                     |  rate error: $error2""".stripMargin

    assert(error2 <= 1e-15, errmsg2)
    assert(ir.dayCounter.name == ir2.dayCounter.name)
    assert(ir.compounding == ir2.compounding)
    assert(ir.frequency == ir2.frequency)

    // check that the equivalent InterestRate with *different*
    // compounding, and frequency is the *expected* InterestRate
    val ir3 = ir.equivalentRate(ir.dayCounter, comp2, freq2, d1, d2)
    val expectedIR = InterestRate(expected, ir.dayCounter, comp2, freq2)
    var r3 = roundingPrecision(ir3.rate)

    val error3 = abs(r3 - expectedIR.rate)
    val errmsg3 = s"""\n original interest rate: ${ir.rate}
                      |     calculated equivalent interest rate: ${ir3.rate}
                      |     truncated equivalent rate: $r3
                      |     expected equivalent interest rate: ${expectedIR.rate}
                      |     rate error: $error3""".stripMargin

    assert(error3 <= 1.0e-15, errmsg3)
    assert(ir3.dayCounter.name == expectedIR.dayCounter.name)
    assert(ir3.compounding == expectedIR.compounding)
    assert(ir3.frequency == expectedIR.frequency)

    // check that the equivalent rate with *different*
    // compounding, and frequency is the *expected* rate
    val ir4 = ir.equivalentRate(ir.dayCounter, comp2, freq2, d1, d2)
    val r4 = roundingPrecision(ir4.rate)

    val error4 = abs(r4 - expected)
    val errmsg4 = s"""\n calculated equivalent rate: $r3
                     |    expected equivalent rate: $expected
                     |    error: $error""".stripMargin

    assert(error4 <= 1.0e-15, errmsg4)
  }

  "InterestRate" should "convert between compounded rates" in {

      // data from "Option Pricing Formulas", Haug, pag.181-182
      //           Rate,   Compounding,       Frequency,   Time, Compounding2,      Frequency2,  Rate2, precision

    checkRateConversion(0.0800, Compounded,        Quarterly,   1.00,   Continuous,          Annual, 0.0792, 4)
    checkRateConversion(0.1200, Continuous,           Annual,   1.00,   Compounded,          Annual, 0.1275, 4)
    checkRateConversion(0.0800, Compounded,        Quarterly,   1.00,   Compounded,          Annual, 0.0824, 4)
    checkRateConversion(0.0700, Compounded,        Quarterly,   1.00,   Compounded,      Semiannual, 0.0706, 4)

      // undocumented, but reasonable :)
    checkRateConversion(0.0100, Compounded,           Annual,   1.00,     Simple,            Annual, 0.0100, 4)
    checkRateConversion(0.0200,     Simple,           Annual,   1.00, Compounded,            Annual, 0.0200, 4)
    checkRateConversion(0.0300, Compounded,       Semiannual,   0.50,     Simple,            Annual, 0.0300, 4)
    checkRateConversion(0.0400,     Simple,           Annual,   0.50, Compounded,        Semiannual, 0.0400, 4)
    checkRateConversion(0.0500, Compounded, EveryFourthMonth,  1.0/3,     Simple,            Annual, 0.0500, 4)
    checkRateConversion(0.0600,     Simple,           Annual,  1.0/3, Compounded,  EveryFourthMonth, 0.0600, 4)
    checkRateConversion(0.0500, Compounded,        Quarterly,   0.25,     Simple,            Annual, 0.0500, 4)
    checkRateConversion(0.0600,     Simple,           Annual,   0.25, Compounded,         Quarterly, 0.0600, 4)
    checkRateConversion(0.0700, Compounded,        Bimonthly,  1.0/6,     Simple,            Annual, 0.0700, 4)
    checkRateConversion(0.0800,     Simple,           Annual,  1.0/6, Compounded,         Bimonthly, 0.0800, 4)
    checkRateConversion(0.0900, Compounded,          Monthly, 1.0/12,     Simple,            Annual, 0.0900, 4)
    checkRateConversion(0.1000,     Simple,           Annual, 1.0/12, Compounded,           Monthly, 0.1000, 4)

    checkRateConversion(0.0300, SimpleThenCompounded, Semiannual,   0.25,               Simple,      Annual, 0.0300, 4)
    checkRateConversion(0.0300, SimpleThenCompounded, Semiannual,   0.25,               Simple,  Semiannual, 0.0300, 4)
    checkRateConversion(0.0300, SimpleThenCompounded, Semiannual,   0.25,               Simple,   Quarterly, 0.0300, 4)
    checkRateConversion(0.0300, SimpleThenCompounded, Semiannual,   0.50,               Simple,      Annual, 0.0300, 4)
    checkRateConversion(0.0300, SimpleThenCompounded, Semiannual,   0.50,               Simple,  Semiannual, 0.0300, 4)
    checkRateConversion(0.0300, SimpleThenCompounded, Semiannual,   0.75,           Compounded,  Semiannual, 0.0300, 4)

    checkRateConversion(0.0400,               Simple, Semiannual,   0.25, SimpleThenCompounded,   Quarterly, 0.0400, 4)
    checkRateConversion(0.0400,               Simple, Semiannual,   0.25, SimpleThenCompounded,  Semiannual, 0.0400, 4)
    checkRateConversion(0.0400,               Simple, Semiannual,   0.25, SimpleThenCompounded,      Annual, 0.0400, 4)

    checkRateConversion(0.0400,           Compounded,  Quarterly,   0.50, SimpleThenCompounded,   Quarterly, 0.0400, 4)
    checkRateConversion(0.0400,               Simple, Semiannual,   0.50, SimpleThenCompounded,  Semiannual, 0.0400, 4)
    checkRateConversion(0.0400,               Simple, Semiannual,   0.50, SimpleThenCompounded,      Annual, 0.0400, 4)

    checkRateConversion(0.0400,           Compounded,  Quarterly,   0.75, SimpleThenCompounded,   Quarterly, 0.0400, 4)
    checkRateConversion(0.0400,           Compounded, Semiannual,   0.75, SimpleThenCompounded,  Semiannual, 0.0400, 4)
    checkRateConversion(0.0400,               Simple, Semiannual,   0.75, SimpleThenCompounded,      Annual, 0.0400, 4)
  }
}
