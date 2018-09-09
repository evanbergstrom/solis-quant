package com.soliscode.finance.quant.math

import scala.collection.mutable

/**
  * Prime numbers calculator
  * Taken from "Monte Carlo Methods in Finance", by Peter Jäckel
  */
object PrimeNumbers {

  private val primeNumbers = mutable.ListBuffer[Long](2,3,5,7,11,13,17,19,23,29,31,37,41,43,47)

  def get(absoluteIndex: Int): Long = {
    while (primeNumbers.length <= absoluteIndex) nextPrimeNumber()
    primeNumbers(absoluteIndex)
  }

  private def nextPrimeNumber() : Unit = {
    var m = primeNumbers.last
    var p, n : Long = 0
    do {
      m += 2
      n = Math.sqrt(m.asInstanceOf[Double]).asInstanceOf[Long]
      var i = 1
      do {
        p = primeNumbers(i)
        i += 1
      } while (m%p != 0 && p <= n)
    } while( p <= n)
    primeNumbers += m
  }
}
