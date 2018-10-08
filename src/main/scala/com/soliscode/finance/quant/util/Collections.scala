/*
 *  Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 *  Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 *  Copyright (C) 2018 Evan Bergstrom
 *
 * This file is provided under the BSD open software licemnse. This is a port of QuantLib, a
 * free-software/open-source library for financial quantitative analysts and developers
 * (http://quantlib.org/) to Scala. The basic structure and design of the library has been
 * preserved, but thebnaming conventions, basic types, collection classes and implementation
 * have been modified to support common Scala idioms.
 *
 * See the full license in the license file (LICENSE.txt)
 */

package com.soliscode.finance.quant.util

import scala.collection.mutable.ArrayBuffer

object Collections {
  implicit class ArrayBufferExt[A](val b: ArrayBuffer[A]) extends AnyVal {

    def mapInPlace(a: A => A): ArrayBuffer[A] = {
      for (i <- b.indices)
        b(i) = a(b(i))
      b
    }

    def mapInPlace(start: Int, end: Int, a: A => A): ArrayBuffer[A] = {
      for (i <- start until end)
        b(i) = a(b(i))
      b
    }

    def updateInPlace(i: Int, a: A => A): ArrayBuffer[A] = {
      b(i) = a(b(i))
      b
    }

    def reverseInPlace(): ArrayBuffer[A] = {
      for (i <- 0 until b.length/2) {
        val tmp = b(i)
        val j = b.length-1-i
        b(i) = b(j)
        b(j) = tmp
      }
      b
    }
  }
}
