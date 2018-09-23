/*
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2007 StatPro Italia srl
 Copyright (C) 2018 Evan Bergstrom

 This file is provided under the BSD open software license. This is a port of QuantLib,
 a free-software/open-source library for financial quantitative analysts and developers
 (http://quantlib.org/) to Scala. The basic structure and design of the library has been
 preserved, but the naming conventions, types, collection classes and implementation
 have been modified to support common Scala idioms.

 See the full license in the license file (LICENSE.txt)
*/

package com.soliscode.finance.quant.time

import scala.language.implicitConversions

object Frequency {
  implicit def freqeuncyToInt(freq: Frequency): Int = freq.toInt
}

sealed trait Frequency { def toInt: Int }

case object NoFrequency extends Frequency { def toInt: Int = -1 }
case object Once extends Frequency { def toInt: Int = 0 }
case object Annual extends Frequency { def toInt: Int = 1 }
case object Semiannual extends Frequency { def toInt: Int = 2 }
case object EveryFourthMonth extends Frequency { def toInt: Int = 3 }
case object Quarterly extends Frequency { def toInt: Int = 4 }
case object Bimonthly extends Frequency { def toInt: Int = 6 }
case object Monthly extends Frequency { def toInt: Int = 12 }
case object EveryFourthWeek extends Frequency { def toInt: Int = 13 }
case object Biweekly extends Frequency { def toInt: Int = 26 }
case object Weekly extends Frequency { def toInt: Int = 52 }
case object Daily extends Frequency { def toInt: Int = 365 }
case object OtherFrequency extends Frequency { def toInt: Int = 999 }
