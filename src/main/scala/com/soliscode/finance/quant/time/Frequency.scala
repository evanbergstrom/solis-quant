package com.soliscode.finance.quant.time

object Frequency {

  sealed trait Frequency

  case object NoFrequency extends Frequency
  case object Once extends Frequency
  case object Annual extends Frequency
  case object Semiannual extends Frequency
  case object EveryFourthMonth extends Frequency
  case object Quarterly extends Frequency
  case object Bimonthly extends Frequency
  case object Monthly extends Frequency
  case object EveryFourthWeek extends Frequency
  case object Biweekly extends Frequency
  case object Weekly extends Frequency
  case object Daily extends Frequency
  case object OtherFrequency extends Frequency

}
