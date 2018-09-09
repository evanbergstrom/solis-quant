package com.soliscode.finance.quant

/**
  * Interest rate compounding rules
  */
object Compounding {

  sealed trait Compounding

  /* 1+rt */
  case object Simple extends Compounding

  /* (1+r)^t */
  case object Compounded extends Compounding

  /* e^{rt} */
  case object Continuous extends Compounding

  /* Simple up to the first period then Compounded */
  case object SimpleThenCompounded extends Compounding

  /* Compounded up to the first period then Simple */
  case object CompoundedThenSimple extends Compounding
}
