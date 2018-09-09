package com.soliscode.finance.quant.time

import java.time.{LocalDate, Period}

// TODO: Eliminate or redesign this class

object DateIterator {

  def apply(from: LocalDate, to: LocalDate): Iterator[LocalDate] = {
    Iterator.iterate(from)(_ plusDays 1).takeWhile(d => d.isBefore(to))
  }

  def withPeriod(from: LocalDate, to: LocalDate, per: Period): Iterator[LocalDate] = {
    Iterator.iterate(from)(_.plus(per)).takeWhile(d => if (per.isNegative) d.isAfter(to) else d.isBefore(to))
  }
}

