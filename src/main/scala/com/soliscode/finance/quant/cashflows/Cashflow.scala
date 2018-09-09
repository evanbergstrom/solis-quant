package com.soliscode.finance.quant.cashflows

import java.time.LocalDate

import com.soliscode.finance.quant.Event
import com.soliscode.finance.quant.patterns.Visitor

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

object Cashflow {
  type Leg = ListBuffer[Cashflow]
}

trait Cashflow extends Event {
  def date: LocalDate
  def amount : Double
  def exCouponDate: LocalDate

  def hasOccured(refDate: LocalDate, includeRefDate: Boolean = true) : Boolean = {
    val now = LocalDate.now()
    if (!refDate.equals(now))
      refDate.isBefore(now)
    else
      includeRefDate
  }

  def tradingExCoupon(refDate: LocalDate): Boolean = {
    val now = LocalDate.now()
    if (exCouponDate.equals(now))
      return false;
    else
      exCouponDate.isBefore(refDate) || exCouponDate.isEqual(refDate)
  }

  def earlierThan(c: Cashflow): Boolean =
    date.isBefore(c.date)

  override def accept[T: TypeTag](visitor: Visitor[T]): Unit = typeOf[T] match {
    case t if t =:= typeOf[Cashflow] => visitor.asInstanceOf[Visitor[Cashflow]].visit(this)
    case _ => super.accept(visitor)
  }
}

