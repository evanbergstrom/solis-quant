package com.soliscode.finance.quant

import java.time.LocalDate

import com.soliscode.finance.quant.patterns.Visitor

import scala.reflect.runtime.universe._

trait Event {

  def date(): LocalDate
  def hasOccurred(refDate: LocalDate, includeRefDate: Boolean): Boolean

  def accept[T : TypeTag](visitor: Visitor[T]) : Unit = typeOf[T] match {
    case t if t =:= typeOf[Event] => visitor.asInstanceOf[Visitor[Event]].visit(this)
    case _ => throw new IllegalArgumentException(s"${visitor.getClass} is not an ${classOf[Event]} visitor")
  }
}
