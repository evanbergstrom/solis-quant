package com.soliscode.finance.quant.patterns


trait Visitor[T] {
  def visit(t: T): Unit
}
