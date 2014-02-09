package com.jarhart.scat

import shapeless._

object MathOperations extends MathOperations

trait MathOperations extends StackPrimitives with MathPrimitives {

  def square[N: Numeric, S <: HList]: Unary[N, S] = (
    dup[N, S] >>
    mul[N, S]
  )
}
