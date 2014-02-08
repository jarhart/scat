package com.jarhart.scat

import shapeless._

object Examples extends Examples

trait Examples extends Operations {

  def square[N: Numeric, S <: HList]: Unary[N, S] = (
    dup >>
    mul
  )

  def sumOfSquares[N: Numeric, S <: HList]: Binary[N, N, S] = (
    square >> (
    swap >> (
    square >>
    add
  )))
}
