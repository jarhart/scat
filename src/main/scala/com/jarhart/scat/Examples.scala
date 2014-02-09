package com.jarhart.scat

import shapeless._

object Examples extends Examples

trait Examples extends Operations {

  def square[N: Numeric, S <: HList]: Unary[N, S] = Op(
    dup[N, S],
    mul[N, S]
  )

  def sumOfSquares[N: Numeric, S <: HList]: Binary[N, N, S] = Op(
    square[N, N :: S],
    swap[N, N, S],
    square[N, N :: S],
    add[N, S]
  )
}
