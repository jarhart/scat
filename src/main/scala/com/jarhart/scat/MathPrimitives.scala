package com.jarhart.scat

import shapeless._

object MathPrimitives extends MathPrimitives

trait MathPrimitives extends Op.Primitives {
  import Numeric.Implicits._

  def neg[N, S <: HList](implicit N: Numeric[N]): Unary[N, S] = for {
    x <- pop
    res <- push(-x)
  } yield res

  def add[N, S <: HList](implicit N: Numeric[N]): Binary[N, N, S] = for {
    y <- pop
    x <- pop
    sum <- push(x + y)
  } yield sum

  def sub[N, S <: HList](implicit N: Numeric[N]): Binary[N, N, S] = for {
    y <- pop
    x <- pop
    diff <- push(x - y)
  } yield diff

  def mul[N, S <: HList](implicit N: Numeric[N]): Binary[N, N, S] = for {
    y <- pop
    x <- pop
    prod <- push(x * y)
  } yield prod

  def div[F, S <: HList](implicit F: Fractional[F]): Binary[F, F, S] = for {
    y <- pop
    x <- pop
    quot <- push(F.div(x, y))
  } yield quot
}
