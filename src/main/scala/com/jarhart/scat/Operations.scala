package com.jarhart.scat

import shapeless._

object Operations extends Operations

trait Operations extends Primitives {
  import Numeric.Implicits._
  import Ordering.Implicits._

  def dup[A, S <: HList]: Op[A :: S, A :: A :: S, A] = for {
    a <- pop
    _ <- push(a)
    _ <- push(a)
  } yield a

  def swap[A, B, S <: HList]: Op[A :: B :: S, B :: A :: S, B] = for {
    a <- pop
    b <- pop
    _ <- push(a)
    _ <- push(b)
  } yield b

  def eql[N, S <: HList](implicit N: Numeric[N]): HBinary[N, N, Boolean, S] = for {
    y <- pop
    x <- pop
    res <- push(y == x)
  } yield res

  def < [N, S <: HList](implicit N: Numeric[N]): HBinary[N, N, Boolean, S] = for {
    y <- pop
    x <- pop
    res <- push(y < x)
  } yield res

  def > [N, S <: HList](implicit N: Numeric[N]): HBinary[N, N, Boolean, S] = for {
    y <- pop
    x <- pop
    res <- push(y > x)
  } yield res

  def inc[S <: HList]: Unary[Int, S] = for {
    x <- pop
    res <- push(x + 1)
  } yield res

  def dec[S <: HList]: Unary[Int, S] = for {
    x <- pop
    res <- push(x - 1)
  } yield res

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

  def mod[S <: HList]: Binary[Int, Int, S] = for {
    y <- pop
    x <- pop
    rem <- push(x % y)
  } yield rem

}
