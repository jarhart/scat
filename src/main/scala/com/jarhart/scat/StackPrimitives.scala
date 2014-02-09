package com.jarhart.scat

import shapeless._

object StackPrimitives extends StackPrimitives

trait StackPrimitives extends Op.Primitives {

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
}
