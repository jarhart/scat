package com.jarhart.scat

import shapeless._

object Primitives extends Primitives

trait Primitives {

  def push[A, S <: HList](a: A) = Op[S, A :: S, A] {
    s => (a, a :: s)
  }

  def pop[A, S <: HList] = Op[A :: S, S, A] {
    case a :: s => (a, s)
  }
}
