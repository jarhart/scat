package com.jarhart.scat

import shapeless._

object CombinatorPrimitives extends CombinatorPrimitives

trait CombinatorPrimitives extends Op.Primitives {

  def exec[A, S1 <: HList, S2 <: HList]: Op[Op[S1, S2, A] :: S1, S2, A] = for {
    op <- pop
    res <- op
  } yield res
}
