package com.jarhart.scat

import shapeless._

trait Program[Ops <: HList, S1 <: HList, S2 <: HList, A] {
  def toOp(ops: Ops): Op[S1, S2, A]
}

object Program {

  implicit def MultiOpProgram[S1 <: HList, S2 <: HList, S3 <: HList, A, B, T <: HList](implicit P: Program[T, S2, S3, B]) =
    new Program[Op[S1, S2, A] :: T, S1, S3, B] {
      def toOp(ops: Op[S1, S2, A] :: T): Op[S1, S3, B] =
        ops.head >> P.toOp(ops.tail)
    }

  implicit def SingletonProgram[S1 <: HList, S2 <: HList, A] =
    new Program[Op[S1, S2, A] :: HNil, S1, S2, A] {
      def toOp(ops: Op[S1, S2, A] :: HNil): Op[S1, S2, A] = ops.head
    }
}
