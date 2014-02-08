package com.jarhart.scat

import shapeless._

case class Op[S1 <: HList, S2 <: HList, A](run: S1 => (A, S2)) {

  def apply(s1: S1): (A, S2) = run(s1)

  def flatMap[S3 <: HList, B](f: A => Op[S2, S3, B]) = Op[S1, S3, B] { s1 =>
    val (a, s2) = run(s1)
    f(a)(s2)
  }

  def map[B](f: A => B) = flatMap(x => Op(s => (f(x), s)))

  def >>=[S3 <: HList, B](f: A => Op[S2, S3, B]): Op[S1, S3, B] = flatMap(f)

  def >>[S3 <: HList, B](o: => Op[S2, S3, B]): Op[S1, S3, B] = flatMap(_ => o)
}
