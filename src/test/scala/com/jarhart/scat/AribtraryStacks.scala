package com.jarhart.scat

import org.scalacheck._

import shapeless._

trait ArbitraryStacks {
  import Generators._

  implicit lazy val arbStack = Arbitrary(genStack)

  object Generators {
    import Gen._

    def genStack: Gen[HList] =
      oneOf(genBookStack, genIntStack, genStringStack, HNil)

    def genBookStack: Gen[Boolean :: HList] = for {
      b <- oneOf(true, false)
      stack <- genStack
    } yield b :: stack

    def genIntStack: Gen[Int :: HList] = for {
      n <- choose(Int.MinValue, Int.MaxValue)
      stack <- genStack
    } yield n :: stack

    def genStringStack: Gen[String :: HList] = for {
      s <- alphaStr
      stack <- genStack
    } yield s :: stack
  }
}
