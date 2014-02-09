package com.jarhart.scat

import org.scalatest.FreeSpec
import org.scalatest.prop._

import shapeless._

class StackPrimitivesSpec extends FreeSpec with PropertyChecks with ArbitraryStacks {

  import StackPrimitives._

  "dup duplicates the top value on the stack" in {
    forAll { (x: Int, stack: HList) =>
      assert(
        dup.run(x :: stack)._2 === x :: x :: stack
      )
    }
  }

  "swap swaps the top two values on the stack" in {
    forAll { (x: Int, y: String, stack: HList) =>
      assert(
        swap.run(x :: y :: stack)._2 === y :: x :: stack
      )
    }
  }
}
