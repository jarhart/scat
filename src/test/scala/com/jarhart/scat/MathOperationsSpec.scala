package com.jarhart.scat

import org.scalatest.FreeSpec
import org.scalatest.prop._

import shapeless._

class MathOperationsSpec extends FreeSpec with PropertyChecks with ArbitraryStacks {

  import MathOperations._
  import Numeric.IntIsIntegral

  "square squares the value on the top of the stack" in {
    forAll { (x: Int, stack: HList) =>
      assert(
        square.run(x :: stack)._2 === (x * x) :: stack
      )
    }
  }
}
