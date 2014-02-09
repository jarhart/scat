package com.jarhart.scat

import org.scalatest.FreeSpec
import org.scalatest.prop._

import shapeless._

class CombinatorPrimitivesSpec extends FreeSpec with PropertyChecks with ArbitraryStacks {

  import CombinatorPrimitives._
  import MathPrimitives._
  import Numeric.IntIsIntegral

  "exec executes the operation on the top of the stack" in {
    forAll { (x: Int, y: Int, stack: HList) =>
      assert(
        exec.run(add[Int, HList] :: y :: x :: stack)._2 === (x + y) :: stack
      )
    }
  }
}
