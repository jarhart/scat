package com.jarhart.scat

import org.scalatest.FreeSpec
import org.scalatest.prop._

import shapeless._

class MathPrimitivesSpec extends FreeSpec with PropertyChecks with ArbitraryStacks {

  import MathPrimitives._
  import Numeric.FloatIsFractional

  "neg negates the top value on the stack" in {
    forAll { (x: Float, stack: HList) =>
      assert(
        neg.run(x :: stack)._2 === -x :: stack
      )
    }
  }

  "add adds the top two values on the stack" in {
    forAll { (x: Float, y: Float, stack: HList) =>
      assert(
        add.run(y :: x :: stack)._2 === (x + y) :: stack
      )
    }
  }

  "sub subtracts the top value on the stack from the one below" in {
    forAll { (x: Float, y: Float, stack: HList) =>
      assert(
        sub.run(y :: x :: stack)._2 === (x - y) :: stack
      )
    }
  }

  "mul multiplies the top two values on the stack" in {
    forAll { (x: Float, y: Float, stack: HList) =>
      assert(
        mul.run(y :: x :: stack)._2 === (x * y) :: stack
      )
    }
  }

  "div divides the top value on the stack by the one below" in {
    forAll { (x: Float, y: Float, stack: HList) =>
      whenever(y != 0.0) {
        assert(
          div.run(y :: x :: stack)._2 === (x / y) :: stack
        )
      }
    }
  }
}
