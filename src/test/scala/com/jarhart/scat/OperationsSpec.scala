package com.jarhart.scat

import shapeless._

class OperationsSpec extends UnitSpec {

  import Operations._
  import Numeric.FloatIsFractional

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

  "and logically ANDs the top two values on the stack" in {
    forAll { (x: Boolean, y: Boolean, stack: HList) =>
      assert(
        and.run(y :: x :: stack)._2 === (x && y) :: stack
      )
    }
  }

  "or logically ORs the top two values on the stack" in {
    forAll { (x: Boolean, y: Boolean, stack: HList) =>
      assert(
        or.run(y :: x :: stack)._2 === (x || y) :: stack
      )
    }
  }

  "not logically NOTs the top value on the stack" in {
    forAll { (x: Boolean, stack: HList) =>
      assert(
        not.run(x :: stack)._2 === (!x) :: stack
      )
    }
  }
}
