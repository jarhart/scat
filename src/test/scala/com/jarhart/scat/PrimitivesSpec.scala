package com.jarhart.scat

import shapeless._

class PrimitivesSpec extends UnitSpec {

  import Primitives._

  "push pushes values onto a stack" in {
    forAll { (x: Int, stack: HList) =>
      assert(
        push(x).run(stack) === (x, x :: stack)
      )
    }
  }

  "pop pops values off of a stack" in {
    forAll { (x: Int, stack: HList) =>
      assert(
        pop.run(x :: stack) === (x, stack)
      )
    }
  }
}
