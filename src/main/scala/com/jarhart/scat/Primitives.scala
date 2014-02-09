package com.jarhart.scat

import shapeless._

object Primitives extends Primitives

trait Primitives
  extends StackPrimitives
    with CombinatorPrimitives
    with MathPrimitives
