package com.jarhart.scat

import shapeless._

object Operations extends Operations

trait Operations
  extends Primitives
    with MathOperations
