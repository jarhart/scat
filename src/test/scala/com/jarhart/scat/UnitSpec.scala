package com.jarhart.scat

import org.scalatest.FreeSpec
import org.scalatest.prop._

abstract class UnitSpec extends FreeSpec with PropertyChecks with ArbitraryStacks
