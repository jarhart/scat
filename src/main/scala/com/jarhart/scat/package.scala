package com.jarhart

import shapeless._

package object scat {

  type Unary[A, S <: HList] = Op[A :: S, A :: S, A]

  type Binary[A, B, S <: HList] = Op[A :: B :: S, A :: S, A]
}
