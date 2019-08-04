
package com.wordtrellis.projecteuler

import org.scalatest.FlatSpec

/**
  * @author Todd Cook
  * @since 4/30/11
  */

class BigSquareRootTest extends FlatSpec {

  val bsr = new BigSquareRoot()

  "BigSquare root of 9 " should "be 3" in {
    val result = bsr.get(new java.math.BigInteger("9"))
    assert(result.longValue === 3L)
  }

  /**
    * The following is necessary because the representations are different
    *
    * scala> var bd =  new java.math.BigDecimal ("0.00000000000000000000")
    * bd: java.math.BigDecimal = 0E-20
    * scala> var bd2 =  new java.math.BigDecimal ("0.0")
    * bd2: java.math.BigDecimal = 0.0
    *
    */

  "BigSquare root of 961 " should "be 31" in {
    val result = bsr.get(new java.math.BigInteger("961"))
    assert(result.longValue === 31L)
    assert(bsr.error.compareTo(bsr.ZERO) === 0)
  }


}