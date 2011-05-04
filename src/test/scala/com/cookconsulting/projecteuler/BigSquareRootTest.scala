/*
* Copyright (c) 2011, Todd Cook.
*  All rights reserved.
*
*  Redistribution and use in source and binary forms, with or without modification,
*  are permitted provided that the following conditions are met:
*
*      * Redistributions of source code must retain the above copyright notice,
*        this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright notice,
*        this list of conditions and the following disclaimer in the documentation
*        and/or other materials provided with the distribution.
*      * Neither the name of the <ORGANIZATION> nor the names of its contributors
*        may be used to endorse or promote products derived from this software
*        without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
*  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
*  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
*  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package com.cookconsulting.projecteuler

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

/**
 * @author Todd Cook
 * @since 4/30/11
 */

class BigSquareRootTest extends AssertionsForJUnit {

  @Test
  def test_BigSquareRoot () {
    var bsr = new BigSquareRoot()
    var result = bsr.get(new java.math.BigInteger("9"))
    println(result)
    assert(result.longValue === 3L)

    /**
     * The following is necessary because the representations are different
     *
     * scala> var bd =  new java.math.BigDecimal ("0.00000000000000000000")
     * bd: java.math.BigDecimal = 0E-20
     * scala> var bd2 =  new java.math.BigDecimal ("0.0")
     *  bd2: java.math.BigDecimal = 0.0
     *
     */
    assert(bsr.error.compareTo(bsr.ZERO) === 0)

    result = bsr.get(new java.math.BigInteger("961"))
    println(result)
    assert(result.longValue === 31L)
    assert(bsr.error.compareTo(bsr.ZERO) === 0)

    result = bsr.get(new java.math.BigInteger("2999"))
    println(result)
    println(bsr.error)
  }

}