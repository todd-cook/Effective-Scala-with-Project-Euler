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

import collection.mutable.ListBuffer

/**
 * Problem 38
 * Take the number 192 and multiply it by each of 1, 2, and 3:
 *  192 * 1 = 192
 *  192 * 2 = 384
 *  192 * 3 = 576
 *
 *  By concatenating each product we get the 1 to 9 pandigital, 192384576.
 *  We will call 192384576 the concatenated product of 192 and (1,2,3)
 *
 *  The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
 *  and 5, giving the pandigital, 918273645, which is the concatenated product
 *  of 9 and (1,2,3,4,5).
 *  What is the largest 1 to 9 pandigital 9-digit number that can be formed as
 *  the concatenated product of an integer with (1,2, ... , n) where n > 1?
 *
 * @author Todd Cook
 * @since 4/24/2011
 */

object problem_38 {

  def isPandigital(x: Int) = (x.toString.toList.distinct.size == 9)

  def productToNineDigits(x: Int): Int = {
    var result = ""
    var ii = 2
    while (result.length < 9) {
      var tmp = for {a <- (1 to ii).toList
      } yield {
        (x * a)
      }
      result = tmp.mkString("")
      ii += 1
    }
    if (result.length > 9) {
      return 0
    }
    result.toInt
  }

  def answer = {
    val candidates = new ListBuffer[Int]()
    (1 to 1000000).foreach(a => {
      var prod = productToNineDigits(a)

      if (prod.toString.indexOf("0") == -1) {
        if (isPandigital(prod)) {
          println("%d yields: %d".format(a, productToNineDigits(a)))
          candidates.append(productToNineDigits(a))
        }
      }
    })
    candidates.toList.sortWith(_ > _)(0)
  }

  def main(args: Array[String]) = {
    println(answer)
  }
}