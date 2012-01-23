/*
 * Copyright (c) 2011, Todd Cook.
 *   All rights reserved.
 *   Redistribution and use in source and binary forms, with or without modification,
 *   are permitted provided that the following conditions are met:
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
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 *   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.cookconsulting.projecteuler

import collection.mutable.HashSet

/**
 * Problem 74
 * The number 145 is well known for the property that the sum of the factorial
 * of its digits is equal to 145:
 *
 * 1! + 4! + 5! = 1 + 24 + 120 = 145
 *
 * Perhaps less well known is 169, in that it produces the longest chain of
 * numbers that link back to 169; it turns out that there are only three such
 * loops that exist:
 *
 * 169 ->  363601  ->  1454  ->  169
 * 871  ->  45361  ->  871
 * 872  ->  45362  ->  872
 *
 * It is not difficult to prove that EVERY starting number will eventually get
 * stuck in a loop. For example,
 *
 * 69  ->  363600  ->  1454  ->  169  ->  363601 ( ->  1454)
 * 78  ->  45360  ->  871  ->  45361 ( ->  871)
 * 540  ->  145 ( ->  145)
 *
 * Starting with 69 produces a chain of five non-repeating terms, but the
 * longest non-repeating chain with a starting number below one million is
 * sixty terms.
 *
 * How many chains, with a starting number below one million, contain exactly
 * sixty non-repeating terms?
 *
 * @author Todd Cook
 * @since 5/21/2011
 */

object problem_74 {

  /**
  * Iterative
  */
  def factorial (n :Int)  :Double = {
    if (n == 0)
      return 1d
    if (n <= 2)
      return n.asInstanceOf[Double];
    var ii = n
    var result = 0d
    result += n
    while (ii > 1) {
      result = result * (ii -1)
      ii -= 1
      }
    result
  }

  val digitFactorials = (0 to 9).toList.map(a => factorial(a))

  def sumDigitFactorials(n: Int): Int = n.toString.toList.map(
    a => digitFactorials(java.lang.Integer.parseInt(a + ""))).sum.toInt

  //    sumDigitFactorials (145)
  //    sumDigitFactorials (169)
  //    sumDigitFactorials (363601)
  //    sumDigitFactorials (1454)
  //    sumDigitFactorials (169)

  def countChainLength(n: Int) = {
    var seed = n
    val links = new HashSet[Int]()
    while (!links.contains(seed)) {
      links.add(seed)
      seed = sumDigitFactorials(seed)
    }
    links.size
  }

  def answer() = {
    val results =
      (1 to 1000000).toList.par.map(a => (a, countChainLength(a))).filter(b => b._2 == 60)
    println(results.toList.size)
    results
  }

  def main(args: Array[String]) = {
    println(answer)
  }
}