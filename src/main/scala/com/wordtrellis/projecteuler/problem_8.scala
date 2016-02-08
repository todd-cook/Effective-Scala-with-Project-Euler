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

package com.wordtrellis.projecteuler

/**
Problem 8
Find the greatest product of five consecutive digits in the 1000-digit number.

73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 */

import scala.collection.mutable.ListBuffer

object problem_8 {

  val thousandDigitNumber = (
    "73167176531330624919225119674426574742355349194934" +
      "96983520312774506326239578318016984801869478851843" +
      "85861560789112949495459501737958331952853208805511" +
      "12540698747158523863050715693290963295227443043557" +
      "66896648950445244523161731856403098711121722383113" +
      "62229893423380308135336276614282806444486645238749" +
      "30358907296290491560440772390713810515859307960866" +
      "70172427121883998797908792274921901699720888093776" +
      "65727333001053367881220235421809751254540594752243" +
      "52584907711670556013604839586446706324415722155397" +
      "53697817977846174064955149290862569321978468622482" +
      "83972241375657056057490261407972968652414535100474" +
      "82166370484403199890008895243450658541227588666881" +
      "16427171479924442928230863465674813919123162824586" +
      "17866458359124566529476545682848912883142607690042" +
      "24219022671055626321111109370544217506941658960408" +
      "07198403850962455444362981230987879927244284909188" +
      "84580156166097919133875499200524063689912560717606" +
      "05886116467109405077541002256983155200055935729725" +
      "71636269561882670428252483600823257530420752963450")

  def findLargestProduct (bigNumber: String, digitsRequired: Int): Long = {
    val maxDigit = bigNumber.length
    // Load the big digit string into a list of Integers, for easy access
    val tmpDigits = new ListBuffer[Int]()
    (0 until maxDigit).foreach(x => tmpDigits.append(
      java.lang.Integer.valueOf(
        bigNumber.substring(x, x + 1))).asInstanceOf[Int])
    val digits = tmpDigits.toList
    // use another ListBuffer to keep track of the numbers
    val productDigits = new ListBuffer[Int]()
    (0 until digitsRequired).foreach(x => productDigits.append(digits(x)))
    // initialize the largest product to the first set of numbers
    var largestProduct = productDigits.toList.foldLeft(1L)(_ * _)
    // placeholder for next position of the digit to add
    var nextPosition = digitsRequired
    // placeholder for the winning combination
    var biggestDigits = List[Int]()

    while (nextPosition < maxDigit) {
      productDigits.remove(0)
      productDigits.append(digits(nextPosition))
      var currentProduct = productDigits.toList.foldLeft(1)(_ * _)
      if (largestProduct < currentProduct) {
        largestProduct = currentProduct
        biggestDigits = productDigits.toList
      }
      nextPosition += 1
    }
    println("Largest Product: " + largestProduct +
              " of " + digitsRequired + " consecutive digits: " + biggestDigits)
    largestProduct
  }

  def main (args: Array[String]) {
    println(findLargestProduct(thousandDigitNumber, 5))
  }
}