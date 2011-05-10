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

/**
 * Problem 30
 *
 * Surprisingly there are only three numbers that can be written as the sum of fourth powers
 * of their digits:
 *
 *  1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
 *  8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
 *  9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)
 *
 *  As 1 = 1^(4) is not a sum it is not included.
 *
 *  The sum of these numbers is 1634 + 8208 + 9474 = 19316.
 *
 *  Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
 *
 * @author : Todd Cook
 * @since : 5/7/2011
 */

import collection.mutable.ListBuffer

object problem_30 {
  /**
   * note the odd construction (a +"") below is required to coerce Char to String otherwise
   * the character code value of the digit as a character will be given instead,
   * e.g. 1, 2, 3  ->  49, 50, 51
   */
  def stringAsDigitList( str:String) = str.toList.map (a =>  java.lang.Integer.parseInt( a +""))

  def numberAsDigitList (num: Int): List[Int] = stringAsDigitList(num.toString)

  def testNumberEqualsPower (num: Int, power:Int) = (numberDigitsToPower(num, power) == num + 0d)

  // note: must initialize the fold left first argument to set type
  def numberDigitsToPower (num: Int, power:Int) =
    numberAsDigitList(num).map(a => math.pow(a, power)).foldLeft(0d)(_ + _)

  def answer = {
    val solutionRange = (2 to 200000) // or wind it up to 10000000, same results
    val solutions = new ListBuffer[Int]()
    solutionRange.foreach(x => if (testNumberEqualsPower(x, 5)) solutions.append(x) )
    println(solutions.toList)
    solutions.toList.sum
  }

   def main (args: Array[String]) = {
    println(answer)
  }
}

/**
 * Solution:
 * List(4150, 4151, 54748, 92727, 93084, 194979)
 * 443839
 *
 */

