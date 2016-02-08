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
 * Problem 34
 * 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
 *
 * Find the sum of all numbers which are equal to the sum of the factorial
 * of their digits.
 *
 * Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 */

import scala.collection.mutable.HashMap
import collection.mutable.ListBuffer

object problem_34 {


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

  def numberAsDigitList (num: Int): List[Int] =
    num.toString.toList.map(x => java.lang.Integer.parseInt(x + ""))

  /**
   * Int, Int map is okay because we only need factorial for 0-9
   */
  val factorialMap = new HashMap[Int, Int]()
  (0 to 9).toList.foreach(x => factorialMap += (x -> (factorial(x)).toInt))

  def isCurious (num: Int): Boolean = {
    val factorialList = numberAsDigitList(num).map(x => (x * 0) + factorialMap(x))
    if (factorialList.foldLeft(0)(_ + _) == num) {
      true
    }
    else {
      false
    }
  }

  /**
   * Run this to see it take forever and deliver the same results as answer2
   */
  def answer = {
    val curiousNumbers = new ListBuffer[Int]()
    var ii = 3
    while (ii < Integer.MAX_VALUE) {
      if (isCurious(ii)) {
        curiousNumbers.append(ii)
      }
      ii += 1
    }
    println("Curious Numbers to 10 digits: " + curiousNumbers.toList.mkString(", "))
    println("Sum of Curious Numbers to 10 digits: " + curiousNumbers.toList.foldLeft(0)(_ + _))
  }

  def answer2 () = {
    val curiousNumbers = new ListBuffer[Int]()
    (3 to 50000).foreach(a => if (isCurious(a)) {
      curiousNumbers.append(a)
    })
    println("Curious numbers to 50000: " + curiousNumbers.toList)
    println("Sum of curious numbers: " + curiousNumbers.toList.sum)

  }

  def main (args: Array[String]) = {
    answer2
  }
}

/**
 *
 * Solution:
 * Curious Numbers to 10 digits: 145, 40585
 * Sum of Curious Numbers to 10 digits: 40730
 *
 */
