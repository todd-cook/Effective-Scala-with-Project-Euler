

package com.wordtrellis.projecteuler

import java.math.BigInteger

/**
 * Problem 20
 * n! means n * (n - 1) * ... * 3 * 2 * 1
 * Find the sum of the digits in the number 100!
 *
 * @author : Todd Cook
 *
 */

object problem_20 {

  /**
  * Iterative
  */
  def factorialBI (n :Int) :BigInteger = {
    if (n == 0)
      return BigInteger.ONE
    if (n <= 2)
      return new BigInteger(n.toString)
    var ii = n
    var result = new BigInteger(n.toString)
    while (ii > 1) {
      result = result.multiply  ( new BigInteger( ( ii -1).toString))
      ii -= 1
      }
    result
  }

  def main(args: Array[String]): Unit = {
    val fact100 = factorialBI(100).toString
    println(fact100.toList.map(a => a + "").map(b => java.lang.Integer.parseInt(b)).sum)
  }
}

/**
 * Solution:
 * result : 9.332621544394418E157 sum: 2.5368695556012736E158
 * res11: Double = 9.332621544394418E157
 */