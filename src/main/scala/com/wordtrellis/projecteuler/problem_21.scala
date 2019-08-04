

package com.wordtrellis.projecteuler

/**
 * Problem 21
 * Let d(n) be defined as the sum of proper divisors of n
 * (numbers less than n which divide evenly into n).
 * If d(a) = b and d(b) = a, where a != b, then a and b are an amicable pair
 * and each of a and b are called amicable numbers.
 *
 * For example, the proper divisors of 220 are
 * 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
 * The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 *
 * @author : Todd Cook
 *
 */

import collection.mutable.ListBuffer

object problem_21 {

  def factors (num: Int): List[Int] = {
    val builder = new ListBuffer[Int]()
    (1 to (num / 2)).toList.foreach(x => if (num % x == 0) {
      builder.append(x)
    })
    builder.toList
  }

  def isAmicable (x: Int): Boolean = x == factors(factors(x).sum).sum && (x != factors(x).sum)

  def answer (): Unit = {
    val amicableNumbers = new ListBuffer[Int]()
    (1 to 10000).toList.foreach(x => if (isAmicable(x)) {
      //println (x)
      amicableNumbers.append(x)
    })
    println("total sum of amicable numbers 1 - 10000: " + amicableNumbers.toList.sum)
  }

  def main (args: Array[String]): Unit = {
    answer()
  }
}