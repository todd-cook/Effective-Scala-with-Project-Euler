

package com.wordtrellis.projecteuler

/**
 * Problem 5
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without
 * any remainder.
 * What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
 *
 * @author : Todd Cook
 *
 */

object problem_5 {

  def calcSmallestNumberDivisibleByEntireRange (ceiling: Int): Int = {
    (1 to ceiling).foldLeft(1) {   (product, n) => {
                                   val r_raw = product % n
                                   val r = if (r_raw == 0) n else r_raw
                                   product * (if (n % r == 0)  n / r  else n)
                                 }
                               }
  }

  def answer: Int = {
    calcSmallestNumberDivisibleByEntireRange(20)
  }

  def main (args: Array[String]): Unit = {
    println(answer)
  }
}