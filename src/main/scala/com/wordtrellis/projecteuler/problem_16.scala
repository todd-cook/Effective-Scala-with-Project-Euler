

package com.wordtrellis.projecteuler

/**
 * Problem 16
 * 2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 *
 * What is the sum of the digits of the number 2^(1000)?
 *
 *
 * @author : Todd Cook
 *
 */

object problem_16 {

  def answer(): Int = {
    val bi = new java.math.BigInteger("2")
    val digitList = bi.pow(1000).toString.toList.map(a => java.lang.Integer.parseInt(a + ""))
    digitList.sum
  }

  def main(args: Array[String]): Unit = {
    println(answer())
  }
}