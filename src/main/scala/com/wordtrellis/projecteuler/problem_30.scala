package com.wordtrellis.projecteuler

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

  */
import scala.collection.mutable.ListBuffer

object problem_30 {

  def main(args: Array[String]): Unit = {
    println(answer)
  }

  def answer: Int = {
    val solutionRange = 2 to 200000 // or wind it up to 10000000, same results
    val solutions     = new ListBuffer[Int]()
    solutionRange.foreach(x => if (testNumberEqualsPower(x, 5)) solutions.append(x))
    println(solutions.toList)
    solutions.toList.sum
  }

  def testNumberEqualsPower(num: Int, power: Int): Boolean =
    numberDigitsToPower(num, power) == num + 0d

  // note: must initialize the fold left first argument to set type
  def numberDigitsToPower(num: Int, power: Int): Double =
    numberAsDigitList(num).map(a => math.pow(a, power)).foldLeft(0d)(_ + _)

  def numberAsDigitList(num: Int): List[Int] = stringAsDigitList(num.toString)

  /**
    * note the odd construction (a +"") below is required to coerce Char to String otherwise
    * the character code value of the digit as a character will be given instead,
    * e.g. 1, 2, 3  ->  49, 50, 51
    */
  def stringAsDigitList(str: String): List[Int] =
    str.toList.map(a => java.lang.Integer.parseInt(a .toString))
}

/**
  * Solution:
  * List(4150, 4151, 54748, 92727, 93084, 194979)
  * 443839
  *
  */
