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
  *
  */
import scala.collection.mutable.{HashMap, ListBuffer}

object problem_34 {

  /**
    * Int, Int map is okay because we only need factorial for 0-9
    */
  val factorialMap = new HashMap[Int, Int]()

  /**
    * Iterative
    */
  def factorial(n: Int): Double = {
    if (n == 0)
      return 1d
    if (n <= 2)
      return n.asInstanceOf[Double]
    var ii     = n
    var result = 0d
    result += n
    while (ii > 1) {
      result = result * (ii - 1)
      ii -= 1
    }
    result
  }

  /**
    * Run this to see it take forever and deliver the same results as answer2
    */
  def answer(): Unit = {
    val curiousNumbers = new ListBuffer[Int]()
    var ii             = 3
    while (ii < Integer.MAX_VALUE) {
      if (isCurious(ii)) {
        curiousNumbers.append(ii)
      }
      ii += 1
    }
    println("Curious Numbers to 10 digits: " + curiousNumbers.toList.mkString(", "))
    println("Sum of Curious Numbers to 10 digits: " + curiousNumbers.toList.sum)
  }
  (0 to 9).toList.foreach(x => factorialMap += (x -> factorial(x).toInt))

  def main(args: Array[String]): Unit = {
    answer2()
  }

  def answer2(): Unit = {
    val curiousNumbers = new ListBuffer[Int]()
    (3 to 50000).foreach(a =>
      if (isCurious(a)) {
        curiousNumbers.append(a)
    })
    println("Curious numbers to 50000: " + curiousNumbers.toList)
    println("Sum of curious numbers: " + curiousNumbers.toList.sum)

  }

  def isCurious(num: Int): Boolean = {
    val factorialList = numberAsDigitList(num).map(x => (x * 0) + factorialMap(x))
    if (factorialList.sum == num) {
      true
    } else {
      false
    }
  }

  def numberAsDigitList(num: Int): List[Int] =
    num.toString.toList.map(x => java.lang.Integer.parseInt(x.toString))
}

/**
  *
  * Solution:
  * Curious Numbers to 10 digits: 145, 40585
  * Sum of Curious Numbers to 10 digits: 40730
  *
  */
