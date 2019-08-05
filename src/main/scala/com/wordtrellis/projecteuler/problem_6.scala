package com.wordtrellis.projecteuler

/**
  * Problem 6
  * The sum of the squares of the first ten natural numbers is,
  * 1^(2) + 2^(2) + ... + 10^(2) = 385
  * The square of the sum of the first ten natural numbers is,
  * (1 + 2 + ... + 10)^(2) = 55^(2) = 3025
  * Hence the difference between the sum of the squares of the first
  * ten natural numbers and the square of the sum is 3025 - 385 = 2640
  * Find the difference between the sum of the squares of the first
  * one hundred natural numbers and the square of the sum.
  *
  * Commentary: for the closed form expressions see:
  * Mathematics for Computer Science, by Prof. Albert R Meyer, pp.34-35, et al.
  *   http://courses.csail.mit.edu/6.042/spring11/spring10-mcs.pdf
  * @author : Todd Cook
  *
  */
object problem_6 {

  def main(args: Array[String]): Unit = {
    println(answer)
    println(differenceBetweenSumOfSquaresAndSquareOfSum(10))
  }

  def differenceBetweenSumOfSquaresAndSquareOfSum(n: Int): Int = {
    math.pow(sumOfRange(n), 2).toInt - sumOfSquares(n)
  }

  /**
    * Closed form expression for summation series
    */
  def sumOfRange(n: Int): Int = n * (n + 1) / 2

  /**
    * Closed form expression for sum of consecutive squares
    */
  def sumOfSquares(n: Int): Int = (2 * n + 1) * (n + 1) * n / 6

  /**
    * Naive brute force implementation
    */
  def answer: Int = {
    val sum            = (1 to 100).toList.sum
    val squareSum      = sum * sum
    var sumSquare: Int = 0
    (1 to 100).foreach(n => sumSquare += n * n)
    //println (squareSum - sumSquare)
    squareSum - sumSquare
  }
}
