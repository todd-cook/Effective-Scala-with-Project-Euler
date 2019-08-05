package com.wordtrellis.projecteuler

/**
  * Problem 14
  * The following iterative sequence is defined for the set of positive integers:
  *   n -> n/2 (n is even)
  *   n -> 3n + 1 (n is odd)
  *   Using the rule above and starting with 13, we generate the following sequence:
  *   13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
  *   It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
  *   Although it has not been proved yet (Collatz Problem), it is thought that all starting
  *   numbers finish at 1.
  *
  *   Which starting number, under one million, produces the longest chain?
  *   NOTE: Once the chain starts the terms are allowed to go above one million.
  *
  * @author : Todd Cook
  *
  */
object problem_14 {

  def main(args: Array[String]): Unit = {
    println(answer())
  }

  def answer(): (Long, Int) = {
    var longestResult = Tuple2[Long, Int](1L, 1)
    var ii            = 999999L
    while (ii > 1) {
      val candidate = process(ii)
      if (candidate._2 > longestResult._2) {
        longestResult = candidate
      }
      ii -= 1
    }
    longestResult
  }

  def process(num: Long): (Long, Int) = {
    var currNumber    = num
    var sequenceCount = 1
    while (currNumber != 1) {
      if (currNumber % 2 == 0) {
        currNumber = processEven(currNumber)
      } else {
        currNumber = processOdd(currNumber)
      }
      sequenceCount += 1
    }
    (num, sequenceCount)
  }

  def processEven(num: Long): Long = num / 2

  def processOdd(num: Long): Long = 3 * num + 1
}
