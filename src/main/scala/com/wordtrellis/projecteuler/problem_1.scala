

package com.wordtrellis.projecteuler

/**
 *  Problem 1
    If we list all the natural numbers below 10 that are multiples of 3 or 5,
    we get 3, 5, 6 and 9. The sum of these multiples is 23.
    Find the sum of all the multiples of 3 or 5 below 1000.
 *
 * @author : Todd Cook
 *
 */

object problem_1 {

    def factorsOfThreeOrFive (ceiling: Int): List[Int] = {
        (1 until ceiling).toList.filter (x => x % 5 == 0 || x % 3 == 0)
    }

    def sumList (digits: List[Int]): Int = digits.sum

    def answer(): Unit = println (sumList (factorsOfThreeOrFive (1000)))

    def main (args: Array[String]): Unit = answer()
}
