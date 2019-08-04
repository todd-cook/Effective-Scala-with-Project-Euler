
package com.wordtrellis.projecteuler

/**
 * Problem 36
 *
 *  The decimal number, 585 = 1001001001_(2) (binary), is palindromic
 *  in both bases.
 *
 *  Find the sum of all numbers, less than one million, which are palindromic
 *  in base 10 and base 2.
 *
 *  (Please note that the palindromic number, in either base, may not
 *  include leading zeros.)
 *
 * @author : Todd Cook
 *
 */

object problem_36 {

  // from prob 4
  def isPalindrome (s: String): Boolean = s.reverse.mkString == s

  def answer: Int = {
    val palindromes = for {x <- 1 to 1000000
                           if isPalindrome(x.toString) &&
                             isPalindrome(java.lang.Integer.toBinaryString(x))
    } yield {
      x
    }
    println(palindromes.toList)
    palindromes.sum
  }

  def main (args: Array[String]): Unit = {
    println(answer)
  }
}