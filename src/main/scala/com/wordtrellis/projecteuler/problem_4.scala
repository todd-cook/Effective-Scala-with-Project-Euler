package com.wordtrellis.projecteuler

/**
  *
  *  Problem 4
  *  A palindromic number reads the same both ways.
  *  The largest palindrome made from the product of two 2-digit numbers is
  *  9009 = 91 * 99.
  *  Find the largest palindrome made from the product of two 3-digit numbers.
  *
  * @author : Todd Cook
  *
  */
object problem_4 {

  def main(args: Array[String]): Unit = {
    println(findLargestPalindromicNumber(3))
  }

  def findLargestPalindromicNumber(digits: Int): (Int, Int, Int) = {
    createPalindromicNumberList(digits - 1, digits).sortWith(_._3 > _._3).head
  }

  def createPalindromicNumberList(digitPlacesStart: Int,
                                  digitPlacesEnd: Int): List[(Int, Int, Int)] = {
    require(digitPlacesStart < digitPlacesEnd)
    val palindromes =
      for (a <- (createCeilingNumber(digitPlacesStart) + 1 to
             createCeilingNumber(digitPlacesEnd)).toList;
           b <- a to createCeilingNumber(digitPlacesEnd);
           p = a * b
           if isPalindrome(p.toString)) yield (a, b, p)
    palindromes
  }

  def isPalindrome(s: String): Boolean = s.reverse.mkString == s

  def createCeilingNumber(digits: Int): Int = ("9" * digits).toInt
}
