

package com.wordtrellis.projecteuler

/**
 * Problem 17
 * If the numbers 1 to 5 are written out in words: one, two, three, four,
 * five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 *
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out
 * in words, how many letters would be used?
 *
 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
 * forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
 * 20 letters. The use of "and" when writing out numbers is in compliance
 * with British usage.
 *
 * @author : Todd Cook
 *
 */

object problem_17 {

  def removeSpaces(str: String): String = str.replaceAll("\\s", "")

  def removeHyphens(str: String): String = str.replaceAll("-", "")

  def answer: Int = {
    val result = (1 to 1000).toList.map(x => NumberString.numberToWords(x)).mkString
    removeHyphens(removeSpaces(result)).length
  }

  def main(args: Array[String]): Unit = {
    println(NumberString.parse("twenty-one"))
    println(NumberString.parse("Five hundred and twenty-one"))
    println(NumberString.parse("two thousand five hundred and twenty-one"))
    //  println(NumberString.parse("twenty-three thousand eight hundred and twenty-one")) //TODO
    println(answer)
  }
}