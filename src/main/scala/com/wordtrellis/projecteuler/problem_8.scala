package com.wordtrellis.projecteuler

/**
Problem 8
Find the greatest product of five consecutive digits in the 1000-digit number.

73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
  *
  * @author : Todd Cook
  *
  */
import scala.collection.mutable.ListBuffer

object problem_8 {

  val thousandDigitNumber: String = ("73167176531330624919225119674426574742355349194934" +
    "96983520312774506326239578318016984801869478851843" +
    "85861560789112949495459501737958331952853208805511" +
    "12540698747158523863050715693290963295227443043557" +
    "66896648950445244523161731856403098711121722383113" +
    "62229893423380308135336276614282806444486645238749" +
    "30358907296290491560440772390713810515859307960866" +
    "70172427121883998797908792274921901699720888093776" +
    "65727333001053367881220235421809751254540594752243" +
    "52584907711670556013604839586446706324415722155397" +
    "53697817977846174064955149290862569321978468622482" +
    "83972241375657056057490261407972968652414535100474" +
    "82166370484403199890008895243450658541227588666881" +
    "16427171479924442928230863465674813919123162824586" +
    "17866458359124566529476545682848912883142607690042" +
    "24219022671055626321111109370544217506941658960408" +
    "07198403850962455444362981230987879927244284909188" +
    "84580156166097919133875499200524063689912560717606" +
    "05886116467109405077541002256983155200055935729725" +
    "71636269561882670428252483600823257530420752963450")

    def main(args: Array[String]) :Unit = {
    println(findLargestProduct(thousandDigitNumber, 5))
  }

  def findLargestProduct(bigNumber: String, digitsRequired: Int): Long = {
    val maxDigit = bigNumber.length
    // Load the big digit string into a list of Integers, for easy access
    val tmpDigits = new ListBuffer[Int]()
    (0 until maxDigit).foreach(x =>
      tmpDigits.append(java.lang.Integer.valueOf(bigNumber.substring(x, x + 1))).asInstanceOf[Int])
    val digits = tmpDigits.toList
    // use another ListBuffer to keep track of the numbers
    val productDigits = new ListBuffer[Int]()
    (0 until digitsRequired).foreach(x => productDigits.append(digits(x)))
    // initialize the largest product to the first set of numbers
    var largestProduct = productDigits.toList.foldLeft(1L)(_ * _)
    // placeholder for next position of the digit to add
    var nextPosition = digitsRequired
    // placeholder for the winning combination
    var biggestDigits = List[Int]()

    while (nextPosition < maxDigit) {
      productDigits.remove(0)
      productDigits.append(digits(nextPosition))
      val currentProduct = productDigits.toList.foldLeft(1)(_ * _)
      if (largestProduct < currentProduct) {
        largestProduct = currentProduct
        biggestDigits = productDigits.toList
      }
      nextPosition += 1
    }
    println(
      "Largest Product: " + largestProduct +
        " of " + digitsRequired + " consecutive digits: " + biggestDigits)
    largestProduct
  }
}
