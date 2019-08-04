
package com.wordtrellis.projecteuler

import org.scalatest.FlatSpec

import scala.collection.mutable.ListBuffer

/**
  * Frequently, Project Euler problems will have an example that may be used
  * to test the behavior of a generified function solution.
  * Here we provide unit tests as a basic verification of the algorithm
  * implementations.
  * We purposely exclude any problems that have an excessively long running time
  *
  * @author : Todd Cook
  *
  */

class ProblemVerifierTest extends FlatSpec {

  "Problem 1" should "have sum of 23 for the factors of three or five under 10" in {
    assert(problem_1.factorsOfThreeOrFive(10) === List(3, 5, 6, 9))
    assert(problem_1.sumList(problem_1.factorsOfThreeOrFive(10)) == 23)
  }

  "Problem 4: largest palindromic number of two digits" should "be 9009" in {
    assert(problem_4.findLargestPalindromicNumber(2) ===
      Tuple3(91, 99, 9009))
  }


  "Problem 4: largest palindromic number of three digits" should "be 906609" in {
    assert(problem_4.findLargestPalindromicNumber(3) ===
      Tuple3(913, 993, 906609))
  }


  "Problem 5: smallest number divisible by range 1-10" should "be  2520" in {
    assert(problem_5.calcSmallestNumberDivisibleByEntireRange(10) === 2520)
  }


  "Problem 5: smallest number divisible by range 1-20" should "be  232792560" in {
    assert(problem_5.calcSmallestNumberDivisibleByEntireRange(20) === 232792560)
  }


  "Problem 6: Sum of Squares" should "have 3025 for 10,2 " in {
    assert(math.pow(problem_6.sumOfRange(10), 2) === 3025)
    assert(problem_6.sumOfSquares(10) === 385)
    assert(problem_6.differenceBetweenSumOfSquaresAndSquareOfSum(10) === 2640)
  }


  "Problem 7: " should "have get the first six primes" in {
    val shortPrimeList = problem_7.getPrimeList(6)
    // 2, 3, 5, 7, 11, and 13
    val desiredResult = List(2, 3, 5, 7, 11, 13)
    assert(shortPrimeList === desiredResult)
    shortPrimeList.foreach(x => if (!problem_7.isPrime(x)) {
      fail("prime failure")
    })
  }

  /*   java.lang.ClassCastException: scala.collection.mutable.ListBuffer cannot be cast to java.lang.Integer
  [info]   at scala.runtime.BoxesRunTime.unboxToInt(BoxesRunTime.java:100)
  [info]
  */
  //  "Problem 8" should "have " in {
  //    assert(problem_8.findLargestProduct("1234560888888888882111", 5) === 32768)
  //  }

  "Problem 10: Sum of 2 million primes" should "have result of: 1.42913828922E11" in {
    val atk = new SieveOfAtkin(2000 * 1000)
    val primes = atk.getPrimes()
    primes.foreach(x => if (!problem_7.isPrime(x)) {
      println("prime failure for: " + x)
    })
    val atkinSum = problem_10.sumBigList(primes)
    assert(atkinSum === 1.42913828922E11)
  }


  "Problem 12: Verify triangle numbers" should "succeed for 1, 3, 6, 10, 15, 21" in {
    val data = List(1, 3, 6, 10, 15, 21)
    data.foreach(x => assert(problem_12.isTriangleNumber(x.toLong) === true))
  }

  "Problem 17: when numbers are written out as words, how many letters " should " be 23 for 342" in {
    assert((1 to 5).toList.map(x =>
      problem_17.removeHyphens(
        problem_17.removeSpaces(
          NumberString.numberToWords(x)))).mkString("").length === 19)
    assert(problem_17.removeHyphens(
      problem_17.removeSpaces(
        NumberString.numberToWords(342).mkString(""))).length === 23)
    assert(problem_17.removeHyphens(
      problem_17.removeSpaces(
        NumberString.numberToWords(115).mkString(""))).length === 20)
  }

  "Problem 24" should "have 6 lexicographic permutations of 0, 1 and 2" in {
    /**
      * The lexicographic permutations of 0, 1 and 2 are:
      * 012   021   102   120   201   210
      */
    val pg = new PermutationGenerator(3)
    assert(pg.next().map(a => a - 1).mkString("") === "012")
    assert(pg.next().map(a => a - 1).mkString("") === "021")
    assert(pg.next().map(a => a - 1).mkString("") === "102")
    assert(pg.next().map(a => a - 1).mkString("") === "120")
    assert(pg.next().map(a => a - 1).mkString("") === "201")
    assert(pg.next().map(a => a - 1).mkString("") === "210")
  }

  "Problem 28" should "have diagonal sums of 101 for the number square" in {
    //21 22 23 24 25
    //20  7  8  9 10
    //19  6  1  2 11
    //18  5  4  3 12
    //17 16 15 14 13
    //
    //It can be verified that the sum of the numbers on the diagonals is 101.
    val sng = new SpiralNumberGrid(5, 1)
    //    println(sng)
    val tlbrDiagonal = sng.getTopLeftToBottomRightDiagonal
    val bltrDiagonal = sng.getBottomLeftToTopRightDiagonal
    //    println("top left, bottom right: " + tlbrDiagonal)
    //    println("sum : %d ".format(tlbrDiagonal.sum))
    //    println("bottom left to top right: " + bltrDiagonal)
    //    println("sum : %d ".format(bltrDiagonal.sum))
    //    println("total: %d ".format(tlbrDiagonal.sum + bltrDiagonal.sum -1 ))
    assert(101 === (tlbrDiagonal.sum + bltrDiagonal.sum - 1))
  }

  "Problem 30: the sum of all the numbers that can be written as the sum of fourth powers of their digits." should "be " in {

    val solutionRange = 2 to 10000000
    val solutions = new ListBuffer[Int]()
    solutionRange.foreach(x => if (problem_30.testNumberEqualsPower(x, 4)) {
      solutions.append(x)
    })
    assert(solutions.toList === List(1634, 8208, 9474))
  }

  "Problem 32:" should " yield pandigital permutations" in {
    assert(problem_32.formPandigitalProductCombo(List(3, 9, 1, 8, 6, 7, 2, 5, 4))
      === (39, 186, 7254))
    assert(problem_32.checkPandigitalPermutation(
      problem_32.formPandigitalProductCombo(List(3, 9, 1, 8, 6, 7, 2, 5, 4))) === true)
  }

  "Problem 45" should "yield pentagonal numbers" in {
    val pentagons = List(1L, 5L, 12L, 22L, 35L, 40755L)
    pentagons.foreach(a => assert(problem_45.isPentagonal(a)))
  }

  //  def test_problem_45_Quadratic() {
  //    assert(problem_45.recoverSeeds(problem_45.triangleNumber(285L)) === (285L, 165L, 143L))
  //  }
  //
}