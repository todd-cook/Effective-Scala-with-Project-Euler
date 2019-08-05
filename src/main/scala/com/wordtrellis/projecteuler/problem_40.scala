package com.wordtrellis.projecteuler

/**
  * Problem 40
  *
  * An irrational decimal fraction is created by concatenating the positive
  * integers:
  *
  * 0.123456789101112131415161718192021...
  *
  * It can be seen that the 12^(th) digit of the fractional part is 1.
  *
  * If d_(n) represents the n^(th) digit of the fractional part, find the value
  * of the following expression.
  *
  * d_(1) * d_(10) * d_(100) * d_(1000) * d_(10000) * d_(100000) * d_(1000000)
  *
  * @author : Todd Cook
  *
  */
object problem_40 {

  val candidates: List[Int] = (0 to 1000000).toList

  /**
    * Binary search implementation
    * not used in solving the optimization of this problem, but
    * included anyway to see how it was modified to discover the target
    * see binarySearchFun() below
    */
  def binarySearch(values: List[Int], goal: Int): Int = {
    var topPlace    = values.length - 1
    var midPlace    = (values.length - 1) / 2
    var botPlace    = 0
    var topValue    = values(topPlace)
    var bottomValue = values(botPlace)
    var midpoint    = values(midPlace)
    val start       = values(midpoint)
    var process     = true
    var ii          = 0

    while (process == true) {
      ii += 1
      val result = createIrrationalDecimalString(midpoint).length
      println("topValue: \t\t" + values(topPlace))
      println("midpoint: \t\t" + values(midPlace))
      println("bottomValue: \t\t" + values(botPlace))
      println("result: \t\t" + result + "\n")

      if (result == goal) {
        println("Final result: \t\t" + midpoint)
        println("via " + ii + " iterations")
        process = false
      }
      if (result < goal) {
        botPlace = midPlace
        midPlace = (topPlace - midPlace) / 2 + botPlace
        midpoint = values(midPlace)
      }
      if (result > goal) {
        topPlace = midPlace
        midPlace = (topPlace - botPlace) / 2 + botPlace
        midpoint = values(midPlace)
      }
    }
    start
  }

  def main(args: Array[String]): Unit = {
    println(answer)
  }

  def answer: Int = {

    /**
      * Originally this answer was arrived at by brute force, e.g.
      *   var digits = (0 to 1000000).toList.mkString ("")
      * However that approach required increasing memory to accommodate the huge amount of data;
      * using a binary search styled narrowing approach, the optimum length was found and
      * this doesn't require extra memory being allocated
      */
    val digits = (1 to 185185).toList.mkString("")

    println(
      java.lang.Integer.parseInt(s"${digits(1 - 1)}") *
        java.lang.Integer.parseInt(s"${digits(10 - 1)}") *
        java.lang.Integer.parseInt(s"${digits(100 - 1)}") *
        java.lang.Integer.parseInt(s"${digits(1000 - 1)}") *
        java.lang.Integer.parseInt(s"${digits(10000 - 1)}") *
        java.lang.Integer.parseInt(s"${digits(100000 - 1)}") *
        java.lang.Integer.parseInt(s"${digits(1000000 - 1)}"))

    // see commentary below
    binarySearchFun(candidates, testCandidate(_), 1000000)
    // demonstration of clause that catches inexact solution
    binarySearchFun(candidates, testCandidate(_), 1000001)
  }

  /**
    * returns:
    * -1 = less than goal
    * 0 = goal
    * 1 = more than goal
    *
    */
  def testCandidate(candidate: Int): Int = {
    createIrrationalDecimalString(candidate).length.asInstanceOf[Int]
  }

  def createIrrationalDecimalString(places: Int): String = (1 to places).toList.mkString("")

  /**
    * A sort of binary search for quickly finding the closest target
    * of course, values must be a sorted list
    */
  def binarySearchFun(values: List[Int], test: Int => Int, goal: Int): Int = {
    var topPlace    = values.length - 1
    var midPlace    = (values.length - 1) / 2
    var botPlace    = 0
    var topValue    = values(topPlace)
    var bottomValue = values(botPlace)
    var midpoint    = values(midPlace)
    val start       = values(midpoint)
    var process     = true
    var ii          = 0

    while (process == true) {
      ii += 1
      val result = test(midpoint)
      println("topValue: \t\t" + values(topPlace))
      println("midpoint: \t\t" + values(midPlace))
      println("bottomValue: \t\t" + values(botPlace))
      println("result: \t\t" + result + "\n")
      if (result == goal) {
        println("Final result: \t\t" + midpoint)
        println("via " + ii + " iterations")
        process = false
      }
      if (result < goal) {
        botPlace = midPlace
        midPlace = (topPlace - midPlace) / 2 + botPlace
        midpoint = values(midPlace)
      }
      if (result > goal) {
        topPlace = midPlace
        midPlace = (topPlace - botPlace) / 2 + botPlace
        midpoint = values(midPlace)
      }
      // the following is necessary to catch conditions
      //  which won't yeild an exact solution
      if (midPlace == botPlace || midPlace == topPlace) {
        println("Closest approximation:\t" + midpoint)
        println("result: \t\t" + result)
        println("via " + ii + " iterations")
        process = false
      }
    }
    start
  }
}

/**
  * Commentary:
  *
  * Optimization problem:
  * Determine the min value to reach one million irrational decimal places
  * one million digits is reached before one millions digits are concatenated
  * together what is the closest number that generates one million digits
  *
  *  naive solution takes forever
  *        var ii = 1000000
  *        while( (0 to ii).toList.mkString("").length > 1000000)
  * {ii -= 1}
  *        println (ii)
  *
  *  Better: exploit the linear nature of the possible solutions by
  *  using a modified binary search:
  *
  *
Results:

topValue: 		1000000
midpoint: 		500000
bottomValue: 		0
result: 		2888896

topValue: 		500000
midpoint: 		250000
bottomValue: 		0
result: 		1388896

topValue: 		250000
midpoint: 		125000
bottomValue: 		0
result: 		638896

topValue: 		250000
midpoint: 		187500
bottomValue: 		125000
result: 		1013896

topValue: 		187500
midpoint: 		156250
bottomValue: 		125000
result: 		826396

topValue: 		187500
midpoint: 		171875
bottomValue: 		156250
result: 		920146

topValue: 		187500
midpoint: 		179687
bottomValue: 		171875
result: 		967018

topValue: 		187500
midpoint: 		183593
bottomValue: 		179687
result: 		990454

topValue: 		187500
midpoint: 		185546
bottomValue: 		183593
result: 		1002172

topValue: 		185546
midpoint: 		184569
bottomValue: 		183593
result: 		996310

topValue: 		185546
midpoint: 		185057
bottomValue: 		184569
result: 		999238

topValue: 		185546
midpoint: 		185301
bottomValue: 		185057
result: 		1000702

topValue: 		185301
midpoint: 		185179
bottomValue: 		185057
result: 		999970

topValue: 		185301
midpoint: 		185240
bottomValue: 		185179
result: 		1000336

topValue: 		185240
midpoint: 		185209
bottomValue: 		185179
result: 		1000150

topValue: 		185209
midpoint: 		185194
bottomValue: 		185179
result: 		1000060

topValue: 		185194
midpoint: 		185186
bottomValue: 		185179
result: 		1000012

topValue: 		185186
midpoint: 		185182
bottomValue: 		185179
result: 		999988

topValue: 		185186
midpoint: 		185184
bottomValue: 		185182
result: 		1000000

Final result: 		185184
via 19 iterations

...

topValue: 		185186
midpoint: 		185185
bottomValue: 		185184
result: 		1000006

Closest approximation:	185184
result: 		1000006
via 20 iterations

  */
