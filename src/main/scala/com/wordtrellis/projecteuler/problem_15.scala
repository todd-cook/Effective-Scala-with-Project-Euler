package com.wordtrellis.projecteuler

/**
  * Problem 15
  * Starting in the top left corner of a 2 * 2 grid, there are 6 routes (without backtracking)
  * to the bottom right corner.
  * How many routes are there through a 20 * 20 grid?
  *
  * o - o - o
  * |   |   |
  * o - o - o
  * |   |   |
  * o - o - o
  *
  * Commentary:
  *  Frequently Project Euler problems give you just enough information to perform the algorithm,
  *  but not enough sample data to deduce the shortcut.
  *  Diagram out the possibilities for the 6 solutions, then do so for 4 point graph, yielding 20
  *  variations. A four point grid yields 2 paths to the required destination. The numbers form
  *  a predictable sequence, the central binomial coefficient.
  *
  * @author : Todd Cook
  *
  */
import scala.collection.mutable.ListBuffer

object problem_15 {

  /**
    * Initial attempts that yielded results indicating the governing factor was a common series:
    * the central binomial coefficient
    */
  def shiftList(myList: List[Int], shift: Int): List[Int] =
    myList.slice(shift, myList.length - shift)

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

  def main(args: Array[String]): Unit = {
    // initial attempts that yielded the paths and the series of numbers...
    (2 to 8).foreach(x => {
      println("finding " + x + " by " + x + " grid possibilities")
      val results = findAnswer(x, x)
      println(results.mkString(", "))
      println(results.length)
    })
    answer()
  }

  def answer(): Unit = {
    // see PascalsTriangle for an explanation of why we don't use the formula (2n)! / (n!)^2
    // of course we could write if for BigInteger but where's the fun in that? It's a good
    // exercise for the reader. Do you really need a Binomial coefficient bigger than a long?
    // Let me know.
    val pt = new PascalsTriangle()
    println(pt.centralBinomialCoefficients())
    println(pt.centralBinomialCoefficients()(20))
  }

  def findAnswer(x: Int, y: Int): List[List[(Int, Int)]] = {
    val ug                 = new UndirectedGraph()
    val tupleGrid          = createTupleGrid(x, y)
    val startingPoint      = List((0, 0))
    val totalLeapsRequired = x + y + 1
    ug.addAllVertices(tupleGrid)
    var results: List[List[(Int, Int)]] = List(startingPoint)
    while (results(0).size < totalLeapsRequired) {
      results = ug.findNextVertices(results)
    }
    results.filter(z => z.last == (x, y))
  }

  def createTupleGrid(x: Int, y: Int): List[(Int, Int)] = {
    val results = new ListBuffer[(Int, Int)]()
    (0 to x).foreach(n => {
      (0 to y).foreach(m => {
        results.append(Tuple2[Int, Int](n, m))
      })
    })
    results.toList
  }
}
