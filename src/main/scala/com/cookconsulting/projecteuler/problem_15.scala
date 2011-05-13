/*
 * Copyright (c) 2011, Todd Cook.
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without modification,
 *  are permitted provided that the following conditions are met:
 *
 *      * Redistributions of source code must retain the above copyright notice,
 *        this list of conditions and the following disclaimer.
 *      * Redistributions in binary form must reproduce the above copyright notice,
 *        this list of conditions and the following disclaimer in the documentation
 *        and/or other materials provided with the distribution.
 *      * Neither the name of the <ORGANIZATION> nor the names of its contributors
 *        may be used to endorse or promote products derived from this software
 *        without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.cookconsulting.projecteuler

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
 * @since : 4/24/2011
 */

import scala.collection.mutable.ListBuffer

object problem_15 {

  def answer () = {
    // see PascalsTriangle for an explanation of why we don't use the formula (2n)! / (n!)^2
    // of course we could write if for BigInteger but where's the fun in that? It's a good
    // exercise for the reader. Do you really need a Binomial coefficient bigger than a long?
    // Let me know.
    val pt = new PascalsTriangle()
    println(pt.centralBinomialCoefficients)
    println((pt.centralBinomialCoefficients())(20))
  }

  /**
   * Initial attempts that yielded results indicating the governing factor was a common series:
   * the central binomial coefficient
   */
  def shiftList (myList: List[Int], shift: Int) = myList.slice(shift, myList.length - shift)

  def createTupleGrid (x: Int, y: Int) = {
    val results = new ListBuffer[Tuple2[Int, Int]]()
    (0 to x).foreach(n => {
      (0 to y).foreach(m => {
        results.append(Tuple2[Int, Int](n, m))
      })
    })
    results.toList
  }

  def findAnswer (x: Int, y: Int): List[List[Tuple2[Int, Int]]] = {
    var ug = new UndirectedGraph()
    var tupleGrid = createTupleGrid(x, y)
    var startingPoint = List((0, 0))
    val totalLeapsRequired = (x + y + 1)
    ug.addAllVertices(tupleGrid)
    var results: List[List[Tuple2[Int, Int]]] = List(startingPoint)
    while (results(0).size < totalLeapsRequired) {
      results = ug.findNextVertices(results)
    }
    results.filter(z => z.last == (x, y))
  }

  def factorial (x: Int) = (1 to x).toList.foldLeft(1)(_ * _)

  def main (args: Array[String]) = {
    // initial attempts that yielded the paths and the series of numbers...
    (2 to 8).foreach(x => {
      println("finding " + x + " by " + x + " grid possibilities")
      var results = findAnswer(x, x)
      println(results.mkString(", "))
      println(results.length)
    })
    answer
  }
}