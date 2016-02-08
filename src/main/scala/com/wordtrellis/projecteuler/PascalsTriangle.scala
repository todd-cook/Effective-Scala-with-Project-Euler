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

package com.wordtrellis.projecteuler

import collection.mutable.ListBuffer

/**
 * Pascal's triangle built using the Java Long datatype; maximum row is 67
 *
 * @author Todd Cook
 * @since 5/2/11 11:52 PM
 */

class PascalsTriangle (val rows: Int = 67) {

  /**
   * This implementation doesn't support negative numbers,
   * however it can be done;
   * see:  http://en.wikipedia.org/wiki/Pascal%27s_triangle#Extensions
   */
  require(rows > 1)

  /**
   * Values inside Pascal's triangle at row 68 (index 67) will overflow the
   * long datatype's maximum value of 9223372036854775807
   */
  require(rows < 68)

  private var triangle = List(Array[Long](rows))
  private var builder = new ListBuffer[Array[Long]]()
  // construct the triangle:
  Iterator.range (1, rows + 1).foreach (x => builder.append(new Array[Long](rows)))
  triangle = builder.toList
  // the first column is all ones
  Iterator.range(0, rows).foreach (y => triangle(y)(0) = 1L)
  // the second column is the natural numbers
  Iterator.range(1, rows).foreach (y => triangle(y)(1) = y.toLong)
  // populate the right edge of the triangle
  Iterator.range(0, rows).foreach (y => triangle(y)(y) = 1L)

  /**
   * Since we've already done the simple step of populating the second
   * column with the sequence of natural numbers, with the fourth row,
   * we have the basic data setup to allow us shows us how to recursively
   * populate the triangle:
   *
   *                               max  Coordinate
   *
   * 1                             0,0
   * 1  1                          1,1
   * 1  2  1                       2,2   (y, x) = (y - 1)(x - 1) + (y - 1)(x)
   * 1  3  3   1                   3,3     3,2  = 2,1 + 2,2
   * 1  4  6   4   1
   * ...etc
   */
  Iterator.range(3, rows).foreach (y => {
    Iterator.range(2, rows).foreach (x => {
      if (x < y) {
        if (triangle(y)(x) == 0L) {
          triangle(y)(x) =
            (triangle(y - 1)(x - 1) + triangle(y - 1)(x))
        }
      }
    })
  })

  /**
   * Prints out the triangle in left-justified, "Matrix" format
   */
  override def toString (): String = {
    val buf = new StringBuilder()
    triangle.foreach(ar => {
      buf.append(ar.toList.filter(_ > 0).mkString("  ")).append("\n")
    })
    buf.toString
  }

  /**
   * @return list of Longs for the requested row
   * useful for binomial expansion
   * see: http://en.wikipedia.org/wiki/Pascals_triangle#Binomial_expansions
   */
  def row (n: Int): List[Long] = {
    require(n > 0)
    triangle(n - 1).toList
  }

  /**
   * Combinatorics math function "n choose k"
   *
   * @param n total number of items
   * @param k number of items to be chosen
   * @return number of combinations of n things taken k at a time
   */
  def nChooseK (n: Int, k: Int): Long = triangle(n)(k)

  /**
   * Helper function
   */
  private def pullDiagonal (index: Int) :List[Long] = {
    var myList = for {y <- (index to rows - 1)
                      result = triangle(y)(y - index)
    } yield {
      (result)
    }
    myList.toList
  }

  /**
   * "A triangular number or triangle number is the number of dots in an
   * equilateral triangle uniformly filled with dots. For example, three
   * dots can be arranged in a triangle; thus three is a triangle number.
   * The nth triangle number is the number of dots in a triangle with n dots
   * on a side.
   * A triangle number is also the Gaussian summation series;
   * the sum of the natural numbers from 1 to n.
   * http://en.wikipedia.org/wiki/Triangular_number
   * @return : List[Long] of triangle numbers
   */
  def triangleNumbers () = pullDiagonal(2)

  /**
   * "A tetrahedral number, or triangular pyramidal number, is a figurate
   * number that represents a pyramid with a triangular base and three sides,
   * called a tetrahedron. The nth tetrahedral number is the sum of the first
   * n triangular numbers.  ...
   * http://en.wikipedia.org/wiki/Tetrahedral_number
   * @return List[Long] of tetrahedral numbers
   */
  def tetrahedralNumbers () = pullDiagonal(3)

  /**
   * "Pentatope numbers belong in the class of figurate numbers, which can be
   * represented as regular, discrete geometric patterns.  ...
   * http://en.wikipedia.org/wiki/Pentatope_number
   * @return List[Long] of pentatope numbers
   */
  def pentatopeNumbers () = pullDiagonal(4)

  /**
   * "They are called central since they show up exactly in the middle of
   * the even-numbered rows in Pascal's triangle. ...
   * http://en.wikipedia.org/wiki/Central_binomial_coefficient
   * Note: the standard closed form expression is:
   * (2n)! / (n!)^2
   * However this overflows the Long datatype and fails when n < 11
   * @return List[Long] of numbers of the central binomial coefficients
   */
  def centralBinomialCoefficients () = {
    val results = for {y <- (0 to rows - 1)
                       row = y
                       if (row % 2 == 0)
                       result = (triangle(row)(math.round(y / 2f)))
    } yield {
      (result)
    }
    results.toList
  }
}

object PascalsTriangle {

  /**
   * Used to show that the closed form expression formula fails with the long data type when
   * n > 11
   * method is marked private to show that it's just for illustration purposes
   */
  private def naiveCentralBinomialCoeffiecent (n: Int): Long = {
    try {
      factorial(2 * n) / math.pow((factorial(n)), 2).toLong
    }
    catch {
      case ex: Exception => println("failure at n: " + n)
      0L
    }
  }

  /**
   * Method is marked private to show that it's just for illustration purposes
   */
  private def factorial (n: Int): Long = {
    var result = 1L
    Iterator.range(1, n + 1).foreach(x => {
      result = result * x
    })
    result
  }

  def main (args: Array[String]) {
    val pt = new PascalsTriangle()
    println(pt.toString)
    println(pt.nChooseK(10, 8))
    println(pt.centralBinomialCoefficients)
    println(pt.triangleNumbers)
    println(pt.tetrahedralNumbers)
    println(pt.pentatopeNumbers)
    println((pt.centralBinomialCoefficients())(20))
    println(((0 to 67).toList.map(naiveCentralBinomialCoeffiecent(_))).mkString(", "))
    println(pt.centralBinomialCoefficients.mkString(", "))
  }
}