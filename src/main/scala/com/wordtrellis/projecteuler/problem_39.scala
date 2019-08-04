
package com.wordtrellis.projecteuler

import collection.mutable.ListBuffer

/**
 * Problem 39
 * If p is the perimeter of a right angle triangle with integral length sides,
 * (a,b,c}, there are exactly three solutions for p = 120.
 *
 * {20,48,52}, {24,45,51}, {30,40,50}
 *
 * For which value of p <= 1000, is the number of solutions maximised?
 *
 * @author : Todd Cook
 *
 */

object problem_39 {

  /**
   *   if b & c even
   *   a is even
   *   if c odd, b even
   *   a is odd
   *  if c even, b odd
   *   a is odd
   *   if c even, b odd
   *   a is odd
   */
  def constraint (a: Int, b: Int, c: Int): Boolean = {
    // if b & c even
    // a is even
    if ((c % 2 == 0) && (b % 2 == 0)) {
      if (a % 2 == 0) {
        return true
      }
    }
    // if c odd, b even
    // a is odd
    if ((c % 2 != 0) && (b % 2 == 0)) {
      if (a % 2 != 0) {
        return true
      }
    }
    // if c even, b odd
    // a is odd
    if ((c % 2 != 0) && (b % 2 == 0)) {
      if (a % 2 != 0) {
        return true
      }
    }
    // if c even, b odd
    // a is odd
    if ((c % 2 != 0) && (b % 2 != 0)) {
      if (a % 2 == 0) {
        return true
      }
    }
    false
  }

  def createCandidates (m: Int): List[(Int, Int, Int)] = {
    val buf = new ListBuffer[(Int, Int, Int)]()
    // a < b < c
    // minimum integral right triangle: 3, 4, 5
    (5 to m).toList.foreach(c => {
      (4 to (c - 1)).toList.foreach(b => {
        (3 to (b - 1)).toList.foreach(a => {
          if (constraint(a, b, c)) {
            if ((a + b + c == m) && (a * a + b * b == c * c)) {
              buf.append((a, b, c))
            }
          }
        })
      })
    })
    buf.toList
  }

  def answer(): Unit = {
    val candidates = createCandidates(120)
    println(candidates)
    (12 to 1000).foreach(n => {
      if (n % 2 == 0) {
        // only even numbered perimeters produce
        // multiple solutions for integral length sides of a right triangle;
        // verified by brute force initial results
        //if (n % 60 == 0){ // factor for most common cluster
        val results = createCandidates(n)
        if (results != Nil) {
          print("perimeter: " + n + " ")
          println(createCandidates(n))
        }
        //}
      }
    })
  }

  def main (args: Array[String]): Unit = {
    println(answer())
  }
}