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
 * @since : 4/24/2011
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

  def createCandidates (m: Int) = {
    val buf = new ListBuffer[Tuple3[Int, Int, Int]]()
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

  def answer = {
    val candidates = createCandidates(120)
    println(candidates)
    (12 to 1000).foreach(n => {
      if (n % 2 == 0) {
        // only even numbered perimeters produce
        // multiple solutions for integral length sides of a right triangle;
        // verified by brute force initial results
        //if (n % 60 == 0){ // most common cluster
        val results = createCandidates(n)
        if (results != Nil) {
          print("perimeter: " + n + " ")
          println(createCandidates(n))
        }
        //}
      }
    })
  }

  def main (args: Array[String]) = {
    println(answer)
  }
}