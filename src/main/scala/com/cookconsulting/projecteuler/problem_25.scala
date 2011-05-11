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
 * Problem 25
 * The Fibonacci sequence is defined by the recurrence relation:
 *
 *     F_(n) = F_(n - 1) + F_(n - 2), where F_(1) = 1 and F_(2) = 1.
 *
 * Hence the first 12 terms will be:
 *
 *     F_(1) = 1
 *     F_(2) = 1
 *     F_(3) = 2
 *     F_(4) = 3
 *     F_(5) = 5
 *     F_(6) = 8
 *     F_(7) = 13
 *     F_(8) = 21
 *     F_(9) = 34
 *     F_(10) = 55
 *     F_(11) = 89
 *     F_(12) = 144
 *
 *  The 12th term, F_(12), is the first term to contain three digits.
 *
 *  What is the first term in the Fibonacci sequence to contain 1000 digits?
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 *
 */

object problem_25 {

  val PHI = (1 + java.lang.Math.sqrt(5d)) / 2

  /**
   * Closed-form expression :
   *  Phi^n  - (1 - Phi)^n / sqrt(5)
   *  see http://en.wikipedia.org/wiki/Fibonacci_sequence#Closed-form_expression
   */
  def fibDouble (n: Int): Double = (java.lang.Math.pow(PHI, n) -
    (java.lang.Math.pow((1 - PHI), n))) / java.lang.Math.sqrt(5)

  val PHI_SEED = new java.math.BigDecimal(PHI)
  val ONE_MINUS_PHI = new java.math.BigDecimal(1 - PHI)
  val SQUARE_ROOT_5 = new java.math.BigDecimal(java.lang.Math.sqrt(5))

  /**
   * See: http://en.wikipedia.org/wiki/Fibonacci_number#Closed-form_expression
   */
  def fib (n: Int): java.math.BigInteger =
    (PHI_SEED.pow(n).subtract(ONE_MINUS_PHI.pow(n))).divide(SQUARE_ROOT_5).toBigInteger

  def answer () = {
    var inc = 4780 // sometimes making an algorithm fast depends on starting in the right place
    var bigInteger = fib(inc)
    while (bigInteger.toString.length < 1000) {
      inc += 1
      bigInteger = fib(inc)
      println("fib: " + inc + " yields length " + bigInteger.toString.length)
    }
    println("Fibonacci " + inc + " has " + bigInteger.toString.length + " digits")
  }

  def main (args: Array[String]) = {
    answer
  }
}

/**
 * Commentary
 * Some scala console trial and error reveals the best seed:
 *
 * scala> fib(3000).toString.length
 * res12: Int = 627
 *
 * scala> fib(5000).toString.length
 * res13: Int = 1045
 *
 * scala> fib(4900).toString.length
 * res14: Int = 1024
 *
 * scala> fib(4750).toString.length
 * res15: Int = 993
 *
 * scala> fib(4800).toString.length
 * res16: Int = 1003
 *
 * scala> fib(4780).toString.length
 * res17: Int = 999
 */