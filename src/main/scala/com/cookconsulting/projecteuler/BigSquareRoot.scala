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

import java.math.BigDecimal
import java.math.BigInteger

/**
 * This is a scala port of Michael Gilleland's code for computing the square root of large
 * numbers using Heron's method.
 * see: http://www.merriampark.com/bigsqrt.htm
 * see also: http://en.wikipedia.org/wiki/Square_root
 *
 * @author Todd Cook
 * @author Michael Gilleland
 * @see
 * @since 4/30/11
 */

class BigSquareRoot {

  val context = java.math.MathContext.DECIMAL128
  val ZERO = new BigDecimal( "0", context)
  val ONE = new BigDecimal( "1", context)
  val TWO = new BigDecimal("2", context)
  val DEFAULT_MAX_ITERATIONS = 50
  val DEFAULT_SCALE = 10

  var error = ZERO
  var iterations = 0
  var traceFlag = false
  var scale = DEFAULT_SCALE
  var maxIterations = DEFAULT_MAX_ITERATIONS

  /**
   * The error is the original number minus (sqrt * sqrt). If the original number
   * was a perfect square, the error is 0.
   */
  def getError () = error

  /**
   * Number of iterations performed when square root was computed
   */
  def getIterations () = iterations

  /**
   * Trace flag
   */
  def getTraceFlag () = traceFlag

  def setTraceFlag (flag: Boolean) {
    traceFlag = flag;
  }

  /**
   * Scale
   */
  def getScale () = scale

  def setScale (scale: Int) {
    this.scale = scale;
  }

  /**
   * Maximum iterations
   */
  def getMaxIterations () = maxIterations

  def setMaxIterations (maxIterations: Int) {
    this.maxIterations = maxIterations;
  }

  /**
   * Get initial approximation
   */
  private def getInitialApproximation (n: BigDecimal) = {
    var integerPart = n.toBigInteger();
    var length = integerPart.toString().length();
    if ((length % 2) == 0) {
      length -= 1;
    }
    length /= 2;
    ONE.movePointRight(length);
  }

  /**
   * Get square root
   */
  def get (n: BigInteger) :BigDecimal = get(new BigDecimal(n, context))

  def get (n: BigDecimal) :BigDecimal ={

    // Make sure n is a positive number
    if (n.compareTo(ZERO) <= 0) {
      throw new IllegalArgumentException();
    }
    var initialGuess = getInitialApproximation(n);
    trace("Initial guess " + initialGuess.toString());
    var lastGuess = ZERO;
    var guess = new BigDecimal(initialGuess.toString(), context);
    // Iterate
    iterations = 0;
    var more = true;
    while (more) {
      lastGuess = guess;
      guess = n.divide(guess, scale, BigDecimal.ROUND_HALF_UP);
      guess = guess.add(lastGuess);
      guess = guess.divide(TWO, scale, BigDecimal.ROUND_HALF_UP);
      trace("Next guess " + guess.toString());
      error = n.subtract(guess.multiply(guess));
      iterations += 1
      if (iterations >= maxIterations) {
        more = false
      }
      else if (lastGuess.equals(guess)) {
        more = error.abs().compareTo(ONE) >= 0;
      }
    }
      guess
  }

  /**
   * Trace
   */
  def trace (s: String) = {
    if (traceFlag) {
      println(s)
    }
  }
}

/**
 * For demo and diagnostics
 */
object BigSquareRoot {
  /**
   * Get random BigInteger
   */
  private def getRandomBigInteger (nDigits: Int) :BigInteger = {
    var sb = new StringBuffer();
    var r = new java.util.Random();
    (0 until nDigits).foreach(ii => sb.append(r.nextInt(10)))
    new BigInteger(sb.toString)
  }

  /**
   * Test and show diagnostics
   */
   def main (args: Array[String]) {
     showDiagnostics
   }

  private def showDiagnostics {

    val app = new BigSquareRoot();
    app.setTraceFlag(true);
    // Generate a random big integer with a hundred digits
    var n = BigSquareRoot.getRandomBigInteger(100);
    // Build an array of test numbers
    val testNums = List("9", "30", "720", "1024", n.toString());
    Iterator.range(0, testNums.length )
      .foreach(i => {
        n = new BigInteger(testNums(i))
      if (i > 0) {
        System.out.println("----------------------------");
      }
      println("Computing the square root of");
      println(n.toString());
      var length = n.toString().length();
      if (length > 20) {
        app.setScale(length / 2);
      }
      var sqrt = app.get(n);
      println("Iterations " + app.getIterations());
      println("Sqrt " + sqrt.toString());
      println(sqrt.multiply(sqrt).toString());
      println(n.toString());
      println("Error " + app.getError().toString());
    })
  }
}