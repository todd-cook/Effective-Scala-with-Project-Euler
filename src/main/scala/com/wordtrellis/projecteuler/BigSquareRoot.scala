package com.wordtrellis.projecteuler

import java.math.{BigDecimal, BigInteger, MathContext}

/**
  * This is a scala port of Michael Gilleland's code for computing the square root of large
  * numbers using Heron's method.
  * see: http://www.merriampark.com/bigsqrt.htm
  * see also: http://en.wikipedia.org/wiki/Square_root
  *
  * @author Todd Cook
  * @author Michael Gilleland
  * @see
  *
  */
class BigSquareRoot {

  val context: MathContext   = java.math.MathContext.DECIMAL128
  val ZERO                   = new BigDecimal("0", context)
  val ONE                    = new BigDecimal("1", context)
  val TWO                    = new BigDecimal("2", context)
  val DEFAULT_MAX_ITERATIONS = 50
  val DEFAULT_SCALE          = 10

  var error: BigDecimal  = ZERO
  var iterations         = 0
  var traceFlag          = false
  var scale: Int         = DEFAULT_SCALE
  var maxIterations: Int = DEFAULT_MAX_ITERATIONS

  /**
    * The error is the original number minus (sqrt * sqrt). If the original number
    * was a perfect square, the error is 0.
    */
  def getError: BigDecimal = error

  /**
    * Number of iterations performed when square root was computed
    */
  def getIterations: Int = iterations

  /**
    * Trace flag
    */
  def getTraceFlag: Boolean = traceFlag

  def setTraceFlag(flag: Boolean) :Unit= {
    traceFlag = flag
  }

  /**
    * Scale
    */
  def getScale: Int = scale

  def setScale(scale: Int): Unit = {
    this.scale = scale
  }

  /**
    * Maximum iterations
    */
  def getMaxIterations: Int = maxIterations

  def setMaxIterations(maxIterations: Int) :Unit= {
    this.maxIterations = maxIterations
  }

  /**
    * Get square root
    */
  def get(n: BigInteger): BigDecimal = get(new BigDecimal(n, context))

  def get(n: BigDecimal): BigDecimal = {

    // Make sure n is a positive number
    if (n.compareTo(ZERO) <= 0) {
      throw new IllegalArgumentException()
    }
    val initialGuess = getInitialApproximation(n)
    trace("Initial guess " + initialGuess.toString)
    var lastGuess = ZERO
    var guess     = new BigDecimal(initialGuess.toString, context)
    // Iterate
    iterations = 0
    var more = true
    while (more) {
      lastGuess = guess
      guess = n.divide(guess, scale, BigDecimal.ROUND_HALF_UP)
      guess = guess.add(lastGuess)
      guess = guess.divide(TWO, scale, BigDecimal.ROUND_HALF_UP)
      trace("Next guess " + guess.toString)
      error = n.subtract(guess.multiply(guess))
      iterations += 1
      if (iterations >= maxIterations) {
        more = false
      } else if (lastGuess.equals(guess)) {
        more = error.abs().compareTo(ONE) >= 0
      }
    }
    guess
  }

  /**
    * Get initial approximation
    */
  private def getInitialApproximation(n: BigDecimal) = {
    val integerPart = n.toBigInteger()
    var length      = integerPart.toString().length()
    if ((length % 2) == 0) {
      length -= 1
    }
    length /= 2
    ONE.movePointRight(length)
  }

  /**
    * Trace
    */
  def trace(s: String): Unit = {
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
    * Test and show diagnostics
    */
    def main(args: Array[String]) :Unit = {
    showDiagnostics()
  }

  private def showDiagnostics()  :Unit={

    val app = new BigSquareRoot()
    app.setTraceFlag(true)
    // Generate a random big integer with a hundred digits
    var n = BigSquareRoot.getRandomBigInteger(100)
    // Build an array of test numbers
    val testNums = List("9", "30", "720", "1024", n.toString())
    Iterator
      .range(0, testNums.length)
      .foreach(i => {
        n = new BigInteger(testNums(i))
        if (i > 0) {
          System.out.println("----------------------------")
        }
        println("Computing the square root of")
        println(n.toString())
        val length = n.toString().length()
        if (length > 20) {
          app.setScale(length / 2)
        }
        val sqrt = app.get(n)
        println("Iterations " + app.getIterations)
        println("Sqrt " + sqrt.toString )
        println(sqrt.multiply(sqrt).toString )
        println(n.toString)
        println("Error " + app.getError.toString )
      })
  }

  /**
    * Get random BigInteger
    */
  private def getRandomBigInteger(nDigits: Int): BigInteger = {
    val sb = new StringBuffer()
    val r  = new java.util.Random()
    (0 until nDigits).foreach(ii => sb.append(r.nextInt(10)))
    new BigInteger(sb.toString)
  }
}
