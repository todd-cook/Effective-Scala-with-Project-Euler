package com.wordtrellis.projecteuler

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
  *
  *
  */
object problem_25 {

  val PHI: Double = (1 + java.lang.Math.sqrt(5d)) / 2
  val PHI_SEED      = new java.math.BigDecimal(PHI)
  val ONE_MINUS_PHI = new java.math.BigDecimal(1 - PHI)
  val SQUARE_ROOT_5 = new java.math.BigDecimal(java.lang.Math.sqrt(5))

  /**
    * Closed-form expression :
    *  Phi^n  - (1 - Phi)^n / sqrt(5)
    *  see http://en.wikipedia.org/wiki/Fibonacci_sequence#Closed-form_expression
    */
  def fibDouble(n: Int): Double =
    (java.lang.Math.pow(PHI, n) -
      (java.lang.Math.pow((1 - PHI), n))) / java.lang.Math.sqrt(5)

  def main(args: Array[String]): Unit = {
    answer()
  }

  def answer(): Unit = {
    var inc        = 4780 // sometimes making an algorithm fast depends on starting in the right place
    var bigInteger = fib(inc)
    while (bigInteger.toString.length < 1000) {
      inc += 1
      bigInteger = fib(inc)
      println("fib: " + inc + " yields length " + bigInteger.toString.length)
    }
    println("Fibonacci " + inc + " has " + bigInteger.toString.length + " digits")
  }

  /**
    * See: http://en.wikipedia.org/wiki/Fibonacci_number#Closed-form_expression
    */
  def fib(n: Int): java.math.BigInteger =
    (PHI_SEED.pow(n).subtract(ONE_MINUS_PHI.pow(n))).divide(SQUARE_ROOT_5).toBigInteger
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
