

package com.wordtrellis.projecteuler

/**
 *  Problem 10
 *  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *  Find the sum of all the primes below two million.
 *
 *  Solution:
 *  Sieve of Eratosthenes
 *  see: http://en.wikipedia.org/wiki/Sieve_of_eratosthenes
 *  1. Create a list of consecutive integers from two to n: (2, 3, 4, ..., n).
 *  2. Initially, let p equal 2, the first prime number.
 *   3. Strike from the list all multiples of p less than or equal to n.
 *   4. Find the first number remaining on the list greater than p
 *   (this number is the next prime); replace p with this number.
 *   5. Repeat steps 3 and 4 until p2 is greater than n.
 *   6. All the remaining numbers on the list are prime.
 *
 *  Or a better more modern version is the Sieve of SieveOfAtkin
 *  http://en.wikipedia.org/wiki/Sieve_of_Atkin
 *
 * First couple attempts are included, see problem_10Fails.scala
 *
 * @author : Todd Cook
 *
 */

object problem_10 {

  /**
   * This method is required because scala's List[T] method sum() = :T
   * so 2 million Int primes summed together will overflow an Int,
   * whereas List[Double] .sum() :Double
   */
  def sumBigList (bigList: List[Int]): Double = {
    //    var bd = 0d
    //    bigList.foreach(x => bd += x)
    //    bd
    // a foolproof implementation for the doubters
    var bi =  new java.math.BigInteger("0")
    bigList.foreach(x => {bi = bi.add( new java.math.BigInteger (x.toString) )})
//    println(bi.toString)
    bi.doubleValue
  }

  /**
   * constructed in seconds:  0.656
   * 164357 primes found
   * Sum of primes: 1.57514803718E11
   */
  def main (args: Array[String]) {
    val start = java.lang.System.currentTimeMillis
    val atk = new SieveOfAtkin(2000000)
    var end = java.lang.System.currentTimeMillis
    println("constructed in seconds:  " + (java.lang.System.currentTimeMillis - start) / 1000d)
    println(atk.getPrimes().size + " primes found")
    println("Sum of primes: " + sumBigList(atk.getPrimes()))
  }
}
