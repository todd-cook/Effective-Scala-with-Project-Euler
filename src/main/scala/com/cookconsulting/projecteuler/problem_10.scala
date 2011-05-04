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
 * @since : Oct 11, 2009
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
    println(bi.toString)
    bi.doubleValue
  }

  /**
   * constructed in seconds:  0.656
   * 164357 primes found
   * Sum of primes: 1.57514803718E11
   */
  def main (args: Array[String]) {
    var start = java.lang.System.currentTimeMillis
    var atk = new SieveOfAtkin(2000000);
    var end = java.lang.System.currentTimeMillis
    println("constructed in seconds:  " + (java.lang.System.currentTimeMillis - start) / 1000d)
    println(atk.getPrimes().size + " primes found");
    println("Sum of primes: " + sumBigList(atk.getPrimes))
  }
}
