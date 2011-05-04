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
 * @author : Todd Cook
 * @since : Oct 11, 2009
 */

import collection.mutable.ListBuffer

class SieveOfAtkin (ceiling: Int) {
  var primes = new ListBuffer[Int]();
  var limit = ceiling;
  findPrimes()

  def getPrimes (): List[Int] = primes.toList

  def exclusiveOR (state: Boolean, shiftVal: Boolean): Boolean = {
    if (!shiftVal) {
      return state
    }
    if (state && shiftVal) {
      return false
    }
    if (!state && shiftVal) {
      return true
    }
    return state
  }

  def findPrimes () = {

    var isPrime: Array[Boolean] = new Array[Boolean](limit + 1);
    var sqrt = (math.sqrt(limit)).intValue

    /**
     * For each entry number n in the sieve list, with modulo-sixty remainder r :
     * If r is 1, 13, 17, 29, 37, 41, 49, or 53, flip the entry for each possible solution to 4x2 + y2 = n.
     * If r is 7, 19, 31, or 43, flip the entry for each possible solution to 3x2 + y2 = n.
     * If r is 11, 23, 47, or 59, flip the entry for each possible solution to 3x2 - y2 = n when x > y.
     * If r is something else, ignore it completely.
     */
    (1 to sqrt).foreach(x => {
      (1 to sqrt).toList.foreach(y => {
        var n = 4 * x * x + y * y;

        if (n <= limit && (n % 60 == 1
          || n % 60 == 13
          || n % 60 == 17
          || n % 60 == 29
          || n % 60 == 37
          || n % 60 == 41
          || n % 60 == 49
          || n % 60 == 53)) {
          isPrime(n) = exclusiveOR(isPrime(n), true)
        }

        n = 3 * x * x + y * y;
        if (n <= limit && (n % 60 == 7
          || n % 60 == 19
          || n % 60 == 31
          || n % 60 == 43)) {
          isPrime(n) = exclusiveOR(isPrime(n), true)
        }

        n = 3 * x * x - y * y;
        if (x > y && n <= limit && (n % 12 == 11
          || n % 60 == 23
          || n % 60 == 47
          || n % 60 == 59)) {
          isPrime(n) = exclusiveOR(isPrime(n), true)
        }
      })
    })

    (5 to sqrt).foreach(n => {
      if (isPrime(n)) {
        var k = n * n
        while (k <= limit) {
          if (k > 0) {
            isPrime(k) = false;
          }
          k *= k
        }
        var nSquared = n * n;
        var k2 = nSquared;
        var ii = 1
        while (k2 <= limit) {
          isPrime(k2) = false;
          k2 = ii * k2
          ii += 1
        }
      }
    })
    primes.append(2);
    primes.append(3);
    primes.append(5);

    (5 to (limit - 1)).foreach(n => {
      if (isPrime(n)) {
        primes.append(n)
      };
    })
  }
}
