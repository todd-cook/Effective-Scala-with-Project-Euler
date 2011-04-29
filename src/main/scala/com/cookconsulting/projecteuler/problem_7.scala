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
Problem 7
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
we can see that the 6th prime is 13.
What is the 10001th prime number?

Solution:
For a discussion of testing prime numbers,
see: http://en.wikipedia.org/wiki/Primality_test

 * @author : Todd Cook
 * @since : 4/24/2011
 */

object problem_7 {

    /**
     *  The simplest primality test is as follows: Given an input number n,
     *  check whether any integer m from 2 to n - 1 divides n.
     *  If n is divisible by any m then n is composite, otherwise it is prime.
     *
     *  However, rather than testing all m up to n - 1,
     *  it is only necessary to test m up to sqrt n:
     *  if n is composite then it can be factored into two values,
     *  at least one of which must be less than or equal to sqrt n.
     *
     *  The efficiency can also be improved by skipping all even m except 2,
     *  since if any even number divides n then 2 does.
     *  It can be improved further by observing that all primes are of
     *  the form 6k +/- 1, with 2 and 3 being the only exceptions.
     *  This is because all integers can be expressed as (6k + i) for some
     *  integer k and for i = -1, 0, 1, 2, 3, or 4;
     *  2 divides (6k + 0), (6k + 2), (6k + 4); and 3 divides (6k + 3).
     *  So a more efficient method is to test if n is divisible by 2 or 3,
     *  then to check through all the numbers of
     *  form 6k (plus or minus) 1  less than or equal to the sqrt n.
     *  This is 3 times as fast as testing all m.
     *   from: http://en.wikipedia.org/wiki/Primality_test
     */
    def isPrime (n: Int): Boolean = {
        if (n == 2 || n == 3)
            return true
        if (n % 2 == 0)
            return false
        if (n % 3 == 0)
            return false
        val ceil = math.sqrt (n).intValue
        (1 to ceil).foreach (x => {
            if ((6 * x) - 1 <= n)
                return true
            if ((6 * x) + 1 <= n)
                return true
        })
        false
    }

    def findNextPrime (currentPrimeNumber: Int): Int = {
        var nextPrimeNumber = 1 + currentPrimeNumber
        while (!isPrime (nextPrimeNumber)) {
            nextPrimeNumber += 1
        }
        nextPrimeNumber
    }

    def getPrimeList (numberOfPrimes: Int): List[Int] = {
        var primes = new ListBuffer[Int] ()
        var primeCount = 0
        var num = 1
        while (primeCount < numberOfPrimes) {
            num = findNextPrime (num)
            primes.append (num)
            primeCount += 1
        }
        primes.toList
    }

    def answer (): Int = {
        var primes = getPrimeList (1001)
        primes.last
    }

    def main (args: Array[String]) {
        println (answer)
    }
}

