

package com.wordtrellis.projecteuler

import collection.mutable.ListBuffer

/**
 * Problem 7
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
 * we can see that the 6th prime is 13.
 * What is the 10001th prime number?
 *
 * @author  Todd Cook
 * @since  4/24/2011
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
   *
   *   from: http://en.wikipedia.org/wiki/Primality_test
   */
  def isPrime(n: Int): Boolean = {
    if (n == 2 || n == 3) {
      return true
    }
    if (n % 2 == 0) {
      return false
    }
    if (n % 3 == 0) {
      return false
    }
    val ceil = math.sqrt(n).round.intValue
    (5 to ceil).foreach(a => {
      if (n % a == 0) {
        return false
      }
    })
    true
  }

  def findNextPrime(currentPrimeNumber: Int): Int = {
    var nextPrimeNumber = 1 + currentPrimeNumber
    while (!isPrime(nextPrimeNumber)) {
      nextPrimeNumber += 1
    }
    nextPrimeNumber
  }

  def getPrimeList(numberOfPrimes: Int): List[Int] = {
    val primes = new ListBuffer[Int]()
    var primeCount = 0
    var num = 1
    while (primeCount < numberOfPrimes) {
      num = findNextPrime(num)
      primes.append(num)
      primeCount += 1
    }
    primes.toList
  }

  def isPrime(n: Long): Boolean = {
    if (n == 2L || n == 3L) {
      return true
    }
    if (n % 2 == 0) {
      return false
    }
    if (n % 3 == 0) {
      return false
    }
    val ceil = math.sqrt(n).longValue
    var seed = 5L
    while (seed < ceil){
      if (n % seed == 0) {
        return false
      }
      seed += 2
    }
    true
  }

  def findNextPrime(currentPrimeNumber: Long): Long = {
    var nextPrimeNumber = 1 + currentPrimeNumber
    while (!isPrime(nextPrimeNumber)) {
      nextPrimeNumber += 1
    }
    nextPrimeNumber
  }

  def answer(): Int = {
    println(getPrimeList(10001).last)
    val primes = new SieveOfAtkin(1000000).getPrimes()
    primes(10000)
  }

  def main(args: Array[String]) {
    println(answer())
  }
}

/**
 * Solution:
 * For a discussion of testing prime numbers,
 * see: http://en.wikipedia.org/wiki/Primality_test
 */