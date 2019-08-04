

package com.wordtrellis.projecteuler

import collection.mutable.ListBuffer

/**
 * My first inclination to solve Problem 10
 * was to use a Sieve of Eratosthenes.
 * This implementation is too slow; it's a good example of functional programming that looks like
 * it should work, but doesn't perform as expected in practice. Hence unit tests are gold.
 *
 * @author Todd Cook
 * @since 4/30/11 6:38 PM
 */

class SieveOfEratosthenes (MAX: Int) {

  val MAX_FACTOR: Int = math.sqrt(MAX).intValue + 1

  val primeCandidates = new ListBuffer[Int]()

  (2 to MAX).foreach(x => {
    if ((x % 2 == 0) || (x % 3 == 0) || (x % 5 == 0) || (x % 7 == 0)) {}
    else {
      primeCandidates.append(x)
    }
  })

  def createSieve () {
    print("removing factors...")
    primeCandidates.foreach(x => {
      (x to MAX_FACTOR).foreach(y => {
        val candidate = y * x
        if (candidate <= MAX) {
          val location = primeCandidates.indexOf(candidate)
          if (location != -1) {
            primeCandidates.remove(location)
          }
        }
      })
    })
    primeCandidates.append(2)
    primeCandidates.append(3)
    primeCandidates.append(5)
    primeCandidates.append(7)
  }

  def primes (): List[Int] = primeCandidates.sortWith(_ < _).toList
}

object problem_10Fails {

  /**
   * This method is required because scala's List[T] method sum() = :T
   * so 2 million Int primes summed together will overflow an Int,
   * whereas List[Double] .sum() :Double
   */
  def sumBigList (bigList: List[Int]): Double = {
    var bd = 0d
    bigList.foreach(x => bd += x)
    bd
  }

  /**
   * Initialized in seconds: 0.895
     removing factors...
     Constructed in seconds: 1209.18
     Found 419776 primes
     Sum: 4.39906638395E11
   */

  def answer (): Unit = {
    val start = java.lang.System.currentTimeMillis
    val soe = new SieveOfEratosthenes(2000000)
    val end = java.lang.System.currentTimeMillis
    println("Initialized in seconds: " + (end - start) / 1000d)
    soe.createSieve()
    println("Constructed in seconds: " + (java.lang.System.currentTimeMillis - start) / 1000d)
    println("Found " + soe.primes().size + " primes")
    println("Sum: " + sumBigList(soe.primes()))
  }

  def main (args: Array[String]): Unit = answer()

}

/**
 *
 * A supposedly good program that however runs out of memory, when pushing 2 million primes
 *
 * from: http://en.literateprograms.org/Sieve_of_Eratosthenes_(Scala)?oldid=8087
 * Copyright (c) 2011 the authors listed at the following URL, and/or
 * the authors of referenced articles or incorporated external code:
 * http://en.literateprograms.org/Sieve_of_Eratosthenes_(Scala)?action=history&offset=20061117025123
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 *  "Software"), to deal in the Software without restriction, including
 *   without limitation the rights to use, copy, modify, merge, publish,
 *    distribute, sublicense, and/or sell copies of the Software, and to
 *    permit persons to whom the Software is furnished to do so, subject to
 *    the following conditions:
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

object Sieve {

  def ints (n: Int): Stream[Int] =
    Stream.cons(n, ints(n + 1))

  def primes (nums: Stream[Int]): Stream[Int] =
    Stream.cons(nums.head,
                primes((nums.tail) filter (x => x % nums.head != 0)))

  def main (args: Array[String]): Unit = {
    //  val n = Integer.parseInt(args(0))
    val n = 2000000
    println(primes(ints(2)).take(n).toList)
  }
}

/**
 * Another example of failure
 */
object problem_10Fail2 {

  // var MAX =1000
  class SieveOfEratosthenes (MAX: Int) {
    var primeCandidates = new ListBuffer[Int]()
    primeCandidates.appendAll(2 :: 3 :: 5 :: ((2 to MAX).filter(x => x % 2 != 0).filter(x => x % 3 != 0).filter(x => x % 5 != 0).toList))

    // 533333

    def createSieve (): Unit = {
      var pos = 3
      while (pos < primeCandidates.size) {
        primeCandidates = sieveTheNumbers(pos, primeCandidates)
        if (pos % 1000 == 0) {
          println("calculating position: " + pos)
        }
        pos += 1
      }
    }

    def sieveTheNumbers (position: Int, numbers: ListBuffer[Int]): ListBuffer[Int] = {
      val number = numbers(position)
      val square = number * number
      //numbers.filter(x => x <= square) ::: numbers.filter(x => x > square).filter ( x => x % number  != 0)

      var ii = 0
      while (numbers(ii) < square) {
        ii += 1
      }

      while (ii < numbers.size) {
        if (numbers(ii) % number == 0) {
          numbers.remove(ii)
        }
        else {
          ii += 1
        }
      }
      numbers
    }

    createSieve()

    def primes (): ListBuffer[Int] = primeCandidates
  }

  def answer (): Unit = {
    val start = java.lang.System.currentTimeMillis
    val soe = new SieveOfEratosthenes(2000000)
    val end = java.lang.System.currentTimeMillis
    println("elapsed milliseconds:  " + (end - start))
    println(soe.primes().mkString(" "))
    println("Sum: " + soe.primes().reduceLeft(_ + _))
  }

  def main (args: Array[String]): Unit = answer()

}
