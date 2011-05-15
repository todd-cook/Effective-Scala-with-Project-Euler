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
 * Problem 35
 *
 * The number, 197, is called a circular prime because all rotations of the
 * digits: 197, 971, and 719, are themselves prime.
 *
 * There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
 * 71, 73, 79, and 97.
 *
 * How many circular primes are there below one million?
 *
 * @author : Todd Cook
 * @since : 5/8/2011
 */

object problem_35 {

  def isCircular (n: Int): Boolean = {
    if (n < 10) {
      return SieveOfAtkin.isPrime(n)
    }
    createCandidates(n).foreach(a => if (!SieveOfAtkin.isPrime(a)) {
      return false
    })
    true
  }

  def createCandidates (n: Int): List[Int] = {
    val numString = n.toString
    val numRepeat = n.toString + n.toString
    val candidates = for {a <- (0 to numString.length - 1)
    } yield {
      ((numRepeat.slice(a, a + 1) + numRepeat.slice(a + 1, a + numString.length)).toInt)
    }
    candidates.toList
  }

  def getCirculars (ceiling: Int): List[Int] = {
    val primes = new SieveOfAtkin(ceiling).getPrimes
    val circulars = new ListBuffer[Int]()
    primes.foreach(a => if (isCircular(a)) {
      circulars.append(a)
    })
    circulars.toList
  }

  def answer = getCirculars(1000000)

  def main (args: Array[String]) = {
    println(answer)
    println(answer.length)
  }
}

/**
 *
 *
 * Commentary:
 * it's not clear why the following approach is slow;
 * probably pushing the big list into the foreach causes it to get copied many times;
 * more sleuthing on this to come:
 *
 *
def isCircular (n: Int, primes: List[Int]): Boolean = {
        val numString = n.toString
        if (numString.length == 1)
            return problem_7.isPrime (n)
        val numRepeat = n.toString + n.toString
        (0 to numString.length - 1).foreach (a => {
            if (primes.indexOf ((numRepeat.slice (a, a + 1) +
                numRepeat.slice (a + 1, a + numString.length)).toInt) > 0)
                return false
        })
        return true
    }

    def getCirculars (ceiling: Int): List[Int] = {
        val primes = new SieveOfAtkin (ceiling).getPrimes
        val circulars = new ListBuffer[Int] ()
        primes.foreach (a => if (isCircular (a, primes)) circulars.append (a))
        circulars.toList
    }

def binarySearch (x: Int, sortedList: List[Int]): Int = {
     var low = 0
     var high = sortedList.length - 1
     var mid = -1
     while (low <= high) {
         mid = (low + high) / 2
         if (sortedList (mid) > x)
             high = mid - 1
         else if (sortedList (mid) < x)
             low = mid + 1
         else
             return mid
     }
     return -mid // value would be inserted at index "low"
 }
 *
 *
 */