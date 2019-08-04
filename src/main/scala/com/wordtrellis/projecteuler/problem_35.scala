
package com.wordtrellis.projecteuler

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
 *
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
    val candidates = for {a <- 0 to numString.length - 1
    } yield {
      (numRepeat.slice(a, a + 1) + numRepeat.slice(a + 1, a + numString.length)).toInt
    }
    candidates.toList
  }

  def getCirculars (ceiling: Int): List[Int] = {
    val primes = new SieveOfAtkin(ceiling).getPrimes()
    val circulars = new ListBuffer[Int]()
    primes.foreach(a => if (isCircular(a)) {
      circulars.append(a)
    })
    circulars.toList
  }

  def answer: List[Int] = getCirculars(1000000)

  def main (args: Array[String]): Unit = {
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