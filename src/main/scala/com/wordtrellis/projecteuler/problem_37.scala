package com.wordtrellis.projecteuler

import scala.collection.mutable.ListBuffer

/**
  * Problem 37
  *
  * The number 3797 has an interesting property. Being prime itself, it is
  * possible to continuously remove digits from left to right, and remain prime
  * at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
  * left: 3797, 379, 37, and 3.
  *
  * Find the sum of the only eleven primes that are both truncatable from left
  * to right and right to left.
  *
  * NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
  *
  * @author Todd Cook
  *
  */
object problem_37 {
  val composites          = new ListBuffer[String]()
  var bases: List[String] = new SieveOfAtkin(100).getPrimes.map(_.toString)
  composites.appendAll(bases)
  var ii = 3

  def getMorePrimes(inc: Int): Boolean = {
    val startSize = composites.size
    bases =
      new SieveOfAtkin(math.pow(10, inc).toInt).getPrimes.map(  _.toString  ).filter(a => a.length == inc)
    bases.foreach(b => {
      var pass = false
      val key  = b.slice(b.length - inc + 1, b.length)
      //  println ("searching composites for sliced base:  " + key + " of " + b)
      if (composites.indexOf(key) > 0) {
        pass = true
      }
      if (pass) {
        val key = b.slice(0, b.length - 1) ///inc + 1)
        //   println ("searching composites for sliced base:  " + key + " of " + b)
        if (composites.indexOf(key) > 0) {
          //println ("found key: " + key)
          composites.append(b)
        }
      }
    })
    startSize < composites.size
  }

  def main(args: Array[String]): Unit = {
    val result = answer
    println(result)
    println(result.size)
    println(result.sum)
  }

  def answer: List[Int] = {
    //        while (ii < 15 && getMorePrimes (ii)) {
    //            ii += 1
    //        }
    //        val singleDigitPrimes = new SieveOfAtkin (10).getPrimes.map (_ .toString)
    //        singleDigitPrimes.foreach (a => composites.remove (composites.indexOf (a)))
    //        composites.toList.filter (b => b.indexOf ('1') == -1)
    val primes = new SieveOfAtkin(1000000).getPrimes
    //  val candidates = primes.filter (a => a > 10)
    //   println( "searching %d primes".format( candidates.size))
    val winners = new ListBuffer[Int]()
    primes.foreach(a => {
      if (a > 10) {
        if (allMembersPrime(truncateIntList(a), primes)) {
          winners.append(a)
        }
      }
    })
    winners.toList
  }

  /**
    * scala> truncateIntList(3797)
    *   res5: List[Int] = List(3, 37, 379, 3797, 7, 97, 797, 3797)
    */
  def truncateIntList(a: Int): List[Int] = {
    val rightToLeft = for { len <- (1 to a.toString.length).toList } yield {
      java.lang.Integer.parseInt(a.toString.slice(0, len) .toString)
    }

    val leftToRight = for { len <- (1 to a.toString.length).toList } yield {
      java.lang.Integer.parseInt(a.toString.reverse.slice(0, len).reverse .toString)
    }

    (rightToLeft ++ leftToRight).distinct
  }

  def allMembersPrime(a: List[Int], primes: List[Int]): Boolean = {
    a.foreach(b =>
      if (!primes.contains(b)) {
        return false
    })
    true
  }
}

/**
  *  Commentary:
  *  List(23, 37, 53, 73, 313, 317, 373, 797, 3137, 3797, 739397)
  *  11
  */
