/*
 * Copyright (c) 2011, Todd Cook.
 *   All rights reserved.
 *   Redistribution and use in source and binary forms, with or without modification,
 *   are permitted provided that the following conditions are met:
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
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 *   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 *   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.wordtrellis.projecteuler

/**
  *   Generate permutations in lexicographic order
  *
  *   Of course, don't make the size too large. Recall that the number of
  *   permutations is n! which can be very large, even when n is as small as 20
  *   20! = 2,432,902,008,176,640,000 and
  *   21! overflows a Java long, which is why we use BigInteger instead.
  *
  * @author : Todd Cook
  * @author : Michael Gilleland
  * @see http://www.merriampark.com/perm.htm
  *
  */
import java.math.BigInteger

import scala.collection.mutable.ListBuffer

class PermutationGenerator(size: Int) {
  require(size > 1)
  var items               = new ListBuffer[Int]()
  var total: BigInteger   = getFactorial(size)
  var numLeft: BigInteger = getFactorial(size)

  Iterator.range(1, size + 1).foreach(ii => items.append(ii))

  /**
    * @return BigInteger - number of permutations not yet generated
    */
  def getNumLeft(): BigInteger = numLeft

  /**
    * @return BigInteger - total number of permutations
    */
  def getTotal(): BigInteger = total

  /**
    * @return Boolean - are there any more permutations?
    */
  def hasMore(): Boolean = (numLeft.compareTo(BigInteger.ZERO) == 1)

  /**
    * @return BigInteger - factorial number of permutations
    */
  def getFactorial(n: Int): BigInteger = {
    var fact = BigInteger.ONE
    Iterator
      .range(1, n + 1)
      .foreach(ii => {
        fact = fact.multiply(new BigInteger(Integer.toString(ii)))
      })
    fact
  }

  /**
    * Generate next permutation
    * @return List of Integers
    * @see http://en.wikipedia.org/wiki/Permutations#Systematic_generation_of_all_permutations
    */
  def next(): List[Int] = {

    if (numLeft.equals(total)) {
      numLeft = numLeft.subtract(BigInteger.ONE)
      return items.toList
    }
    var temp = 0 // placeholder for swapping
    // Find largest index j with a[j] < a[j+1]
    var j = items.toList.length - 2
    while (items(j) > items(j + 1)) {
      j -= 1
    }
    // Find index k such that items[k] is the smallest integer
    // greater than items[j] to the right of items[j]
    var k = items.length - 1
    while (items(j) > items(k)) {
      k -= 1
    }
    // Interchange items[j] and items[k]
    temp = items(k)
    items(k) = items(j)
    items(j) = temp
    // Put tail end of permutation after jth position in increasing order
    var r = items.length - 1
    var s = j + 1

    while (r > s) {
      temp = items(s)
      items(s) = items(r)
      items(r) = temp
      r -= 1
      s += 1
    }
    numLeft = numLeft.subtract(BigInteger.ONE)
    items.toList
  }
}

/**
  * Example usage
  */
object PermutationGenerator {
  def main(args: Array[String]): Unit = {
    var pg = new PermutationGenerator(4)
    while (pg.hasMore()) {
      println(pg.next().toList)
    }
  }

  /**
    * Count the number of transpositions; the distance from
    * a permutation to the paragon; primiere model
    * http://en.wikipedia.org/wiki/Parity_of_a_permutation
    */
  def parityOfAPermutation(model: List[Int], permutation: List[Int]): Int = {
    var parity = 0
    var mold   = new ListBuffer[Int]()
    mold.appendAll(model)
//    println("mold = " + mold.toList.mkString)
//    println("permutation = " + permutation.toList.mkString)
    Iterator
      .range(0, model.length - 1)
      .foreach(a => {
        var initialModelValue       = mold(a)
        var initialPermutationValue = permutation(a)
        if (initialModelValue != initialPermutationValue) {
          parity += 1
          var tmp  = permutation.indexOf(initialModelValue)
          var tmp2 = mold(tmp)
          mold(tmp) = initialModelValue
          mold(a) = tmp2
          //      println("mold = " +mold.toList.mkString)
        }
      })
    parity
  }

}
