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
import collection.mutable.HashMap

/**
 * Problem 32
 *
 * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n
 * exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
 *
 * The product 7254 is unusual, as the identity, 39 * 186 = 7254, containing multiplicand,
 * multiplier, and product is 1 through 9 pandigital.
 *
 * Find the sum of all products whose multiplicand/multiplier/product identity can be written as
 * a 1 through 9 pandigital.
 * HINT: Some products can be obtained in more than one way so be sure to only include it once
 * in your sum.
 *
 * @author : Todd Cook
 * @since : 5/7/2011
 */
object problem_32 {

  def formPandigitalProductCombo (numbers: List[Int]): Tuple3[Int, Int, Int] = {
    val groupOne = (numbers(0) * 10) + numbers(1)
    val groupTwo = ((numbers(2) * 100) + (numbers(3) * 10) + (numbers(4)))
    val groupThree = ((numbers(5) * 1000) + (numbers(6) * 100) + (numbers(7) * 10) + (numbers(8)))
    (groupOne, groupTwo, groupThree)
  }

  def checkPandigitalPermutation (pandigitalCombo: Tuple3[Int, Int, Int]): Boolean =
    (pandigitalCombo._1 * pandigitalCombo._2 == pandigitalCombo._3)

  def main (args: Array[String]) {
    // generate all permutations; 9! = 362880 and they fit in memory
    val buf = new ListBuffer[List[Int]]()
    val pg = new PermutationGenerator(9)
    while (pg.hasMore) {
      buf.append(pg.next)
    }
    val permutations = buf.toList
    println("Total permutations: " + permutations.size)
    buf.clear
    val answers = new ListBuffer[Tuple3[Int, Int, Int]]()
    permutations.foreach(a => {
      val b = formPandigitalProductCombo(a)
      if (checkPandigitalPermutation(b)) {
        answers.append(b)
      }
    })
    println("Found " + answers.size + " pandigital permutations")
    println(answers.toList)
    // now we isolate the products that only occur once
    val map = new HashMap[Int, Tuple2[Int, Int]]()
    val solutions = new ListBuffer[Tuple3[Int, Int, Int]]()
    answers.toList.foreach(x => map.put(x._3, (x._1, x._2)))
    // this is a little clumsy and ugly, however other, more syntactically sugary attempts failed.
    val it = map.keySet.iterator
    while (it.hasNext) {
      val a = it.next
      val tmp = map.get(a).get
      solutions.append((tmp._1, tmp._2, a))
    }
    println(solutions.toList)
    val products = for {product <- solutions.toList}
                    yield (product._3)
    println(products)
    println("sum of products: " + products.sum)
  }
}
