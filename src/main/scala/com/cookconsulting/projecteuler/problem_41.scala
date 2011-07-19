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

package com.cookconsulting.projecteuler

import collection.mutable.ListBuffer

/**
 *
 * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n
 * exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
 *
 * What is the largest n-digit pandigital prime that exists?
 *
 * @author Todd Cook
 * @since 5/15/2011
 */

object problem_41 {

  def checkForPandigitalPrimes(digits: Int): List[Long] = {
    val pg = new PermutationGenerator(digits)
    val candidates = new ListBuffer[Long]()
    while (pg.hasMore) {
      val candidate = pg.next.mkString("").toLong
      if (candidate % 2 != 0 && candidate % 3 != 0) {
        candidates.append(candidate)
      }
    }
    println("Using %d digits, number of pandigital candidates: %d".format(digits, candidates.size))
    candidates.filter(a => (problem_7.isPrime(a))).toList
  }

  def answer() = {
    val pandigitalPrimes = new ListBuffer[Long]()
    (4 to 9).toList.foreach(a => pandigitalPrimes.appendAll(checkForPandigitalPrimes(a)))
    println(pandigitalPrimes)
    pandigitalPrimes.toList.sortWith(_ > _)(0)
  }

  def main(args: Array[String]) {
    println(answer)
  }
}