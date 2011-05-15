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
 *  Problem 23
 *  A perfect number is a number for which the sum of its proper divisors is
 *  exactly equal to the number.
 *  For example, the sum of the proper divisors of 28 would be
 *  1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
 *
 *  A number n is called deficient if the sum of its proper divisors is less
 *  than n and it is called abundant if this sum exceeds n.
 *
 *  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
 *  number that can be written as the sum of two abundant numbers is 24. By
 *  mathematical analysis, it can be shown that all integers greater than
 *  28123 can be written as the sum of two abundant numbers. However, this
 *  upper limit cannot be reduced any further by analysis even though it is
 *  known that the greatest number that cannot be expressed as the sum of
 *  two abundant numbers is less than this limit.
 *
 *  Find the sum of all the positive integers which cannot be written as the
 *  sum of two abundant numbers.
 *
 * @author : Todd Cook
 * @since : 5/13/2011
 */

object problem_23 {

  def properDivisors (num: Int): List[Int] = {
    for {a <- (1 to (num / 2) + 1).toList
         if (num % a == 0)
        } yield  (a)
  }

  def isAbundant (num: Int) = if (num  > 11) (num < properDivisors(num).foldLeft(0)(_ + _)) else false

  def answer () = {

    val LIMIT = 28123
    val abundantNumbers = for {a <- (1 to LIMIT)
                               if (isAbundant(a))
                              } yield (a)

    println("number of abundant numbers 1 to %d:  %d".format(LIMIT,  abundantNumbers.length))
    val nonAbundantSums = new Array[Boolean](LIMIT + 1)

    (1 to LIMIT).foreach(a =>  nonAbundantSums(a) = true)
    //abundantNumbers.foreach(b => nonAbundantSums(b) = true)

    // we only need half of the abundant numbers, since we have to do a Cartesian product
    val halfAbundant = abundantNumbers.filter(a => (a < (LIMIT )))
    halfAbundant.foreach(a => {
      abundantNumbers.foreach(b => {
        val abundantSum = a + b
        if (abundantSum <= LIMIT) {
          nonAbundantSums(abundantSum) = false
        }
      })
    })

    var total = 0
    (1 until nonAbundantSums.length).foreach(a => if (nonAbundantSums(a)) {
      total += a
    })
    println("sum of non-abundant integers between 1 - 28123: " + total)
    total
  }

  def main (args: Array[String]) = {
    println(answer)
  }
}

/**
 * Solution:
 * Number of abundant numbers 1 to 28123: 6966
 * sum of non-abundant integers between 1 - 28123: 4140860.0
 * 4140860.0
 *
 */