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

package com.wordtrellis.projecteuler

/**
 * Problem 5
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without
 * any remainder.
 * What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 */

object problem_5 {

  def calcSmallestNumberDivisibleByEntireRange (ceiling: Int) = {
    (1 to ceiling).foldLeft(1) {
                                 (product, n) => {
                                   val r_raw = product % n
                                   val r = if (r_raw == 0) {
                                     n
                                   }
                                   else {
                                     r_raw
                                   }
                                   product * (if (n % r == 0) {
                                     (n / r)
                                   }
                                   else {
                                     n
                                   })
                                 }
                               }
  }

  def answer = {
    calcSmallestNumberDivisibleByEntireRange(20)
  }

  def main (args: Array[String]) = {
    println(answer)
  }
}