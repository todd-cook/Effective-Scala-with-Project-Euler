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
 * Problem 14
 * The following iterative sequence is defined for the set of positive integers:
 *   n -> n/2 (n is even)
 *   n -> 3n + 1 (n is odd)
 *   Using the rule above and starting with 13, we generate the following sequence:
 *   13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
 *   It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
 *   Although it has not been proved yet (Collatz Problem), it is thought that all starting
 *   numbers finish at 1.
 *
 *   Which starting number, under one million, produces the longest chain?
 *   NOTE: Once the chain starts the terms are allowed to go above one million.
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 */

object problem_14 {

  def processEven (num: Long): Long = num / 2

  def processOdd (num: Long): Long = 3 * num + 1

  def process (num: Long): Tuple2[Long, Int] = {
    var currNumber = num
    var sequenceCount = 1
    while (currNumber != 1) {
      if (currNumber % 2 == 0) {
        currNumber = processEven(currNumber)
      }
      else {
        currNumber = processOdd(currNumber)
      }
      sequenceCount += 1
    }
    (num, sequenceCount)
  }

  def answer () = {
    var longestResult = Tuple2[Long, Int](1L, 1)
    var ii = 999999L
    while (ii > 1) {
      var candidate = process(ii)
      if (candidate._2 > longestResult._2) {
        longestResult = candidate
      }
      ii -= 1
    }
    longestResult
  }

  def main (args: Array[String]) = {
     println(answer)
  }
}