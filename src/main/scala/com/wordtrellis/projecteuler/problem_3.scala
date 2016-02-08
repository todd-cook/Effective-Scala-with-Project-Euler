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
 *
 *  Problem 3
 *  The prime factors of 13195 are 5, 7, 13 and 29.
 *  What is the largest prime factor of the number 600851475143 ?
 *
 * @author : Todd Cook
 * @since : 4/24/11
 */

object problem_3 {

  def answer = {
    lazy val naturals: Stream[Int] = Stream.cons(1, naturals.map(_ + 1))
    var theNum = 600851475143L
    val result = naturals.drop(1).dropWhile(n => {
      while (theNum % n == 0) {
        println(n)
        theNum /= n
      }
      theNum > 1
    })
    result(0)
  }

  /**
   * So the Stream of prime factors stars out with 2 in order to give a starting place for
   * looking for factors. The while loop then iterates through Longs starting with the last
   * value in primes. Once a factor is found, that factor is divided out of limit and the
   * new factor (tmp) is returned by the anonymous function as the next Long in primes.
   * This continues until all the prime factors of limit are found, at which point limit is
   * 1 and the loop just keeps returning the highest factor that was found.
   * The next line forces the lazy evaluation for the first 10 factors of primes, and then prints
   * the last factor, which gives us the answer. 10 was just an arbitrary number large enough
   * to get the factor we were looking for. A more general solution would take from primes until
   * it got two sequential elements that were the same, and then print that one.
   *
   * from http://grokcode.com/75/learning-scala-with-project-euler/
   */
  def answer2 = {
    var limit = 600851475143L
    lazy val primes: Stream[Long] =
      Stream.cons(2,
                  primes.map(x => {
                    var tmp = x
                    while ((limit % tmp) != 0 && limit != 1) {
                      tmp += 1
                    }
                    limit = limit / tmp;
                    tmp
                  }))

    print(primes.take(10).last)
  }

  def main (args: Array[String]) = {
    println(answer)
    println(answer2)
  }
}