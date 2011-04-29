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
Problem 9
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^(2) + b^(2) = c^(2)

For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 */

object problem_9 {

    def isPythagoreanTriplet (x: Tuple3[Int, Int, Int]): Boolean ={
                                                ((x._1 * x._1) + (x._2 * x._2) == (x._3 * x._3)) }

    def isTarget (x: Tuple3[Int, Int, Int], result: Int): Boolean = {
        (x._1 + x._2 + x._3 == result)
    }

    def findPythagoreanTriplet (result: Int): Tuple3[Int, Int, Int] = {
        var ceiling = result / 10
        (0 to ceiling).toList.foreach (x => {
            (0 to ceiling).toList.foreach (y => {
                if (x > y) {
                    val pythagTriplet = euclidsFormula (x, y)
                    if (isPythagoreanTriplet (pythagTriplet)) {
                        // println ("pythagorean triplet " + x + " " + y)
                        if (isTarget (pythagTriplet, result)) {
                            //  println ("result found")
                            return pythagTriplet;
                        }
                    }
                }
            })
        })
        (0, 0, 0)
    }

    /**
     * Euclid's formula for Pythagorean Triple
     * see  http://en.wikipedia.org/wiki/Pythagorean_triplet
     */
    def euclidsFormula (m: Int, n: Int): Tuple3[Int, Int, Int] = {
        require (m > n)
        val a = 2 * m * n
        val b = (m * m) - (n * n)
        val c = (m * m) + (n * n)
        (a, b, c)
    }

    def main (args: Array[String]) {
        println (findPythagoreanTriplet (1000))
    }
}