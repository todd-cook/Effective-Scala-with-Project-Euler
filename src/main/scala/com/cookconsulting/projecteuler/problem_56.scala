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

/**
 * Problem 56
 *
 * A googol (10^(100)) is a massive number: one followed by one-hundred zeros;
 * 100^(100) is almost unimaginably large: one followed by two-hundred zeros.
 * Despite their size, the sum of the digits in each number is only 1.
 *
 * Considering natural numbers of the form, a^(b), where a, b < 100,
 * what is the maximum digital sum?
 *
 * @author Todd Cook
 * @since 5/15/2011
 */

object problem_56 {

    def sumDigits (str: String): Int = str.toList.map (a => java.lang.Integer.parseInt (a + "")).sum

    def answer () = {
        var winner = (0, 0)
        var high = 0
        (1 until 100).foreach (a => {
            (1 until 100).foreach (b => {
                var tmp = sumDigits (new java.math.BigInteger (a.toString).pow (b).toString)
                if (tmp > high) {
                    high = tmp
                    winner = (a, b)
                }
            })
        })
        println (winner)
        high
    }

    def main (args: Array[String]) = {
        println (answer)
    }
}