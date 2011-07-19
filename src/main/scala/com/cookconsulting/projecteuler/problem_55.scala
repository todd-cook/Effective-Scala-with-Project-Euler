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
 * Problem 55
 *
 * If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
 *
 * Not all numbers produce palindromes so quickly. For example,
 *
 * 349 + 943 = 1292,
 * 1292 + 2921 = 4213
 * 4213 + 3124 = 7337
 *
 * That is, 349 took three iterations to arrive at a palindrome.
 *
 * Although no one has proved it yet, it is thought that some numbers, like 196,
 * never produce a palindrome. A number that never forms a palindrome through
 * the reverse and add process is called a Lychrel number. Due to the
 * theoretical nature of these numbers, and for the purpose of this problem,
 * we shall assume that a number is Lychrel until proven otherwise. In addition
 * you are given that for every number below ten-thousand, it will either (i)
 * become a palindrome in less than fifty iterations, or, (ii) no one, with
 * all the computing power that exists, has managed so far to map it to a
 * palindrome. In fact, 10677 is the first number to be shown to require
 * over fifty iterations before producing
 * a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).
 *
 * Surprisingly, there are palindromic numbers that are themselves Lychrel
 * numbers; the first example is 4994.
 *
 * How many Lychrel numbers are there below ten-thousand?
 *
 * @author Todd Cook
 * @since 5/15/2011
 */

object problem_55 {

    def isPalindromic (a: Long): Boolean = (a.toString.reverse == a.toString)

    def isLychrel (a: Long): Boolean = {
        var candidate = a
        (1 to 30).foreach (b => {
            var newCandidate = candidate + candidate.toString.reverse.toLong
            if (isPalindromic (newCandidate)) return false
            else candidate = newCandidate
        })
        true
    }

    def answer () = {
        var ii = 0
        (1 until 10000).foreach (a => if (isLychrel (a + 0L)) ii += 1)
      println(  ii + " Lychrel numbers")
    }

    def main (args: Array[String]) = {
                println(isLychrel(56L))
                println(isLychrel(57L))
                println(isLychrel(59L))
                println(isLychrel(89L))
        println (answer)
    }
}

/**
 *
 * Commentary
 * notes from: http://en.wikipedia.org/wiki/Lychrel_numbers
 * The reverse and add process produces the sum of a number and the number
 * formed by reversing the order of its digits. e.g.
 * 56 + 65 = 121, 125 + 521 = 646.
 *
 * Some numbers become palindromes quickly after repeated reversal and
 * addition, and are therefore not Lychrel numbers. All 1 digit and 2
 * digit numbers eventually become palindromes after repeated reversal
 * and addition. About 80% of all numbers under 10,000 resolve into a
 * palindrome in 4 or fewer steps. About 90% solve in 7 steps or less.
 * Here are a few examples of non-Lychrel numbers:
 *
 * 56 becomes palindromic after one iteration: 56+65 = 121.
 * 57 becomes palindromic after two iterations: 57+75 = 132, 132+231 = 363.
 * 59 is not a Lychrel number since it becomes a palindrome after
 * 3 iterations: 59+95 = 154, 154+451 = 605, 605+506 = 1111
 * 89 takes an unusually large 24 iterations (the most of any number
 * under 10,000 that is known to resolve into a palindrome)
 * to reach the palindrome 8813200023188.
 */