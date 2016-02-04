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
 *
 *  Problem 4
 *  A palindromic number reads the same both ways.
 *  The largest palindrome made from the product of two 2-digit numbers is
 *  9009 = 91 * 99.
 *  Find the largest palindrome made from the product of two 3-digit numbers.
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 */

object problem_4 {

    def isPalindrome (s: String): Boolean = s.reverse.mkString == s

    def createCeilingNumber (digits: Int): Int = ("9" * digits).toInt

    def createPalindromicNumberList (digitPlacesStart: Int,
                                     digitPlacesEnd: Int) :List[Tuple3[Int, Int, Int]] = {
        require(digitPlacesStart < digitPlacesEnd)
        val palindromes =
            for (a <- (createCeilingNumber (digitPlacesStart) + 1 to
                           createCeilingNumber (digitPlacesEnd)).toList;
                 b <- (a to createCeilingNumber (digitPlacesEnd));
                 p = a * b
                 if isPalindrome (p.toString)
            ) yield (a, b, p)
        palindromes
    }

    def findLargestPalindromicNumber (digits: Int): Tuple3[Int, Int, Int] = {
        createPalindromicNumberList (digits - 1, digits).sortWith (_._3 > _._3).head
    }

    def main (args: Array[String]) = {
        println (findLargestPalindromicNumber (3))
    }
}