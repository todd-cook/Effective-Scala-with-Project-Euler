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
 * Problem 6
    The sum of the squares of the first ten natural numbers is,
    1^(2) + 2^(2) + ... + 10^(2) = 385
    The square of the sum of the first ten natural numbers is,
    (1 + 2 + ... + 10)^(2) = 55^(2) = 3025
    Hence the difference between the sum of the squares of the first
    ten natural numbers and the square of the sum is 3025 - 385 = 2640
    Find the difference between the sum of the squares of the first
    one hundred natural numbers and the square of the sum.

TODO fix
implicit def iterableWithSum(it: Iterable[Int]) = new { def sum: BigInt = it.foldLeft(0)(_+_) }
// result is coerced to be a BigInt, so I can take advantage of the "pow" method on BigInt.
(1 to 100).sum.pow(2) - (1 to 100).map(n => n * n).sum
 * @author : Todd Cook
 * @since : 4/24/2011
 */

object problem_6 {


    /**
     * Gauss's closed form expression for summation series
     */
    def sumOfRange (n: Int) = n * (n + 1) / 2

    /**
     * Gauss's closed form expression for sum of consecutive squares
     */
    def sumOfSquares (n: Int) = {(2 * n + 1) * (n + 1) * n / 6}

    def differenceBetweenSumOfSquaresAndSquareOfSum( n: Int)={
      math.pow(sumOfRange(n), 2).toInt -  sumOfSquares (n)
    }

    /**
     * Brute force
     */
    def answer = {
        var sum = (1 to 100).foldLeft (0) (_ + _);
        var squareSum = sum * sum

        var sumSquare: Int = 0
        (1 to 100).foreach (n => sumSquare += n * n)
        println (squareSum - sumSquare)
        (squareSum - sumSquare)
    }

    def main (args: Array[String]) = {
        println (answer)
        println( differenceBetweenSumOfSquaresAndSquareOfSum (10))
    }
}