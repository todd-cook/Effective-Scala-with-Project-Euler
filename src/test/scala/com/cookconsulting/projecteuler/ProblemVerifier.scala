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
 * Frequently, Project Euler problems will have an example that may be used
 * to test the behavior of a generified function solution.
 * Here we provide unit tests as a basic verification of the algorithm
 * implementations.
 * We purposely exclude any problems that have an excessively long running time
 * @author : Todd Cook
 * @since : 4/24/11
 */

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class ProblemVerifier extends AssertionsForJUnit {

    @Test
    def test_problem_1 () {
        assert (problem_1.factorsOfThreeOrFive (10) === List (3, 5, 6, 9))
        assert (problem_1.sumList (problem_1.factorsOfThreeOrFive (10)) == 23)
    }

    @Test
    def test_problem_2 () {
        assert ((problem_2.makeFibo (89, Nil)) ===
                List (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89))
    }

    @Test
    def test_problem_4 () {
        assert (problem_4.findLargestPalindromicNumber (2) ===
                Tuple3 (91, 99, 9009))
    }

    @Test
    def test_problem_5 () {
        assert (problem_5.calcSmallestNumberDivisibleByRange (10) === 2520)
    }

    @Test
    def test_problem_6 () {
        assert (math.pow (problem_6.sumOfRange (10), 2) === 3025)
        assert (problem_6.sumOfSquares (10) === 385)
        assert (problem_6.differenceBetweenSumOfSquaresAndSquareOfSum (10) === 2640)
    }

    @Test
    def test_problem_7 () {
        var shortPrimeList = problem_7.getPrimeList (6)
        // 2, 3, 5, 7, 11, and 13
        val desiredResult = List (2, 3, 5, 7, 11, 13)
        assert (shortPrimeList === desiredResult)
        shortPrimeList.foreach (x => if (!problem_7.isPrime (x)) {
            fail ("prime failure")
        })
    }

    @Test
    def test_problem_8 () {
         assert (problem_8.findLargestProduct ("1234560888888888882111", 5) === 32768)
    }


}