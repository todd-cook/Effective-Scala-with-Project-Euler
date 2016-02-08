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
package com.wordtrellis.projecteuler

/**
 * Problem 92
 * A number chain is created by continuously adding the square of the digits in
 * a number to form a new number until it has been seen before.
 *
 * For example,
 *
 * 44 ->  32  ->  13  ->  10  ->  1  ->  1
 * 85  ->  89  ->  145  ->  42  ->  20  ->  4  ->  16  ->  37  ->  58  ->  89
 *
 * Therefore any chain that arrives at 1 or 89 will become stuck in an endless
 * loop. What is most amazing is that EVERY starting number will eventually
 * arrive at 1 or 89.
 *
 * How many starting numbers below ten million will arrive at 89?
 *
 * @author Todd Cook
 * @since 5/22/2011
 */

object problem_92{

    def buildChain (a:Int, desiredGoal :Int, undesiredGoal :Int) :Boolean = {
        val ints = a.toString.toList.map(a => java.lang.Integer.parseInt(a +""))
        val squareSum = ints.zip(ints).map(b => b._1 * b._2).sum
        if (squareSum == desiredGoal) return true
        if (squareSum == undesiredGoal) return false
        buildChain(squareSum, desiredGoal , undesiredGoal)
    }

    def answer() ={
        var eightyNine = 0
        (2 to 10000000).par.foreach(a => if (buildChain(a, 89, 1)) eightyNine += 1)
        eightyNine
    }

    def main (args: Array[String]) = {
        println (answer)
    }
}
/**
 * 8581146
 */