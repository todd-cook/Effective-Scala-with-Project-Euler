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
 *  Problem 97
 *
 *  The first known prime found to exceed one million digits was discovered in 1999, and is a
 *  Mersenne prime of the form 26972593−1; it contains exactly 2,098,960 digits. Subsequently
 *  other Mersenne primes, of the form 2p−1, have been found which contain more digits.
 *
 *  However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits:
 *  28433 * 2^7830457 + 1
 *
 *  Find the last ten digits of this prime number.
 * @author Todd Cook
 * @since 5/22/2011
 */

object problem_97 {

  def answer() = {
    var start = System.currentTimeMillis
    val seed = new java.math.BigInteger("28433")
    val bigTwo = new java.math.BigInteger("2").pow(7830457)
    val nonMersenne = seed.multiply(bigTwo).add(java.math.BigInteger.ONE).toString
    println("Computation took %f seconds".format((System.currentTimeMillis - start) / 1000d))
    nonMersenne.slice(nonMersenne.length - 10, nonMersenne.length)
 }

  def main(args: Array[String]) {
    println(answer)
  }
}

/**
 * Computation took 1098.184000 seconds
 * 8739992577
 * at 128m
 */