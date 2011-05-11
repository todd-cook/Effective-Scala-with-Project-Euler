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
 * Problem 28
 *
 * Starting with the number 1 and moving to the right in a clockwise direction
 * a 5 by 5 spiral is formed as follows:
 *
 *  21 22 23 24 25
 *  20  7  8  9 10
 *  19  6  1  2 11
 *  18  5  4  3 12
 *  17 16 15 14 13
 *
 * It can be verified that the sum of the numbers on the diagonals is 101.
 *
 * What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
 * formed in the same way?
 *
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 */

object problem_28 {

  def answer () {
    val numberGrid = new SpiralNumberGrid(1001, 1)
    val tlbrDiagonalSum = numberGrid.getTopLeftToBottomRightDiagonal.foldLeft(0)(_ + _)
    val bltrDiagonalSum = numberGrid.getBottomLeftToTopRightDiagonal.foldLeft(0)(_ + _)
    println("diagonal tlbrDiagonal sum for a 1001 number grid: " + tlbrDiagonalSum)
    println("diagonal bltrDiagonal sum for a 1001 number grid: " + bltrDiagonalSum)
    println("Sum of the diagonals for a 1001 number grid: " + (tlbrDiagonalSum + bltrDiagonalSum))
  }

  def main (args: Array[String]) = answer()
}