

package com.wordtrellis.projecteuler

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
 *
 */

object problem_28 {

  def answer() {
    val numberGrid = new SpiralNumberGrid(1001, 1)
    val tlbrDiagonalSum = numberGrid.getTopLeftToBottomRightDiagonal.foldLeft(0)(_ + _)
    val bltrDiagonalSum = numberGrid.getBottomLeftToTopRightDiagonal.foldLeft(0)(_ + _)
    println("diagonal tlbrDiagonal sum for a 1001 number grid: " + tlbrDiagonalSum)
    println("diagonal bltrDiagonal sum for a 1001 number grid: " + bltrDiagonalSum)
    // we have to subtract 1, because it is the axis value of the diagonals, and otherwise
    // it would be counted twice when it appears only once
    println("Sum of the diagonals for a 1001 number grid: %d ".format(
      tlbrDiagonalSum + bltrDiagonalSum - 1))
  }

  def main(args: Array[String]): Unit = answer()
}