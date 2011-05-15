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
 * Spiral Number Grid creates a grid of numbers that spiral.
 * Used to solve Project Euler problem 28
 *
 * This class will need some refinement before it grow up into a useful library class
 * @author Todd Cook
 * @since 5/10/11 11:50 PM
 */

import collection.mutable.ListBuffer

/**
 * Produces a spiral number grid in the form of a clockwise outline of the letter G:
 *   7  8  9
 *   6  1  2
 *   5  4  3
 *
 */
class SpiralNumberGrid(items: Int, start: Int) {

  val middle = items / 2
  val MAX = items * items
  // Temp data
  private var currPosition: Tuple2[Int, Int] = (middle, middle)
  private var values = new ListBuffer[ListBuffer[Int]]()
  // prepopulate the number grid
  (1 to items).foreach(a => {
    var row = new ListBuffer[Int]()
    row.appendAll((1 to items).toList.map(_ * 0))
    values.append(row)
  })

  private def right(seed: Tuple2[Int, Int]) = (seed._1, seed._2 + 1)

  private def down(seed: Tuple2[Int, Int]) = (seed._1 + 1, seed._2)

  private def left(seed: Tuple2[Int, Int]) = (seed._1, seed._2 - 1)

  private def up(seed: Tuple2[Int, Int]) = (seed._1 - 1, seed._2)

  private var loop = 1
  private var ii = 1

  private def spiral(): Unit = {
    try {
      while (ii < MAX) {
        (1 to loop).foreach(a => {
          values(currPosition._1)(currPosition._2) = ii
          currPosition = right(currPosition)
          ii += 1
          if (ii > MAX) {
            return
          }
          values(currPosition._1)(currPosition._2) = ii
        })
        (1 to loop).foreach(a => {
          currPosition = down(currPosition)
          ii += 1
          values(currPosition._1)(currPosition._2) = ii
        })
        (1 to (loop + 1)).foreach(a => {
          currPosition = left(currPosition)
          ii += 1
          values(currPosition._1)(currPosition._2) = ii
        })
        (1 to (loop + 1)).foreach(a => {
          currPosition = up(currPosition)
          ii += 1
          values(currPosition._1)(currPosition._2) = ii
        })
        loop += 2
      }
    }
    catch {
      case ex: Exception => println("failure at ii: " + ii)
    }
  }
  // create the spiral on construction
  spiral()

  override def toString() = {
    var buf = new StringBuilder()
    (0 to values.size - 1).foreach(ii => buf.append(values(ii).mkString("  ") + "\n"))
    buf.toString
  }

  def getTopLeftToBottomRightDiagonal :List[Int] = {
    var diagonalValues = new ListBuffer[Int]()
    (0 to values(0).length - 1).foreach(x => diagonalValues.append(values(x)(x)))
    diagonalValues.toList
  }

  def getBottomLeftToTopRightDiagonal :List[Int] = {
    var diagonalValues = new ListBuffer[Int]()
    var colvals = (0 to values(0).length - 1).toList
    var rowvals = (0 to values(0).length - 1).toList.reverse
    colvals.foreach(x => diagonalValues.append(values(rowvals(x))(colvals(x))))
    diagonalValues.toList
  }
}

/**
 * Produces a spiral where the first move is up
 * Another version with more clutter
 */
class SpiralNumberGrid2(items: Int, start: Int) {
  var middle = java.lang.Math.rint((0.2d + items) / 2).intValue
  var rows = 0
  var columns = 0
  // now StartPosition: 2,3, Direction East, initial value 0
  var currPosition: Tuple2[Int, Int] = (middle - 2, middle - 1)
  //(middle, middle)
  // prepopulate the number grid
  var values = new ListBuffer[ListBuffer[Int]]()
  var direction = 1
  var lineLength = 1
  var ii = 2

  def spiral() {
    var (row, col) = currPosition
    var currDirection = direction % 4
    // the clockwise spiral goes in a progression:
    // East, South, West, North
    if (currDirection == 0) {
      // North
      lineLength += 1
      (1 to lineLength).foreach(newVal => {
        row -= 1
        ii += 1
        if (row > -1 && col > -1) {
          values(row).update(col, ii)
        }
      }
      )
    }
    else if (currDirection == 1) {
      //East
      (1 to lineLength).foreach(newVal => {
        col += 1
        ii += 1
        if (row > -1 && col > -1) {
          values(row).update(col, ii)
        }
      })
    }
    else if (currDirection == 2) {
      // South
      lineLength += 1
      (1 to lineLength).foreach(newVal => {
        row += 1
        ii += 1
        if (row > -1 && col > -1) {
          values(row).update(col, ii)
        }
      })
    }
    else if (currDirection == 3) {
      //West
      (1 to lineLength).foreach(newVal => {
        col -= 1
        ii += 1
        if (row > -1 && col > -1) {
          values(row).update(col, ii)
        }
      })
    }
    direction += 1
    currPosition = (row, col)
  }

  override def toString() = {
    var buf = new StringBuilder()
    (0 to values.size - 1).foreach(ii => buf.append(values(ii).mkString("  ") + "\n"))
    buf.toString
  }

  def getTopLeftToBottomRightDiagonal(): List[Int] = {
    var diagonalValues = new ListBuffer[Int]()
    (0 to values(0).length - 1).foreach(x => diagonalValues.append(values(x)(x)))
    diagonalValues.toList
  }

  def getBottomLeftToTopRightDiagonal(): List[Int] = {
    var diagonalValues = new ListBuffer[Int]()
    var colvals = (0 to values(0).length - 1).toList
    var rowvals = (0 to values(0).length - 1).toList.reverse
    colvals.foreach(x => diagonalValues.append(values(rowvals(x))(colvals(x))))
    diagonalValues.toList
  }

  /**
   * The current position is the starting place, and it must be moved back
   * two positions, once because ListBuffer, like List, has elements that are
   * addressed with a zero based index, secondly the value is shifted because
   * with each turn of the spiral it will be augmented
   *       0            // clockwise directions with mod % percent values
   *    3     1
   *       2
   */
  def initialize() {
    for (newRow <- 1 to items) {
      var row = new ListBuffer[Int]()
      row.appendAll((1 to items).toList.map(_ * 0))
      values.append(row)
    }
    //println("initial grid: \n" + this.toString)
    // prepopulate positions 1 and two
    //set middle
    values(middle - 1).update(middle - 1, 1)
    // set the second position
    values(middle - 2).update(middle - 1, 2)
    while (ii < (items * items)) {
      spiral()
    }
  }
  initialize()
}

object SpiralNumberGrid {
  def main(args: Array[String]) = {
    val sng = new SpiralNumberGrid(5, 1)
    println(sng)
  }
}
