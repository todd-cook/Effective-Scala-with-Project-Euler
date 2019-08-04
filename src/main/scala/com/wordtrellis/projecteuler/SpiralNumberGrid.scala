
package com.wordtrellis.projecteuler

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

  val middle: Int = items / 2
  val MAX: Int = items * items
  // Temp data
  private var currPosition: (Int, Int) = (middle, middle)
  private val values = new ListBuffer[ListBuffer[Int]]()
  // prepopulate the number grid
  (1 to items).foreach(a => {
    val row = new ListBuffer[Int]()
    row.appendAll((1 to items).toList.map(_ * 0))
    values.append(row)
  })

  private def right(seed: (Int, Int)) = (seed._1, seed._2 + 1)

  private def down(seed: (Int, Int)) = (seed._1 + 1, seed._2)

  private def left(seed: (Int, Int)) = (seed._1, seed._2 - 1)

  private def up(seed: (Int, Int)) = (seed._1 - 1, seed._2)

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

  override def toString: String = {
    val buf = new StringBuilder()
    (0 to values.size - 1).foreach(ii => buf.append(values(ii).mkString("  ") + "\n"))
    buf.toString
  }

  def getTopLeftToBottomRightDiagonal :List[Int] = {
    val diagonalValues = new ListBuffer[Int]()
    (0 to values(0).length - 1).foreach(x => diagonalValues.append(values(x)(x)))
    diagonalValues.toList
  }

  def getBottomLeftToTopRightDiagonal :List[Int] = {
    val diagonalValues = new ListBuffer[Int]()
    val colvals = (0 to values(0).length - 1).toList
    val rowvals = (0 to values(0).length - 1).toList.reverse
    colvals.foreach(x => diagonalValues.append(values(rowvals(x))(colvals(x))))
    diagonalValues.toList
  }
}

/**
 * Produces a spiral where the first move is up
 * Another version with more clutter
 */
class SpiralNumberGrid2(items: Int, start: Int) {
  val middle: Int = java.lang.Math.rint((0.2d + items) / 2).intValue
  var rows = 0
  var columns = 0
  // now StartPosition: 2,3, Direction East, initial value 0
  var currPosition: (Int, Int) = (middle - 2, middle - 1)
  //(middle, middle)
  // prepopulate the number grid
  val values = new ListBuffer[ListBuffer[Int]]()
  var direction = 1
  var lineLength = 1
  var ii = 2

  def spiral() {
    var (row, col) = currPosition
    val currDirection = direction % 4
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

  override def toString: String = {
    val buf = new StringBuilder()
    (0 to values.size - 1).foreach(ii => buf.append(values(ii).mkString("  ") + "\n"))
    buf.toString
  }

  def getTopLeftToBottomRightDiagonal: List[Int] = {
    val diagonalValues = new ListBuffer[Int]()
    (0 to values(0).length - 1).foreach(x => diagonalValues.append(values(x)(x)))
    diagonalValues.toList
  }

  def getBottomLeftToTopRightDiagonal: List[Int] = {
    val diagonalValues = new ListBuffer[Int]()
    val colvals = (0 to values(0).length - 1).toList
    val rowvals = (0 to values(0).length - 1).toList.reverse
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
      val row = new ListBuffer[Int]()
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
  def main(args: Array[String]): Unit = {
    val sng = new SpiralNumberGrid(5, 1)
    println(sng)
  }
}
