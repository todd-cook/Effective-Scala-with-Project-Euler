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
 * Problem 11
 * In the 20*20 grid below, four numbers along a diagonal line have been marked in red.
 *
 * 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
 * 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
 * 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
 * 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
 * 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
 * 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
 * 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
 * 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
 * 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
 * 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
 * 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
 * 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
 * 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
 * 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
 * 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
 * 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
 * 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
 * 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
 * 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
 * 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
 *
 * The product of these numbers is 26 * 63 * 78 * 14 = 1788696.
 *
 * What is the greatest product of four adjacent numbers in any direction
 * (up, down, left, right, or diagonally) in the 20 * 20 grid?
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 */

object problem_11 {

  var numbers = List[List[Int]](
    List(8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8),
    List(49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0),
    List(81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65),
    List(52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91),
    List(22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80),
    List(24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50),
    List(32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70),
    List(67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21),
    List(24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72),
    List(21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95),
    List(78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92),
    List(16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57),
    List(86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58),
    List(19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40),
    List(4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66),
    List(88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69),
    List(4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36),
    List(20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16),
    List(20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54),
    List(1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48))

  def horizontalRow(column: Int, row: Int, amount: Int): List[Int] = {
    (0 until amount).toList.map(x => numbers(row - 1)(column - 1 + x))
  }

  def verticalRow(column: Int, row: Int, amount: Int): List[Int] = {
    (0 until amount).toList.map(x => numbers(row - 1 + x)(column - 1))
  }

  //  N
  //W   E
  //  S
  def nwseDiagonal(column: Int, row: Int, amount: Int): List[Int] = {
    (1 to amount).toList.map(x => numbers(row - 1 + x)(column - 1 + x))
  }

  def swneDiagonal(column: Int, row: Int, amount: Int): List[Int] = {
    val yList = (0 until amount).toList.reverse
    val xList = (0 until amount).toList
    var coords = yList.zip(xList)
    coords.map(a => numbers(a._1 + (row - amount))(a._2 + column))
  }

  def checkMaxCombo(item: List[Int], item2: List[Int]): List[Int] = {
    if (item.reduceLeft(_ * _) > item2.reduceLeft(_ * _)) item else item2
  }

  def answer() = {
    var totalLength = numbers(0).size
    var maxBoundary = numbers(0).size - 3
    var maxcombo = List[Int](0, 0, 0)
    (1 to totalLength).foreach(y => (1 to maxBoundary).foreach(x => {
      maxcombo = checkMaxCombo(maxcombo, horizontalRow(x, y, 4))
    }))
    // now we switch the x,y boundaries
    (1 to maxBoundary).foreach(y => (1 to totalLength).foreach(x => {
      maxcombo = checkMaxCombo(maxcombo, verticalRow(x, y, 4))
    }))
    // nwse  x:  0 to maxboundary
    //	y: 0 to maxboundary
    (0 to maxBoundary - 1).foreach(y => (0 to maxBoundary - 1).foreach(x => {
      maxcombo = checkMaxCombo(maxcombo, nwseDiagonal(x, y, 4))
    }))
    // swne  x: 0 to totalLength - 4
    //	Y:  4 to totalLength
    (4 to totalLength).foreach(y => (0 to totalLength - 4).foreach(x => {
      maxcombo = checkMaxCombo(maxcombo, swneDiagonal(x, y, 4))
    }))
    println(maxcombo)
    maxcombo.reduceLeft(_ * _)
  }

  def main(args: Array[String]) = {
    println(answer)
  }
}