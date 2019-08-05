package com.wordtrellis.projecteuler

import scala.collection.mutable.{HashSet, ListBuffer}

/**
  * Problem 18
  *
  * By starting at the top of the triangle below and moving to adjacent numbers
  * on the row below, the maximum total from top to bottom is 23.
  *
  * 3
  * 7 4
  * 2 4 6
  * 8 5 9 3
  *
  * That is, 3 + 7 + 4 + 9 = 23.
  *
  * Find the maximum total from top to bottom of the triangle below:
  *
  * 75
  * 95 64
  * 17 47 82
  * 18 35 87 10
  * 20 04 82 47 65
  * 19 01 23 75 03 34
  * 88 02 77 73 07 63 67
  * 99 65 04 28 06 16 70 92
  * 41 41 26 56 83 40 80 70 33
  * 41 48 72 33 47 32 37 16 94 29
  * 53 71 44 65 25 43 91 52 97 51 14
  * 70 11 33 28 77 73 17 78 39 68 17 57
  * 91 71 52 38 17 14 91 43 58 50 27 29 48
  * 63 66 04 68 89 53 67 30 73 16 69 87 40 31
  * 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
  *
  * NOTE: As there are only 16384 routes, it is possible to solve this problem
  * by trying every route. However, Problem 67, is the same challenge with a
  * triangle containing one-hundred rows; it cannot be solved by brute force,
  * and requires a clever method! ;o)
  *
  * @author : Todd Cook
  *
  */
object problem_18 {

  val combolist = List(
    List(75),
    List(95, 64),
    List(17, 47, 82),
    List(18, 35, 87, 10),
    List(20, 4, 82, 47, 65),
    List(19, 1, 23, 75, 3, 34),
    List(88, 2, 77, 73, 7, 63, 67),
    List(99, 65, 4, 28, 6, 16, 70, 92),
    List(41, 41, 26, 56, 83, 40, 80, 70, 33),
    List(41, 48, 72, 33, 47, 32, 37, 16, 94, 29),
    List(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),
    List(70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),
    List(91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),
    List(63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),
    List(4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23)
  )

  def main(args: Array[String]): Unit = answer()

  def answer(): Unit = {
    var row               = 1
    var currentCandidates = List(new CandidatePermutation(0, List(75)))
    while (row < combolist.length) {
      // create a new ListBuffer to hold each iteration of permutations
      val newCandidates = new ListBuffer[CandidatePermutation]()
      currentCandidates.foreach(candidate => {
        newCandidates.appendAll(generatePermutationsForRow(candidate, row, combolist))
      })
      //println("Permutations size before trimming: " +
      //           newCandidates.length + " for row: " + (row + 1))
      if (row > 8) { // } && row % 3 == 0) {
        condenseEarlyPermutations(newCandidates, row)
      }
      currentCandidates = newCandidates.toList
      println("Permutations size: " + currentCandidates.length + " for row: " + (row + 1))
      //    currentCandidates.foreach (println (_))
      row += 1
    }
    val candidates = currentCandidates.toList.sortWith(_.columns.sum > _.columns.sum)
    val topfive    = candidates.slice(0, 5)
    println("Top Five Results:")
    topfive.foreach(a => println(a.columns.mkString(", ") + " = " + a.columns.sum))
    println("Done!")
  }

  def generatePermutationsForRow(seed: CandidatePermutation,
                                 row: Int,
                                 combos: List[List[Int]]): List[CandidatePermutation] = {
    val col           = seed.lastColumn
    val less          = col //- 1
    val equal         = col
    val more          = col + 1
    val max           = combos(row).length - 1
    val tmpCandidates = new ListBuffer[CandidatePermutation]()
    if (less > -1) {
      tmpCandidates.append(
        new CandidatePermutation(less, (combos(row)(less) :: seed.columns.reverse).reverse))
    }
    if (more < max) {
      tmpCandidates.append(
        new CandidatePermutation(more, (combos(row)(more) :: seed.columns.reverse).reverse))
    }
    tmpCandidates.append(
      new CandidatePermutation(equal, (combos(row)(equal) :: seed.columns.reverse).reverse))
    tmpCandidates.toList
  }

  def condenseEarlyPermutations(candidates: ListBuffer[CandidatePermutation], row: Int): Unit = {
    val trimFactor = 5
    val trimRow    = row - trimFactor
    var currentMax = 0
    candidates.foreach(a => {
      val tmp = a.columns.slice(0, trimRow).sum
      if (tmp > currentMax) {
        currentMax = tmp
      }
    })
    candidates.foreach(a => {
      if (a.columns.slice(0, trimRow).sum < currentMax) {
        candidates.remove(candidates.indexOf(a))
      }
    })
    val hashSet = new HashSet[List[Int]]()
    candidates.foreach(a => hashSet.add(a.columns.slice(0, row - trimFactor)))
    //println("unique seeds starting with " + (trimRow + 1) + " digits" + hashSet.size)
    //println (hashSet.mkString)
  }

  class CandidatePermutation(val lastColumn: Int, val columns: List[Int]) {
    override def toString: String = columns.mkString(", ")
  }

  class Permutation(val values: List[Int]) {
    override def toString: String =
      "Values: " + values.mkString(", ") +
        " total sum of values: " + totalvalue()

    def totalvalue(): Int = values.sum
  }
}

/**
  * Commentary:
  * To solve this problem successfully and efficiently one has to limit the number of permutations,
  * however, limiting them too much can quickly distort the curve of maximum values and throw off
  * the final results. It is very important to know: Do I only care about the one final result? Or
  * would it also be helpful to have the closest candidates as well?
  *
  * Generating the permutations brute force leads to ballooning datasets & huge memory consumption:
  *
  * Permutations size: 1 for row: 2
  * Permutations size: 2 for row: 3
  * Permutations size: 5 for row: 4
  * Permutations size: 13 for row: 5
  * Permutations size: 35 for row: 6
  * Permutations size: 96 for row: 7
  * Permutations size: 267 for row: 8
  * Permutations size: 750 for row: 9
  * Permutations size: 2123 for row: 10
  * Permutations size: 6046 for row: 11
  * Permutations size: 17303 for row: 12
  * Permutations size: 49721 for row: 13
  * Permutations size: 143365 for row: 14
  * Permutations size: 414584 for row: 15
  *
  *  Top Five Results:
  *
  *  75, 95, 47, 87, 82, 75, 77, 65, 41, 72, 71, 70, 91, 66, 98 = 1112
  *  75, 95, 47, 87, 82, 75, 77, 28, 83, 32, 91, 78, 91, 67, 98 = 1106
  *  75, 95, 47, 87, 82, 75, 73, 28, 83, 32, 91, 78, 91, 67, 98 = 1102
  *  75, 95, 47, 87, 82, 75, 77, 28, 83, 32, 91, 73, 91, 67, 98 = 1101
  *  75, 95, 47, 87, 82, 75, 77, 65, 26, 72, 71, 70, 91, 66, 98 = 1097
  *
  *
  *  The same results can be accomplished through trimming the earlier, high-scoring permutations;
  *  i.e. after the 9th permutation has been reached, the high scoring permutation may be chosen
  *  as the defining route. The tuning of the algorithm depends on the standard deviation of the
  *  dataset points; and how long we are willing for the path value to anneal itself to a better
  *  value.
  *
  * Permutations size: 1 for row: 2
  * Permutations size: 2 for row: 3
  * Permutations size: 5 for row: 4
  * Permutations size: 13 for row: 5
  * Permutations size: 35 for row: 6
  * Permutations size: 96 for row: 7
  * Permutations size: 267 for row: 8
  * Permutations size: 750 for row: 9
  * Permutations size: 1373 for row: 10
  * Permutations size: 3923 for row: 11
  * Permutations size: 11257 for row: 12
  * Permutations size: 2038 for row: 13
  * Permutations size: 5981 for row: 14
  * Permutations size: 17551 for row: 15
  * Top Five Results:
  * 75, 95, 47, 87, 82, 75, 77, 65, 41, 72, 71, 70, 91, 66, 98 = 1112
  * 75, 95, 47, 87, 82, 75, 77, 28, 83, 32, 91, 78, 91, 67, 98 = 1106
  * 75, 95, 47, 87, 82, 75, 73, 28, 83, 32, 91, 78, 91, 67, 98 = 1102
  * 75, 95, 47, 87, 82, 75, 77, 28, 83, 32, 91, 73, 91, 67, 98 = 1101
  * 75, 95, 47, 87, 82, 75, 77, 65, 26, 72, 71, 70, 91, 66, 98 = 1097
  *
  *
  * Note: trimming every row after 9 is too aggressive:
  * ...Permutations size: 750 for row: 9
  * Permutations size: 1373 for row: 10
  * Permutations size: 1800 for row: 11
  * Permutations size: 1800 for row: 12
  * Permutations size: 2038 for row: 13
  * Permutations size: 1800 for row: 14
  * Permutations size: 1373 for row: 15
  * Top Five Results:
  * 75, 95, 47, 87, 82, 75, 77, 65, 41, 72, 71, 70, 91, 66, 98 = 1112
  * 75, 95, 47, 87, 82, 75, 77, 65, 26, 72, 71, 70, 91, 66, 98 = 1097
  * 75, 95, 47, 87, 82, 75, 77, 65, 41, 72, 71, 70, 71, 66, 98 = 1092
  * 75, 95, 47, 87, 82, 75, 77, 65, 41, 48, 71, 70, 91, 66, 98 = 1088
  * 75, 95, 47, 87, 82, 75, 77, 65, 41, 48, 71, 70, 91, 66, 98 = 1088
  *
  * Several lines of code are commented out above, you should uncomment them and run the code to
  * see more diagnostics.
  *
  */
