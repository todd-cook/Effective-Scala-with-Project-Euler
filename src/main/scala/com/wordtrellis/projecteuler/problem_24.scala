package com.wordtrellis.projecteuler

/**
  * Problem 24
  * A permutation is an ordered arrangement of objects. For example, 3124 is one possible
  * permutation of the digits 1, 2, 3 and 4.
  * If all of the permutations are listed numerically or alphabetically, we call it lexicographic
  * order. The lexicographic permutations of 0, 1 and 2 are:
  *
  * 012   021   102   120   201   210
  *
  * What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
  *
  * @author : Todd Cook
  *
  */
object problem_24 {

  def main(args: Array[String]): Unit = {
    answer()
  }

  def answer(): Unit = {
    val pg  = new PermutationGenerator(10)
    var inc = 1

    while (inc < 1000000) {
      pg.next()
      inc += 1
    }
    val millionthPermutation = pg.next()
    println(millionthPermutation)
    // lower the permutations by one, since the answer wants zero
    println(millionthPermutation.map(a => a - 1).mkString(""))
  }
}
