

package com.wordtrellis.projecteuler

import collection.mutable.ListBuffer
import collection.mutable.HashMap


/**
 * Problem 32
 *
 * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n
 * exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
 *
 * The product 7254 is unusual, as the identity, 39 * 186 = 7254, containing multiplicand,
 * multiplier, and product is 1 through 9 pandigital.
 *
 * Find the sum of all products whose multiplicand/multiplier/product identity can be written as
 * a 1 through 9 pandigital.
 * HINT: Some products can be obtained in more than one way so be sure to only include it once
 * in your sum.
 *
 * @author : Todd Cook

 */
object problem_32 {

  /**
   * This pandigital formula: 2 digits * 3 digits = 4 digits
   * is the only way it works; experimentation showed that
   * 1 digit * 4 digits = 4 digits does not yield pandigital permutations
   */
  def formPandigitalProductCombo (numbers: List[Int]): (Int, Int, Int) = {
    val groupOne = (numbers(0) * 10) + numbers(1)
    val groupTwo = (numbers(2) * 100) + (numbers(3) * 10) + numbers(4)
    val groupThree = (numbers(5) * 1000) + (numbers(6) * 100) + (numbers(7) * 10) + numbers(8)
    (groupOne, groupTwo, groupThree)
  }

  def checkPandigitalPermutation (pandigitalCombo: (Int, Int, Int)): Boolean =
    pandigitalCombo._1 * pandigitalCombo._2 == pandigitalCombo._3

  def main (args: Array[String]) {
    // generate all permutations; 9! = 362880; but test each one immediately, no need to store
    val pg = new PermutationGenerator(9)
    val answers = new ListBuffer[(Int, Int, Int)]()
    while (pg.hasMore()) {
      val b = formPandigitalProductCombo(pg.next())
      if (checkPandigitalPermutation(b)) {
        answers.append(b)
      }
    }
    println("Total permutations: " + pg.getTotal)
    println("Found " + answers.size + " pandigital permutations")
    println(answers.toList)
    // now we isolate the products that only occur once
    val map = new  HashMap[Int, (Int, Int)]()
    val solutions = new ListBuffer[(Int, Int, Int)]()
    answers.toList.foreach(x => map.put(x._3, (x._1, x._2)))
    // this is a little clumsy and ugly, however other, more syntactically sugary attempts failed.
    val it = map.keySet.iterator
    while (it.hasNext) {
      val a = it.next
      val tmp = map.get(a).get
      solutions.append((tmp._1, tmp._2, a))
    }
    println(solutions.toList)
    val products = for {product <- solutions.toList} yield product._3
    println(products)
    println("sum of products: " + products.sum)
  }
}
