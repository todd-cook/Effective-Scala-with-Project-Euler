

package com.wordtrellis.projecteuler

import collection.mutable.ListBuffer

/**
 * Problem 38
 * Take the number 192 and multiply it by each of 1, 2, and 3:
 *  192 * 1 = 192
 *  192 * 2 = 384
 *  192 * 3 = 576
 *
 *  By concatenating each product we get the 1 to 9 pandigital, 192384576.
 *  We will call 192384576 the concatenated product of 192 and (1,2,3)
 *
 *  The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
 *  and 5, giving the pandigital, 918273645, which is the concatenated product
 *  of 9 and (1,2,3,4,5).
 *  What is the largest 1 to 9 pandigital 9-digit number that can be formed as
 *  the concatenated product of an integer with (1,2, ... , n) where n > 1?
 *
 * @author Todd Cook
 * @since 4/24/2011
 */

object problem_38 {

  def isPandigital(x: Int): Boolean = x.toString.toList.distinct.size == 9

  def productToNineDigits(x: Int): Int = {
    var result = ""
    var ii = 2
    while (result.length < 9) {
      val tmp = for {a <- (1 to ii).toList
      } yield {
        x * a
      }
      result = tmp.mkString("")
      ii += 1
    }
    if (result.length > 9) {
      return 0
    }
    result.toInt
  }

  def answer: Int = {
    val candidates = new ListBuffer[Int]()
    (1 to 1000000).foreach(a => {
      val prod = productToNineDigits(a)

      if (prod.toString.indexOf("0") == -1) {
        if (isPandigital(prod)) {
          println("%d yields: %d".format(a, productToNineDigits(a)))
          candidates.append(productToNineDigits(a))
        }
      }
    })
    candidates.toList.sortWith(_ > _)(0)
  }

  def main(args: Array[String]): Unit = {
    println(answer)
  }
}