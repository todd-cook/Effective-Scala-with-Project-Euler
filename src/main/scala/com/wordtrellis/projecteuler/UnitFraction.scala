
package com.wordtrellis.projecteuler

import collection.mutable.ListBuffer

/**
 * Unit Fraction is useful for determining the number of places it takes for a repeating decimal
 * to repeat.
 * see Project Euler problem 26
 * @author Todd Cook
 * @since 5/10/11 10:35 PM
 */

class UnitFraction (val num: Int, val MAX_SAMPLE_LENGTH: Int = 2000) {
  private val placeDivisors = new ListBuffer[Int]()

  def seed: Int = num

  def places (): List[Int] = placeDivisors.toList

  def initialize (): Unit = {
    val divisor = num
    var base = 1
    var processing = true
    placeDivisors.append(0)

    while (processing) {
      base = 10 * base
      if (base < divisor) {
        placeDivisors.append(0)
        base = 10 * base
      }
      val tmpResult = base / divisor
      base = base - (divisor * tmpResult) // .asInstanceOf[Int]
      placeDivisors.append(tmpResult.asInstanceOf[Int])
      if (base == 0) {
        processing = false
      }
      if (placeDivisors.length > MAX_SAMPLE_LENGTH) {
        processing = false
      }
    }
  }

  initialize()

  override def toString: String = placeDivisors.mkString("")
}