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
  private var placeDivisors = new ListBuffer[Int]()

  def seed = num

  def places () = placeDivisors.toList

  def initialize () = {
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

  override def toString () = placeDivisors.mkString("")
}