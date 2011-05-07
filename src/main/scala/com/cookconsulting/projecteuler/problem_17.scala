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
 * Problem 17
 * If the numbers 1 to 5 are written out in words: one, two, three, four,
 * five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 *
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out
 * in words, how many letters would be used?
 *
 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
 * forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
 * 20 letters. The use of "and" when writing out numbers is in compliance
 * with British usage.
 *
 * @author : Todd Cook
 * @since : 4/24/2011
 */

object NumberString extends Enumeration {
  type NumberString = Value
  val one = Value(1, "one")
  val two = Value(2, "two")
  val three = Value(3, "three")
  val four = Value(4, "four")
  val five = Value(5, "five")
  val six = Value(6, "six")
  val seven = Value(7, "seven")
  val eight = Value(8, "eight")
  val nine = Value(9, "nine")
  val ten = Value(10, "ten")
  val eleven = Value(11, "eleven")
  val twelve = Value(12, "twelve")
  val thirteen = Value(13, "thirteen")
  val fourteen = Value(14, "fourteen")
  val fifteen = Value(15, "fifteen")
  val sixteen = Value(16, "sixteen")
  val seventeen = Value(17, "seventeen")
  val eighteen = Value(18, "eighteen")
  val nineteen = Value(19, "nineteen")
  val twenty = Value(20, "twenty")
  val thirty = Value(30, "thirty")
  val forty = Value(40, "forty")
  val fifty = Value(50, "fifty")
  val sixty = Value(60, "sixty")
  val seventy = Value(70, "seventy")
  val eighty = Value(80, "eighty")
  val ninety = Value(90, "ninety")
}

object problem_17 {
  val number_strings = NumberString.values.toList.map(_.toString)
  val number_values = NumberString.values.toList.map(_.id)
  val stringsToNumbers = number_strings zip number_values toMap
  val numbersToStrings = number_values zip number_strings toMap

  def numberToWords (n: Int): String = {
    var result = ""
    if (n.toString.length >= 4 && n > 999 && n < 99999) {
      result = numbersToStrings.get((n.toString.slice(
        0, n.toString.length - 3)).toInt).get + " thousand"
      if (n % 1000 == 0) {
        return result;
      }
      else {

        var numString = n.toString
        if (n.toString.length == 5) {
          numString = numString.slice(2, 5)
        }
        else {
          numString = numString.slice(1, 4)
        }
        if (numString.toInt < 100) {
          return result + " and " + numberToWords(numString.toInt)
        }
        else {
          return result + " " + numberToWords(numString.toInt)
        }
      }
    }
    if (n.toString.length == 3 && n > 0 && n <= 999) {
      result = numbersToStrings.get(
        (n.toString.slice(0, 1)).toInt).get + " hundred"
      if (n % 100 == 0) {
        return result;
      }
      else {
        result += " and " + numberToWords(n.toString.slice(1, 3).toInt)
      }
    }
    if (n.toString.length <= 2 && n < 21 && n > 0) {
      result += numbersToStrings.get(n).get
    }
    if (n > 20 && n < 100) {
      var tensValue = numbersToStrings.get(
        (n.toString.slice(0, 1) + "0").toInt).get
      if ((n.toString.slice(1, 2)).toInt != 0) {
        var onesValue = numbersToStrings.get(
          (n.toString.slice(1, 2)).toInt).get
        result += tensValue + "-" + onesValue
      }
      else {
        result += tensValue
      }
    }
    result
  }

  def answer = {
    (1 to 1000).toList.map(x =>
                             numberToWords(x)).mkString.replaceAll("\\s ", "")
      .replaceAll("-", "").length
  }

  def main (args: Array[String]) = {
    //        println (numberToWords (15280))
    //        println (numberToWords (15000))
    println(answer)
  }
}