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

package com.wordtrellis.projecteuler

/**
 * Problem 22
 * Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over
 * five-thousand first names, begin by sorting it into alphabetical order. Then working out the
 * alphabetical value for each name, multiply this value by its alphabetical position in the list
 * to obtain a name score.
 *
 * For example, when the list is sorted into alphabetical order, COLIN, which is worth
 * 3 + 15 + 12 + 9 + 14 = 53,
 * is the 938th name in the list. So, COLIN would obtain a score of 938 * 53 = 49714.
 *
 * What is the total of all the name scores in the file?
 *
 * @author : Todd Cook
 * @since : 5/7/2011
 */

import collection.mutable.ListBuffer
import scala.io.Source
import java.io.File

object problem_22 {

  val offset = ('A'.charValue - 1)
  private var names = List[String]()

  def getNumericValue (word: String): Int = {
    word.toList.map(x => (x.charValue - offset)).foldLeft(0)(_ + _)
  }

  def initialize (fileLocation: String) {
    for (line <- Source.fromFile(fileLocation).getLines) {
      names = line.replaceAll("\"", "").split(",").toList.sortWith(_ < _)
    }
  }

  def score () = {
    var nameValues = new ListBuffer[Tuple2[Int, Int]]()
    var totalSum = 0L
    names.foreach(x => totalSum += getNumericValue(x) * (names.indexOf(x) + 1))
    println("total sum: " + totalSum)
    totalSum
  }

  def validate (word: String) {
    println(word.toList.map(x => (x.charValue - offset)).mkString(", "))
    println(word.toList.map(x => (x.charValue - offset)).foldLeft(0)(_ + _))
    println(names.indexOf(word) + 1)
    println(word + " = " + getNumericValue(word) * (names.indexOf(word) + 1))
  }

  def answer (fileLocation: String) = {
    initialize(fileLocation)
    score
  }

  def main (args: Array[String]) = {
    if (args.length > 0) {
      println(answer(args(0)))
    }
    else {
      var location = new File(".").getCanonicalPath + File.separator + "src" + File.separator +
        "main" + File.separator + "resources" + File.separator + "names.txt"
      println(location)
      println(answer(location))
      validate("COLIN")
    }
  }
}