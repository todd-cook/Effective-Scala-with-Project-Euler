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

  */
import java.io.File

import scala.collection.mutable.ListBuffer
import scala.io.Source

object problem_22 {

  val offset: Int   = 'A'.charValue - 1
  private var names = List[String]()

  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      println(answer(args(0)))
    } else {
      val location = new File(".").getCanonicalPath + File.separator + "src" + File.separator +
        "main" + File.separator + "resources" + File.separator + "names.txt"
      println(location)
      println(answer(location))
      validate("COLIN")
    }
  }

  def validate(word: String) :Unit= {
    println(word.toList.map(x => x.charValue - offset).mkString(", "))
    println(word.toList.map(x => x.charValue - offset).sum)
    println(names.indexOf(word) + 1)
    println(word + " = " + getNumericValue(word) * (names.indexOf(word) + 1))
  }

  def getNumericValue(word: String): Int = {
    word.toList.map(x => x.charValue - offset).sum
  }

  def answer(fileLocation: String): Long = {
    initialize(fileLocation)
    score()
  }

  def initialize(fileLocation: String) :Unit = {
    for (line <- Source.fromFile(fileLocation).getLines) {
      names = line.replaceAll("\"", "").split(",").toList.sortWith(_ < _)
    }
  }

  def score(): Long = {
    var nameValues = new ListBuffer[(Int, Int)]()
    var totalSum   = 0L
    names.foreach(x => totalSum += getNumericValue(x) * (names.indexOf(x) + 1))
    println("total sum: " + totalSum)
    totalSum
  }
}