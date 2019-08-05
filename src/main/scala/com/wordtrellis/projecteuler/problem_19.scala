package com.wordtrellis.projecteuler

/**
  * Problem 19
  * You are given the following information, but you may prefer to do some research for yourself.
  *
  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
  *    April, June and November.
  *    All the rest have thirty-one,
  *    Saving February alone,
  *    Which has twenty-eight, rain or shine.
  *    And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is
  * divisible by 400.
  *
  * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to
  * 31 Dec 2000)?
  *
  * Solution:
  * Use the standard libraries
  *
  * @author : Todd Cook
  *
  */
import java.util.Calendar

object problem_19 {

  def main(args: Array[String]): Unit = {
    println(answer())
  }

  def answer(): Int = {
    val cal = new java.util.GregorianCalendar()
    val sdf = new java.text.SimpleDateFormat("yyyy-mm-DD")
    cal.setTime(sdf.parse("1901-01-01"))
    var sundaysTheFirst = 0
    while (cal.get(Calendar.YEAR) < 2001) {
      cal.add(Calendar.MONTH, 1)
      if (cal.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY) {
        // println("Sunday found: " + cal.getTime)
        sundaysTheFirst += 1
      }
    }
    println("Number of months starting on Sunday: " + sundaysTheFirst)
    sundaysTheFirst
  }
}
