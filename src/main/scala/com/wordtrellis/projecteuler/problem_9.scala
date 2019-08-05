package com.wordtrellis.projecteuler

/**
  * Problem 9
  * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
  * a^(2) + b^(2) = c^(2)
  *
  * For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).
  *
  * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  * Find the product abc.
  *
  * @author : Todd Cook
  *
  */
object problem_9 {

    def main(args: Array[String]) :Unit = {
    println(answer())
  }

  def answer(): Int = {
    val t3 = findPythagoreanTriplet(1000)
    println(t3)
    t3._1 * t3._2 * t3._3
  }

  /**
    * Although Euclid's formula will allow us to generate Pythagorean Triplets,
    * we should be able to narrow the range of parameters which will produce the desired result, e.g.
    *  scala> euclidsFormula( 100,1)
    *  res40: (Int, Int, Int) = (200,9999,10001)
    *
    *  scala> math.sqrt(10001)
    *  res41: Double = 100.00499987500625
    *
    *  scala> euclidsFormula( 101,1)
    *  res42: (Int, Int, Int) = (202,10200,10202)
    *
    *  scala> math.sqrt(10202)
    *  res43: Double = 101.00495037373169
    *
    *
    */
  def findPythagoreanTriplet(result: Int): (Int, Int, Int) = {
    val ceiling = math.sqrt(result).intValue
    (1 to ceiling).toList.foreach(m => {
      (1 to ceiling).toList.foreach(n => {
        if (m > n) {
          val pythagoreanTriplet = euclidsFormula(m, n)
          if (isPythagoreanTriplet(pythagoreanTriplet) && isTarget(pythagoreanTriplet, result)) {
            println("result found using m, n: %d, %d".format(m, n))
            return pythagoreanTriplet
          }
        }
      })
    })
    (0, 0, 0)
  }

  def isPythagoreanTriplet(x: (Int, Int, Int)): Boolean = {
    (x._1 * x._1) + (x._2 * x._2) == (x._3 * x._3)
  }

  def isTarget(x: (Int, Int, Int), result: Int): Boolean = {
    x._1 + x._2 + x._3 == result
  }

  /**
    * Euclid's formula for Pythagorean Triple, see
    * http://en.wikipedia.org/wiki/Pythagorean_triplet
    */
  def euclidsFormula(m: Int, n: Int): (Int, Int, Int) = {
    require(m > n)
    require(n > 0)
    val a = 2 * m * n
    val b = (m * m) - (n * n)
    val c = (m * m) + (n * n)
    (a, b, c)
  }
}
