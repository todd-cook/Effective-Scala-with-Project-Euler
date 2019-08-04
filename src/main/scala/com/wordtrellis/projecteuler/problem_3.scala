

package com.wordtrellis.projecteuler

/**
 *
 *  Problem 3
 *  The prime factors of 13195 are 5, 7, 13 and 29.
 *  What is the largest prime factor of the number 600851475143 ?
 *
 * @author : Todd Cook
 *
 */

object problem_3 {

  def answer: Int = {
    lazy val naturals: Stream[Int] = Stream.cons(1, naturals.map(_ + 1))
    var theNum = 600851475143L
    val result = naturals.drop(1).dropWhile(n => {
      while (theNum % n == 0) {
        println(n)
        theNum /= n
      }
      theNum > 1
    })
    result(0)
  }

  /**
   * So the Stream of prime factors stars out with 2 in order to give a starting place for
   * looking for factors. The while loop then iterates through Longs starting with the last
   * value in primes. Once a factor is found, that factor is divided out of limit and the
   * new factor (tmp) is returned by the anonymous function as the next Long in primes.
   * This continues until all the prime factors of limit are found, at which point limit is
   * 1 and the loop just keeps returning the highest factor that was found.
   * The next line forces the lazy evaluation for the first 10 factors of primes, and then prints
   * the last factor, which gives us the answer. 10 was just an arbitrary number large enough
   * to get the factor we were looking for. A more general solution would take from primes until
   * it got two sequential elements that were the same, and then print that one.
   *
   * from http://grokcode.com/75/learning-scala-with-project-euler/
   */
  def answer2(): Unit = {
    var limit = 600851475143L
    lazy val primes: Stream[Long] =
      Stream.cons(2,
                  primes.map(x => {
                    var tmp = x
                    while ((limit % tmp) != 0 && limit != 1) {
                      tmp += 1
                    }
                    limit = limit / tmp
                    tmp
                  }))

    print(primes.take(10).last)
  }

  def main (args: Array[String]): Unit = {
    println(answer)
    println(answer2())
  }
}