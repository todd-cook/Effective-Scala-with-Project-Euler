

package com.wordtrellis.projecteuler

/**
 * Problem 31
 *
 * In England the currency is made up of pound, &pound;, and pence, p, and
 * there are eight coins in general circulation:
 *
 * 1p, 2p, 5p, 10p, 20p, 50p, 1 pound (100p) and 2 pound (200p).
 *
 * It is possible to make 2 pound in the following way:
 *
 * 1 pound 1 + 1 50p + 2 20p + 1 5p + 1 2p + 3 1p
 *
 * How many different ways can 2 pound be made using any number of coins?
 *
 * @author : Todd Cook
 *
 */

object problem_31 {
  def main (args: Array[String]) {
    val coinConstraints = new CoinConstraints(List(1, 2, 5, 10, 20, 50, 100, 200), 200)
    val coinCollection = new CoinCollection(coinConstraints)
    coinCollection.generateAllCombinations()
    println("total possible combinations: " + coinCollection.possibleCombinations.toList.length)
  }
}

/**
 *
 * Commentary:
 * Generating combinations is very expensive
 * to solve problem 31 in worst case:
 * 34825 * 8 Bytes (64 bit ints) * 200 == circa 44 megs
 * Although reusable, this solution need excessive memory to generate all the combinations
 * -Xmx98M
 *
 * Examine the utility classes: CoinCollection, CoinConstraints, CoinCombination
 *
 *
 * Results:
 *
 * possible combinations: 1 for coins up to: 1
 * possible combinations: 101 for coins up to: 2
 * possible combinations: 1091 for coins up to: 5
 * possible combinations: 8141 for coins up to: 10
 * possible combinations: 25971 for coins up to: 20
 * possible combinations: 32225 for coins up to: 50
 * possible combinations: 34824 for coins up to: 100
 * possible combinations: 34825 for coins up to: 200
 * total possible combinations: 34825
 *
 */
