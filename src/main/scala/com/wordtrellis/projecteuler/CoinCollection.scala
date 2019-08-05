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

import scala.collection.mutable.{HashSet, ListBuffer}

/**
  * Coin Constraints class defines the problem & solution rules
  * Note: To model new combinations, one only need to change the first
  * two variables: possible coins and totalSum
  *
  * Used in solve Project Euler Problem 31
  * @author Todd Cook
  *
  */
class CoinConstraints(val possibleCoins: List[Int], val totalSum: Int) {
  val minValue = possibleCoins(0)
  // initial seed is a list of the smallest denomination
  val initialSeed: List[Int] = (1 to totalSum / minValue).toList.map(x => x * 0 + minValue)

  def maxOccurrencesFor(coin: Int): Int = totalSum / coin
}

/**
  * Coin Combination: data structure
  */
class CoinCombination(val coins: List[Int]) {

  val totalValue: Int = coins.sum

  override def hashCode: Int = coins.hashCode

  override def equals(other: Any): Boolean = other match {
    case that: CoinCombination => this.coins.hashCode == that.coins.hashCode
    case _                     => false
  }

  override def toString: String = coins.mkString(", ")
}

/**
  * Class Coin Collection contains all the combinations generated
  * and provides factory methods for increasing the combinations
  */
class CoinCollection(val coinConstraints: CoinConstraints) {

  val possibleCombinations = new HashSet[CoinCombination]()

  def add(coinCombination: CoinCombination): Boolean = possibleCombinations.add(coinCombination)

  def generateAllCombinations(): List[CoinCombination] = {
    coinConstraints.possibleCoins.foreach(coin =>
      expandCombinations(coin, coinConstraints.maxOccurrencesFor(coin)))
    possibleCombinations.toList
  }

  // initialize
  add(create(coinConstraints.initialSeed))

  def expandCombinations(newCoin: Int, maxOccurs: Int): Unit = {
    val newCombos = new HashSet[CoinCombination]()
    possibleCombinations.foreach(a =>
      (1 to maxOccurs).foreach(b => {
        newCombos.add(substitute(a.coins, newCoin, b))
      }))
    newCombos.foreach(c => possibleCombinations.add(c))
    println(
      "possible combinations: " + possibleCombinations.size +
        " for coins values up to: " + newCoin)
  }

  // improvements could be made here
  private def substitute(coins: List[Int], newCoin: Int, occurrences: Int): CoinCombination = {
    val coinBuffer = new ListBuffer[Int]()
    (1 to occurrences).foreach(x => coinBuffer.append(newCoin))
    var nextItem = 0
    var total    = coinBuffer.sum
    while (total < coinConstraints.totalSum) {
      coinBuffer.append(coins(nextItem))
      total += coins(nextItem)
      nextItem += 1
    }
    val newCombo = create(coinBuffer.toList)
    if (newCombo.totalValue == coinConstraints.totalSum) {
      return newCombo
    }
    // if the calculated value is more than the max value, return the initial seed
    create(coinConstraints.initialSeed)
  }

  def create(coins: List[Int]) = new CoinCombination(coins.sortWith(_ < _))
}
