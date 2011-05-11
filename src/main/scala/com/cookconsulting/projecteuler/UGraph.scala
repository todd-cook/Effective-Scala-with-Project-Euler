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

package com.cookconsulting.projecteuler

/**
 * Undirected Graph
 * used for initial solution probing for Project Euler problem 15
 *
 * @author Todd Cook
 * @since 5/10/11 10:41 PM
 */

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.List

class UGraph {
  val vertices = new HashSet[Tuple2[Int, Int]]()

  def addVertex (vertex: Tuple2[Int, Int]) = vertices.add(vertex)

  def addAllVertices (newVertices: List[Tuple2[Int, Int]]) = {
    newVertices.foreach(t => vertices.add(t))
  }

  def adjacents (vertex: Tuple2[Int, Int]): List[Tuple2[Int, Int]] = {
    val buf = new ListBuffer[Tuple2[Int, Int]]()
    vertices.toList.foreach(t => {
      if ((t._1 == vertex._1) && (t._2 == vertex._2 + 1)) {
        buf.append((t))
      }
      else if ((t._1 == vertex._1 + 1) && (t._2 == vertex._2)) {
        buf.append((t))
      }
    })

    buf.toList
  }

  override def toString = vertices.mkString

  def findNextVertices (pod: List[List[Tuple2[Int, Int]]]): List[List[Tuple2[Int, Int]]] = {
    val results = new ListBuffer[List[Tuple2[Int, Int]]]()

    pod.foreach(seed => {
      var nextVertices = adjacents(seed.last)
      nextVertices.foreach(newVertex => {
        val candidateBuilder = new ListBuffer[Tuple2[Int, Int]]()
        seed.foreach(y => candidateBuilder.append(y))
        candidateBuilder.append(newVertex)
        results.append(candidateBuilder.toList)
      })
    })
    results.toList
  }
}