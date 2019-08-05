package com.wordtrellis.projecteuler

/**
  * Undirected Graph
  * used for initial solution probing for Project Euler problem 15
  *
  * @author Todd Cook
  *
  */
import scala.collection.mutable.{HashSet, ListBuffer}

class UndirectedGraph {
  val vertices = new HashSet[(Int, Int)]()

  def addVertex(vertex: (Int, Int)): Boolean = vertices.add(vertex)

  def addAllVertices(newVertices: List[(Int, Int)]): Unit = {
    newVertices.foreach(t => vertices.add(t))
  }

  override def toString: String = vertices.mkString

  def findNextVertices(pod: List[List[(Int, Int)]]): List[List[(Int, Int)]] = {
    val results = new ListBuffer[List[(Int, Int)]]()

    pod.foreach(seed => {
      val nextVertices = adjacents(seed.last)
      nextVertices.foreach(newVertex => {
        val candidateBuilder = new ListBuffer[(Int, Int)]()
        seed.foreach(y => candidateBuilder.append(y))
        candidateBuilder.append(newVertex)
        results.append(candidateBuilder.toList)
      })
    })
    results.toList
  }

  def adjacents(vertex: (Int, Int)): List[(Int, Int)] = {
    val buf = new ListBuffer[(Int, Int)]()
    vertices.toList.foreach(t => {
      if ((t._1 == vertex._1) && (t._2 == vertex._2 + 1)) {
        buf.append(t)
      } else if ((t._1 == vertex._1 + 1) && (t._2 == vertex._2)) {
        buf.append(t)
      }
    })

    buf.toList
  }
}
