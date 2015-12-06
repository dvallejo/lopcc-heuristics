package com.scalera.lopcc.util

import scala.util.Random

case class Graph(
  maxNumNodes: Int,
  matrix: List[List[Double]],
  nodes: List[Int]
) {

  def isEmpty: Boolean = nodes.isEmpty

  def setEdge(i: Int, j: Int, cost: Double): Graph =
    this.copy(
      matrix = matrix.updated(i, matrix(i).updated(j, cost))
    )

  def getNodeCost(i: Int): Double = matrix(i)(i)

  def getEdgeCost(i: Int, j: Int): Double =
    if (i == j) 0 else matrix(i)(j)

  def existsNode(i: Int): Boolean = nodes.contains(i)

  def insertNode(node: Int): Graph =
    this.copy(
      nodes = node :: nodes
    )

  def removeNode(node: Int): Graph =
    this.copy(
      nodes = nodes.filter(_ != node)
    )

  def randomNode: Int = Random.nextInt(maxNumNodes)

}

object Graph {

  def empty(size: Int): Graph =
    Graph(
      maxNumNodes = size,
      matrix = List.fill(size)(List.fill(size)(0)),
      nodes = (0 to size-1).toList
    )

  def dummy: Graph =
    Graph(
      maxNumNodes = 3,
      matrix = List(List(1, 10, 2), List(5, 7, 16), List(3, 5, 8)),
      nodes = (0 to 2).toList
    )
}
