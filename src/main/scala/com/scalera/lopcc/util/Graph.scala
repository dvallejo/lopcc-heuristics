package com.scalera.lopcc.util

import scala.util.Random

/**
  * Graph used to generate a solution
  * @param maxNumNodes size of the graph
  * @param matrix matrix with the weights of the graph
  * @param nodes nodes of the graph
  */
case class Graph(
  maxNumNodes: Int,
  matrix: List[List[Double]],
  nodes: List[Int]
) {

  /**
    * Return if the graph is empty
    * @return if the graph is empty
    */
  def isEmpty: Boolean = nodes.isEmpty

  /**
    * Update the value of an edge
    * @param i the origin of the edge
    * @param j the destination of the edge
    * @param cost the new cost
    * @return the graph updated
    */
  def setEdge(i: Int, j: Int, cost: Double): Graph =
    this.copy(
      matrix = matrix.updated(i, matrix(i).updated(j, cost))
    )

  /**
    * Return the weight of a node
    * @param i node
    * @return the weight
    */
  def getNodeCost(i: Int): Double = matrix(i)(i)

  /**
    * Return the weight of a edge
    * @param i the origin of the edge
    * @param j the destination of the edge
    * @return the weight of the edge between i and j
    */
  def getEdgeCost(i: Int, j: Int): Double =
    if (i == j) 0 else matrix(i)(j)

  /**
    * Return if a node is contained in the graph
    * @param i node
    * @return if the node is in the graph
    */
  def existsNode(i: Int): Boolean = nodes.contains(i)

  /**
    * Insert a node in the graph
    * @param node node to insert
    * @return the new graph
    */
  def insertNode(node: Int): Graph =
    this.copy(
      nodes = node :: nodes
    )

  /**
    * Remove a node of the graph
    * @param node node to delete
    * @return the new graph
    */
  def removeNode(node: Int): Graph =
    this.copy(
      nodes = nodes.filter(_ != node)
    )

  /**
    * Return a random node of the graph
    * @return the node selected
    */
  def randomNode: Int = Random.nextInt(maxNumNodes)

  /**
    * Return the cost of a solution with only two nodes
    * @param current the last node of the solution
    * @param node the first node of the solution
    * @return the total cost
    */
  def getPartialCost(current: Int, node: Int): Double = {
    val alphaNode = getNodeCost(node)
    alphaNode + getNodeCost(current) + getEdgeCost(current, node)*alphaNode
  }

  /**
    * Return the node with the minor cost adding the weight and the sum of the edges
    * @return the node with the minor cost
    */
  def getNodeMinEdgeCost: Int =
    nodes.map { node =>
      val nodeCost = getNodeCost(node)
      val edgesCost =
        nodes.filter(_ != node).map { n =>
          getEdgeCost(node, n)
        }.sum
      
      val totalCost = nodeCost + edgesCost
      (node, totalCost)
    }.sortBy(_._2).head._1

}

object Graph {

  /**
    * Create a graph with all the weights set to zero
    * @param size size of the graph
    * @return the graph created
    */
  def empty(size: Int): Graph =
    Graph(
      maxNumNodes = size,
      matrix = List.fill(size)(List.fill(size)(0)),
      nodes = (0 to size-1).toList
    )

  /**
    * Create a dummy graph in order to use it as an example
    * @return the dummy graph
    */
  def dummy: Graph =
    Graph(
      maxNumNodes = 3,
      matrix = List(List(1, 10, 2), List(5, 7, 16), List(3, 5, 8)),
      nodes = (0 to 2).toList
    )

  /**
    * Create a graph with all the weights set to a value
    * @param size size of the graph
    * @param initialCost weight of all the nodes and edges
    * @return
    */
  def initial(size: Int, initialCost: Double = 1.0): Graph =
    Graph(
      maxNumNodes = size,
      matrix = List.fill(size)(List.fill(size)(initialCost)),
      nodes = (0 to size-1).toList
    )
}
