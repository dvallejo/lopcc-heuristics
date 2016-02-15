package com.scalera.lopcc.problem

import com.scalera.lopcc.util.Graph

/**
  * Solution of the problem
  * @param alphas alpha parameters
  * @param nodes list with the nodes
  * @param maxNodes size of the solution
  * @param totalCost cost of the solution
  */
case class Solution(
  alphas: List[Double],
  nodes: List[Int],
  maxNodes: Int,
  totalCost: Double = Double.MaxValue
) {

  /**
    * Get an alpha parameter
    * @param i index of the alphas list
    * @return the alpha parameter
    */
  def getAlpha(i: Int): Double = alphas(i)

  /**
    * Return the total cost
    * @return the total cost
    */
  def getCost: Double = alphas.sum

  /**
    * Return the number of holes in the solution
    * @return the number of holes
    */
  def getHoles: Int = maxNodes - nodes.size

  /**
    * Return if the solution is complete or if is a partial solution
    * @return if a solution is complete
    */
  def isComplete: Boolean = maxNodes == nodes.size

  /**
    * Compare two solutions
    * @param sol Solution
    * @return if the current solution is better than the other solution
    */
  def isBetter(sol: Solution): Boolean = totalCost < sol.totalCost

  /**
    * Insert a node in the solution
    * @param node node to insert
    * @param graph Graph
    * @return the new solution
    */
  def insertNode(node: Int, graph: Graph): Solution = {
    val newSol = calculateAlpha(node, graph).copy(nodes = node :: nodes)
    newSol.copy(totalCost = newSol.getCost)
  }

  /**
    * Remove a specific node of the solution
    * @param node node to remove
    * @return the new solution
    */
  def removeNode(node: Int): Solution = {
    val newSol = this.copy(
                  alphas = resetAlpha(node)
                ).copy(
                  nodes = nodes.filter(_ != node)
                )
    newSol.copy(
      totalCost = newSol.getCost
    )
  }

  /**
    * Remove the last node included in the solution
    * @return the new Solution and the node removed
    */
  def removeLast: (Solution, Int) = (removeNode(nodes.last), nodes.last)

  def calculateAlpha(node: Int, graph: Graph): Solution =
    this.copy(
      alphas = setAlpha(node, graph)
    )

  /**
    * Return a string with the result of the solution
    * @return the result of the solution as a string
    */
  def prettyPrint = s"""$totalCost with this order: [${nodes.mkString(",")}]"""

  /**
    * Update an alpha parameter
    * @param node node associated to the alpha parameter
    * @param graph Graph
    * @return the new list with the alpha parameter updated
    */
  private def setAlpha(node: Int, graph: Graph): List[Double] = {

    val nodeCost = graph.getNodeCost(node)

    val cost =
      nodes.foldLeft(nodeCost){
        case (alpha, i) => alpha + alphas(i) * graph.getEdgeCost(node, i)
      }

    alphas.updated(node, cost)
  }

  /**
    * Reset an alpha parameter
    * @param node node associated to the alpha parameter
    * @return the new list with the alpha reset
    */
  private def resetAlpha(node: Int): List[Double] = alphas.updated(node, 0.0)
}

object Solution {

  /**
    * Obtain an empty solution
    * @param size size of the solution
    * @return the empty solution
    */
  def empty(size: Int): Solution =
    Solution(
      alphas = List.fill(size)(0.0),
      nodes = List.empty[Int],
      maxNodes = size
    )

  /**
    * Obtain the cost of a list of nodes
    * @param graph Graph
    * @param nodes list of nodes
    * @return the cost of the list
    */
  def getCost(graph: Graph, nodes: List[Int]): Double =
    nodes.foldRight((graph, Solution.empty(nodes.size))) {
      case (node, (g, sol)) =>
        val newGraph: Graph = graph.removeNode(node)
        (newGraph, sol.insertNode(node, newGraph))
    }._2.getCost

}
