package com.scalera.lopcc.problem

import com.scalera.lopcc.util.Graph

case class Solution(
  alphas: List[Double],
  nodes: List[Int],
  maxNodes: Int,
  totalCost: Double = Double.MaxValue
) {

  def getAlpha(i: Int): Double = alphas(i)

  def getCost: Double = alphas.sum

  def getHoles: Int = maxNodes - nodes.size

  def isComplete: Boolean = maxNodes == nodes.size

  def isBetter(sol: Solution): Boolean = totalCost < sol.totalCost

  def insertNode(node: Int, graph: Graph): Solution = {
    val newSol = calculateAlpha(node, graph).copy(nodes = node :: nodes)
    newSol.copy(totalCost = newSol.getCost)
  }

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

  def removeLast: (Solution, Int) = (removeNode(nodes.last), nodes.last)

  def calculateAlpha(node: Int, graph: Graph): Solution =
    this.copy(
      alphas = setAlpha(node, graph)
    )

  def prettyPrint = s"""$totalCost with this order: [${nodes.mkString(",")}]"""

  private def setAlpha(node: Int, graph: Graph): List[Double] = {

    val nodeCost = graph.getNodeCost(node)

    val cost =
      nodes.foldLeft(nodeCost){
        case (alpha, i) => alpha + alphas(i) * graph.getEdgeCost(node, i)
      }

    alphas.updated(node, cost)
  }

  private def resetAlpha(node: Int): List[Double] = alphas.updated(node, 0.0)
}

object Solution {

  def empty(size: Int): Solution =
    Solution(
      alphas = List.fill(size)(0.0),
      nodes = List.empty[Int],
      maxNodes = size
    )

  def getCost(graph: Graph, nodes: List[Int]): Double =
    nodes.reverse.foldLeft((graph, Solution.empty(nodes.size))) {
      case ((graph, sol), node) => 
        val newGraph: Graph = graph.removeNode(node)
        (newGraph, sol.insertNode(node, newGraph))
    }._2.getCost

}
