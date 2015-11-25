package com.scalera.lopcc.problem

import com.scalera.lopcc.util.Graph

trait Bounds {

  def getLB0(graph: Graph, sol: Solution): Double =
    getWeightsMissingNodes(graph, sol) +
    getSumEdges(graph, sol) + getSumAlphas(sol)

  def getLB1(graph: Graph, sol: Solution): Double =
    getLB0(graph, sol) + getMinorOrderCost(graph, sol)

  private def getWeightsMissingNodes(graph: Graph, sol: Solution): Double =
    sol.nodes.map(graph.getNodeCost).sum

  private def getSumEdges(graph: Graph, sol: Solution): Double =
    (for {
      node1 <- sol.nodes
      node2 <- sol.nodes
      if node1 != node2
    } yield graph.getEdgeCost(node1, node2) * sol.getAlpha(node2)).sum

  private def getSumAlphas(sol: Solution): Double =
    sol.nodes.map(sol.getAlpha).sum

  private def getMinorOrderCost(graph: Graph, sol: Solution): Double = {
    val betas = graph.nodes.map(getBeta(_, graph, sol))
    (for {
      node1 <- graph.nodes
      node2 <- graph.nodes
      if node1 > node2
    } yield {
      val firstNode1 = betas(node2) * graph.getEdgeCost(node1, node2)
      val firstNode2 = betas(node1) * graph.getEdgeCost(node2, node1)
      if(firstNode1 < firstNode2) firstNode1 else firstNode2
    }).sum

  }

  private def getBeta(node: Int, graph: Graph, sol: Solution): Double =
    graph.getNodeCost(node) +
    sol.nodes.filter(_ != node).map { n =>
      graph.getEdgeCost(node, n) * sol.getAlpha(n)
    }.sum
}
