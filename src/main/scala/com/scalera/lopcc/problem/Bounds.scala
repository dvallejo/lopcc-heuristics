package com.scalera.lopcc.problem

import com.scalera.lopcc.util.Graph

trait Bounds {

  /**
    * Calculate bound LB0
    * @param graph Graph
    * @param sol Solution
    * @return the cost of a bound LB0
    */
  def getLB0(graph: Graph, sol: Solution): Double =
    getWeightsMissingNodes(graph) +
    getSumEdges(graph, sol) + getSumAlphas(sol)

  /**
    * Calculate bound LB1
    * @param graph Graph
    * @param sol Solution
    * @return the cost of a bound LB1
    */
  def getLB1(graph: Graph, sol: Solution): Double =
    getLB0(graph, sol) + getMinorOrderCost(graph, sol)

  /**
    * Return the sum of the nodes contained in the graph
    * @param graph Graph
    * @return the sum calculated
    */
  private def getWeightsMissingNodes(graph: Graph): Double =
    graph.nodes.map(graph.getNodeCost).sum

  /**
    * Return the sum of the edges between the nodes of the graph and the nodes in the solution
    * @param graph Graph
    * @param sol Solution
    * @return the sum calculated
    */
  private def getSumEdges(graph: Graph, sol: Solution): Double =
    (for {
      node1 <- graph.nodes
      node2 <- sol.nodes
    } yield graph.getEdgeCost(node1, node2) * sol.getAlpha(node2)).sum

  /**
    * Return the sum of the alpha parameters of a solution
    * @param sol Solution
    * @return the sum calculated
    */
  private def getSumAlphas(sol: Solution): Double = sol.getCost

  /**
    * Return the result the best try of several combinations with beta parameters
    * @param graph Graph
    * @param sol Solution
    * @return the best combination
    */
  private def getMinorOrderCost(graph: Graph, sol: Solution): Double = {
    val betas = (0 to graph.maxNumNodes - 1).map(getBeta(_, graph, sol))
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

  /**
    * Return the beta parameter calculated in order to obtain a LB1 bound
    * @param node Node of the graph
    * @param graph Graph
    * @param sol Solution
    * @return the beta parameter
    */
  private def getBeta(node: Int, graph: Graph, sol: Solution): Double =
    graph.getNodeCost(node) +
    sol.nodes.filter(_ != node).map { n =>
      graph.getEdgeCost(node, n) * sol.getAlpha(n)
    }.sum
}
