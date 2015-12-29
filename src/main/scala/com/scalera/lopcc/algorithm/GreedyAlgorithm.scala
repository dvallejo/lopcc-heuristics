package com.scalera.lopcc.algorithm

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

/**
  * Greedy Algorithm
  */
object GreedyAlgorithm extends Algorithm() {

  /**
    * Return a solution calculated by a greedy algorithm
    * @param graph Graph
    * @return
    */
  def execute(graph: Graph): Solution = {
    val sol = Solution.empty(graph.maxNumNodes)

    graph.nodes.foldLeft((graph, sol)) {
      case ((acumGraph, acumSol), _) =>
        val node = acumGraph.getNodeMinEdgeCost
        insertInSolution(node, acumSol, acumGraph)
    }._2
  }
    
}
