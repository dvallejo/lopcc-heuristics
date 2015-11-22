package com.scalera.lopcc.heuristic

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph

trait Algorithm {

  def isFeasible(node: Int, graph: Graph): Boolean =
    graph.existsNode(node)

  def insertInSolution(
    node: Int,
    sol: Solution,
    graph: Graph
  ): (Graph, Solution) = {

    val newGraph: Graph = graph.removeNode(node)
    (newGraph, sol.insertNode(node, newGraph))
  }
}
