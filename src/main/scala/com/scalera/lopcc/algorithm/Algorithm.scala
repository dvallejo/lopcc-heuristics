package com.scalera.lopcc.algorithm

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph

trait Algorithm {

  def execute(graph: Graph): Solution

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

  def removeLastFromSolution(sol: Solution, graph: Graph): (Graph, Solution) = {
    val (newSol, node) = sol.removeLast
    (graph.insertNode(node), newSol)
  }

  def getInitBound(graph: Graph): Double =
    GreedyAlgorithm.execute(graph).totalCost

}
