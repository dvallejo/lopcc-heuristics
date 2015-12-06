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

  def getInitSolution(graph: Graph): Solution =
    GreedyAlgorithm.execute(graph)

  def getInitBound(graph: Graph): Double =
    getInitSolution(graph).totalCost

  def getRandomSolution(graph: Graph): Solution =
    FeasibleAlgorithm.execute(graph)

  def getRandomBound(graph: Graph): Double =
    getRandomSolution(graph).totalCost

}
