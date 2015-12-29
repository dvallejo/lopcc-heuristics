package com.scalera.lopcc.algorithm

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.algorithm.genetic.GeneticAlgorithm
import com.scalera.lopcc.algorithm.ant.AntAlgorithm

abstract class Algorithm(initBoundSelection: String = "random") {

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
    initBoundSelection match {
      case "random" => getRandomSolution(graph).totalCost
      case "randomx100" => getRandomSolutionx100(graph).totalCost
      case "ant" => getSolutionFromAnts(graph).totalCost
      case "genetic" => getSolutionFromGA(graph).totalCost
      case "greedy" => getGreedySolution(graph).totalCost
      case _ => getRandomSolution(graph).totalCost
    }

  def getGreedySolution(graph: Graph): Solution =
    GreedyAlgorithm.execute(graph)

  def getRandomSolution(graph: Graph): Solution =
    FeasibleAlgorithm.execute(graph)

  def getRandomSolutionx100(graph: Graph): Solution =
    (1 to 100).map( _ =>
      FeasibleAlgorithm.execute(graph)
    ).sortBy(_.totalCost).head

  def getSolutionFromGA(graph: Graph): Solution =
    GeneticAlgorithm.execute(graph)

  def getSolutionFromAnts(graph: Graph): Solution =
    AntAlgorithm.execute(graph)

  def getRandomBound(graph: Graph): Double =
    getRandomSolution(graph).totalCost

}
