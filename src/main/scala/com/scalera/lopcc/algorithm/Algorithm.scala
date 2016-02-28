package com.scalera.lopcc.algorithm

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.algorithm.genetic.GeneticAlgorithm
import com.scalera.lopcc.algorithm.ant.AntAlgorithm

/**
  * Common functionality for any algorithms of this problem
  * @param initBoundSelection if it is necessary calculate an
  *                           initial bound, this param indicates the way to calculate it
  */
abstract class Algorithm(initBoundSelection: String = "random") {

  /**
    * Execution of an algorithm
    * @param graph Graph
    * @return
    */
  def execute(graph: Graph): Solution

  /**
    * Return if a node is feasible to insert it in the solution
    * @param node node to insert
    * @param graph Graph
    * @return if a node is feasible
    */
  def isFeasible(node: Int, graph: Graph): Boolean =
    graph.existsNode(node)

  /**
    * Insert a node in the solution and also it is removed of the graph
    * @param node node to insert in the solution
    * @param sol Solution
    * @param graph Graph
    * @return the new graph and the new solution
    */
  def insertInSolution(
    node: Int,
    sol: Solution,
    graph: Graph
  ): (Graph, Solution) = {

    val newGraph: Graph = graph.removeNode(node)
    (newGraph, sol.insertNode(node, newGraph))
  }

  /**
    * Remove the last node inserted in the solution
    * @param sol Solution
    * @param graph Graph
    * @return the new graph and the new solution
    */
  def removeLastFromSolution(sol: Solution, graph: Graph): (Graph, Solution) = {
    val (newSol, node) = sol.removeLast
    (graph.insertNode(node), newSol)
  }

  /**
    * Calculate an initial bound
    * @param graph Graph
    * @return the cost of the initial bound calculated
    */
  def getInitBound(graph: Graph): Solution =
    initBoundSelection match {
      case "random" => getRandomSolution(graph)
      case "randomx100" => getRandomSolutionx100(graph)
      case "ant" => getSolutionFromAnts(graph)
      case "genetic" => getSolutionFromGA(graph)
      case "greedy" => getGreedySolution(graph)
      case _ => getRandomSolution(graph)
    }

  /**
    * Generate a solution with a greedy algorithm
    * @param graph Graph
    * @return the solution calculated
    */
  def getGreedySolution(graph: Graph): Solution =
    GreedyAlgorithm.execute(graph)

  /**
    * Generate a solution with a random algorithm
    * @param graph Graph
    * @return the solution calculated
    */
  def getRandomSolution(graph: Graph): Solution =
    FeasibleAlgorithm.execute(graph)

  /**
    * Generate a solution with the best solution obtained running the algorithm 100 times
    * @param graph Graph
    * @return the solution calculated
    */
  def getRandomSolutionx100(graph: Graph): Solution =
    (1 to 100).map( _ =>
      FeasibleAlgorithm.execute(graph)
    ).sortBy(_.totalCost).head

  /**
    * Generate a solution with a genetic algorithm
    * @param graph Graph
    * @return the solution calculated
    */
  def getSolutionFromGA(graph: Graph): Solution =
    GeneticAlgorithm.execute(graph)

  /**
    * Generate a solution with a ACO algorithm
    * @param graph Graph
    * @return the solution calculated
    */
  def getSolutionFromAnts(graph: Graph): Solution =
    AntAlgorithm.execute(graph)

  /**
    * Return the cost of a random bound
    * @param graph Graph
    * @return the cost obtained
    */
  def getRandomBound(graph: Graph): Double =
    getRandomSolution(graph).totalCost

  def calculatePrunedNodes(solution: Solution): Int = {

    var prunedNodes = 1
    var aux = solution.getHoles
    var nodesIteration = 1

    for (hole <- (0 to solution.getHoles - 1)) {
      nodesIteration = nodesIteration * aux
      prunedNodes = prunedNodes + nodesIteration
      aux = aux - 1
    }

    prunedNodes
  }

}
