package com.scalera.lopcc.algorithm

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

import scala.util.Random

/**
  * Feasible Algorithm
  */
object FeasibleAlgorithm extends Algorithm() {

  /**
    * Return a random solution
    * @param graph Graph
    * @return a random solution
    */
  def execute(graph: Graph): Solution = {
    val sol = Solution.empty(graph.maxNumNodes)
    generateSolutionRec(graph, sol)
  }

  /**
    * Generate a random solution in a recursive way
    * @param graph Graph
    * @param sol Solution
    * @return a random solution
    */
  def generateSolutionRec(graph: Graph, sol: Solution): Solution =
    if(graph.isEmpty)
      sol
    else {
      val node = Random.nextInt(graph.maxNumNodes)
      
      if(isFeasible(node, graph)) {
        val (newGraph, newSolution) = insertInSolution(node, sol, graph)
        generateSolutionRec(newGraph, newSolution)
      } else
        generateSolutionRec(graph, sol)
    }
}
