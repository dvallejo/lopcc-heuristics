package com.scalera.lopcc.algorithm

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

/**
  * Backtracking algorithm
  */
object BacktrackingAlgorithm extends Algorithm() {

  /**
    * Execute a backtracking algorithm
    * @param graph Graph
    * @return the best solution
    */
  def execute(graph: Graph): Solution = {
    val sol = Solution.empty(graph.maxNumNodes)
    backtracking(graph, sol, sol)
  }

  /**
    * Recursive function that implements a backtracking algorithm
    * @param graph Graph
    * @param sol Solution
    * @param partialSol partial Solution
    * @return the best solution
    */
  def backtracking(graph: Graph, sol: Solution, partialSol: Solution): Solution =
    if(partialSol.isComplete)
      if(partialSol.isBetter(sol)) partialSol else sol
    else
      graph.nodes.filter(isFeasible(_, graph)).foldLeft(sol) {
        case (acumSol, node) =>
          val (newGraph, newSol) = insertInSolution(node, partialSol, graph)
          val finalSol = backtracking(newGraph, acumSol, newSol)
          if(finalSol.isBetter(acumSol)) finalSol else acumSol
      }
}
