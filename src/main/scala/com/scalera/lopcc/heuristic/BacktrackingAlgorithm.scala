package com.scalera.lopcc.heuristic

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

object BacktrackingAlgorithm extends Algorithm{

  def execute(graph: Graph): Solution = {
    val sol = Solution.empty(graph.maxNumNodes)
    backtracking(graph, sol, sol)
  }

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
