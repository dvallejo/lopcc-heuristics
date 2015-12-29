package com.scalera.lopcc.algorithm

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

import scala.util.Random

object FeasibleAlgorithm extends Algorithm() {

  def execute(graph: Graph): Solution = {
    val sol = Solution.empty(graph.maxNumNodes)
    generateSolutionRec(graph, sol)
  }

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
