package com.scalera.lopcc

import parser.Parser
import heuristic.FeasibleAlgorithm
import heuristic.BacktrackingAlgorithm
import heuristic.GreedyAlgorithm
import heuristic.BranchAndBoundAlgorithm

object Main extends App with Parser {

  val filePath = "/RMarti/tipo00/mrho1.txt"

  val graph = com.scalera.lopcc.util.Graph.dummy
  //val graph = processFile

  // val feasibleSol =
  //   FeasibleAlgorithm.execute(graph)

  // println(s"\nFeasible: $feasibleSol\n")

  // val bestSol =
  //   BacktrackingAlgorithm.execute(graph)

  // println(s"\nBacktracking: $bestSol\n")

  // val bestGreedySol =
  //   GreedyAlgorithm.execute(graph)

  // println(s"\nGreedy: $bestGreedySol\n")

  val bestSolBAndB =
    BranchAndBoundAlgorithm.execute(graph)

  println(s"\nBranch And Bound: $bestSolBAndB\n")

}
