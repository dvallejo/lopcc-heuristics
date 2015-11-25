package com.scalera.lopcc

import parser.Parser
import heuristic.FeasibleAlgorithm
import heuristic.BacktrackingAlgorithm

object Main extends App with Parser {

  val filePath = "/RMarti/tipo00/mrho1.txt"

  val graph = com.scalera.lopcc.util.Graph.dummy
  //val graph = processFile

  val feasibleSol =
    FeasibleAlgorithm.execute(graph)

  println(s"\n$feasibleSol\n")

  val bestSol =
    BacktrackingAlgorithm.execute(graph)

  println(s"\n$bestSol\n")
}
