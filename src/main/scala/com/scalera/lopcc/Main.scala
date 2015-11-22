package com.scalera.lopcc

import parser.Parser
import heuristic.FeasibleAlgorithm

object Main extends App with Parser {

  val filePath = "/RMarti/tipo00/mrho1.txt"

  val graph = processFile

  val feasibleSol =
    FeasibleAlgorithm.generateSolution(graph)

  println(s"\n$feasibleSol\n")
}
