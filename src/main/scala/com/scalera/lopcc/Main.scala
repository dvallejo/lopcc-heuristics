package com.scalera.lopcc

import parser.Parser
import algorithm._
import problem.Solution
import util.Graph

object Main extends Parser {

  def main(args: Array[String]) {

    args match {
      case Array(algorithm, path) =>
        processRequest(algorithm, Some(path))
      
      case Array(algorithm) =>
        processRequest(algorithm, None)
      
      case _ => 
        println("usage: program algorithm [path]")
    }

  }

  private def processRequest(algorithm: String, path: Option[String]): Unit = {

    val graph = parseGraph(path)

    algorithm match {

      case "feasible" =>
        executeAlgorithm(FeasibleAlgorithm, graph)

      case "greedy" =>
        executeAlgorithm(GreedyAlgorithm, graph)

      case "backtracking" =>
        executeAlgorithm(BacktrackingAlgorithm, graph)

      case "branchAndBound" =>
        executeAlgorithm(BranchAndBoundAlgorithm, graph)

      case other =>
        println(s"Algorithm $other not supported")
    }
  }

  private def parseGraph(path: Option[String]): Graph = {
    path.fold {
      println("Graph dummy chosen")
      Graph.dummy
    } { pathFile =>
      println(s"Graph from file: $pathFile")
      processFile(pathFile)
    }
  }

  private def executeAlgorithm(algorithm: Algorithm, graph: Graph): Unit = {
    println(s"${algorithm.getClass.getSimpleName.init.toString} chosen")
    val sol: Solution = algorithm.execute(graph)        
    println(s"Solution: ${sol.prettyPrint}")
  }

}
