package com.scalera.lopcc

import parser.Parser
import algorithm._
import algorithm.ant.AntAlgorithm
import algorithm.genetic.GeneticAlgorithm
import problem.Solution
import util.Graph

object Main extends Parser {

  def main(args: Array[String]) {

    args match {
      case Array(algorithm, boundType, path) =>
        processRequest(algorithm, Some(boundType), Some(path))

      case Array(algorithm, path) =>
        processRequest(algorithm, None, Some(path))

      case Array(algorithm) =>
        processRequest(algorithm, None, None)
      
      case _ => 
        println("usage: random|greedy|backtracking|branchAndBound|ant|genetic [boundType] [path]")
    }

  }

  private def processRequest(algorithm: String, boundType: Option[String], path: Option[String]): Unit = {

    val graph = parseGraph(path)

    algorithm match {

      case "random" =>
        executeAlgorithm(FeasibleAlgorithm, graph)

      case "greedy" =>
        executeAlgorithm(GreedyAlgorithm, graph)

      case "backtracking" =>
        executeAlgorithm(BacktrackingAlgorithm, graph)

      case "branchAndBound" =>
        val boundSelection = boundType.getOrElse("random")
        println(s"Initial bound calculated by $boundSelection algorithm")
        executeAlgorithm(BranchAndBoundAlgorithm(boundSelection), graph)

      case "ant" =>
        executeAlgorithm(AntAlgorithm, graph)

      case "genetic" =>
        executeAlgorithm(GeneticAlgorithm, graph)

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
