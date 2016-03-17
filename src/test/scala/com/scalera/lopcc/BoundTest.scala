package com.scalera.lopcc

import com.scalera.lopcc.parser.Parser
import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem._
import com.scalera.lopcc.algorithm.genetic.GeneticAlgorithm
import com.scalera.lopcc.algorithm.FeasibleAlgorithm

object BoundTest extends App with Bounds with Parser {

  var graph = processFile("/RMarti/RND/t1d35.1")

  val bestSolution = GeneticAlgorithm.execute(graph)
  println(s"Best solution: ${bestSolution.getCost} with bound: ${getLB1(graph.copy(nodes = List.empty[Int]), bestSolution)}")

  var badSolution = FeasibleAlgorithm.execute(graph)
  graph = graph.copy(nodes = List.empty[Int])
  println(s"Bad solution: ${badSolution.getCost} with bound: ${getLB1(graph, badSolution)}")

  (1 to badSolution.maxNodes).foreach { case _ =>

    val (newSol, node) = badSolution.removeLast
    val newGraph = graph.insertNode(node)

    val bound = getLB1(newGraph, newSol)
    println(s"Bound of the badSolution: ${newSol.nodes}: $bound  -----------------> ${bound > bestSolution.getCost}")
    
    badSolution = newSol
    graph = newGraph
  }

}