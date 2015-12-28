package com.scalera.lopcc.algorithm.ant

import scala.util.Random
import scala.math.BigDecimal

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

case class Ant(alpha: Double = 0.5, beta: Double = 1.2) {

  def goAnt(initial: Int, graph: Graph, pheromoneGraph: Graph): Solution = {

    val newGraph = graph.removeNode(initial)
    val sol = Solution.empty(graph.maxNumNodes).insertNode(initial, newGraph)
    
    (0 to graph.maxNumNodes - 2).foldRight((newGraph, sol)) {
      case(_, (g, s)) =>
        val nextNode = nextStep(s.nodes.last, g, pheromoneGraph)
        val nextGraph = g.removeNode(nextNode)
        (nextGraph, s.insertNode(nextNode, nextGraph))
    }._2
  }

  private def nextStep(
    current: Int, 
    graph: Graph, 
    pheromoneGraph: Graph
  ): Int =
    choiceWay(travelProbabilities(current, graph, pheromoneGraph))

  private def travelProbabilities(
    current: Int,
    graph: Graph,
    pheromoneGraph: Graph
  ): List[(Int, Double)] = {
    
    if(graph.nodes.forall(node => pheromoneGraph.getEdgeCost(current, node) == 0))
      graph.nodes.map(node => (node, 1.0 / graph.nodes.size))
    else {
      val normalizationFactor = graph.nodes.map { node =>
        val pheromone = pheromoneGraph.getEdgeCost(current, node)
        val benefit = graph.getMinEdgeCost(current, node)
        Math.pow(pheromone, alpha) / Math.pow(benefit, beta)
      }.sum

      graph.nodes.toList.map { node =>
        val pheromone = pheromoneGraph.getEdgeCost(current, node)
        val benefit = graph.getMinEdgeCost(current, node)
        val a = Math.pow(pheromone, alpha)
        val b = 1.0 / Math.pow(benefit, beta)
        (node, ((a * b) / normalizationFactor))
      }
    }
  }

  private def choiceWay(options: List[(Int, Double)]): Int = {
    val (items, weights) = options.unzip
    val intervals = items.zip(weights.scanLeft(0.0)(_ + _).tail)
    val maxProbability: Double = Random.nextDouble() * weights.sum
    intervals.find{ case(_, weight) => weight >= maxProbability}.get._1
  }

}
