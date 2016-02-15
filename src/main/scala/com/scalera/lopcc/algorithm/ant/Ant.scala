package com.scalera.lopcc.algorithm.ant

import scala.util.Random
import scala.math.BigDecimal

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

/**
  * Ant of the ACO
  * @param alpha alpha parameter
  * @param beta beta parameter
  */
case class Ant(alpha: Double = 0.5, beta: Double = 1.2) {

  /**
    * Return the solution reached by an ant
    * @param initial initial node
    * @param graph Graph
    * @param pheromoneGraph graph with the pheromones
    * @return the solution found
    */
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

  /**
    * The ant decides the next node to go
    * @param current current node
    * @param graph Graph
    * @param pheromoneGraph graph with the pheromones
    * @return the next node
    */
  private def nextStep(
    current: Int, 
    graph: Graph, 
    pheromoneGraph: Graph
  ): Int =
    choiceWay(travelProbabilities(current, graph, pheromoneGraph))

  /**
    * Generate the probabilities to go to a specific node
    * @param current current node
    * @param graph Graph
    * @param pheromoneGraph graph with the pheromones
    * @return the probabilities to go to the feasible nodes
    */
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
        val benefit = graph.getPartialCost(current, node)
        Math.pow(pheromone, alpha) / Math.pow(benefit, beta)
      }.sum

      graph.nodes.map { node =>
        val pheromone = pheromoneGraph.getEdgeCost(current, node)
        val benefit = graph.getPartialCost(current, node)
        val a = Math.pow(pheromone, alpha)
        val b = 1.0 / Math.pow(benefit, beta)
        (node, (a * b) / normalizationFactor)
      }
    }
  }

  /**
    * Choice a specific node to go
    * @param options the probabilities of each node
    * @return the node chosen
    */
  private def choiceWay(options: List[(Int, Double)]): Int = {
    val (items, weights) = options.unzip
    val intervals = items.zip(weights.scanLeft(0.0)(_ + _).tail)
    val maxProbability: Double = Random.nextDouble() * weights.sum
    intervals.find{ case(_, weight) => weight >= maxProbability}.get._1
  }

}
