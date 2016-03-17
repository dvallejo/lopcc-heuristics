package com.scalera.lopcc.algorithm.ant

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph

import scala.util.Random

/**
  * Colony of ants
  * @param ant ant mode
  * @param pheromoneConstant pheromone constant
  * @param evaporationRate evaporation rate constant
  * @param antsPerIteration ants per iteration constant
  */
case class AntColony(
  ant: Ant,
  pheromoneConstant: Double = 1000.0,
  evaporationRate: Double = 0.2,
  antsPerIteration: Int = 20,
  alpha: Double,
  beta: Double
) {

  /**
    * Execute the ACO algorithm
    * @param graph Graph
    * @param initialSolution initial solution
    * @param iterations number of iterations
    * @return the best solution found
    */
  def solve(graph: Graph, initialSolution: Solution, iterations: Int): Solution = {
    
    def recursiveSol(
      pheromoneGraph: Graph,
      bestSol: Solution,
      iteration: Int,
      convergence: Boolean
    ): Solution =
      if(iteration > iterations) {
        bestSol
      } else {
        val newSolutions = goAntColony(graph, pheromoneGraph, iteration)
        
        var newPheromoneGraph = pheromoneGraph
        var newBestSol = bestSol

        newSolutions.foreach { solution =>
          newPheromoneGraph = depositPheromones(solution, newPheromoneGraph)
          if (solution.isBetter(newBestSol)) {
            println("Nueva solución obtenida: " + solution.nodes)
            newBestSol = solution
          }
        }

        //The best one deposites twice
        newPheromoneGraph = depositPheromones(newBestSol, newPheromoneGraph)
        newPheromoneGraph = evaporatePheromones(newPheromoneGraph)

        recursiveSol(
          newPheromoneGraph,
          newBestSol,
          iteration + 1,
          convergence
        )
      }
    
    val initialPheromoneGraph = initializePheromones(graph)
    recursiveSol(initialPheromoneGraph, initialSolution, 0, false)
    
  }

  private def getInitialNode(graph: Graph, pheromoneGraph: Graph, iteration: Int): Int = {
    val normalizationFactor = graph.nodes.map { node =>
      val pheromone = pheromoneGraph.getNodeCost(node)
      val benefit = graph.getNodeCost(node)
      Math.pow(pheromone, alpha) / Math.pow(benefit, beta)
    }.sum

    val options = graph.nodes
      .map { node =>
        val pheromone = pheromoneGraph.getNodeCost(node)
        val benefit =  graph.getNodeCost(node)
        val a = Math.pow(pheromone, alpha)
        val b = 1.0 / Math.pow(benefit, beta)
        (node, (a * b) / normalizationFactor)
      }
      .sortBy(_._2)
      .reverse

    val (items, weights) = options.unzip
    val intervals = items.zip(weights.scanLeft(0.0)(_ + _).tail)
    val maxProbability: Double = Random.nextDouble() * weights.sum
    
    intervals.find { case(_, weight) => 
      weight > maxProbability
    }
    .headOption
    .map(_._1)
    .getOrElse(options.head._1)
  }

  /**
    * Return the solution reached by an ant
    * @param graph Graph
    * @param pheromoneGraph graph with the pheromones
    * @return the solution found
    */
  private def goAnt(graph: Graph, pheromoneGraph: Graph, iteration: Int): Solution =
    ant.goAnt(getInitialNode(graph, pheromoneGraph, iteration), graph, pheromoneGraph)

  /**
    * Generate a new solution for each ant
    * @param graph Graph
    * @param pheromoneGraph graph with the pheromones
    * @return the solution found
    */
  private def goAntColony(graph: Graph, pheromoneGraph: Graph, iteration: Int): List[Solution] =
    (1 to antsPerIteration).toList.map(_ => goAnt(graph, pheromoneGraph, iteration))

  /**
    * Generate an initial graph of pheromones
    * @param graph Graph
    * @return the graph generated
    */
  private def initializePheromones(graph: Graph): Graph =
    Graph.initial(graph.maxNumNodes, pheromoneConstant)

  /**
    * Evaporate pheromones according with the evaporation rate
    * @param pheromoneGraph graph with the pheromones
    * @return the new graph
    */
  private def evaporatePheromones(pheromoneGraph: Graph): Graph =
    pheromoneGraph.copy(
      matrix = pheromoneGraph.matrix.map { nodeCosts =>
        nodeCosts.map { pheromone =>
            (1.0 - evaporationRate) * pheromone
        }
      }
    )

  /**
    * Deposite pheromones in the nodes of a solution
    * @param solution Solution
    * @param pheromoneGraph graph with the pheromones
    * @return the new graph
    */
  private def depositPheromones(
    solution: Solution,
    pheromoneGraph: Graph): Graph = {
    val addition = (pheromoneConstant / solution.getCost)
    val pheromoneNode = pheromoneGraph.getNodeCost(solution.nodes.last)
    
    val newPheromoneGraph = (0 to solution.maxNodes-2).foldRight(pheromoneGraph) {
      case (level, g) =>
        val pheromone = g.getEdgeCost(solution.nodes(level + 1), solution.nodes(level))
        g.setEdge(solution.nodes(level + 1), solution.nodes(level), pheromone + addition)
    }

    newPheromoneGraph.setEdge(solution.nodes.last, solution.nodes.last, pheromoneNode + addition)
  }

}
