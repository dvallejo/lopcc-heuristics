package com.scalera.lopcc.algorithm.genetic

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.algorithm.Algorithm
import v2.Population
// import v1.Population
/**
  * Genetic Algorithm
  */
object GeneticAlgorithm extends Algorithm {

  val populationSize = 100
  val iterations = 1000

  /**
    * Execute a genetic algorithm
    * @param graph Graph
    * @return the best solution obtained
    */
  def execute(graph: Graph): Solution = {
    
    var population = Population.initial(populationSize, graph.maxNumNodes, graph)

    var i: Int = 0
    var convergence: Boolean = false

    while (i < iterations && !convergence ) {
      i = i + 1
      population = population.evolve
      convergence = population.chromosomes.toSet.size == 1
    }

    if (convergence)
      println(s"Convergence reached in iteration $i")

    val sol = Solution.empty(graph.maxNumNodes)

    population.best.genes.foldRight((graph, sol)) {
      case (node, (acumGraph, acumSol)) =>
        insertInSolution(node, acumSol, acumGraph)
    }._2

  }
}
