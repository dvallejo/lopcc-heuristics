package com.scalera.lopcc.algorithm.genetic

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.algorithm.Algorithm

/**
  * Genetic Algorithm
  */
object GeneticAlgorithm extends Algorithm {

  val populationSize = 1000
  val iterations = 100

  /**
    * Execute a genetic algorithm
    * @param graph Graph
    * @return the best solution obtained
    */
  def execute(graph: Graph): Solution = {
    
    val population = Population.initial(populationSize, graph.maxNumNodes, graph)
    
    val finalPopulation = 
      (1 to iterations).foldLeft(population) {
        case (population, _) => population.evolve
      }

    val sol = Solution.empty(graph.maxNumNodes)

    finalPopulation.best.genes.foldRight((graph, sol)) {
      case (node, (acumGraph, acumSol)) =>
        insertInSolution(node, acumSol, acumGraph)
    }._2

  }
}
