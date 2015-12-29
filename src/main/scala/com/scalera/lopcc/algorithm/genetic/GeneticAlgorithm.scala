package com.scalera.lopcc.algorithm.genetic

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.algorithm.Algorithm

object GeneticAlgorithm extends Algorithm {

  val populationSize = 100
  val iterations = 1000

  def execute(graph: Graph): Solution = {
    
    val population = Population.initial(populationSize, graph.maxNumNodes, graph)
    
    val finalPopulation = 
      (1 to iterations).foldLeft(population) {
        case (population, _) => population.evolve
      }

    val sol = Solution.empty(graph.maxNumNodes)

    finalPopulation.best.genes.foldLeft((graph, sol)) {
      case ((acumGraph, acumSol), node) =>
        insertInSolution(node, acumSol, acumGraph)
    }._2

  }
}
