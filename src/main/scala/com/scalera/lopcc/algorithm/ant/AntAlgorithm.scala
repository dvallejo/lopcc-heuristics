package com.scalera.lopcc.algorithm.ant

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.algorithm.Algorithm

object AntAlgorithm extends Algorithm {

  val alpha = 0.5
  val beta = 1.2

  val pheromoneConstant = 1000.0
  val evaporationRate = 0.2
  val antsPerIteration = 10
  val iterations = 1000

  val colony =
    AntColony(
      ant = Ant(alpha, beta),
      pheromoneConstant = pheromoneConstant,
      evaporationRate = evaporationRate,
      antsPerIteration = antsPerIteration
    )

  def execute(graph: Graph): Solution =
    colony.solve(graph = graph, initialSolution = getRandomSolution(graph), iterations = iterations)
}
