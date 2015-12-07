package com.scalera.lopcc.algorithm.ant

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.algorithm.Algorithm

object AntAlgorithm extends Algorithm {

  val alpha = 0.5
  val beta = 1.2

  val pheromoneConstant = 1
  val evaporationRate = 0.3
  val antsPerIteration = 40
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
