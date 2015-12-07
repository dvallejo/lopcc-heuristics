package com.scalera.lopcc.algorithm.ant

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph

case class AntColony(
  ant: Ant,
  pheromoneConstant: Double = 1000.0,
  evaporationRate: Double = 0.5,
  antsPerIteration: Int = 20
) {

  def solve(graph: Graph, initialSolution: Solution, iterations: Int): Solution = {
    
    def recursiveSol(
      pheromoneGraph: PheromoneGraph,
      bestSol: Solution,
      iteration: Int
    ): Solution =
      if(iteration == iterations) {
        //println("Pheromone graph: " + pheromoneGraph.weights.map(k => (k._1, k._2.toList.sortBy(_._2).last)))
        bestSol
      }else {
        val newSolutions = goAntColony(graph, pheromoneGraph)
        
        var newPheromoneGraph = pheromoneGraph
        var newBestSol = bestSol

        newSolutions.foreach { solution =>
          newPheromoneGraph = depositPheromones(solution, newPheromoneGraph)

          if (solution.isBetter(newBestSol)) {
            newPheromoneGraph = depositPheromones(solution, newPheromoneGraph)
            newBestSol = solution
          }
        }

        newPheromoneGraph = evaporatePheromones(newPheromoneGraph)

        recursiveSol(
          newPheromoneGraph,
          newBestSol,
          iteration + 1
        )
      }
    
    val initialPheromoneGraph = initializePheromones(graph)
    recursiveSol(initialPheromoneGraph, initialSolution, 0)
    
  }

  private def goAnt(graph: Graph, pheromoneGraph: PheromoneGraph): Solution =
    ant.goAnt(graph.randomNode, graph, pheromoneGraph)

  private def goAntColony(graph: Graph, pheromoneGraph: PheromoneGraph): List[Solution] =
    (1 to antsPerIteration).toList.map(_ => goAnt(graph, pheromoneGraph))

  private def initializePheromones(graph: Graph): PheromoneGraph =
    PheromoneGraph.initial(graph.maxNumNodes)

  private def evaporatePheromones(pheromoneGraph: PheromoneGraph): PheromoneGraph =
    pheromoneGraph.copy(
      weights = pheromoneGraph.weights.map {
        case (level, weightLevel) => (level, weightLevel.map {
          case ((from, to), pheromone) =>
            ((from, to), (1.0 - evaporationRate) * pheromone)
        })
      }
    )

  private def depositPheromones(
    solution: Solution,
    pheromoneGraph: PheromoneGraph): PheromoneGraph =
    (0 to solution.maxNodes-2).foldRight(pheromoneGraph) { 
      case (level, g) =>
        val pheromone = g.getCost(level)(solution.nodes(level), solution.nodes(level + 1))
        val addition = pheromoneConstant / solution.getCost
        g.setCost(level, solution.nodes(level), solution.nodes(level + 1), pheromone + addition)
    }

}
