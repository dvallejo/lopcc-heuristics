package com.scalera.lopcc.algorithm.ant

import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.util.Graph

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
  evaporationRate: Double = 0.5,
  antsPerIteration: Int = 20
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
      iteration: Int
    ): Solution =
      if(iteration == iterations) {
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

        //The best one deposites twice
        newPheromoneGraph = depositPheromones(newBestSol, newPheromoneGraph)

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

  /**
    * Return the solution reached by an ant
    * @param graph Graph
    * @param pheromoneGraph graph with the pheromones
    * @return the solution found
    */
  private def goAnt(graph: Graph, pheromoneGraph: Graph): Solution =
    ant.goAnt(graph.randomNode, graph, pheromoneGraph)

  /**
    * Generate a new solution for each ant
    * @param graph Graph
    * @param pheromoneGraph graph with the pheromones
    * @return the solution found
    */
  private def goAntColony(graph: Graph, pheromoneGraph: Graph): List[Solution] =
    (1 to antsPerIteration).toList.map(_ => goAnt(graph, pheromoneGraph))

  /**
    * Generate an initial graph of pheromones
    * @param graph Graph
    * @return the graph generated
    */
  private def initializePheromones(graph: Graph): Graph =
    Graph.initial(graph.maxNumNodes)

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
    pheromoneGraph: Graph): Graph =
    (0 to solution.maxNodes-2).foldRight(pheromoneGraph) { 
      case (level, g) =>
        val pheromone = g.getEdgeCost(solution.nodes(level), solution.nodes(level + 1))
        val addition = pheromoneConstant / solution.getCost
        g.setEdge(solution.nodes(level), solution.nodes(level + 1), pheromone + addition)
    }

}
