package com.scalera.lopcc.algorithm.genetic

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

case class Population(chromosomes: List[Chromosome], graph: Graph) {

  def populationSize = chromosomes.size
  def numGenes = chromosomes.head.genes.size

  def evolve: Population = {

    val partition = populationSize / 3

    val newGeneration: Population =
      Population.initial(populationSize, numGenes, graph)

    val bestNewGeneration: List[Chromosome] =
      getNBest(newGeneration.chromosomes, partition)

    val bestCurrentGeneration: List[Chromosome] =
      getNBest(chromosomes, partition)

    val crossGenerations: List[Chromosome] =
      bestCurrentGeneration.zip(bestNewGeneration).map {
        case (c1, c2) =>
          if(Solution.getCost(graph, c1.genes) < Solution.getCost(graph, c2.genes))
            Chromosome.cross(c1, c2)
          else
            Chromosome.cross(c2, c1)
      }

    val bestCrossGenerations: List[Chromosome] =
      getNBest(crossGenerations, populationSize - partition * 2)

    Population(
      chromosomes = bestCurrentGeneration ::: bestNewGeneration ::: bestCrossGenerations,
      graph = graph
    )
  }

  def sortByCost(chromosomes: List[Chromosome]): List[Chromosome] = 
    chromosomes.sortBy(ch => Solution.getCost(graph, ch.genes))

  def getNBest(chromosomes: List[Chromosome], n: Int): List[Chromosome] =
    sortByCost(chromosomes).takeRight(n)

  def best: Chromosome =
    sortByCost(chromosomes).last

}

object Population {

  def initial(populationSize: Int, numGenes: Int, graph: Graph): Population =
    Population(
      (1 to populationSize).map(_ => Chromosome.generateRandom(numGenes)).toList,
      graph
    )

}