package com.scalera.lopcc.algorithm.genetic

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

case class Population(chromosomes: List[(Chromosome, Double)], graph: Graph) {

  def populationSize = chromosomes.size
  def numGenes = chromosomes.head._1.genes.size

  def evolve: Population = {

    val partition = populationSize / 3

    val newGeneration: Population =
      Population.initial(populationSize, numGenes, graph)

    val bestNewGeneration: List[(Chromosome, Double)] =
      newGeneration.chromosomes.take(partition)

    val bestCurrentGeneration: List[(Chromosome, Double)] =
      chromosomes.take(partition)

    val crossGenerations: List[(Chromosome, Double)] =
      bestCurrentGeneration.zip(bestNewGeneration).map {
        case ((ch1, cost1), (ch2, cost2)) =>
          if(cost1 < cost2) {
            val newChromosome = Chromosome.cross(ch1, ch2)
            (newChromosome, Solution.getCost(graph, newChromosome.genes))
          } else {
            val newChromosome = Chromosome.cross(ch2, ch1)
            (newChromosome, Solution.getCost(graph, newChromosome.genes))
          }
      }.sortBy(_._2)

    val bestCrossGenerations: List[(Chromosome, Double)] =
      crossGenerations.take(populationSize - partition * 2)

    val nextGeneration = (bestCurrentGeneration ::: bestNewGeneration ::: bestCrossGenerations)

    Population(
      chromosomes = nextGeneration.sortBy(_._2),
      graph = graph
    )
  }

  def best: Chromosome =
    chromosomes.head._1

}

object Population {

  def initial(populationSize: Int, numGenes: Int, graph: Graph): Population =
    Population(
      (1 to populationSize)
        .map(_ => Chromosome.generateRandom(numGenes))
        .map(ch => (ch, Solution.getCost(graph, ch.genes)))
        .toList
        .sortBy(_._2),
      graph
    )

}