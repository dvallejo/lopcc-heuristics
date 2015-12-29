package com.scalera.lopcc.algorithm.genetic

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

/**
  * Population with several chromosomes
  * @param chromosomes chromosomes of the population
  * @param graph Graph
  */
case class Population(chromosomes: List[(Chromosome, Double)], graph: Graph) {

  def populationSize = chromosomes.size
  def numGenes = chromosomes.head._1.genes.size

  /**
    * Evolve the population. The new generation will have three components:
    * - The third best of the current population
    * - The third best of a new population generated randomly
    * - Chromosomes result of cross the two population defined above
    * @return the new population
    */
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

    val nextGeneration = bestCurrentGeneration ::: bestNewGeneration ::: bestCrossGenerations

    Population(
      chromosomes = nextGeneration.sortBy(_._2),
      graph = graph
    )
  }

  /**
    * Return the best solution of the population
    * @return the best chromosome
    */
  def best: Chromosome =
    chromosomes.head._1

}

object Population {

  /**
    * Create a random population with the chromosomes sorted by cost
    * @param populationSize size of the population
    * @param numGenes num of genes of each chromosome
    * @param graph Graph
    * @return the new population
    */
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