package com.scalera.lopcc.algorithm.genetic.v2

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution
import com.scalera.lopcc.algorithm.genetic.Chromosome
import Population._

import scala.util.Random

/**
  * Population with several chromosomes
  * @param chromosomes chromosomes of the population
  * @param graph Graph
  */
case class Population(chromosomes: Chromosomes, graph: Graph) {

  def populationSize = chromosomes.size
  def numGenes = chromosomes.head._1.genes.size
  
  val eliteSize = 3
  val tournamentSize = 3
  val crossTimes = 100
  val mutationProbability = 0.7

  /**
    * Evolve the population. The new generation will have three components:
    * - The third best of the current population
    * - The third best of a new population generated randomly
    * - Chromosomes result of cross the two population defined above
    * @return the new population
    */
  def evolve: Population = {

    val newGeneration = createNewGenerationByTournament(chromosomes)

    val theBestOnes = chromosomes.take(eliteSize)

    val crossGeneration: Chromosomes =
      (1 to crossTimes).foldLeft(List.empty[(Chromosome, Double)]) {
        case (l, _) =>
          
          val (ch1, cost1) = newGeneration(Random.nextInt(newGeneration.size))
          val (ch2, cost2) = newGeneration(Random.nextInt(newGeneration.size))

          val newChromosome: Chromosome = 
            if (cost1 < cost2)
              Chromosome.cross(ch1, ch2)
            else
              Chromosome.cross(ch2, ch1)

          (newChromosome, Solution.getCost(graph, newChromosome.genes)) :: l
      }
      .union(newGeneration)
      .sortBy(_._2)
      .take(populationSize - eliteSize)

    val nextGeneration = 
      if (mutationProbability > 0)
        theBestOnes ::: mutation(crossGeneration)
      else
        theBestOnes ::: crossGeneration

    Population(
      chromosomes = nextGeneration.sortBy(_._2),
      graph = graph
    )
  }

  def mutation(chromosomes: Chromosomes) = chromosomes.map { chromosome =>
    val rnd = Random.nextDouble
    if (rnd < mutationProbability) {
      val newCh = Chromosome.mutate(chromosome._1)
      (newCh, Solution.getCost(graph, newCh.genes))
    } else
      chromosome
  }

  def createNewGenerationByTournament(chromosomes: Chromosomes): Chromosomes =
    (1 to populationSize - (eliteSize - 1)).foldLeft(List.empty[(Chromosome, Double)]) {
      case (l, i) => tournament(chromosomes) :: l
    }

  def chooseRandomChromosome(chromosomes: Chromosomes): (Chromosome, Double) =
    chromosomes(Random.nextInt(populationSize))

  def tournament(chromosomes: Chromosomes): (Chromosome, Double) =
    (1 to tournamentSize).foldLeft(List.empty[(Chromosome, Double)]) {
      case (l, i) => chooseRandomChromosome(chromosomes) :: l
    }
    .sortBy(_._2)
    .head

  /**
    * Return the best solution of the population
    * @return the best chromosome
    */
  def best: Chromosome =
    chromosomes.head._1

}

object Population {

  type Chromosomes = List[(Chromosome, Double)]

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