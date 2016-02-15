package com.scalera.lopcc.algorithm.genetic

import scala.util.Random

/**
  * Chromosome of a specific solution
  * @param genes
  */
case class Chromosome(genes: List[Int])

object Chromosome {

  /**
    * Cross two chromosomes in order to have a new one
    * @param cBest the best chromosome of the two
    * @param cWorst the worse chromosome of the two
    * @return the new chromosome
    */
  def cross(cBest: Chromosome, cWorst: Chromosome): Chromosome = {

    def insert(result: List[Int], element: Int): List[Int] =
      result.take(result.size / 2) union element :: result.drop(result.size / 2)

    def crossRecursive(result: List[Int], right: Boolean): List[Int] =
      if(cBest.genes.size == result.size)
        result
      else if(right) {
        val element = cBest.genes.reverse.find(i => !result.contains(i)).get
        val newResult = insert(result, element)
        crossRecursive(newResult, !right)
      } else {
        val element = cWorst.genes.find(i => !result.contains(i)).get
        val newResult = insert(result, element)
        crossRecursive(newResult, !right)
      }

    Chromosome(crossRecursive(List.empty[Int], true))
  }

  /**
    * Generate a random Chromosome with a random solution
    * @param n size of the chromosome
    * @return the new chromosome
    */
  def generateRandom(n: Int): Chromosome =
    Chromosome(
      (0 to n-1).foldLeft((List.empty[Int], (0 to n-1).toList)) {
        case ((l, pending), _) =>
          val randomIndex = Random.nextInt(pending.size)
          (pending(randomIndex) :: l, pending.filter(_ != pending(randomIndex)))
      }._1
    )
}