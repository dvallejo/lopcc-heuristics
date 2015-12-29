package com.scalera.lopcc.algorithm.genetic

import scala.util.Random

case class Chromosome(genes: List[Int])

object Chromosome {

  def cross(cBest: Chromosome, cWorst: Chromosome): Chromosome = {

    def crossRecursive(result: List[Int], right: Boolean): List[Int] =
      if(cBest.genes.size == result.size)
        result
      else if(right) {
        val element = cBest.genes.reverse.find(i => !result.contains(i)).get
        crossRecursive(result :+ element, !right)
      } else {
        val element = cWorst.genes.find(i => !result.contains(i)).get
        crossRecursive(element :: result, !right)
      }

    Chromosome(crossRecursive(List.empty[Int], true))

  }

  def generateRandom(n: Int): Chromosome =
    Chromosome(
      (0 to n-1).foldLeft((List.empty[Int], (0 to n-1).toList)) {
        case ((l, pending), _) =>
          val randomIndex = Random.nextInt(pending.size)
          (pending(randomIndex) :: l, pending.filter(_ != pending(randomIndex)))
      }._1
    )
}