package com.scalera.lopcc.algorithm.ant

import scala.util.Random

case class PheromoneGraph(
  maxNumNodes: Int,
  weights: Map[Int, Map[(Int, Int), Double]]
) {

  def getCost(level: Int)(from: Int, to: Int): Double =
    weights(level)((from, to))

  def setCost(level: Int, from: Int, to: Int, cost: Double): PheromoneGraph =
    this.copy(
      weights = weights + (level -> (weights(level) + ((from, to) -> cost)))
    )
}

object PheromoneGraph {

  def initial(size: Int): PheromoneGraph =
    PheromoneGraph(
      maxNumNodes = size,
      (1 to size).map ( step =>
        (
          step,
          (for {
            x <- (0 to size - 1)
            y <- (0 to size - 1)
          } yield ((x, y), 1.0)).toMap
        )
      ).toMap
    )
}
