package com.scalera.lopcc.util

import com.scalera.lopcc.problem.Solution

/**
  * Node of a priority queue
  * @param graph Graph
  * @param sol Solution
  * @param bound Bound calculated with the solution
  */
case class QueueNode(
  graph: Graph,
  sol: Solution,
  bound: Double
) extends Ordered[QueueNode] {

  override def compare(that: QueueNode) =
    bound compare that.bound
}