package com.scalera.lopcc.util

import com.scalera.lopcc.problem.Solution

case class QueueNode(
  graph: Graph,
  sol: Solution,
  bound: Double
) extends Ordered[QueueNode] {

  override def compare(that: QueueNode) =
    bound compare that.bound
}