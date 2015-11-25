package com.scalera.lopcc.heuristic

import com.scalera.lopcc.util.Graph
import com.scalera.lopcc.problem.Solution

// import scala.collection.mutable.PriorityQueue

object BranchAndBoundAlgorithm extends Algorithm {

  def execute(graph: Graph): Solution = {
    val sol = Solution.empty(graph.maxNumNodes)
    val initBound = getInitBound(graph)
    branchAndBound(graph, sol, initBound)
  }

  def branchAndBound(graph: Graph, sol: Solution, bound: Double): Solution = ???
  //   val queue = new PriorityQueue.empty[QueueNode] 
}
