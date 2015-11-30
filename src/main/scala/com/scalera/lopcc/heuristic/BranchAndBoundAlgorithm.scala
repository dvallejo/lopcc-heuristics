package com.scalera.lopcc.heuristic

import com.scalera.lopcc.util.{ Graph, Childrens, QueueNode }
import com.scalera.lopcc.problem.{ Solution, Bounds }

import scala.collection.mutable.PriorityQueue

object BranchAndBoundAlgorithm extends Algorithm with Bounds with Childrens {

  def execute(graph: Graph): Solution = {
    val initBound = getInitBound(graph)
    branchAndBound(graph, initBound)
  }

  def branchAndBound(graph: Graph, bound: Double): Solution = {
    
    var sol = Solution.empty(graph.maxNumNodes)
    val queue = PriorityQueue.empty[QueueNode]
    var upperBound = bound

    val firstNode = QueueNode(graph, sol, getLB1(graph, sol))
    queue.enqueue(firstNode)

    while(!queue.isEmpty) {

      queue.dequeue() match {
        case QueueNode(nodeGraph, nodeSol, nodeBound) =>

        if (nodeSol.isComplete) {
          if (nodeSol.isBetter(sol)) {
            sol = nodeSol
            upperBound = sol.totalCost
          }
        } else if (nodeBound <= upperBound)     
          feasibleChildrens(nodeSol, nodeGraph)
            .filter(_.bound <= upperBound)
            .foreach( children => queue.enqueue(children))
      }
    }

    sol

  }
}
