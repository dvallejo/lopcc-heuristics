package com.scalera.lopcc.algorithm

import com.scalera.lopcc.util.{ Graph, Children, QueueNode }
import com.scalera.lopcc.problem.{ Solution, Bounds }

import scala.collection.mutable.PriorityQueue

/**
  * Branch and bound algorithm
  * @param initBoundSelection if it is necessary calculate an
  *                           initial bound, this param indicates the way to calculate it
  */
case class BranchAndBoundAlgorithm(initBoundSelection: String) 
  extends Algorithm(initBoundSelection) with Bounds with Children {

  /**
    * Execute a branch and bound algorithm
    * @param graph Graph
    * @return the best solution
    */
  def execute(graph: Graph): Solution = {
    val initBound = getInitBound(graph)
    branchAndBound(graph, initBound)
  }

  /**
    * Function that implements a backtracking algorithm
    * @param graph Graph
    * @param bound initial bound
    * @return the best solution
    */
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
          feasibleChildren(nodeSol, nodeGraph)
            .filter(_.bound <= upperBound)
            .foreach( children => queue.enqueue(children))
      }
    }

    sol

  }
}
