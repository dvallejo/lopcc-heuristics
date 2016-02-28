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

  var prunedNodes: Int = 0

  /**
    * Execute a branch and bound algorithm
    * @param graph Graph
    * @return the best solution
    */
  def execute(graph: Graph): Solution = {
    val initBound = getInitBound(graph)
    println("Calculated initial bound: " + initBound)
    prunedNodes = 0
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
              println("Found a better solution: " + sol.totalCost)
            }
          } else if (nodeBound <= upperBound) {
            val children = feasibleChildren(nodeSol, nodeGraph)

            val (pruned, feasible) = children.partition(_.bound >= upperBound)

            pruned.foreach { node =>
              prunedNodes = prunedNodes + calculatePrunedNodes(node.sol)
            }

            feasible.foreach( children => queue.enqueue(children))
          } else
            prunedNodes = prunedNodes + calculatePrunedNodes(nodeSol)
      }
    }

    println("Num pruned nodes: " + prunedNodes)

    sol

  }
}
