package com.scalera.lopcc.util

import com.scalera.lopcc.problem.{ Solution, Bounds }

/**
  * Functionality for Branch And Bound algorithms
  */
trait Children extends Bounds {

  /**
    * Return all the feasible children of a partial solution
    * @param sol Solution
    * @param graph Graph
    * @return all the feasible children
    */
  def feasibleChildren(sol: Solution, graph: Graph): List[QueueNode] =

    graph.nodes.foldLeft(List.empty[QueueNode]) {
      case (children, node) =>
        val newGraph = graph.removeNode(node)
        val newSol = sol.insertNode(node, graph)

        children :+ QueueNode(newGraph, newSol, getLB1(newGraph, newSol))
    }

}