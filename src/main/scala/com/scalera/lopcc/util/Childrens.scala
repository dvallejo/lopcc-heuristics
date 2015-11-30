package com.scalera.lopcc.util

import com.scalera.lopcc.problem.{ Solution, Bounds }

trait Childrens extends Bounds {

  def feasibleChildrens(sol: Solution, graph: Graph): List[QueueNode] =

    graph.nodes.foldLeft(List.empty[QueueNode]) {
      case (childrens, node) =>
        val newGraph = graph.removeNode(node)
        val newSol = sol.insertNode(node, graph)

        childrens :+ QueueNode(newGraph, newSol, getLB1(newGraph, newSol))
    }

}