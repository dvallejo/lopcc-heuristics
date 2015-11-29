package com.scalera.lopcc.util

import com.scalera.lopcc.problem.{ Solution, Bounds }

case class Childrens(childrens: List[QueueNode]) extends Bounds {

  def getChildren: QueueNode = childrens.last

  def isEmpty: Boolean = childrens.isEmpty

  def feasibleChildrens(partialSol: Solution, graph: Graph): Childrens = {

    val newChildrens = 
      partialSol.nodes.foldLeft(List.empty[QueueNode]) {
        case (childrens, node) =>
          val newGraph = graph.removeNode(node)
          val newSol = partialSol.insertNode(node, graph)

          childrens :+ QueueNode(newGraph, newSol, getLB1(newGraph, newSol))
      }

    this.copy(childrens = newChildrens)

  }

}