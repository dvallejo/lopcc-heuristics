package com.scalera.lopcc.util

import com.scalera.lopcc.problem.Solution

case class Childrens(childrens: List[QueueNode]) {

  def getChildren: QueueNode = childrens.last

  def isEmpty: Boolean = childrens.isEmpty

  def feasibleChildrens(partialSol: Solution, graph: Graph): Solution = ???

}