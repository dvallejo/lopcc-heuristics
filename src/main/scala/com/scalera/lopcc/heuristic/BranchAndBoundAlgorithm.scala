package com.scalera.lopcc.heuristic

import com.scalera.lopcc.util.{ Graph, Childrens, QueueNode }
import com.scalera.lopcc.problem.{ Solution, Bounds }

import scala.collection.mutable.PriorityQueue

object BranchAndBoundAlgorithm extends Algorithm with Bounds {

  def execute(graph: Graph): Solution = {
    val initBound = getInitBound(graph)
    branchAndBound(graph, initBound)
  }

  def branchAndBound(graph: Graph, bound: Double): Solution = {
    
    var sol = Solution.empty(graph.maxNumNodes)
    // val queue = PriorityQueue.empty[QueueNode]
    // val childrens = Childrens(List.empty[QueueNode])
    
    // var partialGraph: Graph = graph
    // var partialSol: Solution = sol
    // var boundAux: Double = getLB1(partialGraph, partialSol)
    // var queueNode: QueueNode = QueueNode(partialGraph, partialSol, boundAux)

    // queue.enqueue(queueNode)

    // while(!queue.isEmpty) {
      
    //   queueNode = queue.dequeue
    //   partialGraph = queueNode.graph
    //   partialSol = queueNode.sol
    //   boundAux = queueNode.bound

    //   if (partialSol.isComplete) {
    //     if (partialSol.isBetter(sol)) {
    //       sol = partialSol
    //       boundAux = sol.getCost
    //     }

    //   } else if(boundAux <= cotaSuperior){  //Podo          
    //       hijos.compleccionesFactibles(partialSol, grafoParcial)
    //       while(childrens.isEmpty()){
    //         nodoCola = hijos.dameHijo()
    //         nodoCola.getNodo().copiar(partialSol)
    //         nodoCola.getGrafo().copiar(grafoParcial)
    //         cotaAux = nodoCola.getCotaSolucion()
    //         if(cotaAux <= cotaSuperior){ //Podo
    //           nodoCola = new NodoColaPrioridad(partialSol, cotaAux, grafoParcial)
    //           cola.add(nodoCola)
    //         }else{
    //           nodosPodados = nodosPodados + calcularNodosPodados(partialSol)
    //         }
    //       }
    //   } else nodosPodados = nodosPodados + calcularNodosPodados(partialSol)
    // }

    sol

  }
}
