package com.scalera.lopcc.parser

import scala.io.{ Source, BufferedSource}

import com.scalera.lopcc.util.Graph

trait Parser {

  val filePath: String

  def getFile: BufferedSource =
    Source.fromFile(getClass.getResource(filePath).getFile)

  def processFile: Graph = {

    val lines: List[String] = getFile.getLines().toList

    val graph = Graph.empty(lines.size)

    lines.zipWithIndex
      .foldLeft(graph) {
        case (g, (line, i)) =>
          processLine(i, line, g)
      }
  }

  def processLine(i: Int, line: String, graph: Graph): Graph = {
    
    val costs: Seq[String] = line.split("\\s+")
    
    costs.zipWithIndex.foldLeft(graph) {
      case (g, (cost, j)) => 
        g.setEdge(i, j, cost.toDouble)
    }
  }
      
}
