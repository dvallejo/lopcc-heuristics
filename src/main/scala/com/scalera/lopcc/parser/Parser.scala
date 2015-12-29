package com.scalera.lopcc.parser

import scala.io.{ Source, BufferedSource}
import scala.util.Try

import com.scalera.lopcc.util.Graph

trait Parser {

  def processFile(filePath: String): Graph = {

    val lines: List[String] = getFile(filePath).getLines().toList

    val graph = Graph.empty(lines.size)

    lines.zipWithIndex
      .foldLeft(graph) {
        case (g, (line, i)) =>
          processLine(i, line, g)
      }
  }

  private def getFile(filePath: String): BufferedSource =
    Try(Source.fromFile(getClass.getResource(filePath).getFile))
      .getOrElse(
        Source.fromFile(filePath)
      )

  private def processLine(i: Int, line: String, graph: Graph): Graph = {
    
    val costs: Seq[String] = line.split("\\s+")
    
    costs.zipWithIndex.foldLeft(graph) {
      case (g, (cost, j)) => 
        g.setEdge(i, j, cost.toDouble)
    }
  }
      
}
