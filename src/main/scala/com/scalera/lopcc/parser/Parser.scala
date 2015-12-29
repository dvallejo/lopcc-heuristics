package com.scalera.lopcc.parser

import scala.io.{ Source, BufferedSource}
import scala.util.Try

import com.scalera.lopcc.util.Graph

trait Parser {

  /**
    * Create a graph from a file
    * @param filePath path of the file
    * @return a graph
    */
  def processFile(filePath: String): Graph = {

    val lines: List[String] = getFile(filePath).getLines().toList

    val graph = Graph.empty(lines.size)

    lines.zipWithIndex
      .foldLeft(graph) {
        case (g, (line, i)) =>
          processLine(i, line, g)
      }
  }

  /**
    * Get a File from a path. If the file is not in the classpath, try again with an absolute path
    * @param filePath path of the file
    * @return the file
    */
  private def getFile(filePath: String): BufferedSource =
    Try(Source.fromFile(getClass.getResource(filePath).getFile))
      .getOrElse(
        Source.fromFile(filePath)
      )

  /**
    * Process a line in order to extract the values of the graph matrix
    * @param i index of the line
    * @param line line of the file
    * @param graph graph partially build
    * @return a graph with the data of the line
    */
  private def processLine(i: Int, line: String, graph: Graph): Graph = {
    
    val costs: Seq[String] = line.split("\\s+")
    
    costs.zipWithIndex.foldLeft(graph) {
      case (g, (cost, j)) => 
        g.setEdge(i, j, cost.toDouble)
    }
  }
      
}
