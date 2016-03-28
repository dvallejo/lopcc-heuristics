package com.scalera.lopcc.generator

import java.io._
import scala.util.Random

object FileGenerator {

  val filename = "src/main/resources/pruebas/dvn"

  def main(args: Array[String]) =
    args match {
      case Array(maxNodes) =>
        (16 to maxNodes.toInt) foreach writeInFile

      case _ => 
        println("usage: maxNumNodes")
    }

  def writeInFile(numNodes: Int) = {
    val writer = new PrintWriter(new File(s"$filename$numNodes.txt"))
    writer.write(createGraph(numNodes))
    writer.close()
  }

  def createGraph(numNodes: Int) =
    Seq.fill(numNodes + 1)(createRow(numNodes)).mkString("\n")

  def createRow(numNodes: Int): String =
    Seq.fill(numNodes)(generateRandomValue).mkString("  ")

  def generateRandomValue: String = {
    val random = Random.nextDouble
    random.toString.take(8)
  }
}
