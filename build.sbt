name := "lopcc-heuristics"

version := "0.1.0"

scalaVersion  := "2.11.2"

scalacOptions ++= Seq(
  "-deprecation", 
  "-unchecked", 
  "-Xlint",
  "-Ywarn-unused",
  "-Ywarn-unused-import"
)
