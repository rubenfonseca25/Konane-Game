ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.2"

lazy val root = (project in file("."))
  .settings(
    name := "Konane_Jogo",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
  )
