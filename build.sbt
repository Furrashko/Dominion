ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "de.furrashko"
ThisBuild / version := "0.1.0-SNAPSHOT"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "Dominion"
  )
