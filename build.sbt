organization := "com.wordtrellis"
//name := "Effective-Scala-With-Project-Euler"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.13.0"
scalacOptions += "-deprecation"

lazy val eswpe = (project in file("."))
  .settings(
    name := "Effective-Scala-With-Project-Euler",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8" % "test"
  )

