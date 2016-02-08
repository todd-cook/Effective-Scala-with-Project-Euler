
organization := "com.wordtrellis"
name := "Effective-Scala-With-Project-Euler"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.11.4"
scalacOptions += "-deprecation"

libraryDependencies ++= Seq(
  "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5" % "test",
  "org.scala-lang" % "scala-library" % "2.11.4"  % "compile",
  "org.scalatest" %  "scalatest_2.11" %  "3.0.0-M15" % "test",
  "org.scala-lang" %  "scala-swing" % "2.11.0-M7" % "compile",
  "junit" % "junit" % "4.8.2" % "test",
  "org.slf4j" % "slf4j-api" % "1.6.1",
  "org.slf4j" % "slf4j-simple" % "1.6.1"
)
