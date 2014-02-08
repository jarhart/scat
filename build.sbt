name := "Scat"

organization := "com.jarhart"

version := "0.0.1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.5",
  "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full,
  "org.scalatest" %% "scalatest" % "2.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)

scalacOptions ++= Seq("-deprecation", "-feature")

initialCommands := """
import com.jarhart.scat._
import shapeless._
import Examples._
"""
