ThisBuild / scalaVersion := "3.6.1"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / libraryDependencies ++= Seq(
  // cats libraries
  "org.typelevel" %% "cats-core" % "2.12.0",
  "org.typelevel" %% "cats-kernel" % "2.12.0",
  "org.typelevel" %% "cats-effect" % "3.6-623178c",

  // test libraries
  "org.scalatest" %% "scalatest" % "3.2.18" % "test",
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % "test"
)

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-Ykind-projector:underscores"
)
