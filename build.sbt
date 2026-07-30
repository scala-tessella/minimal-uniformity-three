// minimal-uniformity — the machine-checked verification artifact for the paper
//   "Minimal uniformity three: unit-edge tilings around the non-Archimedean vertex types".
// It contains only the paper's proof specs; every piece of machinery is the pinned research-core library.
// The specs live in package io.github.scala_tessella.minimal_uniformity and import the library from
// io.github.scala_tessella.research_core. All specs run under `sbt test` — exact, in-JVM, no external tools.

ThisBuild / scalaVersion  := "3.8.4"
ThisBuild / organization  := "io.github.scala-tessella"
ThisBuild / versionScheme := Some("early-semver")

lazy val root = project
  .in(file("."))
  .settings(
    name           := "minimal-uniformity",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "io.github.scala-tessella" %% "research-core"   % "0.3.0",
      "org.scalatest"            %% "scalatest"       % "3.2.20"   % Test,
      "org.scalacheck"           %% "scalacheck"      % "1.19.0"   % Test,
      "org.scalatestplus"        %% "scalacheck-1-19" % "3.2.20.0" % Test
    )
  )
