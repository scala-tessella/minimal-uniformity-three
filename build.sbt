// minimal-uniformity-three — the machine-checked verification artifact for the paper
//   "Minimal uniformity three: unit-edge tilings around the non-Archimedean vertex types".
// It contains only the paper's proof specs; every piece of machinery is the pinned research-core library.
// The specs live in package io.github.scala_tessella.minimal_uniformity_three and import the library from
// io.github.scala_tessella.research_core. All specs run under `sbt test` — exact, in-JVM, no external
// tools; the opt-in A2 certification campaign (K2CompletenessProbe, -Dcert.k2) additionally shells out to
// kissat/drat-trim built by tools/install-sat-tools.sh.

ThisBuild / scalaVersion  := "3.8.4"
ThisBuild / organization  := "io.github.scala-tessella"
ThisBuild / versionScheme := Some("early-semver")

lazy val root = project
  .in(file("."))
  .settings(
    name           := "minimal-uniformity-three",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "io.github.scala-tessella" %% "research-core"        % "0.3.1",
      "io.github.scala-tessella" %% "research-core-solver" % "0.3.1",
      "org.scalatest"            %% "scalatest"       % "3.2.20"   % Test,
      "org.scalacheck"           %% "scalacheck"      % "1.19.0"   % Test,
      "org.scalatestplus"        %% "scalacheck-1-19" % "3.2.20.0" % Test
    )
  )
