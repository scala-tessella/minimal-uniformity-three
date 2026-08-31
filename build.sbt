// minimal-uniformity-three — the machine-checked verification artifact for the paper
//   "Minimal uniformity of the non-Archimedean vertex types in unit-edge tilings".
// It contains only the paper's proof specs; every piece of machinery is the pinned research-core library.
// The specs live in package io.github.scala_tessella.minimal_uniformity_three and import the library from
// io.github.scala_tessella.research_core. All specs run under `sbt test` — exact, in-JVM, no external
// tools; the opt-in campaigns are guarded by `-D` flags (README's "Reproduce"), and the k <= 2 certification
// campaign (K2CompletenessProbe, -Dcert.k2) additionally shells out to kissat/drat-trim built by
// tools/install-sat-tools.sh. SAT4J arrives transitively with research-core-solver, which Lattice3366Probe
// drives directly.

ThisBuild / scalaVersion  := "3.8.4"
ThisBuild / organization  := "io.github.scala-tessella"
ThisBuild / versionScheme := Some("early-semver")

lazy val root = project
  .in(file("."))
  .settings(
    name           := "minimal-uniformity-three",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "io.github.scala-tessella" %% "research-core"        % "0.8.0",
      "io.github.scala-tessella" %% "research-core-solver" % "0.8.0",
      "org.scalatest"            %% "scalatest"       % "3.2.20"   % Test,
      "org.scalacheck"           %% "scalacheck"      % "1.19.0"   % Test,
      "org.scalatestplus"        %% "scalacheck-1-19" % "3.2.20.0" % Test
    )
  )
