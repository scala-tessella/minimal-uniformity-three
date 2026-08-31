package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{canonicalKey, DSet, DSymbol}
import io.github.scala_tessella.research_core.TilePatch.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** CONSTRUCTIVE REALIZATION (enable with `-Dwitness.realize`) — certify, per witness pattern, that its tiling
  * EXISTS without importing any realization theorem: the symbol is developed exactly at its rigid point;
  * every tile passes the embedded-simple certificate; a translation lattice is verified with per-class
  * witnesses and the exact cell-area identity; every representative vertex has corners summing to 2π AND
  * CHAINING exactly (partition, not just sum — the local-homeomorphism condition); the extracted torus symbol
  * has total involutions and its minimal image is canonically isomorphic to the witness symbol. The glued
  * cell is then a closed surface mapping to the torus $\E^2/\Lambda$ by a local isometry, hence a covering of
  * degree = area ratio = 1, hence an isometry: the lattice orbit of the cell IS an edge-to-edge unit-edge
  * tiling realizing the symbol. Convexity is nowhere used — reflex patterns certify on equal terms.
  */
class WitnessRealizationProbe extends AnyFlatSpec with Matchers:

  private def symbolFromKey(key: String): DSymbol =
    val rows          = key.split(";").map(_.trim).filter(_.nonEmpty)
    val n             = rows.length
    val op            = Array.ofDim[Int](n + 1, 3)
    val m01           = Array.ofDim[Int](n + 1)
    val m12           = Array.ofDim[Int](n + 1)
    for (r, i) <- rows.zipWithIndex do
      val Array(ops, ms) = r.split('|')
      val parts          = ops.split(',').map(_.toInt)
      op(i + 1)(0) = parts(0); op(i + 1)(1) = parts(1); op(i + 1)(2) = parts(2)
      m01(i + 1) = parts(3); m12(i + 1) = ms.toInt
    val dset          = new DSet(op)
    val (orbs, index) = DelaneySymbols.collectOrbits(dset)
    val vs            = Array.tabulate(orbs.length): k =>
      val o = orbs(k)
      val d = o.elements.head
      (if o.i == 0 then m01(d) else m12(d)) / o.r
    new DSymbol(dset, orbs, index, vs)

  private def rigidPoint(ds: DSymbol, z: List[Int]): Option[Array[Frac]] =
    val sys      = MetricLayer.angleSystem(ds)
    val unforced = UClass
      .candidates(ds, z)
      .filter: r =>
        val irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !r(k) => k }
        UClass.noneForcedRegular(ds, r, irregular)
    for
      reg <- unforced.sortBy(-_.size).headOption
      rows = MetricLayer.designatedRows(ds, reg)
      if MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows)).isEmpty
      x   <- MetricLayer.particularSolution(rows, sys.vars)
    yield x

  private val targets = List(
    ("(3.4.3.12) w k=3", 10.0),
    ("(3.4.4.6) w k=3", 10.0),
    ("(3.3.6.6) w k=3", 10.0),
    ("(5.5.10) w k=4", 10.0),
    ("(3.4.4.6) 22ch cand1", 14.0),
    ("(3.3.4.12) fusion28 #1", 14.0),
    ("(3.3.4.12) fusion28 #2", 14.0),
    ("(3.9.18) w k=4", 14.0),
    ("(3.8.24) w k=3", 18.0),
    ("(3.10.15) pattern k=5", 28.0),
    ("(3.7.42) P1 k=10", 30.0),
    ("(3.4.4.6) saturated k=4 reflex-a", 10.0),
    ("(3.4.4.6) saturated k=4 convex", 10.0),
    ("(3.4.4.6) saturated k=4 reflex-b", 10.0),
    ("(5.5.10) penrose-rhombi k=5", 10.0),
    ("(3.3.4.12) uplus pattern k=7", 12.0),
    ("(4.5.20) pattern k=7", 16.0)
  )

  /** The banked de-fusion endpoints (certs/defusion-endpoints), if present locally. */
  private def endpointTargets: List[(String, List[Int], String, Double)] =
    val tsv = java.nio.file.Path.of("certs", "defusion-endpoints", "endpoints.tsv")
    if !java.nio.file.Files.exists(tsv) then Nil
    else
      java.nio.file.Files.readAllLines(tsv).toArray.map(_.toString).drop(1).filter(_.nonEmpty).toList.map:
        row =>
          val cols = row.split('\t')
          val z    = cols(0).takeWhile(_ != ')').drop(1).split('.').map(_.toInt).toList
          (s"${cols(0)}/${cols(1)}", z, cols(7), 14.0)

  "the witness patterns" should "realize constructively (enable with -Dwitness.realize)" in:
    assume(sys.props.contains("witness.realize"), "realization certificates — enable with -Dwitness.realize")
    var ok   = 0
    val only = sys.props.get("witness.realize.only").filter(_.nonEmpty) // substring filter on the name
    val all  =
      (targets.map((n, r) =>
        val (_, z, key) = SaturationProbe.entries.find(_._1 == n).get
        (n, z, key, r, Option.empty[StrictWitnesses.Entry])
      ) ++ endpointTargets.map((n, z, key, r) => (n, z, key, r, Option.empty[StrictWitnesses.Entry]))
        // the strict-class witnesses (2026-08-30): their point is the strict designation's, pinned for the
        // flexible family, and their patch is judged under strict legality
        ++ StrictWitnesses.entries.map(e => (e.name, e.z, e.key, 10.0, Some(e))))
        .filter((n, _, _, _, _) => only.forall(n.contains))
    for (name, z, key, radius, strictEntry) <- all do
      val t0 = System.nanoTime
      try
        val ds = symbolFromKey(key)
        strictEntry.fold(rigidPoint(ds, z))(StrictWitnesses.solution(_, ds)) match
          case None    => println(s"FAILED $name: no rigid point")
          case Some(x) =>
            val state = seed(
              ExactDeveloper.develop(ds, x, radius),
              z,
              strict = strictEntry.isDefined,
              isolated = strictEntry.isDefined
            )
            if !valid(state) then println(s"FAILED $name: invalid patch")
            else if !state.tiles.forall(_.poly.isSimpleCertified) then
              println(s"FAILED $name: a tile fails the embedding certificate")
            else
              Periodicity.certifiedCell(state) match
                case None              => println(s"FAILED $name: no certified cell at radius $radius")
                case Some((t1, t2, c)) =>
                  SymbolExtractor.torusSymbol(state, t1, t2) match
                    case None        => println(s"FAILED $name: torus symbol (chaining/involutions/reps)")
                    case Some(torus) =>
                      val min      = SymbolExtractor.minimalImage(torus)
                      val entryMin = SymbolExtractor.minimalImage(ds)
                      val iso      = min.canonicalKey == entryMin.canonicalKey
                      if !iso then println(s"FAILED $name: minimal image (C=${min.size}) is not the symbol")
                      else
                        ok += 1
                        println(f"REALIZED $name: C=${min.size} k=${min.orbs.count(_.i == 1)} " +
                          f"tiles=${state.tiles.length} cellArea≈${c.cellArea.approx._2}%.3f " +
                          f"[${(System.nanoTime - t0) / 1e9}%.0fs]")
      catch
        case e: Throwable =>
          println(f"FAILED $name: ${e.getClass.getSimpleName}: ${e.getMessage} " +
            f"[${(System.nanoTime - t0) / 1e9}%.0fs]")
    println(s"== constructively realized: $ok/${all.length}")
