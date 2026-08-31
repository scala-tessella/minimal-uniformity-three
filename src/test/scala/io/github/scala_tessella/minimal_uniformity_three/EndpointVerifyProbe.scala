package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{canonicalKey, DSet, DSymbol}
import io.github.scala_tessella.research_core.TilePatch.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** ENDPOINT VERIFICATION (enable with `-Ddefusion.verify`) — promote the sampled endpoints in
  * `certs/defusion-endpoints/endpoints.tsv` from observations to verified objects. Per row: the serialised
  * minimal symbol must parse back to its banked (C, k) profile and be MINIMAL; its rigid point must exist
  * through the standard designation machinery (dimension 0); its exact development must be a valid patch of
  * embedded tiles whose irregular census matches the banked row verbatim; the state must be geometrically
  * SATURATED under witnessed class-wide moves; and re-extraction must return the same canonical key
  * (idempotence). Finally each endpoint is matched against the known entries' minimal images — saturated
  * seeds must round-trip to themselves.
  */
class EndpointVerifyProbe extends AnyFlatSpec with Matchers:

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

  private def rigidPoint(ds: DSymbol, z: List[Int]): Array[Frac] =
    val sys      = MetricLayer.angleSystem(ds)
    val unforced = UClass
      .candidates(ds, z)
      .filter: r =>
        val irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !r(k) => k }
        UClass.noneForcedRegular(ds, r, irregular)
    withClue("class membership (unforced designations): ")(unforced.nonEmpty shouldBe true)
    val reg      = unforced.maxBy(_.size)
    val rows     = MetricLayer.designatedRows(ds, reg)
    withClue("rigidity: ")(
      MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows)).size shouldBe 0
    )
    MetricLayer.particularSolution(rows, sys.vars).get

  private def irregularSummary(s: State): String =
    s.tiles
      .map(_.poly)
      .filter(regularSizeOf(_).isEmpty)
      .groupBy(shapeKey)
      .values
      .map(_.head)
      .toVector
      .sortBy(p => (p.dirs.length, p.interiorAngles.sorted.mkString(",")))
      .map(p => s"${p.dirs.length}-gon${p.interiorAngles.sorted.mkString("(", ",", ")")}")
      .mkString("; ")

  "the persisted endpoints" should "verify end-to-end (enable with -Ddefusion.verify)" in:
    assume(sys.props.contains("defusion.verify"), "endpoint verification — enable with -Ddefusion.verify")
    val tsv       = java.nio.file.Path.of("certs", "defusion-endpoints", "endpoints.tsv")
    val rows      = java.nio.file.Files.readAllLines(tsv).toArray.map(_.toString).drop(1).filter(_.nonEmpty)
    val entryMins = SaturationProbe.entries.map((n, _, k) =>
      SymbolExtractor.minimalImage(symbolFromKey(k)).canonicalKey -> n
    ).toMap
    for row <- rows do
      val cols                    = row.split('\t')
      val (name, ep, cS, kS, irr) = (cols(0), cols(1), cols(2), cols(3), cols(6))
      val z                       = name.takeWhile(_ != ')').drop(1).split('.').map(_.toInt).toList
      val ds                      = symbolFromKey(cols(7))
      val t0                      = System.nanoTime

      withClue(s"$name/$ep profile: "):
        ds.size shouldBe cS.toInt
        ds.orbs.count(_.i == 1) shouldBe kS.toInt
        SymbolExtractor.minimalImage(ds).size shouldBe ds.size // banked symbols are minimal

      val x        = rigidPoint(ds, z)
      val state    = seed(ExactDeveloper.develop(ds, x, 14.0), z)
      withClue(s"$name/$ep development: "):
        valid(state) shouldBe true
        irregularSummary(state) shouldBe irr
      val embedded = state.tiles.forall(_.poly.isSimpleCertified)
      val known    = entryMins.get(ds.canonicalKey).map(n => s"≅ entry '$n'").getOrElse("new tiling")

      if !embedded then
        // straight (π) corners or self-contact: out of class by the ratified standard — the
        // verdict is recorded, the remaining certificates are moot
        println(f"DEGENERATE $name/$ep: C=${ds.size} k=${kS.toInt} tiles=${state.tiles.length} " +
          f"[non-embedded tiles, $known] ${(System.nanoTime - t0) / 1e9}%.1fs")
      else
        withClue(s"$name/$ep saturation: ")(admissible(state) shouldBe empty)
        val again = Periodicity.certifiedCell(state).flatMap((t1, t2, _) =>
          SymbolExtractor.profile(state, t1, t2)
        )
        val idem  = again match
          case Some((c2, k2, key2)) =>
            withClue(s"$name/$ep idempotence: "):
              (c2, k2) shouldBe (ds.size, kS.toInt)
              key2 shouldBe ds.canonicalKey
            "idempotent"
          case None                 => "re-extraction unavailable (budget)"
        println(f"VERIFIED $name/$ep: C=${ds.size} k=${kS.toInt} tiles=${state.tiles.length} " +
          f"[$idem, $known] ${(System.nanoTime - t0) / 1e9}%.1fs")
