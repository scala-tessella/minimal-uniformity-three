package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** THE STRICT RE-FILTER (enable with `-Dstrict.refilter`; `-Dstrict.refilter.z=3.4.4.6` to take one species,
  * `-Dstrict.refilter.log=<path>` for a flushed log).
  *
  * The strict contiguous-arc class is a SUBSET of the spliced class the campaigns swept, so every strict
  * tiling over a swept window is already among that window's banked hits: no walk is needed, only a re-read.
  * Phase 1 re-derives every genuine designation of every banked hit under `UClass.candidates(_, _, strict =
  * true)` — the six hits files of `certs/uclass-k4` (≤ 24 and 25–29 chambers, k ≤ 4, both valence regimes)
  * and `certs/uclass-k3` (25–26, k = 3), plus every banked pattern and the appendix's flexible family —
  * deduplicating symbols by canonical key, and writes the survivors to
  * `certs/uclass-strict/strict-candidates.tsv`. Phase 2 closure-triages each survivor the way the k = 5
  * survivor mining did: a RIGID designation → positive, non-straight, EXACTLY closing corner solution →
  * radius-6 development → strict vertex legality and embedding; a one-parameter FLEXIBLE family is resolved
  * as in the (3²6²) nonlinear-closure probe — exact simple window, dense residual scan with a Lipschitz
  * certificate, identically-closed families realized at a rational mid-window parameter, isolated roots at
  * matched small rationals; 2-D families are reported open. A passing realization is verified at radius 10
  * with a certified translation cell and its minimal symbol (true C and k), union-decided per irregular class
  * (essential irregularity, `def:essential`), checked for convexity, and saturation-checked under STRICT
  * legality. Witnesses go to `certs/uclass-strict/strict-witnesses.tsv`.
  *
  * The lost bounds this can restore: $(3.4^2.6) = 3$ and $(3^2.6^2) = 3$ need a strict n = 3 witness, which
  * the ≤ 24 files carry (the k ≤ 4 sweep includes its n = 3 symbols — the two lost spliced witnesses are
  * looked up by canonical key as a check); the saturated $(3.4^2.6)$ row needs a strictly saturated witness;
  * $\U^{+}(3^2.4.12) \le 7$ needs an essentially irregular one.
  */
class StrictRefilterProbe extends AnyFlatSpec with Matchers:

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
      val o = orbs(k); val d = o.elements.head
      (if o.i == 0 then m01(d) else m12(d)) / o.r
    new DSymbol(dset, orbs, index, vs)

  private val hitsFiles = List(
    "certs/uclass-k4/window-hits-min1-maxSize24.tsv",
    "certs/uclass-k4/window-hits-min1-maxSize24-v2.tsv",
    "certs/uclass-k4/window-hits-min25-maxSize29.tsv",
    "certs/uclass-k4/window-hits-min25-maxSize29-v2.tsv",
    "certs/uclass-k3/window-hits-min25-maxSize26.tsv",
    "certs/uclass-k3/window-hits-min25-maxSize26-v2.tsv",
    // the k <= 6 window of the three-letter species (25-36 chambers, valence-2 regime, vertex-orbit shapes
    // 6-cycle / 4-cycle / 2-chain): UClassK3ShardProbe -Duclass.k3s.maxN=6 -Duclass.k3s.v2 -Duclass.k3s.vcap=6
    // -Duclass.k3s.shapes3, 2026-08-30 — the (4.5.20) = 7 and (3.7.42) >= 7 window
    "certs/uclass-k6/window-hits-min1-maxSize24-v2-shapes3.tsv",
    "certs/uclass-k6/window-hits-min25-maxSize36-v2-shapes3.tsv",
    // the same window at k <= 7 (1-42 chambers, maxN=7): the (3.7.42) >= 8 window
    "certs/uclass-k7/window-hits-min1-maxSize42-v2-shapes3.tsv",
    // the same window at k <= 8 (1-48 chambers, maxN=8): silent for (3.7.42) at eight orbits
    "certs/uclass-k8/window-hits-min1-maxSize48-v2-shapes3.tsv",
    // the same window at k <= 9 (1-54 chambers, maxN=9): the last (3.7.42) level
    "certs/uclass-k9/window-hits-min1-maxSize54-v2-shapes3.tsv"
  )

  /** Patterns the manuscript relies on that the hits files may not hold (30 chambers, k = 7, the flexible
    * family): every banked entry, plus the appendix's 24-chamber flexible $(3^2.6^2)$ family of
    * `prop:uplus3366`. Each enters the same re-filter and triage, so its strict SUB-designations (a further
    * face orbit declared irregular) get a closure verdict too.
    */
  private val extraPatterns: List[(String, List[Int], String)] =
    SaturationProbe.entries ++ List(
      (
        "(3.3.6.6) uplus flexible 24ch (appendix)",
        List(3, 3, 6, 6),
        "2,3,2,6|4;1,4,1,6|4;5,1,6,6|4;7,2,8,6|4;3,7,9,6|4;9,10,3,3|4;4,5,11,6|4;11,12,4,3|4;6,13,5,3|4;" +
          "14,6,12,3|4;8,15,7,3|4;16,8,10,3|4;17,9,18,3|4;10,17,16,3|4;19,11,20,3|4;12,19,14,3|4;" +
          "13,14,21,3|4;21,20,13,6|4;15,16,22,3|4;22,18,15,6|4;18,23,17,6|4;20,24,19,6|4;24,21,24,6|4;" +
          "23,22,23,6|4"
      )
    )

  /** Piece universe of the union decision per species: the ζ₁₂ lattice carries {3, 4, 6, 12}, the ζ₁₀ one {5,
    * 10}; elsewhere the decision is relative to the species' own letters and labelled so.
    */
  private def pieces(z: List[Int]): List[Int] =
    if z.forall(Set(3, 4, 6, 12)) then List(3, 4, 6, 12) else z.distinct

  /** `-Dstrict.refilter.isolated`: the ISOLATED reading — strict arc AND at most one irregular tile per
    * vertex (2026-08-30 measurement).
    */
  private val isolated = sys.props.contains("strict.refilter.isolated")

  private def genuine(ds: DSymbol, z: List[Int], strict: Boolean): List[Set[Int]] =
    UClass.candidates(ds, z, strict, strict && isolated).filter: r =>
      val irr = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !r(k) => k }
      UClass.noneForcedRegular(ds, r, irr)

  private def field(line: String, name: String): String =
    line.split('\t').find(_.startsWith(s"$name=")).map(_.drop(name.length + 1)).getOrElse("")

  /** Exact 1-D simple window of a flexible family x0 + t·v: t with every corner in (0, 2) — reflex admitted,
    * straight-corner punctures checked at realization. (From the (3²6²) nonlinear-closure resolver.)
    */
  private def window1(x0: Array[Frac], v: Array[Frac]): Option[(Double, Double)] =
    scala.util.boundary:
      var lo = Double.NegativeInfinity
      var hi = Double.PositiveInfinity
      for j <- x0.indices do
        val a = v(j).toDouble
        val b = x0(j).toDouble
        if math.abs(a) > 1e-15 then
          val t0 = -b / a
          val t2 = (2 - b) / a
          if a > 0 then { lo = math.max(lo, t0); hi = math.min(hi, t2) }
          else { lo = math.max(lo, t2); hi = math.min(hi, t0) }
        else if b <= 1e-15 || b >= 2 - 1e-15 then scala.util.boundary.break(None)
      if lo < hi then Some((lo, hi)) else None

  /** Lipschitz bound on t ↦ maxClosureResidual along the family (π · max over faces of the summed partial
    * slopes along the face walk), so a sampled minimum above L·δ/2 certifies positivity on the window.
    */
  private def lipschitz(ds: DSymbol, corner: Array[Int], v: Array[Frac]): Double =
    val vs = v.map(_.toDouble)
    var l  = 0.0
    for o <- ds.orbs if o.i == 0 do
      val d0   = o.elements.head
      val m    = ds.m(0, 1, d0)
      val frag = collection.mutable.ArrayBuffer.empty[Int]
      var cur  = d0
      var go   = true
      while go do
        frag += corner(cur)
        cur = ds.get(1, ds.get(0, cur))
        if cur == d0 then go = false
      var run  = 0.0
      var sum  = 0.0
      for i <- 0 until m do
        sum += vs(frag(i % frag.length))
        run += math.abs(sum)
      l = math.max(l, math.Pi * run)
    math.max(l, 1e-6)

  private def honest(x: Array[Frac]): Boolean =
    x.forall { g =>
      val d = g.toDouble; d > 1e-9 && d < 2 - 1e-9 && math.abs(d - 1) > 1e-9
    }

  it should
    "re-filter the banked hits under the strict reading and triage the survivors (-Dstrict.refilter)" in:
      assume(sys.props.contains("strict.refilter"), "strict re-filter — enable with -Dstrict.refilter")
      val only                 = sys.props.get("strict.refilter.z").filter(_.nonEmpty)
      val outDir               = Path.of("certs", "uclass-strict")
      val prefix               = if isolated then "isolated-" else ""
      Files.createDirectories(outDir)
      val logW                 = sys.props.get("strict.refilter.log").map(p =>
        new java.io.PrintWriter(new java.io.FileWriter(p, true))
      )
      def log(s: String): Unit =
        println(s); logW.foreach { w => w.println(s); w.flush() }

      // ---- the banked patterns, looked up by canonical key: are the n = 3 symbols really in the files?
      val lookFor = extraPatterns.map: (name, z, key) =>
        (name, DelaneySymbols.canonicalKey(symbolFromKey(key)).stripSuffix(";"))
      val located = collection.mutable.Map.empty[String, List[String]].withDefaultValue(Nil)

      // ---- phase 1: the strict re-filter
      val seen  = collection.mutable.Map.empty[String, String] // (species, canonical key) -> first source
      val cands = collection.mutable.ListBuffer.empty[(String, List[Int], DSymbol, Int, Set[Int], String)]
      var read  = 0
      for f <- hitsFiles do
        val lines = Files.readAllLines(Path.of(f)).asScala.toVector.filter(_.startsWith("z="))
        log(s"== $f: ${lines.size} hits")
        for line <- lines do
          val zName = field(line, "z")
          val key   = field(line, "key").stripSuffix(";")
          for (name, k) <- lookFor if k == key do located(name) = located(name) :+ s"$f n=${field(line, "n")}"
          if only.forall(_ == zName) then
            read += 1
            val id = s"$zName|$key"
            if !seen.contains(id) then
              seen(id) = f
              val z  = zName.split('.').map(_.toInt).toList
              val ds = symbolFromKey(key)
              val n  = ds.orbs.count(_.i == 1)
              for reg <- genuine(ds, z, strict = true) do cands += ((zName, z, ds, n, reg, f))
      for (name, z, key) <- extraPatterns do
        val zName = z.mkString(".")
        if only.forall(_ == zName) then
          val ds = symbolFromKey(key)
          val id = s"$zName|${DelaneySymbols.canonicalKey(ds).stripSuffix(";")}"
          if !seen.contains(id) then
            seen(id) = s"banked:$name"
            for reg <- genuine(ds, z, strict = true) do
              cands += ((zName, z, ds, ds.orbs.count(_.i == 1), reg, s"banked:$name"))
      log(s"read $read hit lines, ${seen.size} distinct (species, symbol) pairs (banked patterns included)")
      for (name, _) <- lookFor do
        log(f"  banked ${name}%-42s ${
            if located(name).isEmpty then "NOT IN THE HITS FILES" else located(name).distinct.mkString("; ")
          }")
      if isolated then
        log(
          "\nISOLATED AUDIT — banked patterns under their strict designation; vertices with > 1 irregular tile:"
        )
        val pinned = StrictWitnesses.entries.map(e => e.name -> e.regular).toMap
        for (name, z, key) <- extraPatterns ++ StrictWitnesses.entries.map(e => (e.name, e.z, e.key)) do
          val ds = symbolFromKey(key)
          val st = UClass.candidates(ds, z, strict = true).filter: r =>
            val irr = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !r(k) => k }
            UClass.noneForcedRegular(ds, r, irr)
          if st.isEmpty then log(f"  ${name}%-44s not in the strict class")
          else
            val reg = pinned.getOrElse(name, st.maxBy(_.size))
            val bad =
              for
                o  <- ds.orbs if o.i == 1
                cfg = DelaneySymbols.vertexConfigOrbits(ds, o.elements.head).get
                if cfg.count((f, _) => !reg(f)) > 1
              yield cfg.map((f, p) => if reg(f) then p.toString else s"${p}i").mkString("(", ".", ")")
            log(f"  ${name}%-44s ${
                if bad.isEmpty then "ISOLATED-OK" else "FAILS: " + bad.distinct.mkString(", ")
              }")

      val bySpecies = cands.groupBy(_._1)
      log(
        s"\nSTRICT CANDIDATES: ${cands.size} designations over ${cands.map(c => (c._1, c._3.canonicalKey)).distinct.size} symbols"
      )
      for (zName, cs) <- bySpecies.toList.sortBy(_._1) do
        val byN = cs.groupBy(_._4).toList.sortBy(_._1).map((n, g) =>
          s"n=$n: ${g.size} designations / ${g.map(_._3.canonicalKey).distinct.size} symbols"
        )
        log(s"  z=$zName  ${byN.mkString(", ")}")
      Files.writeString(
        outDir.resolve(s"${prefix}strict-candidates.tsv"),
        (s"count=${cands.size}" +: cands.toList.map: (zName, _, ds, n, reg, f) =>
          val irr = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !reg(k) => k }
          s"z=$zName\tn=$n\tchambers=${ds.size}\tregularOrbits=${reg.toList.sorted.mkString(",")}\t" +
            s"irregularOrbits=${irr.mkString(",")}\tsource=$f\tkey=${DelaneySymbols.canonicalKey(ds)}"
        ).mkString("\n")
      )

      // ---- phase 2: closure triage
      val witnesses = collection.mutable.ListBuffer.empty[String]
      var flexible  = 0
      var flexDead  = 0
      var flexOpen  = 0

      /** The verification chain at one exact corner solution: strict-valid embedded development at radius 6,
        * then at radius 10 with species vertex, certified cell, minimal symbol, union decision, convexity and
        * STRICT saturation. Returns the witness row.
        */
      def verify(
          label: String,
          zName: String,
          z: List[Int],
          ds: DSymbol,
          n: Int,
          reg: Set[Int],
          x: Array[Frac],
          f: String,
          tag: String
      ): Option[String] =
        val re6 = TilePatch.seed(ExactDeveloper.develop(ds, x, 6.0), z, strict = true, isolated = isolated)
        if !TilePatch.valid(re6) then
          log(
            s"$label develops (${re6.tiles.length} tiles) but a vertex is strict-illegal or over-wound — DEAD"
          );
          None
        else if !re6.tiles.forall(_.poly.isSimpleCertified) then
          log(s"$label embedding failure at radius 6 — DEAD"); None
        else
          log(s"$label closed, strict-valid at radius 6 (${re6.tiles.length} tiles) — verifying at radius 10")
          val re      = TilePatch.seed(ExactDeveloper.develop(ds, x, 10.0), z, strict = true, isolated = isolated)
          val species = re.interiorWords.values.count(w =>
            w.forall(_.isDefined) && UClass.strictArcLegal(w, z) && w.length == z.length
          )
          if !TilePatch.valid(re) then { log(s"$label INVALID at radius 10 — DEAD"); None }
          else if !re.tiles.forall(_.poly.isSimpleCertified) then {
            log(s"$label EMBEDDING FAILURE at radius 10 — DEAD"); None
          } else if species == 0 then { log(s"$label no species vertex at radius 10 — DEAD"); None }
          else
            val irr                                                 =
              re.tiles.map(_.poly).filter(TilePatch.regularSizeOf(_).isEmpty).groupBy(TilePatch.shapeKey)
            val qs                                                  = pieces(z)
            def unionOf(t: ExactPlane.UnitPolygon): Option[Boolean] =
              if t.areaApprox <= 12.0 then Some(Defusion.regularUnion(t, qs)) else None
            val irrReport                                           = irr.values.map { g =>
              val t    = g.head
              val angs = t.interiorAngles.map(_ * 360 / t.n).sorted.mkString("/")
              f"${t.dirs.length}-gon($angs)A=${t.areaApprox}%.4f union=${unionOf(t).fold("skipped")(_.toString)}"
            }.mkString("; ")
            val essential                                           = irr.nonEmpty && irr.values.forall(g => unionOf(g.head).contains(false))
            val convex                                              = re.tiles.forall(_.poly.interiorAngles.forall(_ < re.n / 2))
            val cell                                                = Periodicity.certifiedCell(re).map { (t1, t2, c) =>
              val min = SymbolExtractor.minimalImage(SymbolExtractor.torusSymbol(re, t1, t2).get)
              (min.size, min.orbs.count(_.i == 1), c.chambers, DelaneySymbols.canonicalKey(min))
            }
            val sat                                                 =
              if re.tiles.length <= 450 && irr.values.forall(_.head.areaApprox <= 12.0)
              then Some(TilePatch.admissible(re).isEmpty)
              else None
            val cellStr                                             = cell.fold("NO CERTIFIED CELL")((c, k, ct, _) => s"minimal C=$c k=$k (cell C_T=$ct)")
            val row                                                 =
              s"z=$zName\tn=$n\tchambers=${ds.size}\tregularOrbits=${reg.toList.sorted.mkString(",")}\t" +
                s"minimal=${cell.fold("?")((c, k, _, _) => s"C=$c,k=$k")}\tconvex=$convex\tstrictSaturated=${sat.fold("skipped")(_.toString)}\t" +
                s"essential=$essential\tpieces=${qs.mkString(",")}\tfamily=$tag\tirregular=$irrReport\tsource=$f\t" +
                s"key=${DelaneySymbols.canonicalKey(ds)}\tminimalKey=${cell.fold("")(_._4)}"
            log(
              s"WITNESS $label $cellStr convex=$convex strictSaturated=${sat.fold("skipped")(_.toString)} essential=$essential $tag irregular: $irrReport"
            )
            Some(row)

      for ((zName, z, ds, n, reg, f), ci) <- cands.zipWithIndex do
        val label = s"[$ci] z=$zName n=$n C=${ds.size} reg=${reg.toList.sorted.mkString(",")} ($f)"
        Try {
          val sys   = MetricLayer.angleSystem(ds)
          val rows  = MetricLayer.designatedRows(ds, reg)
          val basis = MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows))
          if basis.size >= 2 then
            flexible += 1; flexOpen += 1
            log(s"$label FLEXIBLE dim=${basis.size} — not resolved (2-D families are left open)")
          else if basis.size == 1 then
            flexible += 1
            MetricLayer.particularSolution(rows, sys.vars) match
              case None     => log(s"$label flexible but inconsistent — DEAD"); flexDead += 1
              case Some(x0) =>
                val v = basis.head
                window1(x0, v) match
                  case None           => log(s"$label FLEXIBLE dim=1, empty simple window — DEAD"); flexDead += 1
                  case Some((lo, hi)) =>
                    def rAt(t: Double): Double =
                      MetricLayer.maxClosureResidual(
                        ds,
                        Array.tabulate(sys.vars)(j => x0(j).toDouble + t * v(j).toDouble)
                      )
                    val m                      = 8000
                    val delta                  = (hi - lo) / m
                    var minR                   = Double.MaxValue; var minT = lo
                    val roots                  = collection.mutable.ArrayBuffer.empty[Double]
                    for i <- 0 to m do
                      val t = lo + i * delta
                      val r = rAt(t)
                      if r < minR then { minR = r; minT = t }
                      if r < 1e-9 then roots += t
                    val l                      = lipschitz(ds, sys.corner, v)
                    if roots.size > m / 2 && x0.indices.exists(j => v(j).num == 0 && x0(j).num == x0(j).den)
                    then
                      log(
                        s"$label FLEXIBLE dim=1 identically closed but a corner is straight along the whole family — DEAD"
                      )
                      flexDead += 1
                    else if roots.size > m / 2 then
                      // identically closed: realize at the FIRST small-denominator rational in the middle 60 % of
                      // the window that passes the corner guards — the exact developer works in Q(zeta_N) with N
                      // growing with the denominator, and a three-decimal parameter (the first attempt) is out of
                      // reach; lattice points (a straight corner) are skipped, not reported open
                      val (a, b) = (lo + 0.2 * (hi - lo), hi - 0.2 * (hi - lo))
                      val tf     = (1 to 200).iterator
                        .flatMap(q =>
                          (math.ceil(a * q).toLong to math.floor(b * q).toLong).map(pp => Frac(pp, q))
                        )
                        .find(t => honest(Array.tabulate(sys.vars)(j => x0(j) + t * v(j))))
                        .getOrElse(Frac(0, 1))
                      val xf     = Array.tabulate(sys.vars)(j => x0(j) + tf * v(j))
                      log(
                        f"$label FLEXIBLE dim=1 window=($lo%.4f,$hi%.4f) identically closed — realizing at t=$tf"
                      )
                      if honest(xf) && MetricLayer.maxClosureResidual(ds, xf.map(_.toDouble)) < 1e-9 then
                        verify(label, zName, z, ds, n, reg, xf, f, s"flexible(t=$tf)").foreach(witnesses += _)
                      else { log(s"$label mid-window realization fails the guards — OPEN"); flexOpen += 1 }
                    else if roots.nonEmpty then
                      val t0   = roots.head
                      val cand = (1 to 60).flatMap(q =>
                        Some(Frac(math.round(t0 * q), q)).filter(fr => math.abs(fr.toDouble - t0) < 2 * delta)
                      ).headOption
                      cand match
                        case Some(tf) =>
                          val xf = Array.tabulate(sys.vars)(j => x0(j) + tf * v(j))
                          val r  = MetricLayer.maxClosureResidual(ds, xf.map(_.toDouble))
                          log(
                            f"$label FLEXIBLE dim=1 isolated root near t=$t0%.6f, rational t=$tf residual=$r%.2e"
                          )
                          if r < 1e-9 && honest(xf) then
                            verify(label, zName, z, ds, n, reg, xf, f, s"root(t=$tf)").foreach(witnesses += _)
                          else if r < 1e-9 then
                            log(
                              s"$label closes only at the degenerate parameter t=$tf (a straight or vanishing corner) — DEAD"
                            )
                            flexDead += 1
                          else
                            log(s"$label rational root does not close exactly — OPEN")
                            flexOpen += 1
                        case None     =>
                          log(
                            f"$label FLEXIBLE dim=1 isolated root near t=$t0%.6f with no small rational — OPEN"
                          );
                          flexOpen += 1
                    else if minR > l * delta / 2 then
                      log(
                        f"$label FLEXIBLE dim=1, closure residual >= $minR%.2e on the window (certified) — DEAD"
                      )
                      flexDead += 1
                    else
                      // positive but the global certificate is short: refine around the minimum with a finer step
                      val fine = 4000
                      val d2   = 2 * delta / fine
                      val loc  = (0 to fine).map(i => rAt(minT - delta + i * d2)).min
                      if loc > l * d2 / 2 then
                        log(
                          f"$label FLEXIBLE dim=1, closure residual >= $loc%.2e after local refinement (certified) — DEAD"
                        )
                        flexDead += 1
                      else
                        // a near-root the grid straddles: match a small rational and decide there exactly
                        val tMin = (0 to fine).minBy(i => rAt(minT - delta + i * d2))
                        val tR   = minT - delta + tMin * d2
                        val near = (1 to 60).flatMap(q =>
                          Some(Frac(math.round(tR * q), q)).filter(fr => math.abs(fr.toDouble - tR) < 4 * d2)
                        ).headOption
                        near.map(tf => (tf, Array.tabulate(sys.vars)(j => x0(j) + tf * v(j)))) match
                          case Some((tf, xf))
                              if MetricLayer.maxClosureResidual(ds, xf.map(_.toDouble)) < 1e-9 =>
                            if honest(xf) then
                              log(f"$label FLEXIBLE dim=1 near-root resolved at rational t=$tf — realizing")
                              verify(label, zName, z, ds, n, reg, xf, f, s"root(t=$tf)").foreach(witnesses +=
                                _)
                            else
                              log(
                                s"$label closes only at the degenerate parameter t=$tf (a straight or vanishing corner) — DEAD"
                              )
                              flexDead += 1
                          case _ =>
                            log(
                              f"$label FLEXIBLE dim=1 minR=$loc%.2e near t=$tR%.6f, no small rational closes — uncertified, OPEN"
                            )
                            flexOpen += 1
          else
            MetricLayer.particularSolution(rows, sys.vars) match
              case None    => log(s"$label inconsistent closure rows — DEAD")
              case Some(x) =>
                val resid = MetricLayer.maxClosureResidual(ds, x.map(_.toDouble))
                if !honest(x) then log(s"$label rigid, degenerate corner — DEAD")
                else if resid >= 1e-9 then log(f"$label rigid, closure residual $resid%.3e — DEAD")
                else verify(label, zName, z, ds, n, reg, x, f, "rigid").foreach(witnesses += _)
        }.failed.foreach(e => log(s"$label ERROR ${e.getClass.getSimpleName}: ${e.getMessage}"))
      log(
        s"\nTRIAGE: ${witnesses.size} strict witness designation(s); $flexible flexible designation(s): $flexDead dead, $flexOpen open"
      )
      Files.writeString(
        outDir.resolve(s"${prefix}strict-witnesses.tsv"),
        (s"count=${witnesses.size}" +: witnesses.toList).mkString("\n")
      )
      log(
        s"wrote ${outDir.resolve(s"${prefix}strict-candidates.tsv")} and ${outDir.resolve(s"${prefix}strict-witnesses.tsv")}"
      )
      logW.foreach(_.close())
