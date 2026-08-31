package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import io.github.scala_tessella.research_core.TilePatch.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** ENDPOINT SAMPLER (enable with `-Ddefusion.endpoints`) — the constructive path's first harvest: seed the
  * unsaturated campaign patterns ((3.4.4.6) 22-chamber candidates, (3.3.4.12) fusion-28 pair), exhaust each
  * under several move-ordering strategies, and report the saturated endpoint censuses. Distinct censuses
  * across strategies = a non-confluence exhibit in the exact engine; identical ones = evidence toward
  * endpoint uniqueness.
  *
  * Sampler evidence, not proof: endpoints are finite-patch observations to be verified at symbol level before
  * banking.
  */
class DefusionEndpointProbe extends AnyFlatSpec with Matchers:

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
    val reg      = unforced.maxBy(_.size)
    val rows     = MetricLayer.designatedRows(ds, reg)
    MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows)).size shouldBe 0
    MetricLayer.particularSolution(rows, sys.vars).get

  /** One irregular congruence class of a state: a representative polygon and instance count. */
  private def census(s: State): Vector[(UnitPolygon, Int)] =
    s.tiles
      .filter(t => regularSizeOf(t.poly).isEmpty)
      .groupBy(t => shapeKey(t.poly))
      .toVector
      .map((_, ts) => (ts.head.poly, ts.length))
      .sortBy((p, _) => (p.dirs.length, p.interiorAngles.sorted.mkString(",")))

  private def show(cen: Vector[(UnitPolygon, Int)]): String =
    cen
      .map((p, c) =>
        f"${p.dirs.length}-gon×$c ${p.interiorAngles.sorted.mkString("(", ",", ")")} A≈${p.areaApprox}%.4f"
      )
      .mkString("; ")

  /** Sign of meanArea(a) − meanArea(b) over the irregular CLASSES (unweighted), EXACT via cross-multiplied
    * shoelace elements.
    */
  private def meanAreaCompare(a: Vector[UnitPolygon], b: Vector[UnitPolygon]): Int =
    val (ta, tb) = (a.map(_.doubleArea).reduce(_ + _), b.map(_.doubleArea).reduce(_ + _))
    (ta.scaled(b.length) - tb.scaled(a.length)).imSign

  private val targets = List(
    ("(3.4.4.6) 22ch cand1", 10.0),
    ("(3.4.4.6) 22ch cand2", 10.0),
    ("(3.4.4.6) 22ch cand3", 10.0),
    ("(3.3.4.12) fusion28 #1", 10.0),
    ("(3.3.4.12) fusion28 #2", 10.0)
  )

  private val strategies: List[(String, Vector[Move] => Move)] = List(
    ("first", _.head),
    ("last", _.last),
    ("mid", ms => ms(ms.length / 2)),
    ("bigQ", ms => ms.maxBy(_.q)),  // biggest-piece-first: the maximal-exhaustion candidate
    ("smallQ", ms => ms.minBy(_.q)) // smallest-piece-first
  )

  /** Plain SVG of a state, exact vertices projected once — irregular tiles highlighted. */
  private def writeSvg(state: State, file: java.nio.file.Path): Unit =
    val polys            = state.tiles.map(t => (t.vertices.map(_.approx), regularSizeOf(t.poly)))
    val xs               = polys.flatMap(_._1.map(_._1))
    val ys               = polys.flatMap(_._1.map(_._2))
    val (x0, y0, x1, y1) = (xs.min, ys.min, xs.max, ys.max)
    val s                = 40.0
    val body             = polys
      .map: (pts, reg) =>
        val fill                  = reg match
          case Some(3) => "#d8e8d0"
          case Some(4) => "#ccdaea"
          case Some(6) => "#eee3cc"
          case Some(_) => "#e2e2e2"
          case None    => "#e8a0a0"
        def r2(v: Double): Double = math.round(v * 100) / 100.0 // Double.toString: locale-safe
        val p                     = pts.map((x, y) => s"${r2((x - x0) * s + 10)},${r2((y1 - y) * s + 10)}").mkString(" ")
        s"""<polygon points="$p" fill="$fill" stroke="#333" stroke-width="0.7"/>"""
      .mkString("\n")
    java.nio.file.Files.createDirectories(file.getParent)
    java.nio.file.Files.writeString(
      file,
      s"""<svg xmlns="http://www.w3.org/2000/svg" width="${((x1 - x0) * s + 20).toInt}" height="${((y1 - y0) *
          s + 20).toInt}">""" +
        "\n" + body + "\n</svg>\n"
    )

  private def slug(name: String): String =
    name.replaceAll("[^A-Za-z0-9]+", "-").stripPrefix("-").stripSuffix("-")

  "the unsaturated patterns" should "exhaust to saturated endpoints (enable with -Ddefusion.endpoints)" in:
    assume(sys.props.contains("defusion.endpoints"), "endpoint sampler — enable with -Ddefusion.endpoints")
    val emit         = sys.props.contains("defusion.artifacts")
    val atlasDir     = java.nio.file.Path.of("atlas", "defusion-endpoints")
    val minimalKeys  = collection.mutable.LinkedHashMap.empty[String, String]
    val artifactRows = collection.mutable.ArrayBuffer.empty[String]
    for (name, radius) <- targets do
      val (_, z, key) = SaturationProbe.entries.find(_._1 == name).get
      val ds          = symbolFromKey(key)
      val t0          = System.nanoTime
      val start       = seed(ExactDeveloper.develop(ds, rigidPoint(ds, z), radius), z)
      withClue(s"$name seed validity: ")(valid(start) shouldBe true)
      println(f"== $name  z=${z.mkString("(", ".", ")")}  radius=$radius  tiles=${start.tiles.length}  " +
        f"interior=${start.interiorWords.size}/${start.cornersByVertex.size}  " +
        f"[develop ${(System.nanoTime - t0) / 1e9}%.1fs]")
      println(s"   seed irregular: ${show(census(start))}")
      if emit then writeSvg(start, atlasDir.resolve(s"${slug(name)}-seed.svg"))
      val outcomes    = strategies.map: (sname, pick) =>
        val t1          = System.nanoTime
        val (end, path) = exhaust(start, maxMoves = 200, pick = pick)
        withClue(s"$name/$sname endpoint validity: ")(valid(end) shouldBe true)
        val cen         = census(end)
        println(f"   [$sname%-6s] moves=${path.length} path=${path
            .map(m => s"${m.shape.length}-${m.kind}-${m.q}q${m.a}")
            .mkString(",")} [${(System.nanoTime - t1) / 1e9}%.1fs]")
        println(s"            endpoint irregular: ${show(cen)}")
        // identity WITHOUT counts (boundary-dependent); keep class representatives + the path
        (cen.map((p, _) => (p.dirs.length, p.interiorAngles.sorted)), cen.map(_._1), path)
      val distinct    = outcomes.distinctBy(_._1)
      println(s"   distinct endpoint profiles across strategies: ${distinct.length}")
      // per-endpoint translation-cell census on a LARGER patch: replay the recorded path on a
      // radius+4 seed (cheap — no move search), then quotient with exact certification
      val bigSeed     = seed(ExactDeveloper.develop(ds, rigidPoint(ds, z), radius + 4.0), z)
      val censuses    = distinct.zipWithIndex.map: (o, i) =>
        o._3.foldLeft(Option(bigSeed))((s, mv) => s.flatMap(applyMove(_, mv))) match
          case None         =>
            println(s"   endpoint$i: path replay failed on the census patch")
            None
          case Some(bigEnd) =>
            Periodicity.certifiedCell(bigEnd) match
              case None              =>
                println(s"   endpoint$i: no CERTIFIED cell within candidate budget")
                None
              case Some((t1, t2, c)) =>
                val a = c.cellArea.approx._2
                println(f"   endpoint$i cell: C_T=${c.chambers} k_T=${c.vertexClasses} " +
                  f"tileClasses=${c.tileClasses.length} cellArea≈$a%.4f density≈${c.chambers / a}%.4f " +
                  f"areaCertified=${c.areaCertified}")
                SymbolExtractor.torusSymbol(bigEnd, t1, t2) match
                  case None        =>
                    println(s"   endpoint$i symbol: extraction failed (no fully-checkable rep)")
                  case Some(torus) =>
                    import DelaneySymbols.canonicalKey
                    val min      = SymbolExtractor.minimalImage(torus)
                    val (cf, kf) = (min.size, min.orbs.count(_.i == 1))
                    val ck       = min.canonicalKey
                    minimalKeys += (s"$name/endpoint$i" -> ck)
                    println(s"   endpoint$i symbol: C=$cf k=$kf minimalKey#${ck.hashCode.toHexString}")
                    if emit then
                      val irr = o._1.map((e, a) => s"$e-gon${a.mkString("(", ",", ")")}").mkString("; ")
                      artifactRows += List(
                        name,
                        s"endpoint$i",
                        cf,
                        kf,
                        c.chambers,
                        c.cellArea.approx._2.toString, // Double.toString: locale-safe decimals
                        irr,
                        SymbolExtractor.entryKey(min)
                      ).mkString("\t")
                      o._3.foldLeft(Option(start))((s, mv) => s.flatMap(applyMove(_, mv))).foreach: small =>
                        writeSvg(small, atlasDir.resolve(s"${slug(name)}-endpoint$i.svg"))
                Some(c)
      // the lexicographic rule: mean irregular class area first, chamber DENSITY as tiebreak
      for
        i <- distinct.indices
        j <- i + 1 until distinct.length
      do
        val sign = meanAreaCompare(distinct(i)._2, distinct(j)._2)
        if sign != 0 then
          println(s"   meanIrregularArea: endpoint$i ${if sign < 0 then "<" else ">"} endpoint$j")
        else
          val verdict = (censuses(i), censuses(j)) match
            case (Some(a), Some(b)) =>
              Periodicity.densityCompare((a.chambers, a.cellArea), (b.chambers, b.cellArea)) match
                case 0 => "chamberDensity also TIES — rule undecided"
                case s => s"chamberDensity picks endpoint${if s < 0 then i else j}"
            case _                  => "chamber tiebreak unavailable (census uncertified)"
          println(s"   meanIrregularArea: endpoint$i == endpoint$j (EXACT TIE) → $verdict")
    println("== isomorphism classes across extracted minimal symbols ==")
    val iso          = minimalKeys.groupBy(_._2).values.filter(_.sizeIs > 1)
    if iso.isEmpty then println("   all extracted symbols pairwise distinct")
    else iso.foreach(g => println(s"   ISOMORPHIC: ${g.keys.mkString(" == ")}"))
    if emit then
      val certsDir = java.nio.file.Path.of("certs", "defusion-endpoints")
      java.nio.file.Files.createDirectories(certsDir)
      val header   = "system\tendpoint\tC\tk\tC_T\tcellArea\tirregular\tminimalEntryKey"
      java.nio.file.Files
        .writeString(certsDir.resolve("endpoints.tsv"), (header +: artifactRows).mkString("", "\n", "\n"))
      println(s"   wrote certs/defusion-endpoints/endpoints.tsv (${artifactRows.length} rows) " +
        "and atlas/defusion-endpoints/*.svg")
