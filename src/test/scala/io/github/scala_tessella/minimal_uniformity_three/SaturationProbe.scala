package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** SATURATION audit (`-Duclass.sat`) — the machine check of the paper's condition (4), the exhaustion
  * principle, over every witness and pattern of the paper.
  *
  * A tiling T ∈ U(z) is SATURATED if no refinement splitting one regular polygon off an irregular tile — an
  * exact corner fill or an edge notch, meeting the boundary edge-to-edge — leaves every affected vertex's
  * SPLICED cyclic word a cyclic subword of z (condition (3) closed under refinement; no new primitive).
  * Soundness of the depth-1 certificate: an inserted regular polygon is edge-adjacent to the regular tiles it
  * touches, contiguous regular runs are permanent under further refinement (only irregular tiles are ever
  * split), and an inserted q outside z's alphabet makes its words non-subwords outright — so q ranges over
  * z's letters only, and a depth-1 word violation refutes every deeper refinement through that insertion.
  *
  * THE FULL-WORD TEST IS THE DEFINITION: adjacent-pair testing is provably too weak — the motivating example
  * is the decagon at the (3.10.15) 36-gon's 144° corner, whose far vertex reads (10.3.10): both pairs are
  * legal arcs, the word is not (in z, 3 neighbours 10 AND 15, never two 10s).
  *
  * DIRECTIONALITY: where the inserted polygon's side within the replaced arc is ambiguous, the checker
  * accepts if EITHER orientation yields a legal word — permissive, so a SATURATED verdict (no legal insertion
  * anywhere) is sound; an UNSATURATED verdict lists candidate insertions to be confirmed by eye or geometry
  * (for the known case, P2's triangle-at-60°, the insertion is genuinely legal: it is the constructive
  * de-fusion P2 → P1).
  */
class SaturationProbe extends AnyFlatSpec with Matchers:

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

  private def faceCornerChambers(ds: DSymbol, d0: Int): Vector[Int] =
    val frag = collection.mutable.ArrayBuffer.empty[Int]
    var cur  = d0
    var go   = true
    while go do
      frag += cur
      cur = ds.get(1, ds.get(0, cur))
      if cur == d0 then go = false
    frag.toVector

  private def particular(rows: Vector[(Array[Frac], Frac)], vars: Int): Option[Array[Frac]] =
    val m          = rows.map((c, r) => c.clone :+ r).toArray
    val nr         = m.length
    val pivotOfCol = Array.fill(vars)(-1)
    var r          = 0
    var c          = 0
    while r < nr && c < vars do
      var pr = r
      while pr < nr && m(pr)(c).isZero do pr += 1
      if pr < nr then
        val t   = m(pr); m(pr) = m(r); m(r) = t
        val inv = m(r)(c)
        for j <- c to vars do m(r)(j) = m(r)(j) / inv
        for i <- 0 until nr if i != r && !m(i)(c).isZero do
          val f = m(i)(c)
          for j <- c to vars do m(i)(j) = m(i)(j) - f * m(r)(j)
        pivotOfCol(c) = r
        r += 1
      c += 1
    if m.exists(row => row.take(vars).forall(_.isZero) && !row(vars).isZero) then None
    else
      val x = Array.fill(vars)(Frac(0, 1))
      for pc <- 0 until vars if pivotOfCol(pc) >= 0 do x(pc) = m(pivotOfCol(pc))(vars)
      Some(x)

  /** Angle sums + regular pins + linear closure equivalents; unhandled categories allowed (the rigid
    * fall-through of `UClassK4ExistenceProbe`) — callers must check dimension 0.
    */
  private def affineRows(ds: DSymbol, reg: Set[Int]): Vector[(Array[Frac], Frac)] =
    val sys                            = MetricLayer.angleSystem(ds)
    val corner                         = sys.corner
    val extra                          = Vector.newBuilder[(Array[Frac], Frac)]
    def eq(c1: Int, c2: Int): Unit     =
      if c1 != c2 then
        val row = Array.fill(sys.vars)(Frac(0, 1))
        row(c1) = Frac(1, 1); row(c2) = Frac(-1, 1)
        extra += ((row, Frac(0, 1)))
    def pin(c: Int, value: Frac): Unit =
      val row = Array.fill(sys.vars)(Frac(0, 1))
      row(c) = Frac(1, 1)
      extra += ((row, value))
    for (o, k) <- ds.orbs.zipWithIndex if o.i == 0 do
      val d0  = o.elements.head
      val p   = ds.m(0, 1, d0)
      val v   = ds.v(0, 1, d0)
      val seq = faceCornerChambers(ds, d0).map(corner)
      val ext = Array.tabulate(p)(i => seq(i % seq.length))
      if reg(k) then ext.foreach(c => pin(c, Frac.make(p - 2L, p)))
      else if p == 3 then ext.foreach(c => pin(c, Frac(1, 3)))
      else if p == 4 then { eq(ext(0), ext(2)); eq(ext(1), ext(3)) }
      else () // v >= 2 identically closed, or unhandled — rigid fall-through
    sys.rows ++ extra.result()

  /** Is the CYCLIC word `w` a contiguous arc of the cyclic word `z`, up to rotation and reflection? Both w
    * and z are cyclic, so BOTH must be tried in all rotations (2026-08-17 fix: the fixed-rotation version
    * wrongly rejected legal words — (6,4,3) IS an arc of (3,4,4,6) via its rotation (3,6,4) against
    * z-reversed — making the earlier saturation REJECTIONS too strict; this matches `UClass.cyclicSubset`
    * exactly).
    */
  private def cyclicSubword(w: Vector[Int], z: Vector[Int]): Boolean =
    w.length <= z.length && {
      val ws    = (0 until math.max(1, w.length)).map(r => w.drop(r) ++ w.take(r))
      val cands = (0 until z.length).flatMap: r =>
        val rot = Vector.tabulate(z.length)(i => z((r + i) % z.length))
        List(rot, rot.reverse)
      cands.exists(c => ws.contains(c.take(w.length)))
    }

  /** The geometric cyclic word around the vertex through chamber `d`: per face visit, (face-orbit index,
    * size); the quotient fragment repeated to the full vertex degree m₁₂.
    */
  private def vertexWord(ds: DSymbol, d: Int): Vector[(Int, Int)] =
    val frag = collection.mutable.ArrayBuffer.empty[(Int, Int)]
    var cur  = d
    var go   = true
    while go do
      frag += ((ds.orbitIndex(1)(cur), ds.m(0, 1, cur)))
      val nxt = ds.get(2, ds.get(1, cur))
      cur = if nxt == 0 then cur else nxt
      if cur == d then go = false
    val m12  = ds.m(1, 2, d)
    Vector.fill(m12 / frag.length)(frag.toVector).flatten

  /** Word test at the vertex through the CORNER CHAMBER `v0` whose σ₀-edge is the edge the inserted piece is
    * flush against. The walk from `v0` puts our face at index 0 and — since the walk's final step returns
    * through σ₂ of `v0` — the face across `v0`'s own edge at index m−1; the inserted `q` is therefore the
    * FIRST element of the replaced arc (adjacent to the index-(m−1) face), with the irregular sliver after it
    * unless `sliverZero`. Orientation is exact, not permissive: the verdicts are sound in both directions.
    */
  private def wordLegal(
      ds: DSymbol,
      v0: Int,
      q: Int,
      sliverZero: Boolean,
      z: Vector[Int],
      reg: Set[Int]
  ): Boolean =
    val word    = vertexWord(ds, v0)
    val arc     = if sliverZero then Vector(Some(q)) else Vector(Some(q), None)
    val letters = word.zipWithIndex.flatMap: (fs, i) =>
      if i == 0 then arc
      else Vector(if reg(fs._1) then Some(fs._2) else None)
    cyclicSubword(letters.flatten, z)

  it should "audit saturation over the witnesses and patterns (enable with -Duclass.sat)" in:
    assume(sys.props.contains("uclass.sat"), "saturation audit — enable with -Duclass.sat")
    val verdicts = collection.mutable.Map.empty[String, Boolean]
    for (name, z, key) <- SaturationProbe.entries do
      val ds       = symbolFromKey(key)
      val sys      = MetricLayer.angleSystem(ds)
      val corner   = sys.corner
      val unforced = UClass
        .candidates(ds, z)
        .filter: r =>
          val irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !r(k) => k }
          UClass.noneForcedRegular(ds, r, irregular)
      withClue(s"$name designations: ")(unforced.nonEmpty shouldBe true)
      val reg      = unforced.maxBy(_.size)
      val rows     = affineRows(ds, reg)
      withClue(s"$name rigidity: "):
        MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, corner, rows)).size shouldBe 0
      val x        = particular(rows, sys.vars).get
      val zc       = z.toVector

      def angleAt(ch: Int): Frac = x(corner(ch))
      def lt(a: Frac, b: Frac)   = a.num * b.den < b.num * a.den
      val legal                  = collection.mutable.LinkedHashSet.empty[String]
      var tested                 = 0
      for (o, k) <- ds.orbs.zipWithIndex if o.i == 0 && !reg(k) do
        for d <- faceCornerChambers(ds, o.elements.head) do
          val alpha = angleAt(d)
          val farA  = ds.get(0, d)
          val farB  = ds.get(0, ds.get(1, d))
          for q <- z.distinct do
            val beta = Frac.make(q - 2L, q)
            tested += 1
            if beta == alpha then
              val okFar    = List(farA, farB).forall: f =>
                val gamma = angleAt(f)
                !lt(gamma, beta) && wordLegal(ds, f, q, sliverZero = gamma == beta, zc, reg)
              val okCorner = wordLegal(ds, d, q, sliverZero = true, zc, reg)
              if okFar && okCorner then
                legal += s"EXACT $q-gon at ${pretty(alpha)} corner of orbit $k"
            else if lt(beta, alpha) then
              val gamma = angleAt(farA)
              if lt(beta, gamma) then
                val ok = List(d, farA).forall(v0 => wordLegal(ds, v0, q, sliverZero = false, zc, reg))
                if ok then legal += s"NOTCH $q-gon on ${pretty(alpha)}/${pretty(gamma)} edge of orbit $k"
      val saturated              = legal.isEmpty
      println(s"$name: insertions tested=$tested legal=${legal.size} -> " +
        (if saturated then "SATURATED" else "UNSATURATED"))
      legal.foreach(l => println(s"  $l"))
      verdicts(name) = saturated

    // teeth: the three patterns must reproduce the geometric (Python) verdicts of 2026-08-17
    verdicts("(3.10.15) pattern k=5") shouldBe true
    verdicts("(3.7.42) P1 k=10") shouldBe true
    verdicts("(3.7.42) P2 fused k=6") shouldBe false

  private def pretty(a: Frac): String = f"${180.0 * a.num.toDouble / a.den.toDouble}%.1f°"

object SaturationProbe:
  /** (name, species z, canonical key) — the four uniformity-3 witnesses (paper appendix), the two
    * uniformity-4 witnesses, the (3.10.15) uniformity-5 pattern, and the two (3.7.42) patterns.
    */
  val entries: List[(String, List[Int], String)] = List(
    (
      // the valence-2 k = 4 (4.5.20) candidate surfaced by the 2026-08-29 v2 sweep: rigid, closing
      // exactly in zeta_20, simple, honest, with one irregular 16-gon and two valence-2 vertices.
      // Convex-dead by a reflex corner, so its standing in U+ turns entirely on this audit.
      "(4.5.20) v2 k=4 candidate",
      List(4, 5, 20),
      "1,2,3,20|3;4,1,5,20|3;3,6,1,4|3;2,7,8,20|3;8,9,2,5|3;6,3,9,4|3;10,4,11,20|3;5,12,4,5|3;9,5,6,5|3;7,10,13,20|2;13,14,7,16|3;15,8,14,5|3;11,13,10,16|2;16,11,12,16|3;12,15,16,5|2;14,16,15,16|2"
    ),
    (
      "(3.8.24) w k=3",
      List(3, 8, 24),
      "1,2,3,12|3;4,1,5,12|3;3,6,1,24|3;2,7,8,12|3;8,9,2,3|3;10,3,9,24|3;7,4,11,12|3;5,12,4,3|3;" +
        "13,5,6,3|3;6,14,13,24|3;11,15,7,8|3;16,8,15,3|3;9,16,10,3|3;14,10,17,24|3;18,11,12,8|3;" +
        "12,13,18,3|3;17,18,14,8|3;15,17,16,8|3"
    ),
    (
      "(3.4.3.12) w k=3",
      List(3, 4, 3, 12),
      "2,1,3,12|4;1,2,4,12|4;4,5,1,3|4;3,6,2,3|4;7,3,8,3|4;9,4,10,3|4;5,9,11,3|4;11,8,5,4|4;" +
        "6,7,12,3|4;12,10,6,6|4;8,11,7,4|4;10,12,9,6|4"
    ),
    (
      "(3.4.4.6) w k=3",
      List(3, 4, 4, 6),
      "1,2,3,3|4;4,1,5,3|4;3,6,1,6|4;2,4,7,3|4;7,8,2,4|4;9,3,10,6|4;5,11,4,4|4;12,5,12,4|4;" +
        "6,9,13,6|4;13,12,6,4|4;14,7,15,4|4;8,10,8,4|4;10,14,9,4|4;11,13,16,4|4;16,15,11,4|4;" +
        "15,16,14,4|4"
    ),
    (
      "(3.3.6.6) w k=3",
      List(3, 3, 6, 6),
      "1,2,1,3|4;3,1,4,3|4;2,3,5,3|4;5,6,2,6|4;4,7,3,6|4;6,4,6,6|4;8,5,9,6|4;7,10,11,6|3;" +
        "11,9,7,4|4;10,8,10,6|3;9,11,8,4|3"
    ),
    (
      "(3.9.18) w k=4",
      List(3, 9, 18),
      "2,3,4,18|3;1,5,6,18|3;7,1,8,18|3;6,9,1,3|3;10,2,11,18|3;4,12,2,3|3;3,10,13,18|3;13,14,3,6|3;" +
        "15,4,14,3|3;5,7,16,18|3;16,17,5,9|3;18,6,17,3|3;8,19,7,6|3;20,8,9,6|3;9,18,20,3|3;" +
        "11,21,10,9|3;22,11,12,9|3;12,15,22,3|3;23,13,21,6|3;14,23,15,6|3;24,16,19,9|3;17,24,18,9|3;" +
        "19,20,24,6|3;21,22,23,9|3"
    ),
    (
      "(5.5.10) w k=4",
      List(5, 5, 10),
      "1,2,3,10|3;4,1,5,10|3;3,6,1,6|3;2,7,8,10|3;8,9,2,5|3;10,3,9,6|3;11,4,12,10|3;5,13,4,5|3;" +
        "14,5,6,5|3;6,10,14,6|3;7,11,15,10|3;15,16,7,5|3;16,8,16,5|3;9,17,10,5|3;12,18,11,5|3;" +
        "13,12,13,5|3;18,14,17,5|3;17,15,18,5|3"
    ),
    (
      "(3.10.15) pattern k=5",
      List(3, 10, 15),
      "4,11,14,36|3;20,5,15,3|3;3,15,18,15|3;1,16,12,36|3;7,2,13,3|3;19,9,11,15|3;5,12,10,3|3;" +
        "16,8,22,36|2;15,6,20,15|3;13,17,7,10|3;21,1,6,36|3;14,7,4,3|3;10,18,5,10|3;12,20,1,3|3;" +
        "9,3,2,15|3;8,4,17,36|3;22,10,16,10|3;18,13,3,10|3;6,19,21,15|2;2,14,9,3|3;11,21,19,36|2;" +
        "17,22,8,10|2"
    ),
    (
      "(3.7.42) P1 k=10",
      List(3, 7, 42),
      "29,32,58,7|3;7,57,4,42|3;38,53,54,3|3;5,45,2,7|3;4,26,7,7|3;11,38,45,3|3;2,47,5,42|3;" +
        "30,50,12,42|3;27,24,20,3|3;37,55,34,3|3;6,60,23,3|3;19,40,8,7|3;56,52,53,6|3;59,20,33,12|3;" +
        "50,33,24,42|3;26,29,55,7|3;39,37,22,3|3;58,34,29,12|3;12,49,30,7|3;25,14,9,12|3;" +
        "55,39,26,3|3;47,44,17,42|3;45,46,11,7|3;43,9,15,3|3;20,58,27,12|3;16,5,21,7|3;9,35,25,3|3;" +
        "41,56,46,6|3;1,16,18,7|3;8,42,19,42|3;52,41,42,6|3;40,1,35,7|3;44,15,14,42|3;51,18,10,12|3;" +
        "48,27,32,3|3;42,54,52,42|3;10,17,51,3|3;3,6,57,3|3;17,21,47,3|3;32,12,48,7|3;28,31,49,6|3;" +
        "36,30,31,42|3;24,48,50,3|3;33,22,59,42|3;23,4,6,7|3;49,23,28,7|3;22,7,39,42|3;35,43,40,3|3;" +
        "46,19,41,7|3;15,8,43,42|3;34,59,37,12|3;31,13,36,6|3;60,3,13,3|3;57,36,3,42|3;21,10,16,3|3;" +
        "13,28,60,6|3;54,2,38,42|3;18,25,1,12|3;14,51,44,12|3;53,11,56,3|3"
    ),
    (
      "(3.7.42) P2 fused k=6",
      List(3, 7, 42),
      "19,28,8,3|3;6,26,27,7|3;13,6,20,7|3;8,7,19,12|3;21,9,26,3|3;2,3,17,7|3;7,4,12,12|3;" +
        "4,23,1,12|3;28,5,25,3|3;18,20,15,8|3;26,12,21,7|3;12,11,7,7|3;3,13,24,7|2;14,16,23,42|3;" +
        "22,17,10,42|3;25,14,28,42|3;27,15,6,42|3;10,18,22,8|2;1,21,4,3|3;24,10,3,8|3;5,19,11,3|3;" +
        "15,22,18,42|2;23,8,14,12|3;20,24,13,8|2;16,27,9,42|3;11,2,5,7|3;17,25,2,42|3;9,1,16,3|3"
    ),
    (
      "(3.4.4.6) 22ch cand1",
      List(3, 4, 4, 6),
      "2,3,2,4|4;1,4,1,4|4;5,1,6,4|4;7,2,8,4|4;3,9,10,4|3;10,11,3,3|4;4,12,13,4|4;13,14,4,6|4;12,5,15,4|3;6,16,5,3|3;17,6,14,3|4;9,7,18,4|4;8,19,7,6|4;19,8,11,6|4;18,20,9,12|3;21,10,20,3|3;11,21,19,3|4;15,22,12,12|4;14,13,17,6|4;22,15,16,12|3;16,17,22,3|4;20,18,21,12|4;"
    ),
    (
      "(3.4.4.6) 22ch cand2",
      List(3, 4, 4, 6),
      "1,2,3,4|4;2,1,4,4|4;3,5,1,4|4;4,6,2,6|4;7,3,8,4|4;9,4,10,6|4;5,11,12,4|4;12,10,5,3|4;6,13,14,6|3;14,8,6,3|4;11,7,15,4|4;8,16,7,3|4;17,9,18,6|3;10,19,9,3|3;15,17,11,6|4;19,12,20,3|4;13,15,21,6|4;21,22,13,8|3;16,14,22,3|3;22,21,16,8|4;18,20,17,8|4;20,18,19,8|3;"
    ),
    (
      "(3.4.4.6) 22ch cand3",
      List(3, 4, 4, 6),
      "1,2,3,12|3;4,1,5,12|3;3,6,1,4|3;2,7,8,12|4;8,9,2,3|3;10,3,9,4|3;7,4,11,12|4;5,12,4,3|4;13,5,6,3|3;6,14,13,4|4;11,15,7,4|4;16,8,17,3|4;9,16,10,3|4;14,10,18,4|4;19,11,20,4|4;12,13,21,3|4;21,20,12,6|4;18,19,14,4|4;15,18,22,4|4;22,17,15,6|4;17,22,16,6|4;20,21,19,6|4;"
    ),
    (
      "(3.3.4.12) fusion28 #1",
      List(3, 3, 4, 12),
      "1,2,3,12|4;4,1,5,12|4;3,6,1,4|4;2,7,8,12|3;8,9,2,12|4;10,3,11,4|4;12,4,13,12|3;5,14,4,12|3;15,5,16,12|4;6,17,18,4|4;18,16,6,3|4;7,19,20,12|4;20,21,7,3|3;22,8,21,12|3;9,22,23,12|3;23,11,9,3|4;17,10,19,4|4;11,24,10,3|4;19,12,17,12|4;13,25,12,3|4;26,13,14,3|3;14,15,26,12|3;16,27,15,3|3;27,18,25,3|4;28,20,24,3|4;21,28,22,3|3;24,23,28,3|3;25,26,27,3|3;"
    ),
    (
      "(3.3.4.12) fusion28 #2",
      List(3, 3, 4, 12),
      "2,3,4,12|3;1,5,6,12|3;7,1,8,12|3;6,9,1,3|3;10,2,11,12|3;4,12,2,3|3;3,10,13,12|4;13,14,3,12|3;15,4,14,3|3;5,7,16,12|4;16,17,5,3|3;18,6,17,3|3;8,19,7,12|4;20,8,9,12|3;9,18,20,3|4;11,21,10,3|4;22,11,12,3|3;12,15,22,3|4;23,13,24,12|4;14,23,15,12|4;25,16,26,3|4;17,25,18,3|4;19,20,27,12|4;27,26,19,4|4;21,22,28,3|4;28,24,21,4|4;24,28,23,4|4;26,27,25,4|4;"
    ),
    (
      "(3.4.4.6) saturated k=4 reflex-a",
      List(3, 4, 4, 6),
      "2,8,2,4|4;1,3,1,4|4;4,2,18,4|4;3,5,17,4|4;6,4,10,4|4;5,7,9,4|3;8,6,30,4|3;7,1,29,4|4;10,14,6,6|3;9,11,5,6|4;12,10,24,6|4;11,13,23,6|4;14,12,26,6|4;13,9,25,6|3;16,20,22,6|4;15,17,21,6|4;18,16,4,6|4;17,19,3,6|4;20,18,28,6|4;19,15,27,6|4;22,24,16,4|4;21,23,15,4|4;24,22,12,4|4;23,21,11,4|4;26,30,14,3|3;25,27,13,3|4;28,26,20,3|4;27,29,19,3|4;30,28,8,3|4;29,25,7,3|3"
    ),
    (
      "(3.4.4.6) saturated k=4 convex",
      List(3, 4, 4, 6),
      "2,6,12,6|3;1,3,11,6|4;4,2,28,6|4;3,5,27,6|4;6,4,20,6|4;5,1,19,6|3;8,14,8,4|4;7,9,7,4|4;10,8,26,4|4;9,11,25,4|4;12,10,2,4|4;11,13,1,4|3;14,12,18,4|3;13,7,17,4|4;16,20,22,6|4;15,17,21,6|4;18,16,14,6|4;17,19,13,6|3;20,18,6,6|3;19,15,5,6|4;22,26,16,3|4;21,23,15,3|4;24,22,30,3|4;23,25,29,3|4;26,24,10,3|4;25,21,9,3|4;28,30,4,4|4;27,29,3,4|4;30,28,24,4|4;29,27,23,4|4"
    ),
    (
      "(3.4.4.6) saturated k=4 reflex-b",
      List(3, 4, 4, 6),
      "2,6,18,6|4;1,3,17,6|3;4,2,27,6|3;3,5,28,6|4;6,4,14,6|4;5,1,15,6|4;7,8,16,6|4;9,7,19,6|4;8,10,20,6|4;11,9,30,6|4;10,12,29,6|4;12,11,13,6|4;13,14,12,4|4;15,13,5,4|4;14,16,6,4|4;16,15,7,4|4;18,22,2,3|3;17,19,1,3|4;20,18,8,3|4;19,21,9,3|4;22,20,25,3|4;21,17,26,3|3;24,30,24,4|4;23,25,23,4|4;26,24,21,4|4;25,27,22,4|3;28,26,3,4|3;27,29,4,4|4;30,28,11,4|4;29,23,10,4|4"
    ),
    (
      "(5.5.10) penrose-rhombi k=5",
      List(5, 5, 10),
      "1,2,11,10|4;3,1,22,10|4;2,4,23,10|3;5,3,8,10|3;4,5,7,10|3;6,7,6,5|3;8,6,5,5|3;7,9,4,5|3;10,8,24,5|3;9,10,25,5|4;11,12,1,5|4;13,11,19,5|4;12,14,18,5|3;15,13,27,5|3;14,15,26,5|4;17,19,21,4|4;16,18,20,4|3;19,17,13,4|3;18,16,12,4|4;21,27,17,4|3;20,22,16,4|4;23,21,2,4|4;22,24,3,4|3;25,23,9,4|3;24,26,10,4|4;27,25,15,4|4;26,20,14,4|3"
    ),
    (
      "(3.3.4.12) uplus pattern k=7",
      List(3, 3, 4, 12),
      "2,4,34,12|4;1,3,33,12|4;4,2,20,12|4;3,1,19,12|4;6,12,48,12|4;5,7,47,12|3;8,6,16,12|3;7,9,15,12|4;10,8,44,12|4;9,11,43,12|4;12,10,32,12|4;11,5,31,12|4;14,24,42,6|4;13,15,41,6|4;16,14,8,6|4;15,17,7,6|3;18,16,46,6|3;17,19,45,6|4;20,18,4,6|4;19,21,3,6|4;22,20,28,6|4;21,23,27,6|2;24,22,26,6|2;23,13,25,6|4;26,32,24,4|4;25,27,23,4|2;28,26,22,4|2;27,29,21,4|4;30,28,38,4|4;29,31,37,4|4;32,30,12,4|4;31,25,11,4|4;34,38,2,3|4;33,35,1,3|4;36,34,50,3|4;35,37,49,3|4;38,36,30,3|4;37,33,29,3|4;40,44,40,3|4;39,41,39,3|4;42,40,14,3|4;41,43,13,3|4;44,42,10,3|4;43,39,9,3|4;46,50,18,3|4;45,47,17,3|3;48,46,6,3|3;47,49,5,3|4;50,48,36,3|4;49,45,35,3|4"
    ),
    (
      "(4.5.20) pattern k=7",
      List(4, 5, 20),
      "2,10,24,20|3;1,3,23,20|3;4,2,20,20|3;3,5,19,20|3;6,4,32,20|3;5,7,31,20|3;8,6,34,20|3;7,9,33,20|3;10,8,14,20|3;9,1,13,20|3;12,20,22,5|3;11,13,21,5|3;14,12,10,5|3;13,15,9,5|3;16,14,40,5|3;15,17,39,5|3;18,16,26,5|3;17,19,25,5|3;20,18,4,5|3;19,11,3,5|3;22,24,12,4|3;21,23,11,4|3;24,22,2,4|3;23,21,1,4|3;26,32,18,16|3;25,27,17,16|3;28,26,38,16|3;27,29,37,16|2;30,28,36,16|2;29,31,35,16|3;32,30,6,16|3;31,25,5,16|3;34,40,8,4|3;33,35,7,4|3;36,34,30,4|3;35,37,29,4|2;38,36,28,4|2;37,39,27,4|3;40,38,16,4|3;39,33,15,4|3"
    )
  )
