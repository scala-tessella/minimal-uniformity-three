package io.github.scala_tessella.minimal_uniformity

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import io.github.scala_tessella.research_core.MetricLayer.AngleSystem
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** ADR-0009 G4, rung-3 EXISTENCE verdict (2026-07-12): FOUR of the ten minimal-uniformity conjectures are
  * settled — minimal uniformity EXACTLY 3 for z = (3.8.24), (3.4.3.12), (3.4².6) (confirming their claimed
  * value) AND for z = (3².6²), CORRECTING its claimed value 7. The ≤ 22-chamber search over ALL ten targets
  * (`UClassK3Probe`, artifacts in `certs/uclass-k3/`) surfaced genuine witnesses for exactly these four
  * (smallest per target pinned below by canonical key; (3².6²) has eight convex survivors, 11–22 chambers)
  * and none for the other six. Per witness this spec verifies the full existence chain, machine-checked end
  * to end:
  *
  *   1. Euclidean symbol, exactly 3 vertex orbits, the expected vertex configurations;
  *   2. exactly ONE U(z) designation surviving the combinatorial check, the pinned linear layer and the
  *      forced-regular filter (the rung-2 lesson) — with the intended irregular face orbit;
  *   3. the pinned system plus the linear closure equivalents (triangle equilateral, quadrilateral = rhombus,
  *      v ≥ 2 identically closed by roots of unity, edge-axis mirror hexagon palindrome) is consistent and
  *      RIGID (affine dimension 0), so the angle point is unique and forced — and the designated-irregular
  *      face takes NON-regular angles there, so the realization's truth-designation is the designation
  *      itself;
  *   4. at that unique point every angle lies in (0, π) (CONVEX) and every face orbit closes EXACTLY, the
  *      edge-vector sum computed in ℚ(ζ₂₄) (all angles are multiples of π/12) — no numerics anywhere.
  *
  * By the paper's realization proposition (prop:realize, convex angles), each point realizes as a genuine
  * edge-to-edge unit-edge tiling of the plane; the tiling is in U(z) with its truth-designation as verified,
  * so by rungs 1–2 (`UClassSpec`, `UClassK2VerdictSpec`: no U(z) tiling has uniformity ≤ 2) its full symmetry
  * group has EXACTLY 3 vertex orbits. Together with the rung-2 lower bound this settles minimal uniformity =
  * 3 for all four targets. Nothing is borrowed from the k ≤ 1 global-affinity verification: closure is
  * checked exactly at the (unique) point itself.
  *
  * The (3.8.24) and (3.4.3.12) witnesses are the manuscript's claimed patterns — (3.8.12ᵢ)/(3.8.24)/
  * (3.12ᵢ.24) with the irregular dodecagon at 135°/165°, and (3.4.3.6ᵢ)/(3.4.3.12)/(3.6ᵢ.3.12) with the
  * irregular hexagon at 90°/150°. The (3.4².6) witness is a DISCOVERY: the manuscript's pattern needs a
  * reflex-cornered dodecagon (210° at (3.4.12ᵢ)), outside the convex category, whereas this tiling —
  * (3.4.4.6)/(3.4.4ᵢ.4)/(6.4.4ᵢ.4) with an irregular 60°/120° rhombus — completes the conjecture entirely
  * within the convex category, unconditionally. The (3².6²) witness is a CORRECTION and a surprise: regular
  * triangles and hexagons with an irregular 60°/120° rhombus, vertex orbits (3.3.6.6)/(3.6.4ᵢ.6)/(6.6.4ᵢ), at
  * 11 chambers — the claimed minimal uniformity was 7.
  *
  * SCOPE (ADR-0009 D5): these are class-U statements. The 60°/120° rhombus is two unit triangles, so both
  * rhombus witnesses are triangle-pair FUSIONS of regular tilings; in the refined class U⁺ (irregular tiles
  * essentially irregular — no unions of unit regular polygons, `UClassEssentialIrregularitySpec`) the
  * (3.8.24) and (3.4.3.12) theorems persist while (3.4².6) and (3².6²) reopen.
  */
class UClassK3ExistenceSpec extends AnyFlatSpec with Matchers:

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

  private def faceCornerSeq(ds: DSymbol, corner: Array[Int], d0: Int): Array[Int] =
    // the geometric corner sequence, as in MetricLayer.faceCornerSeq (kept local: test-side re-derivation)
    val frag = collection.mutable.ArrayBuffer.empty[Int]
    var cur  = d0
    var go   = true
    while go do
      frag += corner(cur)
      cur = ds.get(1, ds.get(0, cur))
      if cur == d0 then go = false
    val m    = ds.m(0, 1, d0)
    Array.tabulate(m)(i => frag(i % frag.length))

  /** RREF over Frac: particular solution of an augmented system, or None if inconsistent. */
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

  /** The affine model of a designation: angle sums + regular pins + per-irregular-face LINEAR closure
    * equivalents (triangle → equilateral; quadrilateral → opposite corners equal; v ≥ 2 → identically closed;
    * edge-axis mirror hexagon → palindrome ends equal). Fails the test if an irregular face fits no category
    * — those would need explicit closure handling.
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
      val seq = faceCornerSeq(ds, corner, d0)
      if reg(k) then seq.foreach(c => pin(c, Frac.make(p - 2L, p)))
      else if p == 3 then seq.foreach(c => pin(c, Frac(1, 3)))
      else if p == 4 then { eq(seq(0), seq(2)); eq(seq(1), seq(3)) }
      else if v >= 2 then ()
      else if p == 6 && o.isChain && o.elements.exists(d => ds.get(0, d) == d) then
        val adjPairs = (0 until 6).filter(j => seq(j) == seq((j + 1) % 6)).map(j => seq(j))
        withClue(s"mirror hexagon orbit $k adjacent-equal pairs: ")(adjPairs.size shouldBe 2)
        eq(adjPairs(0), adjPairs(1))
      else fail(s"irregular face orbit $k (p=$p, v=$v) fits no linear closure category")
    sys.rows ++ extra.result()

  /** `Σ e^{iφ}` of the face through `d0` at `x` (π units, all multiples of 1/12), EXACTLY in ℚ(ζ₂₄). */
  private def closesExactly(ds: DSymbol, corner: Array[Int], d0: Int, x: Array[Frac]): Boolean =
    val seq  = faceCornerSeq(ds, corner, d0)
    var kAcc = 0
    var sx   = Cyclo24.Cyclo.zero
    var sy   = Cyclo24.Cyclo.zero
    for j <- seq.indices do
      sx = sx + Cyclo24.Cyclo.cosPi12(kAcc)
      sy = sy + Cyclo24.Cyclo.sinPi12(kAcc)
      val g = x(seq(j))
      require(12L * g.num % g.den == 0, s"corner angle $g is not a multiple of π/12")
      kAcc += 12 - (12L * g.num / g.den).toInt
    sx.isZero && sy.isZero

  /** One rung-3 existence witness: target, canonical key, expected configurations (walk order, `i` marking
    * faces of the irregular-designated orbit), and the irregular face's forced corner angles (π units).
    */
  private case class Witness(
      z: List[Int],
      key: String,
      configs: Set[String],
      irregularAngles: Set[Frac]
  )

  private val witnesses = List(
    Witness(
      z = List(3, 8, 24),
      key =
        "1,2,3,12|3;4,1,5,12|3;3,6,1,24|3;2,7,8,12|3;8,9,2,3|3;10,3,9,24|3;7,4,11,12|3;5,12,4,3|3;13,5,6,3|3;6,14,13,24|3;11,15,7,8|3;16,8,15,3|3;9,16,10,3|3;14,10,17,24|3;18,11,12,8|3;12,13,18,3|3;17,18,14,8|3;15,17,16,8|3",
      configs = Set("24.8.3", "12i.8.3", "12i.3.24"),
      irregularAngles = Set(Frac(3, 4), Frac(11, 12)) // 135°, 165°
    ),
    Witness(
      z = List(3, 4, 3, 12),
      key =
        "2,1,3,12|4;1,2,4,12|4;4,5,1,3|4;3,6,2,3|4;7,3,8,3|4;9,4,10,3|4;5,9,11,3|4;11,8,5,4|4;6,7,12,3|4;12,10,6,6|4;8,11,7,4|4;10,12,9,6|4",
      configs = Set("12.3.4.3", "12.3.6i.3", "3.6i.3.4"),
      irregularAngles = Set(Frac(1, 2), Frac(5, 6))   // 90°, 150°
    ),
    Witness(
      z = List(3, 4, 4, 6),
      key =
        "1,2,3,3|4;4,1,5,3|4;3,6,1,6|4;2,4,7,3|4;7,8,2,4|4;9,3,10,6|4;5,11,4,4|4;12,5,12,4|4;6,9,13,6|4;13,12,6,4|4;14,7,15,4|4;8,10,8,4|4;10,14,9,4|4;11,13,16,4|4;16,15,11,4|4;15,16,14,4|4",
      configs = Set("3.4.4.6", "3.4.4i.4", "6.4.4i.4"),
      irregularAngles = Set(Frac(1, 3), Frac(2, 3))   // 60°, 120°
    ),
    Witness(
      z = List(3, 3, 6, 6),
      key =
        "1,2,1,3|4;3,1,4,3|4;2,3,5,3|4;5,6,2,6|4;4,7,3,6|4;6,4,6,6|4;8,5,9,6|4;7,10,11,6|3;11,9,7,4|4;10,8,10,6|3;9,11,8,4|3",
      configs = Set("3.6.6.3", "3.6.4i.6", "6.6.4i"),
      irregularAngles = Set(Frac(1, 3), Frac(2, 3))   // 60°, 120° — corrects the claimed uniformity 7
    )
  )

  private def symbols: List[(Witness, DSymbol)] = witnesses.map(w => (w, symbolFromKey(w.key)))

  /** The unique designation surviving candidates + the forced-regular filter (asserted unique). */
  private def designationOf(ds: DSymbol, z: List[Int]): Set[Int] =
    val unforced = UClass
      .candidates(ds, z)
      .filter(reg =>
        ds.orbs.zipWithIndex.forall((o, k) => o.i != 0 || reg(k) || !UClass.forcedRegular(ds, reg, k))
      )
    unforced should have size 1
    unforced.head

  behavior of "the rung-3 existence witnesses (ADR-0009 G4)"

  it should "reconstruct faithfully: Euclidean symbols with exactly 3 vertex orbits" in:
    for (w, ds) <- symbols do
      withClue(s"z=${w.z.mkString(".")}: "):
        DelaneySymbols.canonicalKey(ds) shouldBe w.key + ";"
        DelaneySymbols.isEuclidean(ds) shouldBe true
        ds.orbs.count(_.i == 1) shouldBe 3

  it should "carry exactly one surviving U(z) designation, with the expected vertex configurations" in:
    for (w, ds) <- symbols do
      withClue(s"z=${w.z.mkString(".")}: "):
        val reg      = designationOf(ds, w.z)
        val rendered = ds.orbs
          .filter(_.i == 1)
          .map(o =>
            DelaneySymbols
              .vertexConfigOrbits(ds, o.elements.head)
              .get
              .map((f, p) => if reg(f) then s"$p" else s"${p}i")
              .mkString(".")
          )
        rendered.toSet shouldBe w.configs

  it should
    "be metrically rigid: the affine model has a unique, convex solution with the forced irregular angles" in:
      for (w, ds) <- symbols do
        withClue(s"z=${w.z.mkString(".")}: "):
          val reg  = designationOf(ds, w.z)
          val sys  = MetricLayer.angleSystem(ds)
          val rows = affineRows(ds, reg)
          MetricLayer.nullspaceBasis(AngleSystem(sys.vars, sys.corner, rows)).size shouldBe 0
          val x    = particular(rows, sys.vars).getOrElse(fail("affine model inconsistent"))
          x.foreach(a => (a.signum > 0 && (Frac(1, 1) - a).signum > 0) shouldBe true) // convex: 0 < θ < π
          for (o, k) <- ds.orbs.zipWithIndex if o.i == 0 && !reg(k) do
            val vals = faceCornerSeq(ds, sys.corner, o.elements.head).distinct.map(x(_))
            vals.map(v => Frac.make(v.num, v.den)).toSet shouldBe w.irregularAngles
            val regV = Frac.make(ds.m(0, 1, o.elements.head) - 2L, ds.m(0, 1, o.elements.head))
            vals.exists(v => !(v - regV).isZero) shouldBe true // genuinely irregular at the point

  it should "satisfy the full angle system and close every face EXACTLY over ℚ(ζ₂₄) at the unique point" in:
    for (w, ds) <- symbols do
      withClue(s"z=${w.z.mkString(".")}: "):
        val reg = designationOf(ds, w.z)
        val sys = MetricLayer.angleSystem(ds)
        val x   = particular(affineRows(ds, reg), sys.vars).get
        MetricLayer.satisfies(sys, x) shouldBe true
        for o <- ds.orbs if o.i == 0 do closesExactly(ds, sys.corner, o.elements.head, x) shouldBe true
