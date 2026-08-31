package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE WITNESSES BEYOND UNIFORMITY THREE, re-verified from their canonical keys alone — the always-on
  * counterpart of [[UClassK3ExistenceSpec]] for the six patterns the paper carries at uniformity 4, 5, 7 and
  *   10. Each is rebuilt from the appendix key and put through the same chain, with one piece replaced: the
  *       ℚ(ζ₂₄) closure test of the k = 3 witnesses assumes every angle is a multiple of π/12, which fails as
  *       soon as pentagons (3π/5), 9-gons (7π/9) or heptagons (5π/7) enter. Closure is decided instead by
  *       VANISHING SUMS OF ROOTS OF UNITY: with every corner angle a rational multiple of π, each edge
  *       direction is ζ_N^a for N = 2·lcm(denominators), and Σ ζ_N^{a_j} = 0 iff the integer polynomial Σ
  *       x^{a_j} is divisible by the N-th cyclotomic polynomial — exact, integer-only, and valid for whatever
  *       field the pattern lives in.
  *
  * Per pattern this spec checks, in order:
  *
  *   1. the key round-trips (so the appendix string and the object verified here are the same symbol), the
  *      symbol is euclidean, and its vertex-orbit count is the claimed uniformity — which is what makes the
  *      pattern an UPPER bound for its species, the lower bound coming from the scans;
  *   2. a designation survives the combinatorial U(z) check and the forced-regular filter, with a non-empty
  *      irregular part;
  *   3. the affine model — angle sums, the pins a regular designation forces, and the linear closure
  *      equivalents — is CONSISTENT and RIGID (nullspace dimension 0), so the angle point is unique and
  *      forced, and at it the irregular-designated faces take genuinely non-regular angles (the
  *      truth-designation is the designation itself);
  *   4. at that point every face orbit CLOSES exactly, every face boundary is SIMPLE, and no corner is
  *      straight. Simplicity is not implied by closure and healthy angles: a reflex boundary can self-touch
  *      with every angle in (0, 2π) \ {π}, so two boundary vertices coincide iff the edge vectors of a proper
  *      contiguous sub-range sum to zero — the same exact vanishing-sums test, applied to every sub-range. A
  *      straight (π) corner is no corner of a simple polygon at all: it subdivides an edge, i.e. the tile has
  *      an edge of length 2, which the unit-edge class excludes;
  *   5. convexity comes out as the paper reports it — the two uniformity-4 hexagon patterns and the
  *      uniformity-10 one convex, the star dodecagon of (3².4.12) (240°), the reflex 36-gon of (3.10.15) and
  *      the 16-gon of (4.5.20) (270°) not.
  *
  * What is NOT here: the step from a closed, simple, rigid angle point to an actual tiling of the plane. That
  * is the paper's realization certificate, discharged pattern by pattern in [[WitnessRealizationProbe]]
  * (opt-in: it develops each pattern out to a radius and verifies a translation lattice), and the saturation
  * verdicts, which are [[SaturationProbe]] and [[JointSaturationProbe]].
  */
class UClassK4ExistenceSpec extends AnyFlatSpec with Matchers:

  /** One pattern: its name in the banked set, species, chamber count, the uniformity the paper claims for it,
    * and whether its tiles are convex.
    */
  private case class Pattern(name: String, z: List[Int], chambers: Int, k: Int, convex: Boolean)

  private val patterns = List(
    Pattern("(3.9.18) w k=4", List(3, 9, 18), 24, 4, convex = true),
    Pattern("(5.5.10) w k=4", List(5, 5, 10), 18, 4, convex = true),
    Pattern("(3.3.4.12) fusion28 #1", List(3, 3, 4, 12), 28, 4, convex = false),
    Pattern("(3.3.4.12) fusion28 #2", List(3, 3, 4, 12), 28, 4, convex = false),
    Pattern("(3.10.15) pattern k=5", List(3, 10, 15), 22, 5, convex = false),
    Pattern("(4.5.20) pattern k=7", List(4, 5, 20), 40, 7, convex = false),
    Pattern("(3.7.42) P1 k=10", List(3, 7, 42), 60, 10, convex = true)
  )

  private def keyOf(name: String): String =
    SaturationProbe.entries.find(_._1 == name).getOrElse(fail(s"no banked pattern named $name"))._3

  private def symbols: List[(Pattern, DSymbol)] =
    patterns.map(p => (p, DelaneySymbols.symbolFromKey(keyOf(p.name))))

  /** The geometric corner sequence of the face through `d0`, as in `MetricLayer`'s own face walk. */
  private def faceCornerSeq(ds: DSymbol, corner: Array[Int], d0: Int): Array[Int] =
    val frag = collection.mutable.ArrayBuffer.empty[Int]
    var cur  = d0
    var go   = true
    while go do
      frag += corner(cur)
      cur = ds.get(1, ds.get(0, cur))
      if cur == d0 then go = false
    val m    = ds.m(0, 1, d0)
    Array.tabulate(m)(i => frag(i % frag.length))

  /** The designations surviving the combinatorial check and the forced-regular filter. */
  private def designations(ds: DSymbol, z: List[Int]): List[Set[Int]] =
    for
      reg      <- UClass.candidates(ds, z)
      irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !reg(k) => k }
      if irregular.nonEmpty && UClass.noneForcedRegular(ds, reg, irregular)
    yield reg

  /** Angle sums plus every linear condition a designation forces: the regular pins, and per irregular face
    * the closure equivalent of its category (triangle equilateral, quadrilateral a rhombus, a face met at
    * valence ≥ 2 identically closed, edge-axis mirror hexagon palindrome). A face fitting no category
    * contributes nothing — sound, since these rows are necessary conditions of closure, and the exact closure
    * test below is run over EVERY face orbit anyway.
    */
  private def affineRows(ds: DSymbol, reg: Set[Int]): Vector[(Array[Frac], Frac)] =
    val sys                            = MetricLayer.angleSystem(ds)
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
      val seq = faceCornerSeq(ds, sys.corner, d0)
      if reg(k) then seq.foreach(c => pin(c, Frac.make(p - 2L, p)))
      else if p == 3 then seq.foreach(c => pin(c, Frac(1, 3)))
      else if p == 4 then { eq(seq(0), seq(2)); eq(seq(1), seq(3)) }
      else if v >= 2 then ()
      else if p == 6 && o.isChain && o.elements.exists(d => ds.get(0, d) == d) then
        val adjPairs = (0 until 6).filter(j => seq(j) == seq((j + 1) % 6)).map(j => seq(j))
        if adjPairs.size == 2 then eq(adjPairs(0), adjPairs(1))
      else ()
    sys.rows ++ extra.result()

  /** True iff `Σ_j ζ_N^{a_j} = 0` exactly: reduce `Σ x^{a_j}` modulo the monic integer N-th cyclotomic
    * polynomial and test the remainder for zero.
    */
  private def rootsOfUnitySumIsZero(exponents: Seq[Int], n: Int): Boolean =
    val c   = Array.fill(n)(BigInt(0))
    exponents.foreach(a => c(((a % n) + n) % n) += 1)
    val phi = CycloRing.cyclotomic(n)
    val rem = c.clone
    for i <- (0 to rem.length - phi.length).reverse do
      val f = rem(i + phi.length - 1)
      if f != BigInt(0) then for j <- phi.indices do rem(i + j) -= f * phi(j)
    rem.forall(_ == BigInt(0))

  private def lcm(a: Long, b: Long): Long =
    def gcd(x: Long, y: Long): Long = if y == 0 then x.abs else gcd(y, x % y)
    (a / gcd(a, b)) * b

  /** The TRUTH-DESIGNATION and its forced angle point. Several designations can be rigid at once and force
    * the SAME point — calling a face orbit regular or irregular is a labelling choice, and the labelling is
    * free wherever the forced angles happen to be regular anyway. The truth-designation is the honest one:
    * every orbit it designates irregular really does take non-regular angles at the forced point. That one is
    * unique, and it is the designation the paper's witness theorems name.
    */
  private def truthDesignation(ds: DSymbol, z: List[Int]): (Set[Int], Array[Frac], Array[Int]) =
    val sys      = MetricLayer.angleSystem(ds)
    val rigid    =
      for
        reg <- designations(ds, z)
        rows = affineRows(ds, reg)
        if MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows)).isEmpty
        x   <- MetricLayer.particularSolution(rows, sys.vars)
        if ds.orbs.zipWithIndex.forall: (o, k) =>
          o.i != 0 || reg(k) || {
            val size = ds.m(0, 1, o.elements.head)
            val regV = Frac.make(size - 2L, size)
            faceCornerSeq(ds, sys.corner, o.elements.head).exists(c => !(x(c) - regV).isZero)
          }
      yield (reg, x)
    withClue("the honest rigid designation is not unique: ")(rigid should have size 1)
    val (reg, x) = rigid.head
    (reg, x, sys.corner)

  /** The exterior-turn exponents of the face through `d0`, in units of π/den. */
  private def faceExponents(
      ds: DSymbol,
      corner: Array[Int],
      d0: Int,
      x: Array[Frac],
      den: Long
  ): IndexedSeq[Int] =
    val seq = faceCornerSeq(ds, corner, d0)
    var acc = 0L
    seq.indices.map: j =>
      val e = acc
      val g = x(seq(j))
      acc += den - (den * g.num / g.den)
      e.toInt

  behavior of "the witnesses of uniformity four and beyond"

  it should "reconstruct faithfully from the appendix keys, with the claimed chamber count and uniformity" in:
    for (p, ds) <- symbols do
      withClue(s"${p.name}: "):
        // the banked key need not itself be canonical; what must hold is that it names ONE symbol —
        // rebuilding from the canonical key gives back the same object
        val canon = DelaneySymbols.canonicalKey(ds)
        DelaneySymbols.canonicalKey(DelaneySymbols.symbolFromKey(canon)) shouldBe canon
        DelaneySymbols.isEuclidean(ds) shouldBe true
        ds.size shouldBe p.chambers
        ds.orbs.count(_.i == 1) shouldBe p.k

  it should "carry a designation legal in U(z) with a non-empty irregular part" in:
    for (p, ds) <- symbols do
      withClue(s"${p.name}: "):
        designations(ds, p.z) should not be empty

  it should "be metrically rigid, with the irregular faces genuinely irregular at the forced point" in:
    for (p, ds) <- symbols do
      withClue(s"${p.name}: "):
        val (reg, x, corner) = truthDesignation(ds, p.z)
        for (o, k) <- ds.orbs.zipWithIndex if o.i == 0 && !reg(k) do
          val size = ds.m(0, 1, o.elements.head)
          val regV = Frac.make(size - 2L, size)
          faceCornerSeq(ds, corner, o.elements.head).exists(c => !(x(c) - regV).isZero) shouldBe true

  it should "close every face orbit exactly, with every boundary simple and no straight corner" in:
    for (p, ds) <- symbols do
      withClue(s"${p.name}: "):
        val (_, x, corner) = truthDesignation(ds, p.z)
        val den            = (0 until MetricLayer.angleSystem(ds).vars).map(x(_).den).foldLeft(1L)(lcm)
        val n              = (2 * den).toInt
        info(s"${p.name}: closure in the ${n}-th roots of unity")
        for o <- ds.orbs if o.i == 0 do
          val exps   = faceExponents(ds, corner, o.elements.head, x, den)
          withClue(s"face orbit through ${o.elements.head} does not close: "):
            rootsOfUnitySumIsZero(exps, n) shouldBe true
          val len    = exps.length
          val simple = (0 until len).forall: from =>
            (1 until len).forall: sub =>
              !rootsOfUnitySumIsZero((0 until sub).map(j => exps((from + j) % len)), n)
          withClue(s"face orbit through ${o.elements.head} is not simple: ")(simple shouldBe true)
        for c <- 0 until MetricLayer.angleSystem(ds).vars do
          withClue(s"corner $c is straight: ")((x(c).num != x(c).den) shouldBe true)

  it should "be convex exactly where the paper reports it" in:
    for (p, ds) <- symbols do
      withClue(s"${p.name}: "):
        val (_, x, _) = truthDesignation(ds, p.z)
        val vars      = MetricLayer.angleSystem(ds).vars
        (0 until vars).forall(c => x(c).num > 0 && x(c).num < x(c).den) shouldBe p.convex
