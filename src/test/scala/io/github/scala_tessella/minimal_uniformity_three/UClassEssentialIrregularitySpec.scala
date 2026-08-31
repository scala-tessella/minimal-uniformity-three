package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Cyclo24.{Cyclo, Rat}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** FUSIONS AND THE EXACT AREA OBSTRUCTION. Call a tile a FUSION if it is a finite union of unit-edge regular
  * polygons with pairwise disjoint interiors, meeting one another and the tile's boundary edges edge-to-edge
  * — the paper's notion, and the subject of its open question for (3².4.12): is there a member of that
  * species whose irregular tile is no fusion? This spec pins the exact AREA OBSTRUCTION that decides most
  * tiles outright, and the corner argument that decides the one the area leaves open.
  *
  * The obstruction, over the alphabet {3,4,6,8,12,24} (unit areas √3/4, 1, 3√3/2, 2+2√2, 6+3√3,
  * 12+6√2+6√3+6√6): any fusion has area t√3/4 + s + h·3√3/2 + o(2+2√2) + d(6+3√3) + D(12+6√2+6√3+6√6) with
  * non-negative integer counts, so a tile whose exact area lies outside that lattice is no fusion. Areas are
  * computed by exact shoelace over unit edge vectors at multiples of π/12 and expressed in the basis {1, √2,
  * √3, √6} of the real subfield of ℚ(ζ₂₄) — no numerics.
  *
  * The verdicts:
  *
  *   - the 90°/150° hexagon of the (3.4.3.12) witness (rational part 3/2 ∉ ℤ) and the 135°/165° dodecagon of
  *     the (3.8.24) one (its √6 part would force a quarter 24-gon) are NO FUSION: those two witnesses carry
  *     genuinely non-decomposable tiles;
  *   - the 60°/120° rhombus IS a fusion (two unit triangles), which is what puts the rhombus patterns of
  *     [[UClassK3ExistenceSpec]] outside the class as defined and into the paper's wider spliced reading; so
  *     is the 90°/180° octagon (four squares). For (3².6²) the paper closes the question this way round:
  *     every irregular tile of the species is a fusion of unit triangles;
  *   - the 90°/210° pinwheel dodecagon of the (3.4².6) witness is INCONCLUSIVE at area level (3 + 3√3 admits
  *     3s+2h, 3s+h+6t, 3s+12t) and yet NO FUSION, by corner forcing: a 90° corner is completable only by a
  *     single square corner (n = 4 is the unique single fill; two corners sum to ≥ 120°), that square's edges
  *     must lie along the two unit boundary edges so it spans both neighbouring 210° vertices, and the
  *     strictly alternating (90, 210) boundary makes consecutive forced squares overload their shared 210°
  *     vertex (90 + 90 leaves 30°, which no corner fills).
  *
  * The degenerate (60,60,240)² "hexagon" of the (3².6²) reflex candidates is pinned as NON-SIMPLE (its
  * boundary revisits the origin after three edges) — closure and healthy angles do not imply a simple
  * polygon. [[EndpointEssentialSpec]] carries the complementary criterion, the forced-square count, for the
  * tiles this area alphabet does not reach.
  */
class UClassEssentialIrregularitySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  /** Shoelace area (×1) of the unit-edge polygon with interior angles `deg` (degrees, cyclic order). */
  private def exactArea(deg: List[Int]): Cyclo =
    // direction of edge j in π/12 units: k_{j+1} = k_j + (12 - deg_j/15)
    var k    = 0
    val dirs = deg.map { d =>
      val cur = k
      k += 12 - d / 15
      cur
    }
    // vertices v_0 = 0, v_{j+1} = v_j + e(dir_j); shoelace sum of cross products, halved
    var x    = Cyclo.zero
    var y    = Cyclo.zero
    var acc  = Cyclo.zero
    for j <- dirs do
      val nx = x + Cyclo.cosPi12(j)
      val ny = y + Cyclo.sinPi12(j)
      acc = acc + (x * ny - nx * y)
      x = nx; y = ny
    acc.scale(Rat.make(1, 2))

  /** Coordinates of a real cyclotomic element in the basis {1, √2, √3, √6} (asserts exactness). */
  private def inRealBasis(c: Cyclo): (Rat, Rat, Rat, Rat) =
    val one = Cyclo.one
    val rt2 = Cyclo.zeta(3) + Cyclo.zeta(-3) // 2cos(π/4)
    val rt3 = Cyclo.zeta(2) + Cyclo.zeta(-2) // 2cos(π/6)
    val rt6 = rt2 * rt3
    val bs  = Vector(one, rt2, rt3, rt6)
    // solve 8x4 rational system bs * v = c exactly (least: Gaussian elimination on the 8 coords)
    val m   = Array.tabulate(8)(r => Array.tabulate(5)(cc => if cc < 4 then bs(cc).c(r) else c.c(r)))
    var row = 0
    for col <- 0 until 4 do
      var pr  = row
      while pr < 8 && m(pr)(col).isZero do pr += 1
      require(pr < 8, "singular basis")
      val t   = m(pr); m(pr) = m(row); m(row) = t
      val inv = m(row)(col)
      for j <- col to 4 do m(row)(j) = m(row)(j) / inv
      for i <- 0 until 8 if i != row && !m(i)(col).isZero do
        val f = m(i)(col)
        for j <- col to 4 do m(i)(j) = m(i)(j) - f * m(row)(j)
      row += 1
    for i <- 4 until 8 do require(m(i)(4).isZero, "element not in the real subfield span")
    (m(0)(4), m(1)(4), m(2)(4), m(3)(4))

  private def area(deg: List[Int]): (Rat, Rat, Rat, Rat) = inRealBasis(exactArea(deg))

  private def rat(n: Long, d: Long): Rat = Rat.make(n, d)

  behavior of "the fusion area obstruction"

  it should "confirm the engine on regular tiles" in:
    area(List(60, 60, 60)) shouldBe ((rat(0, 1), rat(0, 1), rat(1, 4), rat(0, 1)))
    area(List(120, 120, 120, 120, 120, 120)) shouldBe ((rat(0, 1), rat(0, 1), rat(3, 2), rat(0, 1)))
    area(List(135, 135, 135, 135, 135, 135, 135, 135)) shouldBe ((rat(2, 1), rat(2, 1), rat(0, 1), rat(0, 1)))

  it should "expose the fused tiles: the rhombus is two triangles, the straight octagon four squares" in:
    area(List(60, 120, 60, 120)) shouldBe ((rat(0, 1), rat(0, 1), rat(1, 2), rat(0, 1))) // = 2·(√3/4)
    area(List(90, 180, 90, 180, 90, 180, 90, 180)) shouldBe
      ((rat(4, 1), rat(0, 1), rat(0, 1), rat(0, 1))) // = 4·1

  it should "certify the (3.4.3.12) hexagon essentially irregular: rational part 3/2 is not an integer" in:
    // any {3,4,6,8,12,24}-union has rational part s + 2o + 6d + 12D ∈ ℤ and √2 part 2o + 6D
    area(List(90, 150, 90, 150, 90, 150)) shouldBe ((rat(3, 2), rat(0, 1), rat(1, 2), rat(0, 1)))

  it should "certify the (3.8.24) dodecagon essentially irregular: the √6 part forces a quarter 24-gon" in:
    // √6 part of a union is 6D, D ∈ ℤ — but here it is 3/2
    area(List(135, 135, 165, 165, 135, 135, 165, 165, 135, 135, 165, 165)) shouldBe
      ((rat(3, 2), rat(3, 2), rat(2, 1), rat(3, 2)))

  it should "leave the 90/210 pinwheel dodecagon area-INCONCLUSIVE (3 + 3√3 is representable)" in:
    area(List(90, 210, 90, 210, 90, 210, 90, 210, 90, 210, 90, 210)) shouldBe
      ((rat(3, 1), rat(0, 1), rat(3, 1), rat(0, 1))) // 3s + 2h, 3s + h + 6t, 3s + 12t all match

  it should "prove the pinwheel essentially irregular anyway: the forced-square contradiction" in:
    // (i) a 90° corner is completable ONLY by a single square corner: the unique n with
    //     (n−2)·180/n = 90 is n = 4, and any TWO regular corners sum to ≥ 120 > 90;
    (3 to 360).filter(n => 180 * (n - 2) == 90 * n) shouldBe List(4)
    // (ii) so at each of the six 90-corners the filling square's edges lie along the two unit
    //     boundary edges — the square is FORCED to span both neighbouring 210-vertices;
    // (iii) the boundary alternates (90, 210), so consecutive forced squares SHARE their
    //     intermediate 210-vertex, placing 90 + 90 = 180 there and leaving 30° — not completable
    //     (a forever-table refusal): no decomposition into unit-edge regular polygons exists.
    gate(Rat.make(30, 1)) shouldBe false

  it should "pin the (3².6²) reflex candidates' hexagon as degenerate: the boundary revisits the origin" in:
    // (60,60,240,…): after three unit edges at directions 0, 120, 240 the walk is back at the start
    val back  = Cyclo.cosPi12(0) + Cyclo.cosPi12(8) + Cyclo.cosPi12(16)
    val backY = Cyclo.sinPi12(0) + Cyclo.sinPi12(8) + Cyclo.sinPi12(16)
    back.isZero shouldBe true
    backY.isZero shouldBe true

  // ---- the CORNER-ANGLE gate ---------------------------------------------------------------------------
  // In a decomposition the pieces at a tile corner of angle θ fill it with regular interior angles
  // (n−2)·180/n — piece edges along the tile's unit boundary edges coincide with them, so no piece edge
  // ends mid-boundary; at a REFLEX corner one piece edge may pass straight through, contributing 180. A
  // corner whose angle (nor, if reflex, angle − 180) is NO finite sum of regular angles certifies
  // essential irregularity by itself — a sound, fast, purely local gate.

  /** θ (degrees) a finite sum of regular interior angles (n−2)·180/n? Equivalently: ∃ k ≤ ⌊θ/60⌋ with Σ 1/nᵢ =
    * (180k − θ)/360 — a bounded Egyptian-fraction search with nondecreasing denominators nᵢ ≥ 3, where at
    * each step 1/n ≤ t ≤ j/n confines n to [⌈1/t⌉, ⌊j/t⌋]: complete, with NO cap on n.
    */
  private def angleSum(theta: Rat): Boolean =
    def egyptian(t: Rat, j: Int, minN: BigInt): Boolean =
      if j == 0 then t.isZero
      else if t.num <= 0 then false
      else
        val lo = minN.max((t.den + t.num - 1) / t.num) // smallest n with 1/n ≤ t
        val hi = (BigInt(j) * t.den) / t.num           // largest n with j·(1/n) ≥ t
        require(hi - lo < 1000000, s"unexpectedly wide Egyptian range for t = ${t.num}/${t.den}")
        Iterator.iterate(lo)(_ + 1).takeWhile(_ <= hi).exists(n => egyptian(t - Rat.make(1, n), j - 1, n))
    theta.num > 0 && {
      val kMax = (theta.num / (theta.den * 60)).toInt
      (1 to kMax).exists: k =>
        val t = (Rat.make(180L * k, 1) - theta) / Rat.make(360, 1)
        t.num >= 0 && egyptian(t, k, BigInt(3))
    }

  private def gate(thetaDeg: Rat): Boolean =
    angleSum(thetaDeg) || ((thetaDeg - Rat.make(180, 1)).num > 0 && angleSum(thetaDeg - Rat.make(180, 1)))

  it should
    "find the corner-angle gate SILENT on every witness corner: each is a regular-angle complement" in:
      // the species are 360° vertex types, so each irregular corner equals the missing regular tile's angle
      // (135 = octagon, 165 = 24-gon, 150 = 12-gon, 210 = 60+150 = 90+120): local checks cannot see this
      // family's irregularity — it is global, which is what the area obstruction captures
      List(60, 90, 120, 135, 150, 165, 180, 210, 240).foreach: d =>
        withClue(s"$d°: ")(gate(Rat.make(d, 1)) shouldBe true)

  it should "confirm the corner-angle gate has teeth on non-complement angles" in:
    List(75, 105, 130, 155).foreach: d =>
      withClue(s"$d°: ")(gate(Rat.make(d, 1)) shouldBe false)

  /** The FOREVER TABLE. The full set of completable angles is infinite (every regular angle qualifies,
    * accumulating at 180°), but restricted to the π/12 lattice — where every forced angle of this research
    * line lives — it is finite and closed: sums involving off-lattice polygons land on the lattice only at
    * totals ≥ 270° (e.g. 108 + 162, 140 + 160), which lattice sums already cover. Computed once, kept
    * forever: the gate refuses EXACTLY {15°, 30°, 45°, 75°, 105°} on the lattice.
    */
  private val latticeRefusals = Set(15, 30, 45, 75, 105)

  it should "derive the forever-table: on the 15° lattice the gate refuses exactly {15,30,45,75,105}" in:
    (1 to 23).foreach: m =>
      val d = 15 * m
      withClue(s"$d°: ")(gate(Rat.make(d, 1)) shouldBe !latticeRefusals(d))

  it should "never refuse a genuine sum of regular interior angles (property test)" in:
    // random regular polygons, greedy prefix kept under 360°: the gate must certify every genuine sum
    val polygons = Gen.listOfN(5, Gen.choose(3, 360))
    forAll(polygons, minSuccessful(300)): ns0 =>
      val ns  = ns0.foldLeft(List.empty[Int]): (acc, n) =>
        val s = (acc :+ n).foldLeft(Rat.zero)((a, m) => a + Rat.make(180L * (m - 2), m))
        if (s - Rat.make(360, 1)).num < 0 then acc :+ n else acc
      val sum = ns.foldLeft(Rat.zero)((a, n) => a + Rat.make(180L * (n - 2), n))
      angleSum(sum) shouldBe true
