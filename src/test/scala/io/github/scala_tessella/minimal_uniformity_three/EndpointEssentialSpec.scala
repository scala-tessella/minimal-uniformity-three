package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Essential irregularity of the de-fusion endpoint tiles, by the FORCED-SQUARE COUNT criterion — the corner
  * gate and the area obstruction are each silent on these tiles, but they combine:
  *
  *   - any decomposition into unit-edge regular polygons uses only n ∈ {3, 4, 6, 8, 12, 24} (areas outside
  *     span{1, √2, √3, √6} are positive and cannot cancel — the Galois-sector argument of the note), and on a
  *     ζ₁₂ tile whose exact area has zero √2- and √6-parts, no octagons or 24-gons either, so #squares +
  *     6·#dodecagons = the area's RATIONAL part;
  *   - a 90° corner is completable only by a single square corner (n = 4 is the unique single fill; any two
  *     regular corners sum to ≥ 120°), whose two edges lie along the two unit boundary edges — the square is
  *     EXACTLY PLACED.
  *
  * More distinct forced squares than the rational area part ⇒ no decomposition exists. The criterion
  * independently re-proves that the pinwheel dodecagon of the (3.4².6) witness is no fusion, and certifies
  * four of the five (3.4²·6) de-fusion endpoint tiles. Where it abstains, the COMPLETE decision
  * `Defusion.regularUnion` settles the question both ways: e70's octagon and the fusion-28 star ARE unions
  * (square+hexagon; 4 squares + 4 triangles), and the FALSE verdicts (pinwheel, e52, e74a, e74b) are now
  * complete decisions, not just one-way certificates.
  *
  * Fixtures: direction words extracted from the verified endpoint symbols of `certs/defusion-endpoints`,
  * hard-coded for spec self-containment.
  */
class EndpointEssentialSpec extends AnyFlatSpec with Matchers:

  // the pinwheel's certified de-fusion endpoints (cand1: the alternating 90/210 dodecagon)
  private val e52      = UnitPolygon(12, Vector(0, 2, 5, 7, 4, 6, 9, 11, 8, 10, 1, 3))         // (C,k)=(52,7)
  private val e74a     = UnitPolygon(12, Vector(0, 2, 5, 4, 7, 9, 6, 8, 11, 10, 1, 3))         // (74,10)
  // the sibling system's endpoints (cand3)
  private val e76      = UnitPolygon(12, Vector(0, 2, 1, 9, 8, 10, 1, 4, 3, 2, 5, 8, 7, 6, 9)) // (76,11)
  private val e70      = UnitPolygon(12, Vector(0, 2, 1, 4, 7, 6, 8, 10))                      // (70,9)
  private val e74b     = UnitPolygon(12, Vector(0, 2, 1, 4, 7, 9, 6, 3, 5, 8, 11, 10))         // (74,10)
  // the alternating 90/210 pinwheel dodecagon of the (3.4².6) witness (sanity: known to be no fusion)
  private val pinwheel = UnitPolygon(12, Vector(0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 1))
  // the fusion-28 star (60/150/240): no 90° corner — the criterion must abstain
  private val star     = UnitPolygon(12, Vector(0, 1, 5, 3, 4, 8, 6, 7, 11, 9, 10, 2))

  /** Exact area of a ζ₁₂ polygon as (rational, √3) coordinates: the reduced shoelace element has degree <
    * φ(12) = 4, and Im(Σ c_k ζ₁₂^k) = c₁/2 + c₂·(√3/2) + c₃; area = Im/2.
    */
  private def areaCoords(p: UnitPolygon): (Frac, Frac) =
    p.n shouldBe 12
    val c = p.doubleArea.reducedKey
    (4 until 12).foreach(i => c(i) shouldBe BigInt(0))
    (Frac.make(c(1).toLong, 4) + Frac.make(c(3).toLong, 2), Frac.make(c(2).toLong, 4))

  /** The exactly-placed square forced at 90° corner k (edges along both boundary edges). */
  private def forcedSquare(p: UnitPolygon, k: Int): Set[Vector[BigInt]] =
    val vs   = p.verticesFrom(Cyc.zero(p.n))
    val v    = vs(k)
    val out  = Cyc.root(p.n, p.dirs(k))
    val back = Cyc.root(p.n, (p.dirs((k - 1 + p.dirs.length) % p.dirs.length) + p.n / 2) % p.n)
    Set(v, v + out, v + out + back, v + back).map(_.reducedKey)

  /** The distinct forced squares of all 90° corners. */
  private def forcedSquares(p: UnitPolygon): Set[Set[Vector[BigInt]]] =
    p.interiorAngles.zipWithIndex.collect { case (3, k) => forcedSquare(p, k) }.toSet

  /** TRUE certifies essential irregularity (one-way, sound). */
  private def essentialByForcedSquares(p: UnitPolygon): Boolean =
    val (a, _) = areaCoords(p)
    (Frac.make(forcedSquares(p).size, 1) - a).num > 0

  "areaCoords" should "give the known exact areas" in:
    areaCoords(UnitPolygon(12, Vector(0, 3, 6, 9))) shouldBe ((Frac(1, 1), Frac(0, 1)))   // square
    areaCoords(UnitPolygon(12, Vector(0, 4, 8))) shouldBe ((Frac(0, 1), Frac.make(1, 4))) // triangle
    areaCoords(pinwheel) shouldBe ((Frac(3, 1), Frac(3, 1)))                              // 3 + 3√3, the note's value
    areaCoords(e52) shouldBe ((Frac(0, 1), Frac(3, 1)))                                   // 3√3: NO squares fit at all
    areaCoords(star) shouldBe ((Frac(4, 1), Frac(1, 1))) // 4 + √3

  "the forced-square criterion" should "re-prove the pinwheel proposition" in:
    forcedSquares(pinwheel).size shouldBe 6 // six 90° corners, six distinct forced squares
    essentialByForcedSquares(pinwheel) shouldBe true

  it should "certify four of the five (3.4².6) de-fusion endpoint tiles essentially irregular" in:
    for (name, p, fc) <- List(
                           ("e52", e52, 3),
                           ("e74a", e74a, 4),
                           ("e76", e76, 3),  // six 90° corners, but adjacent pairs share squares
                           ("e74b", e74b, 2) // four 90° corners pairing into two shared squares
                         )
    do
      withClue(s"$name forced squares: ")(forcedSquares(p).size shouldBe fc)
      withClue(s"$name: ")(essentialByForcedSquares(p) shouldBe true)

  it should "expose e70's octagon as a genuine square-hexagon union (the rhombus phenomenon)" in:
    // the (70,9) endpoint's tile is NOT essentially irregular: its two 90° corners force ONE
    // shared square, and splitting it off leaves exactly the regular hexagon — a decomposable
    // tile surviving saturation because the in-class split is word-illegal
    forcedSquares(e70).size shouldBe 1
    essentialByForcedSquares(e70) shouldBe false
    val splits = Defusion.flushSplits(e70, List(4)).filter(_.arcLen == 3)
    splits.nonEmpty shouldBe true
    TilePatch.regularSizeOf(splits.head.split.remainders.head.poly) shouldBe Some(6)

  it should "abstain on the fusion-28 star (no 90° corner; triangles fit its area)" in:
    forcedSquares(star) shouldBe empty
    essentialByForcedSquares(star) shouldBe false // one-way: no verdict, not a decomposition

  // the COMPLETE decision: exhaustive corner-peeling over the justified piece universe — on
  // ζ₁₂ tiles with √2- and √6-free area only {3, 4, 6, 12} can appear (Galois sectors + area
  // parts), so regularUnion is a full decision procedure here
  private val zeta12Pieces = List(3, 4, 6, 12)

  "regularUnion" should "decide the known unions and non-unions completely" in:
    Defusion.regularUnion(UnitPolygon(12, Vector(0, 3, 6, 9)), zeta12Pieces) shouldBe true // a square IS one
    Defusion.regularUnion(UnitPolygon(12, Vector(0, 2, 6, 8)), zeta12Pieces) shouldBe
      true                                                                                 // rhombus = 2 triangles
    Defusion.regularUnion(e70, zeta12Pieces) shouldBe true                                 // square + hexagon
    Defusion.regularUnion(pinwheel, zeta12Pieces) shouldBe
      false                                                                                // the pinwheel dodecagon is no fusion
    Defusion.regularUnion(e52, zeta12Pieces) shouldBe false
    Defusion.regularUnion(e74a, zeta12Pieces) shouldBe false
    Defusion.regularUnion(e74b, zeta12Pieces) shouldBe false

  it should "decide the fusion-28 star: a UNION after all (4 squares + 4 triangles)" in:
    // the count criterion's abstention resolved: the star decomposes — its area 4 + √3 admits
    // only s = 4, t = 4, and the exhaustive search realises it. The star of the realized
    // (3².4.12) patterns IS a fusion, which is why the paper's open question for the species
    // — a member whose irregular tile is no fusion — is not answered by them.
    Defusion.regularUnion(star, zeta12Pieces) shouldBe true
