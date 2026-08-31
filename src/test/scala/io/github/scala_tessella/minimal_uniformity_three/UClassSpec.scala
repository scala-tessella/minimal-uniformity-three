package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.TypeCompatibility

import io.github.scala_tessella.research_core.DelaneySymbols.{DSymbol, Tiling}
import io.github.scala_tessella.research_core.Signatures.normalize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE k = 1 SCAN — the base case of the paper's lower bound: no tiling in U(z) has ONE vertex orbit, for any
  * of the ten non-Archimedean species. It is immediate from the one-orbit catalogue (its 93 equivariant types
  * carry only the 11 Archimedean vertex configurations, so no vertex can be all-regular of a non-Archimedean
  * type z), but it is asserted here through the general [[UClass]] machinery that the two- and three-orbit
  * scans reuse — a scan that could not see a witness at k = 1 would be worth nothing at k = 3. The
  * machinery's positive path is validated on an Archimedean z, and the three readings of the
  * around-every-vertex condition are pinned on hand examples: the spliced reading ([[UClass.cyclicSubset]]),
  * the arc clause ([[UClass.isArc]], [[UClass.strictArcLegal]]) and the definition's isolated reading
  * ([[UClass.isolatedLegal]]), which part exactly on species with a repeated letter.
  */
class UClassSpec extends AnyFlatSpec with Matchers:

  private lazy val relaxed: List[(Tiling, DSymbol)] =
    DelaneySymbols.enumerateRelaxedDetailed(maxN = 1, maxSize = 12)

  behavior of "the U class at k = 1"

  it should "target only derived arithmetic types (fairness cross-check against the 21)" in:
    UClass.targets.foreach: (z, _) =>
      TypeCompatibility.arithmeticFigures should contain(normalize(z))

  it should "validate the machinery's positive path: U(4⁴) has k = 1 candidates (the square tiling)" in:
    relaxed.flatMap((_, ds) => UClass.candidates(ds, List(4, 4, 4, 4))) should not be empty

  it should "prove that no U(z) tiling with one vertex orbit exists, for each of the ten species" in:
    UClass.targets.foreach: (z, claimed) =>
      withClue(s"z=${z.mkString(".")} (claimed uniformity $claimed): "):
        relaxed.flatMap((_, ds) => UClass.designations(ds, z)) shouldBe empty

  it should "match the manuscript-era cyclic-subset semantics on hand examples" in:
    UClass.cyclicSubset(List(3, 3), List(3, 3, 6, 6)) shouldBe true
    UClass.cyclicSubset(List(3, 3), List(3, 6, 3, 6)) shouldBe false
    UClass.cyclicSubset(List(4, 8), List(4, 8, 8)) shouldBe true
    UClass.cyclicSubset(List(8, 4, 8), List(4, 8, 8)) shouldBe true
    UClass.cyclicSubset(List(3, 8), List(4, 8, 8)) shouldBe false

  it should "read arcs of the cyclic word z without rotating them (the strict reading)" in:
    UClass.isArc(List(4, 6, 3), List(3, 4, 4, 6)) shouldBe true  // wraps
    UClass.isArc(List(3, 6, 4), List(3, 4, 4, 6)) shouldBe true  // reversed
    UClass.isArc(List(4, 3, 4), List(3, 4, 4, 6)) shouldBe false // a rotation of the arc 3.4.4 — not an arc
    UClass.cyclicSubset(List(4, 3, 4), List(3, 4, 4, 6)) shouldBe true
    UClass.isArc(List(6, 3, 6), List(3, 3, 6, 6)) shouldBe false
    UClass.isArc(List(4, 3, 12), List(3, 3, 4, 12)) shouldBe false
    UClass.isArc(List(3, 4, 4, 6), List(3, 4, 4, 6)) shouldBe true
    UClass.isArc(List(3, 4, 4, 6, 3), List(3, 4, 4, 6)) shouldBe false

  it should "judge vertices under the strict reading: one run of regular corners forming an arc of z" in:
    val z                                = List(3, 4, 4, 6)
    def w(xs: Int*): Vector[Option[Int]] = xs.map(x => Option.when(x > 0)(x)).toVector
    UClass.strictArcLegal(w(3, 4, 0, 4), z) shouldBe false                // (3.4.4_i.4): spliced 4.3.4 passes the default
    TilePatch.vertexLegal(w(3, 4, 0, 4), z) shouldBe true
    TilePatch.vertexLegal(w(3, 4, 0, 4), z, strict = true) shouldBe false
    UClass.strictArcLegal(w(3, 4, 4, 0), z) shouldBe true                 // run 3.4.4 starting at position 0
    UClass.strictArcLegal(w(4, 0, 3, 4), z) shouldBe true                 // run wraps: 3.4.4 from position 2
    UClass.strictArcLegal(w(4, 0, 4, 0), z) shouldBe false                // split arc
    UClass.strictArcLegal(w(0, 0, 0), z) shouldBe false                   // condition (2)
    UClass.strictArcLegal(w(4, 6, 3, 4), z) shouldBe true                 // all-regular, a rotation of z
    UClass.strictArcLegal(w(3, 4, 6, 4), z) shouldBe false                // all-regular but not z
    UClass.strictArcLegal(w(3, 3, 0, 6), List(3, 3, 6, 6)) shouldBe true
    UClass.strictArcLegal(w(6, 3, 0, 6), List(3, 3, 6, 6)) shouldBe true  // the run is 6.6.3, an arc
    UClass.strictArcLegal(w(6, 3, 6, 0), List(3, 3, 6, 6)) shouldBe false // the run is 6.3.6
    UClass.strictArcLegal(w(5, 0, 5), List(5, 5, 10)) shouldBe true       // cyclically adjacent: the run is 5.5
    UClass.strictArcLegal(w(5, 0, 5, 0), List(5, 5, 10)) shouldBe false // the Penrose-rhombi split arc
