package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.DelaneySymbols.{DSymbol, Tiling}
import io.github.scala_tessella.research_core.Signatures.normalize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** ADR-0009 G4, rung k = 1: the minimal-uniformity conjectures over the complete k = 1 catalogue. Rung 1
  * upgrades every conjecture's first step to a THEOREM: no U(z) tiling with ONE vertex orbit exists for any
  * of the ten targets — immediate from G1 (the 93 types carry only the 11 Archimedean configurations, so no
  * vertex can be all-regular of type z), but asserted through the general [[UClass]] machinery that the k ≥ 2
  * campaigns reuse. The machinery's positive path is validated on an Archimedean z, and the cyclic-subset
  * semantics on hand examples.
  */
class UClassSpec extends AnyFlatSpec with Matchers:

  private lazy val relaxed: List[(Tiling, DSymbol)] =
    DelaneySymbols.enumerateRelaxedDetailed(maxN = 1, maxSize = 12)

  behavior of "the U class at k = 1 (ADR-0009 G4, rung 1)"

  it should "target only derived arithmetic types (fairness cross-check against the 21)" in:
    UClass.targets.foreach: (z, _) =>
      TypeCompatibility.arithmeticFigures should contain(normalize(z))

  it should "validate the machinery's positive path: U(4⁴) has k = 1 candidates (the square tiling)" in:
    relaxed.flatMap((_, ds) => UClass.candidates(ds, List(4, 4, 4, 4))) should not be empty

  it should "prove rung 1: no U(z) tiling with one vertex orbit exists, for each of the ten targets" in:
    UClass.targets.foreach: (z, claimed) =>
      withClue(s"z=${z.mkString(".")} (claimed uniformity $claimed): "):
        relaxed.flatMap((_, ds) => UClass.designations(ds, z)) shouldBe empty

  it should "match the manuscript-era cyclic-subset semantics on hand examples" in:
    UClass.cyclicSubset(List(3, 3), List(3, 3, 6, 6)) shouldBe true
    UClass.cyclicSubset(List(3, 3), List(3, 6, 3, 6)) shouldBe false
    UClass.cyclicSubset(List(4, 8), List(4, 8, 8)) shouldBe true
    UClass.cyclicSubset(List(8, 4, 8), List(4, 8, 8)) shouldBe true
    UClass.cyclicSubset(List(3, 8), List(4, 8, 8)) shouldBe false
