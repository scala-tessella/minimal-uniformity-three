package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE k ≤ 2 REFUTATION. The exhaustive ≤ 24-chamber scan ([[UClassK2Probe]]; catalogue: 1363 relaxed symbols
  * with ≤ 2 vertex orbits, archived under `certs/uclass-k2-record/`) produced exactly ONE combinatorial +
  * pinned-linear survivor across all ten targets — for z = (3.4.4.6), the 12-chamber symbol pinned below by
  * its canonical key, with vertex orbits (3.4.6.4) and (3.4.4.6) and one of its two hexagon orbits designated
  * irregular. This spec REFUTES it at the linear level: the designated hexagon is FORCED regular (60 + 90 + γ
  * + 90 = 360 at every corner of the (3.4.6ᵢ.4) vertex, and an equiangular equilateral hexagon is regular),
  * so every realization's truth-designation is the all-regular one, which fails the cyclic-subset constraint.
  * Hence the paper's lower bound at two orbits: NO tiling in U(z) has uniformity ≤ 2, for any of the ten
  * non-Archimedean species — every minimal uniformity in the table is at least 3.
  */
class UClassK2VerdictSpec extends AnyFlatSpec with Matchers:

  private val survivorKey =
    "1,1,2,6|4;2,3,1,4|4;4,2,5,4|4;3,6,7,4|4;7,5,3,3|4;6,4,8,4|4;5,9,4,3|4;8,10,6,4|4;9,7,11,3|4;10,8,12,4|4;11,12,9,6|4;12,11,10,6|4"

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

  private lazy val ds = symbolFromKey(survivorKey)
  private val z       = List(3, 4, 4, 6)

  behavior of "the single two-orbit survivor"

  it should "reconstruct faithfully from its canonical key" in:
    DelaneySymbols.canonicalKey(ds) shouldBe survivorKey + ";"
    DelaneySymbols.isEuclidean(ds) shouldBe true

  it should "have exactly the campaign's surviving designation, and for no other target" in:
    UClass.candidates(ds, z) shouldBe List(Set(1, 2, 3, 4))
    UClass.targets.filterNot(_._1 == z).foreach: (other, _) =>
      withClue(s"z=${other.mkString(".")}: "):
        UClass.candidates(ds, other) shouldBe empty

  it should "be refuted: the designated-irregular hexagon is forced regular by the pinned system" in:
    val reg       = Set(1, 2, 3, 4)
    val irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !reg(k) => k }
    irregular should have size 1
    UClass.forcedRegular(ds, reg, irregular.head) shouldBe true
