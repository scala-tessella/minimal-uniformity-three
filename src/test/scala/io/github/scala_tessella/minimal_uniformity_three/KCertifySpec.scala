package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*
import io.github.scala_tessella.research_core.solver.*

import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** Fast teeth for [[KCertify]], the k-class generalization of the certified [[K2Certify]] encoding:
  *
  *   - REGRESSION at k = 2: `KCertify.enumerate(c, 2)` equals `K2Certify.enumerate(c)` op for op. The k = 2
  *     encoding is DRAT-certified (track A2), so this pins the generalization against a trusted reference
  *     rather than against the generator alone — and it exercises the parts that were rewritten (per-class
  *     counters instead of the complement trick, indicator coloring instead of one boolean, anchors as a
  *     list, the 7^k master constraint).
  *   - AGREEMENT at k = 3: the SAT enumeration equals the BFS relabelings of the tier-1 ≤ 3-orbit universe,
  *     op for op, at every C ≤ 7 — two independent enumerators, one universe, now including genuinely 3-orbit
  *     classes.
  *   - the orbit layer has TEETH at k = 3: a valid, connected FOUR-vertex-orbit D-set is excluded.
  *   - the tier-1 layer has TEETH at k = 3: every ≤ 3-orbit D-set failing the relaxation is excluded, and
  *     such D-sets exist in the raw slice (so the exclusion is not vacuous).
  *   - the STAIRCASE layer (`staircase = true`): the SAT enumeration equals the staircase-filtered universe
  *     op for op at k = 3 / C ≤ 7 and k = 2 / C ≤ 6 (the encoding's minimal certifiable weight per chamber is
  *     exactly the true bin floor — no cheating up or down), and every
  *     tier-1-feasible-but-staircase-infeasible D-set is excluded WITH the flag and included WITHOUT it (the
  *     flag off is byte-identical to the pre-staircase encoding by construction).
  */
class KCertifySpec extends AnyFlatSpec with Matchers:

  private def ops(ds: DSet): List[Int] = (1 to ds.size).flatMap(d => (0 to 2).map(i => ds.get(i, d))).toList

  private def universe(maxN: Int, maxSize: Int, tier1: Boolean): Vector[DSet] =
    val out = mutable.ArrayBuffer.empty[DSet]
    DelaneySymbols.relaxedOrbitBoundedDSets(
      maxN = maxN,
      maxSize = maxSize,
      parallelism = 1,
      sink = ds => out.synchronized(out += ds),
      tier1 = tier1
    )
    out.toVector

  "KCertify at k = 2" should "reproduce the certified K2Certify encoding, op for op" in:
    for c <- 1 to 6 do
      withClue(s"C=$c: ")(KCertify.enumerate(c, 2).map(ops).toSet shouldBe
        K2Certify.enumerate(c).map(ops).toSet)

  "KCertify at k = 3" should "agree with the tier-1 <= 3-orbit universe generator at C <= 7" in:
    val byC = universe(3, 7, tier1 = true).groupBy(_.size)
    for c <- 1 to 7 do
      val expected = byC.getOrElse(c, Vector.empty).flatMap(DelaneySymbols.bfsRelabelings).map(ops).toSet
      val models   = KCertify.enumerate(c, 3)
      withClue(s"C=$c: "):
        models.map(ops).toSet shouldBe expected
        models.size shouldBe expected.size

  it should "exclude a valid 4-vertex-orbit D-set (orbit layer has teeth)" in:
    // chambers 1..4: σ₀ = (12)(34), σ₁ = id, σ₂ = id — connected under σ₀, four singleton (1,2)-orbits
    val a                                 = Array.ofDim[Int](5, 3)
    def set(i: Int, d: Int, e: Int): Unit = { a(d)(i) = e; a(e)(i) = d }
    set(0, 1, 2); set(0, 3, 4)
    for d <- 1 to 4 do { set(1, d, d); set(2, d, d) }
    val ds                                = new DSet(a)
    DelaneySymbols.bfsRelabelings(ds).size should be > 0
    val models                            = KCertify.enumerate(4, 3).map(ops).toSet
    DelaneySymbols.bfsRelabelings(ds).map(ops).foreach(l => models should not contain l)

  it should "exclude tier-1-infeasible <= 3-orbit D-sets, non-vacuously" in:
    val raw        = universe(3, 6, tier1 = false)
    val infeasible = raw.filterNot(ds => DelaneySymbols.tier1Feasible(ds))
    infeasible should not be empty
    val modelsByC  = (1 to 6).map(c => c -> KCertify.enumerate(c, 3).map(ops).toSet).toMap
    for ds <- infeasible; l <- DelaneySymbols.bfsRelabelings(ds) do
      modelsByC(ds.size) should not contain ops(l)

  /** The T = 3 staircase feasibility, reimplemented from the involutions alone — independent of both the
    * encoding and the sizing probe's copy, so the agreement below is a three-way cross-check.
    */
  private def staircaseFeasible(ds: DSet): Boolean =
    def period(d: Int): Int =
      var e = ds.get(0, ds.get(1, d))
      var r = 1
      while e != d do { e = ds.get(0, ds.get(1, e)); r += 1 }
      r
    val wSum                = (1 to ds.size)
      .map(period)
      .map(r => if r == 1 || r == 3 then 0 else if r <= 5 then 1 else if r <= 11 then 2 else 3)
      .sum
    var vSum12              = 0
    val seen                = Array.fill(ds.size + 1)(false)
    for d <- 1 to ds.size if !seen(d) do
      var e       = d
      var k       = 1
      var len     = 0
      var isChain = false
      var go      = true
      while go do
        if !seen(e) then { seen(e) = true; len += 1 }
        val ek = ds.get(k, e)
        if ek == e then isChain = true
        e = ek
        k = 3 - k
        if e == d && k == 1 then go = false
      vSum12 +=
        (if isChain then if len == 1 then 4 else if len == 2 then 6 else 12
         else if len == 2 then 8
         else if len == 4 then 12
         else 24)
    wSum <= vSum12 - 2 * ds.size

  "KCertify with the staircase" should "agree with the staircase-filtered universe at k = 3, C <= 7" in:
    val byC = universe(3, 7, tier1 = true).filter(staircaseFeasible).groupBy(_.size)
    for c <- 1 to 7 do
      val expected = byC.getOrElse(c, Vector.empty).flatMap(DelaneySymbols.bfsRelabelings).map(ops).toSet
      val models   = KCertify.enumerate(c, 3, staircase = true)
      withClue(s"C=$c: "):
        models.map(ops).toSet shouldBe expected
        models.size shouldBe expected.size

  it should "agree with the staircase-filtered universe at k = 2, C <= 6" in:
    val byC = universe(2, 6, tier1 = true).filter(staircaseFeasible).groupBy(_.size)
    for c <- 1 to 6 do
      val expected = byC.getOrElse(c, Vector.empty).flatMap(DelaneySymbols.bfsRelabelings).map(ops).toSet
      withClue(s"C=$c: ")(KCertify.enumerate(c, 2, staircase = true).map(ops).toSet shouldBe expected)

  it should "exclude tier-1-feasible but staircase-infeasible D-sets, and only with the flag ON" in:
    val discriminators = universe(3, 7, tier1 = true).filterNot(staircaseFeasible)
    discriminators should not be empty // the sizing table says the two universes split by C = 6
    val onByC  = discriminators.map(_.size).distinct.sorted
      .map(c => c -> KCertify.enumerate(c, 3, staircase = true).map(ops).toSet)
      .toMap
    val offByC = discriminators.map(_.size).distinct.sorted
      .map(c => c -> KCertify.enumerate(c, 3).map(ops).toSet)
      .toMap
    for ds <- discriminators; l <- DelaneySymbols.bfsRelabelings(ds) do
      onByC(ds.size) should not contain ops(l)
      offByC(ds.size) should contain(ops(l))
