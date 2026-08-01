package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.DelaneySymbols
import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.{AtomicLong, AtomicLongArray}

/** ADR-0009 paper certification, track A2 — SIZING scan for the k ≤ 2 certification universe (all complete
  * D-sets, ≤ 2 vertex orbits, canonically labeled, NO curvature pruning): counts per chamber count C up to
  * `-Dcert.k2.size=<maxSize>` (default 14). The numbers decide the obligation architecture: per-C blocking à
  * la track A is feasible only where the labeling counts stay within DIMACS/kissat reach, and the growth
  * ratio per two chambers extrapolates the top slices. Two modes:
  *
  *   - RAW (default): the unpruned ≤ 2-orbit universe, with per-D-set evaluation of the tier-1 curvature
  *     relaxation ([[DelaneySymbols.tier1Feasible]]) and of exact euclidean feasibility — asserting the
  *     TIER-1 LEMMA implementation on every D-set (euclidean-feasible ⇒ tier-1) and reporting both cuts;
  *   - TIER-1 (`-Dcert.k2.tier1`): generation with the tier-1 tree prune + filter — the actual A2
  *     certification universe, reachable at larger maxSize than the raw walk.
  *
  * Cross-checks: the 1-orbit slice at C ≤ 12 must reproduce the per-C counts of the certified track-A
  * universe ([[DelaneySymbols.relaxedDSets]], 2178 in total) — in RAW mode directly; and in both modes the
  * euclidean slice is a subset of the tier-1 slice.
  */
class K2UniverseSizingProbe extends AnyFlatSpec with Matchers:

  /** Number of ⟨σ₁,σ₂⟩-orbits (vertices) of a complete D-set. */
  private def vertexOrbitCount(ds: DSet): Int =
    val seen  = Array.fill(ds.size + 1)(false)
    var count = 0
    for d <- 1 to ds.size if !seen(d) do
      count += 1
      val stack = collection.mutable.ArrayDeque(d)
      seen(d) = true
      while stack.nonEmpty do
        val e = stack.removeLast()
        for i <- 1 to 2 do
          val ei = ds.get(i, e)
          if !seen(ei) then { seen(ei) = true; stack.append(ei) }
    count

  /** Exact euclidean feasibility (max curvature ≥ 0), reimplemented locally in twelfths as an independent
    * check of the tier-1 lemma: Σ over (0,1)- and (1,2)-orbits of 12·k/minV ≥ 6C.
    */
  private def euclideanFeasibleLocal(ds: DSet): Boolean =
    var sum12 = 0.0
    for (i, j) <- List((0, 1), (1, 2)) do
      val seen = Array.fill(ds.size + 1)(false)
      for d <- 1 to ds.size if !seen(d) do
        var e       = d
        var k       = i
        var len     = 0
        var isChain = false
        var go      = true
        while go do
          if !seen(e) then { seen(e) = true; len += 1 }
          val ek = ds.get(k, e)
          if ek == e then isChain = true
          e = ek
          k = i + j - k
          if e == d && k == i then go = false
        val r       = if isChain then len else (len + 1) / 2
        val minV    = math.ceil(3.0 / r).toInt
        sum12 += 12.0 * (if isChain then 1 else 2) / minV
    sum12 >= 6.0 * ds.size - 1e-9

  it should "size the k <= 2 certification universe per chamber count (enable with -Dcert.k2.size)" in:
    assume(sys.props.contains("cert.k2.size"), "sizing scan — enable with -Dcert.k2.size[=maxSize]")
    val maxSize      = sys.props.get("cert.k2.size").filter(_.nonEmpty).fold(14)(_.toInt)
    val tier1Mode    = sys.props.contains("cert.k2.tier1")
    val bySize       = new AtomicLongArray(maxSize + 1)
    val oneOrbBySize = new AtomicLongArray(maxSize + 1)
    val tier1BySize  = new AtomicLongArray(maxSize + 1)
    val euclBySize   = new AtomicLongArray(maxSize + 1)
    val lemmaBreaks  = new AtomicLong(0)
    val total        = DelaneySymbols.relaxedOrbitBoundedDSets(
      maxN = 2,
      maxSize = maxSize,
      sink = ds =>
        bySize.incrementAndGet(ds.size)
        if vertexOrbitCount(ds) == 1 then oneOrbBySize.incrementAndGet(ds.size)
        val t1 = tier1Mode || DelaneySymbols.tier1Feasible(ds)
        if t1 then tier1BySize.incrementAndGet(ds.size)
        if euclideanFeasibleLocal(ds) then
          euclBySize.incrementAndGet(ds.size)
          if !t1 then lemmaBreaks.incrementAndGet() // euclidean-feasible must imply tier-1
      ,
      log = println,
      tier1 = tier1Mode
    )
    println(s"UNIVERSE (maxN=2, maxSize=$maxSize, tier1Mode=$tier1Mode): $total D-sets")
    println("C\tdsets\t1-orbit\ttier1\teuclidean")
    for c <- 1 to maxSize do
      println(s"$c\t${bySize.get(c)}\t${oneOrbBySize.get(c)}\t${tier1BySize.get(c)}\t${euclBySize.get(c)}")
    lemmaBreaks.get shouldBe 0
    // regression gate (raw mode): the 1-orbit slice is the certified track-A universe, per chamber count
    if !tier1Mode then
      val k1 = collection.mutable.Map.empty[Int, Long].withDefaultValue(0L)
      DelaneySymbols.relaxedDSets(math.min(maxSize, 12)).foreach(ds => k1(ds.size) += 1)
      for c <- 1 to math.min(maxSize, 12) do
        withClue(s"1-orbit slice C=$c: ")(oneOrbBySize.get(c) shouldBe k1(c))
      if maxSize >= 12 then k1.values.sum shouldBe 2178
