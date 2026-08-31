package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE STRICT CONTIGUOUS-ARC READING of condition (3) against the banked patterns — the known-answer fixture
  * for [[UClass.strictArcLegal]]. `UClass.cyclicSubset` rotates the spliced regular sub-configuration before
  * matching (the manuscript's `def:rvs` semantics); the strict reading demands that the regular tiles form
  * one contiguous run around the vertex AND that the run, read in order, is an arc of $z$ — an arc has
  * endpoints, so no rotation. The 2026-08-30 audit found the two readings to diverge on exactly the species
  * with a REPEATED letter, where splicing out an irregular tile can manufacture an adjacency $z$ lacks:
  * $(3.4.4_i.4)$ splices to $(4.3.4)$, which is not an arc of $(3.4.4.6)$. Eight of the twenty-one banked
  * patterns fail, all in $(3.4^2.6)$, $(3^2.6^2)$, $(3^2.4.12)$ and $(5^2.10)$; every distinct-letter species
  * passes, and so do the 2026-08-29 patterns behind $(3.10.15) = 5$, $(3.7.42) \\le 10$, $(4.5.20) \\le 7$.
  */
class StrictArcSpec extends AnyFlatSpec with Matchers:

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
      val o = orbs(k); val d = o.elements.head
      (if o.i == 0 then m01(d) else m12(d)) / o.r
    new DSymbol(dset, orbs, index, vs)

  /** The banked patterns whose TRUTH designation (the widest genuine one — the realized tiling) fails the
    * strict reading (2026-08-30 audit), with the offending arcs.
    */
  private val strictFailures: Map[String, String] = Map(
    "(3.4.4.6) w k=3"                  -> "4.3.4, 4.6.4",
    "(3.3.6.6) w k=3"                  -> "6.3.6",
    "(3.4.4.6) 22ch cand2"             -> "3.4.6",
    "(3.4.4.6) saturated k=4 reflex-a" -> "4.6.4",
    "(3.4.4.6) saturated k=4 convex"   -> "4.3.4",
    "(3.4.4.6) saturated k=4 reflex-b" -> "4.6.4",
    "(5.5.10) penrose-rhombi k=5"      -> "split arc 5.5",
    "(3.3.4.12) uplus pattern k=7"     -> "4.3.12"
  )

  /** Of those, the SYMBOLS that keep no strict designation at all. The other four keep a smaller one — a
    * further face orbit declared irregular restores an arc, and whether that designation is realizable (the
    * extra face genuinely non-regular in a closed configuration) is the closure question
    * [[StrictRefilterProbe]] answers.
    */
  private val symbolDead: Set[String] = Set(
    "(3.4.4.6) w k=3",
    "(3.3.6.6) w k=3",
    "(3.4.4.6) 22ch cand2",
    "(5.5.10) penrose-rhombi k=5"
  )

  private def genuine(ds: DSymbol, z: List[Int], strict: Boolean): List[Set[Int]] =
    UClass.candidates(ds, z, strict).filter: r =>
      val irr = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !r(k) => k }
      UClass.noneForcedRegular(ds, r, irr)

  behavior of "the strict contiguous-arc reading on the banked patterns"

  it should "keep every banked pattern a candidate under the default (spliced) reading" in:
    for (name, z, key) <- SaturationProbe.entries do
      withClue(s"$name: ")(genuine(symbolFromKey(key), z, strict = false) should not be empty)

  it should "only ever shrink the designation list, and empty it for exactly four symbols" in:
    val dead = SaturationProbe.entries.flatMap: (name, z, key) =>
      val ds    = symbolFromKey(key)
      val d     = genuine(ds, z, strict = false)
      val st    = genuine(ds, z, strict = true)
      val truth = d.maxBy(_.size)
      withClue(s"$name: ")(d should contain allElementsOf st)
      withClue(s"$name: ")(st.contains(truth) shouldBe !strictFailures.contains(name))
      Option.when(st.isEmpty)(name)
    dead.toSet shouldBe symbolDead

  it should "name the offending arcs — the strict class differs on repeated letters only" in:
    for (name, z, key) <- SaturationProbe.entries do
      val ds  = symbolFromKey(key)
      // the widest default designation, as the audit read it
      val reg = genuine(ds, z, strict = false).maxBy(_.size)
      val bad = (for
        o   <- ds.orbs if o.i == 1
        cfg  = DelaneySymbols.vertexConfigOrbits(ds, o.elements.head).get.toVector
        word = cfg.map((f, p) => Option.when(reg(f))(p))
        if !UClass.strictArcLegal(word, z)
      yield
        val n      = word.length
        val regPos = word.indices.filter(i => word(i).isDefined)
        regPos.filter(i => word((i + n - 1) % n).isEmpty) match
          case Seq(s0) => (0 until regPos.size).map(k => word((s0 + k) % n).get).mkString(".")
          case _       => s"split arc ${regPos.map(i => word(i).get).mkString(".")}"
      ).distinct
      withClue(s"$name: "):
        bad.mkString(", ") shouldBe strictFailures.getOrElse(name, "")
        if bad.nonEmpty then z.distinct.length should be < z.length
