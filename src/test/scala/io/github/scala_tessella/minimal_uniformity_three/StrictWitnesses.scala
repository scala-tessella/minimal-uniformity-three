package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}

/** THE STRICT-CLASS WITNESSES — the patterns that carry the manuscript's upper bounds under the strict
  * contiguous-arc reading of condition (3) ([[UClass.strictArcLegal]]), found by the 2026-08-30 re-filter
  * ([[StrictRefilterProbe]]) of the banked k ≤ 4 hits. Each entry names its symbol (canonical key), the
  * regular designation the strict reading admits, and — for the one flexible family — the corner PIN that
  * fixes the parameter: the first chamber of the irregular face orbit of the given size takes the given angle
  * (in π units). The expected verdicts are pinned by [[StrictWitnessSpec]].
  */
object StrictWitnesses:

  final case class Entry(
      name: String,
      z: List[Int],
      key: String,
      regular: Set[Int],
      pin: Option[(Int, Frac)],
      k: Int,
      convex: Boolean,
      essential: Boolean,
      strictSaturated: Boolean,
      splicedSaturated: Boolean,
      irregular: String
  )

  private def banked(name: String): String = SaturationProbe.entries.find(_._1 == name).get._3

  val entries: List[Entry] = List(
    // (3.4^2.6) = 3 in the strict class: the reflex pinwheel of prop:reflex. Its 90°/210° dodecagon is not
    // a union of regular polygons (prop:pinwheel), and under the strict reading the square that split off
    // legally in the spliced class no longer does, so the pattern is strictly SATURATED: the U, U+ and
    // saturated rows all read 3.
    Entry(
      "(3.4.4.6) strict pinwheel k=3 C22",
      List(3, 4, 4, 6),
      banked("(3.4.4.6) 22ch cand1"),
      Set(0, 1, 2),
      None,
      k = 3,
      convex = false,
      essential = true,
      strictSaturated = true,
      splicedSaturated = false,
      irregular = "12-gon(90^6/210^6)"
    ),
    // the second 22-chamber reflex pattern: the (90,90,210,210)^3 dodecagon, a union of three squares and
    // a hexagon — it satisfies (1)–(3) at uniformity 3 but not condition (4): a square splits off, so it
    // is NOT a member of U(3.4^2.6) as defined (kept as the spliced-reading contrast: unsaturated there too)
    Entry(
      "(3.4.4.6) strict fused-dodecagon k=3 C22",
      List(3, 4, 4, 6),
      banked("(3.4.4.6) 22ch cand3"),
      Set(1, 2, 3, 4),
      None,
      k = 3,
      convex = false,
      essential = false,
      strictSaturated = false,
      splicedSaturated = false,
      irregular = "12-gon(90^6/210^6)"
    ),
    // (3^2.6^2) <= 5: the first member of the class, found by the lattice search (Lattice3366Probe) on the
    // 3 x 3 torus; regular triangles and hexagons around a reflex hexagon (60^2 120^3 240) — a union of four
    // unit triangles, as thm:uplus3366 says every irregular tile of the species is — SATURATED under the
    // definition; five vertex orbits (6.3.3.6), (3.3.T), (6.6.3.T) and TWO of (6.6.T), one on the tile's mirror
    // (ReflexTile3366Spec reads them off the symbol: 4, 3, 8, 3 and 6 chambers)
    Entry(
      "(3.3.6.6) strict lattice k=5 C24",
      List(3, 3, 6, 6),
      "2,1,3,6|3;1,4,5,6|3;5,6,1,6|3;7,2,8,6|3;3,9,2,6|3;10,3,6,6|3;4,11,12,6|4;12,13,4,6|3;14,5,13,6|3;" +
        "6,15,10,6|4;16,7,17,6|4;8,14,7,6|4;18,8,9,6|3;9,12,18,6|4;19,10,20,6|4;11,16,21,6|3;21,22,11,3|4;" +
        "13,19,14,6|4;15,18,22,6|4;22,23,15,3|4;17,24,16,3|3;20,17,19,3|4;24,20,23,3|4;23,21,24,3|3",
      Set(1, 2),
      None,
      k = 5,
      convex = false,
      essential = false,
      strictSaturated = true,
      splicedSaturated = true,
      irregular = "6-gon(60^2/120^3/240)"
    )
  )

  def symbolFromKey(key: String): DSymbol =
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

  /** The exact corner solution of an entry: the unique point of a rigid designation, or the pinned point of
    * the one-parameter family. None if the designation is not admitted, not of the expected dimension, or the
    * pin cannot be met.
    */
  def solution(e: Entry, ds: DSymbol): Option[Array[Frac]] =
    val sys   = MetricLayer.angleSystem(ds)
    val rows  = MetricLayer.designatedRows(ds, e.regular)
    val basis = MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows))
    (e.pin, basis.size) match
      case (None, 0)                => MetricLayer.particularSolution(rows, sys.vars)
      case (Some((size, angle)), 1) =>
        for
          x0  <- MetricLayer.particularSolution(rows, sys.vars)
          v    = basis.head
          orb <- ds.orbs.zipWithIndex.collectFirst:
                   case (o, k) if o.i == 0 && !e.regular(k) && ds.m(0, 1, o.elements.head) == size => o
          c    = sys.corner(orb.elements.head)
          if v(c).num != 0
          d    = angle + Frac.make(-x0(c).num, x0(c).den)
          t    = Frac.make(d.num * v(c).den, d.den * v(c).num)
        yield Array.tabulate(sys.vars)(j => x0(j) + t * v(j))
      case _                        => None
