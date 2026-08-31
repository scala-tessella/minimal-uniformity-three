package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import io.github.scala_tessella.minimal_uniformity_three.StrictWitnesses.{
  entries, solution, symbolFromKey, Entry
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE STRICT-CLASS WITNESSES, certified (default tier): for every [[StrictWitnesses]] entry — canonical-key
  * round trip and vertex-orbit count; the designation is admitted under the class as defined (contiguous arc,
  * at most one irregular tile per vertex) and genuine; the corner solution is honest and closes exactly; the
  * radius-10 development is legal and embedded with a species vertex; the certified translation cell's
  * minimal symbol IS the entry; and the verdicts the manuscript rows rest on — convexity, the irregular tile
  * classes, essential irregularity by the complete union decision on the ζ₁₂ pieces, and saturation under
  * BOTH the class as defined and the spliced reading (the pinwheel is the instructive case: unsaturated
  * spliced, saturated as defined).
  */
class StrictWitnessSpec extends AnyFlatSpec with Matchers:

  private val pieces = List(3, 4, 6, 12)

  /** `n-gon(a^k/b^l/…)`: the corner multiset in degrees, run-length compressed. */
  private def compact(t: UnitPolygon): String =
    val degs = t.interiorAngles.map(_ * 360 / t.n).sorted
    val runs = degs.foldLeft(List.empty[(Int, Int)]):
      case ((a, k) :: rest, d) if a == d => (a, k + 1) :: rest
      case (acc, d)                      => (d, 1) :: acc
    s"${t.dirs.length}-gon(${runs.reverse.map((a, k) => if k == 1 then s"$a" else s"$a^$k").mkString("/")})"

  private def genuineStrict(ds: DelaneySymbols.DSymbol, z: List[Int]): List[Set[Int]] =
    UClass.candidates(ds, z, strict = true, isolated = true).filter: r =>
      val irr = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !r(k) => k }
      UClass.noneForcedRegular(ds, r, irr)

  private def realized(e: Entry) =
    val ds = symbolFromKey(e.key)
    val x  = solution(e, ds).getOrElse(fail(s"${e.name}: no solution for the designation"))
    (ds, x, TilePatch.seed(ExactDeveloper.develop(ds, x, 10.0), e.z, strict = true, isolated = true))

  behavior of "the strict-class witnesses (2026-08-30)"

  for e <- entries do
    it should s"certify ${e.name}" in:
      val ds          = symbolFromKey(e.key)
      DelaneySymbols.canonicalKey(ds).stripSuffix(";") shouldBe e.key.stripSuffix(";")
      ds.orbs.count(_.i == 1) shouldBe e.k
      genuineStrict(ds, e.z) should contain(e.regular)
      val x           = solution(e, ds).getOrElse(fail("no solution"))
      x.forall { g =>
        val d = g.toDouble; d > 1e-9 && d < 2 - 1e-9 && math.abs(d - 1) > 1e-9
      } shouldBe true
      MetricLayer.maxClosureResidual(ds, x.map(_.toDouble)) should be < 1e-9
      val re          = TilePatch.seed(ExactDeveloper.develop(ds, x, 10.0), e.z, strict = true, isolated = true)
      TilePatch.valid(re) shouldBe true
      re.tiles.forall(_.poly.isSimpleCertified) shouldBe true
      re.interiorWords.values.count(w =>
        w.forall(_.isDefined) && w.length == e.z.length && UClass.isolatedLegal(w, e.z)
      ) should be > 0
      // the verdicts
      re.tiles.forall(_.poly.interiorAngles.forall(_ < re.n / 2)) shouldBe e.convex
      val irr         = re.tiles.map(_.poly).filter(TilePatch.regularSizeOf(_).isEmpty).groupBy(TilePatch.shapeKey)
      irr.values.map(g => compact(g.head)).toList.sorted.mkString("; ") shouldBe e.irregular
      irr.values.forall(g => !Defusion.regularUnion(g.head, pieces)) shouldBe e.essential
      TilePatch.admissible(re).isEmpty shouldBe e.strictSaturated
      TilePatch.admissible(re.copy(strict = false, isolated = false)).isEmpty shouldBe e.splicedSaturated
      // the certified cell reproduces the symbol
      val (t1, t2, _) = Periodicity.certifiedCell(re).getOrElse(fail("no certified cell at radius 10"))
      val min         = SymbolExtractor.minimalImage(SymbolExtractor.torusSymbol(re, t1, t2).get)
      DelaneySymbols.canonicalKey(min).stripSuffix(";") shouldBe e.key.stripSuffix(";")
      min.orbs.count(_.i == 1) shouldBe e.k

  it should "describe a symbol under the class (enable with -Dstrict.describe=<key>)" in:
    assume(sys.props.contains("strict.describe"), "describe — enable with -Dstrict.describe=<key>")
    val key = sys.props("strict.describe")
    val z   = sys.props.get(
      "strict.describe.z"
    ).filter(_.nonEmpty).getOrElse("3.3.6.6").split('.').map(_.toInt).toList
    val ds  = symbolFromKey(key)
    println(
      s"chambers=${ds.size} k=${ds.orbs.count(_.i == 1)} canonical=${DelaneySymbols.canonicalKey(ds).stripSuffix(";") == key.stripSuffix(";")}"
    )
    for reg <- genuineStrict(ds, z) do
      val sys   = MetricLayer.angleSystem(ds)
      val rows  = MetricLayer.designatedRows(ds, reg)
      val basis = MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows))
      println(s"designation regular=${reg.toList.sorted.mkString(",")} dim=${basis.size}")
      if basis.isEmpty then
        val x   = MetricLayer.particularSolution(rows, sys.vars).get
        val re  = TilePatch.seed(ExactDeveloper.develop(ds, x, 10.0), z, strict = true, isolated = true)
        val irr = re.tiles.map(_.poly).filter(TilePatch.regularSizeOf(_).isEmpty).groupBy(TilePatch.shapeKey)
        println(
          s"  closure=${MetricLayer.maxClosureResidual(ds, x.map(_.toDouble))} valid=${TilePatch.valid(re)} embedded=${re.tiles.forall(_.poly.isSimpleCertified)}"
        )
        println(s"  convex=${re.tiles.forall(_.poly.interiorAngles.forall(_ <
            re.n / 2))} irregular=${irr.values.map(g => compact(g.head)).toList.sorted.mkString("; ")}")
        println(
          s"  essential=${irr.values.forall(g => !Defusion.regularUnion(g.head, pieces))} saturated(as defined)=${TilePatch.admissible(re).isEmpty} saturated(spliced)=${TilePatch.admissible(re.copy(strict = false, isolated = false)).isEmpty}"
        )
        println(s"  vertex words: ${re.interiorWords.values.map(w =>
            w.map(_.fold("i")(_.toString)).mkString("(", ".", ")")
          ).toList.distinct.sorted.mkString(" ")}")
