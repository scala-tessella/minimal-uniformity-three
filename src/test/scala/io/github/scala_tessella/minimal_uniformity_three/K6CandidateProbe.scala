package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{canonicalKey, DSymbol}
import io.github.scala_tessella.minimal_uniformity_three.StrictWitnesses.symbolFromKey
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** THE SIX-ORBIT (4.5.20) CANDIDATES OF THE k ≤ 6 WINDOW, verified to the end (guarded: -Dk6.verify;
  * -Dk6.verify.radius=<r>, default 14; -Dk6.verify.z=<species>, default 4.5.20; -Dk6.verify.n=<orbits>,
  * default 6).
  *
  * The three-letter window (`UClassK3ShardProbe -Duclass.k3s.maxN=6 -Duclass.k3s.v2 -Duclass.k3s.vcap=6
  * -Duclass.k3s.shapes3`, 1–36 chambers) is complete for uniformity ≤ 6 by lem:chambers, and
  * `StrictRefilterProbe` leaves (4.5.20) ten rigid six-orbit designations: four fail exact closure, two are
  * unsaturated, and four are closed and strict-valid but their radius-10 development neither certifies a
  * translation cell (20-gons are large) nor gets a saturation verdict (the re-filter skips tiles above area
  * 12). This probe re-derives every strict designation of every six-orbit symbol of the window and verifies
  * each rigid one at a larger radius with the saturation check forced: exact closure, strict validity,
  * embedding, a species vertex, `TilePatch.admissible` (empty ⇔ condition (4)), the certified cell and the
  * minimal symbol. A saturated, realized survivor is a member of uniformity 6; none means (4.5.20) = 7.
  */
class K6CandidateProbe extends AnyFlatSpec with Matchers:

  private val hitsFiles = List(
    "certs/uclass-k6/window-hits-min1-maxSize24-v2-shapes3.tsv",
    "certs/uclass-k6/window-hits-min25-maxSize36-v2-shapes3.tsv",
    "certs/uclass-k7/window-hits-min1-maxSize42-v2-shapes3.tsv",
    "certs/uclass-k8/window-hits-min1-maxSize48-v2-shapes3.tsv",
    "certs/uclass-k9/window-hits-min1-maxSize54-v2-shapes3.tsv"
  )

  private def field(line: String, name: String): String =
    line.split('\t').find(_.startsWith(name + "=")).map(_.drop(name.length + 1)).getOrElse("")

  behavior of "the six-orbit candidates of the three-letter window"

  it should "verify every rigid strict designation at a larger radius, saturation included" in:
    assume(sys.props.contains("k6.verify"), "guarded — enable with -Dk6.verify")
    val radius = sys.props.get("k6.verify.radius").fold(14.0)(_.toDouble)
    val zName  = sys.props.get("k6.verify.z").getOrElse("4.5.20")
    val nWant  = sys.props.get("k6.verify.n").fold(6)(_.toInt)
    val z      = zName.split('.').map(_.toInt).toList
    val keys   =
      (for
        f    <- hitsFiles
        line <- Files.readAllLines(Path.of(f)).asScala if line.startsWith("z=")
        if field(line, "z") == zName && field(line, "n") == nWant.toString
      yield field(line, "key").stripSuffix(";")).distinct
    println(s"z=$zName n=$nWant: ${keys.size} symbols in the window; radius $radius")
    var idx    = 0
    for key <- keys do
      val ds   = symbolFromKey(key)
      val sys  = MetricLayer.angleSystem(ds)
      val regs = UClass.candidates(ds, z, strict = true, isolated = true).filter { r =>
        val irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !r(k) => k }
        UClass.noneForcedRegular(ds, r, irregular)
      }
      for reg <- regs do
        idx += 1
        val label = s"[$idx] C=${ds.size} reg=${reg.toList.sorted.mkString(",")}"
        val rows  = MetricLayer.designatedRows(ds, reg)
        val rigid = MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows)).isEmpty
        MetricLayer.particularSolution(rows, sys.vars) match
          case None              => println(s"$label: linear layer inconsistent — DEAD")
          case Some(_) if !rigid => println(s"$label: FLEXIBLE — not expected here")
          case Some(x)           =>
            val residual = MetricLayer.maxClosureResidual(ds, x.map(_.toDouble))
            if residual > 1e-9 then println(f"$label: closure residual $residual%.3e — DEAD")
            else
              val t0      = System.nanoTime
              val re      =
                TilePatch.seed(ExactDeveloper.develop(ds, x, radius), z, strict = true, isolated = true)
              val species = re.interiorWords.values.count(w =>
                w.forall(_.isDefined) && UClass.strictArcLegal(w, z) && w.length == z.length
              )
              if !TilePatch.valid(re) then println(s"$label: INVALID at radius $radius — DEAD")
              else if !re.tiles.forall(_.poly.isSimpleCertified) then
                println(s"$label: EMBEDDING FAILURE at radius $radius — DEAD")
              else
                val irr    =
                  re.tiles.map(_.poly).filter(TilePatch.regularSizeOf(_).isEmpty).groupBy(TilePatch.shapeKey)
                val shapes = irr.values.map { g =>
                  val t = g.head
                  f"${t.dirs.length}-gon(${t.interiorAngles.map(_ * 360 / t.n).sorted.mkString("/")})A=${t.areaApprox}%.3f"
                }.mkString("; ")
                val moves  = TilePatch.admissible(re)
                val cell   = Periodicity.certifiedCell(re).flatMap { (t1, t2, c) =>
                  SymbolExtractor.torusSymbol(re, t1, t2).map { torus =>
                    val min = SymbolExtractor.minimalImage(torus)
                    (
                      min.size,
                      min.orbs.count(_.i == 1),
                      min.canonicalKey == SymbolExtractor.minimalImage(ds).canonicalKey
                    )
                  }
                }
                println(
                  f"$label: closed, valid, embedded; tiles=${re.tiles.length} species=$species " +
                    f"SATURATED=${moves.isEmpty} (${moves.length} admissible splits) " +
                    f"cell=${cell.fold("NONE (no certified cell or torus symbol)")((c, k, iso) =>
                        s"minimal C=$c k=$k ${if iso then "= the symbol" else "≠ the symbol"}"
                      )} " +
                    f"irregular: $shapes [${(System.nanoTime - t0) / 1e9}%.0fs]"
                )
                if moves.nonEmpty then moves.take(3).foreach(m => println(s"    split: $m"))
