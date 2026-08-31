package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

/** THE VALENCE-2 GATE (`-Dcert.v2.gate[=maxN]`, default 2) — the k ≤ maxN / C ≤ 12·maxN re-sweep over the
  * valence-2 extended class, where vertices may have degree 2 while faces keep ≥ 3 sides. It (a) proves the
  * flag-off world is untouched and (b) surfaces every NEW hit the extended class admits, i.e. the candidates
  * that could threaten the paper's values. The extension matters: three of the paper's witnesses carry
  * valence-2 vertex orbits.
  *
  *   - PIN (flag off): the euclid walk at maxN = 2 / C ≤ 24 must emit exactly 917 D-sets, and the flag-on
  *     universe must CONTAIN the flag-off one (supersets by construction — a violation is a table bug);
  *   - GATE (flag on): the U-scan (combinatorial + pinned-linear + un-forced, exactly the sharded sweep's
  *     layer stack) runs over the extended symbol catalogue — `euclideanSymbolsOf(valence2 = true)` emits m₁₂ =
  *     2 symbols — and every hit ABSENT from the flag-off scan is printed in full: those are the valence-2
  *     candidates, to be resolved at closure level before any row is called safe.
  */
class Valence2GateProbe extends AnyFlatSpec with Matchers:

  private def opLine(ds: DSet): String = (1 to ds.size).map(d =>
    s"${ds.get(0, d)},${ds.get(1, d)},${ds.get(2, d)}"
  ).mkString(";")

  it should "re-secure the small-k rows in the valence-2 world (enable with -Dcert.v2.gate)" in:
    assume(sys.props.contains("cert.v2.gate"), "valence-2 gate — enable with -Dcert.v2.gate[=maxN]")
    val maxN    = sys.props.get("cert.v2.gate").filter(_.nonEmpty).fold(2)(_.toInt)
    // `.size` caps the window below the 12·maxN bound (the 25–36 band of the k = 3 gate belongs to the
    // sharded walk, not to this in-memory probe)
    val maxSize = sys.props.get("cert.v2.gate.size").filter(_.nonEmpty).fold(12 * maxN)(_.toInt)

    def universe(v2: Boolean): Vector[DSet] =
      val out = new java.util.concurrent.ConcurrentLinkedQueue[DSet]
      DelaneySymbols.relaxedOrbitBoundedDSets(
        maxN = maxN,
        maxSize = maxSize,
        sink = ds => out.add(ds),
        log = println,
        euclid = true,
        valence2 = v2
      )
      out.asScala.toVector.sortBy(ds => (ds.size, opLine(ds)))

    def hits(dsets: Vector[DSet], v2: Boolean): Vector[String] =
      val all = DelaneySymbols.euclideanSymbolsOf(dsets, maxN = maxN, valence2 = v2)
      val out =
        for
          (t, ds)  <- all.toVector
          (z, cl)  <- UClass.targets
          reg      <- UClass.candidates(ds, z)
          irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !reg(k) => k }
          if UClass.noneForcedRegular(ds, reg, irregular)
        yield s"z=${z.mkString(".")}\tclaimed=$cl\tn=${t.n}\tchambers=${ds.size}\t" +
          s"sigs=${t.vertices.map(_.mkString("."))}\tregularOrbits=${reg.toList.sorted.mkString(",")}\t" +
          s"irregularOrbits=${irregular.mkString(",")}\tkey=${DelaneySymbols.canonicalKey(ds)}"
      out.sorted

    val base = universe(v2 = false)
    println(s"BASELINE euclid universe (maxN=$maxN, C<=$maxSize, valence2=false): ${base.size} D-sets")
    if maxN == 2 && maxSize == 24 then base.size shouldBe 917 // the pinned count that gates the walk
    val ext      = universe(v2 = true)
    println(s"EXTENDED euclid universe (valence2=true): ${ext.size} D-sets (+${ext.size - base.size})")
    val baseKeys = base.map(opLine).toSet
    ext.map(opLine).toSet should contain allElementsOf baseKeys // superset — tables only weaken

    val baseHits = hits(base, v2 = false)
    val extHits  = hits(ext, v2 = true)
    println(s"U-scan hits: baseline=${baseHits.size} extended=${extHits.size}")
    val newHits  = extHits.filterNot(baseHits.toSet)
    println(s"NEW valence-2 hits: ${newHits.size}")
    newHits.foreach(println)
