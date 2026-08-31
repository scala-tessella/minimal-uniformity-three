package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import io.github.scala_tessella.research_core.TilePatch.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*

/** THE JOINT (MULTI-SPLIT) SATURATION AUDIT (enable with `-Djoint.sat`): the paper's refinement definition
  * admits splitting SEVERAL regular polygons off simultaneously, while the standing certificates test single
  * moves. The closure rests on three facts: refinement legality is VERTEX-LOCAL; every split piece meets the
  * ORIGINAL tile's boundary edge-to-edge, so it is one of the flush candidates; and the census shows no two
  * irregular tiles share a vertex in any banked pattern (the Penrose exception has zero candidates) — so
  * joint repair can only happen among candidates of ONE tile.
  *
  * A multi-split of one tile is exactly an ITERATED flush split: apply candidates one at a time to successive
  * remainders, admitting only pieces whose flush arc lies on the original boundary. The audit therefore runs
  * a DFS over those iterated splits per irregular class (translation-lattice representative with fully
  * interior vertices), testing every reached state with the full patch validity — the complete joint word
  * test at every affected vertex, seam vertices included. Zero legal states of size ≥ 1 certifies saturation
  * under the full definition; known-unsaturated entries double as calibration (they must show a legal size-1
  * state). The three flexible (3²6²) witnesses are audited from their realized parameters the same way.
  */
class JointSaturationProbe extends AnyFlatSpec with Matchers:

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

  final private case class Node(pieces: Vector[PlacedTile], rems: Vector[PlacedTile])

  /** flushSplits is placement-independent — memoize per polygon shape (the DFS revisits shapes massively).
    * The enumeration can explode on giant tiles (the (3.10.15) 36-gon ran > 4 h): bound it with a wall-clock
    * budget; a timed-out shape is negative-cached and its audit reported as GEOMETRIC-TIMEOUT, never as
    * saturated.
    */
  private val splitsMemo =
    collection.mutable.Map.empty[(String, List[Int]), Option[Vector[Defusion.FlushSplit]]]

  private def gonArea(q: Int): Double = q / (4.0 * math.tan(math.Pi / q))

  private def splitsOf(poly: ExactPlane.UnitPolygon, qs: List[Int]): Option[Vector[Defusion.FlushSplit]] =
    if poly.dirs.length > 24 then None // giant tiles: enumeration infeasible — report deferral, never race it
    else
      // a piece larger than the tile can never split off — prune the sizes before enumerating
      val qsEff = qs.filter(q => gonArea(q) <= poly.areaApprox + 1e-6)
      splitsMemo.getOrElseUpdate(
        (poly.dirs.mkString(","), qsEff), {
          val t0 = System.nanoTime
          val r  = Defusion.flushSplits(poly, qsEff)
          val ms = (System.nanoTime - t0) / 1000000
          if ms > 5000 then
            println(
              s"      [slow flushSplits: ${poly.dirs.length}-gon qs=$qsEff, ${r.length} candidates in ${ms}ms]"
            )
          Some(r)
        }
      )

  private def keyOf(t: PlacedTile): String = s"${t.anchor.reducedKey}|${t.poly.dirs.mkString(",")}"

  /** DFS over iterated original-boundary flush splits of one placed instance; returns (statesExplored,
    * legalStateSizes, capped).
    */
  private def audit(
      re: State,
      idx: Int,
      z: List[Int],
      cap: Int
  ): (Int, Vector[Int], Boolean, Boolean, Vector[Node]) =
    val states    = Vector.newBuilder[Node]
    val inst      = re.tiles(idx)
    val vs0       = inst.vertices
    val myKeys    = vs0.map(_.reducedKey).toSet
    val boundary  = vs0.indices.map { i =>
      Set(vs0(i).reducedKey, vs0((i + 1) % vs0.length).reducedKey)
    }.toSet
    // the refinement touches only this tile, so legality elsewhere is inherited from the valid patch:
    // check the tile's own vertices and the split's seam vertices on the local neighbourhood star only
    val localBase = re.tiles.zipWithIndex.collect {
      case (t, j) if j != idx && t.vertices.exists(v => myKeys(v.reducedKey)) => t
    }

    /** (legalNow, deadForever): a vertex with no irregular corner left has its word FINAL — if illegal there,
      * no further split can repair it (later pieces only refine remaining irregular corners), so the whole
      * subtree is dead.
      */
    def evalState(tiles2: Vector[PlacedTile]): (Boolean, Boolean) =
      val st       = State(re.n, re.z, localBase ++ tiles2)
      val keys     = myKeys ++ tiles2.flatMap(_.vertices.map(_.reducedKey))
      var legalAll = true
      var dead     = false
      for k <- keys if !dead do
        st.cornersByVertex.get(k) match
          case None     => legalAll = false; dead = true
          case Some(cs) =>
            val ok = cs.map(_.angle).sum == st.n && vertexLegal(cs.map(_.letter), z) &&
              vertexChained(cs, st.n)
            if !ok then
              legalAll = false
              if cs.forall(_.letter.isDefined) then dead = true // final and illegal — unrepairable
      (legalAll && tiles2.forall(_.poly.isSimpleCertified), dead)
    var explored                                                  = 0
    var capped                                                    = false
    var timedOut                                                  = false
    val legal                                                     = Vector.newBuilder[Int]
    val seen                                                      = collection.mutable.Set.empty[String]
    val stack                                                     = collection.mutable.Stack(Node(Vector.empty, Vector(inst)))
    while stack.nonEmpty && !capped do
      val node = stack.pop()
      explored += 1
      if explored % 5000 == 0 then println(s"      [DFS heartbeat: $explored states, stack=${stack.size}]")
      if explored >= cap then capped = true
      for (r, ri) <- node.rems.zipWithIndex if regularSizeOf(r.poly).isEmpty do
        val vsr = r.vertices
        val fsO = splitsOf(r.poly, z.distinct)
        if fsO.isEmpty then { timedOut = true; capped = true }
        for f <- fsO.getOrElse(Vector.empty) do
          val onBoundary = (0 until f.arcLen).forall { j =>
            boundary(Set(
              vsr((f.edge + j) % vsr.length).reducedKey,
              vsr((f.edge + j + 1) % vsr.length).reducedKey
            ))
          }
          if onBoundary then
            val piece   = PlacedTile(f.split.regular, vsr(f.edge))
            val tail    = vsr((f.edge + f.arcLen) % vsr.length)
            val newRems = f.split.remainders.map(p => PlacedTile(p.poly, tail + p.rel))
            val child   = Node(node.pieces :+ piece, node.rems.patch(ri, newRems, 1))
            val ck      = (child.pieces ++ child.rems).map(keyOf).sorted.mkString(";")
            if !seen(ck) then
              seen += ck
              val (legalNow, dead) = evalState(child.pieces ++ child.rems)
              if legalNow then legal += child.pieces.length
              if !dead then
                states += child
                stack.push(child)
    (explored, legal.result(), capped, timedOut, states.result())

  private def auditState(name: String, re: State, z: List[Int]): Unit =
    println(s"  [$name: patch=${re.tiles.length} tiles — certifying cell…]")
    Periodicity.certifiedCell(re) match
      case None              => println(s"$name: NO CERTIFIED CELL — cannot partition classes")
      case Some((t1, t2, _)) =>
        val eq      = Periodicity.latticeEquiv(t1, t2)
        val canon   = re.tiles.map(Periodicity.canonicalPlacement)
        val classes = Periodicity.partitionBy(
          canon.zipWithIndex.toVector.filter(p => regularSizeOf(p._1.poly).isEmpty)
        )((a, b) => a._1.poly.dirs == b._1.poly.dirs && eq(a._1.anchor, b._1.anchor))
        for cls <- classes do
          cls.find { (_, i) =>
            re.tiles(i).vertices.forall(v => re.interiorWords.contains(v.reducedKey))
          } match
            case None         =>
              println(
                s"$name class ${cls.head._1.poly.dirs.length}-gon: NO fully interior instance — UNTESTED"
              )
            case Some((_, i)) =>
              val (explored, legal, capped, timedOut, _) = audit(re, i, z, 300000)
              val p                                      = re.tiles(i).poly.dirs.length
              println(s"$name class $p-gon: states=$explored legalMultiSplits=${legal.size}${
                  if legal.nonEmpty then s" (sizes ${legal.sorted.distinct.mkString(",")})" else ""
                }${if capped then " CAPPED" else ""} -> ${
                  if timedOut then "GEOMETRIC-TIMEOUT — joint verdict deferred"
                  else if legal.isEmpty && !capped then "SATURATED under the full definition"
                  else if legal.nonEmpty then "REFINABLE"
                  else "inconclusive (cap)"
                }")

  /** Development radii large enough for cell certification, matching the realization probe. */
  private val radiusOf = Map(
    "(3.9.18) w k=4"               -> 14.0,
    "(3.8.24) w k=3"               -> 18.0,
    "(3.10.15) pattern k=5"        -> 28.0,
    "(3.7.42) P1 k=10"             -> 30.0,
    "(3.7.42) P2 fused k=6"        -> 30.0,
    "(4.5.20) pattern k=7"         -> 16.0,
    "(3.3.4.12) uplus pattern k=7" -> 12.0
  ).withDefaultValue(8.0)

  "the saturated patterns" should "survive the joint multi-split audit (enable with -Djoint.sat)" in:
    assume(sys.props.contains("joint.sat"), "joint audit — enable with -Djoint.sat")
    val only   = sys.props.get("joint.sat.only").filter(_.nonEmpty).map(_.split(';').toVector)
    // Part A: the rigid banked entries
    for (name, z, key) <- SaturationProbe.entries if only.forall(_.exists(name.contains)) do
      println(s"\n=== $name (radius ${radiusOf(name)}) — developing… ===")
      val ds  = symbolFromKey(key)
      val sys = MetricLayer.angleSystem(ds)
      val unf = UClass.candidates(ds, z).filter: reg =>
        val ir = ds.orbs.zipWithIndex.collect { case (o, kk) if o.i == 0 && !reg(kk) => kk }
        UClass.noneForcedRegular(ds, reg, ir)
      val ran = unf.nonEmpty && {
        val rows = MetricLayer.designatedRows(ds, unf.maxBy(_.size))
        MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows)).isEmpty &&
        MetricLayer.particularSolution(rows, sys.vars).exists { x =>
          x.forall { g =>
            val d = g.toDouble; d > 1e-9 && d < 2 - 1e-9 && math.abs(d - 1) > 1e-9
          } && MetricLayer.maxClosureResidual(ds, x.map(_.toDouble)) < 1e-9 && {
            auditState(name, seed(ExactDeveloper.develop(ds, x, radiusOf(name)), z), z)
            true
          }
        }
      }
      if !ran then println(s"$name: not rigid/realizable at the audit designation — skipped")
    // Part B: the flexible (3^2.6^2) witnesses at their realized parameters
    val hitsK3 = Vector(
      "certs/uclass-k3/hits-maxSize24.tsv",
      "certs/uclass-k3/window-hits-maxSize28.tsv",
      "certs/uclass-k3/window-hits-min29-maxSize30.tsv"
    ).flatMap(p => java.nio.file.Files.readAllLines(java.nio.file.Path.of(p)).asScala)
      .filter(_.startsWith("z=3.3.6.6\t")).distinct
    val z66    = List(3, 3, 6, 6)
    for (label, hitIdx, t) <- Vector(
                                ("(3.3.6.6) flexible convex", 13, Frac(5, 12)),
                                ("(3.3.6.6) flexible reflex", 16, Frac(5, 12)),
                                ("(3.3.6.6) flexible rhombi", 64, Frac(5, 8))
                              )
    do
      val ds    = symbolFromKey(hitsK3(hitIdx - 1).split('\t').last.stripPrefix("key="))
      val sys   = MetricLayer.angleSystem(ds)
      val unf   = UClass.candidates(ds, z66).filter: reg =>
        val ir = ds.orbs.zipWithIndex.collect { case (o, kk) if o.i == 0 && !reg(kk) => kk }
        UClass.noneForcedRegular(ds, reg, ir)
      val rows  = MetricLayer.designatedRows(ds, unf.head)
      val basis = MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys.vars, sys.corner, rows))
      (MetricLayer.particularSolution(rows, sys.vars), basis.headOption) match
        case (Some(x0), Some(v)) =>
          val xf     = Array.tabulate(sys.vars)(j => x0(j) + t * v(j))
          val re     = seed(ExactDeveloper.develop(ds, xf, 9.0), z66)
          val shared =
            re.interiorWords.keySet.count(vk => re.cornersByVertex(vk).count(_.letter.isEmpty) >= 2)
          println(s"$label: sharedIrregularVertices=$shared")
          auditState(label, re, z66)
          // Part C: cross-tile audit — the seam gussets share vertices, so per-tile is not enough when
          // candidates exist. Sound scoped criterion: legality at the audited tile's vertices (and its
          // own seam vertices) depends ONLY on its and its direct neighbours' decompositions.
          val irrIdx = re.tiles.indices.filter(i => regularSizeOf(re.tiles(i).poly).isEmpty)
          irrIdx.find(i => re.tiles(i).vertices.forall(v => re.interiorWords.contains(v.reducedKey))) match
            case None    => println(s"$label cross-tile: no interior instance — UNTESTED")
            case Some(i) =>
              val (_, _, _, _, statesT) = audit(re, i, z66, 20000)
              if statesT.forall(_.pieces.isEmpty) then
                println(s"$label cross-tile: zero candidates — vacuously closed")
              else
                val myKeys                                            = re.tiles(i).vertices.map(_.reducedKey).toSet
                val neighbors                                         = irrIdx.filter(j =>
                  j != i && re.tiles(j).vertices.exists(v => myKeys(v.reducedKey))
                ).toVector
                val statesN                                           = neighbors.indices.map(k =>
                  audit(re, neighbors(k), z66, 20000)._5 :+ Node(Vector.empty, Vector(re.tiles(neighbors(k))))
                ).toVector
                def combos(k: Int): Iterator[Vector[Node]]            =
                  if k == neighbors.length then Iterator(Vector.empty)
                  else combos(k + 1).flatMap(tail => statesN(k).iterator.map(n => n +: tail))
                def scopedLegal(dt: Node, dns: Vector[Node]): Boolean =
                  val repl   = (i -> dt) +: neighbors.zip(dns)
                  val tiles2 = re.tiles.indices.flatMap { j =>
                    repl.find(_._1 == j) match
                      case Some((_, nd)) => nd.pieces ++ nd.rems
                      case None          => Vector(re.tiles(j))
                  }.toVector
                  val st     = State(re.n, re.z, tiles2)
                  val keys   = myKeys ++ (dt.pieces ++ dt.rems).flatMap(_.vertices.map(_.reducedKey))
                  keys.forall { k =>
                    st.cornersByVertex.get(k).exists { cs =>
                      cs.map(_.angle).sum == st.n &&
                      vertexLegal(cs.map(_.letter), z66) && vertexChained(cs, st.n)
                    }
                  }
                var alive                                             = 0
                for dt <- statesT if dt.pieces.nonEmpty do
                  if combos(0).exists(dns => scopedLegal(dt, dns)) then alive += 1
                println(s"$label cross-tile: tileStates=${statesT.count(_.pieces.nonEmpty)} " +
                  s"neighbors=${neighbors.length} survivingTileStates=$alive -> ${
                      if alive == 0 then "SATURATED under the full definition (cross-tile closed)"
                      else "REPAIRABLE — needs escalation"
                    }")
        case _                   => println(s"$label: no flexible solution — SKIPPED")
