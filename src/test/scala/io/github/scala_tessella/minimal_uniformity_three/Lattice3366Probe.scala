package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.CycloRing.Cyc
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import io.github.scala_tessella.research_core.TilePatch.{PlacedTile, State}
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE LATTICE SEARCH FOR U(3²6²) (enable with `-Dlattice.3366`; `-Dlattice.3366.index=<max index>` the
  * translation lattices to try (every Hermite normal form up to that index),
  * `-Dlattice.3366.max=<models per torus>`). By `thm:uplus3366` every member of the class is a
  * triangular-lattice tiling: regular triangles, regular hexagons and fused lattice polygons. On an n × n
  * torus of the lattice each unit triangle is a regular triangle, part of the hexagon centred at one of its
  * vertices, or part of an irregular tile; edge presence follows from the kinds (adjacent irregular triangles
  * share their edge — two irregular tiles cannot meet), and the class conditions are a finite table per
  * vertex: at most one run of irregular triangles, of size 1, 2, 4, 5 (no straight corner) or 6 (an interior
  * point), the regular fans reading in order as an arc of 3.3.6.6, or the full word. Vertex (0,0) is fixed to
  * be a species vertex. Every model is lifted to the plane, its irregular tiles checked to be finite disks,
  * the tiling placed exactly on ℤ[ζ₆], validated under the class, and its certified cell and minimal symbol
  * extracted: the true uniformity of a member, or a refusal.
  */
class Lattice3366Probe extends AnyFlatSpec with Matchers:

  private val z = List(3, 3, 6, 6)

  /** Lattice directions ω^k as (x, y) steps in the basis (1, ω). */
  private val dir: Vector[(Int, Int)] = Vector((1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1))

  /** The torus of the lattice ⟨(a, 0), (b, c)⟩ in the basis (1, ω) — Hermite normal form, index a·c; every
    * translation lattice of the plane lattice is one of these. Triangles: up(p) = {p, p+1, p+ω}, down(p) =
    * {p+1, p+1+ω, p+ω}.
    */
  final class Torus(val a: Int, val b: Int, val c: Int):
    val index: Int                              = a * c
    def v(x: Int, y: Int): Int                  =
      val q  = Math.floorDiv(y, c)
      val yy = y - q * c
      val xx = Math.floorMod(x - q * b, a)
      xx * c + yy
    def xy(id: Int): (Int, Int)                 = (id / c, id % c)
    def tri(x: Int, y: Int, down: Boolean): Int = 2 * v(x, y) + (if down then 1 else 0)

    /** The triangle between directions ω^k and ω^{k+1} at vertex (x, y). */
    def around(x: Int, y: Int, k: Int): Int = k match
      case 0 => tri(x, y, false)
      case 1 => tri(x - 1, y, true)
      case 2 => tri(x - 1, y, false)
      case 3 => tri(x - 1, y - 1, true)
      case 4 => tri(x, y - 1, false)
      case _ => tri(x, y - 1, true)

    /** The three vertices of a triangle (CCW). */
    def verts(t: Int): Vector[Int] =
      val (x, y) = xy(t / 2)
      if t % 2 == 0 then Vector(v(x, y), v(x + 1, y), v(x, y + 1))
      else Vector(v(x + 1, y), v(x + 1, y + 1), v(x, y + 1))

    /** The triangle across edge j (from vertex j to vertex j+1) of triangle t. */
    def across(t: Int, j: Int): Int =
      val (x, y) = xy(t / 2)
      if t % 2 == 0 then
        j match
          case 0 => tri(x, y - 1, true) // edge p -> p+1
          case 1 => tri(x, y, true)     // edge p+1 -> p+ω
          case _ => tri(x - 1, y, true) // edge p+ω -> p
      else
        j match
          case 0 => tri(x + 1, y, false) // edge p+1 -> p+1+ω
          case 1 => tri(x, y + 1, false) // edge p+1+ω -> p+ω
          case _ => tri(x, y, false)     // edge p+ω -> p+1

  /** Legality of the six fans around a vertex: labels R, I, or H(k) (covered by the hexagon at neighbour k,
    * which covers triangles k−1 and k).
    */
  private def legal(labels: Vector[String]): Boolean =
    val fans = collection.mutable.ArrayBuffer.empty[String]
    var k    = 0
    while k < 6 do
      labels(k) match
        case "R" => fans += "3"; k += 1
        case "I" => fans += "i"; k += 1
        case h   =>
          // a hexagon fan spans two triangles; count it once, at its first triangle in cyclic order
          if k == 0 && labels(5) == h then { k += 1 } // its first triangle is t_5: counted when k = 5
          else { fans += "6"; k += (if k + 1 < 6 && labels(k + 1) == h then 2 else 1) }
    val f    = fans.toVector
    val m    = f.length
    if !f.contains("i") then
      f.indices.exists(r => (0 until m).map(j => f((r + j) % m)) == Vector("3", "3", "6", "6"))
    else
      val runs = f.indices.count(j => f(j) == "i" && f((j - 1 + m) % m) != "i")
      if m == 1 then true // six irregular triangles: an interior point
      else if runs != 1 then false
      else
        val iCount = f.count(_ == "i")
        if iCount == 3 then false // a straight corner
        else
          val start = f.indices.find(j => f(j) != "i" && f((j - 1 + m) % m) == "i").get
          val word  = (0 until m - iCount).map(j => f((start + j) % m)).map(_.toInt).toList
          UClass.isArc(word, z)

  it should "search the triangular-lattice tori for members of U(3^2.6^2) (enable with -Dlattice.3366)" in:
    assume(sys.props.contains("lattice.3366"), "lattice search — enable with -Dlattice.3366")
    val minIdx = sys.props.get("lattice.3366.from").filter(_.nonEmpty).fold(1)(_.toInt)
    val maxIdx = sys.props.get("lattice.3366.index").filter(_.nonEmpty).fold(12)(_.toInt)
    val maxM   = sys.props.get("lattice.3366.max").filter(_.nonEmpty).fold(200)(_.toInt)
    val only   = sys.props.get("lattice.3366.only").filter(_.nonEmpty) // "a,b,c;a,b,c" restricts the lattices
    val tori   = only match
      case Some(l) => l.split(';').toList.map(_.split(',').map(_.trim.toInt)).map(t => (t(0), t(1), t(2)))
      case None    =>
        (for N <- minIdx to maxIdx; a <- 1 to N if N % a == 0; c = N / a; b <- 0 until a
        yield (a, b, c)).toList
    val best   = collection.mutable.Map.empty[Int, String]             // k -> key of the first member found with that k
    for (a, b, c) <- tori do
      val T                            = new Torus(a, b, c)
      val nv                           = T.index
      val nt                           = 2 * nv
      val hexV                         = (v: Int) => v + 1
      val rV                           = (t: Int) => nv + t + 1
      val iV                           = (t: Int) => nv + nt + t + 1
      val solver                       = SolverFactory.newDefault()
      solver.newVar(nv + 2 * nt)
      solver.setTimeout(3600)
      var clauses                      = 0
      def clause(lits: Seq[Int]): Unit = { solver.addClause(new VecInt(lits.toArray)); clauses += 1 }
      // exactly one kind per triangle
      for t <- 0 until nt do
        val opts = rV(t) :: iV(t) :: T.verts(t).map(hexV).toList
        clause(opts)
        for a <- opts; b <- opts if a < b do clause(List(-a, -b))
      // hexagons do not overlap: adjacent centres excluded; and a hexagon centre pulls in its six triangles
      for x <- 0 until a; y <- 0 until c do
        val v  = T.v(x, y)
        for k <- 0 until 6 do
          val (dx, dy) = dir(k)
          val w        = T.v(x + dx, y + dy)
          if v < w then clause(List(-hexV(v), -hexV(w)))
        // vertex legality (when v is not a hexagon centre)
        val ts = (0 until 6).map(k => T.around(x, y, k))
        val cs = (0 until 6).map(k => T.v(x + dir(k)._1, y + dir(k)._2))
        val fs = (0 until 64).filter(f =>
          (0 until 6).forall(k => !((f >> k & 1) == 1 && (f >> ((k + 1) % 6) & 1) == 1))
        )
        for f <- fs do
          val covered = (0 until 6).filter(k => (f >> k & 1) == 1).flatMap(k => List((k + 5) % 6, k)).toSet
          val free    = (0 until 6).filterNot(covered)
          for a <- 0 until (1 << free.length) do
            val labels = Vector.tabulate(6) { k =>
              if covered(k) then s"H${if (f >> k & 1) == 1 then k else (k + 1) % 6}"
              else if (a >> free.indexOf(k) & 1) == 1 then "I" else "R"
            }
            if !legal(labels) then
              val lits = hexV(v) +:
                ((0 until 6).map(k => if (f >> k & 1) == 1 then -hexV(cs(k)) else hexV(cs(k))) ++
                  free.map(k => if labels(k) == "I" then -iV(ts(k)) else -rV(ts(k))))
              clause(lits)
      // an irregular tile is not a lone triangle, nor a regular hexagon in disguise
      for t <- 0 until nt do clause(-iV(t) +: (0 until 3).map(j => iV(T.across(t, j))))
      for x <- 0 until a; y <- 0 until c do
        val ts = (0 until 6).map(k => T.around(x, y, k))
        clause(ts.map(t => -iV(t)) ++ ts.map(t => iV(T.across(t, 1))).distinct)
      // the species vertex at (0, 0): triangles 0, 1 regular, hexagons at neighbours 3 and 5
      clause(List(rV(T.around(0, 0, 0)))); clause(List(rV(T.around(0, 0, 1))))
      clause(List(hexV(T.v(dir(3)._1, dir(3)._2)))); clause(List(hexV(T.v(dir(5)._1, dir(5)._2))))
      println(s"\n== lattice <($a,0),($b,$c)> index ${T.index}: ${nv + 2 * nt} vars, $clauses clauses")
      var models                       = 0
      var sat                          = solver.isSatisfiable
      val seen                         = collection.mutable.Set.empty[String]
      val configs                      = collection.mutable.Set.empty[String]
      var skippedTranslates            = 0
      while sat && models < maxM do
        models += 1
        val m     = solver.model()
        val hex   = (0 until nv).filter(v => m(hexV(v) - 1) > 0).toSet
        val irr   = (0 until nt).filter(t => m(iV(t) - 1) > 0).toSet
        // a translate of a model already verified is the same tiling with another species vertex at the
        // origin: canonicalise under the torus translations and verify each tiling once
        val canon = (for dx <- 0 until a; dy <- 0 until c yield
          val hv = (0 until nv).map { v =>
            val (x, y) = T.xy(v); if hex(T.v(x + dx, y + dy)) then '1' else '0'
          }.mkString
          val it = (0 until nt).map { t =>
            val (x, y) = T.xy(t / 2); if irr(T.tri(x + dx, y + dy, t % 2 == 1)) then '1' else '0'
          }.mkString
          hv + it
        ).min
        if configs.add(canon) then verify(T, hex, irr, seen, best) else skippedTranslates += 1
        clause((0 until nv).map(v => if hex(v) then -hexV(v) else hexV(v)) ++ (0 until nt).map(t =>
          if irr(t) then -iV(t) else iV(t)
        ))
        sat = solver.isSatisfiable
      println(s"lattice <($a,0),($b,$c)>: ${
          if models == 0 then "UNSAT" else s"$models model(s) examined, $skippedTranslates translates skipped"
        }")
    println(
      s"\nSUMMARY over all lattices of index <= $maxIdx: uniformities found ${best.keys.toList.sorted.mkString(",")}"
    )
    for (k, key) <- best.toList.sortBy(_._1) do println(s"  k=$k  key=$key")

  /** Lift a model to the plane, check its irregular tiles, place everything exactly and extract the symbol.
    */
  private def verify(
      T: Torus,
      hex: Set[Int],
      irr: Set[Int],
      seen: collection.mutable.Set[String],
      best: collection.mutable.Map[Int, String]
  ): Unit =
    val (a, b, c)                                                             = (T.a, T.b, T.c)
    // plane triangles as (px, py, down); torus id by reduction
    def tid(px: Int, py: Int, down: Boolean)                                  = T.tri(px, py, down)
    def pverts(px: Int, py: Int, down: Boolean): Vector[(Int, Int)]           =
      if !down then Vector((px, py), (px + 1, py), (px, py + 1))
      else Vector((px + 1, py), (px + 1, py + 1), (px, py + 1))
    def pacross(px: Int, py: Int, down: Boolean, j: Int): (Int, Int, Boolean) =
      if !down then
        j match
          case 0 => (px, py - 1, true);
          case 1 => (px, py, true);
          case _ => (px - 1, py, true)
      else
        j match
          case 0 => (px + 1, py, false);
          case 1 => (px, py + 1, false);
          case _ => (px, py, false)
    // components of irregular triangles on the torus
    val comp                                                                  = collection.mutable.Map.empty[Int, Int]
    var nc                                                                    = 0
    for t <- irr.toList.sorted if !comp.contains(t) do
      val stack = collection.mutable.Stack(t); comp(t) = nc
      while stack.nonEmpty do
        val u = stack.pop()
        for j <- 0 until 3 do
          val w = T.across(u, j)
          if irr(w) && !comp.contains(w) then { comp(w) = nc; stack.push(w) }
      nc += 1
    // lift each component: finite disk?
    val tiles                                                                 = collection.mutable.ArrayBuffer.empty[(UnitPolygon, (Int, Int))] // polygon + plane anchor
    var ok                                                                    = true
    for c <- 0 until nc if ok do
      val size     = comp.count(_._2 == c)
      val t0       = comp.find(_._2 == c).get._1
      val (x0, y0) = T.xy(t0 / 2)
      val start    = (x0, y0, t0 % 2 == 1)
      val lifted   = collection.mutable.Set(start)
      val stack    = collection.mutable.Stack(start)
      while stack.nonEmpty && lifted.size <= size do
        val (px, py, d) = stack.pop()
        for j <- 0 until 3 do
          val w = pacross(px, py, d, j)
          if irr(tid(w._1, w._2, w._3)) && !lifted(w) then { lifted += w; stack.push(w) }
      if lifted.size != size then {
        println(s"  a fused region wraps the torus (lift ${lifted.size} > $size) — rejected"); ok = false
      } else
        // boundary walk: directed edges with the tile on the left
        val edges =
          for
            (px, py, d) <- lifted.toVector; j <- 0 until 3
            w            = pacross(px, py, d, j)
            if !irr(tid(w._1, w._2, w._3))
            vs           = pverts(px, py, d)
          yield (vs(j), vs((j + 1) % 3))
        val next  = edges.groupMap(_._1)(_._2)
        if next.values.exists(_.length != 1) || edges.length != next.size then {
          println("  a fused region touches itself — rejected"); ok = false
        } else
          val startV = edges.head._1
          val walk   = collection.mutable.ArrayBuffer(startV)
          var cur    = next(startV).head
          while cur != startV do { walk += cur; cur = next(cur).head }
          if walk.length != edges.length then {
            println("  a fused region has a hole — rejected"); ok = false
          } else
            val dirs = walk.indices.map { i =>
              val (a, b) = (walk(i), walk((i + 1) % walk.length))
              dir.indexOf((b._1 - a._1, b._2 - a._2))
            }.toVector
            tiles += ((UnitPolygon(6, dirs), startV))
    if ok then
      // place a patch: R periods of the torus in every direction
      val R                       = 3
      def cyc(p: (Int, Int)): Cyc = Cyc.root(6, 0).scaled(p._1) + Cyc.root(6, 1).scaled(p._2)
      val placed                  = collection.mutable.ArrayBuffer.empty[PlacedTile]
      for i <- -R to R; j <- -R to R do
        val off = (i * a + j * b, j * c)
        for (poly, anchor) <- tiles do
          placed += PlacedTile(poly, cyc((anchor._1 + off._1, anchor._2 + off._2)))
        for x <- 0 until a; y <- 0 until c do
          val v = T.v(x, y)
          if hex(v) then
            placed += PlacedTile(Defusion.regularGon(6, 6, 0), cyc((x + off._1 - 0, y + off._2 - 1)))
          for d <- List(false, true) do
            val t = T.tri(x, y, d)
            if !irr(t) && !T.verts(t).exists(hex) then
              placed +=
                (if !d then PlacedTile(Defusion.regularGon(6, 3, 0), cyc((x + off._1, y + off._2)))
                 else PlacedTile(Defusion.regularGon(6, 3, 1), cyc((x + off._1 + 1, y + off._2))))
      val state                   = State(6, z, placed.toVector, strict = true, isolated = true)
      val shapes                  = tiles.map(_._1).map(p => p.interiorAngles.map(_ * 60).mkString("(", ".", ")")).distinct
      if !TilePatch.valid(state) then println(s"  INVALID patch (irregular tiles $shapes) — rejected")
      else if !state.tiles.forall(_.poly.isSimpleCertified) then println("  embedding failure — rejected")
      else
        Periodicity.certifiedCell(state) match
          case None              => println(s"  valid patch, no certified cell at 7 x 7 periods (irregular tiles $shapes)")
          case Some((t1, t2, _)) =>
            SymbolExtractor.torusSymbol(state, t1, t2) match
              case None        =>
                println(s"  certified cell but no torus symbol (irregular tiles $shapes) — skipped")
              case Some(torus) =>
                val min = SymbolExtractor.minimalImage(torus)
                val key = DelaneySymbols.canonicalKey(min)
                val k   = min.orbs.count(_.i == 1)
                if seen.add(key) then
                  println(
                    s"  MEMBER: minimal symbol C=${min.size} k=$k  irregular tiles ${shapes.mkString(" ")}"
                  )
                  if !best.contains(k) then { best(k) = key; println(s"    key=$key") }
