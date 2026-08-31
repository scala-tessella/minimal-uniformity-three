package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE VERTEX-ORBIT CAP OF THE WALK, validated (guarded: -Dvcap.check, ~a minute). A species whose vertices
  * all have valence ≤ 3 — the six three-letter species — lives in the D-sets whose (1,2)-orbits have at most
  * 6 chambers, and `vertexCap = 6` prunes the walk to them. Two fixtures before any window is opened on it:
  * the capped walk must equal the uncapped walk filtered by the cap (the prune removes nothing else), and the
  * (4.5.20) valence-2 four-orbit candidate of the 2026-08-29 sweep (16 chambers, two valence-2 vertices) must
  * be among the capped emissions at four orbits.
  */
class VertexCapProbe extends AnyFlatSpec with Matchers:

  private def opLine(ds: DSet): String = (1 to ds.size).map(d =>
    s"${ds.get(0, d)},${ds.get(1, d)},${ds.get(2, d)}"
  ).mkString(";")

  private def walk(maxN: Int, maxSize: Int, cap: Int, v2: Boolean): Set[String] =
    val out = new java.util.concurrent.ConcurrentLinkedQueue[String]
    DelaneySymbols.relaxedOrbitBoundedDSets(
      maxN = maxN,
      maxSize = maxSize,
      sink = ds => out.add(opLine(ds)),
      euclid = true,
      valence2 = v2,
      vertexCap = cap
    )
    import scala.jdk.CollectionConverters.*
    out.asScala.toSet

  behavior of "the (1,2)-orbit cap of the orbit-bounded walk"

  it should "equal the uncapped walk filtered by the cap, and keep the (4.5.20) v2 four-orbit candidate" in:
    assume(sys.props.contains("vcap.check"), "guarded — enable with -Dvcap.check")
    val maxSize = sys.props.get("vcap.check.size").fold(16)(_.toInt)
    for v2 <- List(false, true) do
      val t0       = System.nanoTime
      val uncapped = walk(3, maxSize, Int.MaxValue, v2)
      val t1       = System.nanoTime
      val capped   = walk(3, maxSize, 6, v2)
      val t2       = System.nanoTime
      val expected = uncapped.filter(l => DelaneySymbols.maxVertexOrbitLength(parse(l)) <= 6)
      println(
        f"maxN=3 size<=$maxSize v2=$v2: uncapped ${uncapped.size} (${(t1 - t0) / 1e9}%.1fs)  capped ${capped.size} " +
          f"(${(t2 - t1) / 1e9}%.1fs)  uncapped∩cap ${expected.size}"
      )
      capped shouldBe expected
    // the v2 four-orbit (4.5.20) candidate: 16 chambers, valence-2 vertices, valence ≤ 3 throughout
    val key     = SaturationProbe.entries.find(_._1 == "(4.5.20) v2 k=4 candidate").get._3
    val ds      = StrictWitnesses.symbolFromKey(key)
    DelaneySymbols.maxVertexOrbitLength(ds.dset) should be <= 6
    val canon   = dsetKey(ds.dset)
    val t3      = System.nanoTime
    val four    = walk(4, 16, 6, v2 = true)
    println(f"maxN=4 size<=16 v2 cap6: ${four.size} D-sets (${(System.nanoTime - t3) / 1e9}%.1fs)")
    four.exists(l => dsetKey(parse(l)) == canon) shouldBe true

  it should "size the three-letter universe at six orbits: counts per chamber count, and the species scan" in:
    assume(sys.props.contains("vcap.size"), "guarded — enable with -Dvcap.size=<maxSize>[,<maxSize>...]")
    val sizes  =
      sys.props.get("vcap.size").filter(_.nonEmpty).map(_.split(',').map(_.toInt).toList).getOrElse(List(
        20,
        24
      ))
    val maxN   = sys.props.get("vcap.size.maxN").fold(6)(_.toInt)
    val minSz  = sys.props.get("vcap.size.min").fold(0)(_.toInt)
    val shapes = !sys.props.contains("vcap.size.noshapes")
    val scan   = sys.props.contains("vcap.size.scan")
    for maxSize <- sizes do
      val bySize = new java.util.concurrent.ConcurrentHashMap[Int, java.util.concurrent.atomic.AtomicLong]
      val hits   = new java.util.concurrent.ConcurrentLinkedQueue[String]
      val t0     = System.nanoTime
      val n      = DelaneySymbols.relaxedOrbitBoundedDSets(
        maxN = maxN,
        maxSize = maxSize,
        sink = ds =>
          bySize.computeIfAbsent(
            ds.size,
            _ => new java.util.concurrent.atomic.AtomicLong(0)
          ).incrementAndGet()
          if scan then
            for
              (t, sym) <- DelaneySymbols.euclideanSymbolsOf(Vector(ds), maxN = maxN, valence2 = true)
              z        <- List(List(4, 5, 20), List(3, 7, 42))
              reg      <- UClass.candidates(sym, z, strict = true, isolated = true)
              irregular = sym.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !reg(k) => k }
              if UClass.noneForcedRegular(sym, reg, irregular)
            do
              hits.add(
                s"HIT z=${z.mkString(".")} n=${t.n} C=${sym.size} ${t.vertices.map(_.mkString(".")).mkString(" ")} " +
                  s"reg=${reg.toList.sorted} irr=${irregular.toList}"
              )
        ,
        log = msg => println(msg),
        euclid = true,
        minSize = minSz,
        valence2 = true,
        vertexCap = 6,
        threeLetter = shapes
      )
      val t1     = System.nanoTime
      import scala.jdk.CollectionConverters.*
      val hist   = bySize.asScala.toList.sortBy(_._1).map((s, c) => s"$s:${c.get}").mkString(" ")
      println(
        f"maxN=$maxN min=$minSz max=$maxSize cap6 shapes=$shapes v2 scan=$scan: $n D-sets in ${(t1 - t0) / 1e9}%.1fs  [$hist]"
      )
      hits.asScala.toList.sorted.foreach(println)

  /** Isomorphism key of a D-set: the minimal BFS-renumbered trace of the three involutions over all starts.
    */
  private def dsetKey(ds: DSet): String =
    val n = ds.size
    (1 to n).map { start =>
      val num   = Array.fill(n + 1)(0)
      var next  = 1
      var queue = List(start)
      num(start) = next
      next += 1
      val sb    = new StringBuilder
      while queue.nonEmpty do
        val x = queue.head
        queue = queue.tail
        for i <- 0 to 2 do
          val y = ds.get(i, x)
          if y != 0 && num(y) == 0 then { num(y) = next; next += 1; queue = queue :+ y }
          sb.append(if y == 0 then 0 else num(y)).append(',')
        sb.append(';')
      sb.toString
    }.min

  private def parse(line: String): DSet =
    val rows = line.split(';')
    val a    = Array.ofDim[Int](rows.length + 1, 3)
    rows.zipWithIndex.foreach: (r, i) =>
      r.split(',').zipWithIndex.foreach((v, j) => a(i + 1)(j) = v.toInt)
    new DSet(a)
