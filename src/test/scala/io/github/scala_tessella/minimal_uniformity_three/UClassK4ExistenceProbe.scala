package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.DelaneySymbols.{DSet, DSymbol}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** CLOSURE-LEVEL RESOLUTION of the candidates a window walk surfaces (`-Duclass.k4x`; by default the three
  * (4.5.20) ones; `-Duclass.k4x.z=5.5.10` etc. to take another species from
  * `certs/uclass-k4/window-hits-min1-maxSize24.tsv`). This is the SEARCH tool — it reads a hits file and
  * decides its candidates; the always-on re-verification of the paper's own witnesses is
  * [[UClassK4ExistenceSpec]], which runs the same pipeline from canonical keys.
  *
  * The k = 3 existence chain ([[UClassK3ExistenceSpec]]) transposed, with the ONE piece that could not be
  * reused: its exact closure test lives in ℚ(ζ₂₄) because every k = 3 angle is a multiple of π/12, while a
  * (4.5.20) tiling carries pentagons (3π/5) and 20-gons (9π/10). Rather than build a second cyclotomic field,
  * closure is decided by VANISHING SUMS OF ROOTS OF UNITY: with every corner angle a rational multiple of π,
  * each edge direction is ζ_N^a for N = 2·lcm(denominators), and `Σ ζ_N^{a_j} = 0` iff the integer polynomial
  * `Σ x^{a_j}` is divisible by the N-th cyclotomic polynomial — exact, integer-only, and valid for any face
  * angles the affine model produces (see [[rootsOfUnitySumIsZero]]).
  *
  * Per candidate: (1) rebuild the symbol; (2) build the affine model — angle sums, pins on the
  * regular-designated orbits, and the LINEAR closure equivalents per irregular face (triangle equilateral,
  * quadrilateral a rhombus, v ≥ 2 identically closed, edge-axis mirror hexagon palindrome); (3) consistency
  * and rigidity (nullspace dimension) — an inconsistent model KILLS the candidate outright, which is how most
  * candidates die; (4) at a rigid point: convexity, exact closure of every face orbit, and that the
  * irregular-designated faces really take non-regular angles (else the truth-designation is larger and the
  * tiling belongs to another designation's count).
  *
  * A face orbit fitting no linear category is REPORTED — and no longer stalls the verdict when the model
  * decides anyway: at dimension 0 the forced point is unique among closed configurations of the HANDLED faces
  * (their linear rows are necessary conditions of closure), so the exact step-4 closure check over EVERY face
  * orbit — the unhandled one included — is a sound verdict in both directions. At dimension 1 the corner
  * angles are affine in the parameter t, so strict convexity (each angle in (0, π)) intersects to an open
  * rational interval computed exactly over Frac: an EMPTY window kills the whole family in the convex
  * category before closure is ever consulted; a non-empty window leaves the candidate UNRESOLVED (its
  * per-face direction data exp(iπ(p_j + q_j·t)) is printed for exact nonlinear analysis). Only dimension ≥ 2
  * with an unhandled face remains a pure "needs more machinery" report.
  */
class UClassK4ExistenceProbe extends AnyFlatSpec with Matchers:

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

  /** The geometric corner sequence of the face through `d0` (as in `MetricLayer.faceCornerSeq`). */
  private def faceCornerSeq(ds: DSymbol, corner: Array[Int], d0: Int): Array[Int] =
    val frag = collection.mutable.ArrayBuffer.empty[Int]
    var cur  = d0
    var go   = true
    while go do
      frag += corner(cur)
      cur = ds.get(1, ds.get(0, cur))
      if cur == d0 then go = false
    val m    = ds.m(0, 1, d0)
    Array.tabulate(m)(i => frag(i % frag.length))

  /** RREF over Frac: a particular solution, or None if the system is inconsistent. */
  private def particular(rows: Vector[(Array[Frac], Frac)], vars: Int): Option[Array[Frac]] =
    val m          = rows.map((c, r) => c.clone :+ r).toArray
    val nr         = m.length
    val pivotOfCol = Array.fill(vars)(-1)
    var r          = 0
    var c          = 0
    while r < nr && c < vars do
      var pr = r
      while pr < nr && m(pr)(c).isZero do pr += 1
      if pr < nr then
        val t   = m(pr); m(pr) = m(r); m(r) = t
        val inv = m(r)(c)
        for j <- c to vars do m(r)(j) = m(r)(j) / inv
        for i <- 0 until nr if i != r && !m(i)(c).isZero do
          val f = m(i)(c)
          for j <- c to vars do m(i)(j) = m(i)(j) - f * m(r)(j)
        pivotOfCol(c) = r
        r += 1
      c += 1
    if m.exists(row => row.take(vars).forall(_.isZero) && !row(vars).isZero) then None
    else
      val x = Array.fill(vars)(Frac(0, 1))
      for pc <- 0 until vars if pivotOfCol(pc) >= 0 do x(pc) = m(pivotOfCol(pc))(vars)
      Some(x)

  /** The n-th cyclotomic polynomial over ℤ, by exact division `x^n − 1 = Π_{d|n} Φ_d(x)`. Monic. */
  private def cyclotomic(n: Int): Array[BigInt] =
    def divide(a: Array[BigInt], b: Array[BigInt]): Array[BigInt] =
      val q   = Array.fill(math.max(1, a.length - b.length + 1))(BigInt(0))
      val rem = a.clone
      for i <- (0 to a.length - b.length).reverse do
        val c = rem(i + b.length - 1) / b(b.length - 1)
        q(i) = c
        for j <- b.indices do rem(i + j) -= c * b(j)
      require(rem.forall(_ == BigInt(0)), "cyclotomic division left a remainder")
      q
    var acc                                                       = Array(BigInt(-1)) ++ Array.fill(n - 1)(BigInt(0)) ++ Array(BigInt(1)) // x^n − 1
    for d <- 1 until n if n % d == 0 do
      val phiD = cyclotomic(d)
      acc = divide(acc, phiD)
    acc

  /** True iff `Σ_j ζ_N^{a_j} = 0` exactly, ζ_N a primitive N-th root of unity: reduce `Σ x^{a_j}` modulo the
    * (monic, integer) N-th cyclotomic polynomial and test the remainder for zero.
    */
  private def rootsOfUnitySumIsZero(exponents: Seq[Int], n: Int): Boolean =
    val c   = Array.fill(n)(BigInt(0))
    exponents.foreach(a => c(((a % n) + n) % n) += 1)
    val phi = cyclotomic(n)
    val rem = c.clone
    for i <- (0 to rem.length - phi.length).reverse do
      val f = rem(i + phi.length - 1)
      if f != BigInt(0) then for j <- phi.indices do rem(i + j) -= f * phi(j)
    rem.forall(_ == BigInt(0))

  /** Area of the unit-edge regular `n`-gon, `(n/4)·cot(π/n)` — increasing in n. */
  private def regularArea(n: Int): Double = n / 4.0 / math.tan(math.Pi / n)

  /** IS THE TILE A FUSION? — the corner-forcing-plus-area criterion, which decides the two uniformity-4
    * witnesses without leaving their cyclotomic fields (the [[UClassEssentialIrregularitySpec]] area
    * obstruction is pinned to the ℚ(ζ₂₄) alphabet {3,4,6,8,12,24}, while these tiles live in ζ₁₈ / ζ₁₀).
    *
    * If a tile is a union of unit-edge regular polygons, then at each of its corners the interior angle is a
    * SUM of corner angles `(n−2)π/n ≥ π/3` of constituent polygons, and every constituent fits inside the
    * tile, so its area is at most the tile's. A convex corner is under π, so at most two terms. Hence: if
    * some corner admits no decomposition whose every polygon fits by area, the tile is no fusion. The area
    * cap used for the tile is the isoperimetric one — a unit-edge p-gon is no larger than the regular p-gon —
    * which is a bound, not an estimate, so the verdict does not rest on the tile's own (algebraic) area.
    */
  private def essentiallyIrregular(p: Int, cornersDeg: List[Double]): (Boolean, String) =
    val cap      = regularArea(p) // the tile is at most this big (equilateral isoperimetric bound)
    val regulars = (3 to 400).map(n => (n, 180.0 * (n - 2) / n, regularArea(n)))
    val blocking = cornersDeg.distinct.flatMap: theta =>
      val singles = regulars.filter((_, a, _) => math.abs(a - theta) < 1e-9)
      val pairs   =
        for
          (n1, a1, _) <- regulars if a1 <= theta - 60 + 1e-9
          (n2, a2, _) <- regulars if math.abs(a1 + a2 - theta) < 1e-9
        yield (n1, n2)
      val fitting = singles.filter((_, _, ar) => ar <= cap + 1e-9).map(t => s"${t._1}-gon") ++
        pairs.filter((n1, n2) => regularArea(n1) <= cap + 1e-9 && regularArea(n2) <= cap + 1e-9)
          .map((n1, n2) => s"$n1+$n2-gon")
      if fitting.isEmpty then
        val needed = (singles.map(t => (t._1, t._3)) ++ pairs.map((n1, _) => (n1, regularArea(n1)))).distinct
        Some(f"$theta%.0f° needs " +
          (if needed.isEmpty then "a corner no regular polygon provides"
           else needed.map((n, ar) => f"a $n-gon (area $ar%.2f > cap $cap%.2f)").mkString(" or ")))
      else None
    (
      blocking.nonEmpty,
      if blocking.isEmpty then "every corner is completable by fitting regulars"
      else blocking.mkString("; ")
    )

  private def lcm(a: Long, b: Long): Long =
    def gcd(x: Long, y: Long): Long = if y == 0 then x.abs else gcd(y, x % y)
    (a / gcd(a, b)) * b

  it should "resolve the k = 4 candidates at closure level (enable with -Duclass.k4x)" in:
    assume(sys.props.contains("uclass.k4x"), "closure-level analysis — enable with -Duclass.k4x")
    val target   = sys.props.get("uclass.k4x.z").filter(_.nonEmpty).getOrElse("4.5.20")
    // which sweep's hits to resolve: `-Duclass.k4x.maxN=5` reads certs/uclass-k5/… (the k = 5 candidates
    // for the claimed-7/10 species are the live falsification targets)
    val fromN    = sys.props.get("uclass.k4x.maxN").filter(_.nonEmpty).fold(4)(_.toInt)
    // which hits file inside that sweep: `-Duclass.k4x.band=25-30` selects a band's verdict file
    val band     = sys.props.get("uclass.k4x.band").filter(_.nonEmpty) match
      case Some(b) => val Array(lo, hi) = b.split('-'); s"window-hits-min$lo-maxSize$hi.tsv"
      case None    => "window-hits-min1-maxSize24.tsv"
    // `-Duclass.k4x.file=<path>` overrides the hits file entirely — for resolving a PROVISIONAL
    // aggregate of banked per-shard scans before a sweep's Phase C assembles the real verdict file
    // (sound: each shard's hits are final for that shard; more candidates may still arrive)
    val hitsPath = sys.props.get("uclass.k4x.file").filter(_.nonEmpty) match
      case Some(p) => Path.of(p)
      case None    => Path.of("certs", s"uclass-k$fromN", band)
    val hits     = Files
      .readAllLines(hitsPath)
      .asScala
      .toVector
      .filter(_.startsWith(s"z=$target\t"))
    println(s"CANDIDATES for z=$target: ${hits.size}")

    val z = target.split('.').map(_.toInt).toList
    for (line, idx) <- hits.zipWithIndex do
      val f            = line.split('\t').map(_.split('=').last)
      val (n, ch)      = (f(2).toInt, f(3).toInt)
      val ds           = symbolFromKey(line.split('\t').last.stripPrefix("key="))
      val sys0         = MetricLayer.angleSystem(ds)
      val corner       = sys0.corner
      // the designation must be RECOMPUTED on the rebuilt symbol: the canonical key renumbers chambers, so
      // the orbit indices recorded in the hits file index a different orbit ordering (this cost a false
      // "DEAD" on the known (3.8.24) witness until the validation run caught it).
      val designations =
        for
          reg      <- UClass.candidates(ds, z)
          irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !reg(k) => k }
          if UClass.noneForcedRegular(ds, reg, irregular)
        yield reg
      println(s"\n--- candidate ${idx + 1}/${hits.size}: n=$n chambers=$ch, " +
        s"${designations.size} surviving designation(s)")
      for reg <- designations do
        val irr = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !reg(k) => k }
        println(s"  designation regular=${reg.toList.sorted.mkString(",")} irregular=${irr.mkString(",")}")

        // (2) the affine model: angle sums + regular pins + linear closure equivalents
        val extra                          = Vector.newBuilder[(Array[Frac], Frac)]
        var unhandled                      = List.empty[String]
        def eq(c1: Int, c2: Int): Unit     =
          if c1 != c2 then
            val row = Array.fill(sys0.vars)(Frac(0, 1))
            row(c1) = Frac(1, 1); row(c2) = Frac(-1, 1)
            extra += ((row, Frac(0, 1)))
        def pin(c: Int, value: Frac): Unit =
          val row = Array.fill(sys0.vars)(Frac(0, 1))
          row(c) = Frac(1, 1)
          extra += ((row, value))
        for (o, k) <- ds.orbs.zipWithIndex if o.i == 0 do
          val d0  = o.elements.head
          val p   = ds.m(0, 1, d0)
          val v   = ds.v(0, 1, d0)
          val seq = faceCornerSeq(ds, corner, d0)
          if reg(k) then seq.foreach(c => pin(c, Frac.make(p - 2L, p)))
          else if p == 3 then seq.foreach(c => pin(c, Frac(1, 3)))
          else if p == 4 then { eq(seq(0), seq(2)); eq(seq(1), seq(3)) }
          else if v >= 2 then ()
          else if p == 6 && o.isChain && o.elements.exists(d => ds.get(0, d) == d) then
            val adjPairs = (0 until 6).filter(j => seq(j) == seq((j + 1) % 6)).map(j => seq(j))
            if adjPairs.size == 2 then eq(adjPairs(0), adjPairs(1))
            else unhandled ::= s"orbit $k (p=$p, mirror hexagon with ${adjPairs.size} adjacent-equal pairs)"
          else unhandled ::= s"orbit $k (p=$p, v=$v)"
        val rows                           = sys0.rows ++ extra.result()

        // (3) consistency and rigidity
        val sol = particular(rows, sys0.vars)
        if sol.isEmpty then
          println("  VERDICT: DEAD — affine model INCONSISTENT (no angle assignment exists)")
        else
          val basis = MetricLayer.nullspaceBasis(MetricLayer.AngleSystem(sys0.vars, corner, rows))
          val nul   = basis.size
          println(s"  affine model consistent, dimension $nul" +
            (if unhandled.nonEmpty then s"; UNHANDLED irregular faces: ${unhandled.mkString(", ")}" else ""))
          if nul == 1 then
            // the 1-parameter family x(t) = x0 + t·b: per face orbit, corner angles are affine in t, so each
            // edge direction is exp(iπ(p_j + q_j t)) with p_j, q_j rational — print them for exact analysis
            val x0                              = sol.get
            val b                               = basis.head
            for (o, k) <- ds.orbs.zipWithIndex if o.i == 0 do
              val seq  = faceCornerSeq(ds, corner, o.elements.head)
              var p    = Frac(0, 1)
              var q    = Frac(0, 1)
              val prTs = seq.indices.map: j =>
                val e = (p, q)
                p = p + (Frac(1, 1) - x0(seq(j)))
                q = q - b(seq(j))
                e
              println(s"  FAMILY face orbit $k (p=${ds.m(0, 1, o.elements.head)}, reg=${reg(k)}): " +
                prTs.map((pj, qj) => s"(${pj.num}/${pj.den} + ${qj.num}/${qj.den}·t)").mkString(" "))
            // exact CONVEXITY WINDOW of the family: every corner angle x0_c + t·b_c must lie strictly in
            // (0, 1) (units of π). Each constraint is linear in t, so the window is an intersection of open
            // rational intervals — if it is EMPTY, every member of the family has a reflex (or degenerate)
            // face angle and the whole family is dead in the convex category, closure never consulted.
            def less(a: Frac, f: Frac): Boolean = a.num * f.den < f.num * a.den // dens normalized > 0
            var lo                              = Option.empty[Frac]            // strict lower bound for t
            var hi                              = Option.empty[Frac]            // strict upper bound for t
            var infeasible                      = false
            for c <- 0 until sys0.vars do
              val a0 = x0(c)
              val bc = b(c)
              if bc.isZero then infeasible ||= a0.num <= 0 || a0.num >= a0.den
              else
                val tAt0   = (Frac(0, 1) - a0) / bc
                val tAt1   = (Frac(1, 1) - a0) / bc
                val (l, h) = if bc.num > 0 then (tAt0, tAt1) else (tAt1, tAt0)
                lo = Some(lo.fold(l)(x => if less(x, l) then l else x))
                hi = Some(hi.fold(h)(x => if less(h, x) then h else x))
            val emptyWindow                     = infeasible || (lo, hi).match
              case (Some(l), Some(h)) => !less(l, h)
              case _                  => false
            if emptyWindow then
              println("  convexity window in t: EMPTY" +
                (if infeasible then " (a t-independent corner angle is outside (0, π))" else ""))
              println("  VERDICT: DEAD in the convex category — EVERY member of the dim-1 family has a " +
                "face angle ≥ π (exact: the convexity window is empty)")
            else
              println(s"  convexity window in t: (${lo.fold("−∞")(f => s"${f.num}/${f.den}")}, " +
                s"${hi.fold("+∞")(f => s"${f.num}/${f.den}")}) — non-empty, closure must be decided")
              println("  VERDICT: UNRESOLVED — flexible family (dim 1) with a non-empty convexity window; " +
                "nonlinear closure data printed above")
          else if nul > 0 then
            println(
              s"  VERDICT: UNRESOLVED — flexible family (dim $nul); closure is not a linear condition here"
            )
          else
            val x                                                  = sol.get
            val angles                                             = (0 until sys0.vars).map(x)
            // convex ⟺ 0 < angle < π, i.e. 0 < num/den < 1 with den > 0 (Frac is normalized)
            val convex                                             = angles.forall(a => a.num > 0 && a.num < a.den)
            // a STRAIGHT corner (angle exactly π) is no corner of a simple polygon: it subdivides an
            // edge, i.e. the tile has a length-2 edge — illegal in the unit-edge class (2026-08-17
            // audit: one fusion-era "GENUINE-REFLEX" pattern carried a π corner unflagged)
            val noPi                                               = angles.forall(a => a.num != a.den)
            // truth-designation: the irregular-designated faces must NOT take their regular angles
            val honest                                             = ds.orbs.zipWithIndex.forall: (o, k) =>
              o.i != 0 || reg(k) || {
                val p = ds.m(0, 1, o.elements.head)
                faceCornerSeq(ds, corner, o.elements.head).exists(c => x(c) != Frac.make(p - 2L, p))
              }
            // (4) exact closure of every face orbit, via vanishing sums of roots of unity
            val den                                                = angles.map(_.den).foldLeft(1L)(lcm)
            val nRoots                                             = (2 * den).toInt
            def faceExps(o: DelaneySymbols.Orbit): IndexedSeq[Int] =
              val seq = faceCornerSeq(ds, corner, o.elements.head)
              var acc = 0L
              seq.indices.map: j =>
                val e = acc
                val g = x(seq(j))
                acc += den - (den * g.num / g.den) // exterior turn, in units of π/den
                e.toInt
            val closes                                             = ds.orbs.forall(o => o.i != 0 || rootsOfUnitySumIsZero(faceExps(o), nRoots))
            // SIMPLICITY of every face boundary (2026-08-17, caught by the author's eye on the valence-2
            // (3².4.12) figure): closure and healthy angles do NOT imply a simple polygon — a reflex
            // boundary can SELF-TOUCH with every angle in (0, 2π)\{π} (the degenerate resolution there:
            // two unit triangles joined by a doubly-traversed slit, all angles 60°/210°). Two boundary
            // vertices coincide iff the edge vectors of a PROPER contiguous sub-range sum to zero — the
            // same exact vanishing-sums test as closure, applied to every sub-range. Convex faces are
            // simple for free; this decides the reflex ones exactly.
            val simple                                             = ds.orbs.forall: o =>
              o.i != 0 || {
                val exps = faceExps(o)
                val p    = exps.length
                (0 until p).forall: from =>
                  (1 until p).forall: len =>
                    !rootsOfUnitySumIsZero((0 until len).map(j => exps((from + j) % p)), nRoots)
              }
            println(f"  rigid; convex=$convex%b simple=$simple%b noStraight=$noPi%b " +
              f"truth-designation-honest=$honest%b closure=$closes%b (roots of unity: ζ_$nRoots)")
            if convex && simple && honest && closes then
              // the fusion question: is each irregular tile a union of unit regular polygons, or not?
              // Report its shape exactly — angles in π units and in degrees (they are rational multiples
              // of π at a rigid point).
              for (o, k) <- ds.orbs.zipWithIndex if o.i == 0 && !reg(k) do
                val seq        = faceCornerSeq(ds, corner, o.elements.head)
                val angs       = seq.map(c => x(c)).toList
                val degs       = angs.map(a => f"${180.0 * a.num.toDouble / a.den.toDouble}%.1f")
                val p          = ds.m(0, 1, o.elements.head)
                println(s"  IRREGULAR face orbit $k: p=$p " +
                  s"angles(π units)=${angs.map(a => s"${a.num}/${a.den}").mkString(",")} " +
                  s"degrees=${degs.mkString(",")}")
                val cornersD   = angs.map(a => 180.0 * a.num.toDouble / a.den.toDouble)
                val (ess, why) = essentiallyIrregular(p, cornersD)
                println(
                  s"    fusion? ${if ess then "NO FUSION" else "NOT decided — may be a union"}: $why"
                )
            println(
              if convex && simple && honest && closes then
                "  VERDICT: GENUINE WITNESS — realizes as a unit-edge tiling"
              else if !closes then "  VERDICT: DEAD — the forced angle point does not close"
              else if !simple || !noPi then
                "  VERDICT: DEAD — DEGENERATE: a face boundary self-touches or carries a straight " +
                  "corner (not a simple unit-edge polygon)"
              else if !convex then "  VERDICT: DEAD in the convex category — a face angle is ≥ π (reflex)"
              else "  VERDICT: DEAD — an irregular-designated face is forced regular"
            )
