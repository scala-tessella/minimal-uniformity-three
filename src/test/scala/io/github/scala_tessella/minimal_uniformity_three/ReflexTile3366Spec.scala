package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import io.github.scala_tessella.minimal_uniformity_three.StrictWitnesses.symbolFromKey
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE MINIMAL UNIFORMITY OF (3².6²) IS 5 — the machine-checked ingredients (default tier, seconds).
  *
  * By thm:uplus3366 every member of U(3².6²) is a tiling of the triangular lattice: regular hexagons, unit
  * triangles and irregular lattice polygons whose corners are the six irregular-corner types of the species
  * (thm:flank), each with its flanks — the regular tiles across the tile's two edges at that corner:
  *
  * A (T,3,6,6) 60° flanks 3|6 B (T,3,3) 240° flanks 3|3 C (T,6,3,3) 120° flanks 6|3 D (T,6,6) 120° flanks 6|6
  * E (T,6) 240° flanks 6|6 F (T,3) 300° flanks 3|3
  *
  * (E and F are the valence-2 corners: the tile hugs a hexagon corner, a triangle corner.) The proof in the
  * manuscript reduces a member of uniformity k ≤ 4 to ONE irregular tile P up to symmetry, whose corners fall
  * into at most three classes under Stab(P) — corners of P in one orbit of the symmetry group are in one
  * orbit of Stab(P), since no vertex lies on two irregular tiles — and derives local lemmas on the corner
  * word of P: every species vertex is the apex of a triangle pair in a B notch of P between two A tips; a B
  * notch is species-apexed (neighbours A, A) or hugged (…A B F C…); a tip's triangle-flanked edge leads to B,
  * C or F; a C's triangle-flanked edge leads to A or into a hugged notch; an F sits between A, B or C
  * corners. This spec enumerates EVERY labelled corner word obeying the flank rule and those lemmas, closed
  * and simple on the exact 2π/6 direction lattice, with at most three symmetry classes of corners: exactly
  * two survive, the hexagram (A B A E)³ and the 30-gon (A B A E E)⁶, and for both the orbit counting of the
  * proof forces the full point group D₆ and a translation lattice of index 39, respectively 63 — neither the
  * norm of a self-conjugate ideal of ℤ[ω] (13 and 7 are ≡ 1 mod 3 to an odd power), so no D₆-invariant
  * translation lattice has that index. Fixtures: the k = 5 member's tile (A B A D D D) appears as soon as
  * four classes are allowed and its banked symbol has exactly the five orbits the stabiliser lemma predicts
  * (species, B, A, and TWO D orbits — one on the tile's mirror).
  */
class ReflexTile3366Spec extends AnyFlatSpec with Matchers:

  /** A corner type: interior angle, and the regular tiles across the tile's two edges there (as a set). */
  enum Kind(val angle: Int, val flanks: Set[Int]):
    case A extends Kind(60, Set(3, 6))
    case B extends Kind(240, Set(3))
    case C extends Kind(120, Set(3, 6))
    case D extends Kind(120, Set(6))
    case E extends Kind(240, Set(6))
    case F extends Kind(300, Set(3))
  import Kind.*

  /** An oriented corner: its kind and the flanks of the incoming and outgoing edges. */
  final case class Oriented(kind: Kind, in: Int, out: Int):
    def flipped: Oriented = Oriented(kind, out, in)

  /** Kind C is (T,6,3,3), whose 120° = the swallowed hexagon; kind D is (T,6,6), 120° = two swallowed
    * triangles: both angles and flank sets are needed to tell the six types apart. Derived from the arcs of
    * (3.3.6.6): the irregular corner is the angle sum of the swallowed arc, its flanks the ends of the
    * complementary regular arc.
    */
  def typesFromArcs: Set[(Int, Set[Int], List[Int])] =
    val z = List(3, 3, 6, 6)
    val m = z.length
    (for
      i   <- 0 until m; len <- 1 until m
      sw   = (0 until len).map(k => z((i + k) % m)).toList
      a    = sw.map(p => 180 * (p - 2) / p).sum
      if a != 180
      reg  = (len until m).map(k => z((i + k) % m)).toList
      regC = List(reg, reg.reverse).min(Ordering.Implicits.seqOrdering[List, Int])
    yield (a, Set(reg.head, reg.last), regC)).toSet

  val oriented: Vector[Oriented] =
    Vector(
      Oriented(A, 3, 6),
      Oriented(A, 6, 3),
      Oriented(B, 3, 3),
      Oriented(C, 6, 3),
      Oriented(C, 3, 6),
      Oriented(D, 6, 6),
      Oriented(E, 6, 6),
      Oriented(F, 3, 3)
    )

  private def turnUnits(k: Kind): Int = (180 - k.angle) / 60

  /** The local lemmas on a cyclic oriented word (flank consistency included). */
  def obeysLemmas(w: Vector[Oriented]): Boolean =
    val n               = w.length
    def at(i: Int)      = w(((i % n) + n) % n)
    def kind(i: Int)    = at(i).kind
    val consistent      = (0 until n).forall(i => at(i).out == at(i + 1).in)
    // the corner reached along the triangle-flanked edge of corner i (kinds A and C have exactly one)
    def acrossT(i: Int) = if at(i).in == 3 then i - 1 else i + 1
    def nb(i: Int)      = (kind(i - 1), kind(i + 1))
    consistent &&
    w.exists(_.kind == A) && w.exists(_.kind == B) &&
    (0 until n).forall { i =>
      kind(i) match
        case B => // species-apexed, or hugged: … A B F C …
          nb(i) ==
            (A, A) ||
            (kind(i - 1) == A && kind(i + 1) == F && kind(i + 2) == C) ||
            (kind(i + 1) == A && kind(i - 1) == F && kind(i - 2) == C)
        case A => Set(B, C, F).contains(kind(acrossT(i)))
        case C =>
          val j = acrossT(i)
          kind(j) == A || (kind(j) == F && kind(2 * j - i) == B)
        case F => Set(A, B, C).contains(kind(i - 1)) && Set(A, B, C).contains(kind(i + 1))
        case _ => true
    }

  /** Symmetries of the labelled cyclic word as permutations of corner positions: rotations, and reflections
    * (which flip every corner's orientation).
    */
  def symmetries(w: Vector[Oriented]): Vector[Int => Int] =
    val n    = w.length
    val rots = (0 until n).filter(r => (0 until n).forall(i => w((i + r) % n) == w(i))).map(r =>
      (i: Int) => (i + r) % n
    )
    val refl = (0 until n).filter(c => (0 until n).forall(i => w(((c - i) % n + n) % n) == w(i).flipped)).map(
      c =>
        (i: Int) => ((c - i) % n + n) % n
    )
    (rots ++ refl).toVector

  def cornerClasses(w: Vector[Oriented]): Int =
    val syms = symmetries(w)
    val n    = w.length
    val seen = Array.fill(n)(false)
    var k    = 0
    for i <- 0 until n if !seen(i) do
      k += 1
      var frontier = List(i)
      seen(i) = true
      while frontier.nonEmpty do
        val x = frontier.head
        frontier = frontier.tail
        for s <- syms do
          val y = s(x)
          if !seen(y) then { seen(y) = true; frontier = y :: frontier }
    k

  def polygon(w: Vector[Oriented]): UnitPolygon =
    val turns = w.map(o => turnUnits(o.kind))
    val dirs  = turns.tail.scanLeft(0)((d, t) => ((d + t) % 6 + 6) % 6)
    UnitPolygon(6, dirs)

  /** Area in unit triangles, exactly: shoelace in Eisenstein coordinates (1, ζ₆), one parallelogram = two
    * triangles.
    */
  def triangles(p: UnitPolygon): Int =
    val step = Vector((1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1))
    val vs   = p.dirs.init.scanLeft((0, 0))((v, d) => (v._1 + step(d)._1, v._2 + step(d)._2))
    val n    = vs.length
    (0 until n).map(i => vs(i)._1 * vs((i + 1) % n)._2 - vs((i + 1) % n)._1 * vs(i)._2).sum.abs

  /** Interior lattice points by Pick's theorem on the triangular lattice: area = 2I + B − 2, B = the corners
    * (every boundary lattice point is a corner: unit edges, no straight corners).
    */
  def interiorPoints(p: UnitPolygon): Int = (triangles(p) - p.dirs.length + 2) / 2

  private def canonical(w: Vector[Oriented]): Vector[Oriented] =
    val rots = w.indices.map(i => w.drop(i) ++ w.take(i))
    val all  = rots ++ rots.map(_.reverse.map(_.flipped))
    all.minBy(_.map(o => s"${o.kind}${o.in}${o.out}").mkString)

  /** Every labelled corner word with at most `classes` symmetry classes of corners obeying the lemmas, closed
    * and simple. A word with at most c classes under a symmetry group of order s has at most c·s corners, s ≤
    * 12, and is a period repeated m ∈ {1, 2, 3, 6} times with a period of at most 6 letters when a reflection
    * is present (orbits of size ≤ 2m) and at most 3 without; both cases are covered by periods of length ≤
    * 2·classes.
    */
  def survivors(classes: Int): Vector[Vector[Oriented]] =
    val found                                        = collection.mutable.Set.empty[Vector[Oriented]]
    def go(acc: Vector[Oriented], maxLen: Int): Unit =
      if acc.nonEmpty then
        for m <- List(1, 2, 3, 6) do
          val w = Vector.fill(m)(acc).flatten
          // the labels' turns must total 2π, or the wrap-around corner would not carry its label
          if w.length >= 3 && w.map(o => turnUnits(o.kind)).sum == 6 && obeysLemmas(w) &&
            cornerClasses(w) <= classes
          then
            val p = polygon(w)
            if p.isSimpleCertified then found += canonical(w)
      if acc.length < maxLen then
        for o <- oriented if acc.isEmpty || acc.last.out == o.in do go(acc :+ o, maxLen)
    go(Vector.empty, 2 * classes)
    found.toVector.sortBy(w => (w.length, w.map(_.kind.toString).mkString))

  private def show(w: Vector[Oriented]): String = w.map(_.kind.toString).mkString

  /** Prime factorisation as a map. */
  private def factors(n: Int): Map[Int, Int] =
    var x   = n
    var p   = 2
    var acc = Map.empty[Int, Int]
    while p * p <= x do
      while x % p == 0 do { acc = acc.updated(p, acc.getOrElse(p, 0) + 1); x /= p }
      p += 1
    if x > 1 then acc.updated(x, acc.getOrElse(x, 0) + 1) else acc

  /** Is n the norm of an ideal of ℤ[ω] equal to its own conjugate — 3^e times a square? Such are exactly the
    * indices of the translation lattices invariant under the full point group D₆ of the triangular lattice.
    */
  def selfConjugateNorm(n: Int): Boolean = factors(n).forall((p, e) => p == 3 || e % 2 == 0)

  behavior of "the reflex irregular tiles of U(3².6²) at uniformity at most 4"

  it should "derive the six corner types from the arcs of (3.3.6.6)" in:
    typesFromArcs shouldBe Set(
      (60, Set(3, 6), List(3, 6, 6)),
      (240, Set(3), List(3, 3)),
      (120, Set(3, 6), List(3, 3, 6)),
      (120, Set(6), List(6, 6)),
      (240, Set(6), List(6)),
      (300, Set(3), List(3))
    )
    Kind.values.map(k => (k.angle, k.flanks)).toSet shouldBe typesFromArcs.map((a, f, _) => (a, f))

  it should "find the k = 5 member's tile, with four classes, when four classes are allowed" in:
    val four   = survivors(4)
    val tile   = four.filter(w => w.map(_.kind) == Vector(A, B, A, D, D, D) || show(w).sorted == "AABDDD")
    tile should not be empty
    tile.foreach(w => cornerClasses(w) shouldBe 4)
    // the bowtie (A B A)² closes but is not simple; with only A and B no word survives at all
    val bowtie = Vector(
      Oriented(A, 6, 3),
      Oriented(B, 3, 3),
      Oriented(A, 3, 6),
      Oriented(A, 6, 3),
      Oriented(B, 3, 3),
      Oriented(A, 3, 6)
    )
    obeysLemmas(bowtie) shouldBe true
    polygon(bowtie).isClosed shouldBe true
    polygon(bowtie).isSimpleCertified shouldBe false
    four.filter(w => w.forall(o => o.kind == A || o.kind == B)) shouldBe empty

  it should "leave exactly the hexagram (A B A E)³ and the 30-gon (A B A E E)⁶ with three classes" in:
    val three = survivors(3)
    three.map(show) shouldBe Vector("AEAB" * 3, "AEEAB" * 6)
    three.map(cornerClasses) shouldBe Vector(3, 3)
    three.map(w => symmetries(w).length) shouldBe Vector(6, 12)
    three.map(w => triangles(polygon(w))) shouldBe Vector(12, 66)
    three.map(w => interiorPoints(polygon(w))) shouldBe Vector(1, 19)

  /** The counting of the proof for a survivor with p notches, e hugged hexagon corners, symmetry group of
    * order s = 2p: per translation cell with point group of order g, the tile occurs i = g/s times; the A
    * corners have trivial stabiliser (nA = g), the B corners and the species vertices lie on mirrors (nB = n0 =
    * g/2), nE = i·e; triangles t = 2·nB; hexagons h = (2n0 + 2nA + nE)/6; the translation index is V + h +
    * i·I(P). Returns the admissible g among the wallpaper point-group orders and the index at each.
    */
  def indices(w: Vector[Oriented]): Vector[(Int, Int)] =
    val p     = w.count(_.kind == B)
    val e     = w.count(_.kind == E)
    val s     = symmetries(w).length
    val inner = interiorPoints(polygon(w))
    for
      g <- Vector(1, 2, 3, 4, 6, 8, 12)
      if g % s == 0
      i  = g / s
      nA = g
      if nA == i * w.count(_.kind == A)
      if g % 2 == 0
      nB   = g / 2
      if nB == i * p
      n0   = nB
      nE   = i * e
      hex6 = 2 * n0 + 2 * nA + nE
      if hex6 % 6 == 0
      h = hex6 / 6
      if 3 * (2 * nB) == 2 * n0 + nA + 2 * nB // triangle corners
      v = n0 + nA + nB + nE
    yield (g, v + h + i * inner)

  it should "force the point group D₆ on both survivors and a translation index no D₆-lattice has" in:
    val three = survivors(3)
    three.map(indices) shouldBe Vector(Vector((12, 39)), Vector((12, 63)))
    selfConjugateNorm(39) shouldBe false
    selfConjugateNorm(63) shouldBe false
    // the criterion on the lattices the sweep did visit: 3ℤ[ω] (index 9) carries the k = 5 member
    List(1, 3, 4, 9, 12, 13 * 13, 3 * 49).forall(selfConjugateNorm) shouldBe true
    List(7, 13, 21, 39, 63, 3 * 13).exists(selfConjugateNorm) shouldBe false

  it should "read the five orbits of the banked k = 5 member as the stabiliser lemma predicts" in:
    val key                                          =
      StrictWitnesses.entries.find(_.name.contains("lattice k=5")).getOrElse(fail("k=5 entry missing")).key
    val ds                                           = symbolFromKey(key)
    val n                                            = ds.size
    def orbits(gens: List[Int]): Vector[Vector[Int]] =
      val seen = Array.fill(n + 1)(false)
      (1 to n).toVector.flatMap { c =>
        if seen(c) then None
        else
          var acc      = Vector.empty[Int]
          var frontier = List(c)
          seen(c) = true
          while frontier.nonEmpty do
            val x = frontier.head
            frontier = frontier.tail
            acc = acc :+ x
            for g <- gens do
              val y = ds.get(g, x)
              if !seen(y) then { seen(y) = true; frontier = y :: frontier }
          Some(acc.sorted)
      }
    val faces                                        = orbits(List(0, 1))
    val faceOf                                       = faces.indices.flatMap(f => faces(f).map(_ -> f)).toMap
    // the irregular hexagon is the 6-gon face orbit with 6 chambers (a mirror), the regular one has 12
    val irregular                                    = faces.indexWhere(o => o.length == 6 && ds.m(0, 1, o.head) == 6)
    irregular should be >= 0
    def around(c: Int): Vector[(Int, Boolean)]       =
      val start = c
      var x     = c
      var acc   = Vector.empty[(Int, Boolean)]
      while
        acc = acc :+ (ds.m(0, 1, x), faceOf(x) == irregular)
        x = ds.get(1, ds.get(2, x))
        x != start
      do ()
      acc
    val verts                                        = orbits(List(1, 2)).map(o => (o.length, around(o.head)))
    // (chambers, tiles around: (size, irregular?)) — chambers = 2·valence / |stabiliser|
    verts.map(_._1).sorted shouldBe Vector(3, 3, 4, 6, 8)
    def kindOf(t: Vector[(Int, Boolean)]): String    =
      val irr = t.count(_._2)
      val reg = t.filterNot(_._2).map(_._1).sorted
      if irr == 0 then "species" else s"T${reg.mkString(".")}"
    verts.map(v => (kindOf(v._2), v._1)).sortBy(_.toString) shouldBe
      Vector(("T3.3", 3), ("T3.6.6", 8), ("T6.6", 3), ("T6.6", 6), ("species", 4)).sortBy(_.toString)
