package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE CONVEX IRREGULAR TILES OF U(z), enumerated exactly (default tier). Under the class as defined, a
  * vertex with an irregular tile shows the regular tiles as an arc of z, so the irregular corner is the angle
  * sum of the COMPLEMENTARY arc and the two regular tiles across the irregular tile's edges there are the
  * endpoints of the regular arc — the letters of z just outside the swallowed arc (the FLANK RULE). A convex
  * irregular tile therefore has corners in the finite alphabet S(z) of proper-arc sums, exterior angles
  * summing to 2π, hence boundedly many corners; every corner word is enumerated, closure is decided exactly
  * on the 2π/N direction lattice, and the flank rule is checked as a cyclic constraint: along each edge the
  * tile across must be admissible at both ends. The output is, per species, the complete list of convex
  * irregular tiles any U(z) tiling can carry — a theorem for every uniformity at once. Fixtures: the four
  * convex manuscript tiles survive; the 60°/120° rhombus of (3.4².6) closes but has no legal collar.
  */
class ConvexTileSpec extends AnyFlatSpec with Matchers:

  /** One way a corner can arise: the swallowed arc's angle, and the regular tiles across the incoming and
    * outgoing edges of the irregular tile at that corner (the letters of z just outside the swallowed arc, in
    * either orientation).
    */
  final case class Corner(angle: Frac, flankIn: Int, flankOut: Int, arc: List[Int])

  private def angleOf(p: Int): Frac         = Frac.make(p - 2L, p)
  private def sum(fs: Seq[Frac]): Frac      = fs.foldLeft(Frac(0, 1))(_ + _)
  private def eq(a: Frac, b: Frac): Boolean = a.num * b.den == b.num * a.den
  private def lt(a: Frac, b: Frac): Boolean = a.num * b.den < b.num * a.den

  /** All corner options of z: proper non-empty contiguous arcs, both orientations, straight corners excluded.
    */
  def corners(z: List[Int]): List[Corner] =
    val m = z.length
    (for
      i  <- 0 until m; len <- 1 until m
      sw  = (0 until len).map(k => z((i + k) % m)).toList
      a   = sum(sw.map(angleOf))
      if !eq(a, Frac(1, 1))
      reg = (len until m).map(k => z((i + k) % m)).toList // the regular arc, from after to before
      c  <- List(Corner(a, reg.head, reg.last, reg), Corner(a, reg.last, reg.head, reg.reverse))
    yield c).toList.distinct

  /** The convex corner ANGLES of z with their exterior turns in units of 2π/n, n the common direction
    * lattice.
    */
  def convexAngles(z: List[Int]): (Int, Map[Frac, Int]) =
    val as                          = corners(z).map(_.angle).filter(lt(_, Frac(1, 1))).distinct
    // turn/2π = (1 − a)/2 ; n = even lcm of the denominators
    val turns                       = as.map(a => Frac.make(a.den - a.num, 2L * a.den))
    def lcm(x: Long, y: Long): Long = x / gcd(x, y) * y
    def gcd(x: Long, y: Long): Long = if y == 0 then x else gcd(y, x % y)
    val n0                          = turns.map(_.den).foldLeft(1L)(lcm)
    val n                           = (if n0 % 2 == 0 then n0 else 2 * n0).toInt
    (n, as.zip(turns).map((a, t) => a -> (t.num * (n / t.den)).toInt).toMap)

  private def canonical(w: Vector[Int]): Vector[Int] =
    val rots = w.indices.map(i => w.drop(i) ++ w.take(i))
    (rots ++ rots.map(_.reverse)).minBy(_.map(u => f"$u%04d").mkString)

  /** Every cyclic word of convex corner angles (as turn units) with total turning 2π, up to rotation and
    * reflection, that closes as a unit polygon and is not a regular polygon.
    */
  def closedConvexWords(z: List[Int]): (Int, Map[Int, Frac], Vector[Vector[Int]]) =
    val (n, byAngle)                          = convexAngles(z)
    val units                                 = byAngle.values.toList.distinct.sorted
    val angleOfUnit                           = byAngle.map((a, u) => u -> a)
    val words                                 = collection.mutable.Set.empty[Vector[Int]]
    def go(acc: Vector[Int], left: Int): Unit =
      if left == 0 then { if acc.length >= 3 then words += canonical(acc) }
      else for u <- units if u <= left do go(acc :+ u, left - u)
    go(Vector.empty, n)
    val closed                                = words.toVector.sorted(Ordering.Implicits.seqOrdering[Vector, Int]).filter { w =>
      val dirs = w.scanLeft(0)(_ + _).init.map(_ % n)
      UnitPolygon(n, dirs).isClosed && w.distinct.length > 1 // a closed equiangular unit polygon is regular
    }
    (n, angleOfUnit, closed)

  /** The collars of a corner word: assignments of corner options with the tile across each edge admissible at
    * both of its ends. Returns the edge letters of each solution.
    */
  def collars(z: List[Int], angles: Vector[Frac]): Vector[Vector[Int]] =
    val opts                                     = angles.map(a => corners(z).filter(c => eq(c.angle, a)))
    val k                                        = angles.length
    val out                                      = collection.mutable.ArrayBuffer.empty[Vector[Int]]
    def go(i: Int, chosen: Vector[Corner]): Unit =
      if i == k then
        if chosen(k - 1).flankOut == chosen(0).flankIn then out += chosen.map(_.flankOut)
      else
        for c <- opts(i) if i == 0 || chosen(i - 1).flankOut == c.flankIn do go(i + 1, chosen :+ c)
    go(0, Vector.empty)
    out.toVector.distinct

  private def deg(a: Frac): Int = (180L * a.num / a.den).toInt

  /** Per species: (n, closed convex irregular corner words with their collars). */
  def catalogue(z: List[Int]): (Int, Vector[(Vector[Int], Vector[Vector[Int]])]) =
    val (n, angleOfUnit, closed) = closedConvexWords(z)
    (n, closed.map(w => (w.map(u => deg(angleOfUnit(u))), collars(z, w.map(angleOfUnit)))))

  private val species = UClass.targets.map(_._1)

  behavior of "the convex irregular tiles of U(z) (class as defined: arc plus one tile per vertex)"

  it should "reproduce the corner alphabets on hand examples" in:
    corners(List(3, 3, 6, 6)).map(c => deg(c.angle)).distinct.sorted shouldBe List(60, 120, 240, 300)
    corners(List(3, 4, 4, 6)).map(c => deg(c.angle)).distinct.sorted shouldBe
      List(60, 90, 120, 150, 210, 240, 270, 300)
    corners(List(4, 5, 20)).map(c => deg(c.angle)).distinct.sorted shouldBe List(90, 108, 162, 198, 252, 270)
    // the pinwheel corner: 210 = 90 + 120 swallows (4.6), flanked by 3 and 4
    corners(List(3, 4, 4, 6)).filter(c => deg(c.angle) == 210).map(c =>
      Set(c.flankIn, c.flankOut)
    ).distinct shouldBe
      List(Set(3, 4))

  it should "kill the 60°/120° rhombus for (3.4².6) by the flank rule, and keep it for (3².6²)" in:
    val rhombus = Vector(Frac(1, 3), Frac(2, 3), Frac(1, 3), Frac(2, 3))
    collars(List(3, 4, 4, 6), rhombus) shouldBe empty
    collars(List(3, 3, 6, 6), rhombus) should not be empty

  it should "keep the four convex manuscript tiles, with a collar" in:
    def has(z: List[Int], word: Vector[Int]): Boolean =
      catalogue(z)._2.exists((w, cs) => canonical(w) == canonical(word) && cs.nonEmpty)
    has(List(3, 8, 24), Vector(135, 135, 165, 165, 135, 135, 165, 165, 135, 135, 165, 165)) shouldBe true
    has(List(3, 4, 3, 12), Vector(90, 150, 90, 150, 90, 150)) shouldBe true
    has(List(3, 9, 18), Vector(140, 60, 160, 140, 60, 160)) shouldBe true
    has(List(5, 5, 10), Vector(108, 108, 144, 108, 108, 144)) shouldBe true

  /** The regular tiles that can flank a regular `p`-gon at a vertex of a CONVEX tiling: from the all-regular
    * vertex z, and from every convex corner option whose regular arc contains `p` — `None` marks the
    * irregular tile across an edge.
    */
  def flankPairs(z: List[Int], p: Int): Set[Set[Option[Int]]] =
    val m                          = z.length
    val reg: Set[Set[Option[Int]]] =
      (for i <- z.indices if z(i) == p yield Set(Some(z((i - 1 + m) % m)), Some(z((i + 1) % m)))).toSet
    val irr: Seq[Set[Option[Int]]] =
      for
        c    <- corners(z) if lt(c.angle, Frac(1, 1))
        arc   = c.arc
        j    <- arc.indices if arc(j) == p
        left  = if j == 0 then None else Some(arc(j - 1))
        right = if j == arc.length - 1 then None else Some(arc(j + 1))
      yield Set[Option[Int]](left, right)
    reg ++ irr.toSet

  it should "prove the convex rows for (3.4².6) and (3².6²): the triangle lemma and the collar census" in:
    // (3.4^2.6): a triangle's two neighbours at any vertex are two DISTINCT members of {square, hexagon, T}
    flankPairs(List(3, 4, 4, 6), 3) shouldBe
      Set(Set(Some(4), Some(6)), Set(None, Some(4)), Set(None, Some(6)))
    // so every triangle is adjacent to exactly one irregular tile; but of the four convex tiles only the
    // (150^2 120^2)^2 octagon admits a triangle in ANY collar, at an edge between two 120° corners whose arcs
    // 3.4.4 give that triangle a square on both other edges — its far vertex would see {4, 4}
    val (_, cat346)  = catalogue(List(3, 4, 4, 6))
    val live346      = cat346.filter(_._2.nonEmpty)
    live346.map(_._1) should contain theSameElementsAs List(
      Vector(150, 150, 150, 90, 150, 150, 150, 90),
      Vector(150, 150, 120, 120, 150, 150, 120, 120),
      Vector(150, 150, 60, 150, 150, 60),
      Vector(150, 90, 150, 90, 150, 90)
    )
    for (w, cs) <- live346 do
      withClue(s"$w: "):
        if w == Vector(150, 150, 120, 120, 150, 150, 120, 120) then
          cs.forall(c => c.count(_ == 3) == 2) shouldBe true
          // the triangles sit between the two 120° corners
          cs.forall(c =>
            c.indices.filter(i => c(i) == 3).forall(i => w(i) == 120 && w((i + 1) % 8) == 120)
          ) shouldBe true
        else cs.forall(c => !c.contains(3)) shouldBe true
    // (3^2.6^2): the rhombus is the only convex irregular tile, with a unique collar 6.3.6.3 up to symmetry;
    // triangles and hexagons both see two DISTINCT members of {3, 6, T} at every vertex
    val (_, cat3366) = catalogue(List(3, 3, 6, 6))
    cat3366.map(_._1) shouldBe Vector(Vector(120, 60, 120, 60))
    cat3366.head._2.map(c => canonical(c)).distinct shouldBe Vector(Vector(3, 6, 3, 6))
    flankPairs(List(3, 3, 6, 6), 3) shouldBe
      Set(Set(Some(3), Some(6)), Set(None, Some(6)), Set(None, Some(3)))
    flankPairs(List(3, 3, 6, 6), 6) shouldBe
      Set(Set(Some(3), Some(6)), Set(None, Some(6)), Set(None, Some(3)))
    // (5^2.10): exactly one convex irregular tile — the witness hexagon
    catalogue(List(5, 5, 10))._2.filter(_._2.nonEmpty).map(_._1) shouldBe
      Vector(Vector(144, 108, 108, 144, 108, 108))

  it should "list the complete convex catalogue per species" in:
    for z <- species do
      val t0         = System.nanoTime
      val (n, cat)   = catalogue(z)
      val withCollar = cat.filter(_._2.nonEmpty)
      println(
        f"\nz=${z.mkString(".")}  N=$n  closed convex irregular words=${cat.size}  with a legal collar=${withCollar.size}  [${(System.nanoTime -
            t0) / 1e9}%.1fs]"
      )
      for (w, cs) <- withCollar do
        println(
          s"  ${w.length}-gon ${w.mkString("(", ".", ")")}  collars=${cs.size}  e.g. edges across: ${cs.head.mkString(".")}"
        )
      if withCollar.isEmpty && cat.nonEmpty then
        println(s"  closed but collar-less: ${cat.map(_._1.mkString("(", ".", ")")).mkString(", ")}")
