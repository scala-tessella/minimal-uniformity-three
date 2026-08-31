package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.Frac
import io.github.scala_tessella.research_core.ExactPlane.UnitPolygon
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE IRREGULAR TILES A THREE-ORBIT MEMBER OF U(z) CAN CARRY (default tier, seconds).
  *
  * No vertex lies on two irregular tiles (thm:flank), so a symmetry carrying a corner of an irregular tile P
  * to a corner of P fixes P: corners of P in one vertex orbit are in one Stab(P)-orbit, and tiles in
  * different orbits use disjoint vertex orbits. A tile whose corners form ONE class is bounded by a
  * vertex-transitive unit polygon — regular, not irregular. Hence a member of uniformity 3 has, up to
  * symmetry, exactly ONE irregular tile, whose corners fall into exactly TWO classes under its own symmetry
  * group, a finite subgroup of a wallpaper group (rotation order 1, 2, 3, 4 or 6). This spec enumerates, per
  * species, every such labelled corner word: corner options from the arcs of z with their flanks (the flank
  * rule), collar-consistent along every edge, closed and simple on the exact direction lattice, with at most
  * two symmetry classes of corners. A vertex of valence 2 is a corner hugging a regular polygon — the type
  * whose regular arc is a single letter — so the valence-2 regime of the three-orbit question reduces to the
  * two-class words with a hugging corner; the chamber bound (ChamberBoundSpec) then places any tiling
  * carrying one inside the catalogue already swept in both regimes. Fixtures: the three uniformity-3
  * witnesses of the manuscript are two-class words of their species.
  */
class TwoClassTileSpec extends AnyFlatSpec with Matchers:

  /** A corner option: angle, the regular tiles across the incoming and outgoing edges, the regular arc. */
  final case class Corner(angle: Frac, flankIn: Int, flankOut: Int, arc: List[Int]):
    def flipped: Corner   = Corner(angle, flankOut, flankIn, arc.reverse)
    def hugging: Boolean  = arc.length == 1
    def deg: Int          = (180L * angle.num / angle.den).toInt
    override def toString = s"$deg[$flankIn|$flankOut]"

  private def angleOf(p: Int): Frac         = Frac.make(p - 2L, p)
  private def sum(fs: Seq[Frac]): Frac      = fs.foldLeft(Frac(0, 1))(_ + _)
  private def eq(a: Frac, b: Frac): Boolean = a.num * b.den == b.num * a.den

  /** All corner options of z (proper non-empty contiguous arcs, both orientations, straight excluded). */
  def corners(z: List[Int]): Vector[Corner] =
    val m = z.length
    (for
      i  <- 0 until m; len <- 1 until m
      sw  = (0 until len).map(k => z((i + k) % m)).toList
      a   = sum(sw.map(angleOf))
      if !eq(a, Frac(1, 1))
      reg = (len until m).map(k => z((i + k) % m)).toList
      c  <- List(Corner(a, reg.head, reg.last, reg), Corner(a, reg.last, reg.head, reg.reverse))
    yield c).toVector.distinct

  /** The even direction lattice on which every corner of z is exact, and the turn of each corner in its
    * units: turn/2π = (1 − a)/2 for an interior angle a (in units of π).
    */
  def lattice(z: List[Int]): (Int, Corner => Int) =
    val cs                          = corners(z)
    val turns                       = cs.map(c => Frac.make(c.angle.den - c.angle.num, 2L * c.angle.den))
    def gcd(x: Long, y: Long): Long = if y == 0 then x else gcd(y, x % y)
    def lcm(x: Long, y: Long): Long = x / gcd(x, y) * y
    val n0                          = turns.map(_.den).foldLeft(1L)(lcm)
    val n                           = (if n0 % 2 == 0 then n0 else 2 * n0).toInt
    val byCorner                    = cs.zip(turns).map((c, t) => c -> (t.num * (n / t.den)).toInt).toMap
    (n, byCorner)

  def symmetries(w: Vector[Corner]): Vector[Int => Int] =
    val n    = w.length
    val rots = (0 until n).filter(r => (0 until n).forall(i => w((i + r) % n) == w(i))).map(r =>
      (i: Int) => (i + r) % n
    )
    val refl = (0 until n).filter(c => (0 until n).forall(i => w(((c - i) % n + n) % n) == w(i).flipped)).map(
      c => (i: Int) => ((c - i) % n + n) % n
    )
    (rots ++ refl).toVector

  def classes(w: Vector[Corner]): Int =
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

  private def canonical(w: Vector[Corner]): Vector[Corner] =
    val rots = w.indices.map(i => w.drop(i) ++ w.take(i))
    (rots ++ rots.map(_.reverse.map(_.flipped))).minBy(_.map(_.toString).mkString(","))

  /** Every collar-consistent labelled corner word of z with at most `maxClasses` symmetry classes of corners,
    * closed and simple. With c classes under a symmetry group of order s ≤ 2m (m the rotation order, in {1,
    * 2, 3, 4, 6}) the word has at most c·s corners, so it is a period of at most 2c letters repeated m times.
    */
  def words(z: List[Int], maxClasses: Int): Vector[Vector[Corner]] =
    val (n, turn)                                  = lattice(z)
    val letters                                    = corners(z)
    val found                                      = collection.mutable.Set.empty[Vector[Corner]]
    def go(acc: Vector[Corner], maxLen: Int): Unit =
      if acc.nonEmpty then
        for m <- List(1, 2, 3, 4, 6) do
          val w = Vector.fill(m)(acc).flatten
          if w.length >= 3 && w.last.flankOut == w.head.flankIn && w.map(turn).sum == n &&
            classes(w) <= maxClasses
          then
            val dirs = w.map(turn).tail.scanLeft(0)((d, t) => ((d + t) % n + n) % n)
            if UnitPolygon(n, dirs).isSimpleCertified then found += canonical(w)
      if acc.length < maxLen then
        for c <- letters if acc.isEmpty || acc.last.flankOut == c.flankIn do go(acc :+ c, maxLen)
    go(Vector.empty, 2 * maxClasses)
    found.toVector.sortBy(w => (w.length, w.map(_.toString).mkString(",")))

  private def show(w: Vector[Corner]): String = w.map(_.deg).mkString("(", ".", ")")

  private val species = UClass.targets.map(_._1)

  behavior of "the two-class irregular tiles of a three-orbit member of U(z)"

  it should "contain the three uniformity-3 witnesses" in:
    def has(z: List[Int], degs: Vector[Int]): Boolean =
      val target = degs
      words(z, 2).exists(w =>
        w.map(_.deg) == target || w.map(_.deg).reverse == target || {
          val d = w.map(_.deg)
          d.indices.exists(i => (d.drop(i) ++ d.take(i)) == target)
        }
      )
    has(List(3, 8, 24), Vector(135, 135, 165, 165, 135, 135, 165, 165, 135, 135, 165, 165)) shouldBe true
    has(List(3, 4, 3, 12), Vector(90, 150, 90, 150, 90, 150)) shouldBe true
    has(List(3, 4, 4, 6), Vector.fill(6)(Vector(90, 210)).flatten) shouldBe true

  it should "list the two-class words per species, and their hugging corners" in:
    for z <- species do
      val ws = words(z, 2)
      println(s"\nz=${z.mkString(".")}  two-class words=${ws.size}")
      for w <- ws do
        val hug = w.filter(_.hugging).map(c => s"${c.deg} hugging ${c.arc.head}").distinct
        println(
          s"  ${w.length}-gon ${show(w)}  collar ${w.map(_.flankOut).mkString(".")}  classes=${classes(w)}" +
            (if hug.nonEmpty then s"  VALENCE-2: ${hug.mkString(", ")}" else "")
        )

  it should "leave (3².4.12) four two-class tiles with a hugging corner, all moot under the chamber bound" in:
    // (120.120.210)^4 and (210.210.90.90)^3 hugging 12-gons, (300.90.90)^6 hugging triangles, and
    // (120.120.210.210)^6 hugging 12-gons: a three-orbit tiling carrying one would have at most 24 chambers
    // (ChamberBoundSpec), and the three-orbit catalogue to 24 chambers, both valence regimes, holds no
    // (3².4.12) member — so the valence-2 regime adds nothing to the species' three-orbit refutation
    val ws = words(List(3, 3, 4, 12), 2)
    ws.filter(_.exists(_.hugging)).map(_.length).sorted shouldBe Vector(12, 12, 18, 24)
