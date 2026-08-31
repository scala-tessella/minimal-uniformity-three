package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.minimal_uniformity_three.StrictWitnesses.symbolFromKey
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE CHAMBER BOUND OF THE CLASS (default tier, instant). A vertex orbit of valence d whose stabiliser has
  * order s is a (1,2)-orbit of 2d/s chambers, so the Delaney symbol of a tiling of uniformity k has C = Σ
  * 2d_i/s_i chambers. In U(z) every vertex has valence at most |z| — the species vertex has |z| tiles, any
  * other one irregular tile and a regular arc of at most |z| − 1 letters — so C ≤ 2k|z|: 8k for the
  * four-letter species and 6k for the six three-letter ones, against the imported 12k. Consequences: a
  * three-orbit member has at most 24 chambers (18 for a three-letter species), so the three-orbit window
  * above 24 chambers, and its valence-2 slice in particular, is vacuous under the definition; (3².4.12) is
  * exactly 4 with no regime caveat. Confronted here with every banked pattern: the formula is an identity on
  * the symbol, the bound holds for every pattern whose vertices obey the class's valence, and the (3.7.42)
  * uniformity-10 witness attains it (60 = 6·10).
  */
class ChamberBoundSpec extends AnyFlatSpec with Matchers:

  behavior of "the chamber bound C = Σ 2d_i/s_i ≤ 2k|z| of U(z)"

  it should "hold on every banked pattern, with equality for the (3.7.42) uniformity-10 witness" in:
    var tight = List.empty[String]
    for (name, z, key) <- SaturationProbe.entries ++ StrictWitnesses.entries.map(e => (e.name, e.z, e.key)) do
      val ds      = symbolFromKey(key)
      val vertexO = ds.orbs.filter(_.i == 1)
      val k       = vertexO.length
      val perOrb  = vertexO.map(o => (o.elements.length, ds.m(1, 2, o.elements.head)))
      val maxVal  = perOrb.map(_._2).max
      val inClass = maxVal <= z.length
      withClue(s"$name: ")(perOrb.map(_._1).sum shouldBe ds.size)
      // a (1,2)-orbit of valence d has at most 2d chambers (trivial stabiliser), at least d (a mirror)
      for (c, d) <- perOrb do
        withClue(s"$name: orbit of valence $d with $c chambers: ")(c should (be <= 2 * d and be >= d))
      if inClass then
        withClue(s"$name: C=${ds.size} > 2k|z| = ${2 * k * z.length}: ")(ds.size should
          be <= 2 * k * z.length)
      if inClass && ds.size == 2 * k * z.length then tight ::= name
      println(f"$name%-40s z=${z.mkString(".")}%-10s k=$k%2d C=${ds.size}%2d  2k|z|=${2 * k * z.length}%2d  max valence $maxVal" +
        (if inClass then "" else "  (valence above |z|: not a member as defined)"))
    tight should contain("(3.7.42) P1 k=10")
