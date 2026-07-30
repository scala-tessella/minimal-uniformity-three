package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** ADR-0009 G4, rung-3 EXISTENCE probe (guarded by `-Duclass.k3x`; chamber budget via `-Duclass.k3x.size`,
  * default 18). All ten targets have lower bound ≥ 3 from rung 2; this probe searches for EXISTENCE at k = 3:
  * 3-vertex-orbit symbols whose U(z) designation survives the combinatorial check, the pinned linear layer
  * AND the forced-regular filter (the rung-2 lesson). It scans ALL TEN targets, not just the claimed-3 ones —
  * a hit for a claimed-4+ target is a conjecture CORRECTION, exactly how (3².6²) fell from its claimed 7 to
  * the proven 3. A SEARCH, not a completeness scan: any budget that surfaces a genuine candidate suffices, so
  * it runs the relaxed enumeration at growing `maxSize` well below the 36-chamber G0 bound (the witnesses are
  * symmetry-rich: actual sizes 11–22). Hits land in `certs/uclass-k3/` for the closure-level analysis pinned
  * in `UClassK3ExistenceSpec`.
  */
class UClassK3Probe extends AnyFlatSpec with Matchers:

  it should
    "search ≤ size-chamber relaxed symbols for k = 3 U(z) existence candidates (enable with -Duclass.k3x)" in:
      assume(sys.props.contains("uclass.k3x"), "long run — enable with -Duclass.k3x")
      val maxSize = sys.props.get("uclass.k3x.size").map(_.toInt).getOrElse(18)
      val dir     = Path.of("certs", "uclass-k3")
      Files.createDirectories(dir)
      val all     = DelaneySymbols.enumerateRelaxedParallel(maxN = 3, maxSize = maxSize, log = println)
      println(s"RELAXED k <= 3 CATALOGUE (maxSize=$maxSize): ${all.size} symbols")
      val hits    =
        for
          (t, ds)  <- all
          (z, cl)  <- UClass.targets // ALL ten targets: a hit for a claimed-4+ z would CORRECT its conjecture
          reg      <- UClass.candidates(ds, z)
          irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !reg(k) => k }
          if UClass.noneForcedRegular(ds, reg, irregular)
        yield (t, ds, z, cl, reg, irregular)
      val lines   = hits.map: (t, ds, z, cl, reg, irr) =>
        s"z=${z.mkString(".")}\tclaimed=$cl\tn=${t.n}\tchambers=${ds.size}\tsigs=${t.vertices.map(_.mkString("."))}\t" +
          s"regularOrbits=${reg.toList.sorted.mkString(",")}\tirregularOrbits=${irr.mkString(",")}\t" +
          s"key=${DelaneySymbols.canonicalKey(ds)}"
      Files.writeString(
        dir.resolve(s"hits-maxSize$maxSize.tsv"),
        (s"count=${hits.size}" +: lines).mkString("\n")
      )
      println(s"RUNG-3 EXISTENCE HITS (combinatorial + pinned-linear + un-forced): ${hits.size}")
      lines.foreach(println)
