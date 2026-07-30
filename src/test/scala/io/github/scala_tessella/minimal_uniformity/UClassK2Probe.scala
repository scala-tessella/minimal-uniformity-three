package io.github.scala_tessella.minimal_uniformity

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** ADR-0009 G4, rung k = 2 — the campaign probe (LONG, hours-scale: the unpruned relaxed D-set tree at 24
  * chambers; guarded by `-Duclass.k2`). Exhaustively enumerates the relaxed symbols with ≤ 2 vertex orbits
  * (complete by the G0 bound), then scans every (symbol, target z, designation) triple through the
  * combinatorial U(z) check and the exact pinned linear layer. ZERO survivors ⇒ no U(z) tiling of uniformity
  * ≤ 2 exists for any target — completing the lower bounds of the claimed-3 conjectures ((3.8.24),
  * (3.4.3.12), (3.4².6)) and advancing all others to ≥ 3. Survivors (if any) are written in full to
  * `certs/uclass-k2/` for affine-layer analysis: a metrically realizable survivor would CORRECT the
  * corresponding conjecture. Progress prints every 15 s via the parallel enumerator's log.
  */
class UClassK2Probe extends AnyFlatSpec with Matchers:

  it should "settle rung k = 2 over all ≤ 24-chamber relaxed symbols (enable with -Duclass.k2)" in:
    assume(sys.props.contains("uclass.k2"), "hours-long run — enable with -Duclass.k2")
    val dir = Path.of("certs", "uclass-k2")
    Files.createDirectories(dir)
    val all = DelaneySymbols.enumerateRelaxedParallel(maxN = 2, maxSize = 24, log = println)
    println(s"RELAXED k <= 2 CATALOGUE: ${all.size} symbols")
    Files.writeString(dir.resolve("catalogue-size.txt"), all.size.toString)
    all.count(_._1.n == 1) shouldBe 93 // the G1 catalogue must reappear inside this one
    val survivors =
      for
        (t, ds) <- all
        (z, _)  <- UClass.targets
        reg     <- UClass.candidates(ds, z)
      yield (t, ds, z, reg)
    val lines     = survivors.map: (t, ds, z, reg) =>
      s"z=${z.mkString(".")}\tchambers=${ds.size}\tsigs=${t.vertices.map(_.mkString("."))}\t" +
        s"regularOrbits=${reg.toList.sorted.mkString(",")}\tkey=${DelaneySymbols.canonicalKey(ds)}"
    Files.writeString(dir.resolve("survivors.tsv"), (s"count=${survivors.size}" +: lines).mkString("\n"))
    println(s"RUNG-2 SURVIVORS AFTER COMBINATORIAL + PINNED-LINEAR: ${survivors.size}")
    lines.foreach(println)
