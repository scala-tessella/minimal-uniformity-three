package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** THE k ≤ 2 CATALOGUE AND SCAN — the producing run behind the paper's lower bound (LONG, hours-scale: the
  * unpruned relaxed D-set tree at 24 chambers; guarded by `-Duclass.k2`). Exhaustively enumerates the relaxed
  * symbols with ≤ 2 vertex orbits — complete to 24 chambers, which is where the paper's chamber bound puts
  * the ceiling for two orbits, and certified complete by [[K2CompletenessProbe]] — then scans every (symbol,
  * species z, designation) triple through the combinatorial U(z) check and the exact pinned linear layer.
  * Exactly one survivor across all ten species, refuted in [[UClassK2VerdictSpec]]: no tiling in U(z) has
  * uniformity ≤ 2. Survivors are written in full to `certs/uclass-k2/` for affine-layer analysis; the
  * producing run's record is archived under `certs/uclass-k2-record/`. Progress prints every 15 s via the
  * parallel enumerator's log.
  */
class UClassK2Probe extends AnyFlatSpec with Matchers:

  it should "scan every ≤ 24-chamber relaxed symbol with at most two orbits (enable with -Duclass.k2)" in:
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
