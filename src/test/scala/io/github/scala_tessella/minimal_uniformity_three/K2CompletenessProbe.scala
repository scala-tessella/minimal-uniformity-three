package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.DelaneySymbols
import io.github.scala_tessella.research_core.solver.Certification.*
import io.github.scala_tessella.research_core.solver.{CertifyRunner, K2Certify}
import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.BufferedWriter
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** ADR-0009 paper certification, track A2 — the k ≤ 2 completeness obligation, end to end (the k = 1 track A
  * pattern of [[K1CompletenessProbe]], one level up). Certification universe: ALL D-sets on ≤ 24 chambers
  * with ≤ 2 vertex orbits satisfying the TIER-1 curvature relaxation
  * ([[DelaneySymbols.relaxedOrbitBoundedDSets]] with `tier1 = true`); the tier-1 lemma (euclidean-feasible ⇒
  * tier-1, proved at [[DelaneySymbols.tier1Feasible]]) bridges it to the class of interest — the raw unpruned
  * ≤ 2-orbit universe (~10⁸ D-sets at the top slices) is out of blocking reach, the tier-1 one is enumerable,
  * and no euclidean symbol lives outside it. Four gates, per chamber count C = 1..24:
  *
  *   - AGREEMENT: the SAT enumeration of [[K2Certify]] models (labeled BFS-numbered D-sets) equals,
  *     op-for-op, the BFS relabelings of the generator's tier-1 universe slice — two independent enumerators,
  *     one universe, EXHAUSTIVE AT EVERY C (the tier-1 cut is what makes this affordable);
  *   - FIDELITY: every SAT4J model satisfies the emitted base CNF (the pure-JVM check of ADR-0008);
  *   - OBLIGATION: base + blocking is UNSAT under kissat with a drat-trim-VERIFIED proof — no tier-1 D-set
  *     beyond the list exists. With the tier-1 lemma, THE certificate of the paper's Theorem 3.1 lower-bound
  *     layer: the trusted-generator k ≤ 2 catalogue leaves the trust base;
  *   - TAIL: the exact JVM tail ([[DelaneySymbols.euclideanSymbolsOf]], maxN = 2) over the certified universe
  *     reproduces the 1363-symbol catalogue (93 one-orbit + 1270 two-orbit).
  *
  * Resumable: the universe is generated once (per-C files + `universe.DONE`), each C writes its verdict file
  * on full success and is skipped on re-runs. Artifacts + manifest under `certs/k2/`. Heartbeat progress
  * throughout (long stages print every 15 s).
  */
class K2CompletenessProbe extends AnyFlatSpec with Matchers:

  private val MaxC = 24

  private def ops(ds: DSet): List[Int] = (1 to ds.size).flatMap(d => (0 to 2).map(i => ds.get(i, d))).toList

  private def parseDSet(line: String): DSet =
    val v = line.split(",").map(_.toInt)
    val c = v.length / 3
    val a = Array.ofDim[Int](c + 1, 3)
    for d <- 1 to c; i <- 0 to 2 do a(d)(i) = v(3 * (d - 1) + i)
    new DSet(a)

  it should
    "certify k <= 2 tier-1 D-set completeness (agreement, fidelity, DRAT obligation, tail = the 1363)" in:
      assume(sys.props.contains("cert.k2"), "hours-scale campaign — enable with -Dcert.k2")
      assume(CertifyRunner.toolsInstalled, "SAT tools not installed — run tools/install-sat-tools.sh")
      val dir               = Path.of("certs", "k2")
      Files.createDirectories(dir)
      def dsetsFile(c: Int) = dir.resolve(s"c$c-dsets.tsv")

      // ---- stage U: the tier-1 certification universe, streamed per chamber count ----------------------
      val universeDone  = dir.resolve("universe.DONE")
      if !Files.exists(universeDone) then
        println(s"[k2] generating the tier-1 universe (maxN=2, maxSize=$MaxC)...")
        val writers = mutable.Map.empty[Int, BufferedWriter]
        val lock    = new Object
        val count   = DelaneySymbols.relaxedOrbitBoundedDSets(
          maxN = 2,
          maxSize = MaxC,
          sink = ds =>
            val line = ops(ds).mkString(",")
            lock.synchronized {
              writers.getOrElseUpdate(ds.size, Files.newBufferedWriter(dsetsFile(ds.size))).write(line + "\n")
            }
          ,
          log = println,
          tier1 = true
        )
        lock.synchronized(writers.values.foreach(_.close()))
        for c <- 1 to MaxC if !Files.exists(dsetsFile(c)) do Files.createFile(dsetsFile(c)) // empty slices
        Files.writeString(universeDone, count.toString)
        println(s"[k2] universe complete: $count D-sets")
      val universeCount = Files.readString(universeDone).trim.toLong

      // ---- per-C obligations, resumable --------------------------------------------------------------
      val manifest = dir.resolve("manifest.tsv")
      if !Files.exists(manifest) then
        Files.writeString(
          manifest,
          "C\tdsets\tlabelings\tmodels\tvars\tbaseClauses\tblocking\tkissatUnsat\tdratVerified\tmillis\n"
        )
      for c <- 1 to MaxC do
        val verdictFile = dir.resolve(s"c$c.verdict")
        if Files.exists(verdictFile) && Files.readString(verdictFile).contains("true\ttrue") then
          println(s"[k2] C=$c already certified — skipping")
        else
          val t0        = System.nanoTime()
          val dsets     = Files.readAllLines(dsetsFile(c)).asScala.toVector.filter(_.nonEmpty).map(parseDSet)
          val labelings = dsets.flatMap(DelaneySymbols.bfsRelabelings).map(ops).toSet
          println(s"[k2] C=$c: ${dsets.size} D-sets, ${labelings.size} labelings — encoding...")
          val baseBody  = dir.resolve(s"c$c-base.body")
          val blockBody = dir.resolve(s"c$c-blocking.body")
          val base      = DimacsSink(baseBody)
          K2Certify.encode(c, base)
          base.close()
          val baseCnf   = dir.resolve(s"c$c-base.cnf")
          assemble(baseCnf, base.maxVar, base.clauseCount, baseBody)
          val parsed    = parseCnf(baseCnf)
          val block     = DimacsSink(blockBody)
          var checked   = 0
          var bad       = 0
          var lastBeat  = System.nanoTime()
          val models    = K2Certify.enumerate(
            c,
            blockingSink = block,
            onModel = m =>
              if violatedClauses(parsed, m).nonEmpty then bad += 1
              checked += 1
              if (System.nanoTime() - lastBeat) / 1e9 > 15 then
                lastBeat = System.nanoTime()
                println(s"  [k2] C=$c: $checked models enumerated...")
          )
          block.close()
          withClue(s"C=$c: "):
            // AGREEMENT: two independent enumerators, one universe
            models.map(ops).toSet shouldBe labelings
            models.size shouldBe labelings.size
            // FIDELITY: every SAT4J model satisfies the emitted base CNF
            bad shouldBe 0
            // OBLIGATION: base + blocking UNSAT, proof VERIFIED
            val instance       = dir.resolve(s"c$c-instance.cnf")
            assemble(instance, base.maxVar, base.clauseCount + block.clauseCount, baseBody, blockBody)
            println(s"  [k2] C=$c: obligation ${base.clauseCount + block.clauseCount} clauses — kissat...")
            val (kUnsat, dVer) = CertifyRunner.certifyCnf(instance, dir.resolve(s"c$c-proof.drat"))
            kUnsat shouldBe true
            dVer shouldBe true
            val millis         = (System.nanoTime() - t0) / 1000000
            Files.writeString(
              manifest,
              s"$c\t${dsets.size}\t${labelings.size}\t${models.size}\t${base.maxVar}\t${base.clauseCount}\t" +
                s"${block.clauseCount}\ttrue\ttrue\t$millis\n",
              StandardOpenOption.APPEND
            )
            Files.writeString(verdictFile, s"true\ttrue\t${dsets.size}\t${labelings.size}\t$millis")
            println(s"[k2] C=$c CERTIFIED (${millis / 1000} s)")

      // ---- TAIL: the exact JVM tail over the certified universe reproduces the 1363 --------------------
      println("[k2] tail: exact euclidean filtering over the certified universe...")
      val universe  = (1 to MaxC).flatMap(c =>
        Files.readAllLines(dsetsFile(c)).asScala.filter(_.nonEmpty).map(parseDSet)
      ).toVector
      universe.size.toLong shouldBe universeCount
      val certified = DelaneySymbols.euclideanSymbolsOf(universe, maxN = 2)
      val keys      = certified.map((_, ds) => DelaneySymbols.canonicalKey(ds)).toSet
      val k1Keys    = certified.filter(_._1.n == 1).map((_, ds) => DelaneySymbols.canonicalKey(ds)).toSet
      keys should have size 1363
      k1Keys should have size 93
      (keys.size - k1Keys.size) shouldBe 1270
      Files.write(dir.resolve("catalogue-keys.txt"), keys.toSeq.sorted.asJava)
      println(s"[k2] COMPLETE: $universeCount tier-1 D-sets certified, tail = 1363 symbols (93 + 1270)")
