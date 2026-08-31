package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*
import io.github.scala_tessella.research_core.solver.*

import io.github.scala_tessella.research_core.solver.Certification.*
import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** THE OBLIGATION RUNNER — the cost of the obligation itself at k classes (`-Dcert.k3.obl[=maxC]`, `.k`,
  * `.from`, band floor `.min`, per-C budget `.secs`), as distinct from the cost of SAT enumeration. Ramps are
  * RESUMABLE per chamber count: every verified slice appends a row to `<tag>obl-manifest.tsv` and is skipped
  * on the next run, so a killed mid-window ramp restarts at the first unfinished C rather than repaying the
  * slices below it.
  *
  * [[KCertifyScalingProbe]] measured SAT4J enumeration and hit a wall at C = 12. But enumeration is the
  * AGREEMENT gate — a cross-check between two enumerators — not the certificate. The certificate is "base CNF
  * + one blocking clause per known universe member is UNSAT", discharged by kissat with a drat-trim-verified
  * proof, and the A2 record is explicit that this half was never the bottleneck (empty slices refuted in
  * 28–81 s while enumeration took ten times longer). So this probe SKIPS enumeration entirely: blocking
  * clauses are built straight from the generator's universe — a labeled D-set's true pair variables are read
  * off its involutions — and only kissat is timed.
  *
  * What the numbers decide: if kissat refutes at chamber counts far above the enumeration wall, the k = 3
  * certification is a schedulable campaign whose agreement gate simply has to stop earlier (or be replaced by
  * a cheaper cross-check). If kissat stalls in the low teens too, the tier-1 relaxation is too weak at three
  * orbits and the route needs a sharper SAT-expressible curvature bound — a research problem, and better to
  * know now than after building the campaign around it.
  */
class KCertifyObligationProbe extends AnyFlatSpec with Matchers:

  private def ops(ds: DSet): List[Int] = (1 to ds.size).flatMap(d => (0 to 2).map(i => ds.get(i, d))).toList

  /** Streamed SHA-256 — the instances are hundreds of MB in the mid-window, so never read whole. */
  private def sha256(p: Path): String =
    val md  = java.security.MessageDigest.getInstance("SHA-256")
    val buf = new Array[Byte](1 << 16)
    val in  = Files.newInputStream(p)
    try
      var n = in.read(buf)
      while n > 0 do
        md.update(buf, 0, n)
        n = in.read(buf)
    finally in.close()
    md.digest.map(b => f"$b%02x").mkString

  it should "time the kissat obligation per chamber count (enable with -Dcert.k3.obl)" in:
    assume(sys.props.contains("cert.k3.obl"), "obligation timing — enable with -Dcert.k3.obl[=maxC]")
    assume(CertifyRunner.toolsInstalled, "SAT tools not installed — run tools/install-sat-tools.sh")
    val maxC      = sys.props.get("cert.k3.obl").filter(_.nonEmpty).fold(16)(_.toInt)
    val k         = sys.props.get("cert.k3.obl.k").filter(_.nonEmpty).fold(3)(_.toInt)
    val from      = sys.props.get("cert.k3.obl.from").filter(_.nonEmpty).fold(8)(_.toInt)
    val budget    = sys.props.get("cert.k3.obl.secs").filter(_.nonEmpty).fold(600)(_.toInt)
    // `-Dcert.k3.obl.stair`: the STAIRCASE obligation — the encoding gains
    // the staircase tile side and the blocking set shrinks to the staircase-feasible slice, which is the
    // point: the obligation then proves completeness of the SMALL universe, the one that stays enumerable
    // in the mid-window. Artifacts carry a `stair-` prefix so the tier-1 ones are not clobbered.
    val stair     = sys.props.contains("cert.k3.obl.stair")
    // `-Dcert.k3.obl.min`: the BAND FLOOR. Without it
    // the universe walk starts from the root — affordable for the tier-1 route in the low teens, hopeless
    // in the mid-window: the floor is what makes C = 25-36 enumerable at all. Staircase mode only (the walk
    // itself requires it); the tier-1 path below stays byte-identical, floor absent.
    val min       = sys.props.get("cert.k3.obl.min").filter(_.nonEmpty).fold(0)(_.toInt)
    require(min == 0 || stair, "-Dcert.k3.obl.min is a staircase-mode device: add -Dcert.k3.obl.stair")
    require(min == 0 || from >= min, s"band floor $min excludes the first slice C=$from")
    // `-Dcert.k3.obl.keepproof`: RETENTION. DRAT proofs grow ~x1.6 per chamber (7 MB at C = 10, 196 MB at
    // C = 16, GBs from C = 24 up) and multi-GB proofs can neither be banked on this disk across a ramp nor
    // travel in an artifact release. What carries reproducibility is the INSTANCE plus the PINNED TOOL
    // BINARIES, so the default is: verify, bank the verdict with all three hashes and the proof size, then
    // DELETE the proof. Pass this flag to keep it (small slices, or a proof headed for external audit).
    val keepProof = sys.props.contains("cert.k3.obl.keepproof")
    val tag       = if stair then "stair-" else ""
    val dir       = Path.of("certs", s"kcert-k$k")
    Files.createDirectories(dir)

    // PER-C RESUMABILITY: one manifest row per slice, appended as it verifies. A slice is re-run only when
    // absent or unverified, so a killed ramp resumes at the first unfinished chamber count instead of
    // repaying the ones below it — at mid-window solve times that difference is days, not minutes.
    val manifest = dir.resolve(s"${tag}obl-manifest.tsv")
    val header   =
      "C\tdsets\tlabelings\tvars\tbaseCls\tblockCls\tkissat_s\tdrat\ttotal_s\t" +
        "proofMB\tinstanceSha256\tkissatSha256\tdratTrimSha256"
    val done     =
      if !Files.exists(manifest) then
        Files.writeString(manifest, header + "\n")
        Set.empty[Int]
      else
        Files
          .readAllLines(manifest)
          .asScala
          .toVector
          .map(_.split('\t'))
          .filter(r => r.length >= 8 && r(7) == "true")
          .map(_(0).toInt)
          .toSet
    if done.nonEmpty then
      println(s"[obl] resuming: verified slices banked = ${done.toVector.sorted.mkString(",")}")
    // pre-retention manifests carry the 9-column header; the new columns append, so old rows stay readable
    // (resume only reads C and the drat verdict) and only the header line needs upgrading in place
    val banked   = Files.readAllLines(manifest).asScala.toVector
    if banked.headOption.exists(_ != header) then
      Files.write(manifest, (header +: banked.tail).asJava)

    // the tool binaries are the other half of the retention record: hash them ONCE per run, not per slice
    val kissatSha = sha256(CertifyRunner.kissat)
    val dratSha   = sha256(CertifyRunner.dratTrim)
    println(s"[obl] tools: kissat ${kissatSha.take(16)} drat-trim ${dratSha.take(16)}; " +
      s"proof retention = ${if keepProof then "KEEP" else "discard after verification"}")

    // the blocking universe over the band, once, sliced by chamber count — LAZY, so a ramp whose every
    // slice is already banked never pays for the walk at all
    lazy val byC: mutable.Map[Int, mutable.ArrayBuffer[DSet]] =
      val acc  = mutable.Map.empty[Int, mutable.ArrayBuffer[DSet]]
      val lock = new Object
      println(
        s"[obl] generating the ${if stair then "staircase" else "tier-1"} universe " +
          s"(maxN=$k, maxSize=$maxC, min=$min)..."
      )
      DelaneySymbols.relaxedOrbitBoundedDSets(
        maxN = k,
        maxSize = maxC,
        sink = ds => lock.synchronized(acc.getOrElseUpdate(ds.size, mutable.ArrayBuffer.empty) += ds),
        log = println,
        tier1 = !stair,
        stair = stair,
        minSize = min
      )
      acc
    println(header)

    var go = true
    var c  = from
    while go && c <= maxC do
      if done(c) then println(s"$c\t- skipped: verified slice banked in ${manifest.getFileName}")
      else
        val dsets     = byC.getOrElse(c, mutable.ArrayBuffer.empty).toVector
        val labelings = dsets.flatMap(DelaneySymbols.bfsRelabelings)
        val t0        = System.nanoTime()
        val baseBody  = dir.resolve(s"${tag}c$c-base.body")
        val base      = DimacsSink(baseBody)
        val enc       = KCertify.encode(c, k, base, staircase = stair)
        base.close()
        // blocking straight from the universe — no enumeration: negate each labeling's true pair variables
        val blockBody = dir.resolve(s"${tag}c$c-blocking.body")
        val block     = DimacsSink(blockBody)
        for ds <- labelings do
          val trues = (for i <- 0 to 2; d <- 1 to c yield enc(i, d, ds.get(i, d))).distinct
          block.clause(trues.map(-_))
        block.close()
        val instance  = dir.resolve(s"${tag}c$c-instance.cnf")
        assemble(instance, base.maxVar, base.clauseCount + block.clauseCount, baseBody, blockBody)
        val proof     = dir.resolve(s"${tag}c$c-proof.drat")
        val tk        = System.nanoTime()
        val (ku, dv)  = CertifyRunner.certifyCnf(instance, proof)
        val kissatS   = (System.nanoTime() - tk) / 1e9
        val totalS    = (System.nanoTime() - t0) / 1e9
        val proofMB   = if Files.exists(proof) then Files.size(proof) / 1048576.0 else 0.0
        val row       =
          f"$c\t${dsets.size}\t${labelings.size}\t${enc.maxVar}\t${base.clauseCount}\t${block.clauseCount}\t" +
            f"$kissatS%.1f\t$dv%b\t$totalS%.1f\t$proofMB%.1f\t${sha256(instance)}\t$kissatSha\t$dratSha"
        if !keepProof then Files.deleteIfExists(proof)
        println(row)
        Files.writeString(manifest, row + "\n", StandardOpenOption.APPEND)
        withClue(s"C=$c obligation: ")(ku shouldBe true) // no tier-1 D-set beyond the enumerated list
        if kissatS > budget then
          println(s"BUDGET EXCEEDED at C=$c — the kissat ceiling for this shape on this machine")
          go = false
      c += 1
