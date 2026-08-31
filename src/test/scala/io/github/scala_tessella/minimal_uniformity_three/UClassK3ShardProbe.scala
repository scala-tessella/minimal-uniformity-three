package io.github.scala_tessella.minimal_uniformity_three

import io.github.scala_tessella.research_core.*

import io.github.scala_tessella.research_core.DelaneySymbols.DSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*

/** THE SHARDED BAND WALK and its U-scan (`-Duclass.k3s`; band via `-Duclass.k3s.min` / `.size`, defaults
  * 31/36; frontier depth `.depth`, default 10; optional shard range `.range=lo-hi` for splitting across
  * machines; optional `.split=<i>:<extraDepth>,...` to sub-shard MONSTER shards into durable
  * `shard-<i>.<j>.tsv` pieces — a fat shard can run for hours, and sub-sharding plus the intra-shard
  * heartbeat make such subtrees interruptible and visible). The durable-progress walk: the euclid walk is
  * split at a canonical-prefix frontier ([[DelaneySymbols.orbitBoundedFrontier]] — the SAME tree,
  * deterministic DFS order, so shard indices are stable across runs and machines given identical
  * min/size/depth) and each shard's result is written atomically on completion — a restart re-runs only
  * unfinished shards, and machines split the range manually. `-Duclass.k3s.v2` walks the VALENCE-2 extended
  * class instead (vertices of degree 2 admitted): a superset universe on a different tree, so its artifacts
  * and its hits file carry a `-v2` suffix and the flag-off pins below do not apply to it. All artifacts under
  * `certs/uclass-k3/shards-min<m>-max<S>-d<D>[-v2]/`: `frontier.tsv` (computed once, reused), `above.tsv`
  * (window members above the frontier), `shard-<i>.tsv` (per-shard window members; existence = done marker).
  * When the FULL range is done, the U-scan layer stack runs over the aggregate and the verdict lands in
  * `certs/uclass-k3/window-hits-min<m>-maxSize<S>.tsv`. Validation pins at min 25 / cap 26: 1,453 window
  * members (65 + 1,388) and 30 hits (16 + 14 at C = 26), from the measured 26/28 full walks.
  */
class UClassK3ShardProbe extends AnyFlatSpec with Matchers:

  private def parseDSet(opLine: String): DSet =
    val triples = opLine.split(';')
    val a       = Array.ofDim[Int](triples.length + 1, 3)
    triples.zipWithIndex.foreach: (t, idx) =>
      t.split(',').zipWithIndex.foreach((v, i) => a(idx + 1)(i) = v.toInt)
    new DSet(a)

  private def opLine(ds: DSet): String = (1 to ds.size).map(d =>
    s"${ds.get(0, d)},${ds.get(1, d)},${ds.get(2, d)}"
  ).mkString(";")

  it should "walk the k = 3 band in resumable canonical-prefix shards (enable with -Duclass.k3s)" in:
    assume(sys.props.contains("uclass.k3s"), "sharded band walk — enable with -Duclass.k3s")
    val minSize = sys.props.get("uclass.k3s.min").filter(_.nonEmpty).fold(31)(_.toInt)
    val maxSize = sys.props.get("uclass.k3s.size").filter(_.nonEmpty).fold(36)(_.toInt)
    val depth   = sys.props.get("uclass.k3s.depth").filter(_.nonEmpty).fold(10)(_.toInt)
    // vertex-orbit bound: 3 for the k = 3 window, 4 for the k = 4 census/existence sweeps
    // (`-Duclass.k3s.maxN=4`). Everything downstream is maxN-generic — the walk, the euclid filter, the
    // v-sweep and the U(z) layers — so only the artifact directory has to keep the runs apart.
    val maxN    = sys.props.get("uclass.k3s.maxN").filter(_.nonEmpty).fold(3)(_.toInt)
    // fork-depth of the in-shard work-stealing walk: below it the walk is sequential, so on the FAT
    // subtrees of this band (shards 9/10: 4–17 G nodes each) depth 12 left ~1.5 of 3 cores idle. 16 keeps
    // pending-task memory bounded (the 2026-08-08 jetsam bug was UNBOUNDED forking) while feeding all cores.
    val forkD   = sys.props.get("uclass.k3s.fork").filter(_.nonEmpty).fold(16)(_.toInt)
    // `-Duclass.k3s.v2`: the VALENCE-2 extended class — vertex orbits may
    // have m12 = 2, so every feasibility table weakens and the walked universe becomes a SUPERSET of the
    // valence->=3 one. The extended tree is a DIFFERENT tree: shard indices do not correspond, so v2 runs get
    // their own artifact directory and their own hits file. Mixing the two would be silently wrong, which is
    // why the separation is in the path and not in a column.
    val v2      = sys.props.contains("uclass.k3s.v2")
    // every (1,2)-orbit at most this many chambers: 6 confines the walk to valence ≤ 3, the regime of the
    // six three-letter species (lem:chambers) — the species-aware window of the (4.5.20)/(3.7.42) rows
    val vcap    = sys.props.get("uclass.k3s.vcap").filter(_.nonEmpty).fold(Int.MaxValue)(_.toInt)
    // the (1,2)-orbit shapes of a three-letter species (6-cycle, 4-cycle, 2-chain): -Duclass.k3s.shapes3
    val shapes3 = sys.props.contains("uclass.k3s.shapes3")
    val vTag    = (if v2 then "-v2" else "") + (if vcap != Int.MaxValue then s"-vcap$vcap" else "") +
      (if shapes3 then "-shapes3" else "")
    val dir     =
      Path.of("certs", s"uclass-k$maxN", s"shards-min$minSize-max$maxSize-d$depth$vTag")
    Files.createDirectories(dir)
    println(
      s"=== UClassK3ShardProbe: band $minSize..$maxSize, maxN=$maxN, depth=$depth, fork=$forkD, " +
        s"valence2=$v2 vertexCap=$vcap shapes3=$shapes3 ===\n=== artifacts: $dir ==="
    )

    // Phase A — the frontier, computed once and reused (deterministic: same params ⇒ same shards).
    val frontierFile = dir.resolve("frontier.tsv")
    val aboveFile    = dir.resolve("above.tsv")
    val frontier     =
      if Files.exists(frontierFile) then
        val lines = Files.readAllLines(frontierFile).asScala.toVector
        println(s"FRONTIER reused: ${lines.size - 1} shards (${frontierFile.getFileName})")
        lines.drop(1).map(_.split('\t')(1))
      else
        val above  = Vector.newBuilder[DSet]
        val f      = DelaneySymbols.orbitBoundedFrontier(
          maxN = maxN,
          maxSize = maxSize,
          depth = depth,
          euclid = true,
          minSize = minSize,
          aboveSink = ds => above.synchronized(above += ds),
          valence2 = v2,
          vertexCap = vcap,
          threeLetter = shapes3
        )
        val aboveV = above.result()
        Files.writeString(
          aboveFile,
          (s"count=${aboveV.size}" +: aboveV.map(ds => s"${ds.size}\t${opLine(ds)}")).mkString("\n")
        )
        Files.writeString(
          frontierFile,
          (s"count=${f.size}" +: f.zipWithIndex.map((s, i) => s"$i\t$s")).mkString("\n")
        )
        println(s"FRONTIER computed: ${f.size} shards at depth $depth; above-frontier: ${aboveV.size}")
        f
    val n            = frontier.size

    // Phase B — walk the requested shard range, skipping finished shards (file existence = done). Shards
    // listed in `-Duclass.k3s.split=<i>:<extraDepth>,...` are SUB-SHARDED: their subtree gets its own
    // deeper mini-frontier and durable `shard-<i>.<j>.tsv` pieces, then aggregates into `shard-<i>.tsv` —
    // monster shards stop being all-or-nothing.
    val splits   = sys.props
      .get("uclass.k3s.split")
      .filter(_.nonEmpty)
      .fold(Map.empty[Int, Int]): s =>
        s.split(',').map { p =>
          val Array(i, d) = p.split(':').map(_.toInt); i -> d
        }.toMap
    val range    = sys.props.get("uclass.k3s.range").filter(_.nonEmpty) match
      case Some(r) => val Array(lo, hi) = r.split('-').map(_.toInt); lo to math.min(hi, n - 1)
      case None    => 0 until n
    var cumDsets = 0L
    var cumNodes = 0L
    val t0       = System.nanoTime()

    def writeRows(target: Path, rows: Vector[DSet]): Unit =
      val tmp = target.resolveSibling(target.getFileName.toString + ".tmp")
      Files.writeString(
        tmp,
        (s"count=${rows.size}" +: rows.map(ds => s"${ds.size}\t${opLine(ds)}")).mkString("\n")
      )
      Files.move(tmp, target, StandardCopyOption.ATOMIC_MOVE)

    // The U-scan of ONE shard's D-sets, written next to it as `hits-<i>.tsv` (header carries the symbol and
    // D-set counts so the aggregate can be reported without re-reading anything). Streaming it per shard is
    // what keeps memory bounded: the k = 5 sweep materialized 784k symbols at once (1.5 GB resident) when
    // the scan ran only after the whole band; per shard it is a few thousand.
    def scanShard(dsets: Vector[DSet], hitsFile: Path): Int =
      // BATCHED (2026-08-30), PARALLEL AND RESUMABLE (2026-08-31): a fat shard's D-sets expand to tens of
      // millions of euclidean symbols, far beyond any heap when held at once, and its scan runs for hours —
      // the k ≤ 9 walk first thrashed a 15 GB heap holding a whole shard's symbols, then lost a four-hour
      // scan to a JVM SIGSEGV inside G1. The scan works in 5,000-D-set batches on a fixed pool; each batch
      // lands durably as hits-N.tsv.b<i> (write + atomic move, the shard convention), so a crashed scan
      // resumes at batch granularity; the final hits file is assembled in batch order — identical to the
      // sequential scan's — and the batch files are then removed.
      val batches       = dsets.grouped(5000).toVector
      def bFile(i: Int) = hitsFile.resolveSibling(hitsFile.getFileName.toString + s".b$i")
      val pool          = java.util.concurrent.Executors.newFixedThreadPool(
        math.max(1, Runtime.getRuntime.availableProcessors - 2)
      )
      try
        val futures =
          for (batch, i) <- batches.zipWithIndex if !Files.exists(bFile(i))
          yield pool.submit(new java.util.concurrent.Callable[Unit] {
            def call(): Unit =
              val all = DelaneySymbols.euclideanSymbolsOf(batch, maxN = maxN, valence2 = v2)
              if minSize > 12 * (maxN - 1) then all.foreach((t, _) => t.n shouldBe maxN) // above G0
              val ls  =
                for
                  (t, ds)  <- all
                  (z, cl)  <- UClass.targets
                  reg      <- UClass.candidates(ds, z)
                  irregular = ds.orbs.zipWithIndex.collect { case (o, k) if o.i == 0 && !reg(k) => k }
                  if UClass.noneForcedRegular(ds, reg, irregular)
                yield s"z=${z.mkString(".")}\tclaimed=$cl\tn=${t.n}\tchambers=${ds.size}\t" +
                  s"sigs=${t.vertices.map(_.mkString("."))}\tregularOrbits=${reg.toList.sorted.mkString(",")}\t" +
                  s"irregularOrbits=${irregular.mkString(",")}\tkey=${DelaneySymbols.canonicalKey(ds)}"
              val tmp = bFile(i).resolveSibling(bFile(i).getFileName.toString + ".tmp")
              Files.writeString(
                tmp,
                (s"count=${ls.size}\tsymbols=${all.size}\tdsets=${batch.size}" +: ls).mkString("\n")
              )
              Files.move(tmp, bFile(i), StandardCopyOption.ATOMIC_MOVE)
          })
        futures.foreach(_.get())
      finally pool.shutdown()
      var symbols       = 0L
      val lines         = Vector.newBuilder[String]
      for i <- batches.indices do
        val ls = Files.readAllLines(bFile(i)).asScala
        symbols += ls.head.split('\t')(1).stripPrefix("symbols=").toLong
        lines ++= ls.drop(1)
      val out           = lines.result()
      val tmp           = hitsFile.resolveSibling(hitsFile.getFileName.toString + ".tmp")
      Files.writeString(
        tmp,
        (s"count=${out.size}\tsymbols=$symbols\tdsets=${dsets.size}" +: out).mkString("\n")
      )
      Files.move(tmp, hitsFile, StandardCopyOption.ATOMIC_MOVE)
      for i <- batches.indices do Files.deleteIfExists(bFile(i))
      out.size

    def readDSets(p: Path): Vector[DSet] =
      Files.readAllLines(p).asScala.drop(1).map(l => parseDSet(l.split('\t')(1))).toVector

    def walkOne(serialized: String, tag: String): Vector[DSet] =
      println(s"$tag start")
      val out              = new java.util.concurrent.ConcurrentLinkedQueue[DSet]
      val (emitted, nodes) = DelaneySymbols.orbitBoundedShardWalk(
        serialized,
        maxN = maxN,
        maxSize = maxSize,
        euclid = true,
        minSize = minSize,
        sink = ds => out.add(ds),
        log = msg => println(s"  [$tag] $msg"),
        forkDepth = forkD,
        valence2 = v2,
        vertexCap = vcap,
        threeLetter = shapes3
      )
      cumDsets += emitted
      cumNodes += nodes
      val secs             = (System.nanoTime() - t0) / 1e9
      println(
        f"$tag done: dsets=$emitted nodes=$nodes  " +
          f"(cum this run: dsets=$cumDsets nodes=$cumNodes, ${secs}%.0fs, ${(cumNodes / math.max(1e-3, secs)).toLong} nodes/s)"
      )
      out.asScala.toVector.sortBy(ds => (ds.size, opLine(ds)))

    for i <- range do
      val shardFile = dir.resolve(s"shard-$i.tsv")
      val hitsFile  = dir.resolve(s"hits-$i.tsv")
      if !Files.exists(shardFile) then
        splits.get(i) match
          case None             => writeRows(shardFile, walkOne(frontier(i), s"shard $i/${n - 1}"))
          case Some(extraDepth) =>
            // deterministic mini-frontier under this shard, reused across resumes
            val subFrontierFile = dir.resolve(s"shard-$i.frontier.tsv")
            val subAboveFile    = dir.resolve(s"shard-$i.above.tsv")
            val subFrontier     =
              if Files.exists(subFrontierFile) then
                Files.readAllLines(subFrontierFile).asScala.toVector.drop(1).map(_.split('\t')(1))
              else
                val above = Vector.newBuilder[DSet]
                val f     = DelaneySymbols.orbitBoundedFrontier(
                  maxN = maxN,
                  maxSize = maxSize,
                  depth = extraDepth,
                  euclid = true,
                  minSize = minSize,
                  aboveSink = ds => above.synchronized(above += ds),
                  from = Some(frontier(i)),
                  valence2 = v2,
                  vertexCap = vcap,
                  threeLetter = shapes3
                )
                writeRows(subAboveFile, above.result().sortBy(ds => (ds.size, opLine(ds))))
                val ftmp  =
                  subFrontierFile.resolveSibling(subFrontierFile.getFileName.toString + ".tmp")
                Files.writeString(
                  ftmp,
                  (s"count=${f.size}" +: f.zipWithIndex.map((s, j) => s"$j\t$s")).mkString("\n")
                )
                Files.move(ftmp, subFrontierFile, StandardCopyOption.ATOMIC_MOVE)
                println(s"shard $i SPLIT: ${f.size} sub-shards at +$extraDepth")
                f
            for j <- subFrontier.indices do
              val subFile = dir.resolve(s"shard-$i.$j.tsv")
              if !Files.exists(subFile) then
                writeRows(subFile, walkOne(subFrontier(j), s"shard $i.$j/${subFrontier.size - 1}"))
            // all sub-shards durable — aggregate into the ordinary shard file
            val rows            = (subFrontier.indices.map(j => dir.resolve(s"shard-$i.$j.tsv")) :+ subAboveFile)
              .filter(Files.exists(_))
              .flatMap(p => Files.readAllLines(p).asScala.drop(1))
              .map(line => parseDSet(line.split('\t')(1)))
              .toVector
              .sortBy(ds => (ds.size, opLine(ds)))
            writeRows(shardFile, rows)
            println(
              s"shard $i/${n - 1} done (aggregated from ${subFrontier.size} sub-shards): dsets=${rows.size}"
            )
      // scan whatever is now on disk — also picks up shards banked by a run predating the streaming scan
      if !Files.exists(hitsFile) then
        val h = scanShard(readDSets(shardFile), hitsFile)
        if h > 0 then println(s"  shard $i hits: $h")

    // Phase C — only when EVERY shard is scanned: concatenate the per-shard hit files. No symbols and no
    // D-sets are held here, so this stage costs nothing however dense the band is.
    val missing = (0 until n).filterNot(i => Files.exists(dir.resolve(s"hits-$i.tsv")))
    if missing.nonEmpty then
      println(
        s"SHARDS INCOMPLETE: ${missing.size} of $n unscanned (first: ${missing.take(10).mkString(",")})"
      )
    else
      val aboveHits = dir.resolve("hits-above.tsv")
      if Files.exists(aboveFile) && !Files.exists(aboveHits) then
        scanShard(readDSets(aboveFile), aboveHits)
      val files     = (0 until n).map(i => dir.resolve(s"hits-$i.tsv")) ++
        Option(aboveHits).filter(Files.exists(_))
      val headers   = files.map(p => Files.readAllLines(p).asScala.head.split('\t').map(_.split('=').last))
      val dsetCount = headers.map(_(2).toInt).sum
      val symCount  = headers.map(_(1).toInt).sum
      val lines     = files.flatMap(p => Files.readAllLines(p).asScala.drop(1)).sorted
      println(s"BAND $minSize..$maxSize COMPLETE: $dsetCount window D-sets across $n shards")
      println(s"BAND EUCLIDEAN SYMBOLS: $symCount")
      if maxN == 3 && minSize == 25 && maxSize == 26 && !v2 then
        dsetCount shouldBe 65 + 1388 // validation pin: the measured 26-point window
        lines.size shouldBe 30 // validation pin: the C = 26 hits of the measured 28-point scan
      Files.writeString(
        Path.of("certs", s"uclass-k$maxN", s"window-hits-min$minSize-maxSize$maxSize$vTag.tsv"),
        (s"count=${lines.size}" +: lines).mkString("\n")
      )
      println(s"RUNG-4a BAND FALSIFIER HITS (combinatorial + pinned-linear + un-forced): ${lines.size}")
      lines.foreach(println)
