# the three-orbit staircase obligations — producing-run record

`stair-obl-manifest.tsv` records the staircase-layer certification obligations discharged so far.
Per chamber count: the universe size, the labelings blocked, variable and clause counts, the solve
time, whether drat-trim verified the proof, the proof's size, and the SHA-256 of the instance and
of both tool binaries — the instance plus the pinned binaries being what carries reproducibility,
since the proofs themselves are regenerable and far too large to store (3.8 GB at C = 24).

**Discharged, all `drat = true`:** every chamber count from **C = 1 to C = 25** without a gap, plus
C = 31, 32, 34 and 36 in the wider band.

SCOPE — read this before citing it. Under the paper's chamber bound only C ≤ 24 matters for the
three-orbit statement, and that range is complete here. It is nonetheless **not** the same kind of
object as the k ≤ 2 certificate (`certs/k2-record/`): that one is a closed campaign whose 24
obligations are re-checkable end to end by `verify.sh`, whereas these slices were discharged over
several sessions on one machine, and the wider band above C = 25 is deliberately partial. Where the
paper's statements reach past what is recorded here, they rest on the generator walk — exact,
deterministic and replayable from a clean checkout — and the paper says so at each theorem.

To extend the record, run `sbt -Dcert.k3.obl=<maxC> -Dcert.k3.obl.stair test` with the toolchain
installed (`tools/install-sat-tools.sh`): the runner emits the CNF, solves it with kissat and
verifies the proof with drat-trim, appending one row per slice and skipping those already banked, so
a killed ramp resumes rather than repaying. The encoding is checked on every ordinary `sbt test` by
`KCertifySpec`, which reproduces the certified two-orbit encoding op for op with the staircase layer
off and the staircase-filtered universe with it on.

## Provenance, and one verdict that changed

C = 1–7 were produced by this artifact's own pipeline against `research-core 0.8.1`. C = 8 and above
come from the long producing runs on the development machine, whose solve times these rows report
(C = 24 alone took 9,524 s and a 3.8 GB proof).

C = 4 is worth a word. Under `research-core 0.8.0` it recorded `drat = false` on a proof that was
perfectly sound. kissat writes binary DRAT, drat-trim decides the format by sniffing the opening
bytes, and every proof begins with `0x64` — the deletion marker, which is also the ASCII `d` that
opens a text deletion line. The format therefore turns on the byte after it, the first literal's
variable-length encoding: the larger instances encode literals above `0x80` and sniff as binary,
while C = 4, with its thousand variables, encoded one into `0x2d` — a printable `-` — and the whole
binary file was parsed as text, reaching the end without a conflict. The misdetection fails closed,
reporting NOT VERIFIED and never a false VERIFIED, so nothing recorded here was ever in doubt.
`research-core 0.8.1` retries with a text proof when a binary one fails to verify, and the C = 4 row
banked above is that retry's verdict.
