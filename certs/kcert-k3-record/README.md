# the three-orbit staircase obligations — producing-run record

`pinB-obl-manifest.tsv` records the staircase-layer certification obligations produced so far: per
chamber count, the universe size, labelings, variable and clause counts, the kissat time and
whether drat-trim verified the proof.

SCOPE — read this before citing it. The k ≤ 2 completeness certificate (`certs/k2-record/`) is
complete: 24 obligations, all VERIFIED, re-checkable with `verify.sh`. The three-orbit staircase
obligations recorded here are NOT a complete certificate; the manifest names exactly the chamber
counts that have been discharged. To extend it, run `sbt -Dcert.k3.obl=<maxC> test` with the toolchain
installed: the obligation runner emits the CNF, solves it with kissat and verifies the proof with drat-trim,
exactly as the two-orbit campaign does. The encoding itself is checked on every ordinary `sbt test` by
`KCertifySpec`, which reproduces the certified two-orbit encoding op for op with the staircase layer off and
the staircase-filtered universe with it on. For the rest of the three-orbit range the paper's statements rest
on the generator walk — exact, deterministic and replayable from a clean checkout, cross-checked
against an independent enumeration — and the paper says so at each theorem. The proofs themselves
are large (hundreds of megabytes per slice) and regenerable, so they are not stored here.
