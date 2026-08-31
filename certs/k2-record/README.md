# k ≤ 2 completeness certificate — producing-run record

Record of the campaign that produced the k ≤ 2 DRAT completeness certificate described in the paper's
computational-methods section: `manifest.tsv` (per chamber count C = 1..24: universe size,
labelings, SAT models, CNF size, kissat/drat-trim verdicts, wall time) and `catalogue-keys.txt`
(the 1,363 canonical keys the exact euclidean tail reproduces from the certified universe).

This directory is a *record*, deliberately separate from `certs/k2/`, where a fresh
`sbt -Dcert.k2 test` run writes its own artifacts (the probe is resumable via verdict files, so it
must start from an empty directory to re-certify from scratch). The DRAT proofs are regenerable by
that run and are not stored.
