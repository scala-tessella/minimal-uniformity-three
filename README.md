# minimal-uniformity-three — verification artifact

[![CI](https://github.com/scala-tessella/minimal-uniformity-three/actions/workflows/ci.yml/badge.svg)](https://github.com/scala-tessella/minimal-uniformity-three/actions/workflows/ci.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21740457.svg)](https://doi.org/10.5281/zenodo.21740457)

Machine-checked companion to the paper

> **Minimal uniformity three: unit-edge tilings around the non-Archimedean vertex types.**

This repository contains **only the paper's proof specs**. All machinery — the Delaney–Dress symbol engine,
the exact angle/moduli layer, cyclotomic arithmetic, the U(z) class construction, and the SAT/DRAT
certification harness — is the pinned
[`research-core`](https://github.com/scala-tessella/research-core) `0.3.1` library. Nothing here is specific
to any other result.

## Reproduce

```bash
sbt test                              # the fast verdicts (exact, in-JVM, no external tools)
sbt -Duclass.k2 -Duclass.k3x test     # + the two exhaustive searches (long: hours at k=2)
tools/install-sat-tools.sh            # build kissat + drat-trim at pinned tags (once, for the lines below)
sbt -Dcert.k2 test                    # + the k <= 2 DRAT completeness certificate (~1 h, resumable)
./verify.sh                           # re-check the 24 proofs with drat-trim alone, independently
```

`sbt test` runs the pinned-verdict specs, which re-verify the extremal witnesses and refutations from their
canonical keys. The heavy, opt-in runs: the two exhaustive searches that *produced* the results —
`UClassK2Probe` (the ≤ 24-chamber k = 2 catalogue) and `UClassK3Probe` (the k = 3 all-species search) —
behind `-Duclass.k2` / `-Duclass.k3x`, and the completeness certification campaign `K2CompletenessProbe`
behind `-Dcert.k2` (the only spec that shells out: kissat to solve, drat-trim to verify the proofs;
everything else is in-JVM). `research-core 0.3.1` resolves from Maven Central.

The certification campaign has been **run in public CI**: green in 24 minutes on an ubuntu runner —
toolchain built from source, all 24 obligations verified, proofs re-checked by `verify.sh` —
[run #5](https://github.com/scala-tessella/minimal-uniformity-three/actions/runs/30699952586). It is
re-runnable by anyone at any time from the [Actions tab](https://github.com/scala-tessella/minimal-uniformity-three/actions/workflows/ci.yml)
("Run workflow" → the `cert-k2` job), so the evidence does not depend on any single machine — including the
author's.

## Claim → check

Run one with `sbt "testOnly *<SpecName>"`.

| Paper result | Spec | Certifies |
|---|---|---|
| The k = 1 scan | `UClassSpec` | The one-vertex-orbit members of each class U(z) — the base case of the minimal-uniformity search. |
| The k ≤ 2 catalogue & scan | `UClassK2Probe` | Enumerates the ≤ 2-orbit catalogue and scans it for class-U(z) survivors; the producing run's record (the 1,363-symbol catalogue size and the single survivor) is archived in `certs/uclass-k2-record/`. *(opt-in: `-Duclass.k2`)* |
| The k ≤ 2 refutation | `UClassK2VerdictSpec` | Refutes the single surviving 2-orbit designation on metric feasibility — no tiling in U(z) has fewer than three vertex orbits. |
| The k = 3 all-species search | `UClassK3Probe` | The exhaustive k = 3 search across all species; the producing runs' hit lists per chamber ramp are archived in `certs/uclass-k3-record/`. *(opt-in: `-Duclass.k3x`)* |
| **The four witnesses** | `UClassK3ExistenceSpec` | Pins the four extremal symbols by canonical key and re-verifies end to end that minimal uniformity of (3.8.24), (3.4.3.12), (3.4².6) and (3².6²) is exactly **3** — including reconstruction of the appendix witness strings. |
| Essential irregularity | `UClassEssentialIrregularitySpec` | Exact areas and the essential-irregularity verdicts: the (3.8.24) and (3.4.3.12) theorems persist under essentially-irregular tiles; (3.4².6) and (3².6²) reopen. |
| **The k ≤ 2 completeness certificate** | `K2CompletenessProbe` | The DRAT certificate behind the lower bound (paper §8): generates the tier-1 certification universe (2,710 D-sets, ≤ 2 vertex orbits, ≤ 24 chambers), and per chamber count C = 1..24 proves base + blocking UNSAT under kissat with drat-trim-verified proofs, with exhaustive two-enumerator agreement and a pure-JVM fidelity check; the exact euclidean tail over the certified universe reproduces the 1,363-symbol catalogue (93 + 1,270). Artifacts land in `certs/k2/`. *(opt-in: `-Dcert.k2`; needs `tools/install-sat-tools.sh`)* |
| The certification universe sizing | `K2UniverseSizingProbe` | The sizing scan behind the certificate's architecture: the raw ≤ 2-orbit universe grows ×5.1 per two chambers (~10⁸ at the top slices — out of blocking reach), the tier-1 relaxation cuts it to 2,710, and the tier-1 lemma (euclidean-feasible ⇒ tier-1) is machine-checked on every raw D-set generated. *(opt-in: `-Dcert.k2.size`)* |

## Scope notes

- **The k ≤ 2 catalogue is DRAT-certified complete** (it was a disclosed trusted-generator gap through
  paper drafts of July 2026): `K2CompletenessProbe` runs the certificate end to end — the tier-1 curvature
  relaxation whose lemma (euclidean-feasible ⇒ tier-1) is proved in `research-core`'s
  `DelaneySymbols.tier1Feasible` scaladoc, per-chamber-count UNSAT obligations with machine-checkable DRAT
  proofs, and the exact euclidean tail reproducing the 1,363-symbol catalogue. The trusted generator
  survives only as one side of the (fully checked) agreement gate. The kissat/drat-trim binaries are built
  at pinned tags by `tools/install-sat-tools.sh`, which records versions and binary hashes.
- **Figure generation is excluded.** The witness figures are drawn by a development-side probe from the
  witnesses' unique metric points; like the companion `31-unit-edge-tilings` repo, this artifact carries the
  *proof*, not presentation. The witnesses' metric data is fully re-verified here by `UClassK3ExistenceSpec`.

## Archival

Deposited on Zenodo as a supplement to the paper record. **Cite the version DOI of the release you checked**,
not the all-versions concept DOI — the latter always resolves to whatever is newest:

| Version | DOI |
|---|---|
| 0.1.1 | [10.5281/zenodo.21740457](https://doi.org/10.5281/zenodo.21740457) |
| 0.1.0 | [10.5281/zenodo.21739355](https://doi.org/10.5281/zenodo.21739355) |

Zenodo assigns a release's version DOI at the moment that release is published, so it cannot be present in
the tree that release archives: the `CITATION.cff` inside a deposit carries no version DOI. The version DOI
is recorded in this table, and in `CITATION.cff` on the main branch, in the first commit after the tag; the
concept DOI (all versions) is [10.5281/zenodo.21739354](https://doi.org/10.5281/zenodo.21739354).

Pinned to `research-core 0.3.1` / `research-core-solver 0.3.1` (each an immutable Central release), archived
as [10.5281/zenodo.21739112](https://doi.org/10.5281/zenodo.21739112). That archived snapshot — not the
`research-core` repository's main branch, which may since have moved on — is the authoritative source for
what this artifact depends on. The pin plus the snapshot make this a closed,
reproducible artifact independent of any moving repository. The DRAT proofs themselves are regenerable by
`-Dcert.k2` (~1 h on a dual-core laptop; also runnable in public from the Actions tab) and are therefore
not stored in git; the producing run's manifest and the 1,363 catalogue keys are, under `certs/k2-record/`.
Every heavy campaign's producing-run record is archived the same way: `certs/uclass-k2-record/` (the k = 2
scan's catalogue size and single survivor) and `certs/uclass-k3-record/` (the k = 3 hit lists per chamber
ramp) — so the claims resting on hours-long opt-in runs are all backed by small, archived, independently
inspectable records.
