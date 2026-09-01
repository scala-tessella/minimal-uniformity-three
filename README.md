# minimal-uniformity-three — verification artifact

[![CI](https://github.com/scala-tessella/minimal-uniformity-three/actions/workflows/ci.yml/badge.svg)](https://github.com/scala-tessella/minimal-uniformity-three/actions/workflows/ci.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21740457.svg)](https://doi.org/10.5281/zenodo.21740457)

Machine-checked companion to the paper

> **Minimal uniformity of the non-Archimedean vertex types in unit-edge tilings.**

Of the 21 ways regular polygons can surround a vertex of the plane, ten — the *non-Archimedean* species —
occur in no vertex-transitive tiling by regular polygons. The paper asks, for each, how uniformly the plane
can be tiled around it when irregular tiles are admitted as sparingly as possible, and proves all ten values:

| species | minimal uniformity | | species | minimal uniformity |
|---|---|---|---|---|
| (3.8.24)   | 3 | | (3².4.12) | 4 |
| (3.4.3.12) | 3 | | (3.10.15) | 5 |
| (3.4².6)   | 3 | | (3².6²)   | 5 |
| (3.9.18)   | 4 | | (4.5.20)  | 7 |
| (5².10)    | 4 | | (3.7.42)  | 10 |

This repository contains **only the paper's proof specs**. All machinery — the Delaney–Dress symbol engine
and its orbit-bounded walk, the exact angle/moduli layer, cyclotomic arithmetic, the exact `ℤ[ζ_N]` plane and
de-fusion engine, the U(z) class construction, and the SAT/DRAT certification harness — is the pinned
[`research-core`](https://github.com/scala-tessella/research-core) `0.8.1` library. Nothing here is specific
to any other result.

## Reproduce

```bash
sbt test                              # the fast verdicts (exact, in-JVM, no external tools) — ~20 s
tools/install-sat-tools.sh            # build kissat + drat-trim at pinned tags (once, for the lines below)
sbt -Dcert.k2 test                    # the k <= 2 DRAT completeness certificate (~1 h, resumable)
./verify.sh                           # re-check the 24 proofs with drat-trim alone, independently
```

`sbt test` runs every always-on spec: the claim verdicts, re-derived in exact arithmetic from the appendix's
canonical keys. Everything heavy is opt-in behind a `-D` flag and appears as *canceled*, not failed, when the
flag is absent. `research-core 0.8.1` resolves from Maven Central.

The certification campaign has been **run in public CI**: green in 24 minutes on an ubuntu runner —
toolchain built from source, all 24 obligations verified, proofs re-checked by `verify.sh` —
[run #5](https://github.com/scala-tessella/minimal-uniformity-three/actions/runs/30699952586). It is
re-runnable by anyone at any time from the [Actions tab](https://github.com/scala-tessella/minimal-uniformity-three/actions/workflows/ci.yml)
("Run workflow" → the `cert-k2` job), so the evidence does not depend on any single machine — including the
author's.

## Claim → check

Run one with `sbt "testOnly *<SpecName>"`.

### The verdicts (always on)

| Paper result | Spec | Certifies |
|---|---|---|
| The k = 1 scan | `UClassSpec` | No tiling in U(z) has one vertex orbit, for any of the ten species — through the same machinery the k ≥ 2 scans use. Also pins the three readings of the around-every-vertex condition on hand examples. |
| The k ≤ 2 refutation | `UClassK2VerdictSpec` | Refutes the single surviving two-orbit designation on forced regularity: **no tiling in U(z) has fewer than three vertex orbits**. |
| **The uniformity-3 witnesses** | `UClassK3ExistenceSpec` | The k = 3 existence chain from canonical keys: euclidean symbol, unique surviving designation, metric rigidity, exact closure in ℚ(ζ₂₄). Covers the (3.8.24) and (3.4.3.12) witnesses of the paper, and the two rhombus patterns legal only under the wider spliced reading — the machine side of the values recorded beside the paper's summary table. |
| **The witnesses beyond three** | `UClassK4ExistenceSpec` | The same chain for the seven patterns at uniformity 4, 5, 7 and 10, closure decided by vanishing sums of roots of unity: the fields come out ℚ(ζ₁₈), ℚ(ζ₁₀), ℚ(ζ₁₂), ℚ(ζ₃₀), ℚ(ζ₂₀) and ℚ(ζ₄₂), exactly as the paper reports, with rigidity, simplicity, no straight corner, and convexity where the paper claims it. |
| The chamber bound | `ChamberBoundSpec` | The paper's chamber bound, confronted with every banked pattern. |
| The convex category | `ConvexTileSpec` | Per species, every closed convex corner word over the exact direction lattice and every collar it admits — the ingredients of the convex-category theorem, including that (5².10) has exactly one convex irregular tile. |
| (3².6²) settled at five | `ReflexTile3366Spec` | The six corner types read off the arcs; every labelled corner word obeying the flank rule, closed and simple on the 2π/6 lattice with at most three symmetry classes of corners; the two survivors with their interior lattice points and translation indices; the five orbits of the banked uniformity-5 symbol. |
| Three-orbit corner words | `TwoClassTileSpec` | Per species, the two-class corner words a three-orbit member could carry. |
| The lower-bound lemmas | `UClassLowerBoundSpec` | The hand cross-check of the window bounds — tile-stabiliser and corner-orbit tables, chamber bounds — confronted with every banked witness: the predicted corner-orbit counts are the observed ones. |
| The readings of condition (3) | `StrictArcSpec` | Where the spliced, contiguous and isolated readings part, on every banked pattern. |
| The banked witnesses under the definition | `StrictWitnessSpec` | Per witness: the designation the definition admits, the condition-(4) verdict under the definition and under the spliced reading, convexity, and whether the irregular tile is a fusion. |
| The staircase encoding | `KCertifySpec` | The certification encoding one level up from two orbits: with the staircase layer off it reproduces the certified two-orbit encoding op for op, with it on it matches the staircase-filtered universe, and the layer has teeth — it excludes D-sets that are tier-1-feasible but staircase-infeasible, and only with the flag on. |
| The fusion question | `UClassEssentialIrregularitySpec`, `EndpointEssentialSpec` | Exact areas over the ℚ(ζ₂₄) alphabet, and the forced-square count where the area abstains: which irregular tiles are unions of unit-edge regular polygons and which are not — the paper's open question for (3².4.12), and its answer for (3².6²). |

### The producing runs (opt-in)

| Search | Spec | Flag |
|---|---|---|
| The k ≤ 2 catalogue and scan | `UClassK2Probe` | `-Duclass.k2` |
| The k = 3 all-species search | `UClassK3Probe` | `-Duclass.k3x` |
| The sharded band walk and its U-scan — three orbits and beyond, both valence regimes, the distinct-letter window prunes | `UClassK3ShardProbe` | `-Duclass.k3s`, `.v2`, `.shapes3` |
| The vertex-cap prune, checked against the unpruned walk | `VertexCapProbe` | `-Dvcap.check`, `-Dvcap.size` |
| Closure-level resolution of a hits file's candidates | `UClassK4ExistenceProbe` | `-Duclass.k4x` |
| Every banked designation re-derived under each reading, with closure triage | `StrictRefilterProbe` | `-Dstrict.refilter` |
| Six-orbit designation verification at radius 14 | `K6CandidateProbe` | `-Dk6.verify` |
| The valence-2 gate | `Valence2GateProbe` | `-Dcert.v2.gate` |
| Saturation: the single-split and multi-split audits | `SaturationProbe`, `JointSaturationProbe` | `-Duclass.sat`, `-Djoint.sat` |
| The realization certificates | `WitnessRealizationProbe` | `-Dwitness.realize` |
| The triangular-lattice search for (3².6²) | `Lattice3366Probe` | `-Dlattice.3366` |
| The de-fusion endpoints, and their round-trip re-verification | `DefusionEndpointProbe`, `EndpointVerifyProbe` | `-Ddefusion.endpoints`, `-Ddefusion.verify` |
| **The k ≤ 2 completeness certificate** | `K2CompletenessProbe` | `-Dcert.k2` (needs `tools/install-sat-tools.sh`) |
| The three-orbit staircase obligations, per chamber count | `KCertifyObligationProbe` | `-Dcert.k3.obl` (needs the toolchain) |
| The certification universe sizing | `K2UniverseSizingProbe` | `-Dcert.k2.size` |

## What certifies what

Not every claim is certified the same way, and the difference matters.

- **DRAT-certified.** The k ≤ 2 catalogue is complete by machine-checkable proof, not by trusting a
  generator: `K2CompletenessProbe` runs the certificate end to end — the tier-1 curvature relaxation whose
  lemma (euclidean-feasible ⇒ tier-1) is proved in `research-core`'s `DelaneySymbols.tier1Feasible` scaladoc,
  per-chamber-count UNSAT obligations with drat-trim-verified proofs, and the exact euclidean tail
  reproducing the 1,363-symbol catalogue. The trusted generator survives only as one side of a fully checked
  agreement gate. `verify.sh` re-checks the deposited proofs with drat-trim alone.
- **Agreement-validated.** Beyond two orbits the statements rest on the generator walk: exact, deterministic,
  shard-resumable, replayable from a clean checkout, and cross-checked against an independent enumeration.
  That is weaker than a DRAT certificate, and the paper says so at each theorem. The three-orbit staircase
  obligations are recorded in `certs/kcert-k3-record/` — C = 1–25 unbroken, which covers the whole range the
  paper's chamber bound makes relevant, though it is not the closed, `verify.sh`-checkable campaign the
  two-orbit certificate is, and that record states the difference; the
  encoding they use is validated on every run by `KCertifySpec`, and `KCertifyObligationProbe` runs a
  chamber count's obligation, so a reader can extend the record rather than take it on trust.
- **Exact and self-contained.** The witness verdicts need no external tool and no long run: they re-derive
  from the appendix's canonical keys in seconds, in exact rational and cyclotomic arithmetic.

## Scope notes

- **Figure generation is excluded.** The witness figures are drawn from the witnesses' unique metric points
  by a development-side probe; like the companion `31-unit-edge-tilings` repo, this artifact carries the
  *proof*, not the presentation. The witnesses' metric data is fully re-verified here.
- **Realization is a separate step.** `UClassK3ExistenceSpec` and `UClassK4ExistenceSpec` certify a rigid,
  closed, simple angle point; the step from there to an actual tiling of the plane is the paper's realization
  certificate, discharged pattern by pattern in `WitnessRealizationProbe`.
- **Hit files carry no trailing newline.** Merge them with `awk 1`, not `cat`.

## Archival

Deposited on Zenodo as a supplement to the paper record. **Cite the version DOI of the release you checked**,
not the all-versions concept DOI — the latter always resolves to whatever is newest:

| Version | DOI |
|---|---|
| 0.2.0 | *(minted on release; recorded here in the first commit after the tag)* |
| 0.1.1 | [10.5281/zenodo.21740457](https://doi.org/10.5281/zenodo.21740457) |
| 0.1.0 | [10.5281/zenodo.21739355](https://doi.org/10.5281/zenodo.21739355) |

Zenodo assigns a release's version DOI at the moment that release is published, so it cannot be present in
the tree that release archives: the `CITATION.cff` inside a deposit carries no version DOI. The version DOI
is recorded in this table, and in `CITATION.cff` on the main branch, in the first commit after the tag; the
concept DOI (all versions) is [10.5281/zenodo.21739354](https://doi.org/10.5281/zenodo.21739354).

Pinned to `research-core 0.8.1` / `research-core-solver 0.8.1`, each an immutable Central release. That
release — not the `research-core` repository's main branch, which may since have moved on — is the
authoritative source for what this artifact depends on. The pin makes this a closed, reproducible artifact
independent of any moving repository. The DRAT proofs are regenerable by `-Dcert.k2` (also runnable in
public from the Actions tab) and are therefore not stored in git.

Every heavy campaign's producing run is backed by a small, archived, independently inspectable record under
`certs/`, each with its own README stating what it is and what it is not:

| record | what it holds |
|---|---|
| `k2-record/` | the k ≤ 2 certificate's manifest and the 1,363 catalogue keys |
| `kcert-k3-record/` | the three-orbit staircase obligations: C = 1–25 unbroken, plus four slices in the wider band |
| `uclass-k2-record/` | the k = 2 scan's catalogue size and its single survivor |
| `uclass-k3-record/` | the k = 3 all-species hit lists per chamber ramp, and the 25–26 chamber band |
| `uclass-k4-record/` | the four-orbit window hits, both valence regimes |
| `uclass-k6-record/` … `uclass-k9-record/` | the distinct-letter window walk at six to nine orbits, with the triage logs that dispose of every hit |
| `uclass-strict-record/` | every designation re-derived under each reading of condition (3), with the closure triage |
