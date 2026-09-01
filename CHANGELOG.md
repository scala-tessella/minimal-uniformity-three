# Changelog

All notable changes to this verification artifact are documented here.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to
[early-semver](https://www.scala-sbt.org/1.x/docs/Publishing.html#Version+scheme). Because the artifact backs
a paper, entries state what a re-check would find different from the previous release — a referee who checked
an earlier version should be able to tell from here whether the claims, the specs, or only the packaging moved.

## [0.2.0] — 2026-09-01

**The artifact catches up with the paper.** Version 0.1.1 covered the ladder as it stood then: four
witnesses, minimal uniformity 3. The paper now proves all ten values — 3, 3, 3, 4, 4, 4, 5, 5, 7, 10 — and
this release carries the instruments behind every one of them. A referee who checked 0.1.1 will find the
verdicts it contained unchanged; what is new is everything above uniformity three, and a good deal of
prose that 0.1.1 got wrong.

### Added

- **`UClassK4ExistenceSpec`** — the always-on existence chain for the seven patterns at uniformity 4, 5, 7
  and 10, re-derived from the appendix's canonical keys. Closure leaves ℚ(ζ₂₄) here, so it is decided by
  vanishing sums of roots of unity, integer-only; the fields come out ℚ(ζ₁₈), ℚ(ζ₁₀), ℚ(ζ₁₂), ℚ(ζ₃₀),
  ℚ(ζ₂₀) and ℚ(ζ₄₂), matching the paper species by species. Per pattern: the claimed chamber count and
  uniformity, a legal designation, metric rigidity at the truth-designation, exact closure, simplicity of
  every face boundary, no straight corner, and convexity exactly where the paper reports it.
- **The lower-bound instruments**: `ChamberBoundSpec`, `ConvexTileSpec`, `ReflexTile3366Spec`,
  `TwoClassTileSpec`, `UClassLowerBoundSpec` — the corner-word enumerations and the hand cross-check of the
  window bounds, each confronted with every banked pattern.
- **The readings of condition (3)**: `StrictArcSpec`, `StrictWitnessSpec` with its witness bank, and the
  opt-in `StrictRefilterProbe`, which re-derives every banked designation under each reading and triages
  the survivors at closure level.
- **The walk and its prunes**, opt-in: `UClassK3ShardProbe` (the sharded band walk, both valence regimes,
  the distinct-letter window prunes), `VertexCapProbe` (the cap checked against the unpruned walk),
  `K6CandidateProbe`, `Valence2GateProbe`, `UClassK4ExistenceProbe`.
- **The staircase certification layer**: `KCertifySpec`, always on — with the layer off the encoding
  reproduces the certified two-orbit one op for op, with it on it matches the staircase-filtered universe,
  and the layer is shown to have teeth — and `KCertifyObligationProbe` (`-Dcert.k3.obl`), which discharges a
  chamber count's obligation end to end. Together they make the three-orbit record extensible by a reader:
  the obligations still outstanding are a matter of solver hours, not of missing code.
- **Saturation**: `SaturationProbe` (the single-split audit of condition (4)) and `JointSaturationProbe`
  (simultaneous splits).
- **Realization and the exact plane**: `WitnessRealizationProbe`, `DefusionEndpointProbe`,
  `EndpointVerifyProbe`, `EndpointEssentialSpec`, and `Lattice3366Probe`, the SAT search on the triangular
  lattice behind the (3².6²) result.
- **Producing-run records** for every new campaign, each with a README stating its scope:
  `certs/uclass-k4-record/`, `certs/uclass-k6-record/` … `certs/uclass-k9-record/` (hit lists in full where
  small, per-species summaries where they run to megabytes, with the triage logs that dispose of every
  hit), `certs/uclass-strict-record/` and `certs/kcert-k3-record/` — the last carrying the three-orbit
  staircase obligations for C = 1–25 unbroken, plus C = 31, 32, 34 and 36, with solve times and the
  instance and tool-binary hashes. `certs/uclass-k3-record/` gains the 25–26 chamber band.

### Changed

- **Pinned to `research-core` 0.8.1** (from 0.3.1), which is where the orbit-bounded walk, the staircase and
  euclidean curvature filters, the valence-2 mode and the exact `ℤ[ζ_N]` plane and de-fusion engine now
  live. None of the promoted specs could be archived before that release existed. 0.8.1 adds the DRAT
  text-proof fallback: drat-trim decides a proof's format by sniffing its opening bytes, and misread a
  sound binary refutation of the C = 4 obligation as text. It fails closed — NOT VERIFIED, never a false
  VERIFIED — so no recorded verdict was in doubt; `certs/kcert-k3-record/README.md` tells the story.
- **`UClassK3ExistenceSpec` is reframed, and this is a correction.** Its documentation claimed four species
  settled at uniformity 3, "correcting (3².6²)'s claimed value 7". That is not what the paper proves:
  (3².6²) is settled at **5**, and its convex rhombus patterns at three orbits are legal only under the
  wider spliced reading of condition (3) — none admits a designation legal under the definition that
  realizes. The same holds for the (3.4².6) rhombus pattern, whose species is witnessed instead by the
  reflex pinwheel. No assertion changed: the spec always computed under the default (spliced) reading and
  every verdict in it still holds. What changed is that it now says so, and names the two rhombus entries
  for what they are — the machine check behind the wider-reading values recorded beside the paper's
  summary table.
- **The refined-class framing is gone from the artifact, as it is from the paper.**
  `UClassEssentialIrregularitySpec` and `EndpointEssentialSpec` are reframed around the notion the paper
  kept: a *fusion*, a tile that is a finite union of unit-edge regular polygons meeting edge-to-edge. The exact area obstruction and the
  forced-square count are unchanged; they now answer the paper's open question for (3².4.12) instead of a
  class that no longer exists.
- **Every private reference is gone.** Fifteen pointers into a private repository — decision-record
  numbers, and the campaign, gate and track names that went with them — were published in 0.1.0 and 0.1.1,
  where a reader could follow none of them. The prose they sat in already carried the explanation, so in
  nearly every case the citation was the one part of the sentence that added nothing.
- README rebuilt around the paper's ten results, with the claim table split into always-on verdicts and
  opt-in producing runs, and a new section on what certifies what: DRAT-certified at two orbits,
  agreement-validated beyond, exact and self-contained for the witness verdicts.
- CI runs the always-on suite on JDK 17 and 21 as before; the opt-in matrix is documented rather than run.

## [0.1.1] — 2026-08-01

Archived as [doi:10.5281/zenodo.21740457](https://doi.org/10.5281/zenodo.21740457) — the version DOI to
cite when pinning this release.

**No claim or spec logic changed** — every verdict a referee checked against 0.1.0 still holds. This
release archives the producing-run records behind the two exhaustive opt-in searches (a referee request:
the claims "exactly one pair survives at k = 2" and "the ≤ 22-chamber k = 3 search leaves convex
candidates for exactly four species" previously rested on hours-long re-runs), and surfaces the public CI
reproduction of the certificate.

### Added

- `certs/uclass-k2-record/` — the k = 2 campaign's record: the 1,363-symbol catalogue size, the single
  surviving designation (`survivors.tsv`, refuted in `UClassK2VerdictSpec`), and the run log.
- `certs/uclass-k3-record/` — the k = 3 all-species search's records: hit lists per chamber ramp
  (≤ 14/18/22/24 — the ≤ 22 list is the paper's exhaustive search, 42 hits), and the run log.
- README: link to the green public CI reproduction of the k ≤ 2 certification campaign (ubuntu runner,
  24 minutes, toolchain built from source, 24/24 obligations verified, proofs re-checked by `verify.sh`),
  with a note that it is re-runnable by anyone from the Actions tab.

## [0.1.0] — 2026-08-01

Initial release: the complete verification surface for the paper, pinned to `research-core 0.3.1` /
`research-core-solver 0.3.1`. Archived as
[doi:10.5281/zenodo.21739355](https://doi.org/10.5281/zenodo.21739355) — the version DOI to cite.

### Added

- **The claim specs**, checked in exact arithmetic under `sbt test` with no external tools: `UClassSpec`
  (the k = 1 scan), `UClassK2VerdictSpec` (the k ≤ 2 refutation), `UClassK3ExistenceSpec` (the four
  extremal witnesses — minimal uniformity of (3.8.24), (3.4.3.12), (3.4².6) and (3².6²) is exactly 3),
  `UClassEssentialIrregularitySpec` (exact areas and the essential-irregularity verdicts).
- **The exhaustive searches** that produced the results, opt-in: `UClassK2Probe` (`-Duclass.k2`, the
  ≤ 24-chamber k ≤ 2 catalogue) and `UClassK3Probe` (`-Duclass.k3x`, the k = 3 all-species search).
- **The k ≤ 2 DRAT completeness certificate** (paper §8), opt-in behind `-Dcert.k2`:
  `K2CompletenessProbe` generates the tier-1 certification universe (2,710 D-sets with at most two vertex
  orbits on ≤ 24 chambers), proves per chamber count C = 1..24 that base + blocking is UNSAT under kissat
  with drat-trim-verified proofs (exhaustive two-enumerator agreement and a pure-JVM fidelity check at
  every C), and reproduces the 1,363-symbol catalogue by the exact euclidean tail. `K2UniverseSizingProbe`
  (`-Dcert.k2.size`) is the sizing scan behind the certificate's architecture and machine-checks the
  tier-1 lemma on every raw D-set it generates.
- `certs/k2-record/` — the producing run's manifest and the 1,363 catalogue keys (the DRAT proofs are
  regenerable by `-Dcert.k2` and are not stored; see the record's README).
- `verify.sh` — independent drat-trim re-verification of the 24 certificate obligations in `certs/k2/`
  (after a `-Dcert.k2` run, or against a downloaded proof pack).
- `tools/install-sat-tools.sh` — builds the external toolchain at pinned versions (kissat `rel-4.0.4`,
  drat-trim commit `2e3b2dc0…`) and records versions and binary hashes.
- Continuous integration (`.github/workflows/ci.yml`): `sbt test` on every push and pull request, on JDK
  17 **and** 21 — the arithmetic is exact, so the verdicts must not depend on the runtime, and the matrix
  asserts that. The opt-in probes appear as *canceled*, not failed. A manual-dispatch job runs the full
  k ≤ 2 certification campaign (tools build + `-Dcert.k2` + `verify.sh`) in public on demand.

[Unreleased]: https://github.com/scala-tessella/minimal-uniformity-three/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/scala-tessella/minimal-uniformity-three/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/scala-tessella/minimal-uniformity-three/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/scala-tessella/minimal-uniformity-three/releases/tag/v0.1.0
