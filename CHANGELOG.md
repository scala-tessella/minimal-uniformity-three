# Changelog

All notable changes to this verification artifact are documented here.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to
[early-semver](https://www.scala-sbt.org/1.x/docs/Publishing.html#Version+scheme). Because the artifact backs
a paper, entries state what a re-check would find different from the previous release — a referee who checked
an earlier version should be able to tell from here whether the claims, the specs, or only the packaging moved.

## [0.1.0] — 2026-08-01

Initial release: the complete verification surface for the paper, pinned to `research-core 0.3.1` /
`research-core-solver 0.3.1`. The version DOI is recorded here, in `CITATION.cff`, and in the README's
Archival table in the first commit after the release tag (Zenodo mints it at publication, so it cannot be
present in the tree the release archives).

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

[0.1.0]: https://github.com/scala-tessella/minimal-uniformity-three/releases/tag/v0.1.0
