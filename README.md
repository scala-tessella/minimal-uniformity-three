# minimal-uniformity — verification artifact

Machine-checked companion to the paper

> **Minimal uniformity three: unit-edge tilings around the non-Archimedean vertex types.**

This repository contains **only the paper's proof specs**. All machinery — the Delaney–Dress symbol engine,
the exact angle/moduli layer, cyclotomic arithmetic, and the U(z) class construction — is the pinned
[`research-core`](https://github.com/scala-tessella/research-core) `0.3.0` library. Nothing here is specific
to any other result.

## Reproduce

```bash
sbt test                              # the fast verdicts (exact, in-JVM, no external tools)
sbt -Duclass.k2 -Duclass.k3x test     # + the two exhaustive searches (long: hours at k=2)
```

`sbt test` runs the pinned-verdict specs, which re-verify the extremal witnesses and refutations from their
canonical keys. The two exhaustive searches that *produced* those results — `UClassK2Probe` (the ≤ 24-chamber
k = 2 catalogue) and `UClassK3Probe` (the k = 3 all-species search) — are heavy and opt-in behind
`-Duclass.k2` / `-Duclass.k3x`. `research-core 0.3.0` resolves from Maven Central.

## Claim → check

Run one with `sbt "testOnly *<SpecName>"`.

| Paper result | Spec | Certifies |
|---|---|---|
| The k = 1 scan | `UClassSpec` | The one-vertex-orbit members of each class U(z) — the base case of the minimal-uniformity search. |
| The k ≤ 2 catalogue & scan | `UClassK2Probe` | Enumerates the ≤ 2-orbit catalogue and scans it for class-U(z) survivors. *(opt-in: `-Duclass.k2`)* |
| The k ≤ 2 refutation | `UClassK2VerdictSpec` | Refutes the single surviving 2-orbit designation on metric feasibility — no tiling in U(z) has fewer than three vertex orbits. |
| The k = 3 all-species search | `UClassK3Probe` | The exhaustive k = 3 search across all species. *(opt-in: `-Duclass.k3x`)* |
| **The four witnesses** | `UClassK3ExistenceSpec` | Pins the four extremal symbols by canonical key and re-verifies end to end that minimal uniformity of (3.8.24), (3.4.3.12), (3.4².6) and (3².6²) is exactly **3** — including reconstruction of the appendix witness strings. |
| Essential irregularity | `UClassEssentialIrregularitySpec` | Exact areas and the essential-irregularity verdicts: the (3.8.24) and (3.4.3.12) theorems persist under essentially-irregular tiles; (3.4².6) and (3².6²) reopen. |

## Scope notes

- **One disclosed completeness gap (as in the paper):** the ≤ 24-chamber k ≤ 2 catalogue behind the lower
  bound is *trusted-generator* — the SAT/DRAT completeness track of the companion classification is not (yet)
  extended to that universe. Everything else is exact and, at k = 1, DRAT-certified upstream in
  `research-core`.
- **Figure generation is excluded.** The paper's `UClassK3FiguresProbe` draws the witness figures; like the
  companion `31-unit-edge-tilings` repo, this artifact carries the *proof*, not presentation. The witnesses'
  metric data is fully re-verified here by `UClassK3ExistenceSpec`.

## Archival

Deposited on Zenodo as a supplement to the paper record, pinned to `research-core 0.3.0` (an immutable Central
release, archived with its own DOI). The pin plus the source snapshot make this a closed, reproducible
artifact independent of any moving repository.
