# k = 3 searches — producing-run record

Records of the three-orbit searches.

**The all-species search** (`UClassK3Probe`), per `maxSize` ramp: every (symbol, species,
designation) hit passing the combinatorial check, the pinned linear layer and the forced-regularity
filter, over all ten species — `hits-maxSize14.tsv`, `-18`, `-22` (42 hits) and `-24` (79).
`run-maxSize24.log` is the campaign's heartbeat. The hits that survive to the closure level are
re-verified from their canonical keys in `UClassK3ExistenceSpec`.

**The 25–26 chamber band** (`UClassK3ShardProbe`), the sharded walk that carries the three-orbit
catalogue past the reach of the unpruned enumeration: `window-hits-min25-maxSize26.tsv` and its
valence-2 twin `-v2.tsv` (30 hits each). These are the band the paper's k = 3 completion rests on;
every designation in them is re-derived under each reading of condition (3) by
`StrictRefilterProbe`, whose triage output is archived in `certs/uclass-strict-record/`.

These directories are *records*, separate from `certs/uclass-k3/`, where a fresh
`sbt -Duclass.k3x test` or `-Duclass.k3s` run writes its own outputs.
