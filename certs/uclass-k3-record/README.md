# k = 3 all-species search — producing-run record

Record of the rung-3 existence searches (2026-07-12/15): per `maxSize` ramp (14, 18, 22, 24
chambers), every (symbol, species, designation) hit passing the combinatorial check, the pinned
linear layer, and the forced-regularity filter, over ALL ten species — `hits-maxSize22.tsv` (42
hits) is the paper's exhaustive ≤ 22-chamber search from which the closure/genuineness levels
leave convex witnesses for exactly four species (re-verified by canonical key in
`UClassK3ExistenceSpec`); `hits-maxSize24.tsv` (79) is the extended ≤ 24-chamber sweep.
`run-maxSize24.log` is the campaign's heartbeat.

This directory is a *record*, separate from `certs/uclass-k3/`, where a fresh
`sbt -Duclass.k3x test` run writes its own outputs.
