# the distinct-letter window at eight orbits — producing-run record

Record of the orbit-bounded walk behind the paper's distinct-letter window theorem, at eight vertex
orbits over 1–48 chambers, in the valence-2 regime. The walk takes two species-level prunes
beside the curvature filters: `vertexCap`, a cap on the chambers of every (1,2)-orbit with open
orbits included, and `threeLetter`, the exact 6-cycle / 4-cycle / 2-chain vertex-orbit shapes. Both
are checked against the unpruned walk filtered by the same condition in `VertexCapProbe`, so the
prune cannot be losing members; the files below are the walk with and without the cap
(`-vcap6-shapes3` and `-shapes3`), which is what makes that check meaningful.

Hit lists are given in full where small and as `-summary.tsv` (species × vertex orbits × chamber
count → hits, with the total in the header comment) where the full list runs to megabytes; the
full lists are regenerable by replaying the walk, which is deterministic and shard-resumable. The
`refilter-*.log` files are the triage that disposes of them: every hit re-derived under the
definition and refuted, which is what the window theorem asserts.

A *record*, separate from `certs/uclass-k8`, where a fresh run writes its own outputs.
