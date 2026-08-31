# the readings of condition (3) — producing-run record

Output of `StrictRefilterProbe`, which re-derives every designation of the banked hits files and of
every banked pattern under each reading of condition (3), then closure-triages the survivors.

| file | reading | rows |
|---|---|---|
| `strict-candidates.tsv` | contiguous (arc clause alone) | 524 |
| `strict-witnesses.tsv` | contiguous, realized | 48 |
| `isolated-strict-candidates.tsv` | the definition (arc clause + one irregular tile per vertex) | 473 |
| `isolated-strict-witnesses.tsv` | the definition, realized | 221 |

The counts the paper quotes are read off these files. They are what lets a referee check the
triage without replaying the hours-long walks that produced the hits: the candidate rows carry the
symbol keys, so any single verdict can be re-derived from its key alone.

A *record*, separate from `certs/uclass-strict/`, where a fresh `-Dstrict.refilter` run writes its
own outputs.
