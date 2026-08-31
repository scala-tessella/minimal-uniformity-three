# the four-orbit catalogue — producing-run record

The four-orbit window walk (`UClassK3ShardProbe` at `maxN = 4`), run twice per band: once in the
valence-≥ 3 world and once with `-Duclass.k3s.v2`, the valence-2 extended class, which is a strict
superset verified to contain the other.

| file | band | hits |
|---|---|---|
| `window-hits-min1-maxSize24.tsv` | 1–24 chambers | 562 |
| `window-hits-min1-maxSize24-v2.tsv` | 1–24, valence-2 | 874 |
| `window-hits-min25-maxSize29.tsv` | 25–29 chambers | 335 |
| `window-hits-min25-maxSize29-v2.tsv` | 25–29, valence-2 | 469 |

Each file states its own row count in a `count=` header line; the files carry no trailing newline, so
merge them with `awk 1` rather than `cat`. Each line is one candidate designation: species, claimed value, vertex-orbit count, chamber count,
vertex signatures, the regular/irregular orbit split and the symbol's key. A hit is a candidate,
not a member — every one of them is re-derived under each reading of condition (3) and triaged to a
verdict by `StrictRefilterProbe` (`certs/uclass-strict-record/`), and the ones that reach the
closure level with a realization are the paper's uniformity-4 witnesses, re-verified from their
keys in `UClassK4ExistenceSpec`.

A *record*, separate from `certs/uclass-k4/`, where a fresh run writes its own outputs.
