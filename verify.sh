#!/usr/bin/env bash
# Independently verify the k <= 2 completeness certificate's UNSAT proofs with drat-trim.
# Completeness is certified iff all 24 obligations (certs/k2/, one per chamber count 1..24) report
# `s VERIFIED`. The proofs are regenerable — run `sbt -Dcert.k2 test` first (or unpack a downloaded
# proof pack into certs/k2/); this script then re-checks them with drat-trim ALONE, independently of
# the JVM pipeline that produced them.
# drat-trim's exit code is unreliable (it returns 1 even on a trivial-UNSAT it VERIFIES), so the
# verdict is the `s VERIFIED` line — and we must NOT use `pipefail`, or that exit code would sink the
# grep match. Needs only drat-trim (tools/install-sat-tools.sh).
set -u
DRAT="${DRAT_TRIM:-tools/bin/drat-trim}"
[ -x "$DRAT" ] || { echo "drat-trim not found at $DRAT — run tools/install-sat-tools.sh"; exit 1; }

pass=0; fail=0
for c in $(seq 1 24); do
  cnf="certs/k2/c$c-instance.cnf"; drat="certs/k2/c$c-proof.drat"
  [ -f "$drat" ] || [ ! -f "$drat.xz" ] || xz -dk "$drat.xz"
  if [ -f "$cnf" ] && [ -f "$drat" ] && "$DRAT" "$cnf" "$drat" 2>/dev/null | grep -q 's VERIFIED'; then
    pass=$((pass + 1))
  else echo "NOT VERIFIED: $cnf"; fail=$((fail + 1)); fi
done

echo "VERIFIED $pass / $((pass + fail))"
[ "$fail" -eq 0 ] && [ "$pass" -eq 24 ] || { echo "expected 24 verified obligations — run sbt -Dcert.k2 test first"; exit 1; }
echo "k <= 2 completeness certified: 24/24 obligations VERIFIED"
