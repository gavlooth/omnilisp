#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

stdlib_file="stdlib/stdlib.lisp"
prims_file="src/lisp/eval_init_primitives.c3"
async_policy_file="scripts/check_async_fallback_policy.sh"
io_reference_doc="docs/reference/07-io-networking.md"

for required in "$stdlib_file" "$prims_file" "$async_policy_file" "$io_reference_doc"; do
  if [[ ! -f "$required" ]]; then
    echo "FAIL: missing required file: $required"
    exit 1
  fi
done

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

expected_map="$tmp_dir/expected_io_status_map.txt"
expected_statuses="$tmp_dir/expected_statuses.txt"
actual_statuses="$tmp_dir/actual_statuses.txt"
expected_fallback_statuses="$tmp_dir/expected_fallback_statuses.txt"
actual_fallback_statuses="$tmp_dir/actual_fallback_statuses.txt"

cat > "$expected_map" <<'MAP'
io/read-file|__raw-read-file|io/read-file-fiber-required|read-file|yes|io/*-fiber-required
io/write-file|__raw-write-file|io/write-file-fiber-required|write-file|yes|io/*-fiber-required
io/file-exists?|__raw-file-exists?|io/file-exists?-fiber-required|file-exists?|yes|io/*-fiber-required
io/read-lines|__raw-read-lines|io/read-lines-fiber-required|read-lines|yes|io/*-fiber-required
io/tcp-connect|__raw-tcp-connect|io/tcp-connect-fiber-required|tcp-connect|yes|io/tcp-*-fiber-required
io/tcp-listen|__raw-tcp-listen|io/tcp-listen-fiber-required|tcp-listen|yes|io/tcp-*-fiber-required
io/tcp-accept|__raw-tcp-accept|io/tcp-accept-fiber-required|tcp-accept|yes|io/tcp-*-fiber-required
io/tcp-read|__raw-tcp-read|io/tcp-read-fiber-required|tcp-read|yes|io/tcp-*-fiber-required
io/tcp-write|__raw-tcp-write|io/tcp-write-fiber-required|tcp-write|yes|io/tcp-*-fiber-required
io/tcp-close|__raw-tcp-close|io/tcp-close-fiber-required|tcp-close|yes|io/tcp-*-fiber-required
io/pipe-connect|__raw-pipe-connect|io/pipe-connect-fiber-required|pipe-connect|yes|
io/pipe-listen|__raw-pipe-listen|io/pipe-listen-fiber-required|pipe-listen|yes|
io/process-wait|__raw-process-wait|io/process-wait-fiber-required|process-wait|yes|io/process-wait-fiber-required
io/dns-resolve|__raw-dns-resolve|io/dns-resolve-fiber-required|dns-resolve|no|
io/async-sleep|__raw-async-sleep|io/async-sleep-fiber-required|async-sleep|no|
io/tls-connect|__raw-tls-connect|io/tls-connect-fiber-required|tls-connect|yes|
io/tls-read|__raw-tls-read|io/tls-read-fiber-required|tls-read|yes|
io/tls-write|__raw-tls-write|io/tls-write-fiber-required|tls-write|yes|
io/tls-close|__raw-tls-close|io/tls-close-fiber-required|tls-close|yes|
io/http-get|__raw-http-get|io/http-get-fiber-required|http-get|yes|
io/http-request|__raw-http-request|io/http-request-fiber-required|http-request|yes|
MAP

fail() {
  echo "FAIL: $*"
  exit 1
}

compare_sets() {
  local lhs_name="$1"
  local lhs_file="$2"
  local rhs_name="$3"
  local rhs_file="$4"
  local missing_file="$tmp_dir/missing_${lhs_name}_vs_${rhs_name}.txt"
  local extra_file="$tmp_dir/extra_${lhs_name}_vs_${rhs_name}.txt"

  comm -23 "$lhs_file" "$rhs_file" > "$missing_file"
  comm -13 "$lhs_file" "$rhs_file" > "$extra_file"

  if [[ -s "$missing_file" || -s "$extra_file" ]]; then
    echo "FAIL: io parity status map mismatch: $lhs_name vs $rhs_name"
    if [[ -s "$missing_file" ]]; then
      echo "  Present in $lhs_name, missing in $rhs_name:"
      sed 's/^/    - /' "$missing_file"
    fi
    if [[ -s "$extra_file" ]]; then
      echo "  Present in $rhs_name, missing in $lhs_name:"
      sed 's/^/    - /' "$extra_file"
    fi
    exit 1
  fi
}

cut -d'|' -f3 "$expected_map" | sort -u > "$expected_statuses"
rg -o --no-filename 'io/[A-Za-z0-9?_-]+-fiber-required' src/lisp -g '*.c3' -g '!tests_*.c3' \
  | sort -u > "$actual_statuses"
compare_sets "expected-map-statuses" "$expected_statuses" "source-statuses" "$actual_statuses"

awk -F'|' '$5 == "yes" { print $3 }' "$expected_map" | sort -u > "$expected_fallback_statuses"
sed 's/\\\\?/?/g' "$async_policy_file" \
  | rg -o --no-filename 'io/[A-Za-z0-9?_-]+-fiber-required' \
  | sort -u > "$actual_fallback_statuses"
compare_sets "expected-fallback-policy-statuses" "$expected_fallback_statuses" "async-policy-statuses" "$actual_fallback_statuses"

while IFS='|' read -r effect raw status wrapper fallback_required doc_pattern; do
  [[ -n "$effect" && -n "$raw" && -n "$status" && -n "$wrapper" && -n "$fallback_required" ]] \
    || fail "malformed io parity status map row: $effect|$raw|$status|$wrapper|$fallback_required|$doc_pattern"

  grep -F "(define [effect] (${effect}" "$stdlib_file" >/dev/null \
    || fail "missing stdlib effect declaration for ${effect}"
  rg -F "(signal ${effect}" "$stdlib_file" >/dev/null \
    || fail "missing stdlib signal wrapper for ${effect}"
  rg -F "{ \"${effect}\"" "$prims_file" | rg -F "\"${raw}\"" >/dev/null \
    || fail "missing fast-path map ${effect} -> ${raw}"
  rg -F "{ \"${raw}\"," src/lisp -g '*.c3' >/dev/null \
    || fail "missing raw primitive registration for ${raw}"
  rg -F "$status" src/lisp -g '*.c3' -g '!tests_*.c3' >/dev/null \
    || fail "missing non-test source emission for status ${status}"

  if [[ -n "$doc_pattern" ]]; then
    rg -F "$doc_pattern" "$io_reference_doc" >/dev/null \
      || fail "missing explicit IO reference status documentation pattern '${doc_pattern}' for ${wrapper}"
  fi
done < "$expected_map"

echo "OK: io parity status map guard passed."
echo "  fiber-required statuses: $(wc -l < "$expected_statuses")"
echo "  async fallback statuses: $(wc -l < "$expected_fallback_statuses")"
