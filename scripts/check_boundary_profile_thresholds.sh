#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

log_file="${1:-build/boundary_profile_memory_lifetime_bench.log}"

# Envelope for the exact profiled memory-lifetime-bench workload recorded in
# docs/plans/boundary-profiling-baseline-2026-03-11.md.
: "${OMNI_BOUNDARY_PROFILE_EXPECT_SPLICE_OK:=2048}"
: "${OMNI_BOUNDARY_PROFILE_EXPECT_DISALLOWED_OK:=2048}"
: "${OMNI_BOUNDARY_PROFILE_EXPECT_REUSE_OK:=2048}"
: "${OMNI_BOUNDARY_PROFILE_EXPECT_PARTIAL_OK:=2048}"
# The current memory-lifetime-bench profile includes product, iterator, tensor,
# and nested-module boundary workloads. The nested-module workload intentionally
# exercises stable destination materialization, so fallback count is capped as a
# regression envelope instead of pinned to the original synthetic-only zero.
: "${OMNI_BOUNDARY_PROFILE_MAX_COPY_FALLBACK_TOTAL:=2623}"
: "${OMNI_BOUNDARY_PROFILE_EXPECT_SPLICE_FAIL_TOTAL:=0}"
: "${OMNI_BOUNDARY_PROFILE_MAX_SPLICE_MS:=80}"
: "${OMNI_BOUNDARY_PROFILE_MAX_DISALLOWED_MS:=80}"
: "${OMNI_BOUNDARY_PROFILE_MAX_REUSE_MS:=40}"
: "${OMNI_BOUNDARY_PROFILE_MAX_PARTIAL_MS:=40}"
: "${OMNI_BOUNDARY_PROFILE_MAX_SCOPE_SPLICE_MS:=40}"
: "${OMNI_BOUNDARY_PROFILE_EXPECT_SCOPE_CHAIN_SCAN_SUPPRESSED:=0}"
: "${OMNI_BOUNDARY_PROFILE_MAX_HINT_MISS_RATIO:=0.75}"

extract_line() {
  local pattern="$1"
  grep -aE "$pattern" "$log_file" | tail -n 1 || true
}

extract_field() {
  local line="$1"
  local key="$2"
  echo "$line" | tr ' ' '\n' | awk -F= -v k="$key" '$1 == k { print $2; exit }'
}

require_line() {
  local label="$1"
  local line="$2"
  if [[ -z "$line" ]]; then
    echo "FAIL: missing ${label} line in ${log_file}" >&2
    exit 1
  fi
}

assert_eq() {
  local label="$1"
  local actual="$2"
  local expected="$3"
  if [[ -z "$actual" ]]; then
    echo "FAIL: missing ${label}" >&2
    exit 1
  fi
  if [[ "$actual" != "$expected" ]]; then
    echo "FAIL: ${label} expected ${expected}, got ${actual}" >&2
    exit 1
  fi
}

assert_le() {
  local label="$1"
  local actual="$2"
  local limit="$3"
  if [[ -z "$actual" ]]; then
    echo "FAIL: missing ${label}" >&2
    exit 1
  fi
  if (( actual > limit )); then
    echo "FAIL: ${label} expected <= ${limit}, got ${actual}" >&2
    exit 1
  fi
}

assert_ratio_le() {
  local label="$1"
  local numerator="$2"
  local denominator="$3"
  local limit="$4"
  if [[ -z "$numerator" || -z "$denominator" || "$denominator" == "0" ]]; then
    echo "FAIL: missing ratio inputs for ${label}" >&2
    exit 1
  fi
  if ! awk -v n="$numerator" -v d="$denominator" -v limit="$limit" 'BEGIN { exit !((n / d) <= limit) }'; then
    echo "FAIL: ${label} expected <= ${limit}, got $(awk -v n="$numerator" -v d="$denominator" 'BEGIN { printf "%.6f", n / d }')" >&2
    exit 1
  fi
}

if [[ ! -f "$log_file" ]]; then
  echo "missing profile log: $log_file" >&2
  exit 1
fi

decision_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=boundary_decision_cost( |$)')"
partial_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=boundary_destination_routed_escape( |$)')"
tail_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=scope_splice_tail( |$)')"
boundary_line="$(extract_line '^OMNI_TEST_SUMMARY suite=boundary_decisions( |$)')"
telemetry_line="$(extract_line '^\[boundary\]\[telemetry\]\[tests\] splice_attempted=')"

require_line "boundary_decision_cost benchmark" "$decision_line"
require_line "boundary_destination_routed_escape benchmark" "$partial_line"
require_line "scope_splice_tail benchmark" "$tail_line"
require_line "boundary_decisions summary" "$boundary_line"
require_line "boundary telemetry" "$telemetry_line"

assert_eq "splice_ok" "$(extract_field "$decision_line" "splice_ok")" "$OMNI_BOUNDARY_PROFILE_EXPECT_SPLICE_OK"
assert_eq "disallowed_ok" "$(extract_field "$decision_line" "disallowed_ok")" "$OMNI_BOUNDARY_PROFILE_EXPECT_DISALLOWED_OK"
assert_eq "reuse_ok" "$(extract_field "$decision_line" "reuse_ok")" "$OMNI_BOUNDARY_PROFILE_EXPECT_REUSE_OK"
assert_eq "partial_ok" "$(extract_field "$partial_line" "partial_ok")" "$OMNI_BOUNDARY_PROFILE_EXPECT_PARTIAL_OK"
assert_le "copy_fallback_total" "$(extract_field "$boundary_line" "copy_fallback_total")" "$OMNI_BOUNDARY_PROFILE_MAX_COPY_FALLBACK_TOTAL"
assert_eq "splice_fail_total" "$(extract_field "$boundary_line" "splice_fail_total")" "$OMNI_BOUNDARY_PROFILE_EXPECT_SPLICE_FAIL_TOTAL"

assert_le "boundary_decision_cost.splice_ms" "$(extract_field "$decision_line" "splice_ms")" "$OMNI_BOUNDARY_PROFILE_MAX_SPLICE_MS"
assert_le "boundary_decision_cost.disallowed_ms" "$(extract_field "$decision_line" "disallowed_ms")" "$OMNI_BOUNDARY_PROFILE_MAX_DISALLOWED_MS"
assert_le "boundary_decision_cost.reuse_ms" "$(extract_field "$decision_line" "reuse_ms")" "$OMNI_BOUNDARY_PROFILE_MAX_REUSE_MS"
assert_le "boundary_destination_routed_escape.partial_ms" "$(extract_field "$partial_line" "partial_ms")" "$OMNI_BOUNDARY_PROFILE_MAX_PARTIAL_MS"
assert_le "scope_splice_tail.splice_ms" "$(extract_field "$tail_line" "splice_ms")" "$OMNI_BOUNDARY_PROFILE_MAX_SCOPE_SPLICE_MS"

assert_eq "scope_chain_scan_suppressed" "$(extract_field "$telemetry_line" "scope_chain_scan_suppressed")" "$OMNI_BOUNDARY_PROFILE_EXPECT_SCOPE_CHAIN_SCAN_SUPPRESSED"
assert_ratio_le \
  "hint_miss_ratio" \
  "$(extract_field "$telemetry_line" "scope_chain_scan_fallback")" \
  "$(extract_field "$telemetry_line" "scope_chain_scan_with_hint")" \
  "$OMNI_BOUNDARY_PROFILE_MAX_HINT_MISS_RATIO"

echo "PASS: boundary profile thresholds satisfied for ${log_file}"
