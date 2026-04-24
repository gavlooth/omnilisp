#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

log_file="${1:-.agents/memory-boundary-telemetry-baseline-2026-04-24.log}"

# Envelope for the memory-boundary telemetry baseline recorded in
# docs/plans/memory-boundary-telemetry-benchmark-baseline-2026-04-24.md.
# Correctness and counter presence are hard gates. Timing and copy-debt drift
# are warnings until repeated bounded-container baselines prove stability.
: "${OMNI_MEM_TELEM_EXPECT_DECISION_ITERS:=2048}"
: "${OMNI_MEM_TELEM_EXPECT_SHAPE_ITERS:=128}"
: "${OMNI_MEM_TELEM_EXPECT_SHAPE_CLOSURE_OK:=32}"
: "${OMNI_MEM_TELEM_EXPECT_STABLE_PASSPORT_OK:=1}"

: "${OMNI_MEM_TELEM_WARN_MAX_SPLICE_MS:=80}"
: "${OMNI_MEM_TELEM_WARN_MAX_DISALLOWED_MS:=80}"
: "${OMNI_MEM_TELEM_WARN_MAX_REUSE_MS:=40}"
: "${OMNI_MEM_TELEM_WARN_MAX_PARTIAL_MS:=40}"
: "${OMNI_MEM_TELEM_WARN_MAX_SCOPE_SPLICE_MS:=40}"
: "${OMNI_MEM_TELEM_WARN_MAX_SHAPE_MS:=120}"

extract_line() {
  local pattern="$1"
  grep -aE "$pattern" "$log_file" | tail -n 1 || true
}

extract_field() {
  local line="$1"
  local key="$2"
  echo "$line" | tr ' ' '\n' | awk -F= -v k="$key" '$1 == k { print $2; exit }'
}

require_file() {
  if [[ ! -f "$log_file" ]]; then
    echo "FAIL: missing memory telemetry benchmark log: $log_file" >&2
    exit 1
  fi
}

require_line() {
  local label="$1"
  local line="$2"
  if [[ -z "$line" ]]; then
    echo "FAIL: missing ${label} line in ${log_file}" >&2
    exit 1
  fi
}

require_field() {
  local label="$1"
  local value="$2"
  if [[ -z "$value" ]]; then
    echo "FAIL: missing ${label}" >&2
    exit 1
  fi
}

assert_eq() {
  local label="$1"
  local actual="$2"
  local expected="$3"
  require_field "$label" "$actual"
  if [[ "$actual" != "$expected" ]]; then
    echo "FAIL: ${label} expected ${expected}, got ${actual}" >&2
    exit 1
  fi
}

assert_gt_zero() {
  local label="$1"
  local actual="$2"
  require_field "$label" "$actual"
  if (( actual <= 0 )); then
    echo "FAIL: ${label} expected > 0, got ${actual}" >&2
    exit 1
  fi
}

warn_gt() {
  local label="$1"
  local actual="$2"
  local limit="$3"
  require_field "$label" "$actual"
  if (( actual > limit )); then
    echo "WARN: ${label} exceeded advisory limit ${limit}, got ${actual}" >&2
  fi
}

warn_nonzero() {
  local label="$1"
  local actual="$2"
  require_field "$label" "$actual"
  if (( actual != 0 )); then
    echo "WARN: ${label} expected baseline value 0, got ${actual}" >&2
  fi
}

require_file

decision_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=boundary_decision_cost( |$)')"
partial_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=boundary_destination_routed_escape( |$)')"
tail_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=scope_splice_tail( |$)')"
equality_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=equality_nested_workspace( |$)')"
shape_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=boundary_value_shape_counters( |$)')"
boundary_line="$(extract_line '^OMNI_TEST_SUMMARY suite=boundary_decisions( |$)')"

require_line "boundary_decision_cost benchmark" "$decision_line"
require_line "boundary_destination_routed_escape benchmark" "$partial_line"
require_line "scope_splice_tail benchmark" "$tail_line"
require_line "equality_nested_workspace benchmark" "$equality_line"
require_line "boundary_value_shape_counters benchmark" "$shape_line"
require_line "boundary_decisions summary" "$boundary_line"

assert_eq "decision.iters" "$(extract_field "$decision_line" "iters")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "decision.splice_ok" "$(extract_field "$decision_line" "splice_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "decision.disallowed_ok" "$(extract_field "$decision_line" "disallowed_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "decision.reuse_ok" "$(extract_field "$decision_line" "reuse_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "partial.partial_ok" "$(extract_field "$partial_line" "partial_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "scope_splice_tail.splice_ok" "$(extract_field "$tail_line" "splice_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"

assert_eq "equality.fixture_ok" "$(extract_field "$equality_line" "fixture_ok")" "1"
assert_eq "equality.list_true_ok" "$(extract_field "$equality_line" "list_true_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "equality.list_false_ok" "$(extract_field "$equality_line" "list_false_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "equality.array_true_ok" "$(extract_field "$equality_line" "array_true_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "equality.array_false_ok" "$(extract_field "$equality_line" "array_false_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "equality.mixed_true_ok" "$(extract_field "$equality_line" "mixed_true_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"
assert_eq "equality.mixed_false_ok" "$(extract_field "$equality_line" "mixed_false_ok")" "$OMNI_MEM_TELEM_EXPECT_DECISION_ITERS"

assert_eq "shape.iters" "$(extract_field "$shape_line" "iters")" "$OMNI_MEM_TELEM_EXPECT_SHAPE_ITERS"
assert_eq "shape.shape_ok" "$(extract_field "$shape_line" "shape_ok")" "$OMNI_MEM_TELEM_EXPECT_SHAPE_ITERS"
assert_eq "shape.closure_env_ok" "$(extract_field "$shape_line" "closure_env_ok")" "$OMNI_MEM_TELEM_EXPECT_SHAPE_CLOSURE_OK"
assert_eq "shape.stable_passport_ok" "$(extract_field "$shape_line" "stable_passport_ok")" "$OMNI_MEM_TELEM_EXPECT_STABLE_PASSPORT_OK"
assert_eq "boundary.splice_fail_total" "$(extract_field "$boundary_line" "splice_fail_total")" "0"

assert_gt_zero "shape.temp_slow_delta" "$(extract_field "$shape_line" "temp_slow_delta")"
assert_gt_zero "shape.escape_slow_delta" "$(extract_field "$shape_line" "escape_slow_delta")"
assert_gt_zero "shape.temp_reset_slack_delta" "$(extract_field "$shape_line" "temp_reset_slack_delta")"
assert_gt_zero "shape.escape_reset_slack_delta" "$(extract_field "$shape_line" "escape_reset_slack_delta")"
assert_gt_zero "shape.array_growth_delta" "$(extract_field "$shape_line" "array_growth_delta")"
assert_gt_zero "shape.hashmap_growth_delta" "$(extract_field "$shape_line" "hashmap_growth_delta")"
assert_gt_zero "shape.set_growth_delta" "$(extract_field "$shape_line" "set_growth_delta")"
assert_gt_zero "shape.closure_env_frame_delta" "$(extract_field "$shape_line" "closure_env_frame_delta")"
assert_gt_zero "shape.string_payload_bytes_delta" "$(extract_field "$shape_line" "string_payload_bytes_delta")"
assert_gt_zero "shape.error_payload_bytes_delta" "$(extract_field "$shape_line" "error_payload_bytes_delta")"
assert_gt_zero "shape.big_integer_input_bytes_delta" "$(extract_field "$shape_line" "big_integer_input_bytes_delta")"
assert_gt_zero "shape.tensor_payload_bytes_delta" "$(extract_field "$shape_line" "tensor_payload_bytes_delta")"
assert_gt_zero "shape.ffi_releasable_delta" "$(extract_field "$shape_line" "ffi_releasable_delta")"
assert_gt_zero "shape.stable_stale_delta" "$(extract_field "$shape_line" "stable_stale_delta")"

warn_nonzero "shape.materialization_copy_bytes_delta" "$(extract_field "$shape_line" "materialization_copy_bytes_delta")"
warn_nonzero "boundary.materialization_copy_bytes" "$(extract_field "$boundary_line" "materialization_copy_bytes")"

warn_gt "decision.splice_ms" "$(extract_field "$decision_line" "splice_ms")" "$OMNI_MEM_TELEM_WARN_MAX_SPLICE_MS"
warn_gt "decision.disallowed_ms" "$(extract_field "$decision_line" "disallowed_ms")" "$OMNI_MEM_TELEM_WARN_MAX_DISALLOWED_MS"
warn_gt "decision.reuse_ms" "$(extract_field "$decision_line" "reuse_ms")" "$OMNI_MEM_TELEM_WARN_MAX_REUSE_MS"
warn_gt "partial.partial_ms" "$(extract_field "$partial_line" "partial_ms")" "$OMNI_MEM_TELEM_WARN_MAX_PARTIAL_MS"
warn_gt "scope_splice_tail.splice_ms" "$(extract_field "$tail_line" "splice_ms")" "$OMNI_MEM_TELEM_WARN_MAX_SCOPE_SPLICE_MS"
warn_gt "shape.shape_ms" "$(extract_field "$shape_line" "shape_ms")" "$OMNI_MEM_TELEM_WARN_MAX_SHAPE_MS"

echo "PASS: memory telemetry benchmark envelope satisfied for ${log_file}"
