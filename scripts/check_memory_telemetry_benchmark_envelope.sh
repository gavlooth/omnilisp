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

assert_nonnegative() {
  local label="$1"
  local actual="$2"
  require_field "$label" "$actual"
  if (( actual < 0 )); then
    echo "FAIL: ${label} expected >= 0, got ${actual}" >&2
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
product_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=finwatch_product_memory( |$)')"
closure_iter_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=closure_iterator_pipeline_memory( |$)')"
tensor_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=tensor_metadata_crossing_memory( |$)')"
nested_module_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=nested_module_return_memory( |$)')"
shape_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=boundary_value_shape_counters( |$)')"
boundary_line="$(extract_line '^OMNI_TEST_SUMMARY suite=boundary_decisions( |$)')"

require_line "boundary_decision_cost benchmark" "$decision_line"
require_line "boundary_destination_routed_escape benchmark" "$partial_line"
require_line "scope_splice_tail benchmark" "$tail_line"
require_line "equality_nested_workspace benchmark" "$equality_line"
require_line "finwatch_product_memory benchmark" "$product_line"
require_line "closure_iterator_pipeline_memory benchmark" "$closure_iter_line"
require_line "tensor_metadata_crossing_memory benchmark" "$tensor_line"
require_line "nested_module_return_memory benchmark" "$nested_module_line"
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

assert_eq "product.iters" "$(extract_field "$product_line" "iters")" "64"
assert_eq "product.product_ok" "$(extract_field "$product_line" "product_ok")" "64"
assert_eq "product.commit_ok" "$(extract_field "$product_line" "commit_ok")" "64"
assert_nonnegative "product.hashmap_construct_delta" "$(extract_field "$product_line" "hashmap_construct_delta")"
assert_nonnegative "product.set_construct_delta" "$(extract_field "$product_line" "set_construct_delta")"
assert_nonnegative "product.array_construct_delta" "$(extract_field "$product_line" "array_construct_delta")"
assert_nonnegative "product.string_payload_bytes_delta" "$(extract_field "$product_line" "string_payload_bytes_delta")"
assert_nonnegative "product.ffi_wrappers_delta" "$(extract_field "$product_line" "ffi_wrappers_delta")"
assert_nonnegative "product.ffi_releasable_delta" "$(extract_field "$product_line" "ffi_releasable_delta")"
assert_nonnegative "product.selected_transplant_delta" "$(extract_field "$product_line" "selected_transplant_delta")"
assert_nonnegative "product.materialization_copy_bytes_delta" "$(extract_field "$product_line" "materialization_copy_bytes_delta")"

assert_eq "closure_iter.iters" "$(extract_field "$closure_iter_line" "iters")" "64"
assert_eq "closure_iter.expected_count" "$(extract_field "$closure_iter_line" "expected_count")" "10"
assert_eq "closure_iter.expected_sum" "$(extract_field "$closure_iter_line" "expected_sum")" "355"
assert_eq "closure_iter.pipeline_ok" "$(extract_field "$closure_iter_line" "pipeline_ok")" "64"
assert_eq "closure_iter.count_total" "$(extract_field "$closure_iter_line" "count_total")" "640"
assert_eq "closure_iter.sum_total" "$(extract_field "$closure_iter_line" "sum_total")" "22720"
assert_gt_zero "closure_iter.iterator_roots_delta" "$(extract_field "$closure_iter_line" "iterator_roots_delta")"
assert_gt_zero "closure_iter.partial_roots_delta" "$(extract_field "$closure_iter_line" "partial_roots_delta")"
assert_gt_zero "closure_iter.closure_env_frame_delta" "$(extract_field "$closure_iter_line" "closure_env_frame_delta")"
assert_gt_zero "closure_iter.closure_env_binding_delta" "$(extract_field "$closure_iter_line" "closure_env_binding_delta")"
assert_nonnegative "closure_iter.closure_roots_delta" "$(extract_field "$closure_iter_line" "closure_roots_delta")"
assert_nonnegative "closure_iter.selected_transplant_delta" "$(extract_field "$closure_iter_line" "selected_transplant_delta")"
assert_nonnegative "closure_iter.materialization_copy_bytes_delta" "$(extract_field "$closure_iter_line" "materialization_copy_bytes_delta")"

assert_eq "tensor.iters" "$(extract_field "$tensor_line" "iters")" "64"
assert_eq "tensor.tensors_per_iter" "$(extract_field "$tensor_line" "tensors_per_iter")" "4"
assert_eq "tensor.tensor_ok" "$(extract_field "$tensor_line" "tensor_ok")" "64"
assert_eq "tensor.commit_ok" "$(extract_field "$tensor_line" "commit_ok")" "64"
assert_gt_zero "tensor.tensor_roots_delta" "$(extract_field "$tensor_line" "tensor_roots_delta")"
assert_gt_zero "tensor.tensor_payload_bytes_delta" "$(extract_field "$tensor_line" "tensor_payload_bytes_delta")"
assert_nonnegative "tensor.hashmap_construct_delta" "$(extract_field "$tensor_line" "hashmap_construct_delta")"
assert_gt_zero "tensor.array_construct_delta" "$(extract_field "$tensor_line" "array_construct_delta")"
assert_nonnegative "tensor.selected_transplant_delta" "$(extract_field "$tensor_line" "selected_transplant_delta")"
assert_nonnegative "tensor.materialization_copy_bytes_delta" "$(extract_field "$tensor_line" "materialization_copy_bytes_delta")"

assert_eq "nested_module.iters" "$(extract_field "$nested_module_line" "iters")" "64"
assert_eq "nested_module.setup_ok" "$(extract_field "$nested_module_line" "setup_ok")" "1"
assert_eq "nested_module.batch_ok" "$(extract_field "$nested_module_line" "batch_ok")" "64"
assert_nonnegative "nested_module.cons_roots_delta" "$(extract_field "$nested_module_line" "cons_roots_delta")"
assert_gt_zero "nested_module.array_construct_delta" "$(extract_field "$nested_module_line" "array_construct_delta")"
assert_nonnegative "nested_module.closure_env_frame_delta" "$(extract_field "$nested_module_line" "closure_env_frame_delta")"
assert_nonnegative "nested_module.closure_env_binding_delta" "$(extract_field "$nested_module_line" "closure_env_binding_delta")"
assert_nonnegative "nested_module.selected_transplant_delta" "$(extract_field "$nested_module_line" "selected_transplant_delta")"
assert_gt_zero "nested_module.selected_stable_materialize_delta" "$(extract_field "$nested_module_line" "selected_stable_materialize_delta")"
assert_gt_zero "nested_module.materialization_copy_bytes_delta" "$(extract_field "$nested_module_line" "materialization_copy_bytes_delta")"

assert_eq "shape.iters" "$(extract_field "$shape_line" "iters")" "$OMNI_MEM_TELEM_EXPECT_SHAPE_ITERS"
assert_eq "shape.shape_ok" "$(extract_field "$shape_line" "shape_ok")" "$OMNI_MEM_TELEM_EXPECT_SHAPE_ITERS"
assert_eq "shape.closure_env_ok" "$(extract_field "$shape_line" "closure_env_ok")" "$OMNI_MEM_TELEM_EXPECT_SHAPE_CLOSURE_OK"
assert_eq "shape.stable_passport_ok" "$(extract_field "$shape_line" "stable_passport_ok")" "$OMNI_MEM_TELEM_EXPECT_STABLE_PASSPORT_OK"
assert_eq "boundary.splice_fail_total" "$(extract_field "$boundary_line" "splice_fail_total")" "0"
assert_eq "boundary.materialization_copy_bytes_forced_no_splice" "$(extract_field "$boundary_line" "materialization_copy_bytes_forced_no_splice")" "0"

assert_gt_zero "shape.temp_slow_delta" "$(extract_field "$shape_line" "temp_slow_delta")"
assert_gt_zero "shape.escape_slow_delta" "$(extract_field "$shape_line" "escape_slow_delta")"
assert_gt_zero "shape.temp_reset_slack_delta" "$(extract_field "$shape_line" "temp_reset_slack_delta")"
assert_gt_zero "shape.escape_reset_slack_delta" "$(extract_field "$shape_line" "escape_reset_slack_delta")"
assert_gt_zero "shape.array_growth_delta" "$(extract_field "$shape_line" "array_growth_delta")"
hashmap_growth_delta="$(extract_field "$shape_line" "hashmap_growth_delta")"
set_growth_delta="$(extract_field "$shape_line" "set_growth_delta")"
assert_nonnegative "shape.hashmap_growth_delta" "$hashmap_growth_delta"
assert_nonnegative "shape.set_growth_delta" "$set_growth_delta"
if [[ "${OMNI_MEM_TELEM_REQUIRE_COLLECTION_GROWTH_ZERO:-0}" == "1" ]]; then
  assert_eq "shape.hashmap_growth_delta" "$hashmap_growth_delta" "0"
  assert_eq "shape.set_growth_delta" "$set_growth_delta" "0"
fi
assert_gt_zero "shape.closure_env_frame_delta" "$(extract_field "$shape_line" "closure_env_frame_delta")"
assert_gt_zero "shape.string_payload_bytes_delta" "$(extract_field "$shape_line" "string_payload_bytes_delta")"
assert_gt_zero "shape.error_payload_bytes_delta" "$(extract_field "$shape_line" "error_payload_bytes_delta")"
assert_gt_zero "shape.big_integer_input_bytes_delta" "$(extract_field "$shape_line" "big_integer_input_bytes_delta")"
assert_gt_zero "shape.tensor_payload_bytes_delta" "$(extract_field "$shape_line" "tensor_payload_bytes_delta")"
assert_gt_zero "shape.ffi_releasable_delta" "$(extract_field "$shape_line" "ffi_releasable_delta")"
assert_gt_zero "shape.stable_stale_delta" "$(extract_field "$shape_line" "stable_stale_delta")"

if [[ "${OMNI_MEM_TELEM_REQUIRE_SLOW_SLACK_HISTOGRAM:-0}" == "1" ]]; then
  require_field "shape.temp_slow_slack_exact_delta" "$(extract_field "$shape_line" "temp_slow_slack_exact_delta")"
  require_field "shape.temp_slow_slack_le512_delta" "$(extract_field "$shape_line" "temp_slow_slack_le512_delta")"
  require_field "shape.temp_slow_slack_le4096_delta" "$(extract_field "$shape_line" "temp_slow_slack_le4096_delta")"
  require_field "shape.temp_slow_slack_gt4096_delta" "$(extract_field "$shape_line" "temp_slow_slack_gt4096_delta")"
  require_field "shape.escape_slow_slack_exact_delta" "$(extract_field "$shape_line" "escape_slow_slack_exact_delta")"
  require_field "shape.escape_slow_slack_le512_delta" "$(extract_field "$shape_line" "escape_slow_slack_le512_delta")"
  require_field "shape.escape_slow_slack_le4096_delta" "$(extract_field "$shape_line" "escape_slow_slack_le4096_delta")"
  require_field "shape.escape_slow_slack_gt4096_delta" "$(extract_field "$shape_line" "escape_slow_slack_gt4096_delta")"
fi

if [[ "${OMNI_MEM_TELEM_REQUIRE_SCOPE_SEQUENCE:-0}" == "1" ]]; then
  assert_gt_zero "shape.temp_slow_sequence_closed_delta" "$(extract_field "$shape_line" "temp_slow_sequence_closed_delta")"
  assert_gt_zero "shape.escape_slow_sequence_closed_delta" "$(extract_field "$shape_line" "escape_slow_sequence_closed_delta")"
  assert_gt_zero "shape.temp_slow_sequence_followup_alloc_delta" "$(extract_field "$shape_line" "temp_slow_sequence_followup_alloc_delta")"
  assert_gt_zero "shape.escape_slow_sequence_followup_alloc_delta" "$(extract_field "$shape_line" "escape_slow_sequence_followup_alloc_delta")"
  assert_gt_zero "shape.temp_slow_sequence_followup_bytes_delta" "$(extract_field "$shape_line" "temp_slow_sequence_followup_bytes_delta")"
  assert_gt_zero "shape.escape_slow_sequence_followup_bytes_delta" "$(extract_field "$shape_line" "escape_slow_sequence_followup_bytes_delta")"
  assert_nonnegative "shape.temp_slow_sequence_unused_close_delta" "$(extract_field "$shape_line" "temp_slow_sequence_unused_close_delta")"
  assert_nonnegative "shape.escape_slow_sequence_unused_close_delta" "$(extract_field "$shape_line" "escape_slow_sequence_unused_close_delta")"
  assert_nonnegative "shape.temp_slow_sequence_no_followup_delta" "$(extract_field "$shape_line" "temp_slow_sequence_no_followup_delta")"
  escape_no_followup_delta="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_delta")"
  assert_gt_zero "shape.escape_slow_sequence_no_followup_delta" "$escape_no_followup_delta"
  escape_no_followup_request_le512="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_request_le512_delta")"
  escape_no_followup_request_le4096="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_request_le4096_delta")"
  escape_no_followup_request_gt4096="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_request_gt4096_delta")"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_request_le512_delta" "$escape_no_followup_request_le512"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_request_le4096_delta" "$escape_no_followup_request_le4096"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_request_gt4096_delta" "$escape_no_followup_request_gt4096"
  escape_no_followup_request_sum=$((escape_no_followup_request_le512 + escape_no_followup_request_le4096 + escape_no_followup_request_gt4096))
  assert_eq "shape.escape_slow_sequence_no_followup_request_sum" "$escape_no_followup_request_sum" "$escape_no_followup_delta"

  escape_no_followup_unused_exact="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_unused_exact_delta")"
  escape_no_followup_unused_le512="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_unused_le512_delta")"
  escape_no_followup_unused_le4096="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_unused_le4096_delta")"
  escape_no_followup_unused_gt4096="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_unused_gt4096_delta")"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_unused_exact_delta" "$escape_no_followup_unused_exact"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_unused_le512_delta" "$escape_no_followup_unused_le512"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_unused_le4096_delta" "$escape_no_followup_unused_le4096"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_unused_gt4096_delta" "$escape_no_followup_unused_gt4096"
  escape_no_followup_unused_sum=$((escape_no_followup_unused_exact + escape_no_followup_unused_le512 + escape_no_followup_unused_le4096 + escape_no_followup_unused_gt4096))
  assert_eq "shape.escape_slow_sequence_no_followup_unused_sum" "$escape_no_followup_unused_sum" "$escape_no_followup_delta"

  escape_no_followup_source_unknown="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_source_unknown_delta")"
  escape_no_followup_source_direct="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_source_direct_delta")"
  escape_no_followup_source_dtor="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_source_dtor_delta")"
  escape_no_followup_source_interp_value="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_source_interp_value_delta")"
  escape_no_followup_source_interp_env="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_source_interp_env_delta")"
  escape_no_followup_source_boundary_payload="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_source_boundary_payload_delta")"
  escape_no_followup_source_promotion_signature="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_source_promotion_signature_delta")"
  escape_no_followup_source_promotion_closure="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_source_promotion_closure_delta")"
  escape_no_followup_source_jit_args="$(extract_field "$shape_line" "escape_slow_sequence_no_followup_source_jit_args_delta")"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_source_unknown_delta" "$escape_no_followup_source_unknown"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_source_direct_delta" "$escape_no_followup_source_direct"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_source_dtor_delta" "$escape_no_followup_source_dtor"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_source_interp_value_delta" "$escape_no_followup_source_interp_value"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_source_interp_env_delta" "$escape_no_followup_source_interp_env"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_source_boundary_payload_delta" "$escape_no_followup_source_boundary_payload"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_source_promotion_signature_delta" "$escape_no_followup_source_promotion_signature"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_source_promotion_closure_delta" "$escape_no_followup_source_promotion_closure"
  assert_nonnegative "shape.escape_slow_sequence_no_followup_source_jit_args_delta" "$escape_no_followup_source_jit_args"
  escape_no_followup_source_sum=$((escape_no_followup_source_unknown + escape_no_followup_source_direct + escape_no_followup_source_dtor + escape_no_followup_source_interp_value + escape_no_followup_source_interp_env + escape_no_followup_source_boundary_payload + escape_no_followup_source_promotion_signature + escape_no_followup_source_promotion_closure + escape_no_followup_source_jit_args))
  assert_eq "shape.escape_slow_sequence_no_followup_source_sum" "$escape_no_followup_source_sum" "$escape_no_followup_delta"

  assert_gt_zero "shape.temp_slow_sequence_large_delta" "$(extract_field "$shape_line" "temp_slow_sequence_large_delta")"
  assert_nonnegative "shape.escape_slow_sequence_large_delta" "$(extract_field "$shape_line" "escape_slow_sequence_large_delta")"
  assert_gt_zero "shape.temp_slow_sequence_large_followup_bytes_delta" "$(extract_field "$shape_line" "temp_slow_sequence_large_followup_bytes_delta")"
  assert_nonnegative "shape.escape_slow_sequence_large_followup_bytes_delta" "$(extract_field "$shape_line" "escape_slow_sequence_large_followup_bytes_delta")"
  assert_nonnegative "shape.temp_slow_sequence_large_unused_close_delta" "$(extract_field "$shape_line" "temp_slow_sequence_large_unused_close_delta")"
  assert_nonnegative "shape.escape_slow_sequence_large_unused_close_delta" "$(extract_field "$shape_line" "escape_slow_sequence_large_unused_close_delta")"
  assert_nonnegative "shape.temp_slow_sequence_large_no_followup_delta" "$(extract_field "$shape_line" "temp_slow_sequence_large_no_followup_delta")"
  assert_nonnegative "shape.escape_slow_sequence_large_no_followup_delta" "$(extract_field "$shape_line" "escape_slow_sequence_large_no_followup_delta")"
fi

warn_nonzero "shape.materialization_copy_bytes_delta" "$(extract_field "$shape_line" "materialization_copy_bytes_delta")"
warn_nonzero "boundary.materialization_copy_bytes_optimizer" "$(extract_field "$boundary_line" "materialization_copy_bytes_optimizer")"

warn_gt "decision.splice_ms" "$(extract_field "$decision_line" "splice_ms")" "$OMNI_MEM_TELEM_WARN_MAX_SPLICE_MS"
warn_gt "decision.disallowed_ms" "$(extract_field "$decision_line" "disallowed_ms")" "$OMNI_MEM_TELEM_WARN_MAX_DISALLOWED_MS"
warn_gt "decision.reuse_ms" "$(extract_field "$decision_line" "reuse_ms")" "$OMNI_MEM_TELEM_WARN_MAX_REUSE_MS"
warn_gt "partial.partial_ms" "$(extract_field "$partial_line" "partial_ms")" "$OMNI_MEM_TELEM_WARN_MAX_PARTIAL_MS"
warn_gt "scope_splice_tail.splice_ms" "$(extract_field "$tail_line" "splice_ms")" "$OMNI_MEM_TELEM_WARN_MAX_SCOPE_SPLICE_MS"
warn_gt "shape.shape_ms" "$(extract_field "$shape_line" "shape_ms")" "$OMNI_MEM_TELEM_WARN_MAX_SHAPE_MS"

echo "PASS: memory telemetry benchmark envelope satisfied for ${log_file}"
