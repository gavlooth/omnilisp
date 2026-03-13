#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
source scripts/c3c_limits.sh

: "${OMNI_HARD_MEM_CAP_METHOD:=docker}"
: "${OMNI_DEDUCE_PERF_BUILD:=1}"
: "${OMNI_DEDUCE_PERF_LOG:=build/deduce_perf_envelope.log}"
: "${OMNI_DEDUCE_PERF_QUIET:=1}"
: "${OMNI_DEDUCE_PERF_SUMMARY:=1}"
: "${OMNI_DEDUCE_PERF_SKIP_TLS:=1}"

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" != "1" ]]; then
  if [[ "$OMNI_HARD_MEM_CAP_METHOD" != "docker" ]]; then
    echo "run_deduce_perf_envelope.sh requires Docker-bound execution outside validation containers." >&2
    echo "Set OMNI_HARD_MEM_CAP_METHOD=docker (default) or run via scripts/run_validation_container.sh." >&2
    exit 2
  fi

  validation_extra="${OMNI_VALIDATION_EXTRA_ARGS:-}"
  if [[ -f /usr/include/yyjson.h && "$validation_extra" != *"/usr/include/yyjson.h"* ]]; then
    validation_extra="${validation_extra} --mount type=bind,src=/usr/include/yyjson.h,dst=/usr/include/yyjson.h,readonly"
  fi
  for header in /usr/include/bearssl*.h; do
    if [[ -f "$header" && "$validation_extra" != *"$header"* ]]; then
      validation_extra="${validation_extra} --mount type=bind,src=${header},dst=${header},readonly"
    fi
  done
  if [[ -f /usr/include/uv.h && "$validation_extra" != *"/usr/include/uv.h"* ]]; then
    validation_extra="${validation_extra} --mount type=bind,src=/usr/include/uv.h,dst=/usr/include/uv.h,readonly"
  fi
  if [[ -d /usr/include/uv && "$validation_extra" != *"/usr/include/uv,dst=/usr/include/uv"* ]]; then
    validation_extra="${validation_extra} --mount type=bind,src=/usr/include/uv,dst=/usr/include/uv,readonly"
  fi
  if [[ -f /usr/include/ffi.h && "$validation_extra" != *"/usr/include/ffi.h"* ]]; then
    validation_extra="${validation_extra} --mount type=bind,src=/usr/include/ffi.h,dst=/usr/include/ffi.h,readonly"
  fi
  if [[ -e /usr/lib/libreplxx.so.0 && "$validation_extra" != *"/usr/lib/libreplxx.so.0"* ]]; then
    validation_extra="${validation_extra} --mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly"
  fi
  if [[ -n "$validation_extra" ]]; then
    # Trim leading whitespace from concatenation flow above.
    # shellcheck disable=SC2001
    validation_extra="$(echo "$validation_extra" | sed 's/^ *//')"
    export OMNI_VALIDATION_EXTRA_ARGS="$validation_extra"
  fi

  exec scripts/run_validation_container.sh scripts/run_deduce_perf_envelope.sh
fi

mkdir -p "$(dirname "$OMNI_DEDUCE_PERF_LOG")"

summary_line() {
  local suite="$1"
  grep -aE "OMNI_BENCH_SUMMARY suite=${suite}( |$)" "$OMNI_DEDUCE_PERF_LOG" | tail -n 1 || true
}

summary_field() {
  local line="$1"
  local key="$2"
  echo "$line" | tr ' ' '\n' | awk -F= -v k="$key" '$1 == k { print $2; exit }'
}

assert_field_equals() {
  local line="$1"
  local key="$2"
  local expected="$3"
  local actual
  actual="$(summary_field "$line" "$key")"
  if [[ -z "$actual" ]]; then
    echo "FAIL: missing ${key}= in summary line" >&2
    echo "  line: $line" >&2
    return 1
  fi
  if [[ "$actual" != "$expected" ]]; then
    echo "FAIL: expected ${key}=${expected}, got ${actual}" >&2
    echo "  line: $line" >&2
    return 1
  fi
}

assert_field_matches_field() {
  local line="$1"
  local left="$2"
  local right="$3"
  local left_val
  local right_val
  left_val="$(summary_field "$line" "$left")"
  right_val="$(summary_field "$line" "$right")"
  if [[ -z "$left_val" || -z "$right_val" ]]; then
    echo "FAIL: missing ${left}= or ${right}= in summary line" >&2
    echo "  line: $line" >&2
    return 1
  fi
  if [[ "$left_val" != "$right_val" ]]; then
    echo "FAIL: expected ${left} (${left_val}) == ${right} (${right_val})" >&2
    echo "  line: $line" >&2
    return 1
  fi
}

echo "=== Deduce Perf Envelope: build ==="
if [[ "$OMNI_DEDUCE_PERF_BUILD" == "1" ]]; then
  omni_c3 build
fi

runtime_ld_library_path="/usr/local/lib"
if [[ -n "${OMNI_DOCKER_TOOLCHAIN_ROOT:-}" ]]; then
  runtime_ld_library_path="/opt/omni-host-toolchain/lib"
fi

run_env=(
  "LD_LIBRARY_PATH=${runtime_ld_library_path}"
  "OMNI_LISP_TEST_SLICE=deduce"
  "OMNI_DEDUCE_BENCH=1"
)
if [[ "$OMNI_DEDUCE_PERF_QUIET" == "1" ]]; then
  run_env+=("OMNI_TEST_QUIET=1")
fi
if [[ "$OMNI_DEDUCE_PERF_SUMMARY" == "1" ]]; then
  run_env+=("OMNI_TEST_SUMMARY=1")
fi
if [[ "$OMNI_DEDUCE_PERF_SKIP_TLS" == "1" ]]; then
  run_env+=("OMNI_SKIP_TLS_INTEGRATION=1")
fi

echo ""
echo "=== Deduce Perf Envelope: benchmark run ==="
env "${run_env[@]}" ./build/main --test-suite lisp > "$OMNI_DEDUCE_PERF_LOG" 2>&1
cat "$OMNI_DEDUCE_PERF_LOG"

scan_line="$(summary_line "deduce_scan_query_count")"
inc_line="$(summary_line "deduce_incremental_mutation")"
join_line="$(summary_line "deduce_selective_join")"
rec_line="$(summary_line "deduce_recursive_closure")"
skew_line="$(summary_line "deduce_skewed_cardinality")"

if [[ -z "$scan_line" || -z "$inc_line" || -z "$join_line" || -z "$rec_line" || -z "$skew_line" ]]; then
  echo "FAIL: missing one or more Deduce benchmark summary lines." >&2
  echo "  deduce_scan_query_count: ${scan_line:-<missing>}" >&2
  echo "  deduce_incremental_mutation: ${inc_line:-<missing>}" >&2
  echo "  deduce_selective_join: ${join_line:-<missing>}" >&2
  echo "  deduce_recursive_closure: ${rec_line:-<missing>}" >&2
  echo "  deduce_skewed_cardinality: ${skew_line:-<missing>}" >&2
  exit 1
fi

assert_field_equals "$scan_line" "seeded_ok" "1"
assert_field_equals "$scan_line" "filter_ok" "1"
assert_field_matches_field "$scan_line" "scan_ok" "iters_scan"
assert_field_matches_field "$scan_line" "scan_range_ok" "iters_scan_range"
assert_field_matches_field "$scan_line" "query_ok" "iters_query"
assert_field_matches_field "$scan_line" "count_ok" "iters_count"
assert_field_equals "$scan_line" "iter_scope_fail" "0"

assert_field_equals "$inc_line" "tracked_seeded_ok" "1"
assert_field_equals "$inc_line" "full_seeded_ok" "1"
assert_field_matches_field "$inc_line" "tracked_analyze_attempted" "tracked_iters"
assert_field_matches_field "$inc_line" "tracked_analyze_ok" "tracked_iters"
assert_field_equals "$inc_line" "tracked_mode_miss" "0"
assert_field_matches_field "$inc_line" "full_analyze_attempted" "full_iters"
assert_field_matches_field "$inc_line" "full_analyze_ok" "full_iters"
assert_field_equals "$inc_line" "full_mode_miss" "0"
assert_field_equals "$inc_line" "iter_scope_fail" "0"

assert_field_equals "$join_line" "seeded_ok" "1"
assert_field_matches_field "$join_line" "explain_attempted" "explain_iters"
assert_field_matches_field "$join_line" "explain_ok" "explain_iters"
assert_field_equals "$join_line" "explain_mode_miss" "0"
assert_field_matches_field "$join_line" "analyze_ok" "analyze_iters"
assert_field_equals "$join_line" "iter_scope_fail" "0"

assert_field_equals "$rec_line" "naive_seeded_ok" "1"
assert_field_equals "$rec_line" "semi_seeded_ok" "1"
assert_field_matches_field "$rec_line" "naive_attempted" "naive_iters"
assert_field_matches_field "$rec_line" "naive_ok" "naive_iters"
assert_field_matches_field "$rec_line" "naive_engine_hit" "naive_iters"
assert_field_equals "$rec_line" "naive_mode_miss" "0"
assert_field_matches_field "$rec_line" "semi_attempted" "semi_iters"
assert_field_matches_field "$rec_line" "semi_ok" "semi_iters"
assert_field_matches_field "$rec_line" "semi_engine_hit" "semi_iters"
assert_field_equals "$rec_line" "semi_mode_miss" "0"
assert_field_equals "$rec_line" "iter_scope_fail" "0"

assert_field_equals "$skew_line" "seeded_ok" "1"
assert_field_matches_field "$skew_line" "explain_attempted" "explain_iters"
assert_field_matches_field "$skew_line" "explain_ok" "explain_iters"
assert_field_equals "$skew_line" "explain_mode_miss" "0"
assert_field_matches_field "$skew_line" "analyze_ok" "analyze_iters"
assert_field_equals "$skew_line" "iter_scope_fail" "0"

echo ""
echo "Deduce perf envelope checks passed."
echo "  log: $OMNI_DEDUCE_PERF_LOG"
