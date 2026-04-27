#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
source scripts/c3c_limits.sh

script_dir="scripts"
facade_guard_script="${script_dir}/check_boundary_facade_usage.sh"
boundary_policy_script="${script_dir}/check_boundary_change_policy.sh"
boundary_threshold_script="${script_dir}/check_boundary_decision_thresholds.sh"
effects_contract_lint_script="${script_dir}/run_effects_contract_lint.sh"

# Boundary-hardening profile defaults.
: "${OMNI_BOUNDARY_ENABLE_FIBER_TEMP:=1}"
: "${OMNI_BOUNDARY_ENABLE_AFFINITY_HARNESS:=1}"
: "${OMNI_BOUNDARY_QUIET:=1}"
: "${OMNI_BOUNDARY_SUMMARY:=1}"
: "${OMNI_BOUNDARY_ASSERT_SUMMARY:=1}"
: "${OMNI_BOUNDARY_ASSERT_DECISION_TELEMETRY:=1}"
: "${OMNI_BOUNDARY_EMIT_JSON:=1}"
: "${OMNI_BOUNDARY_SUMMARY_JSON:=build/boundary_hardening_summary.json}"
: "${OMNI_BOUNDARY_SKIP_TLS_INTEGRATION:=1}"
: "${OMNI_BOUNDARY_ALERT_THRESHOLDS:=1}"
: "${OMNI_BOUNDARY_DECISION_BASELINE:=scripts/boundary_decision_baseline.env}"
: "${OMNI_BOUNDARY_EFFECTS_LINT_JSON:=build/effects_contract_lint_summary.json}"
if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" == "1" ]]; then
  : "${OMNI_HARD_MEM_CAP_METHOD:=none}"
else
  : "${OMNI_HARD_MEM_CAP_METHOD:=docker}"
fi
: "${OMNI_HARD_MEM_CAP_PERCENT:=30}"

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" != "1" && "${OMNI_HARD_MEM_CAP_METHOD}" != "docker" ]]; then
  echo "run_boundary_hardening.sh requires Docker-bound execution outside validation containers." >&2
  echo "Set OMNI_HARD_MEM_CAP_METHOD=docker (default) or run via scripts/run_validation_container.sh." >&2
  exit 2
fi

if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" ]] && ! command -v docker >/dev/null 2>&1; then
  echo "run_boundary_hardening.sh requires docker in PATH when OMNI_HARD_MEM_CAP_METHOD=docker." >&2
  exit 127
fi

if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" ]]; then
  : "${OMNI_C3_HARD_CAP_ENABLED:=1}"
fi

normal_log="build/boundary_hardening_normal.log"
asan_log="build/boundary_hardening_asan.log"
secondary_stage="asan"

mkdir -p build

extract_summary_line() {
  local log_file="$1"
  local suite="$2"
  grep -aE "OMNI_TEST_SUMMARY suite=${suite}( |$)" "$log_file" | tail -n 1 || true
}

extract_summary_field() {
  local line="$1"
  local key="$2"
  echo "$line" | tr ' ' '\n' | awk -F= -v k="$key" '$1 == k { print $2; exit }'
}

run_stage_with_log() {
  local log_file="$1"
  shift
  omni_run_with_hard_cap "$@" > "$log_file" 2>&1
  cat "$log_file"
}

append_stage_with_log() {
  local log_file="$1"
  shift
  local start_line=1
  if [[ -f "$log_file" ]]; then
    start_line=$(($(wc -l < "$log_file") + 1))
  fi
  omni_run_with_hard_cap "$@" >> "$log_file" 2>&1
  sed -n "${start_line},\$p" "$log_file"
}

assert_suite_fail_zero() {
  local log_file="$1"
  local suite="$2"
  local stage="$3"
  local line
  line="$(extract_summary_line "$log_file" "$suite")"
  if [[ -z "$line" ]]; then
    echo "FAIL: [$stage] missing OMNI_TEST_SUMMARY for suite=$suite"
    return 1
  fi
  local fail_val
  fail_val="$(extract_summary_field "$line" "fail")"
  if [[ -z "$fail_val" ]]; then
    echo "FAIL: [$stage] summary for suite=$suite has no fail= field"
    echo "  line: $line"
    return 1
  fi
  if [[ "$fail_val" != "0" ]]; then
    echo "FAIL: [$stage] suite=$suite reported fail=$fail_val"
    echo "  line: $line"
    return 1
  fi
  return 0
}

assert_fiber_temp_enabled() {
  local log_file="$1"
  local stage="$2"
  local line
  line="$(extract_summary_line "$log_file" "fiber_temp_pool")"
  if [[ -z "$line" ]]; then
    echo "FAIL: [$stage] missing OMNI_TEST_SUMMARY for suite=fiber_temp_pool"
    return 1
  fi
  local enabled
  enabled="$(extract_summary_field "$line" "enabled")"
  if [[ "$enabled" != "1" ]]; then
    echo "FAIL: [$stage] expected fiber_temp_pool enabled=1, got enabled=${enabled:-<missing>}"
    echo "  line: $line"
    return 1
  fi
  return 0
}

assert_boundary_decision_telemetry() {
  local log_file="$1"
  local stage="$2"
  local line
  line="$(extract_summary_line "$log_file" "boundary_decisions")"
  if [[ -z "$line" ]]; then
    echo "FAIL: [$stage] missing OMNI_TEST_SUMMARY for suite=boundary_decisions"
    return 1
  fi

  local required_keys=(
    splice_attempted
    splice_succeeded
    splice_fail_total
    promotion_attempted
    promotion_aborted_budget
    promotion_aborted_pre_aborted
    copy_fallback_total
  )

  local key
  for key in "${required_keys[@]}"; do
    local val
    val="$(extract_summary_field "$line" "$key")"
    if [[ -z "$val" ]]; then
      echo "FAIL: [$stage] boundary_decisions summary missing ${key}="
      echo "  line: $line"
      return 1
    fi
  done
  return 0
}

assert_stage_summary() {
  local log_file="$1"
  local stage="$2"

  assert_suite_fail_zero "$log_file" "stack_engine" "$stage"
  assert_suite_fail_zero "$log_file" "scope_region" "$stage"
  assert_suite_fail_zero "$log_file" "unified" "$stage"
  assert_suite_fail_zero "$log_file" "compiler" "$stage"

  if [[ "$OMNI_BOUNDARY_ENABLE_AFFINITY_HARNESS" == "1" ]]; then
    assert_suite_fail_zero "$log_file" "stack_affinity_harness" "$stage"
  fi

  if [[ "$OMNI_BOUNDARY_ENABLE_FIBER_TEMP" == "1" ]]; then
    assert_fiber_temp_enabled "$log_file" "$stage"
  fi

  if [[ "$OMNI_BOUNDARY_ASSERT_DECISION_TELEMETRY" == "1" ]]; then
    assert_boundary_decision_telemetry "$log_file" "$stage"
  fi
}

base_env=(
  "LD_LIBRARY_PATH=/usr/local/lib"
)

if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" && -n "${OMNI_DOCKER_TOOLCHAIN_ROOT:-}" ]]; then
  base_env[0]="LD_LIBRARY_PATH=/opt/omni-host-toolchain/lib"
fi

if [[ "$OMNI_BOUNDARY_ENABLE_FIBER_TEMP" == "1" ]]; then
  base_env+=("OMNI_FIBER_TEMP=1")
fi
if [[ "$OMNI_BOUNDARY_ENABLE_AFFINITY_HARNESS" == "1" ]]; then
  base_env+=("OMNI_STACK_AFFINITY_HARNESS=1")
fi
if [[ "$OMNI_BOUNDARY_QUIET" == "1" ]]; then
  base_env+=("OMNI_TEST_QUIET=1")
fi
if [[ "$OMNI_BOUNDARY_SUMMARY" == "1" ]]; then
  base_env+=("OMNI_TEST_SUMMARY=1")
fi
if [[ "$OMNI_BOUNDARY_SKIP_TLS_INTEGRATION" == "1" ]]; then
  base_env+=("OMNI_SKIP_TLS_INTEGRATION=1")
fi

if [[ "$OMNI_BOUNDARY_ASSERT_SUMMARY" == "1" && "$OMNI_BOUNDARY_SUMMARY" != "1" ]]; then
  echo "FAIL: OMNI_BOUNDARY_ASSERT_SUMMARY=1 requires OMNI_BOUNDARY_SUMMARY=1"
  exit 1
fi

echo "=== Boundary Hardening: Stage 0 (boundary facade guard) ==="
"$facade_guard_script"

echo ""
echo "=== Boundary Hardening: Stage 0b (effects contract lint) ==="
OMNI_EFFECTS_POLICY_RANGE="${OMNI_BOUNDARY_POLICY_RANGE:-}" \
  "$effects_contract_lint_script" "$OMNI_BOUNDARY_EFFECTS_LINT_JSON"

echo ""
echo "=== Boundary Hardening: Stage 1 (normal build) ==="
omni_c3 build -D OMNI_BOUNDARY_INSTR_COUNTERS

echo ""
echo "=== Boundary Hardening: Stage 2 (normal run) ==="
run_stage_with_log "$normal_log" env "${base_env[@]}" ./build/main --test-suite all
echo ""
echo "=== Boundary Hardening: Stage 2b (normal compiler slice) ==="
append_stage_with_log "$normal_log" env "${base_env[@]}" OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp

echo ""
echo "=== Boundary Hardening: Stage 3 (ASAN build) ==="
omni_run_with_hard_cap c3c clean
asan_build_output=""
if asan_build_output="$(omni_c3 build --sanitize=address -D OMNI_BOUNDARY_INSTR_COUNTERS 2>&1)"; then
  printf "%s\n" "$asan_build_output"
else
  printf "%s\n" "$asan_build_output"
  if grep -qi "address sanitizer is only supported" <<<"$asan_build_output"; then
    secondary_stage="valgrind"
    echo "ASAN is unsupported by this toolchain/target; rebuilding normal binary for Valgrind fallback."
    omni_run_with_hard_cap c3c clean
    omni_c3 build -D OMNI_BOUNDARY_INSTR_COUNTERS
  else
    exit 1
  fi
fi

echo ""
if [[ "$secondary_stage" == "asan" ]]; then
  echo "=== Boundary Hardening: Stage 4 (ASAN run) ==="
  run_stage_with_log \
    "$asan_log" \
    env \
      "${base_env[@]}" \
      "ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1" \
      ./build/main --test-suite all
else
  echo "=== Boundary Hardening: Stage 4 (Valgrind fallback run) ==="
  run_stage_with_log \
    "$asan_log" \
    valgrind \
      --trace-children=yes \
      --leak-check=full \
      --show-leak-kinds=definite,indirect,possible \
    --error-exitcode=99 \
    env \
      "${base_env[@]}" \
      OMNI_LISP_TEST_SLICE=memory-lifetime-smoke \
      ./build/main --test-suite lisp
fi
echo ""
if [[ "$secondary_stage" == "asan" ]]; then
  echo "=== Boundary Hardening: Stage 4b (ASAN compiler slice) ==="
  append_stage_with_log \
    "$asan_log" \
    env \
      "${base_env[@]}" \
      "ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1" \
      OMNI_LISP_TEST_SLICE=compiler \
      ./build/main --test-suite lisp
else
  echo "=== Boundary Hardening: Stage 4b (Valgrind fallback compiler slice skipped) ==="
  echo "Normal compiler slice already passed; Valgrind fallback is scoped to memory-lifetime-smoke."
fi

if [[ "$OMNI_BOUNDARY_ASSERT_SUMMARY" == "1" ]]; then
  echo ""
  echo "=== Boundary Hardening: Stage 5 (summary assertions) ==="
  assert_stage_summary "$normal_log" "normal"
  if [[ "$secondary_stage" == "asan" ]]; then
    assert_stage_summary "$asan_log" "$secondary_stage"
  else
    assert_suite_fail_zero "$asan_log" "unified" "$secondary_stage"
  fi
  echo "Summary assertions passed."
fi

if [[ "$secondary_stage" == "asan" && "$OMNI_BOUNDARY_EMIT_JSON" == "1" && "$OMNI_BOUNDARY_SUMMARY" == "1" ]]; then
  echo ""
  echo "=== Boundary Hardening: Stage 6 (summary artifact) ==="
  scripts/parse_boundary_summary.sh "$normal_log" "$asan_log" "$OMNI_BOUNDARY_SUMMARY_JSON"
fi

if [[ "$secondary_stage" == "asan" && "$OMNI_BOUNDARY_ALERT_THRESHOLDS" == "1" ]]; then
  echo ""
  echo "=== Boundary Hardening: Stage 7 (boundary telemetry thresholds) ==="
  "$boundary_threshold_script" "$normal_log" "$asan_log" "$OMNI_BOUNDARY_DECISION_BASELINE"
fi

echo ""
echo "=== Boundary Hardening: Stage 8 (boundary change policy) ==="
OMNI_BOUNDARY_SECONDARY_KIND="$secondary_stage" "$boundary_policy_script" "$normal_log" "$asan_log"

echo ""
echo "Boundary hardening profile passed."
echo "  normal log: $normal_log"
echo "  ${secondary_stage} log:   $asan_log"
if [[ "$secondary_stage" == "asan" && "$OMNI_BOUNDARY_EMIT_JSON" == "1" && "$OMNI_BOUNDARY_SUMMARY" == "1" ]]; then
  echo "  summary:    $OMNI_BOUNDARY_SUMMARY_JSON"
elif [[ "$secondary_stage" == "valgrind" && "$OMNI_BOUNDARY_EMIT_JSON" == "1" && "$OMNI_BOUNDARY_SUMMARY" == "1" ]]; then
  echo "  summary:    skipped for Valgrind fallback"
fi
echo "  lint json:  $OMNI_BOUNDARY_EFFECTS_LINT_JSON"
