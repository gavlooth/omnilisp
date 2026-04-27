#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

normal_log="${1:-build/boundary_hardening_normal.log}"
asan_log="${2:-build/boundary_hardening_asan.log}"
baseline_file="${3:-scripts/boundary_decision_baseline.env}"
secondary_kind="${OMNI_BOUNDARY_SECONDARY_KIND:-}"

: "${OMNI_BOUNDARY_ALERT_COPY_FALLBACK_FACTOR:=1.30}"
: "${OMNI_BOUNDARY_ALERT_SPLICE_SUCCESS_DROP_PCT:=5.0}"

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

require_file() {
  local path="$1"
  if [[ ! -f "$path" ]]; then
    echo "FAIL: threshold check missing file: $path"
    exit 1
  fi
}

calc_success_bp() {
  local succeeded="$1"
  local attempted="$2"
  awk -v s="$succeeded" -v a="$attempted" 'BEGIN { if (a <= 0) { print 0; exit } printf "%.6f", (s * 10000.0) / a }'
}

check_stage() {
  local stage="$1"
  local log_file="$2"
  local baseline_attempted="$3"
  local baseline_succeeded="$4"
  local baseline_fallback="$5"

  local line
  line="$(extract_summary_line "$log_file" "boundary_decisions")"
  if [[ -z "$line" ]]; then
    echo "FAIL: [$stage] missing boundary_decisions summary line"
    return 1
  fi

  local cur_attempted cur_succeeded cur_fallback
  cur_attempted="$(extract_summary_field "$line" "splice_attempted")"
  cur_succeeded="$(extract_summary_field "$line" "splice_succeeded")"
  cur_fallback="$(extract_summary_field "$line" "copy_fallback_total")"
  if [[ -z "$cur_attempted" || -z "$cur_succeeded" || -z "$cur_fallback" ]]; then
    echo "FAIL: [$stage] boundary_decisions line missing required fields"
    echo "  line: $line"
    return 1
  fi

  local baseline_bp current_bp min_allowed_bp
  baseline_bp="$(calc_success_bp "$baseline_succeeded" "$baseline_attempted")"
  current_bp="$(calc_success_bp "$cur_succeeded" "$cur_attempted")"
  min_allowed_bp="$(awk -v b="$baseline_bp" -v drop="$OMNI_BOUNDARY_ALERT_SPLICE_SUCCESS_DROP_PCT" 'BEGIN { printf "%.6f", b - (drop * 100.0) }')"

  if ! awk -v cur="$current_bp" -v min="$min_allowed_bp" 'BEGIN { exit !(cur + 0.0 >= min + 0.0) }'; then
    echo "FAIL: [$stage] splice success ratio dropped beyond threshold"
    echo "  baseline_bp=$baseline_bp current_bp=$current_bp min_allowed_bp=$min_allowed_bp"
    return 1
  fi

  local max_allowed_fallback
  max_allowed_fallback="$(awk -v b="$baseline_fallback" -v f="$OMNI_BOUNDARY_ALERT_COPY_FALLBACK_FACTOR" 'BEGIN { printf "%.6f", b * f }')"
  if ! awk -v cur="$cur_fallback" -v max="$max_allowed_fallback" 'BEGIN { exit !(cur + 0.0 <= max + 0.0) }'; then
    echo "FAIL: [$stage] copy fallback total exceeded threshold"
    echo "  baseline=$baseline_fallback current=$cur_fallback max_allowed=$max_allowed_fallback"
    return 1
  fi

  echo "OK: [$stage] boundary decision thresholds passed (fallback=$cur_fallback success_bp=$current_bp)"
  return 0
}

require_file "$normal_log"
require_file "$asan_log"
require_file "$baseline_file"

# shellcheck disable=SC1090
source "$baseline_file"

if [[ -z "$secondary_kind" ]]; then
  if grep -qa "Memcheck" "$asan_log"; then
    secondary_kind="valgrind"
  else
    secondary_kind="asan"
  fi
fi

check_stage "normal" "$normal_log" \
  "${BASELINE_NORMAL_SPLICE_ATTEMPTED:-0}" \
  "${BASELINE_NORMAL_SPLICE_SUCCEEDED:-0}" \
  "${BASELINE_NORMAL_COPY_FALLBACK_TOTAL:-0}"

if [[ "$secondary_kind" == "valgrind" ]]; then
  check_stage "valgrind" "$asan_log" \
    "${BASELINE_VALGRIND_SPLICE_ATTEMPTED:-0}" \
    "${BASELINE_VALGRIND_SPLICE_SUCCEEDED:-0}" \
    "${BASELINE_VALGRIND_COPY_FALLBACK_TOTAL:-0}"
else
  check_stage "asan" "$asan_log" \
    "${BASELINE_ASAN_SPLICE_ATTEMPTED:-0}" \
    "${BASELINE_ASAN_SPLICE_SUCCEEDED:-0}" \
    "${BASELINE_ASAN_COPY_FALLBACK_TOTAL:-0}"
fi

echo "Boundary decision threshold checks passed."
