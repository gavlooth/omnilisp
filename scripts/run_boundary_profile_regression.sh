#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

log_file="${1:-build/boundary_profile_memory_lifetime_bench.log}"
summary_json="${2:-build/boundary_profile_summary.json}"
: "${OMNI_BOUNDARY_PROFILE_SLICE:=memory-lifetime-bench}"
: "${OMNI_BOUNDARY_PROFILE_MONITOR_LOG:=build/docker_validation_boundary_profile_stats.log}"

ld_library_path="/workspace/build/container-libs:/usr/local/lib"
if [[ -n "${OMNI_DOCKER_TOOLCHAIN_ROOT:-}" ]]; then
  ld_library_path="/workspace/build/container-libs:/opt/omni-host-toolchain/lib:/usr/local/lib"
fi

mkdir -p \
  "$(dirname "$log_file")" \
  "$(dirname "$summary_json")" \
  "$(dirname "$OMNI_BOUNDARY_PROFILE_MONITOR_LOG")"

stdout_log="${log_file}.stdout"
stderr_log="${log_file}.stderr"

OMNI_VALIDATION_MONITOR_LOG="$OMNI_BOUNDARY_PROFILE_MONITOR_LOG" \
scripts/run_validation_container.sh env \
  LD_LIBRARY_PATH="$ld_library_path" \
  OMNI_TEST_SUMMARY="${OMNI_TEST_SUMMARY:-1}" \
  OMNI_TEST_QUIET="${OMNI_TEST_QUIET:-1}" \
  OMNI_SKIP_TLS_INTEGRATION="${OMNI_SKIP_TLS_INTEGRATION:-1}" \
  OMNI_BOUNDARY_VERBOSE_TELEMETRY="${OMNI_BOUNDARY_VERBOSE_TELEMETRY:-1}" \
  OMNI_BOUNDARY_TRAVERSAL_SUMMARY="${OMNI_BOUNDARY_TRAVERSAL_SUMMARY:-1}" \
  OMNI_BOUNDARY_BENCH="${OMNI_BOUNDARY_BENCH:-1}" \
  OMNI_LISP_TEST_SLICE="$OMNI_BOUNDARY_PROFILE_SLICE" \
  ./build/main --test-suite lisp \
  > "$stdout_log" \
  2> "$stderr_log"

cat "$stdout_log" "$stderr_log" | tee "$log_file"

scripts/parse_boundary_profile_summary.sh "$log_file" "$summary_json"
scripts/check_boundary_profile_thresholds.sh "$log_file"

echo "Boundary profile regression passed."
echo "  log: $log_file"
echo "  summary: $summary_json"
echo "  monitor: $OMNI_BOUNDARY_PROFILE_MONITOR_LOG"
