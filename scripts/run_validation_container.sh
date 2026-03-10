#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
source scripts/c3c_limits.sh

if ! command -v docker >/dev/null 2>&1; then
  echo "docker not found in PATH" >&2
  exit 127
fi

default_validation_image="$(omni_default_validation_image)"
: "${OMNI_VALIDATION_IMAGE:=${OMNI_DOCKER_IMAGE:-$default_validation_image}}"
: "${OMNI_HARD_MEM_CAP_PERCENT:=30}"
: "${OMNI_VALIDATION_CPUS:=}"
: "${OMNI_VALIDATION_PIDS_LIMIT:=512}"
: "${OMNI_VALIDATION_NETWORK:=none}"
: "${OMNI_VALIDATION_TIMEOUT_SEC:=0}"
: "${OMNI_VALIDATION_RUN_AS_USER:=1}"
: "${OMNI_VALIDATION_REQUIRE_LOCAL_IMAGE:=1}"
: "${OMNI_VALIDATION_MONITOR:=1}"
: "${OMNI_VALIDATION_MONITOR_INTERVAL_SEC:=2}"
: "${OMNI_VALIDATION_MONITOR_LOG:=build/docker_validation_container_stats.log}"
: "${OMNI_VALIDATION_TOOLCHAIN_ROOT:=}"

if [[ -z "$OMNI_VALIDATION_CPUS" ]]; then
  OMNI_VALIDATION_CPUS="$(omni_default_cpus_cap)"
fi

validation_cpu_cap="$(omni_default_cpus_cap)"
OMNI_VALIDATION_CPUS="$(
  awk -v req="$OMNI_VALIDATION_CPUS" -v cap="$validation_cpu_cap" '
    BEGIN {
      if (req !~ /^[0-9]+([.][0-9]+)?$/) req = cap;
      value = req + 0.0;
      maxv = cap + 0.0;
      if (value <= 0.0) value = 1.0;
      if (value > maxv) value = maxv;
      printf "%.2f\n", value;
    }
  '
)"

validation_mem_mb="${OMNI_VALIDATION_MEM_MB:-}"
if [[ -z "$validation_mem_mb" ]]; then
  validation_mem_mb="$(omni_hard_cap_mb 2>/dev/null || true)"
fi
if [[ -z "$validation_mem_mb" ]]; then
  echo "failed to resolve validation memory cap; set OMNI_VALIDATION_MEM_MB" >&2
  exit 2
fi

if [[ "$#" -eq 0 ]]; then
  set -- scripts/run_global_gates.sh
fi
validation_cmd="$(printf "%q " "$@")"
validation_cmd="${validation_cmd% }"

if [[ "$OMNI_VALIDATION_REQUIRE_LOCAL_IMAGE" == "1" ]] && ! docker image inspect "$OMNI_VALIDATION_IMAGE" >/dev/null 2>&1; then
  echo "validation image not found locally: ${OMNI_VALIDATION_IMAGE}" >&2
  echo "Build it with: OMNI_VALIDATION_IMAGE=${OMNI_VALIDATION_IMAGE} scripts/build_validation_image.sh" >&2
  echo "Or disable local-image preflight: OMNI_VALIDATION_REQUIRE_LOCAL_IMAGE=0" >&2
  exit 2
fi

mkdir -p "$(dirname "$OMNI_VALIDATION_MONITOR_LOG")"
container_name="${OMNI_VALIDATION_CONTAINER_NAME:-omni-validation-${USER:-user}-$$-$RANDOM}"

cleanup() {
  docker rm -f "$container_name" >/dev/null 2>&1 || true
}
trap cleanup EXIT

if [[ "$OMNI_VALIDATION_MONITOR" == "1" ]]; then
  if [[ ! -s "$OMNI_VALIDATION_MONITOR_LOG" ]]; then
    printf "# timestamp_utc|mem_usage|mem_percent|cpu_percent|pids\n" > "$OMNI_VALIDATION_MONITOR_LOG"
  fi
  printf "# run_start=%s container=%s image=%s mem_cap_mb=%s cpus=%s pids=%s timeout_sec=%s cmd=%s\n" \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
    "$container_name" \
    "$OMNI_VALIDATION_IMAGE" \
    "$validation_mem_mb" \
    "$OMNI_VALIDATION_CPUS" \
    "$OMNI_VALIDATION_PIDS_LIMIT" \
    "$OMNI_VALIDATION_TIMEOUT_SEC" \
    "$validation_cmd" >> "$OMNI_VALIDATION_MONITOR_LOG"
fi

run_cmd=(
  docker run --rm --name "$container_name"
  --entrypoint /workspace/scripts/container_exec.sh
  --memory "${validation_mem_mb}m"
  --memory-swap "${validation_mem_mb}m"
  --cpus "$OMNI_VALIDATION_CPUS"
  --pids-limit "$OMNI_VALIDATION_PIDS_LIMIT"
  --network "$OMNI_VALIDATION_NETWORK"
  --security-opt no-new-privileges
  --mount "type=bind,src=$PWD,dst=/workspace"
  -w /workspace
)

if [[ "$OMNI_VALIDATION_RUN_AS_USER" == "1" ]]; then
  run_cmd+=(--user "$(id -u):$(id -g)")
fi

if [[ -n "$OMNI_VALIDATION_TOOLCHAIN_ROOT" ]]; then
  if [[ -d "$OMNI_VALIDATION_TOOLCHAIN_ROOT" ]]; then
    run_cmd+=(--mount "type=bind,src=${OMNI_VALIDATION_TOOLCHAIN_ROOT},dst=/opt/omni-host-toolchain,readonly")
  else
    echo "OMNI_VALIDATION_TOOLCHAIN_ROOT does not exist: ${OMNI_VALIDATION_TOOLCHAIN_ROOT}" >&2
    exit 2
  fi
fi

if [[ -n "${OMNI_VALIDATION_EXTRA_ARGS:-}" ]]; then
  # shellcheck disable=SC2206
  extra_args=(${OMNI_VALIDATION_EXTRA_ARGS})
  run_cmd+=("${extra_args[@]}")
fi

run_cmd+=("$OMNI_VALIDATION_IMAGE" "$@")

monitor_loop() {
  while docker ps --format '{{.Names}}' 2>/dev/null | grep -Fxq "$container_name"; do
    ts="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    stats="$(docker stats --no-stream --format '{{.MemUsage}}|{{.MemPerc}}|{{.CPUPerc}}|{{.PIDs}}' "$container_name" 2>/dev/null || true)"
    if [[ -n "$stats" ]]; then
      printf "%s|%s\n" "$ts" "$stats" >> "$OMNI_VALIDATION_MONITOR_LOG"
    fi
    sleep "$OMNI_VALIDATION_MONITOR_INTERVAL_SEC"
  done
}

if [[ "$OMNI_VALIDATION_TIMEOUT_SEC" =~ ^[0-9]+$ ]] && (( OMNI_VALIDATION_TIMEOUT_SEC > 0 )); then
  omni_timeout_run "$OMNI_VALIDATION_TIMEOUT_SEC" "${run_cmd[@]}" &
else
  "${run_cmd[@]}" &
fi
run_pid=$!

monitor_pid=""
if [[ "$OMNI_VALIDATION_MONITOR" == "1" ]]; then
  monitor_loop &
  monitor_pid=$!
fi

set +e
wait "$run_pid"
run_rc=$?
set -e

if [[ -n "$monitor_pid" ]]; then
  wait "$monitor_pid" || true
fi

exit "$run_rc"
