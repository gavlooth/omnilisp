#!/usr/bin/env bash

# Shared c3c wrapper with a repo-wide memory ceiling policy.
# Default: cap compiler memory to 30% of host RAM and run capped commands in Docker.

omni_default_cap_percent() {
  local percent="${OMNI_HARD_RESOURCE_CAP_PERCENT:-${OMNI_HARD_MEM_CAP_PERCENT:-30}}"
  if [[ ! "$percent" =~ ^[0-9]+$ ]] || (( percent <= 0 || percent > 100 )); then
    percent=30
  fi
  printf "%d\n" "$percent"
}

omni_default_validation_image() {
  printf "%s\n" "${OMNI_VALIDATION_DEFAULT_IMAGE:-omni-validation:2026-03-10}"
}

omni_detect_total_mem_mb() {
  if [[ -r /proc/meminfo ]]; then
    awk '/^MemTotal:/ { printf "%d\n", $2 / 1024; exit }' /proc/meminfo
    return 0
  fi

  if command -v sysctl >/dev/null 2>&1; then
    local total_bytes
    total_bytes="$(sysctl -n hw.memsize 2>/dev/null || true)"
    if [[ -z "$total_bytes" ]]; then
      total_bytes="$(sysctl -n hw.physmem 2>/dev/null || true)"
    fi
    if [[ -n "$total_bytes" && "$total_bytes" =~ ^[0-9]+$ ]]; then
      printf "%d\n" "$((total_bytes / 1024 / 1024))"
      return 0
    fi
  fi

  return 1
}

omni_c3_max_mem_mb() {
  if [[ -n "${OMNI_C3_MAX_MEM_MB:-}" ]]; then
    printf "%s\n" "${OMNI_C3_MAX_MEM_MB}"
    return 0
  fi

  local total_mem_mb
  if ! total_mem_mb="$(omni_detect_total_mem_mb)"; then
    return 1
  fi

  if [[ ! "$total_mem_mb" =~ ^[0-9]+$ ]] || (( total_mem_mb <= 0 )); then
    return 1
  fi

  local cap_mb
  cap_mb=$((total_mem_mb * 30 / 100))
  if (( cap_mb < 1 )); then
    cap_mb=1
  fi
  printf "%d\n" "$cap_mb"
}

omni_hard_cap_mb() {
  local total_mem_mb
  if ! total_mem_mb="$(omni_detect_total_mem_mb)"; then
    return 1
  fi

  if [[ ! "$total_mem_mb" =~ ^[0-9]+$ ]] || (( total_mem_mb <= 0 )); then
    return 1
  fi

  local percent
  percent="$(omni_default_cap_percent)"

  local max_cap_mb
  max_cap_mb=$((total_mem_mb * percent / 100))
  if (( max_cap_mb < 1 )); then
    max_cap_mb=1
  fi

  local requested_cap_mb="${OMNI_HARD_MEM_CAP_MB:-}"
  if [[ -n "$requested_cap_mb" ]]; then
    if [[ ! "$requested_cap_mb" =~ ^[0-9]+$ ]] || (( requested_cap_mb <= 0 )); then
      requested_cap_mb="$max_cap_mb"
    fi
    if (( requested_cap_mb > max_cap_mb )); then
      requested_cap_mb="$max_cap_mb"
    fi
    printf "%s\n" "$requested_cap_mb"
    return 0
  fi

  local cap_mb
  cap_mb=$((total_mem_mb * percent / 100))
  if (( cap_mb < 1 )); then
    cap_mb=1
  fi
  printf "%d\n" "$cap_mb"
}

omni_clamped_mem_cap_mb() {
  local requested_cap_mb="${1:-}"
  if [[ -n "$requested_cap_mb" ]]; then
    OMNI_HARD_MEM_CAP_MB="$requested_cap_mb" omni_hard_cap_mb
    return $?
  fi
  omni_hard_cap_mb
}

omni_detect_total_cpus() {
  if command -v nproc >/dev/null 2>&1; then
    local nproc_count
    nproc_count="$(nproc --all 2>/dev/null || true)"
    if [[ "$nproc_count" =~ ^[0-9]+$ ]] && (( nproc_count > 0 )); then
      printf "%d\n" "$nproc_count"
      return 0
    fi
  fi

  if command -v getconf >/dev/null 2>&1; then
    local getconf_count
    getconf_count="$(getconf _NPROCESSORS_ONLN 2>/dev/null || true)"
    if [[ "$getconf_count" =~ ^[0-9]+$ ]] && (( getconf_count > 0 )); then
      printf "%d\n" "$getconf_count"
      return 0
    fi
  fi

  if command -v sysctl >/dev/null 2>&1; then
    local sysctl_count
    sysctl_count="$(sysctl -n hw.ncpu 2>/dev/null || true)"
    if [[ "$sysctl_count" =~ ^[0-9]+$ ]] && (( sysctl_count > 0 )); then
      printf "%d\n" "$sysctl_count"
      return 0
    fi
  fi

  return 1
}

omni_default_cpus_cap() {
  local total_cpus
  if ! total_cpus="$(omni_detect_total_cpus)"; then
    printf "1\n"
    return 0
  fi

  local percent
  percent="$(omni_default_cap_percent)"

  awk -v total="$total_cpus" -v pct="$percent" '
    BEGIN {
      cap = (total * pct) / 100.0;
      if (cap < 1.0) cap = 1.0;
      if (cap > total) cap = total;
      printf "%.2f\n", cap;
    }
  '
}

omni_effective_docker_cpus() {
  local default_cap
  default_cap="$(omni_default_cpus_cap)"

  local requested="${OMNI_DOCKER_CPUS:-$default_cap}"
  if [[ ! "$requested" =~ ^[0-9]+([.][0-9]+)?$ ]]; then
    requested="$default_cap"
  fi

  awk -v req="$requested" -v cap="$default_cap" '
    BEGIN {
      value = req + 0.0;
      maxv = cap + 0.0;
      if (value <= 0.0) value = 1.0;
      if (value > maxv) value = maxv;
      printf "%.2f\n", value;
    }
  '
}

omni_docker_monitor_loop() {
  local container_name="$1"
  local log_file="$2"
  local interval="${OMNI_DOCKER_MONITOR_INTERVAL_SEC:-2}"
  if [[ ! "$interval" =~ ^[0-9]+$ ]] || (( interval <= 0 )); then
    interval=2
  fi

  while docker ps --format '{{.Names}}' 2>/dev/null | grep -Fxq "$container_name"; do
    local timestamp
    timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    local stats
    stats="$(docker stats --no-stream --format '{{.MemUsage}}|{{.MemPerc}}|{{.CPUPerc}}|{{.PIDs}}' "$container_name" 2>/dev/null || true)"
    if [[ -n "$stats" ]]; then
      printf "%s|%s\n" "$timestamp" "$stats" >> "$log_file"
    fi
    sleep "$interval"
  done
}

omni_timeout_run() {
  local timeout_sec="$1"
  shift

  if ! [[ "$timeout_sec" =~ ^[0-9]+$ ]] || (( timeout_sec <= 0 )); then
    "$@"
    return $?
  fi

  if timeout --help 2>&1 | grep -q -- "--signal"; then
    timeout --signal=TERM --kill-after=15 "${timeout_sec}s" "$@"
    return $?
  fi

  if timeout -t 1 true >/dev/null 2>&1; then
    timeout -t "$timeout_sec" "$@"
    return $?
  fi

  "$@"
}

omni_shell_split_words() {
  local input="${1:-}"
  if [[ -z "$input" ]]; then
    return 0
  fi

  python3 - "$input" <<'PY'
import shlex
import sys

for token in shlex.split(sys.argv[1]):
    print(token)
PY
}

omni_validation_lock_path() {
  printf "%s\n" "${OMNI_VALIDATION_BUILD_LOCK_PATH:-build/.validation-build.lock}"
}

omni_validation_lock_acquire() {
  if [[ "${OMNI_VALIDATION_BUILD_LOCK_HELD:-0}" == "1" ]]; then
    return 0
  fi

  if ! command -v flock >/dev/null 2>&1; then
    echo "omni_validation_lock_acquire: flock not found in PATH" >&2
    return 127
  fi

  local lock_path
  lock_path="$(omni_validation_lock_path)"
  mkdir -p "$(dirname "$lock_path")"

  local lock_fd
  exec {lock_fd}>"$lock_path"
  flock -x "$lock_fd"

  OMNI_VALIDATION_BUILD_LOCK_HELD=1
  OMNI_VALIDATION_BUILD_LOCK_FD="$lock_fd"
  export OMNI_VALIDATION_BUILD_LOCK_HELD OMNI_VALIDATION_BUILD_LOCK_FD
}

omni_validation_lock_release() {
  if [[ "${OMNI_VALIDATION_BUILD_LOCK_HELD:-0}" != "1" ]]; then
    return 0
  fi

  local lock_fd="${OMNI_VALIDATION_BUILD_LOCK_FD:-}"
  if [[ -n "$lock_fd" ]]; then
    eval "exec ${lock_fd}>&-"
  fi

  unset OMNI_VALIDATION_BUILD_LOCK_HELD OMNI_VALIDATION_BUILD_LOCK_FD
}

omni_run_with_docker_cap() {
  local cap_mb="$1"
  shift

  if ! command -v docker >/dev/null 2>&1; then
    echo "omni_run_with_docker_cap: docker not found in PATH" >&2
    return 127
  fi

  local default_image
  default_image="$(omni_default_validation_image)"
  local image="${OMNI_DOCKER_IMAGE:-$default_image}"
  local cpus
  cpus="$(omni_effective_docker_cpus)"
  local pids_limit="${OMNI_DOCKER_PIDS_LIMIT:-512}"
  local network_mode="${OMNI_DOCKER_NETWORK:-none}"
  local timeout_sec="${OMNI_DOCKER_TIMEOUT_SEC:-0}"
  local run_as_user="${OMNI_DOCKER_RUN_AS_USER:-1}"
  local monitor_enabled="${OMNI_DOCKER_MONITOR:-1}"
  local require_local_image="${OMNI_DOCKER_REQUIRE_LOCAL_IMAGE:-1}"
  local monitor_log="${OMNI_DOCKER_MONITOR_LOG:-build/docker_resource_stats.log}"
  local container_name="omni-cap-${USER:-user}-$$-$RANDOM"

  if [[ "$require_local_image" == "1" ]] && ! docker image inspect "$image" >/dev/null 2>&1; then
    echo "omni_run_with_docker_cap: docker image not found locally: ${image}" >&2
    echo "Build it with: OMNI_VALIDATION_IMAGE=${image} scripts/build_validation_image.sh" >&2
    echo "Or disable local-image preflight: OMNI_DOCKER_REQUIRE_LOCAL_IMAGE=0" >&2
    return 2
  fi

  mkdir -p "$(dirname "$monitor_log")"
  if [[ "$monitor_enabled" == "1" ]]; then
    if [[ ! -s "$monitor_log" ]]; then
      printf "# timestamp_utc|mem_usage|mem_percent|cpu_percent|pids\n" > "$monitor_log"
    fi
    printf "# run_start=%s container=%s image=%s mem_cap_mb=%s cpus=%s pids=%s timeout_sec=%s\n" \
      "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
      "$container_name" \
      "$image" \
      "$cap_mb" \
      "$cpus" \
      "$pids_limit" \
      "$timeout_sec" >> "$monitor_log"
  fi

  local -a docker_cmd=(
    docker run --rm --name "$container_name"
    --memory "${cap_mb}m"
    --memory-swap "${cap_mb}m"
    --cpus "$cpus"
    --pids-limit "$pids_limit"
    --network "$network_mode"
    --security-opt no-new-privileges
    --mount "type=bind,src=$PWD,dst=/workspace"
    -w /workspace
  )

  local toolchain_root="${OMNI_DOCKER_TOOLCHAIN_ROOT:-}"
  if [[ -n "$toolchain_root" ]]; then
    if [[ -d "$toolchain_root" ]]; then
      docker_cmd+=(
        --mount "type=bind,src=${toolchain_root},dst=/opt/omni-host-toolchain,readonly"
        -e "PATH=/opt/omni-host-toolchain/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        -e "LD_LIBRARY_PATH=/opt/omni-host-toolchain/lib"
      )
    else
      echo "omni_run_with_docker_cap: OMNI_DOCKER_TOOLCHAIN_ROOT not found: ${toolchain_root}" >&2
      return 2
    fi
  fi

  if [[ "$run_as_user" == "1" ]]; then
    docker_cmd+=(--user "$(id -u):$(id -g)")
  fi

  if [[ "${OMNI_VALIDATION_BUILD_LOCK_HELD:-0}" != "1" ]]; then
    omni_validation_lock_acquire
    local validation_lock_acquired=1
  else
    local validation_lock_acquired=0
  fi

  if [[ "${OMNI_VALIDATION_BUILD_LOCK_HELD:-0}" == "1" ]]; then
    docker_cmd+=(-e OMNI_VALIDATION_BUILD_LOCK_HELD=1)
    if [[ -n "${OMNI_VALIDATION_BUILD_LOCK_FD:-}" ]]; then
      docker_cmd+=(-e "OMNI_VALIDATION_BUILD_LOCK_FD=${OMNI_VALIDATION_BUILD_LOCK_FD}")
    fi
  fi

  if [[ -d /usr/include/boost ]]; then
    docker_cmd+=(--mount "type=bind,src=/usr/include/boost,dst=/usr/include/boost,readonly")
  fi

  if [[ -n "${OMNI_DOCKER_EXTRA_ARGS:-}" ]]; then
    local -a extra_args=()
    mapfile -t extra_args < <(omni_shell_split_words "$OMNI_DOCKER_EXTRA_ARGS")
    docker_cmd+=("${extra_args[@]}")
  fi

  docker_cmd+=("$image" "$@")

  local monitor_pid=""
  if [[ "$timeout_sec" =~ ^[0-9]+$ ]] && (( timeout_sec > 0 )); then
    omni_timeout_run "$timeout_sec" "${docker_cmd[@]}" &
  else
    "${docker_cmd[@]}" &
  fi
  local run_pid=$!

  if [[ "$monitor_enabled" == "1" ]]; then
    omni_docker_monitor_loop "$container_name" "$monitor_log" &
    monitor_pid=$!
  fi

  local run_rc=0
  wait "$run_pid"
  run_rc=$?

  if [[ -n "$monitor_pid" ]]; then
    wait "$monitor_pid" || true
  fi

  if [[ "${validation_lock_acquired:-0}" == "1" ]]; then
    omni_validation_lock_release
  fi

  return "$run_rc"
}

omni_run_with_hard_cap() {
  if [[ "${OMNI_HARD_MEM_CAP_ENABLED:-1}" != "1" ]]; then
    "$@"
    return $?
  fi

  local cap_mb
  if ! cap_mb="$(omni_hard_cap_mb 2>/dev/null)"; then
    "$@"
    return $?
  fi

  local method="${OMNI_HARD_MEM_CAP_METHOD:-docker}"
  if [[ "$method" == "systemd" || "$method" == "auto" ]]; then
    if command -v systemd-run >/dev/null 2>&1; then
      if systemd-run --user --scope --quiet true >/dev/null 2>&1; then
        systemd-run --user --scope --quiet -p "MemoryMax=${cap_mb}M" -- "$@"
        return $?
      fi
    fi
    if [[ "$method" == "systemd" ]]; then
      return 1
    fi
  fi

  if [[ "$method" == "docker" ]]; then
    omni_run_with_docker_cap "$cap_mb" "$@"
    return $?
  fi

  if [[ "$method" == "none" ]]; then
    "$@"
    return $?
  fi

  # Default/fallback hard cap using process address-space limits.
  (
    ulimit -S -v "$((cap_mb * 1024))"
    exec "$@"
  )
}

omni_c3() {
  local -a c3_global_flags
  local max_mem_mb

  if max_mem_mb="$(omni_c3_max_mem_mb 2>/dev/null)"; then
    c3_global_flags+=(--max-mem "$max_mem_mb")
  fi

  if [[ -n "${OMNI_C3_THREADS:-}" ]]; then
    c3_global_flags+=(--threads "${OMNI_C3_THREADS}")
  fi

  if [[ "${OMNI_C3_HARD_CAP_ENABLED:-0}" == "1" ]]; then
    omni_run_with_hard_cap c3c "${c3_global_flags[@]}" "$@"
    return $?
  fi
  c3c "${c3_global_flags[@]}" "$@"
}
