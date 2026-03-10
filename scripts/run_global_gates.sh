#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
source scripts/c3c_limits.sh

# Memory-aware global gate runner.
# - build/asan-build use omni_c3 (30% host RAM cap by default)
# - test phase can split suites into separate processes to reduce peak RSS

: "${OMNI_GLOBAL_GATES_SPLIT:=1}"
: "${OMNI_GLOBAL_GATES_SUITES:=stack scope lisp}"
: "${OMNI_GLOBAL_GATES_LISP_SLICES:=basic memory-lifetime-smoke memory-stress list-closure arithmetic-comparison string-type diagnostics jit-policy advanced escape-scope limit-busting tco-recycling closure-lifecycle pika unicode compression data-format json async reader-dispatch schema deduce scheduler http atomic compiler}"
: "${OMNI_GLOBAL_GATES_INCLUDE_LIFETIME_SOAK:=0}"
: "${OMNI_GLOBAL_GATES_TEST_QUIET:=1}"
: "${OMNI_GLOBAL_GATES_TEST_SUMMARY:=1}"
: "${OMNI_GLOBAL_GATES_SKIP_TLS_INTEGRATION:=1}"
: "${OMNI_GLOBAL_GATES_ASAN_OPTIONS:=detect_leaks=0:halt_on_error=1:abort_on_error=1:quarantine_size_mb=64:thread_local_quarantine_size_kb=256:malloc_context_size=5}"
: "${OMNI_HARD_MEM_CAP_METHOD:=docker}"
: "${OMNI_HARD_MEM_CAP_PERCENT:=30}"

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" != "1" && "$OMNI_HARD_MEM_CAP_METHOD" != "docker" ]]; then
  echo "run_global_gates.sh requires Docker-bound execution outside validation containers." >&2
  echo "Set OMNI_HARD_MEM_CAP_METHOD=docker (default) or run via scripts/run_validation_container.sh." >&2
  exit 2
fi

if [[ "$OMNI_HARD_MEM_CAP_METHOD" == "docker" ]] && ! command -v docker >/dev/null 2>&1; then
  echo "run_global_gates.sh requires docker in PATH when OMNI_HARD_MEM_CAP_METHOD=docker." >&2
  exit 127
fi

if [[ "$OMNI_HARD_MEM_CAP_METHOD" == "docker" ]]; then
  : "${OMNI_C3_HARD_CAP_ENABLED:=1}"
fi

hard_cap_mb=""
if hard_cap_mb="$(omni_hard_cap_mb 2>/dev/null)"; then
  :
else
  hard_cap_mb=""
fi

runtime_ld_library_path="/usr/local/lib"
if [[ "$OMNI_HARD_MEM_CAP_METHOD" == "docker" && -n "${OMNI_DOCKER_TOOLCHAIN_ROOT:-}" ]]; then
  runtime_ld_library_path="/opt/omni-host-toolchain/lib"
fi

base_env=(
  "LD_LIBRARY_PATH=${runtime_ld_library_path}"
)

if [[ "$OMNI_GLOBAL_GATES_TEST_QUIET" == "1" ]]; then
  base_env+=("OMNI_TEST_QUIET=1")
fi
if [[ "$OMNI_GLOBAL_GATES_TEST_SUMMARY" == "1" ]]; then
  base_env+=("OMNI_TEST_SUMMARY=1")
fi
if [[ "$OMNI_GLOBAL_GATES_SKIP_TLS_INTEGRATION" == "1" ]]; then
  base_env+=("OMNI_SKIP_TLS_INTEGRATION=1")
fi

run_suite() {
  local mode="$1"
  local suite="$2"
  local -a run_env=("${base_env[@]}")
  if [[ "$mode" == "asan" ]]; then
    local asan_opts="${OMNI_GLOBAL_GATES_ASAN_OPTIONS}"
    if [[ -n "$hard_cap_mb" && "$asan_opts" != *"hard_rss_limit_mb="* ]]; then
      asan_opts="${asan_opts}:hard_rss_limit_mb=${hard_cap_mb}"
    fi
    run_env+=("ASAN_OPTIONS=${asan_opts}")
  fi
  echo "=== ${mode^^} test suite: ${suite} ==="
  if [[ "$suite" == "lisp" && "$OMNI_GLOBAL_GATES_SPLIT" == "1" ]]; then
    local lisp_slices="$OMNI_GLOBAL_GATES_LISP_SLICES"
    if [[ "$OMNI_GLOBAL_GATES_INCLUDE_LIFETIME_SOAK" == "1" ]]; then
      case " ${lisp_slices} " in
        *" memory-lifetime-soak "*) ;;
        *) lisp_slices="${lisp_slices} memory-lifetime-soak" ;;
      esac
    fi
    local slice
    for slice in $lisp_slices; do
      echo "=== ${mode^^} lisp slice: ${slice} ==="
      if [[ "$mode" == "asan" ]]; then
        if [[ "$OMNI_HARD_MEM_CAP_METHOD" == "docker" ]]; then
          omni_run_with_hard_cap env "${run_env[@]}" "OMNI_LISP_TEST_SLICE=${slice}" ./build/main --test-suite "$suite"
        else
          env "${run_env[@]}" "OMNI_LISP_TEST_SLICE=${slice}" ./build/main --test-suite "$suite"
        fi
      else
        omni_run_with_hard_cap env "${run_env[@]}" "OMNI_LISP_TEST_SLICE=${slice}" ./build/main --test-suite "$suite"
      fi
    done
    return 0
  fi

  if [[ "$mode" == "asan" ]]; then
    if [[ "$OMNI_HARD_MEM_CAP_METHOD" == "docker" ]]; then
      omni_run_with_hard_cap env "${run_env[@]}" ./build/main --test-suite "$suite"
    else
      env "${run_env[@]}" ./build/main --test-suite "$suite"
    fi
  else
    omni_run_with_hard_cap env "${run_env[@]}" ./build/main --test-suite "$suite"
  fi
}

run_tests() {
  local mode="$1"
  if [[ "$OMNI_GLOBAL_GATES_SPLIT" == "1" ]]; then
    local suite
    for suite in $OMNI_GLOBAL_GATES_SUITES; do
      run_suite "$mode" "$suite"
    done
    return 0
  fi

  local -a run_env=("${base_env[@]}")
  if [[ "$mode" == "asan" ]]; then
    local asan_opts="${OMNI_GLOBAL_GATES_ASAN_OPTIONS}"
    if [[ -n "$hard_cap_mb" && "$asan_opts" != *"hard_rss_limit_mb="* ]]; then
      asan_opts="${asan_opts}:hard_rss_limit_mb=${hard_cap_mb}"
    fi
    run_env+=("ASAN_OPTIONS=${asan_opts}")
  fi
  echo "=== ${mode^^} full test run ==="
  if [[ "$mode" == "asan" ]]; then
    if [[ "$OMNI_HARD_MEM_CAP_METHOD" == "docker" ]]; then
      omni_run_with_hard_cap env "${run_env[@]}" ./build/main
    else
      env "${run_env[@]}" ./build/main
    fi
  else
    omni_run_with_hard_cap env "${run_env[@]}" ./build/main
  fi
}

resolved_percent="$(omni_default_cap_percent)"
echo "Hard memory cap enabled: OMNI_HARD_MEM_CAP_ENABLED=${OMNI_HARD_MEM_CAP_ENABLED:-1} method=${OMNI_HARD_MEM_CAP_METHOD:-docker} percent=${resolved_percent} mb_override=${OMNI_HARD_MEM_CAP_MB:-auto} resolved_mb=${hard_cap_mb:-unknown}"
echo "c3c compiler max-mem cap: OMNI_C3_MAX_MEM_MB=${OMNI_C3_MAX_MEM_MB:-auto} c3_hard_cap=${OMNI_C3_HARD_CAP_ENABLED:-0}"
if [[ "$OMNI_HARD_MEM_CAP_METHOD" == "docker" ]]; then
  effective_docker_cpus="$(omni_effective_docker_cpus)"
  docker_default_image="$(omni_default_validation_image)"
  echo "docker cap profile: image=${OMNI_DOCKER_IMAGE:-$docker_default_image} cpus=${effective_docker_cpus} pids=${OMNI_DOCKER_PIDS_LIMIT:-512} timeout=${OMNI_DOCKER_TIMEOUT_SEC:-0}s monitor=${OMNI_DOCKER_MONITOR:-1}"
fi
echo "lisp slices: ${OMNI_GLOBAL_GATES_LISP_SLICES} include_lifetime_soak=${OMNI_GLOBAL_GATES_INCLUDE_LIFETIME_SOAK}"
echo ""

echo "=== Stage 1: normal build ==="
omni_c3 build

echo ""
echo "=== Stage 2: normal tests ==="
run_tests "normal"

echo ""
echo "=== Stage 3: ASAN build ==="
omni_c3 build --sanitize=address

echo ""
echo "=== Stage 4: ASAN tests ==="
run_tests "asan"

echo ""
echo "Global gates passed."
