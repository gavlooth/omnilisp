#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."
source scripts/c3c_limits.sh

e2e_expected_diff_manifest="scripts/baselines/e2e_expected_diff.txt"
e2e_expected_diff_metadata="scripts/baselines/e2e_expected_diff.tsv"

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" == "1" ]]; then
  : "${OMNI_HARD_MEM_CAP_METHOD:=none}"
else
  : "${OMNI_HARD_MEM_CAP_METHOD:=docker}"
fi
: "${OMNI_HARD_MEM_CAP_PERCENT:=30}"

omni_e2e_append_validation_mount() {
  local mount_arg="$1"
  if [[ "$validation_extra" != *"$mount_arg"* ]]; then
    validation_extra="${validation_extra} --mount ${mount_arg}"
  fi
  if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" &&
        "$mount_arg" != "type=bind,src=/usr/include/boost,dst=/usr/include/boost,readonly" &&
        "${OMNI_DOCKER_EXTRA_ARGS:-}" != *"$mount_arg"* &&
        "$docker_validation_extra" != *"$mount_arg"* ]]; then
    docker_validation_extra="${docker_validation_extra} --mount ${mount_arg}"
  fi
}

if [[ "${1:-}" == "--self-test-validation-mount-bridge" ]]; then
  validation_extra=""
  docker_validation_extra=""
  OMNI_HARD_MEM_CAP_METHOD=docker
  OMNI_DOCKER_EXTRA_ARGS=""

  test_mount="type=bind,src=/tmp/omni-e2e-self-test,dst=/tmp/omni-e2e-self-test,readonly"
  omni_e2e_append_validation_mount "$test_mount"
  if [[ "$validation_extra" != *"$test_mount"* || "$docker_validation_extra" != *"$test_mount"* ]]; then
    echo "FAIL: validation mount bridge did not populate validation and docker args" >&2
    exit 1
  fi

  boost_mount="type=bind,src=/usr/include/boost,dst=/usr/include/boost,readonly"
  omni_e2e_append_validation_mount "$boost_mount"
  if [[ "$validation_extra" != *"$boost_mount"* || "$docker_validation_extra" == *"$boost_mount"* ]]; then
    echo "FAIL: validation mount bridge did not keep boost out of docker extras" >&2
    exit 1
  fi

  existing_mount="type=bind,src=/tmp/omni-e2e-existing,dst=/tmp/omni-e2e-existing,readonly"
  OMNI_DOCKER_EXTRA_ARGS="--mount ${existing_mount}"
  omni_e2e_append_validation_mount "$existing_mount"
  if [[ "$docker_validation_extra" == *"$existing_mount"* ]]; then
    echo "FAIL: validation mount bridge duplicated existing docker mount" >&2
    exit 1
  fi

  echo "OK: validation mount bridge self-test passed."
  exit 0
fi

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" != "1" && "${OMNI_HARD_MEM_CAP_METHOD}" != "docker" ]]; then
  echo "run_e2e.sh requires Docker-bound execution outside validation containers." >&2
  echo "Set OMNI_HARD_MEM_CAP_METHOD=docker (default) or run via scripts/run_validation_container.sh." >&2
  exit 2
fi

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" != "1" ]]; then
  validation_extra="${OMNI_VALIDATION_EXTRA_ARGS:-}"
  docker_validation_extra=""

  if [[ -f /usr/include/yyjson.h ]]; then
    omni_e2e_append_validation_mount "type=bind,src=/usr/include/yyjson.h,dst=/usr/include/yyjson.h,readonly"
  fi
  for header in /usr/include/bearssl*.h; do
    if [[ -f "$header" ]]; then
      omni_e2e_append_validation_mount "type=bind,src=${header},dst=${header},readonly"
    fi
  done
  if [[ -f /usr/include/uv.h ]]; then
    omni_e2e_append_validation_mount "type=bind,src=/usr/include/uv.h,dst=/usr/include/uv.h,readonly"
  fi
  if [[ -d /usr/include/uv ]]; then
    omni_e2e_append_validation_mount "type=bind,src=/usr/include/uv,dst=/usr/include/uv,readonly"
  fi
  if [[ -d /usr/include/boost ]]; then
    omni_e2e_append_validation_mount "type=bind,src=/usr/include/boost,dst=/usr/include/boost,readonly"
  fi
  if [[ -f /usr/include/ffi.h ]]; then
    omni_e2e_append_validation_mount "type=bind,src=/usr/include/ffi.h,dst=/usr/include/ffi.h,readonly"
  fi
  if [[ -e /usr/lib/libreplxx.so.0 ]]; then
    omni_e2e_append_validation_mount "type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly"
  fi
  if [[ -n "$validation_extra" ]]; then
    # Trim leading whitespace from concatenation flow above.
    # shellcheck disable=SC2001
    validation_extra="$(echo "$validation_extra" | sed 's/^ *//')"
    export OMNI_VALIDATION_EXTRA_ARGS="$validation_extra"
    if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" && -n "$docker_validation_extra" ]]; then
      docker_validation_extra="$(echo "$docker_validation_extra" | sed 's/^ *//; s/ *$//')"
      if [[ -n "$docker_validation_extra" && -n "${OMNI_DOCKER_EXTRA_ARGS:-}" ]]; then
        export OMNI_DOCKER_EXTRA_ARGS="${OMNI_DOCKER_EXTRA_ARGS} ${docker_validation_extra}"
      elif [[ -n "$docker_validation_extra" ]]; then
        export OMNI_DOCKER_EXTRA_ARGS="$docker_validation_extra"
      fi
    fi
  fi
fi

if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" ]] && ! command -v docker >/dev/null 2>&1; then
  echo "run_e2e.sh requires docker in PATH when OMNI_HARD_MEM_CAP_METHOD=docker." >&2
  exit 127
fi

if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" ]]; then
  : "${OMNI_C3_HARD_CAP_ENABLED:=1}"
fi

runtime_ld_library_path="${OMNI_RUNTIME_TOOLCHAIN_LIB_PATH:-/usr/local/lib}"
if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" && -n "${OMNI_DOCKER_TOOLCHAIN_ROOT:-}" ]]; then
  runtime_ld_library_path="/opt/omni-host-toolchain/lib"
fi

echo "=== Stage 1: Building main binary ==="
omni_c3 build

echo ""
echo "=== Stage 2: Generating e2e test files ==="
set +e
stage2_output="$(omni_run_with_hard_cap env "LD_LIBRARY_PATH=${runtime_ld_library_path}" ./build/main --gen-e2e)"
stage2_status=$?
set -e
printf '%s\n' "$stage2_output"
if [[ "$stage2_status" -ne 0 ]]; then
    echo "E2E generation failed before producing generated test files." >&2
    exit "$stage2_status"
fi
if [[ "$stage2_output" != *"=== Generated 431 e2e tests ==="* ]]; then
    echo "E2E generation contract failed: expected exactly 431 generated tests." >&2
    exit 1
fi

echo ""
echo "=== Stage 3: Building e2e test binary ==="
# Mirroring the AOT build command from src/entry.c3
omni_run_with_hard_cap ./scripts/build_omni_chelpers.sh

stage3_compile_sources=(
  src/main*.c3
  src/entry_*.c3
  src/scope_region*.c3
  src/stack_engine*.c3
  src/ffi_bindings.c3
  src/lisp/*.c3
  src/pika/*.c3
  build/e2e_test.c3
)

omni_run_with_hard_cap ./scripts/check_e2e_baseline_policy.sh --stage3-source-parity

omni_c3 compile \
  "${stage3_compile_sources[@]}" \
  -o build/e2e_test \
  -L build -L "${runtime_ld_library_path}" -L deps/lib \
  -l omni_chelpers -l lightning -l ffi -l dl -l m -l replxx -l stdc++ \
  -l utf8proc -l deflate -l yyjson -l uv -l bearssl -l lmdb

if [[ "${OMNI_E2E_COMPILE_ONLY:-0}" == "1" ]]; then
    echo "E2E compile gate passed (generation + compile only)."
    exit 0
fi

echo ""
echo "=== Stage 4: Running e2e tests ==="
set +e
omni_run_with_hard_cap env "LD_LIBRARY_PATH=${runtime_ld_library_path}" ./build/e2e_test > build/e2e_actual.txt
stage4_status=$?
set -e
if [[ "$stage4_status" -ne 0 ]]; then
    echo "E2E test binary failed with status ${stage4_status}." >&2
    echo "Captured output: build/e2e_actual.txt" >&2
    if [[ -s build/e2e_actual.txt ]]; then
        echo "" >&2
        echo "=== e2e_actual.txt head ===" >&2
        head -20 build/e2e_actual.txt >&2
        echo "" >&2
        echo "=== e2e_actual.txt tail ===" >&2
        tail -20 build/e2e_actual.txt >&2
    else
        echo "build/e2e_actual.txt is empty." >&2
    fi
    exit "$stage4_status"
fi

echo ""
echo "=== Stage 5: Comparing output ==="
if diff build/e2e_expected.txt build/e2e_actual.txt > build/e2e_diff.txt 2>&1; then
    TESTS=$(wc -l < build/e2e_expected.txt)
    if [[ "$TESTS" -ne 431 ]]; then
        echo "E2E generation contract failed: expected 431 output rows, got $TESTS." >&2
        exit 1
    fi
    echo "ALL $TESTS e2e compiler tests passed!"
    rm -f build/e2e_diff.txt
elif [[ -f "${e2e_expected_diff_manifest}" ]] && cmp -s build/e2e_diff.txt "${e2e_expected_diff_manifest}"; then
    ./scripts/check_e2e_baseline_policy.sh
    tracked_rows="unknown"
    if [[ -f "${e2e_expected_diff_metadata}" ]]; then
        tracked_rows=$(awk 'BEGIN { count = 0 } NR > 1 && $0 !~ /^[[:space:]]*$/ { count++ } END { print count }' "${e2e_expected_diff_metadata}")
    fi
    echo "E2E baseline matched expected diff manifest."
    echo "Manifest: ${e2e_expected_diff_manifest}"
    echo "Ownership: ${e2e_expected_diff_metadata}"
    echo "Tracked rows: ${tracked_rows}"
    echo "Legacy mismatches remain, but baseline drift is clean."
else
    echo "E2E FAILURES (see build/e2e_diff.txt):"
    head -30 build/e2e_diff.txt
    if [[ -f "${e2e_expected_diff_manifest}" ]]; then
        echo ""
        echo "=== Baseline Drift vs Expected Manifest ==="
        diff -u "${e2e_expected_diff_manifest}" build/e2e_diff.txt | head -60 || true
    fi
    exit 1
fi
