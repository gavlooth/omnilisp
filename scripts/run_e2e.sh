#!/bin/bash
set -e

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

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" != "1" && "${OMNI_HARD_MEM_CAP_METHOD}" != "docker" ]]; then
  echo "run_e2e.sh requires Docker-bound execution outside validation containers." >&2
  echo "Set OMNI_HARD_MEM_CAP_METHOD=docker (default) or run via scripts/run_validation_container.sh." >&2
  exit 2
fi

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" != "1" ]]; then
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
fi

if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" ]] && ! command -v docker >/dev/null 2>&1; then
  echo "run_e2e.sh requires docker in PATH when OMNI_HARD_MEM_CAP_METHOD=docker." >&2
  exit 127
fi

if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" ]]; then
  : "${OMNI_C3_HARD_CAP_ENABLED:=1}"
fi

runtime_ld_library_path="/usr/local/lib"
if [[ "${OMNI_HARD_MEM_CAP_METHOD}" == "docker" && -n "${OMNI_DOCKER_TOOLCHAIN_ROOT:-}" ]]; then
  runtime_ld_library_path="/opt/omni-host-toolchain/lib"
fi

echo "=== Stage 1: Building main binary ==="
omni_c3 build

echo ""
echo "=== Stage 2: Generating e2e test files ==="
omni_run_with_hard_cap env "LD_LIBRARY_PATH=${runtime_ld_library_path}" ./build/main --gen-e2e

echo ""
echo "=== Stage 3: Building e2e test binary ==="
# Mirroring the AOT build command from src/entry.c3
omni_run_with_hard_cap ./scripts/build_omni_chelpers.sh

omni_c3 compile \
  src/main*.c3 src/scope_region*.c3 src/stack_engine*.c3 src/ffi_bindings.c3 \
  src/lisp/*.c3 src/pika/*.c3 \
  build/e2e_test.c3 \
  -o build/e2e_test \
  -L build -L /usr/local/lib -L deps/lib \
  -l omni_chelpers -l lightning -l ffi -l dl -l m -l replxx -l stdc++ \
  -l utf8proc -l deflate -l yyjson -l uv -l bearssl -l lmdb

if [[ "${OMNI_E2E_COMPILE_ONLY:-0}" == "1" ]]; then
    echo "E2E compile gate passed (generation + compile only)."
    exit 0
fi

echo ""
echo "=== Stage 4: Running e2e tests ==="
omni_run_with_hard_cap env "LD_LIBRARY_PATH=${runtime_ld_library_path}" ./build/e2e_test > build/e2e_actual.txt

echo ""
echo "=== Stage 5: Comparing output ==="
if diff build/e2e_expected.txt build/e2e_actual.txt > build/e2e_diff.txt 2>&1; then
    TESTS=$(wc -l < build/e2e_expected.txt)
    echo "ALL $TESTS e2e compiler tests passed!"
    rm -f build/e2e_diff.txt
elif [[ -f "${e2e_expected_diff_manifest}" ]] && cmp -s build/e2e_diff.txt "${e2e_expected_diff_manifest}"; then
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
