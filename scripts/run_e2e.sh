#!/bin/bash
set -e

cd "$(dirname "$0")/.."
source scripts/c3c_limits.sh

: "${OMNI_HARD_MEM_CAP_METHOD:=docker}"
: "${OMNI_HARD_MEM_CAP_PERCENT:=30}"

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" != "1" && "${OMNI_HARD_MEM_CAP_METHOD}" != "docker" ]]; then
  echo "run_e2e.sh requires Docker-bound execution outside validation containers." >&2
  echo "Set OMNI_HARD_MEM_CAP_METHOD=docker (default) or run via scripts/run_validation_container.sh." >&2
  exit 2
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
else
    echo "E2E FAILURES (see build/e2e_diff.txt):"
    head -30 build/e2e_diff.txt
    exit 1
fi
