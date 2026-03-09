#!/bin/bash
set -e

cd "$(dirname "$0")/.."

echo "=== Stage 1: Building main binary ==="
c3c build

echo ""
echo "=== Stage 2: Generating e2e test files ==="
LD_LIBRARY_PATH=/usr/local/lib ./build/main --gen-e2e

echo ""
echo "=== Stage 3: Building e2e test binary ==="
# Mirroring the AOT build command from src/entry.c3
./scripts/build_omni_chelpers.sh

c3c compile \
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
LD_LIBRARY_PATH=/usr/local/lib ./build/e2e_test > build/e2e_actual.txt

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
