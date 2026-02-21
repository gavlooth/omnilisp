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
c3c compile \
  src/main.c3 src/context.c3 src/continuation.c3 src/delimited.c3 src/ghost_index.c3 \
  src/ffi_bindings.c3 \
  src/test_ghost_lookup.c3 src/test_arena_bug.c3 src/test_alignment.c3 \
  src/test_sparse_set.c3 src/test_typeid.c3 src/test_destructor_registry.c3 \
  src/test_slot_table.c3 src/test_region.c3 \
  src/lisp/eval.c3 src/lisp/parser.c3 src/lisp/value.c3 \
  src/lisp/jit.c3 src/lisp/compiler.c3 src/lisp/runtime.c3 \
  build/e2e_test.c3 \
  -o build/e2e_test -l lightning -l readline -l dl -l m -L /usr/local/lib -L build

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
