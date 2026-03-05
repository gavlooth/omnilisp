#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

# Boundary-hardening profile defaults.
: "${OMNI_BOUNDARY_ENABLE_FIBER_TEMP:=1}"
: "${OMNI_BOUNDARY_ENABLE_AFFINITY_HARNESS:=1}"
: "${OMNI_BOUNDARY_QUIET:=1}"
: "${OMNI_BOUNDARY_SUMMARY:=1}"

base_env=(
  "LD_LIBRARY_PATH=/usr/local/lib"
)

if [[ "$OMNI_BOUNDARY_ENABLE_FIBER_TEMP" == "1" ]]; then
  base_env+=("OMNI_FIBER_TEMP=1")
fi
if [[ "$OMNI_BOUNDARY_ENABLE_AFFINITY_HARNESS" == "1" ]]; then
  base_env+=("OMNI_STACK_AFFINITY_HARNESS=1")
fi
if [[ "$OMNI_BOUNDARY_QUIET" == "1" ]]; then
  base_env+=("OMNI_TEST_QUIET=1")
fi
if [[ "$OMNI_BOUNDARY_SUMMARY" == "1" ]]; then
  base_env+=("OMNI_TEST_SUMMARY=1")
fi

echo "=== Boundary Hardening: Stage 1 (normal build) ==="
c3c build

echo ""
echo "=== Boundary Hardening: Stage 2 (normal run) ==="
env "${base_env[@]}" ./build/main

echo ""
echo "=== Boundary Hardening: Stage 3 (ASAN build) ==="
c3c clean
c3c build --sanitize=address

echo ""
echo "=== Boundary Hardening: Stage 4 (ASAN run) ==="
env \
  "${base_env[@]}" \
  "ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1" \
  ./build/main

echo ""
echo "Boundary hardening profile passed."
