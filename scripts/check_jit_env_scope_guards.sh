#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

if [[ -z "${OMNI_VALIDATION_TOOLCHAIN_ROOT:-}" && -d /usr/local ]]; then
  export OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local
fi

if [[ -z "${OMNI_VALIDATION_EXTRA_ARGS:-}" && -e /usr/lib/libreplxx.so.0 ]]; then
  export OMNI_VALIDATION_EXTRA_ARGS="--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly"
fi

echo "==> Building Omni"
scripts/run_validation_container.sh c3c build

echo "==> Running JIT env/scope guards in the bounded validation container"
scripts/run_validation_container.sh \
  env \
  LD_LIBRARY_PATH=/usr/lib:/usr/local/lib \
  OMNI_TEST_QUIET=1 \
  OMNI_SKIP_TLS_INTEGRATION=1 \
  OMNI_LISP_TEST_SLICE=jit-policy \
  ./build/main --test-suite lisp

echo "OK: JIT env/scope guards passed."
