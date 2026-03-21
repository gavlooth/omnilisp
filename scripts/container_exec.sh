#!/usr/bin/env bash
set -euo pipefail

cd /workspace
export OMNI_IN_VALIDATION_CONTAINER=1

toolchain_root="/opt/omni-host-toolchain"
if [[ -d "${toolchain_root}/bin" ]]; then
  export PATH="${toolchain_root}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
fi
if [[ -d "${toolchain_root}/lib" ]]; then
  export LD_LIBRARY_PATH="${toolchain_root}/lib"
fi

# Avoid recursive container-on-container invocation unless explicitly overridden.
: "${OMNI_HARD_MEM_CAP_METHOD:=none}"
: "${OMNI_VALIDATION_STACK_KB:=16384}"

if [[ "$OMNI_VALIDATION_STACK_KB" =~ ^[0-9]+$ ]] && (( OMNI_VALIDATION_STACK_KB > 0 )); then
  ulimit -S -s "$OMNI_VALIDATION_STACK_KB" || true
fi

if [[ $# -eq 0 ]]; then
  set -- scripts/run_global_gates.sh
fi

exec "$@"
