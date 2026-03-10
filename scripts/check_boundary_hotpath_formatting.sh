#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

declare -a hotpath_files=(
  "src/lisp/eval_boundary_commit_flow.c3"
  "src/lisp/jit_jit_eval_scopes.c3"
  "src/lisp/eval_env_copy.c3"
  "src/lisp/eval_boundary_api.c3"
)

# Formatting calls are forbidden on hot boundary paths.
hits="$(rg -n "io::(e?printfn|printn)" "${hotpath_files[@]}" || true)"
if [[ -z "$hits" ]]; then
  echo "OK: no boundary hot-path formatting calls found."
  exit 0
fi

# Allowed cold diagnostic exception:
# - copy_env invariant failure reporter is @noinline and crash-only.
allowed_pattern='^src/lisp/eval_env_copy\.c3:[0-9]+:    io::eprintfn\("\[copy-env\] invariant failure: %s", \(ZString\)msg\);$'

violations="$(printf '%s\n' "$hits" | rg -v "$allowed_pattern" || true)"

if [[ -n "$violations" ]]; then
  echo "FAIL: boundary hot-path formatting calls detected:"
  printf '%s\n' "$violations"
  exit 1
fi

echo "OK: boundary hot-path formatting calls are clean (only allowlisted cold invariant reporter found)."
