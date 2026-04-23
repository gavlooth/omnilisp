#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

files=(
  "src/lisp/jit_compiler_compile.c3"
  "src/lisp/runtime_backend_hooks_cache.c3"
)

violations="$(
  awk '
    BEGIN { in_debug = 0; in_diag = 0 }
    /^\s*\$if DEBUG_BUILD:/ { in_debug = 1; in_diag = 0; next }
    /^\s*\$endif/ { in_debug = 0; in_diag = 0; next }
    in_debug {
      line = $0
      sub(/^[[:space:]]+/, "", line)
      if (line == "") next
      if (in_diag) {
        if (line ~ /\);[[:space:]]*$/) in_diag = 0
        next
      }
      if (line ~ /^io::e?printfn\(/) {
        if (line !~ /\);[[:space:]]*$/) in_diag = 1
        next
      }
      if (line ~ /^\/\/.*/) next
      printf "%s:%d:%s\n", FILENAME, FNR, $0
    }
  ' "${files[@]}"
)"

if [[ -n "$violations" ]]; then
  echo "FAIL: DEBUG_BUILD block contains non-diagnostic statements" >&2
  printf '%s\n' "$violations" | sed 's/^/  /' >&2
  exit 1
fi

echo "OK: DEBUG_BUILD blocks in JIT/cache surfaces contain diagnostics only."
