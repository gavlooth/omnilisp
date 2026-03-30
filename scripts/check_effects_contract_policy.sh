#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

diff_range="${OMNI_EFFECTS_POLICY_RANGE:-${OMNI_BOUNDARY_POLICY_RANGE:-}}"

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

has_rg() {
  command -v rg >/dev/null 2>&1
}

search_regex_quiet() {
  local pattern="$1"
  local file="$2"
  if has_rg; then
    rg -n "$pattern" "$file" >/dev/null
    return $?
  fi
  grep -n -E -- "$pattern" "$file" >/dev/null
}

search_regex_print() {
  local pattern="$1"
  local file="$2"
  if has_rg; then
    rg -n "$pattern" "$file"
    return $?
  fi
  grep -n -E -- "$pattern" "$file"
}

emit_diff_for_path() {
  local path="$1"

  if [[ -n "$diff_range" ]]; then
    git diff -U0 "$diff_range" -- "$path" || true
    return
  fi

  local emitted=0
  if ! git diff --quiet --ignore-submodules -- "$path"; then
    git diff -U0 -- "$path" || true
    emitted=1
  fi
  if ! git diff --cached --quiet --ignore-submodules -- "$path"; then
    git diff -U0 --cached -- "$path" || true
    emitted=1
  fi

  if ((emitted == 1)); then
    return
  fi

  if git rev-parse --verify HEAD~1 >/dev/null 2>&1; then
    git diff -U0 HEAD~1..HEAD -- "$path" || true
    return
  fi

  git diff-tree --no-commit-id -U0 -r HEAD -- "$path" || true
}

extract_added_lines() {
  local path="$1"
  emit_diff_for_path "$path" | awk '/^\+[^+]/ { print substr($0, 2) }'
}

resolve_primitive_source() {
  local fn_name="$1"
  local marker="fn Value* ${fn_name}("

  if has_rg; then
    rg -l -F "$marker" src/lisp/*.c3 2>/dev/null | head -n 1 || true
    return 0
  fi
  grep -l -F -- "$marker" src/lisp/*.c3 2>/dev/null | head -n 1 || true
}

extract_function_body() {
  local file="$1"
  local fn_name="$2"
  local out_file="$3"
  awk -v marker="fn Value* ${fn_name}(" '
    BEGIN {
      in_fn = 0
      seen_open = 0
      completed = 0
      depth = 0
      found = 0
    }
    {
      if (!in_fn && index($0, marker) > 0) {
        in_fn = 1
        found = 1
      }
      if (in_fn) {
        print $0
        line = $0
        opens = gsub(/\{/, "", line)
        closes = gsub(/\}/, "", line)
        if (opens > 0) seen_open = 1
        depth += opens
        depth -= closes
        if (seen_open && depth <= 0) {
          completed = 1
          exit 0
        }
      }
    }
    END {
      if (!found) exit 2
      if (!completed) exit 3
    }
  ' "$file" > "$out_file"
}

check_new_public_primitives_forbidden_failure_patterns() {
  local added_table="$tmp_dir/added_primitive_rows.txt"
  local new_public_prims="$tmp_dir/new_public_prims.txt"

  extract_added_lines "src/lisp/eval_init_primitives.c3" > "$added_table"
  sed -n 's/.*{ "\([^"]\+\)", *&\(prim_[A-Za-z0-9_]\+\),.*/\1 \2/p' "$added_table" \
    | awk '$1 !~ /^__/ { print }' \
    | sort -u > "$new_public_prims"

  local count
  count="$(wc -l < "$new_public_prims" | tr -d ' ')"
  if [[ "$count" == "0" ]]; then
    echo "OK: effects-contract policy (no newly-added public primitives in diff)."
    return 0
  fi

  local violations=0
  while IFS= read -r row; do
    [[ -z "$row" ]] && continue
    local prim_name fn_name source_file body_file
    prim_name="${row%% *}"
    fn_name="${row##* }"
    source_file="$(resolve_primitive_source "$fn_name")"
    if [[ -z "$source_file" ]]; then
      echo "FAIL: effects-contract policy cannot find implementation for ${prim_name} (${fn_name})"
      violations=1
      continue
    fi
    body_file="$tmp_dir/${fn_name}.body"
    if ! extract_function_body "$source_file" "$fn_name" "$body_file"; then
      echo "FAIL: effects-contract policy cannot extract body for ${prim_name} (${fn_name}) in ${source_file}"
      violations=1
      continue
    fi

    if search_regex_quiet 'raise_error\(' "$body_file"; then
      echo "FAIL: effects-contract policy forbids raise_error(...) in newly-added public primitive ${prim_name} (${source_file})"
      search_regex_print 'raise_error\(' "$body_file" | sed 's/^/    /'
      violations=1
    fi
    if search_regex_quiet 'make_error\(' "$body_file"; then
      echo "FAIL: effects-contract policy forbids make_error(...) in newly-added public primitive ${prim_name} (${source_file})"
      search_regex_print 'make_error\(' "$body_file" | sed 's/^/    /'
      violations=1
    fi
  done < "$new_public_prims"

  if ((violations == 1)); then
    return 1
  fi

  echo "OK: effects-contract policy checked $count newly-added public primitive(s)."
  return 0
}

check_stdlib_direct_raise_additions() {
  local added_stdlib="$tmp_dir/added_stdlib_lines.txt"
  local direct_raise_hits="$tmp_dir/direct_raise_hits.txt"

  extract_added_lines "stdlib/stdlib.lisp" > "$added_stdlib"
  search_regex_print '\(signal[[:space:]]+raise[[:space:]]' "$added_stdlib" > "$direct_raise_hits" || true

  if [[ -s "$direct_raise_hits" ]]; then
    echo "FAIL: effects-contract policy forbids adding direct (signal raise ...) in stdlib wrappers."
    echo "Use runtime canonical payload helpers instead of embedding ad-hoc raise payloads."
    sed 's/^/  - /' "$direct_raise_hits"
    return 1
  fi

  echo "OK: effects-contract policy found no newly-added direct stdlib (signal raise ...) calls."
}

check_migrated_surfaces_no_legacy_failure_constructors() {
  local files=(
    "src/lisp/tls_primitives.c3"
    "src/lisp/tls_handle_lifecycle.c3"
    "src/lisp/jit_jit_handle_signal.c3"
    "src/lisp/jit_jit_runtime_effects.c3"
    "src/lisp/jit_jit_reset_shift.c3"
    "src/lisp/jit_jit_compile_effects_modules.c3"
  )

  local violations=0
  local file
  for file in "${files[@]}"; do
    if [[ ! -f "$file" ]]; then
      echo "FAIL: effects-contract policy expected migrated surface file missing: $file"
      violations=1
      continue
    fi

    if search_regex_quiet 'raise_error\(' "$file"; then
      echo "FAIL: effects-contract policy forbids raise_error(...) in migrated surface $file"
      search_regex_print 'raise_error\(' "$file" | sed 's/^/    /'
      violations=1
    fi
    if search_regex_quiet 'make_error\(' "$file"; then
      echo "FAIL: effects-contract policy forbids make_error(...) in migrated surface $file"
      search_regex_print 'make_error\(' "$file" | sed 's/^/    /'
      violations=1
    fi
  done

  if ((violations == 1)); then
    return 1
  fi

  echo "OK: effects-contract policy verified migrated surfaces use canonical payload error constructors."
}

main() {
  check_new_public_primitives_forbidden_failure_patterns
  check_stdlib_direct_raise_additions
  check_migrated_surfaces_no_legacy_failure_constructors
}

main "$@"
