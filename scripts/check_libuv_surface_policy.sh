#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

prims_file="src/lisp/eval_init_primitives.c3"
stdlib_file="stdlib/stdlib.lisp"
diff_range="${OMNI_EFFECTS_POLICY_RANGE:-${OMNI_BOUNDARY_POLICY_RANGE:-}}"

for required in "$prims_file" "$stdlib_file"; do
  if [[ ! -f "$required" ]]; then
    echo "FAIL: libuv surface policy missing required file: $required"
    exit 1
  fi
done

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

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

added_prim_rows="$tmp_dir/added_prim_rows.txt"
added_stdlib_lines="$tmp_dir/added_stdlib_lines.txt"
added_raw="$tmp_dir/added_raw.txt"
added_io_effects="$tmp_dir/added_io_effects.txt"
wrappers_to_check="$tmp_dir/wrappers_to_check.txt"

extract_added_lines "$prims_file" > "$added_prim_rows"
extract_added_lines "$stdlib_file" > "$added_stdlib_lines"

sed -n 's/.*{ "\(__raw-[^"]\+\)", *&\(prim_[A-Za-z0-9_]\+\),.*/\1 \2/p' "$added_prim_rows" \
  | awk '{ print $1 }' \
  | sort -u > "$added_raw"

sed -n 's/^[[:space:]]*(define \[effect\][[:space:]]*(io\/\([^[:space:])]\+\).*/\1/p' "$added_stdlib_lines" \
  | sort -u > "$added_io_effects"

{
  while IFS= read -r raw; do
    [[ -z "$raw" ]] && continue
    printf "%s\n" "${raw#__raw-}"
  done < "$added_raw"

  while IFS= read -r effect_name; do
    [[ -z "$effect_name" ]] && continue
    printf "%s\n" "$effect_name"
  done < "$added_io_effects"
} | sort -u > "$wrappers_to_check"

checked_count="$(wc -l < "$wrappers_to_check" | tr -d ' ')"
if [[ "$checked_count" == "0" ]]; then
  echo "OK: libuv surface policy (no newly-added io effect/raw wrapper APIs in diff)."
  exit 0
fi

violations=0
while IFS= read -r wrapper; do
  [[ -z "$wrapper" ]] && continue
  raw_name="__raw-${wrapper}"

  if ! rg -q -F "{ \"${raw_name}\"," "$prims_file"; then
    echo "FAIL: libuv surface policy missing raw primitive registration for io/${wrapper} (${raw_name})."
    violations=1
  fi

  if ! rg -q -F "(define [effect] (io/${wrapper}" "$stdlib_file"; then
    echo "FAIL: libuv surface policy missing stdlib io effect declaration for wrapper '${wrapper}'."
    violations=1
  fi

  if ! rg -q -F "(define (${wrapper}" "$stdlib_file"; then
    echo "FAIL: libuv surface policy requires function-style wrapper define for '${wrapper}'."
    echo "      expected: (define (${wrapper} ...)"
    violations=1
  fi

  if rg -q -F "(define ${wrapper} (lambda" "$added_stdlib_lines"; then
    echo "FAIL: libuv surface policy forbids introducing new io wrapper '${wrapper}' as lambda alias."
    echo "      use function-style define (typed/dispatched surface) instead."
    violations=1
  fi
done < "$wrappers_to_check"

if ((violations == 1)); then
  exit 1
fi

echo "OK: libuv surface policy checked ${checked_count} newly-added io wrapper API name(s)."
