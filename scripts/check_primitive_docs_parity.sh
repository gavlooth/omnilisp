#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

prims_file="src/lisp/eval_init_primitives.c3"
stdlib_file="stdlib/stdlib.lisp"
docs_a="docs/reference/11-appendix-primitives.md"
docs_b="docs/reference/12-appendix-stdlib.md"
docs_c="docs/LANGUAGE_SPEC.md"
docs_d="docs/reference/08-libraries.md"
diff_range="${OMNI_EFFECTS_POLICY_RANGE:-${OMNI_BOUNDARY_POLICY_RANGE:-}}"

for required in "$prims_file" "$stdlib_file" "$docs_a" "$docs_b" "$docs_c" "$docs_d"; do
  if [[ ! -f "$required" ]]; then
    echo "FAIL: primitive docs parity missing required file: $required"
    exit 1
  fi
done

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

has_rg() {
  command -v rg >/dev/null 2>&1
}

search_fixed_quiet() {
  local needle="$1"
  shift
  if has_rg; then
    rg -q -F "$needle" "$@"
    return $?
  fi
  grep -F -q -- "$needle" "$@"
}

registered_public="$tmp_dir/registered_public_prims.txt"
docs_blob="$tmp_dir/docs_blob.txt"
missing="$tmp_dir/missing_prims.txt"

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

assert_docs_reference() {
  local literal="$1"
  shift
  if search_fixed_quiet "\`${literal}\`" "$@"; then
    return 0
  fi
  return 1
}

sed -n 's/.*{ "\([^"]\+\)", *&\(prim_[A-Za-z0-9_]\+\),.*/\1/p' "$prims_file" \
  | awk '$0 !~ /^__/' \
  | sort -u > "$registered_public"

cat "$docs_a" "$docs_b" "$docs_c" "$docs_d" > "$docs_blob"

> "$missing"
while IFS= read -r prim; do
  [[ -z "$prim" ]] && continue
  if ! search_fixed_quiet "\`${prim}\`" "$docs_blob"; then
    printf "%s\n" "$prim" >> "$missing"
  fi
done < "$registered_public"

if [[ -s "$missing" ]]; then
  echo "FAIL: undocumented public primitives detected."
  echo "Add entries for the following names in docs/reference or LANGUAGE_SPEC:"
  sed 's/^/  - /' "$missing"
  exit 1
fi

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

raw_added_count="$(wc -l < "$added_raw" | tr -d ' ')"
wrapper_added_count="$(wc -l < "$wrappers_to_check" | tr -d ' ')"
raw_violations=0

if [[ "$raw_added_count" != "0" || "$wrapper_added_count" != "0" ]]; then
  while IFS= read -r wrapper; do
    [[ -z "$wrapper" ]] && continue
    raw_name="__raw-${wrapper}"

    if ! search_fixed_quiet "{ \"${raw_name}\"," "$prims_file"; then
      echo "FAIL: docs parity missing raw primitive registration for io/${wrapper} (${raw_name})."
      raw_violations=1
    fi

    if ! search_fixed_quiet "(define [effect] (io/${wrapper}" "$stdlib_file"; then
      echo "FAIL: docs parity missing io effect declaration for wrapper '${wrapper}'."
      raw_violations=1
    fi

    if ! search_fixed_quiet "(define (${wrapper}" "$stdlib_file"; then
      echo "FAIL: docs parity missing function-style wrapper define for '${wrapper}'."
      raw_violations=1
    fi

    if ! assert_docs_reference "$raw_name" "$docs_a" "$docs_c"; then
      echo "FAIL: docs parity missing raw primitive doc entry for '${raw_name}'."
      raw_violations=1
    fi

    if ! assert_docs_reference "$wrapper" "$docs_b" "$docs_c"; then
      echo "FAIL: docs parity missing wrapper doc entry for '${wrapper}'."
      raw_violations=1
    fi
  done < "$wrappers_to_check"
fi

if ((raw_violations == 1)); then
  exit 1
fi

count="$(wc -l < "$registered_public" | tr -d ' ')"
echo "OK: primitive docs parity passed for $count public primitive(s)."
echo "OK: raw/wrapper docs parity checked ${wrapper_added_count} newly-added io wrapper API name(s)."
