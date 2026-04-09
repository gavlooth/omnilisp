#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

runner="scripts/run_e2e.sh"
manifest="scripts/baselines/e2e_expected_diff.txt"
metadata="scripts/baselines/e2e_expected_diff.tsv"
entry_backend="src/entry_build_backend_compile.c3"
expected_stage3_sources=(
  'src/main*.c3'
  'src/scope_region*.c3'
  'src/stack_engine*.c3'
  'src/ffi_bindings.c3'
  'src/lisp/*.c3'
  'src/pika/*.c3'
  'build/e2e_test.c3'
)

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

check_stage3_source_parity() {
  mapfile -t actual_stage3_sources < <(
    awk '
      /^stage3_compile_sources=\(/ { in_block=1; next }
      in_block && /^\)/ { exit }
      in_block {
        gsub(/^[[:space:]]+|[[:space:]]+$/, "");
        if ($0 != "") print $0
      }
    ' "$runner"
  )

  [[ ${#actual_stage3_sources[@]} -eq ${#expected_stage3_sources[@]} ]] \
    || fail "${runner} no longer declares the expected Stage 3 source list"
  for i in "${!expected_stage3_sources[@]}"; do
    [[ "${actual_stage3_sources[$i]}" == "${expected_stage3_sources[$i]}" ]] \
      || fail "${runner} Stage 3 source list drifted at position $((i + 1))"
  done

  grep -q -F '${stage3_compile_sources[@]}' "$runner" \
    || fail "${runner} no longer compiles Stage 3 from the guarded source array"

  grep -q -F 'append_matching_sources(&sources, "src", "main", ".c3")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes main sources to AOT compile parity"
  grep -q -F 'append_matching_sources(&sources, "src", "scope_region", ".c3")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes scope_region sources to AOT compile parity"
  grep -q -F 'append_matching_sources(&sources, "src", "stack_engine", ".c3")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes stack_engine sources to AOT compile parity"
  grep -q -F 'append_matching_sources(&sources, "src/lisp", "", ".c3")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes lisp sources to AOT compile parity"
  grep -q -F 'append_matching_sources(&sources, "src/pika", "", ".c3")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes pika sources to AOT compile parity"
  grep -q -F 'sources.push("src/ffi_bindings.c3")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes ffi_bindings.c3 to AOT compile parity"
  grep -q -F 'sources.push(((ZString)temp_c3_path).str_view())' "$entry_backend" \
    || fail "${entry_backend} no longer appends the generated e2e test source"
}

if [[ "${1:-}" == "--stage3-source-parity" ]]; then
  check_stage3_source_parity
  echo "OK: Stage 3 e2e compile source parity checks passed."
  exit 0
fi

for file in "$runner" "$manifest" "$metadata"; do
  [[ -f "$file" ]] || fail "missing required file: $file"
done

check_stage3_source_parity

grep -q -F 'e2e_expected_diff_manifest="scripts/baselines/e2e_expected_diff.txt"' "$runner" \
  || fail "${runner} does not point at ${manifest}"
grep -q -F 'e2e_expected_diff_metadata="scripts/baselines/e2e_expected_diff.tsv"' "$runner" \
  || fail "${runner} does not point at ${metadata}"
grep -q -F 'cmp -s build/e2e_diff.txt "${e2e_expected_diff_manifest}"' "$runner" \
  || fail "${runner} no longer matches diff output against the tracked manifest"

metadata_header="$(head -n 1 "$metadata")"
[[ "$metadata_header" == $'diff_key\towner_area\treview_rule\tnote' ]] \
  || fail "${metadata} has an unexpected header: ${metadata_header}"

mapfile -t manifest_keys < <(awk '/^[0-9][0-9,]*[acd][0-9][0-9,]*$/ { print $0 }' "$manifest")

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

if (( ${#manifest_keys[@]} > 0 )); then
  printf '%s\n' "${manifest_keys[@]}" | sort > "$tmp_dir/manifest_keys.txt"
  printf '%s\n' "${manifest_keys[@]}" | sort | uniq -d > "$tmp_dir/manifest_dups.txt"
  [[ ! -s "$tmp_dir/manifest_dups.txt" ]] \
    || fail "${manifest} contains duplicate diff keys: $(tr '\n' ' ' < "$tmp_dir/manifest_dups.txt")"
else
  : > "$tmp_dir/manifest_keys.txt"
  : > "$tmp_dir/manifest_dups.txt"
fi

awk -F '\t' '
  NR > 1 && $0 !~ /^[[:space:]]*$/ { print $1 }
' "$metadata" > "$tmp_dir/metadata_keys_raw.txt"

: > "$tmp_dir/metadata_issues.txt"

line_no=1
while IFS=$'\t' read -r diff_key owner_area review_rule note extra; do
  (( line_no += 1 ))
  if [[ -z "${diff_key}${owner_area}${review_rule}${note}${extra}" ]]; then
    continue
  fi

  if [[ -n "$extra" ]]; then
    printf 'bad_field_count\t%s\n' "$line_no" >> "$tmp_dir/metadata_issues.txt"
    continue
  fi
  if [[ -z "$diff_key" ]]; then
    printf 'blank_key\t%s\n' "$line_no" >> "$tmp_dir/metadata_issues.txt"
  fi
  if [[ -z "$owner_area" ]]; then
    printf 'blank_owner\t%s\n' "$line_no" >> "$tmp_dir/metadata_issues.txt"
  fi
  if [[ -z "$review_rule" ]]; then
    printf 'blank_review\t%s\n' "$line_no" >> "$tmp_dir/metadata_issues.txt"
  fi
  if [[ "$review_rule" != *"memory/CHANGELOG.md"* || "$review_rule" != *"docs/areas/types-dispatch.md"* ]]; then
    printf 'bad_review_rule\t%s\t%s\n' "$line_no" "$diff_key" >> "$tmp_dir/metadata_issues.txt"
  fi
done < <(tail -n +2 "$metadata")

[[ ! -s "$tmp_dir/metadata_issues.txt" ]] || {
  sed 's/^/  /' "$tmp_dir/metadata_issues.txt" >&2
  fail "${metadata} contains malformed ownership rows"
}

if [[ -s "$tmp_dir/metadata_keys_raw.txt" ]]; then
  sort "$tmp_dir/metadata_keys_raw.txt" > "$tmp_dir/metadata_keys.txt"
  sort "$tmp_dir/metadata_keys_raw.txt" | uniq -d > "$tmp_dir/metadata_dups.txt"
  [[ ! -s "$tmp_dir/metadata_dups.txt" ]] \
    || fail "${metadata} contains duplicate diff keys: $(tr '\n' ' ' < "$tmp_dir/metadata_dups.txt")"
else
  : > "$tmp_dir/metadata_keys.txt"
  : > "$tmp_dir/metadata_dups.txt"
fi

comm -23 "$tmp_dir/manifest_keys.txt" "$tmp_dir/metadata_keys.txt" > "$tmp_dir/missing_metadata.txt"
[[ ! -s "$tmp_dir/missing_metadata.txt" ]] \
  || fail "${metadata} is missing ownership rows for: $(tr '\n' ' ' < "$tmp_dir/missing_metadata.txt")"

comm -13 "$tmp_dir/manifest_keys.txt" "$tmp_dir/metadata_keys.txt" > "$tmp_dir/extra_metadata.txt"
[[ ! -s "$tmp_dir/extra_metadata.txt" ]] \
  || fail "${metadata} contains extra ownership rows not present in ${manifest}: $(tr '\n' ' ' < "$tmp_dir/extra_metadata.txt")"

if [[ -f build/e2e_diff.txt ]]; then
  if (( ${#manifest_keys[@]} == 0 )); then
    fail "build/e2e_diff.txt exists but ${manifest} is empty; expected a fully clean e2e baseline"
  elif cmp -s build/e2e_diff.txt "$manifest"; then
    echo "OK: build/e2e_diff.txt matches the tracked baseline manifest."
  else
    fail "build/e2e_diff.txt exists but does not match ${manifest}"
  fi
else
  if (( ${#manifest_keys[@]} == 0 )); then
    echo "OK: e2e baseline policy is clean; no tracked baseline diff rows remain."
  else
    echo "OK: manifest and ownership policy are internally consistent."
  fi
  echo "Note: build/e2e_diff.txt not present; skipped live diff artifact comparison."
fi
