#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

runner="scripts/run_e2e.sh"
manifest="scripts/baselines/e2e_expected_diff.txt"
metadata="scripts/baselines/e2e_expected_diff.tsv"
entry_backend="src/entry_build_backend_compile.c3"
runtime_manifest="src/entry_build_runtime_manifest.c3"
lisp_manifest_part0="src/entry_build_runtime_manifest_lisp_part0.c3"
lisp_manifest_part1="src/entry_build_runtime_manifest_lisp_part1.c3"
lisp_manifest_part2="src/entry_build_runtime_manifest_lisp_part2.c3"
lisp_manifest_part3="src/entry_build_runtime_manifest_lisp_part3.c3"
expected_stage3_sources=(
  'src/main*.c3'
  'src/entry_*.c3'
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

check_lisp_runtime_manifest_parity() {
  mapfile -t expected_lisp_sources < <(
    find src/lisp -maxdepth 1 -type f -name '*.c3' ! -name 'tests_*.c3' | sort
  )
  mapfile -t manifest_lisp_sources < <(
    sed -n 's/.*"\(src\/lisp\/[^"]*\.c3\)".*/\1/p' \
      "$lisp_manifest_part0" \
      "$lisp_manifest_part1" \
      "$lisp_manifest_part2" \
      "$lisp_manifest_part3" |
      sort
  )

  mapfile -t duplicate_manifest_lisp_sources < <(
    printf '%s\n' "${manifest_lisp_sources[@]}" | uniq -d
  )
  [[ ${#duplicate_manifest_lisp_sources[@]} -eq 0 ]] \
    || fail "AOT lisp runtime manifests contain duplicate sources: ${duplicate_manifest_lisp_sources[*]}"

  [[ ${#manifest_lisp_sources[@]} -eq ${#expected_lisp_sources[@]} ]] \
    || fail "AOT lisp runtime manifests do not contain every non-test src/lisp/*.c3 source"
  for i in "${!expected_lisp_sources[@]}"; do
    [[ "${manifest_lisp_sources[$i]}" == "${expected_lisp_sources[$i]}" ]] \
      || fail "AOT lisp runtime manifest drifted: expected ${expected_lisp_sources[$i]}, found ${manifest_lisp_sources[$i]:-<missing>}"
  done
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

  grep -q -F 'append_aot_runtime_sources(&sources)' "$entry_backend" \
    || fail "${entry_backend} no longer appends the guarded AOT runtime source manifests"
  grep -q -F 'append_manifest_sources(out_sources, AOT_RUNTIME_ROOT_SOURCES[..], "AOT runtime root source")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes root sources to AOT compile parity"
  grep -q -F 'append_manifest_sources(out_sources, AOT_RUNTIME_LISP_SOURCES_PART0[..], "AOT runtime lisp source")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes lisp part 0 sources to AOT compile parity"
  grep -q -F 'append_manifest_sources(out_sources, AOT_RUNTIME_LISP_SOURCES_PART1[..], "AOT runtime lisp source")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes lisp part 1 sources to AOT compile parity"
  grep -q -F 'append_manifest_sources(out_sources, AOT_RUNTIME_LISP_SOURCES_PART2[..], "AOT runtime lisp source")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes lisp part 2 sources to AOT compile parity"
  grep -q -F 'append_manifest_sources(out_sources, AOT_RUNTIME_LISP_SOURCES_PART3[..], "AOT runtime lisp source")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes lisp part 3 sources to AOT compile parity"
  grep -q -F 'append_manifest_sources(out_sources, AOT_RUNTIME_PIKA_SOURCES[..], "AOT runtime pika source")' "$entry_backend" \
    || fail "${entry_backend} no longer contributes pika sources to AOT compile parity"
  grep -q -F 'if (!ensure_required_file(source, label)) return false;' "$entry_backend" \
    || fail "${entry_backend} no longer validates manifest source files before AOT compile"
  grep -q -F 'out_sources.push(source);' "$entry_backend" \
    || fail "${entry_backend} no longer appends manifest source entries to the AOT compile"
  grep -q -F 'sources.push(((ZString)temp_c3_path).str_view())' "$entry_backend" \
    || fail "${entry_backend} no longer appends the generated e2e test source"

  grep -q -F '"src/main.c3"' "$runtime_manifest" \
    || fail "${runtime_manifest} no longer contributes main.c3 to AOT compile parity"
  grep -q -F '"src/entry_interp_helpers.c3"' "$runtime_manifest" \
    || fail "${runtime_manifest} no longer contributes entry helper sources to AOT compile parity"
  grep -q -F '"src/scope_region.c3"' "$runtime_manifest" \
    || fail "${runtime_manifest} no longer contributes scope_region sources to AOT compile parity"
  grep -q -F '"src/stack_engine.c3"' "$runtime_manifest" \
    || fail "${runtime_manifest} no longer contributes stack_engine sources to AOT compile parity"
  grep -q -F '"src/ffi_bindings.c3"' "$runtime_manifest" \
    || fail "${runtime_manifest} no longer contributes ffi_bindings.c3 to AOT compile parity"
  grep -q -F '"src/pika/lisp_pika.c3"' "$runtime_manifest" \
    || fail "${runtime_manifest} no longer contributes pika sources to AOT compile parity"
  grep -q -F '"src/lisp/aot.c3"' "$lisp_manifest_part0" \
    || fail "${lisp_manifest_part0} no longer contributes lisp AOT sources to AOT compile parity"
  grep -q -F '"src/lisp/eval_boundary_provenance_reachability.c3"' "$lisp_manifest_part1" \
    || fail "${lisp_manifest_part1} no longer contributes lisp runtime boundary sources to AOT compile parity"
  grep -q -F '"src/lisp/parser_application.c3"' "$lisp_manifest_part2" \
    || fail "${lisp_manifest_part2} no longer contributes lisp parser sources to AOT compile parity"
  grep -q -F '"src/lisp/tensor_vulkan_backend.c3"' "$lisp_manifest_part3" \
    || fail "${lisp_manifest_part3} no longer contributes lisp Vulkan backend sources to AOT compile parity"
  grep -q -F '"src/lisp/prim_ml_vulkan_losses.c3"' "$lisp_manifest_part3" \
    || fail "${lisp_manifest_part3} no longer contributes Vulkan ML loss sources to AOT compile parity"

  check_lisp_runtime_manifest_parity
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
