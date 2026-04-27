#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

runner="scripts/run_e2e.sh"
generator="src/lisp/tests_e2e_generation.c3"
core_cases="src/lisp/tests_e2e_generation_cases_core.c3"
extended_cases="src/lisp/tests_e2e_generation_cases_extended.c3"
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

review_rule_has_required_doc() {
  local review_rule="$1"
  [[ "$review_rule" == *"memory/CHANGELOG.md"* || "$review_rule" == *"docs/areas/types-dispatch.md"* ]]
}

check_review_rule_self_test() {
  review_rule_has_required_doc "review memory/CHANGELOG.md before changing this baseline" \
    || fail "review-rule self-test rejected memory/CHANGELOG.md"
  review_rule_has_required_doc "review docs/areas/types-dispatch.md before changing this baseline" \
    || fail "review-rule self-test rejected docs/areas/types-dispatch.md"
  review_rule_has_required_doc "review memory/CHANGELOG.md and docs/areas/types-dispatch.md" \
    || fail "review-rule self-test rejected both required docs"
  if review_rule_has_required_doc "review docs/areas/memory-runtime.md only"; then
    fail "review-rule self-test accepted a row without an approved review document"
  fi
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

check_generated_case_table_policy() {
  local expected_count=431
  local cases_tsv="$tmp_dir/generated_cases.tsv"
  local parser_issues="$tmp_dir/generated_case_parser_issues.txt"
  local duplicate_names="$tmp_dir/generated_case_duplicate_names.txt"
  local duplicate_exprs="$tmp_dir/generated_case_duplicate_exprs.txt"
  local stale_comments="$tmp_dir/generated_case_stale_comments.txt"
  local comment_scan_files=(src/lisp/tests_e2e_generation*.c3)

  awk '
    /^[[:space:]]*\/\// {
      lowered = tolower($0)
      if (lowered ~ /skip-count|skip_count|generated .*skipped|[0-9]+[[:space:]]+skipped/) {
        print FILENAME ":" FNR ":" $0
      }
    }
  ' "${comment_scan_files[@]}" > "$stale_comments"

  awk -v out="$cases_tsv" -v issues="$parser_issues" '
    function trim(s) {
      gsub(/^[[:space:]]+|[[:space:]]+$/, "", s)
      return s
    }

    function parse_quoted(s, pos, quote,    i, ch, escaped, val) {
      parsed_value = ""
      parsed_next = 0
      while (pos <= length(s) && substr(s, pos, 1) ~ /[[:space:]]/) pos++
      if (substr(s, pos, 1) != quote) return 0
      pos++
      escaped = 0
      val = ""
      for (i = pos; i <= length(s); i++) {
        ch = substr(s, i, 1)
        if (escaped) {
          val = val "\\" ch
          escaped = 0
        } else if (ch == "\\") {
          escaped = 1
        } else if (ch == quote) {
          parsed_value = val
          parsed_next = i + 1
          return 1
        } else {
          val = val ch
        }
      }
      return 0
    }

    function parse_case(buf, file, line_no,    pos, comma_pos, quote, name, expr, rest) {
      pos = index(buf, "{")
      if (pos == 0) {
        print file ":" line_no ": generated case row missing opening brace" >> issues
        return
      }
      pos++
      if (!parse_quoted(buf, pos, "\"")) {
        print file ":" line_no ": generated case row missing quoted name" >> issues
        return
      }
      name = parsed_value
      pos = parsed_next
      comma_pos = index(substr(buf, pos), ",")
      if (comma_pos == 0) {
        print file ":" line_no ": generated case row missing comma" >> issues
        return
      }
      pos += comma_pos
      while (pos <= length(buf) && substr(buf, pos, 1) ~ /[[:space:]]/) pos++
      quote = substr(buf, pos, 1)
      if (quote != "\"" && quote != "`") {
        print file ":" line_no ": generated case row expression must be a string/backtick literal" >> issues
        return
      }
      if (!parse_quoted(buf, pos, quote)) {
        print file ":" line_no ": generated case row has unterminated expression literal" >> issues
        return
      }
      expr = parsed_value
      rest = trim(substr(buf, parsed_next))
      if (rest !~ /^},?$/) {
        print file ":" line_no ": generated case row has unexpected trailing text: " rest >> issues
        return
      }
      print file ":" line_no "\t" name "\t" expr >> out
    }

    BEGIN {
      row_open = 0
      buf = ""
      start_line = 0
    }

    !row_open && /^[[:space:]]*\{[[:space:]]*"/ {
      row_open = 1
      buf = $0
      start_line = FNR
      start_file = FILENAME
      if ($0 ~ /\}[[:space:]]*,?[[:space:]]*$/) {
        parse_case(buf, start_file, start_line)
        row_open = 0
        buf = ""
      }
      next
    }

    row_open {
      buf = buf " " $0
      if ($0 ~ /\}[[:space:]]*,?[[:space:]]*$/) {
        parse_case(buf, start_file, start_line)
        row_open = 0
        buf = ""
      }
      next
    }

    END {
      if (row_open) {
        print start_file ":" start_line ": generated case row was not closed" >> issues
      }
    }
  ' "$core_cases" "$extended_cases"

  [[ ! -s "$parser_issues" ]] || {
    sed 's/^/  /' "$parser_issues" >&2
    fail "generated e2e case table parser found malformed rows"
  }
  [[ ! -s "$stale_comments" ]] || {
    sed 's/^/  /' "$stale_comments" >&2
    fail "generated e2e case comments contain stale skip-count reporting language"
  }

  local actual_count
  actual_count=$(wc -l < "$cases_tsv")
  [[ "$actual_count" -eq "$expected_count" ]] \
    || fail "generated e2e case table must contain exactly ${expected_count} rows; found ${actual_count}"

  cut -f2 "$cases_tsv" | sort | uniq -d > "$duplicate_names"
  [[ ! -s "$duplicate_names" ]] || {
    echo "Duplicate generated e2e case names:" >&2
    while IFS= read -r duplicate; do
      awk -F '\t' -v key="$duplicate" '$2 == key { print "  " $1 ": " $2 }' "$cases_tsv" >&2
    done < "$duplicate_names"
    fail "generated e2e case names must be unique"
  }

  cut -f3 "$cases_tsv" | sort | uniq -d > "$duplicate_exprs"
  [[ ! -s "$duplicate_exprs" ]] || {
    echo "Duplicate generated e2e case expressions:" >&2
    while IFS= read -r duplicate; do
      awk -F '\t' -v key="$duplicate" '$3 == key { print "  " $1 ": " $2 " -> " $3 }' "$cases_tsv" >&2
    done < "$duplicate_exprs"
    fail "generated e2e case expressions must be unique"
  }

  awk -F '\t' '$2 == "block boolean shadowing if" && $3 == "(block (if true 10 20) (let (false true) (if false 30 40)))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the boolean-shadowing if regression row"
  awk -F '\t' '$2 == "match sibling var declarations" && $3 == "(match 2 (x x) (x x))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the match sibling declaration regression row"
  awk -F '\t' '$2 == "if boolean shadowing branches" && $3 == "(if (= 1 2) (let (false 1) false) (let (false 2) false))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the branch-local boolean-shadowing regression row"
  awk -F '\t' '$2 == "closure boolean shadow capture" && $3 == "(((lambda (false) (lambda () false)) 9))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the closure boolean-shadowing capture regression row"
  awk -F '\t' '$2 == "closure callable shadow capture" && $3 == "(((lambda (+) (lambda (x) (+ x 2))) (lambda (a b) 88)) 1)" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the closure callable-shadowing capture regression row"
  awk -F '\t' '$2 == "same-name let shadow" && $3 == "(let (x 1) (block (let (x 2) x) x))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the same-name let shadow regression row"
  awk -F '\t' '$2 == "same-name let closure capture" && $3 == "(let (x 1) (let (f (let (x 2) (lambda () x))) (+ (f) x)))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the same-name let closure-capture regression row"
  awk -F '\t' '$2 == "same-name mutable let capture" && $3 == "(let (x 1) (block (let (x 0) (let (inc (lambda () (set! x (+ x 1)))) (inc) x)) x))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the same-name mutable let regression row"
  awk -F '\t' '$2 == "same-name let rec shadow" && $3 == "(let (loop 99) (block (let ^rec (loop (lambda (n) (if (= n 0) 0 (loop (- n 1))))) (loop 3)) loop))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the same-name recursive let regression row"
  awk -F '\t' '$2 == "mutable capture through checkpoint" && $3 == "(let (x 0) (block (checkpoint (let (f (lambda () (set! x 1))) (f))) x))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the checkpoint mutable-capture regression row"
  awk -F '\t' '$2 == "mutable capture through capture" && $3 == "(let (x 0) (block (checkpoint (capture k (block (let (f (lambda () (set! x 1))) (f)) (k 0)))) x))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the capture mutable-capture regression row"
  awk -F '\t' '$2 == "checkpoint local mutable capture" && $3 == "(checkpoint (let (x 0) (block (let (f (lambda () (set! x 1))) (f)) x)))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the checkpoint-local mutable-capture regression row"
  awk -F '\t' '$2 == "checkpoint boxed outer restore" && $3 == "(let (x 0) (block (let (g (lambda () x)) (block (set! x 2) (checkpoint (let (f (lambda () (set! x 3))) (f))) x))))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the checkpoint boxed-outer restore regression row"
  awk -F '\t' '$2 == "module define shadows outer let collision" && $3 == "(let (auditx 0) (block (module auditmod (export auditx) (block (define auditx 1))) (with auditmod auditx)))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the module define/outer-let collision regression row"
  awk -F '\t' '$2 == "module set shadows outer let collision" && $3 == "(let (auditx 0) (block (module auditmodset (export auditx) (block (define auditx 1) (set! auditx 2))) (with auditmodset auditx)))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the module set/outer-let collision regression row"
  awk -F '\t' '$2 == "with inner let shadows module alias" && $3 == "(let (auditx 0) (block (module auditmodinner (export auditx) (define auditx 1)) (with auditmodinner (let (auditx 3) auditx))))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the inner-let/module-alias collision regression row"
  awk -F '\t' '$2 == "outer let restored after with module collision" && $3 == "(let (auditx 7) (block (module auditmod2 (export auditx) (define auditx 1)) (with auditmod2 auditx) auditx))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the module/outer-let restore regression row"
  awk -F '\t' '$2 == "module type shadows outer let collision" && $3 == "(let (AuditShadowType 0) (block (module audittypemod (export AuditShadowType) (define [abstract] AuditShadowType)) (with audittypemod AuditShadowType)))" { found = 1 } END { exit found ? 0 : 1 }' "$cases_tsv" \
    || fail "generated e2e corpus lost the module type/outer-let collision regression row"
}

if [[ "${1:-}" == "--stage3-source-parity" ]]; then
  check_stage3_source_parity
  echo "OK: Stage 3 e2e compile source parity checks passed."
  exit 0
fi

if [[ "${1:-}" == "--self-test-review-rule" ]]; then
  check_review_rule_self_test
  echo "OK: baseline review-rule self-test passed."
  exit 0
fi

for file in "$runner" "$manifest" "$metadata"; do
  [[ -f "$file" ]] || fail "missing required file: $file"
done
[[ -f "$generator" ]] || fail "missing required file: $generator"
[[ -f "$core_cases" ]] || fail "missing required file: $core_cases"
[[ -f "$extended_cases" ]] || fail "missing required file: $extended_cases"

check_stage3_source_parity

grep -q -F 'e2e_expected_diff_manifest="scripts/baselines/e2e_expected_diff.txt"' "$runner" \
  || fail "${runner} does not point at ${manifest}"
grep -q -F 'e2e_expected_diff_metadata="scripts/baselines/e2e_expected_diff.tsv"' "$runner" \
  || fail "${runner} does not point at ${metadata}"
grep -q -F 'cmp -s build/e2e_diff.txt "${e2e_expected_diff_manifest}"' "$runner" \
  || fail "${runner} no longer matches diff output against the tracked manifest"
grep -q -F '=== Generated 431 e2e tests ===' "$runner" \
  || fail "${runner} no longer enforces the generated e2e case count"
if grep -q -E 'skip_count|Generated .*skipped' "$generator"; then
  fail "${generator} reintroduced unmeasured e2e skip-count reporting"
fi
if grep -q -E '"(Integer|Boolean) shorthand' "$core_cases" "$extended_cases"; then
  fail "generated e2e cases contain noncanonical shorthand-labelled constructor rows"
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

check_generated_case_table_policy

metadata_header="$(head -n 1 "$metadata")"
[[ "$metadata_header" == $'diff_key\towner_area\treview_rule\tnote' ]] \
  || fail "${metadata} has an unexpected header: ${metadata_header}"

mapfile -t manifest_keys < <(awk '/^[0-9][0-9,]*[acd][0-9][0-9,]*$/ { print $0 }' "$manifest")

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
  if ! review_rule_has_required_doc "$review_rule"; then
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
