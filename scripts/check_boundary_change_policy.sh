#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

normal_log="${1:-build/boundary_hardening_normal.log}"
asan_log="${2:-build/boundary_hardening_asan.log}"
diff_range="${OMNI_BOUNDARY_POLICY_RANGE:-}"

is_boundary_sensitive_file() {
  local file="$1"
  case "$file" in
    src/stack_engine.c3) return 0 ;;
    src/lisp/eval_boundary_api.c3) return 0 ;;
    src/lisp/eval_promotion_copy.c3) return 0 ;;
    src/lisp/eval_promotion_escape.c3) return 0 ;;
    src/lisp/eval_env_copy.c3) return 0 ;;
    src/lisp/jit_jit_eval_scopes.c3) return 0 ;;
    src/lisp/eval_run_pipeline.c3) return 0 ;;
  esac
  return 1
}

collect_changed_files() {
  if [[ -n "$diff_range" ]]; then
    git diff --name-only "$diff_range" || true
    return
  fi

  if git rev-parse --verify HEAD~1 >/dev/null 2>&1; then
    git diff --name-only HEAD~1..HEAD || true
    return
  fi

  git diff-tree --no-commit-id --name-only -r HEAD || true
}

extract_summary_line() {
  local log_file="$1"
  local suite="$2"
  grep -E "OMNI_TEST_SUMMARY suite=${suite}( |$)" "$log_file" | tail -n 1 || true
}

extract_summary_field() {
  local line="$1"
  local key="$2"
  echo "$line" | tr ' ' '\n' | awk -F= -v k="$key" '$1 == k { print $2; exit }'
}

require_suite_fail_zero() {
  local log_file="$1"
  local suite="$2"
  local stage="$3"
  local line
  line="$(extract_summary_line "$log_file" "$suite")"
  if [[ -z "$line" ]]; then
    echo "FAIL: [policy/$stage] missing OMNI_TEST_SUMMARY for suite=$suite"
    return 1
  fi
  local fail_val
  fail_val="$(extract_summary_field "$line" "fail")"
  if [[ "$fail_val" != "0" ]]; then
    echo "FAIL: [policy/$stage] suite=$suite fail=${fail_val:-<missing>}"
    echo "  line: $line"
    return 1
  fi
  return 0
}

require_fiber_temp_enabled() {
  local log_file="$1"
  local stage="$2"
  local line
  line="$(extract_summary_line "$log_file" "fiber_temp_pool")"
  if [[ -z "$line" ]]; then
    echo "FAIL: [policy/$stage] missing OMNI_TEST_SUMMARY for suite=fiber_temp_pool"
    return 1
  fi
  local enabled
  enabled="$(extract_summary_field "$line" "enabled")"
  if [[ "$enabled" != "1" ]]; then
    echo "FAIL: [policy/$stage] expected fiber_temp_pool enabled=1, got ${enabled:-<missing>}"
    echo "  line: $line"
    return 1
  fi
  return 0
}

main() {
  local -a changed_files=()
  while IFS= read -r file; do
    [[ -z "$file" ]] && continue
    changed_files+=("$file")
  done < <(collect_changed_files)

  local boundary_touched=0
  local -a boundary_files=()
  for file in "${changed_files[@]}"; do
    if is_boundary_sensitive_file "$file"; then
      boundary_touched=1
      boundary_files+=("$file")
    fi
  done

  if ((boundary_touched == 0)); then
    echo "OK: boundary change policy skipped (no boundary-sensitive file changes)."
    return 0
  fi

  if [[ ! -f "$normal_log" ]]; then
    echo "FAIL: boundary change policy requires normal log: $normal_log"
    return 1
  fi
  if [[ ! -f "$asan_log" ]]; then
    echo "FAIL: boundary change policy requires ASAN log: $asan_log"
    return 1
  fi

  echo "Boundary-sensitive changes detected:"
  for file in "${boundary_files[@]}"; do
    echo "  - $file"
  done

  require_suite_fail_zero "$normal_log" "stack_engine" "normal"
  require_suite_fail_zero "$normal_log" "scope_region" "normal"
  require_suite_fail_zero "$normal_log" "unified" "normal"
  require_suite_fail_zero "$normal_log" "compiler" "normal"
  require_suite_fail_zero "$asan_log" "stack_engine" "asan"
  require_suite_fail_zero "$asan_log" "scope_region" "asan"
  require_suite_fail_zero "$asan_log" "unified" "asan"
  require_suite_fail_zero "$asan_log" "compiler" "asan"
  require_fiber_temp_enabled "$normal_log" "normal"
  require_fiber_temp_enabled "$asan_log" "asan"

  echo "OK: boundary change policy satisfied (normal + ASAN evidence present)."
}

main "$@"
