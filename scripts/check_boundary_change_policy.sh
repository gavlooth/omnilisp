#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

normal_log="${1:-build/boundary_hardening_normal.log}"
asan_log="${2:-build/boundary_hardening_asan.log}"
diff_range="${OMNI_BOUNDARY_POLICY_RANGE:-}"
sensitive_file_list="${OMNI_BOUNDARY_SENSITIVE_FILE_LIST:-scripts/boundary_sensitive_files.txt}"
value_policy_guard="${OMNI_BOUNDARY_VALUE_POLICY_GUARD:-scripts/check_boundary_value_policy_coverage.py}"
ownership_inventory_guard="${OMNI_BOUNDARY_OWNERSHIP_INVENTORY_GUARD:-scripts/check_memory_ownership_inventory.py}"

declare -A boundary_sensitive_files=()

load_sensitive_files() {
  if [[ ! -f "$sensitive_file_list" ]]; then
    echo "FAIL: boundary-sensitive file list missing: $sensitive_file_list"
    exit 1
  fi

  while IFS= read -r raw || [[ -n "$raw" ]]; do
    local line="$raw"
    line="${line%%#*}"
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    [[ -z "$line" ]] && continue
    boundary_sensitive_files["$line"]=1
  done < "$sensitive_file_list"
}

is_boundary_sensitive_file() {
  local file="$1"
  [[ -n "${boundary_sensitive_files["$file"]:-}" ]]
}

collect_changed_files() {
  if [[ -n "$diff_range" ]]; then
    git diff --name-only "$diff_range" | awk 'NF' | LC_ALL=C sort -u || true
    return
  fi

  local local_changes=0
  if ! git diff --quiet --ignore-submodules --; then
    local_changes=1
  fi
  if ! git diff --cached --quiet --ignore-submodules --; then
    local_changes=1
  fi
  if [[ -n "$(git ls-files --others --exclude-standard || true)" ]]; then
    local_changes=1
  fi

  if ((local_changes == 1)); then
    {
      git diff --name-only -- || true
      git diff --name-only --cached -- || true
      git ls-files --others --exclude-standard || true
    } | awk 'NF' | LC_ALL=C sort -u
    return
  fi

  if git rev-parse --verify HEAD~1 >/dev/null 2>&1; then
    git diff --name-only HEAD~1..HEAD | awk 'NF' | LC_ALL=C sort -u || true
    return
  fi

  git diff-tree --no-commit-id --name-only -r HEAD | awk 'NF' | LC_ALL=C sort -u || true
}

extract_summary_line() {
  local log_file="$1"
  local suite="$2"
  grep -aE "OMNI_TEST_SUMMARY suite=${suite}( |$)" "$log_file" | tail -n 1 || true
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
  "$value_policy_guard"
  load_sensitive_files

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
    echo "Sensitive file list: $sensitive_file_list"
    return 0
  fi

  if [[ -x "$ownership_inventory_guard" ]]; then
    "$ownership_inventory_guard" "${changed_files[@]}"
  else
    python3 "$ownership_inventory_guard" "${changed_files[@]}"
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
  echo "Sensitive file list: $sensitive_file_list"

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
