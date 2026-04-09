#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

todo_file="TODO.md"
changelog_file="memory/CHANGELOG.md"
memory_runtime_doc="docs/areas/memory-runtime.md"
types_dispatch_doc="docs/areas/types-dispatch.md"
fail() {
  echo "FAIL: $1"
  exit 1
}

require_file() {
  local file="$1"
  [[ -f "$file" ]] || fail "missing required file: $file"
}

extract_status() {
  local file="$1"
  sed -n 's/^Status: `\([^`]*\)`.*/\1/p' "$file" | head -n 1
}

extract_as_of() {
  local file="$1"
  sed -n 's/^As of: \([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\)$/\1/p' "$file" | head -n 1
}

extract_todo_count() {
  sed -n 's/^Current actionable count: \([0-9][0-9]*\)$/\1/p' "$todo_file" | head -n 1
}

extract_latest_changelog_date() {
  rg -n '^## [0-9]{4}-[0-9]{2}-[0-9]{2}$' "$changelog_file" | head -n 1 | awk '{print $2}' || true
}

compare_dates() {
  local left="$1"
  local op="$2"
  local right="$3"
  local left_ts right_ts
  left_ts="$(date -u -d "$left" +%s 2>/dev/null || true)"
  right_ts="$(date -u -d "$right" +%s 2>/dev/null || true)"
  [[ -n "$left_ts" && -n "$right_ts" ]] || fail "invalid date comparison: $left vs $right"
  case "$op" in
    lt) (( left_ts < right_ts ));;
    le) (( left_ts <= right_ts ));;
    gt) (( left_ts > right_ts ));;
    ge) (( left_ts >= right_ts ));;
    eq) (( left_ts == right_ts ));;
    *) fail "unsupported comparison operator: $op";;
  esac
}

require_file "$todo_file"
require_file "$changelog_file"
require_file "$memory_runtime_doc"
require_file "$types_dispatch_doc"

todo_count="$(extract_todo_count)"
[[ -n "$todo_count" ]] || fail "could not parse Current actionable count from $todo_file"

if [[ "$todo_count" == "0" ]]; then
  if ! grep -Eiq -- '^[[:space:]]*-[[:space:]]*(none currently;|none\.)[[:space:]]*$' "$todo_file"; then
    fail "TODO.md reports 0 actionable items but does not explicitly declare that no live blocker queue is open"
  fi
fi

latest_changelog_date="$(extract_latest_changelog_date)"
[[ -n "$latest_changelog_date" ]] || fail "could not parse latest changelog date from $changelog_file"

for doc in "$memory_runtime_doc" "$types_dispatch_doc"; do
  as_of="$(extract_as_of "$doc")"
  [[ -n "$as_of" ]] || fail "could not parse As of date from $doc"
  compare_dates "$as_of" ge "$latest_changelog_date" || fail "$doc is stale: As of $as_of lags latest changelog date $latest_changelog_date"
done

memory_status="$(extract_status "$memory_runtime_doc")"
types_status="$(extract_status "$types_dispatch_doc")"

[[ "$memory_status" == "green" ]] || fail "$memory_runtime_doc must be green on the current fully validated runtime baseline (got $memory_status)"
[[ "$types_status" == "green" ]] || fail "$types_dispatch_doc must be green once the e2e baseline cleanup is closed (got $types_status)"

echo "OK: status consistency checks passed."
echo "  latest changelog date: $latest_changelog_date"
echo "  TODO actionable count: $todo_count"
echo "  memory runtime status: $memory_status"
echo "  types dispatch status: $types_status"
