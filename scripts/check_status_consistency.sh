#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

todo_file="TODO.md"
todo_parts_dir="docs/todo_parts"
changelog_file="memory/CHANGELOG.md"
changelog_parts_dir="memory/changelog_parts"
memory_runtime_doc="docs/areas/memory-runtime.md"
types_dispatch_doc="docs/areas/types-dispatch.md"
ffi_foreign_runtime_doc="docs/areas/ffi-foreign-runtime.md"
validation_status_doc="docs/areas/validation-status.md"
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
  local targets=("$file")
  local parts_dir="${file%.md}_parts"
  if [[ -d "$parts_dir" ]]; then
    targets+=("$parts_dir")
  fi
  rg --no-filename '^Status: `' "${targets[@]}" |
    sed -n 's/^Status: `\([^`]*\)`.*/\1/p' |
    head -n 1
}

extract_as_of() {
  local file="$1"
  local targets=("$file")
  local parts_dir="${file%.md}_parts"
  if [[ -d "$parts_dir" ]]; then
    targets+=("$parts_dir")
  fi
  rg --no-filename '^As of: [0-9]{4}-[0-9]{2}-[0-9]{2}$' "${targets[@]}" |
    sed -n 's/^As of: \([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\)$/\1/p' |
    head -n 1
}

extract_todo_count() {
  local count
  count="$(sed -n 's/^Current actionable count: \([0-9][0-9]*\)$/\1/p' "$todo_file" | head -n 1)"
  if [[ -n "$count" ]]; then
    echo "$count"
    return
  fi
  if [[ -d "$todo_parts_dir" ]]; then
    (rg -c '^[[:space:]]*-[[:space:]]+\[[[:space:]]\]' "$todo_parts_dir" || true) | awk -F: '{total += $2} END {print total + 0}'
  fi
}

find_unlabeled_open_todos() {
  if [[ -d "$todo_parts_dir" ]]; then
    (rg -n '^[[:space:]]*-[[:space:]]+\[[[:space:]]\]' "$todo_parts_dir" || true) |
      awk '$0 !~ /\][[:space:]]+`/'
  fi
}

extract_latest_changelog_date() {
  local targets=("$changelog_file")
  if [[ -d "$changelog_parts_dir" ]]; then
    targets+=("$changelog_parts_dir")
  fi
  rg --no-filename '^## [0-9]{4}-[0-9]{2}-[0-9]{2}([[:space:]]|$)' "${targets[@]}" |
    sed -n 's/^## \([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\).*/\1/p' |
    sort -r |
    head -n 1
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
require_file "$ffi_foreign_runtime_doc"
require_file "$validation_status_doc"

todo_count="$(extract_todo_count)"
[[ -n "$todo_count" ]] || fail "could not parse TODO actionable count from $todo_file or $todo_parts_dir"
unlabeled_open_todos="$(find_unlabeled_open_todos)"
[[ -z "$unlabeled_open_todos" ]] || fail "unchecked TODO entries must start with a backtick task ID:
$unlabeled_open_todos"

if [[ "$todo_count" == "0" ]]; then
  if ! grep -Eiq -- '^[[:space:]]*-[[:space:]]*(none currently;|none\.)[[:space:]]*$' "$todo_file"; then
    fail "TODO.md reports 0 actionable items but does not explicitly declare that no live blocker queue is open"
  fi
fi

latest_changelog_date="$(extract_latest_changelog_date)"
[[ -n "$latest_changelog_date" ]] || fail "could not parse latest changelog date from $changelog_file or $changelog_parts_dir"

for doc in "$memory_runtime_doc" "$types_dispatch_doc" "$ffi_foreign_runtime_doc" "$validation_status_doc"; do
  as_of="$(extract_as_of "$doc")"
  [[ -n "$as_of" ]] || fail "could not parse As of date from $doc"
  compare_dates "$as_of" ge "$latest_changelog_date" || fail "$doc is stale: As of $as_of lags latest changelog date $latest_changelog_date"
done

memory_status="$(extract_status "$memory_runtime_doc")"
types_status="$(extract_status "$types_dispatch_doc")"
ffi_status="$(extract_status "$ffi_foreign_runtime_doc")"
validation_status="$(extract_status "$validation_status_doc")"

[[ "$memory_status" == "green" ]] || fail "$memory_runtime_doc must be green on the current fully validated runtime baseline (got $memory_status)"
[[ "$types_status" == "green" ]] || fail "$types_dispatch_doc must be green once the e2e baseline cleanup is closed (got $types_status)"
[[ "$ffi_status" == "yellow" ]] || fail "$ffi_foreign_runtime_doc must remain yellow until the non-C adapter/backend lanes are implemented (got $ffi_status)"
[[ "$validation_status" == "green" ]] || fail "$validation_status_doc must be green while no validation residual lane is open (got $validation_status)"

echo "OK: status consistency checks passed."
echo "  latest changelog date: $latest_changelog_date"
echo "  TODO actionable count: $todo_count"
echo "  memory runtime status: $memory_status"
echo "  types dispatch status: $types_status"
echo "  ffi foreign runtime status: $ffi_status"
echo "  validation status: $validation_status"
