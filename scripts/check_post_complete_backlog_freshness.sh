#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

fail() {
  echo "FAIL: $1" >&2
  exit 1
}

bash scripts/check_status_consistency.sh

todo_count="$(sed -n 's/^Current actionable count: \([0-9][0-9]*\)$/\1/p' TODO.md | head -n 1)"
if [[ -z "$todo_count" ]]; then
  todo_count="$(
    (rg -c '^[[:space:]]*-[[:space:]]+\[[[:space:]]\]' docs/todo_parts 2>/dev/null || true) |
      awk -F: '{total += $2} END {print total + 0}'
  )"
fi
[[ "$todo_count" == "0" ]] || fail "post-complete backlog requires Current actionable count: 0, got ${todo_count:-unparseable}"

open_items="$(
  rg -n \
    --glob '!**/archive/**' \
    --glob '!**/TODO-backup-*.md' \
    '^[[:space:]]*-[[:space:]]+\[[[:space:]]\]' \
    TODO.md docs/todo_parts .agents/PLAN.md docs/plans 2>/dev/null || true
)"

[[ -z "$open_items" ]] || fail "post-complete backlog still has open checklist items:
$open_items"

echo "OK: post-complete backlog freshness checks passed."
