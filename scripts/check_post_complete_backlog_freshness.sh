#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

backlog_file="docs/plans/post-complete-backlog.md"
changelog_file="memory/CHANGELOG.md"

release_cycle_env="${OMNI_POST_COMPLETE_BACKLOG_RELEASE_CYCLES:-1}"
fallback_days_env="${OMNI_POST_COMPLETE_BACKLOG_FALLBACK_DAYS:-30}"

if [[ ! -f "$backlog_file" ]]; then
  echo "FAIL: missing backlog file: $backlog_file"
  exit 1
fi

if [[ ! -f "$changelog_file" ]]; then
  echo "FAIL: missing changelog file: $changelog_file"
  exit 1
fi

parse_int() {
  if [[ "$1" =~ ^[0-9]+$ ]]; then
    printf '%s' "$1"
  else
    echo "FAIL: invalid numeric value in ${2:-environment}"
    exit 1
  fi
}

release_cycles="$(parse_int "$release_cycle_env" "OMNI_POST_COMPLETE_BACKLOG_RELEASE_CYCLES")"
fallback_days="$(parse_int "$fallback_days_env" "OMNI_POST_COMPLETE_BACKLOG_FALLBACK_DAYS")"

mapfile -t release_dates < <(rg '^## [0-9]{4}-[0-9]{2}-[0-9]{2}$' "$changelog_file" | awk '{print $2}')

if [[ "${#release_dates[@]}" -eq 0 ]]; then
  echo "FAIL: could not parse release dates from ${changelog_file}"
  exit 1
fi

cutoff_date="${release_dates[0]}"
if (( release_cycles < "${#release_dates[@]}" )); then
  cutoff_date="${release_dates[${release_cycles}]}"
fi

cutoff_ts="$(date -u -d "$cutoff_date" +%s || true)"
if [[ -z "$cutoff_ts" ]]; then
  fallback_ts="$(date -u -d "${fallback_days} days ago" +%s)"
  cutoff_ts="$fallback_ts"
  cutoff_date="$(date -u -d "$fallback_days days ago" +%Y-%m-%d)"
fi

stale_items=()

line_no=0
while IFS= read -r line; do
  ((line_no += 1))
  trimmed="${line#"${line%%[![:space:]]*}"}"
  if [[ "$trimmed" != "- [ ] "* ]]; then
    continue
  fi

  # Ignore checklist markers that include explicit skip tags.
  if [[ "$trimmed" == *"(skip-staleness-check)"* ]]; then
    continue
  fi

  line_ts="$(git log -L "${line_no},${line_no}:${backlog_file}" --format=%ct -n 1 --no-patch 2>/dev/null || true)"
  if [[ -z "$line_ts" ]]; then
    stale_items+=("line ${line_no}: no git history found; item: ${line#- [ ] }")
    continue
  fi

  if (( line_ts < cutoff_ts )); then
    line_date="$(date -u -d "@${line_ts}" +%Y-%m-%d)"
    stale_items+=("line ${line_no}: last updated ${line_date}, cutoff ${cutoff_date}; item: ${line#- [ ] }")
  fi
done < "$backlog_file"

if [[ "${#stale_items[@]}" -eq 0 ]]; then
  echo "OK: post-complete-backlog stale-check passed. Cutoff=${cutoff_date} (release cycles back=${release_cycles})."
  exit 0
fi

echo "FAIL: stale unchecked items found in ${backlog_file}."
echo "  Staleness rule: line last updated before ${cutoff_date} (release cycles back=${release_cycles})."
for item in "${stale_items[@]}"; do
  echo "  - ${item}"
done
echo "Hint: refresh items by rewriting their lines or add (skip-staleness-check) to opt out."
echo "  Optional: set OMNI_POST_COMPLETE_BACKLOG_RELEASE_CYCLES and OMNI_POST_COMPLETE_BACKLOG_FALLBACK_DAYS."
exit 1
