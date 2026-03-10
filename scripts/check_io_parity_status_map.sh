#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

map_file="docs/plans/library-gaps-todo.md"
diff_range="${OMNI_IO_PARITY_POLICY_RANGE:-}"

if [[ ! -f "$map_file" ]]; then
  echo "FAIL: missing parity map file: $map_file"
  exit 1
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

table_rows="$tmp_dir/table_rows.txt"
impl_files="$tmp_dir/impl_files.txt"

awk '
  /^Current `io\/\*` effect backend map:/ { in_map = 1; next }
  in_map && /^A0 summary snapshot:/ { in_map = 0 }
  in_map && /^\| `io\// { print }
' "$map_file" > "$table_rows"

if [[ ! -s "$table_rows" ]]; then
  echo "FAIL: could not extract io backend map table rows from $map_file"
  exit 1
fi

trim() {
  printf "%s" "$1" | sed 's/^[[:space:]]*//; s/[[:space:]]*$//'
}

done_count=0
partial_count=0
non_count=0
total_rows=0

while IFS= read -r line; do
  total_rows=$((total_rows + 1))

  IFS='|' read -r _ effect raw impl backend status notes _ <<< "$line"
  status="$(trim "$status")"
  case "$status" in
    done-libuv) done_count=$((done_count + 1)) ;;
    partial-libuv) partial_count=$((partial_count + 1)) ;;
    non-libuv) non_count=$((non_count + 1)) ;;
    *)
      echo "FAIL: unknown status '$status' in row:"
      echo "  $line"
      exit 1
      ;;
  esac

  impl="$(trim "$impl")"
  impl="${impl//\`/}"
  IFS=',' read -r -a impl_parts <<< "$impl"
  for path in "${impl_parts[@]}"; do
    path="$(trim "$path")"
    [[ -z "$path" ]] && continue
    printf "%s\n" "$path" >> "$impl_files"
  done
done < "$table_rows"

sort -u "$impl_files" -o "$impl_files"

parse_summary_count() {
  local kind="$1"
  local out_file="$2"
  if ! sed -n "s/^- \`${kind}\`: \\([0-9][0-9]*\\)\\/\\([0-9][0-9]*\\).*/\\1 \\2/p" "$map_file" | head -n 1 > "$out_file"; then
    return 1
  fi
  [[ -s "$out_file" ]]
}

done_summary="$tmp_dir/done_summary.txt"
partial_summary="$tmp_dir/partial_summary.txt"
non_summary="$tmp_dir/non_summary.txt"

if ! parse_summary_count "done-libuv" "$done_summary"; then
  echo "FAIL: missing or malformed done-libuv summary count in $map_file"
  exit 1
fi
if ! parse_summary_count "partial-libuv" "$partial_summary"; then
  echo "FAIL: missing or malformed partial-libuv summary count in $map_file"
  exit 1
fi
if ! parse_summary_count "non-libuv" "$non_summary"; then
  echo "FAIL: missing or malformed non-libuv summary count in $map_file"
  exit 1
fi

read -r done_expected done_total < "$done_summary"
read -r partial_expected partial_total < "$partial_summary"
read -r non_expected non_total < "$non_summary"

if [[ "$done_total" != "$partial_total" || "$done_total" != "$non_total" ]]; then
  echo "FAIL: inconsistent summary totals in $map_file"
  echo "  done-libuv total: $done_total"
  echo "  partial-libuv total: $partial_total"
  echo "  non-libuv total: $non_total"
  exit 1
fi

if [[ "$done_total" -ne "$total_rows" ]]; then
  echo "FAIL: summary total ($done_total) does not match backend-map rows ($total_rows)"
  exit 1
fi

if [[ "$done_expected" -ne "$done_count" ||
      "$partial_expected" -ne "$partial_count" ||
      "$non_expected" -ne "$non_count" ]]; then
  echo "FAIL: status summary drift detected in $map_file"
  echo "  done-libuv: summary=$done_expected computed=$done_count"
  echo "  partial-libuv: summary=$partial_expected computed=$partial_count"
  echo "  non-libuv: summary=$non_expected computed=$non_count"
  exit 1
fi

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

changed_files="$tmp_dir/changed_files.txt"
collect_changed_files | sed '/^[[:space:]]*$/d' | sort -u > "$changed_files"

if [[ -s "$changed_files" ]]; then
  mapped_changes="$tmp_dir/mapped_changes.txt"
  comm -12 "$changed_files" "$impl_files" > "$mapped_changes"

  map_touched=0
  if grep -Fxq "$map_file" "$changed_files"; then
    map_touched=1
  fi

  if [[ -s "$mapped_changes" && "$map_touched" -eq 0 ]]; then
    echo "FAIL: io implementation files changed without parity map update: $map_file"
    sed 's/^/  - /' "$mapped_changes"
    exit 1
  fi
fi

echo "OK: io parity status map guard passed."
echo "  done-libuv: $done_count"
echo "  partial-libuv: $partial_count"
echo "  non-libuv: $non_count"
echo "  total rows: $total_rows"
