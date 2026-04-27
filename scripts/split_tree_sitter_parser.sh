#!/usr/bin/env bash
set -euo pipefail

parser_file="${1:-src/parser.c}"
if [[ ! -f "$parser_file" && -f "tooling/tree-sitter-omni/src/parser.c" ]]; then
  parser_file="tooling/tree-sitter-omni/src/parser.c"
fi

[[ -f "$parser_file" ]] || {
  echo "missing Tree-sitter parser file: $parser_file" >&2
  exit 1
}

parser_dir="$(dirname "$parser_file")"

is_split_wrapper() {
  local include_count=0
  local include_file

  if grep -q -v '^#include "parser_part_[0-9][0-9]*\.c\.inc"$' "$parser_file"; then
    return 1
  fi

  while IFS= read -r include_file; do
    include_count=$((include_count + 1))
    [[ -f "$parser_dir/$include_file" ]] || return 1
  done < <(sed -n 's/^#include "\(parser_part_[0-9][0-9]*\.c\.inc\)"$/\1/p' "$parser_file")

  (( include_count > 0 ))
}

if head -n 1 "$parser_file" | grep -q '^#include "parser_part_[0-9][0-9]*\.c\.inc"$'; then
  if is_split_wrapper; then
    echo "Tree-sitter parser is already split: $parser_file"
    exit 0
  fi
  echo "malformed Tree-sitter split parser wrapper: $parser_file" >&2
  exit 1
fi

max_part_lines="${OMNI_TREE_SITTER_PARSER_MAX_PART_LINES:-700}"
default_part_lines="670 61 700 657 239"
part_lines="${OMNI_TREE_SITTER_PARSER_PART_LINES:-$default_part_lines}"
total_lines="$(wc -l < "$parser_file" | sed 's/^[[:space:]]*//')"

if [[ -z "${OMNI_TREE_SITTER_PARSER_PART_LINES:-}" ]]; then
  hide_symbols_line="$(grep -n '^#ifdef TREE_SITTER_HIDE_SYMBOLS$' "$parser_file" | head -n 1 | cut -d: -f1 || true)"
  if [[ -n "$hide_symbols_line" ]]; then
    fixed_prefix_lines=$((670 + 61 + 700 + 657))
    if (( hide_symbols_line > fixed_prefix_lines + 1 )); then
      part_lines="670 61 700 657 $((hide_symbols_line - fixed_prefix_lines - 1))"
    fi
  fi
fi

tmp_dir="$(mktemp -d "${TMPDIR:-/tmp}/omni-tree-sitter-parser-split.XXXXXX")"
trap 'rm -rf "$tmp_dir"' EXIT

start_line=1
part_index=1

write_part() {
  local count="$1"
  local end_line

  (( count > 0 )) || return 0
  (( start_line <= total_lines )) || return 0

  end_line=$((start_line + count - 1))
  if (( end_line > total_lines )); then
    end_line="$total_lines"
  fi

  sed -n "${start_line},${end_line}p" "$parser_file" > "$tmp_dir/parser_part_${part_index}.c.inc"
  start_line=$((end_line + 1))
  part_index=$((part_index + 1))
}

for count in $part_lines; do
  write_part "$count"
done

while (( start_line <= total_lines )); do
  write_part "$max_part_lines"
done

parts=()
while IFS= read -r -d '' part_file; do
  parts+=("$part_file")
done < <(find "$tmp_dir" -maxdepth 1 -type f -name 'parser_part_*.c.inc' -print0 | sort -z)
((${#parts[@]} > 0)) || {
  echo "Tree-sitter parser split produced no part files: $parser_file" >&2
  exit 1
}

rm -f "$parser_dir"/parser_part_*.c.inc
mv "${parts[@]}" "$parser_dir"/

: > "$parser_file"
for ((i = 1; i < part_index; i++)); do
  printf '#include "parser_part_%d.c.inc"\n' "$i" >> "$parser_file"
done

echo "Split $parser_file into $((part_index - 1)) parser_part_*.c.inc files"
