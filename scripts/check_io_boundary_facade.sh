#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

stdlib_file="stdlib/stdlib.lisp"
prims_file="src/lisp/eval_init_primitives.c3"
prim_tables_file="src/lisp/eval_init_primitive_tables.c3"

for required in "$stdlib_file" "$prims_file" "$prim_tables_file"; do
  if [[ ! -f "$required" ]]; then
    echo "FAIL: missing required file: $required"
    exit 1
  fi
done

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

effects_stdlib="$tmp_dir/effects_stdlib.txt"
signals_stdlib="$tmp_dir/signals_stdlib.txt"
fastpath_effects="$tmp_dir/fastpath_effects.txt"
fastpath_raws="$tmp_dir/fastpath_raws.txt"
registered_raws="$tmp_dir/registered_raws.txt"
registered_raws_parts="$tmp_dir/registered_raws_parts.txt"

extract_sorted_unique() {
  local src="$1"
  local sed_expr="$2"
  local out="$3"
  sed -n "$sed_expr" "$src" | sort -u > "$out"
}

extract_sorted_unique "$stdlib_file" 's/.*(define \[effect\] (\(io\/[^ )]*\).*/\1/p' "$effects_stdlib"
extract_sorted_unique "$stdlib_file" 's/.*(signal \(io\/[^ )]*\).*/\1/p' "$signals_stdlib"
extract_sorted_unique "$prims_file" 's/.*{ "\(io\/[^"]*\)",.*/\1/p' "$fastpath_effects"
extract_sorted_unique "$prims_file" 's/.*{ "io\/[^"]*", "\(__raw-[^"]*\)".*/\1/p' "$fastpath_raws"
sed -n 's/.*{ "\(__raw-[^"]*\)",.*/\1/p' "$prims_file" "$prim_tables_file" > "$registered_raws_parts"
sort -u "$registered_raws_parts" > "$registered_raws"

compare_sets() {
  local lhs_name="$1"
  local lhs_file="$2"
  local rhs_name="$3"
  local rhs_file="$4"
  local missing_file="$tmp_dir/missing_${lhs_name}_vs_${rhs_name}.txt"
  local extra_file="$tmp_dir/extra_${lhs_name}_vs_${rhs_name}.txt"

  comm -23 "$lhs_file" "$rhs_file" > "$missing_file"
  comm -13 "$lhs_file" "$rhs_file" > "$extra_file"

  if [[ -s "$missing_file" || -s "$extra_file" ]]; then
    echo "FAIL: io boundary facade mismatch: $lhs_name vs $rhs_name"
    if [[ -s "$missing_file" ]]; then
      echo "  Present in $lhs_name, missing in $rhs_name:"
      sed 's/^/    - /' "$missing_file"
    fi
    if [[ -s "$extra_file" ]]; then
      echo "  Present in $rhs_name, missing in $lhs_name:"
      sed 's/^/    - /' "$extra_file"
    fi
    return 1
  fi
  return 0
}

compare_sets "stdlib-effects" "$effects_stdlib" "stdlib-signals" "$signals_stdlib"
compare_sets "stdlib-effects" "$effects_stdlib" "fastpath-effects" "$fastpath_effects"

missing_fastpath_raw="$tmp_dir/missing_fastpath_raw.txt"
comm -23 "$fastpath_raws" "$registered_raws" > "$missing_fastpath_raw"
if [[ -s "$missing_fastpath_raw" ]]; then
  echo "FAIL: io fast-path maps to unregistered raw primitive(s):"
  sed 's/^/  - /' "$missing_fastpath_raw"
  exit 1
fi

direct_raw_calls="$tmp_dir/direct_raw_calls.txt"
awk '
  {
    line = $0
    sub(/;.*/, "", line)
    if (line ~ /\(__raw-[^ )]*/) {
      print FNR ":" $0
    }
  }
' "$stdlib_file" > "$direct_raw_calls"
if [[ -s "$direct_raw_calls" ]]; then
  echo "FAIL: direct __raw-* invocation found in stdlib; use (signal io/...) facade instead."
  sed 's/^/  - /' "$direct_raw_calls"
  exit 1
fi

echo "OK: io boundary facade guard passed."
echo "  stdlib effects: $(wc -l < "$effects_stdlib")"
echo "  fast-path effects: $(wc -l < "$fastpath_effects")"
