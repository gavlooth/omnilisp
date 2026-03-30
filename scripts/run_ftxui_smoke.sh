#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

runtime_ld_library_path="${OMNI_FTXUI_RUNTIME_LD_LIBRARY_PATH:-/usr/local/lib}"

run_example() {
  local path="$1"
  LD_LIBRARY_PATH="$runtime_ld_library_path" ./build/main "$path"
}

echo "=== FTXUI smoke: module_value_smoke ==="
run_example examples/libraries/ftxui/module_value_smoke.omni

echo ""
echo "=== FTXUI smoke: module_effect_smoke ==="
run_example examples/libraries/ftxui/module_effect_smoke.omni

echo ""
echo "=== FTXUI smoke: smoke.omni ==="
run_example examples/libraries/ftxui/smoke.omni

echo ""
echo "=== FTXUI smoke: demo.omni ==="
printf q | LD_LIBRARY_PATH="$runtime_ld_library_path" ./build/main examples/libraries/ftxui/demo.omni
