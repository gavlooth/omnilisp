#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

OBJ_DIR="build/obj/omni_chelpers"
ARCHIVE="build/libomni_chelpers.a"

mkdir -p "$OBJ_DIR"

sources=(
  "csrc/stack_helpers.c"
  "csrc/ffi_helpers.c"
  "csrc/json_helpers.c"
  "csrc/tls_helpers.c"
  "csrc/uv_helpers.c"
  "csrc/uv_helpers_work.c"
  "csrc/uv_helpers_thread.c"
  "csrc/uv_helpers_pipe.c"
  "csrc/uv_helpers_tcp.c"
  "csrc/uv_helpers_process.c"
  "csrc/uv_helpers_signal.c"
  "csrc/toml_helpers.c"
  "third_party/tomlc17/tomlc17.c"
)

objects=()
for src in "${sources[@]}"; do
  obj="$OBJ_DIR/$(basename "${src%.c}").o"
  cc -O2 -c "$src" -o "$obj"
  objects+=("$obj")
done

ar rcs "$ARCHIVE" "${objects[@]}"
