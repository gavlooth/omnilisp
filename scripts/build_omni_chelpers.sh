#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

OBJ_DIR="build/obj/omni_chelpers"
ARCHIVE="build/libomni_chelpers.a"
FTXUI_ARCHIVE="build/libomni_ftxui.a"

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
ftxui_objects=()
compile_c_source() {
  local src="$1"
  local obj_name="${src//\//__}"
  obj_name="${obj_name//./_}.o"
  local obj="$OBJ_DIR/$obj_name"
  cc -O2 \
    -Ideps/src/yyjson/src \
    -Ideps/src/BearSSL/inc \
    -Ideps/src/libuv/include \
    -c "$src" -o "$obj"
  objects+=("$obj")
}

for src in "${sources[@]}"; do
  compile_c_source "$src"
done

compile_cxx_source() {
  local src="$1"
  local obj_name="${src//\//__}"
  obj_name="${obj_name//./_}.o"
  local obj="$OBJ_DIR/$obj_name"
  "${CXX:-c++}" -O2 -std=c++17 -Ithird_party/ftxui/include -Ithird_party/ftxui/src -c "$src" -o "$obj"
  objects+=("$obj")
  ftxui_objects+=("$obj")
}

while IFS= read -r src; do
  [[ -z "$src" ]] && continue
  compile_cxx_source "$src"
done < <(
  find third_party/ftxui/src/ftxui -type f -name '*.cpp' \
    ! -name '*_test.cpp' \
    ! -name '*_fuzzer.cpp' \
    | sort
)

compile_cxx_source "csrc/ftxui_shim.cpp"

ar rcs "$ARCHIVE" "${objects[@]}"
ar rcs "$FTXUI_ARCHIVE" "${ftxui_objects[@]}"
