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
  "csrc/tensor_blas_helpers.c"
  "csrc/tensor_lapack_helpers.c"
  "csrc/tensor_cuda_helpers.c"
  "csrc/tensor_vulkan_helpers_core.c"
  "csrc/tensor_vulkan_helpers_map_contract.c"
  "csrc/tensor_vulkan_helpers_map_contract_f32.c"
  "csrc/tensor_vulkan_helpers_map_contract_complex.c"
  "csrc/tensor_vulkan_helpers_map_contract_dot.c"
  "csrc/tensor_vulkan_helpers_contract_f64.c"
  "csrc/tensor_vulkan_helpers_contract_f32.c"
  "csrc/tensor_vulkan_helpers_ml_reduction.c"
  "csrc/tensor_vulkan_helpers_ml_softmax.c"
  "csrc/tensor_vulkan_helpers_contract_complex128.c"
  "csrc/tensor_vulkan_helpers_contract_complex64.c"
  "csrc/tensor_vulkan_helpers_map_special.c"
  "csrc/tensor_vulkan_helpers.c"
  "csrc/tensor_vulkan_helpers_dispatch_basic.c"
  "csrc/tensor_vulkan_helpers_dispatch_storage.c"
  "csrc/tensor_vulkan_helpers_dispatch_multi_output.c"
  "csrc/tensor_vulkan_helpers_dispatch_solve_multi.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_status.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_diag_trace.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_norm_rank.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_factorization_f64_primary.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_factorization_f64_secondary.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_factorization_complex_a.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_factorization_complex_b.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_spectral.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_spectral_norm.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_spectral_f32.c"
  "csrc/tensor_vulkan_helpers_matrix_ops_transpose_and_rest.c"
  "csrc/tensor_vulkan_map_add_scalar_f64_spv.c"
  "csrc/tensor_vulkan_map_f32_spv.c"
  "csrc/tensor_vulkan_map_f64_spv.c"
  "csrc/tensor_vulkan_map_complex128_spv.c"
  "csrc/tensor_vulkan_map_complex64_spv.c"
  "csrc/tensor_vulkan_map_complex128_unary_spv.c"
  "csrc/tensor_vulkan_map_complex64_unary_spv.c"
  "csrc/tensor_vulkan_map_complex128_to_real_spv.c"
  "csrc/tensor_vulkan_map_complex64_to_real_spv.c"
  "csrc/tensor_vulkan_map_unary_f32_spv.c"
  "csrc/tensor_vulkan_map_unary_f64_spv.c"
  "csrc/tensor_vulkan_normal_quantile_f32_spv.c"
  "csrc/tensor_vulkan_normal_quantile_f64_spv.c"
  "csrc/tensor_vulkan_round_i64_f32_spv.c"
  "csrc/tensor_vulkan_round_i64_f64_spv.c"
  "csrc/tensor_vulkan_contract_dot_f64_spv.c"
  "csrc/tensor_vulkan_contract_f32_spv.c"
  "csrc/tensor_vulkan_contract_f64_spv.c"
  "csrc/tensor_vulkan_ml_reduction_f32_spv.c"
  "csrc/tensor_vulkan_ml_reduction_f64_spv.c"
  "csrc/tensor_vulkan_ml_softmax_f32_spv.c"
  "csrc/tensor_vulkan_contract_complex128_spv.c"
  "csrc/tensor_vulkan_contract_complex64_spv.c"
  "csrc/tensor_vulkan_transpose_f32_spv.c"
  "csrc/tensor_vulkan_transpose_f64_spv.c"
  "csrc/tensor_vulkan_transpose_complex128_spv.c"
  "csrc/tensor_vulkan_transpose_complex64_spv.c"
  "csrc/tensor_vulkan_diagonal_f32_spv.c"
  "csrc/tensor_vulkan_diagonal_f64_spv.c"
  "csrc/tensor_vulkan_diagonal_complex128_spv.c"
  "csrc/tensor_vulkan_diagonal_complex64_spv.c"
  "csrc/tensor_vulkan_diagonal_matrix_f32_spv.c"
  "csrc/tensor_vulkan_diagonal_matrix_f64_spv.c"
  "csrc/tensor_vulkan_diagonal_matrix_complex128_spv.c"
  "csrc/tensor_vulkan_diagonal_matrix_complex64_spv.c"
  "csrc/tensor_vulkan_trace_f32_spv.c"
  "csrc/tensor_vulkan_trace_f64_spv.c"
  "csrc/tensor_vulkan_trace_complex128_spv.c"
  "csrc/tensor_vulkan_trace_complex64_spv.c"
  "csrc/tensor_vulkan_norm_f32_spv.c"
  "csrc/tensor_vulkan_norm_f64_spv.c"
  "csrc/tensor_vulkan_norm_complex128_spv.c"
  "csrc/tensor_vulkan_norm_complex64_spv.c"
  "csrc/tensor_vulkan_rank_f32_spv.c"
  "csrc/tensor_vulkan_rank_f64_spv.c"
  "csrc/tensor_vulkan_rank_complex128_spv.c"
  "csrc/tensor_vulkan_rank_complex64_spv.c"
  "csrc/tensor_vulkan_lu_f32_spv.c"
  "csrc/tensor_vulkan_lu_f64_spv.c"
  "csrc/tensor_vulkan_lu_complex128_spv.c"
  "csrc/tensor_vulkan_lu_complex64_spv.c"
  "csrc/tensor_vulkan_determinant_f32_spv.c"
  "csrc/tensor_vulkan_determinant_f64_spv.c"
  "csrc/tensor_vulkan_determinant_complex128_spv.c"
  "csrc/tensor_vulkan_determinant_complex64_spv.c"
  "csrc/tensor_vulkan_solve_f32_spv.c"
  "csrc/tensor_vulkan_solve_f64_spv.c"
  "csrc/tensor_vulkan_solve_complex128_spv.c"
  "csrc/tensor_vulkan_solve_complex64_spv.c"
  "csrc/tensor_vulkan_solve_parallel_init_f32_spv.c"
  "csrc/tensor_vulkan_solve_parallel_init_f64_spv.c"
  "csrc/tensor_vulkan_solve_parallel_f32_spv.c"
  "csrc/tensor_vulkan_solve_parallel_f64_spv.c"
  "csrc/tensor_vulkan_solve_multi_pivot_f32_spv.c"
  "csrc/tensor_vulkan_solve_multi_pivot_f64_spv.c"
  "csrc/tensor_vulkan_solve_multi_pivot_reduce_f32_spv.c"
  "csrc/tensor_vulkan_solve_multi_pivot_reduce_f64_spv.c"
  "csrc/tensor_vulkan_solve_multi_pivot_commit_f32_spv.c"
  "csrc/tensor_vulkan_solve_multi_pivot_commit_f64_spv.c"
  "csrc/tensor_vulkan_solve_multi_row_swap_f32_spv.c"
  "csrc/tensor_vulkan_solve_multi_row_swap_f64_spv.c"
  "csrc/tensor_vulkan_solve_multi_factor_f32_spv.c"
  "csrc/tensor_vulkan_solve_multi_factor_f64_spv.c"
  "csrc/tensor_vulkan_solve_multi_eliminate_f32_spv.c"
  "csrc/tensor_vulkan_solve_multi_eliminate_f64_spv.c"
  "csrc/tensor_vulkan_solve_multi_backsolve_f32_spv.c"
  "csrc/tensor_vulkan_solve_multi_backsolve_f64_spv.c"
  "csrc/tensor_vulkan_inverse_f32_spv.c"
  "csrc/tensor_vulkan_inverse_f64_spv.c"
  "csrc/tensor_vulkan_inverse_complex128_spv.c"
  "csrc/tensor_vulkan_inverse_complex64_spv.c"
  "csrc/tensor_vulkan_cholesky_f32_spv.c"
  "csrc/tensor_vulkan_cholesky_f64_spv.c"
  "csrc/tensor_vulkan_cholesky_complex128_spv.c"
  "csrc/tensor_vulkan_cholesky_complex64_spv.c"
  "csrc/tensor_vulkan_qr_f32_spv.c"
  "csrc/tensor_vulkan_qr_f64_spv.c"
  "csrc/tensor_vulkan_qr_complex128_spv.c"
  "csrc/tensor_vulkan_qr_complex64_spv.c"
  "csrc/tensor_vulkan_singular_values_f32_spv.c"
  "csrc/tensor_vulkan_singular_values_f64_spv.c"
  "csrc/tensor_vulkan_singular_values_complex128_spv.c"
  "csrc/tensor_vulkan_singular_values_complex64_spv.c"
  "csrc/tensor_vulkan_svd_f32_spv.c"
  "csrc/tensor_vulkan_svd_f64_spv.c"
  "csrc/tensor_vulkan_symmetric_eigen_f64_spv.c"
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

cxx_helper_sources=(
  "csrc/big_integer_helpers.cpp"
  "csrc/big_float_helpers.cpp"
  "csrc/big_complex_helpers.cpp"
  "csrc/boost_math_helpers.cpp"
)

objects=()
ftxui_objects=()

dep_is_newer_than_object() {
  local obj="$1"
  local depfile="$2"
  local dep_path

  [[ -f "$depfile" ]] || return 1

  while IFS= read -r dep_path; do
    [[ -z "$dep_path" ]] && continue
    [[ "$dep_path" == "$obj" ]] && continue
    [[ "$dep_path" == "\\" ]] && continue
    if [[ -f "$dep_path" && "$dep_path" -nt "$obj" ]]; then
      return 0
    fi
  done < <(sed -e 's/\\$//' -e 's/:/ /' "$depfile" | tr ' ' '\n')

  return 1
}

object_needs_rebuild() {
  local src="$1"
  local obj="$2"
  local depfile="$3"

  [[ ! -f "$obj" ]] && return 0
  [[ "$src" -nt "$obj" ]] && return 0
  if [[ -f "$depfile" ]]; then
    dep_is_newer_than_object "$obj" "$depfile" && return 0
  fi

  return 1
}

archive_needs_rebuild() {
  local archive="$1"
  shift

  [[ ! -f "$archive" ]] && return 0
  local members
  members="$(ar t "$archive" 2>/dev/null || true)"
  local member_count
  member_count="$(printf '%s\n' "$members" | sed '/^$/d' | wc -l)"
  [[ "$member_count" -ne "$#" ]] && return 0

  local obj
  for obj in "$@"; do
    local member="${obj##*/}"
    if ! grep -Fxq "$member" <<< "$members"; then
      return 0
    fi
    [[ "$obj" -nt "$archive" ]] && return 0
  done

  return 1
}

compile_c_source() {
  local src="$1"
  local obj_name="${src//\//__}"
  obj_name="${obj_name//./_}.o"
  local obj="$OBJ_DIR/$obj_name"
  local depfile="$obj.d"
  if object_needs_rebuild "$src" "$obj" "$depfile"; then
    cc -O2 \
      -Ideps/src/yyjson/src \
      -Ideps/src/BearSSL/inc \
      -Ideps/src/libuv/include \
      -MMD -MP -MF "$depfile" \
      -c "$src" -o "$obj"
  fi
  objects+=("$obj")
}

for src in "${sources[@]}"; do
  compile_c_source "$src"
done

compile_cxx_helper_source() {
  local src="$1"
  local obj_name="${src//\//__}"
  obj_name="${obj_name//./_}.o"
  local obj="$OBJ_DIR/$obj_name"
  local depfile="$obj.d"
  if object_needs_rebuild "$src" "$obj" "$depfile"; then
    "${CXX:-c++}" -O2 -std=c++17 -MMD -MP -MF "$depfile" -c "$src" -o "$obj"
  fi
  objects+=("$obj")
}

for src in "${cxx_helper_sources[@]}"; do
  compile_cxx_helper_source "$src"
done

compile_cxx_source() {
  local src="$1"
  local obj_name="${src//\//__}"
  obj_name="${obj_name//./_}.o"
  local obj="$OBJ_DIR/$obj_name"
  local depfile="$obj.d"
  if object_needs_rebuild "$src" "$obj" "$depfile"; then
    "${CXX:-c++}" -O2 -std=c++17 -Ithird_party/ftxui/include -Ithird_party/ftxui/src -MMD -MP -MF "$depfile" -c "$src" -o "$obj"
  fi
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

if archive_needs_rebuild "$ARCHIVE" "${objects[@]}"; then
  ar rcs "$ARCHIVE" "${objects[@]}"
fi

if archive_needs_rebuild "$FTXUI_ARCHIVE" "${ftxui_objects[@]}"; then
  ar rcs "$FTXUI_ARCHIVE" "${ftxui_objects[@]}"
fi
