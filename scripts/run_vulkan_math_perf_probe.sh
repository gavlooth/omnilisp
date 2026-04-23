#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

: "${OMNI_VULKAN_MATH_PERF_BUILD:=0}"
: "${OMNI_VULKAN_MATH_PERF_LOG:=build/vulkan_math_perf_probe.log}"
: "${OMNI_VULKAN_MATH_PERF_TIMEOUT:=45s}"
: "${OMNI_VULKAN_MATH_PERF_SCALE:=0}"

if [[ "$OMNI_VULKAN_MATH_PERF_BUILD" == "1" ]]; then
  source scripts/c3c_limits.sh
  omni_c3 build --obj-out obj
fi

mkdir -p "$(dirname "$OMNI_VULKAN_MATH_PERF_LOG")"
: > "$OMNI_VULKAN_MATH_PERF_LOG"

runtime_ld_library_path="${LD_LIBRARY_PATH:-/usr/local/lib}"
if [[ -n "${OMNI_DOCKER_TOOLCHAIN_ROOT:-}" ]]; then
  runtime_ld_library_path="/opt/omni-host-toolchain/lib"
fi

extract_elapsed_ms() {
  sed -n 's/^\[\([0-9][0-9]*\).*/\1/p' <<< "$1"
}

normalize_output() {
  tr ' ' ',' <<< "$1"
}

run_probe() {
  local suite="$1"
  local label="$2"
  local dtype="$3"
  local n="$4"
  local expr="$5"
  local out
  local ms
  local normalized

  echo "=== Vulkan Math Perf Probe: ${label} ===" | tee -a "$OMNI_VULKAN_MATH_PERF_LOG"
  out="$(timeout "$OMNI_VULKAN_MATH_PERF_TIMEOUT" env LD_LIBRARY_PATH="$runtime_ld_library_path" ./build/main --eval "$expr")"
  echo "$out" | tee -a "$OMNI_VULKAN_MATH_PERF_LOG"

  if [[ "$out" == "skip" ]]; then
    echo "OMNI_BENCH_SUMMARY suite=${suite} label=${label} dtype=${dtype} n=${n} available=0" \
      | tee -a "$OMNI_VULKAN_MATH_PERF_LOG"
    return 0
  fi

  ms="$(extract_elapsed_ms "$out")"
  if [[ -z "$ms" ]]; then
    echo "FAIL: ${label} did not produce a timed array result: ${out}" >&2
    return 1
  fi

  normalized="$(normalize_output "$out")"
  echo "OMNI_BENCH_SUMMARY suite=${suite} label=${label} dtype=${dtype} n=${n} available=1 ms=${ms} result=${normalized}" \
    | tee -a "$OMNI_VULKAN_MATH_PERF_LOG"
}

svd_expr() {
  local dtype="$1"
  local n="$2"
  local matrix_expr="$3"
  cat <<EOF
(let (b (ref (tensor-backends) 3)) (if (ref b '$(tr '[:upper:]' '[:lower:]' <<< "$dtype")) (let (start (time-ms) s (to-device (matrix/singular-values (to-device ${matrix_expr} 'vulkan)) 'cpu) finish (time-ms)) (Array (- finish start) (length s) (ref s [0]) (ref s [$(("$n" - 1))]))) 'skip))
EOF
}

eigen_expr() {
  local capability="$1"
  local n="$2"
  local matrix_expr="$3"
  cat <<EOF
(let (b (ref (tensor-backends) 3)) (if (ref b '${capability}) (let (start (time-ms) e (to-device (matrix/eigenvalues (to-device ${matrix_expr} 'vulkan)) 'cpu) finish (time-ms)) (Array (- finish start) (length e) (ref e [0]) (ref e [$(("$n" - 1))]))) 'skip))
EOF
}

general_eigenpairs_expr() {
  local capability="$1"
  local n="$2"
  local matrix_expr="$3"
  cat <<EOF
(let (b (ref (tensor-backends) 3)) (if (ref b '${capability}) (let (start (time-ms) ep (matrix/eigenpairs (to-device ${matrix_expr} 'vulkan)) vals (to-device (ref ep 'values) 'cpu) finish (time-ms)) (Array (- finish start) (length vals) (ref vals [0]) (ref vals [$(("$n" - 1))]) (device (ref ep 'values)) (device (ref ep 'vectors)))) 'skip))
EOF
}

solve_expr() {
  local dtype="$1"
  local n="$2"
  local matrix_expr="$3"
  local rhs_expr="$4"
  cat <<EOF
(let (b (ref (tensor-backends) 3)) (if (ref b '$(tr '[:upper:]' '[:lower:]' <<< "$dtype")) (let (start (time-ms) s (to-device (matrix/solve (to-device ${matrix_expr} 'vulkan) (to-device ${rhs_expr} 'vulkan)) 'cpu) finish (time-ms)) (Array (- finish start) (length s) (ref s [0]) (ref s [$(("$n" - 1))]))) 'skip))
EOF
}

run_probe "vulkan_math_svd" "f64_identity_64" "Float64" "64" \
  "$(svd_expr "Float64" "64" "(matrix/identity 64 Float64)")"
run_probe "vulkan_math_svd" "f64_identity_65" "Float64" "65" \
  "$(svd_expr "Float64" "65" "(matrix/identity 65 Float64)")"
run_probe "vulkan_math_svd" "f64_ones_65" "Float64" "65" \
  "$(svd_expr "Float64" "65" "(Tensor Float64 [65 65] 1.0)")"
run_probe "vulkan_math_svd" "f64_zero_65" "Float64" "65" \
  "$(svd_expr "Float64" "65" "(Tensor Float64 [65 65] 0.0)")"
run_probe "vulkan_math_svd" "f32_identity_65" "Float32" "65" \
  "$(svd_expr "Float32" "65" "(matrix/identity 65 Float32)")"

run_probe "vulkan_math_eigen" "f64_identity_64" "Float64" "64" \
  "$(eigen_expr "float64" "64" "(matrix/identity 64 Float64)")"
run_probe "vulkan_math_eigen" "f64_identity_65" "Float64" "65" \
  "$(eigen_expr "float64" "65" "(matrix/identity 65 Float64)")"
run_probe "vulkan_math_eigen" "f64_ones_65" "Float64" "65" \
  "$(eigen_expr "float64" "65" "(Tensor Float64 [65 65] 1.0)")"
run_probe "vulkan_math_eigen" "c64_zero_65" "Complex64" "65" \
  "$(eigen_expr "complex64" "65" "(Tensor Complex64 [65 65] 0)")"
run_probe "vulkan_math_general_eigenpairs" "f64_exact_complex_shift_2" "Float64" "2" \
  "$(general_eigenpairs_expr "matrix-eigenpairs-float64" "2" "(Tensor Float64 [2 2] [0 -1 1 0])")"
run_probe "vulkan_math_general_eigenpairs" "f32_exact_complex_shift_2" "Float32" "2" \
  "$(general_eigenpairs_expr "matrix-eigenpairs-float32" "2" "(Tensor Float32 [2 2] [0 -1 1 0])")"
run_probe "vulkan_math_general_eigenpairs" "c128_triangular_3" "Complex128" "3" \
  "$(general_eigenpairs_expr "matrix-eigenpairs-complex128" "3" "(Tensor Complex128 [3 3] [(Complex128 4 0) (Complex128 2 1) (Complex128 -1 0) 0 (Complex128 1 -2) (Complex128 3 0) 0 0 (Complex128 -2 1)])")"
run_probe "vulkan_math_general_eigenpairs" "c64_exact_complex_shift_2" "Complex64" "2" \
  "$(general_eigenpairs_expr "matrix-eigenpairs-complex64" "2" "(Tensor Complex64 [2 2] [0 -1 1 0])")"
run_probe "vulkan_math_general_eigenpairs" "f64_mixed_block_deflation_3" "Float64" "3" \
  "$(general_eigenpairs_expr "matrix-eigenpairs-float64" "3" "(Tensor Float64 [3 3] [0 -1 0 1 0 0 0 0 2])")"
run_probe "vulkan_math_general_eigenpairs" "f32_mixed_block_deflation_3" "Float32" "3" \
  "$(general_eigenpairs_expr "matrix-eigenpairs-float32" "3" "(Tensor Float32 [3 3] [0 -1 0 1 0 0 0 0 2])")"
run_probe "vulkan_math_general_eigenpairs" "c128_mixed_block_deflation_3" "Complex128" "3" \
  "$(general_eigenpairs_expr "matrix-eigenpairs-complex128" "3" "(Tensor Complex128 [3 3] [0 -1 0 1 0 0 0 0 2])")"
run_probe "vulkan_math_general_eigenpairs" "c64_mixed_block_deflation_3" "Complex64" "3" \
  "$(general_eigenpairs_expr "matrix-eigenpairs-complex64" "3" "(Tensor Complex64 [3 3] [0 -1 0 1 0 0 0 0 2])")"
run_probe "vulkan_math_solve" "f64_identity_65" "Float64" "65" \
  "$(solve_expr "Float64" "65" "(matrix/identity 65 Float64)" "(Tensor Float64 [65] 1.0)")"
run_probe "vulkan_math_solve" "f64_i_plus_ones_65" "Float64" "65" \
  "$(solve_expr "Float64" "65" "(map + (matrix/identity 65 Float64) (Tensor Float64 [65 65] 1.0))" "(Tensor Float64 [65] 66.0)")"

if [[ "$OMNI_VULKAN_MATH_PERF_SCALE" == "1" ]]; then
  run_probe "vulkan_math_svd" "f64_identity_96" "Float64" "96" \
    "$(svd_expr "Float64" "96" "(matrix/identity 96 Float64)")"
  run_probe "vulkan_math_svd" "f64_identity_128" "Float64" "128" \
    "$(svd_expr "Float64" "128" "(matrix/identity 128 Float64)")"
  run_probe "vulkan_math_svd" "f64_identity_192" "Float64" "192" \
    "$(svd_expr "Float64" "192" "(matrix/identity 192 Float64)")"
  run_probe "vulkan_math_svd" "f64_ones_128" "Float64" "128" \
    "$(svd_expr "Float64" "128" "(Tensor Float64 [128 128] 1.0)")"

  run_probe "vulkan_math_eigen" "f64_identity_128" "Float64" "128" \
    "$(eigen_expr "float64" "128" "(matrix/identity 128 Float64)")"
  run_probe "vulkan_math_eigen" "f64_ones_128" "Float64" "128" \
    "$(eigen_expr "float64" "128" "(Tensor Float64 [128 128] 1.0)")"
  run_probe "vulkan_math_solve" "f64_identity_128" "Float64" "128" \
    "$(solve_expr "Float64" "128" "(matrix/identity 128 Float64)" "(Tensor Float64 [128] 1.0)")"
  run_probe "vulkan_math_solve" "f64_i_plus_ones_128" "Float64" "128" \
    "$(solve_expr "Float64" "128" "(map + (matrix/identity 128 Float64) (Tensor Float64 [128 128] 1.0))" "(Tensor Float64 [128] 129.0)")"
  run_probe "vulkan_math_solve" "f64_identity_192" "Float64" "192" \
    "$(solve_expr "Float64" "192" "(matrix/identity 192 Float64)" "(Tensor Float64 [192] 1.0)")"
fi
