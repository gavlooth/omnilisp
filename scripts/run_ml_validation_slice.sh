#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
source scripts/c3c_limits.sh

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" == "1" ]]; then
  : "${OMNI_HARD_MEM_CAP_METHOD:=none}"
else
  : "${OMNI_HARD_MEM_CAP_METHOD:=docker}"
fi
: "${OMNI_ML_VALIDATION_BUILD:=1}"
: "${OMNI_ML_VALIDATION_LOG:=build/ml_validation_slice.log}"
: "${OMNI_ML_VALIDATION_QUIET:=1}"
: "${OMNI_ML_VALIDATION_SUMMARY:=1}"
: "${OMNI_ML_VALIDATION_BENCH:=1}"

if [[ "${OMNI_IN_VALIDATION_CONTAINER:-0}" != "1" ]]; then
  if [[ "$OMNI_HARD_MEM_CAP_METHOD" != "docker" ]]; then
    echo "run_ml_validation_slice.sh requires Docker-bound execution outside validation containers." >&2
    echo "Set OMNI_HARD_MEM_CAP_METHOD=docker (default) or run via scripts/run_validation_container.sh." >&2
    exit 2
  fi

  if [[ "$OMNI_ML_VALIDATION_BUILD" == "1" ]]; then
    echo "=== ML Validation Slice: host build ==="
    omni_c3 build --obj-out obj
  fi

  exec scripts/run_validation_container.sh env \
    "OMNI_ML_VALIDATION_BUILD=0" \
    "OMNI_ML_VALIDATION_LOG=${OMNI_ML_VALIDATION_LOG}" \
    "OMNI_ML_VALIDATION_QUIET=${OMNI_ML_VALIDATION_QUIET}" \
    "OMNI_ML_VALIDATION_SUMMARY=${OMNI_ML_VALIDATION_SUMMARY}" \
    "OMNI_ML_VALIDATION_BENCH=${OMNI_ML_VALIDATION_BENCH}" \
    scripts/run_ml_validation_slice.sh
fi

mkdir -p "$(dirname "$OMNI_ML_VALIDATION_LOG")"

summary_line() {
  local suite="$1"
  grep -aE "OMNI_BENCH_SUMMARY suite=${suite}( |$)" "$OMNI_ML_VALIDATION_LOG" | tail -n 1 || true
}

summary_field() {
  local line="$1"
  local key="$2"
  echo "$line" | tr ' ' '\n' | awk -F= -v k="$key" '$1 == k { print $2; exit }'
}

assert_field_equals() {
  local line="$1"
  local key="$2"
  local expected="$3"
  local actual
  actual="$(summary_field "$line" "$key")"
  if [[ -z "$actual" ]]; then
    echo "FAIL: missing ${key}= in summary line" >&2
    echo "  line: $line" >&2
    return 1
  fi
  if [[ "$actual" != "$expected" ]]; then
    echo "FAIL: expected ${key}=${expected}, got ${actual}" >&2
    echo "  line: $line" >&2
    return 1
  fi
}

echo "=== ML Validation Slice: build ==="
if [[ "$OMNI_ML_VALIDATION_BUILD" == "1" ]]; then
  omni_c3 build --obj-out obj
fi

runtime_ld_library_path="/usr/local/lib"
if [[ -n "${OMNI_DOCKER_TOOLCHAIN_ROOT:-}" ]]; then
  runtime_ld_library_path="/opt/omni-host-toolchain/lib"
fi

run_env=(
  "LD_LIBRARY_PATH=${runtime_ld_library_path}"
  "OMNI_LISP_TEST_SLICE=advanced"
  "OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module"
)
if [[ "$OMNI_ML_VALIDATION_QUIET" == "1" ]]; then
  run_env+=("OMNI_TEST_QUIET=1")
fi
if [[ "$OMNI_ML_VALIDATION_SUMMARY" == "1" ]]; then
  run_env+=("OMNI_TEST_SUMMARY=1")
fi
if [[ "$OMNI_ML_VALIDATION_BENCH" == "1" ]]; then
  run_env+=("OMNI_ML_BENCH=1")
fi

echo ""
echo "=== ML Validation Slice: focused advanced collections ==="
env "${run_env[@]}" ./build/main --test-suite lisp > "$OMNI_ML_VALIDATION_LOG" 2>&1
cat "$OMNI_ML_VALIDATION_LOG"

if [[ "$OMNI_ML_VALIDATION_BENCH" == "1" ]]; then
  inference_line="$(summary_line "ml_inference_oracle")"
  train_line="$(summary_line "ml_training_step_oracle")"

  if [[ -z "$inference_line" || -z "$train_line" ]]; then
    echo "FAIL: missing one or more ML benchmark summary lines." >&2
    echo "  ml_inference_oracle: ${inference_line:-<missing>}" >&2
    echo "  ml_training_step_oracle: ${train_line:-<missing>}" >&2
    exit 1
  fi

  assert_field_equals "$inference_line" "iters" "128"
  assert_field_equals "$inference_line" "inference_ok" "128"
  assert_field_equals "$train_line" "iters" "64"
  assert_field_equals "$train_line" "train_ok" "64"
fi
