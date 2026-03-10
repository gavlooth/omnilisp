#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
source scripts/c3c_limits.sh

if ! command -v docker >/dev/null 2>&1; then
  echo "docker not found in PATH" >&2
  exit 127
fi

default_validation_image="$(omni_default_validation_image)"
: "${OMNI_VALIDATION_IMAGE:=$default_validation_image}"
: "${OMNI_VALIDATION_DOCKERFILE:=docker/validation.Dockerfile}"
: "${OMNI_VALIDATION_BUILD_CONTEXT:=.}"
: "${OMNI_VALIDATION_BUILD_PULL:=1}"

if [[ ! -f "$OMNI_VALIDATION_DOCKERFILE" ]]; then
  echo "validation Dockerfile not found: ${OMNI_VALIDATION_DOCKERFILE}" >&2
  exit 2
fi

build_cmd=(
  docker build
  -f "$OMNI_VALIDATION_DOCKERFILE"
  -t "$OMNI_VALIDATION_IMAGE"
)

if [[ "$OMNI_VALIDATION_BUILD_PULL" == "1" ]]; then
  build_cmd+=(--pull)
fi

if [[ -n "${OMNI_VALIDATION_BUILD_ARGS:-}" ]]; then
  # shellcheck disable=SC2206
  extra_build_args=(${OMNI_VALIDATION_BUILD_ARGS})
  build_cmd+=("${extra_build_args[@]}")
fi

build_cmd+=("$OMNI_VALIDATION_BUILD_CONTEXT")

echo "Building validation image: ${OMNI_VALIDATION_IMAGE}"
echo "  dockerfile: ${OMNI_VALIDATION_DOCKERFILE}"
echo "  context:    ${OMNI_VALIDATION_BUILD_CONTEXT}"
"${build_cmd[@]}"

echo "Validation image built: ${OMNI_VALIDATION_IMAGE}"
