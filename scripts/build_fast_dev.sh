#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PROJECT_JSON="$ROOT_DIR/project.json"
FAST_PROFILE="${OMNI_FAST_DEV_PROFILE:-default}"
case "$FAST_PROFILE" in
  default)
    DEFAULT_OUTPUT_DIR="$ROOT_DIR/build/dev-fast"
    DEFAULT_OUTPUT_BIN="$DEFAULT_OUTPUT_DIR/main-dev"
    DEFAULT_BUILD_DIR="$ROOT_DIR/build/dev-fast-obj"
    DEFAULT_PROJECT_DIR="$ROOT_DIR/build/dev-fast-project"
    ;;
  nodeduce)
    DEFAULT_OUTPUT_DIR="$ROOT_DIR/build/dev-fast-nodeduce"
    DEFAULT_OUTPUT_BIN="$DEFAULT_OUTPUT_DIR/main-dev-nodeduce"
    DEFAULT_BUILD_DIR="$ROOT_DIR/build/dev-fast-nodeduce-obj"
    DEFAULT_PROJECT_DIR="$ROOT_DIR/build/dev-fast-nodeduce-project"
    ;;
  *)
    echo "Unknown OMNI_FAST_DEV_PROFILE='$FAST_PROFILE' (expected: default, nodeduce)" >&2
    exit 2
    ;;
esac

OUTPUT_DIR="${OMNI_FAST_DEV_OUTPUT_DIR:-$DEFAULT_OUTPUT_DIR}"
OUTPUT_BIN="${OMNI_FAST_DEV_OUTPUT:-$DEFAULT_OUTPUT_BIN}"
BUILD_DIR="${OMNI_FAST_DEV_BUILD_DIR:-$DEFAULT_BUILD_DIR}"
FAST_PROJECT_DIR="${OMNI_FAST_DEV_PROJECT_DIR:-$DEFAULT_PROJECT_DIR}"
CHELPER_ARCHIVE="$ROOT_DIR/build/libomni_chelpers.a"
FTXUI_ARCHIVE="$ROOT_DIR/build/libomni_ftxui.a"
SOURCE_MANIFEST="$FAST_PROJECT_DIR/sources_manifest.txt"
GENERATOR_SCRIPT="$ROOT_DIR/tools/fast-dev/generate_fast_dev_project.py"

mkdir -p "$BUILD_DIR" "$OUTPUT_DIR" "$(dirname "$OUTPUT_BIN")" "$FAST_PROJECT_DIR"

if [[ ! -f "$CHELPER_ARCHIVE" || ! -f "$FTXUI_ARCHIVE" ]] || find "$ROOT_DIR/csrc" "$ROOT_DIR/third_party/tomlc17" "$ROOT_DIR/third_party/ftxui" "$ROOT_DIR/scripts/build_omni_chelpers.sh" -type f -newer "$CHELPER_ARCHIVE" | grep -q . || find "$ROOT_DIR/csrc" "$ROOT_DIR/third_party/ftxui" "$ROOT_DIR/scripts/build_omni_chelpers.sh" -type f -newer "$FTXUI_ARCHIVE" | grep -q .; then
  "$ROOT_DIR/scripts/build_omni_chelpers.sh" >/dev/null
fi

SOURCE_COUNT="$(
  python3 "$ROOT_DIR/tools/fast-dev/generate_fast_dev_project.py" \
    generate \
    "$ROOT_DIR" \
    "$PROJECT_JSON" \
    "$FAST_PROJECT_DIR/project.json" \
    "$BUILD_DIR" \
    "$OUTPUT_DIR" \
    "$OUTPUT_BIN" \
    "$SOURCE_MANIFEST" \
    "$FAST_PROFILE"
)"

printf 'fast-dev[%s] build: %s C3 sources -> %s\n' "$FAST_PROFILE" "$SOURCE_COUNT" "$OUTPUT_BIN"

if [[ "${1:-}" == "--profile" ]]; then
  python3 "$ROOT_DIR/tools/fast-dev/generate_fast_dev_project.py" profile "$SOURCE_MANIFEST" "$FAST_PROFILE"
  exit 0
fi

if python3 "$ROOT_DIR/tools/fast-dev/generate_fast_dev_project.py" \
  is-up-to-date \
  "$OUTPUT_BIN" \
  "$PROJECT_JSON" \
  "$CHELPER_ARCHIVE" \
  "$FTXUI_ARCHIVE" \
  "$SOURCE_MANIFEST" \
  "$0" \
  "$GENERATOR_SCRIPT"
then
  printf 'fast-dev[%s] build: up to date\n' "$FAST_PROFILE"
  exit 0
fi

exec c3c build main-dev --path "$FAST_PROJECT_DIR" "$@"
