#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
export OMNI_FAST_DEV_PROFILE="nodeduce"
exec "$ROOT_DIR/scripts/build_fast_dev.sh" "$@"
