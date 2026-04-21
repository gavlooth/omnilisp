#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

: "${OMNI_FILE_SIZE_LIMIT:=1000}"

failures=()
tracked_files=()

load_tracked_files() {
  if command -v jj >/dev/null 2>&1 && jj root >/dev/null 2>&1; then
    while IFS= read -r path; do
      tracked_files+=("$path")
    done < <(jj file list)
    return
  fi

  while IFS= read -r -d '' path; do
    tracked_files+=("$path")
  done < <(git ls-files -z)
}

is_excluded_path() {
  local path="$1"

  case "$path" in
    .claude-flow/tasks/store.json|.codegraph/*)
      return 0
      ;;
    *.bak|*.disabled)
      return 0
      ;;
    *__pycache__*|*.pyc)
      return 0
      ;;
  esac

  return 1
}

is_text_like() {
  local path="$1"
  local mime
  mime="$(file --brief --mime-type -- "$path" 2>/dev/null || true)"
  case "$mime" in
    text/*|application/json|application/x-empty)
      return 0
      ;;
  esac
  return 1
}

is_code_like_path() {
  local path="$1"

  case "$path" in
    Makefile|makefile|CMakeLists.txt)
      return 0
      ;;
    *.c|*.h|*.cc|*.hh|*.cpp|*.hpp|*.cxx|*.hxx|*.inc)
      return 0
      ;;
    *.c3|*.omni|*.sh|*.py|*.js|*.jsx|*.ts|*.tsx|*.rs|*.go|*.zig|*.lua)
      return 0
      ;;
  esac

  return 1
}

load_tracked_files

for path in "${tracked_files[@]}"; do
  if is_excluded_path "$path"; then
    continue
  fi
  if [[ ! -f "$path" ]]; then
    continue
  fi
  if ! is_code_like_path "$path"; then
    continue
  fi
  if ! is_text_like "$path"; then
    continue
  fi

  line_count="$(wc -l < "$path")"
  if (( line_count > OMNI_FILE_SIZE_LIMIT )); then
    failures+=("${line_count} ${path}")
  fi
done

if ((${#failures[@]} > 0)); then
  printf 'FAIL: tracked code files above %s LOC:\n' "$OMNI_FILE_SIZE_LIMIT" >&2
  printf '  %s\n' "${failures[@]}" | sort -nr >&2
  printf '\nExcluded categories: docs/operational artifacts, repo state artifacts, backup/disabled snapshots, bytecode caches, and non-text binaries.\n' >&2
  exit 1
fi

printf 'File size gate passed: no tracked code files above %s LOC.\n' "$OMNI_FILE_SIZE_LIMIT"
