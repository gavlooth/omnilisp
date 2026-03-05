#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

policy_file="${OMNI_BOUNDARY_FACADE_POLICY_FILE:-scripts/boundary_facade_policy.txt}"

declare -a policy_ignore_globs=()
declare -A policy_allow=()

load_policy() {
  if [[ ! -f "$policy_file" ]]; then
    echo "FAIL: boundary facade policy file missing: $policy_file"
    exit 1
  fi

  while IFS= read -r raw || [[ -n "$raw" ]]; do
    local line="$raw"
    line="${line%%#*}"
    # Trim leading/trailing whitespace.
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    [[ -z "$line" ]] && continue

    local kind a b extra
    read -r kind a b extra <<< "$line"
    case "$kind" in
      ignore_glob)
        if [[ -z "${a:-}" || -n "${b:-}" ]]; then
          echo "FAIL: invalid ignore_glob entry in $policy_file: '$raw'"
          exit 1
        fi
        policy_ignore_globs+=("$a")
        ;;
      allow)
        if [[ -z "${a:-}" || -z "${b:-}" || -n "${extra:-}" ]]; then
          echo "FAIL: invalid allow entry in $policy_file: '$raw'"
          exit 1
        fi
        policy_allow["$a:$b"]=1
        ;;
      *)
        echo "FAIL: unknown policy directive '$kind' in $policy_file"
        exit 1
        ;;
    esac
  done < "$policy_file"
}

is_ignored_file() {
  local file="$1"
  local glob
  for glob in "${policy_ignore_globs[@]}"; do
    case "$file" in
      $glob) return 0 ;;
    esac
  done
  return 1
}

is_allowed_callsite() {
  local symbol="$1"
  local file="$2"
  [[ -n "${policy_allow["$symbol:$file"]:-}" ]]
}

check_symbol_usage() {
  local symbol="$1"
  local -n violations_ref="$2"
  local pattern
  pattern="\\b${symbol}\\("
  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    local file="${line%%:*}"
    if is_ignored_file "$file"; then
      continue
    fi
    if is_allowed_callsite "$symbol" "$file"; then
      continue
    fi
    violations_ref+=("$line")
  done < <(rg -n --no-heading "$pattern" src/lisp || true)
}

main() {
  load_policy

  local -a legacy_symbols=(
    copy_to_parent
    promote_to_escape
    promote_to_root
    copy_env_to_scope_inner
    scope_splice_escapes
  )
  local -a violations=()

  for symbol in "${legacy_symbols[@]}"; do
    check_symbol_usage "$symbol" violations
  done

  if ((${#violations[@]} == 0)); then
    echo "OK: boundary facade guard found no disallowed legacy boundary callsites."
    echo "Policy file: $policy_file"
    return 0
  fi

  echo "FAIL: disallowed direct legacy boundary callsites detected."
  echo "Use boundary_* facade entry points instead."
  echo "Policy file: $policy_file"
  echo ""
  for v in "${violations[@]}"; do
    echo "  $v"
  done
  return 1
}

main "$@"
