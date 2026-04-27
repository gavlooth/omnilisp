#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

todo_file="TODO.md"
todo_parts_dir="docs/todo_parts"
changelog_file="memory/CHANGELOG.md"
changelog_parts_dir="memory/changelog_parts"
memory_runtime_doc="docs/areas/memory-runtime.md"
types_dispatch_doc="docs/areas/types-dispatch.md"
ffi_foreign_runtime_doc="docs/areas/ffi-foreign-runtime.md"
validation_status_doc="docs/areas/validation-status.md"
session_report_index="docs/SESSION_REPORT.md"
plans_index="docs/plans/README.md"
agents_plan=".agents/PLAN.md"
areas_index="docs/areas/README.md"
fail() {
  echo "FAIL: $1"
  exit 1
}

require_file() {
  local file="$1"
  [[ -f "$file" ]] || fail "missing required file: $file"
}

extract_status() {
  local file="$1"
  local targets=("$file")
  local parts_dir="${file%.md}_parts"
  if [[ -d "$parts_dir" ]]; then
    targets+=("$parts_dir")
  fi
  rg --no-filename '^Status: `' "${targets[@]}" |
    sed -n 's/^Status: `\([^`]*\)`.*/\1/p' |
    head -n 1
}

extract_as_of() {
  local file="$1"
  local targets=("$file")
  local parts_dir="${file%.md}_parts"
  if [[ -d "$parts_dir" ]]; then
    targets+=("$parts_dir")
  fi
  rg --no-filename '^As of: [0-9]{4}-[0-9]{2}-[0-9]{2}$' "${targets[@]}" |
    sed -n 's/^As of: \([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\)$/\1/p' |
    head -n 1
}

extract_advertised_todo_count() {
  local count
  count="$(sed -nE 's/^-?[[:space:]]*Current actionable count: ([0-9]+)\.?$/\1/p' "$todo_file" | head -n 1)"
  echo "$count"
}

count_open_todos() {
  local targets=()
  [[ -f "$todo_file" ]] && targets+=("$todo_file")
  [[ -d "$todo_parts_dir" ]] && targets+=("$todo_parts_dir")
  if (( ${#targets[@]} == 0 )); then
    echo 0
    return
  fi
  (rg -c '^[[:space:]]*-[[:space:]]+\[[[:space:]]\]' "${targets[@]}" || true) |
    awk -F: '{total += $2} END {print total + 0}'
}

find_unlabeled_open_todos() {
  if [[ -d "$todo_parts_dir" ]]; then
    (rg -n '^[[:space:]]*-[[:space:]]+\[[[:space:]]\]' "$todo_parts_dir" || true) |
      awk '$0 !~ /\][[:space:]]+`/'
  fi
}

extract_latest_changelog_date() {
  local targets=("$changelog_file")
  if [[ -d "$changelog_parts_dir" ]]; then
    targets+=("$changelog_parts_dir")
  fi
  rg --no-filename '^## [0-9]{4}-[0-9]{2}-[0-9]{2}([[:space:]]|$)' "${targets[@]}" |
    sed -n 's/^## \([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\).*/\1/p' |
    sort -r |
    head -n 1
}

compare_dates() {
  local left="$1"
  local op="$2"
  local right="$3"
  local left_ts right_ts
  left_ts="$(date -u -d "$left" +%s 2>/dev/null || true)"
  right_ts="$(date -u -d "$right" +%s 2>/dev/null || true)"
  [[ -n "$left_ts" && -n "$right_ts" ]] || fail "invalid date comparison: $left vs $right"
  case "$op" in
    lt) (( left_ts < right_ts ));;
    le) (( left_ts <= right_ts ));;
    gt) (( left_ts > right_ts ));;
    ge) (( left_ts >= right_ts ));;
    eq) (( left_ts == right_ts ));;
    *) fail "unsupported comparison operator: $op";;
  esac
}

require_file "$todo_file"
require_file "$changelog_file"
require_file "$memory_runtime_doc"
require_file "$types_dispatch_doc"
require_file "$ffi_foreign_runtime_doc"
require_file "$validation_status_doc"
require_file "$session_report_index"
require_file "$plans_index"
require_file "$agents_plan"
require_file "$areas_index"

advertised_todo_count="$(extract_advertised_todo_count)"
actual_todo_count="$(count_open_todos)"
if [[ -n "$advertised_todo_count" ]]; then
  [[ "$advertised_todo_count" == "$actual_todo_count" ]] ||
    fail "$todo_file advertises $advertised_todo_count actionable items but open checkbox scan found $actual_todo_count"
  todo_count="$advertised_todo_count"
else
  todo_count="$actual_todo_count"
fi
[[ -n "$todo_count" ]] || fail "could not parse TODO actionable count from $todo_file or $todo_parts_dir"
unlabeled_open_todos="$(find_unlabeled_open_todos)"
[[ -z "$unlabeled_open_todos" ]] || fail "unchecked TODO entries must start with a backtick task ID:
$unlabeled_open_todos"

if [[ "$todo_count" == "0" ]]; then
  if ! grep -Eiq -- '^[[:space:]]*-[[:space:]]*(none currently;|none\.)[[:space:]]*$' "$todo_file"; then
    fail "TODO.md reports 0 actionable items but does not explicitly declare that no live blocker queue is open"
  fi
fi

validate_advertised_todo_count() {
  local file="$1"
  local advertised
  advertised="$(sed -nE 's/.*Current actionable count: ([0-9]+).*/\1/p' "$file" | head -n 1)"
  [[ -n "$advertised" ]] || fail "$file does not advertise the TODO actionable count"
  [[ "$advertised" == "$todo_count" ]] ||
    fail "$file advertises TODO actionable count $advertised but TODO.md reports $todo_count"
}

validate_advertised_todo_count "$plans_index"
validate_advertised_todo_count "$agents_plan"

latest_changelog_date="$(extract_latest_changelog_date)"
[[ -n "$latest_changelog_date" ]] || fail "could not parse latest changelog date from $changelog_file or $changelog_parts_dir"

for doc in "$memory_runtime_doc" "$types_dispatch_doc" "$ffi_foreign_runtime_doc" "$validation_status_doc"; do
  as_of="$(extract_as_of "$doc")"
  [[ -n "$as_of" ]] || fail "could not parse As of date from $doc"
  compare_dates "$as_of" ge "$latest_changelog_date" || fail "$doc is stale: As of $as_of lags latest changelog date $latest_changelog_date"
done

memory_status="$(extract_status "$memory_runtime_doc")"
types_status="$(extract_status "$types_dispatch_doc")"
ffi_status="$(extract_status "$ffi_foreign_runtime_doc")"
validation_status="$(extract_status "$validation_status_doc")"

[[ "$memory_status" == "green" ]] || fail "$memory_runtime_doc must be green on the current fully validated runtime baseline (got $memory_status)"
[[ "$types_status" == "green" ]] || fail "$types_dispatch_doc must be green once the e2e baseline cleanup is closed (got $types_status)"
[[ "$ffi_status" == "green" ]] || fail "$ffi_foreign_runtime_doc must be green after retained FFI interop lanes are implemented and Python/Julia plus polyglot/plugin are out of scope (got $ffi_status)"
if [[ "$validation_status" == "green" ]]; then
  if grep -Eq 'OMNI_SKIP_TLS_INTEGRATION=1|not a replacement for the TLS integration gate' "$validation_status_doc"; then
    fail "$validation_status_doc cannot be green while its latest broad evidence skips TLS integration"
  fi
elif [[ "$validation_status" == "yellow" ]]; then
  [[ "$todo_count" != "0" ]] || fail "$validation_status_doc is yellow but TODO.md reports no open residuals"
  rg -q 'VALIDATION-[0-9]+-' "$todo_file" "$todo_parts_dir" ||
    fail "$validation_status_doc is yellow but no TODO-backed validation residual is open"
else
  fail "$validation_status_doc status must be green or yellow on the current validation baseline (got $validation_status)"
fi

validate_index_line_counts() {
  local index_file="$1"
  local index_dir="$(dirname "$index_file")"
  while IFS= read -r line; do
    local filepath count
    filepath="$(echo "$line" | sed -n 's/.*](\([^)]*\)).*/\1/p')"
    if [[ -z "$filepath" ]]; then
      filepath="$(echo "$line" | sed -n 's/.*(\([^)]*\)).*/\1/p')"
    fi
    count="$(echo "$line" | sed -n 's/.*(\([0-9][0-9]*\) lines).*/\1/p')"
    if [[ -n "$filepath" && -n "$count" ]]; then
      if [[ ! "$filepath" = /* ]]; then
        filepath="$index_dir/$filepath"
      fi
      [[ -f "$filepath" ]] || fail "$index_file references missing part file: $filepath"
      local actual
      actual="$(wc -l < "$filepath")"
      actual="$(echo "$actual" | sed 's/^[[:space:]]*//')"
      [[ "$actual" == "$count" ]] || fail "$index_file lists $count lines for $filepath but actual is $actual"
    fi
  done < <(grep -E '\.md.*\([0-9]+ lines\)' "$index_file" || true)
}

validate_all_index_line_counts() {
  local index_targets=()
  [[ -f "$todo_file" ]] && index_targets+=("$todo_file")
  [[ -d ".agents" ]] && index_targets+=(".agents")
  [[ -d "docs" ]] && index_targets+=("docs")
  [[ -d "memory" ]] && index_targets+=("memory")
  if (( ${#index_targets[@]} == 0 )); then
    return 0
  fi
  while IFS= read -r index_file; do
    validate_index_line_counts "$index_file"
  done < <(rg -l '\([0-9]+ lines\)' "${index_targets[@]}" --glob '*.md' || true)
}

validate_all_index_line_counts

validate_markdown_file_links() {
  local index_file="$1"
  local index_dir="$(dirname "$index_file")"
  while IFS= read -r target; do
    [[ -n "$target" ]] || continue
    [[ "$target" =~ ^[a-zA-Z][a-zA-Z0-9+.-]*: ]] && continue
    target="${target%%#*}"
    target="${target%%[[:space:]]*}"
    [[ -n "$target" ]] || continue
    if [[ ! "$target" = /* ]]; then
      target="$index_dir/$target"
    fi
    [[ -e "$target" ]] || fail "$index_file references missing markdown target: $target"
  done < <(grep -oE '\[[^][]+\]\([^)]+\)' "$index_file" | sed -n 's/.*](\([^)]*\)).*/\1/p')
}

validate_all_markdown_file_links() {
  local link_targets=()
  while IFS= read -r root_markdown; do
    link_targets+=("$root_markdown")
  done < <(find . -maxdepth 1 -type f -name '*.md' -printf '%P\n' | sort)
  [[ -d ".agents" ]] && link_targets+=(".agents")
  [[ -d "docs" ]] && link_targets+=("docs")
  [[ -d "memory" ]] && link_targets+=("memory")
  [[ -d "tooling" ]] && link_targets+=("tooling")
  if (( ${#link_targets[@]} == 0 )); then
    return 0
  fi
  while IFS= read -r markdown_file; do
    validate_markdown_file_links "$markdown_file"
  done < <(rg --files "${link_targets[@]}" --glob '*.md' --glob '!docs/archive/**' --glob '!memory/archive/**' || true)
}

validate_all_markdown_file_links

validate_no_stale_workspace_paths() {
  local stale_root="/home/heefoo/Documents/code/Omni"
  local stale_matches
  stale_matches="$(rg -n -F "$stale_root" README.md docs tooling --glob '*.md' --glob '!docs/archive/**' || true)"
  [[ -z "$stale_matches" ]] || fail "root README and non-archive docs/tooling must not contain stale absolute workspace paths:
$stale_matches"
}

validate_no_stale_workspace_paths

validate_script_executable_modes() {
  local non_executable_runners
  non_executable_runners="$(find scripts -maxdepth 1 -type f -name '*.sh' ! -name 'c3c_limits.sh' ! -perm -111 -print | sort || true)"
  [[ -z "$non_executable_runners" ]] || fail "top-level runner scripts must be executable:
$non_executable_runners"

  [[ ! -x "scripts/c3c_limits.sh" ]] ||
    fail "scripts/c3c_limits.sh is a sourced helper and must not be executable"

  local wrong_tracked_modes
  if command -v jj >/dev/null 2>&1 && [[ -d ".jj" ]]; then
    wrong_tracked_modes="$(jj file list -T 'path ++ " " ++ executable ++ "\n"' 'glob:scripts/*.sh' |
      awk '$1 == "scripts/c3c_limits.sh" {
             if ($2 != "false") print $1 " tracked executable=true (expected false)";
             next
           }
           $2 != "true" {
             print $1 " tracked executable=" $2 " (expected true)"
           }')"
  else
    wrong_tracked_modes="$(git ls-files --stage 'scripts/*.sh' |
      awk '$4 == "scripts/c3c_limits.sh" {
             if ($1 != "100644") print $4 " tracked as " $1 " (expected 100644)";
             next
           }
           $1 != "100755" {
             print $4 " tracked as " $1 " (expected 100755)"
           }')"
  fi
  [[ -z "$wrong_tracked_modes" ]] || fail "top-level runner scripts must have tracked executable modes:
$wrong_tracked_modes"
}

validate_script_executable_modes

validate_first_party_shell_syntax() {
  local failed=()
  local shell_script shebang
  while IFS= read -r shell_script; do
    shebang="$(head -n 1 "$shell_script")"
    if [[ "$shebang" == *bash* ]]; then
      if ! bash -n "$shell_script"; then
        failed+=("bash:$shell_script")
      fi
    elif [[ "$shebang" == *sh* ]]; then
      if ! sh -n "$shell_script"; then
        failed+=("sh:$shell_script")
      fi
    fi
  done < <(rg -l '^#!.*(bash|sh)' scripts deps tooling --glob '!deps/src/**' --glob '!tooling/tree-sitter-omni/node_modules/**' | sort)

  if (( ${#failed[@]} > 0 )); then
    printf -v failed_list '%s\n' "${failed[@]}"
    fail "first-party shell scripts must parse cleanly:
$failed_list"
  fi
}

validate_first_party_shell_syntax

validate_first_party_bash_strict_mode() {
  local missing_strict_mode
  missing_strict_mode="$(rg -l '^#!.*bash' scripts deps tooling --glob '!deps/src/**' --glob '!tooling/tree-sitter-omni/node_modules/**' | sort |
    while IFS= read -r shell_script; do
      [[ "$shell_script" == "scripts/c3c_limits.sh" ]] && continue
      if ! sed -n '1,8p' "$shell_script" | grep -Fxq 'set -euo pipefail'; then
        printf '%s\n' "$shell_script"
      fi
    done)"
  [[ -z "$missing_strict_mode" ]] || fail "first-party Bash scripts must enable strict mode:
$missing_strict_mode"
}

validate_first_party_bash_strict_mode

validate_first_party_posix_sh_strict_mode() {
  local missing_strict_mode
  missing_strict_mode="$(rg -l '^#!.*sh' scripts deps tooling --glob '!deps/src/**' --glob '!tooling/tree-sitter-omni/node_modules/**' | sort |
    while IFS= read -r shell_script; do
      if head -n 1 "$shell_script" | grep -Eq 'bash'; then
        continue
      fi
      if ! sed -n '1,8p' "$shell_script" | grep -Eq '^set -eu($|[[:space:]])'; then
        printf '%s\n' "$shell_script"
      fi
    done)"
  [[ -z "$missing_strict_mode" ]] || fail "first-party POSIX sh scripts must enable set -eu:
$missing_strict_mode"
}

validate_first_party_posix_sh_strict_mode

validate_first_party_python_syntax() {
  local python_files=()
  local python_file
  while IFS= read -r python_file; do
    python_files+=("$python_file")
  done < <(find scripts tools tooling -path 'tooling/tree-sitter-omni/node_modules' -prune -o -type f -name '*.py' -print | sort)

  if (( ${#python_files[@]} == 0 )); then
    return 0
  fi

  python3 - "${python_files[@]}" <<'PY' ||
import ast
import pathlib
import sys

failures = []
for filename in sys.argv[1:]:
    path = pathlib.Path(filename)
    try:
        ast.parse(path.read_text(encoding="utf-8"), filename=filename)
    except SyntaxError as exc:
        failures.append(f"{filename}:{exc.lineno}:{exc.offset}: {exc.msg}")
    except UnicodeDecodeError as exc:
        failures.append(f"{filename}: utf-8 decode failed: {exc}")

if failures:
    print("\n".join(failures), file=sys.stderr)
    sys.exit(1)
PY
    fail "first-party Python files must parse cleanly"
}

validate_first_party_python_syntax

validate_first_party_python_cli_argv_guards() {
  local raw_argv_indexing
  raw_argv_indexing="$(rg -n 'sys\.argv\[[0-9]+\]' scripts tools tooling --glob '*.py' --glob '!tooling/tree-sitter-omni/node_modules/**' || true)"
  [[ -z "$raw_argv_indexing" ]] || fail "first-party Python CLIs must validate arguments before indexing sys.argv:
$raw_argv_indexing"
}

validate_first_party_python_cli_argv_guards

validate_fast_dev_generator_tracked() {
  local generator="tools/fast-dev/generate_fast_dev_project.py"
  require_file "$generator"
  git ls-files --error-unmatch "$generator" >/dev/null 2>&1 ||
    fail "$generator must be tracked because scripts/build_fast_dev.sh invokes it"
}

validate_fast_dev_generator_tracked

validate_first_party_python_entrypoints_tracked() {
  local required_python_files=(
    "scripts/dialectic_mcp_single.py"
    "scripts/fix_json_pointer_options.py"
    "scripts/remove_dup_data_format_raise.py"
    "scripts/remove_dup_jit_raise.py"
    "scripts/remove_dup_runtime_raise.py"
    "scripts/remove_dup_string_raise.py"
    "scripts/tests/test_dialectic_mcp.py"
    "tooling/tests/omni_cli_json_smoke.py"
    "tooling/tests/omni_fmt_smoke.py"
    "tooling/tests/omni_init_smoke.py"
    "tooling/tests/omni_repl_server_smoke.py"
  )
  local missing_or_untracked=()
  local python_file
  for python_file in "${required_python_files[@]}"; do
    if [[ ! -f "$python_file" ]] || ! git ls-files --error-unmatch "$python_file" >/dev/null 2>&1; then
      missing_or_untracked+=("$python_file")
    fi
  done

  if (( ${#missing_or_untracked[@]} > 0 )); then
    printf -v missing_list '%s\n' "${missing_or_untracked[@]}"
    fail "documented first-party Python entrypoints and smoke tests must be tracked:
$missing_list"
  fi
}

validate_first_party_python_entrypoints_tracked

validate_dialectic_mcp_package_smoke() {
  local help_stdout help_stderr
  help_stdout="$(mktemp)"
  help_stderr="$(mktemp)"
  trap 'rm -f "$help_stdout" "$help_stderr"' RETURN

  python3 -m unittest discover -s scripts/tests >/dev/null 2>&1 ||
    fail "Dialectic MCP package unit tests must pass"

  python3 -m scripts.dialectic_mcp.cli --help >"$help_stdout" 2>"$help_stderr" ||
    fail "Dialectic MCP package-module help must exit successfully"
  grep -q '^usage:' "$help_stdout" ||
    fail "Dialectic MCP package-module help must print argparse usage"
  [[ ! -s "$help_stderr" ]] ||
    fail "Dialectic MCP package-module help must not emit stderr:
$(cat "$help_stderr")"
}

validate_dialectic_mcp_package_smoke

validate_lsp_python_entrypoint_help() {
  local entrypoint
  for entrypoint in \
    "tooling/omni-lsp/omni_lsp.py" \
    "tooling/omni-lsp/tests/smoke_test.py" \
    "tooling/omni-lsp/tests/check_json_smoke.py"; do
    local help_stdout help_stderr
    help_stdout="$(mktemp)"
    help_stderr="$(mktemp)"
    python3 "$entrypoint" --help >"$help_stdout" 2>"$help_stderr" ||
      fail "$entrypoint --help must exit successfully"
    grep -q '^usage:' "$help_stdout" ||
      fail "$entrypoint --help must print argparse usage"
    [[ ! -s "$help_stderr" ]] ||
      fail "$entrypoint --help must not emit stderr:
$(cat "$help_stderr")"
    rm -f "$help_stdout" "$help_stderr"
  done
}

validate_lsp_python_entrypoint_help

validate_python_policy_guard_help() {
  local entrypoint
  for entrypoint in \
    "scripts/check_boundary_value_policy_coverage.py" \
    "scripts/check_memory_ownership_inventory.py" \
    "tools/fast-dev/generate_fast_dev_project.py"; do
    local help_stdout help_stderr
    help_stdout="$(mktemp)"
    help_stderr="$(mktemp)"
    python3 "$entrypoint" --help >"$help_stdout" 2>"$help_stderr" ||
      fail "$entrypoint --help must exit successfully"
    grep -q '^usage:' "$help_stdout" ||
      fail "$entrypoint --help must print usage"
    [[ ! -s "$help_stderr" ]] ||
      fail "$entrypoint --help must not emit stderr:
$(cat "$help_stderr")"
    rm -f "$help_stdout" "$help_stderr"
  done
}

validate_python_policy_guard_help

validate_one_off_python_helper_help() {
  local entrypoint
  for entrypoint in \
    "scripts/fix_json_pointer_options.py" \
    "scripts/migrate_big_integer.py" \
    "scripts/remove_dup_data_format_raise.py" \
    "scripts/remove_dup_jit_raise.py" \
    "scripts/remove_dup_runtime_raise.py" \
    "scripts/remove_dup_string_raise.py"; do
    local help_stdout help_stderr
    help_stdout="$(mktemp)"
    help_stderr="$(mktemp)"
    python3 "$entrypoint" --help >"$help_stdout" 2>"$help_stderr" ||
      fail "$entrypoint --help must exit successfully"
    grep -q '^usage:' "$help_stdout" ||
      fail "$entrypoint --help must print usage"
    [[ ! -s "$help_stderr" ]] ||
      fail "$entrypoint --help must not emit stderr:
$(cat "$help_stderr")"
    rm -f "$help_stdout" "$help_stderr"
  done
}

validate_one_off_python_helper_help

validate_first_party_json_syntax() {
  local json_files=()
  local json_file
  while IFS= read -r json_file; do
    json_files+=("$json_file")
  done < <(find . \
    -path './.claude' -prune -o \
    -path './.claude-flow' -prune -o \
    -path './.swarm' -prune -o \
    -path './build' -prune -o \
    -path './deps/src' -prune -o \
    -path './third_party' -prune -o \
    -path './tooling/tree-sitter-omni/node_modules' -prune -o \
    -type f -name '*.json' -print | sort)

  if (( ${#json_files[@]} == 0 )); then
    return 0
  fi

  python3 - "${json_files[@]}" <<'PY' ||
import json
import pathlib
import sys

failures = []
for filename in sys.argv[1:]:
    path = pathlib.Path(filename)
    try:
        with path.open(encoding="utf-8") as handle:
            json.load(handle)
    except json.JSONDecodeError as exc:
        failures.append(f"{filename}:{exc.lineno}:{exc.colno}: {exc.msg}")
    except UnicodeDecodeError as exc:
        failures.append(f"{filename}: utf-8 decode failed: {exc}")

if failures:
    print("\n".join(failures), file=sys.stderr)
    sys.exit(1)
PY
    fail "first-party JSON files must parse cleanly"
}

validate_first_party_json_syntax

validate_first_party_toml_syntax() {
  local toml_files=()
  local toml_file
  while IFS= read -r toml_file; do
    toml_files+=("$toml_file")
  done < <(git ls-files '*.toml' 'Cargo.lock' | sort)

  if (( ${#toml_files[@]} == 0 )); then
    return 0
  fi

  python3 - "${toml_files[@]}" <<'PY' ||
import pathlib
import sys
import tomllib

failures = []
for filename in sys.argv[1:]:
    path = pathlib.Path(filename)
    try:
        with path.open("rb") as handle:
            tomllib.load(handle)
    except tomllib.TOMLDecodeError as exc:
        failures.append(f"{filename}: {exc}")
    except UnicodeDecodeError as exc:
        failures.append(f"{filename}: utf-8 decode failed: {exc}")

if failures:
    print("\n".join(failures), file=sys.stderr)
    sys.exit(1)
PY
    fail "first-party TOML files must parse cleanly"
}

validate_first_party_toml_syntax

validate_no_checked_in_toml_api_keys() {
  local api_key_matches
  api_key_matches="$(rg -n '^[[:space:]]*[A-Za-z0-9_-]*api[_-]?key[[:space:]]*=[[:space:]]*"[^"]+"' . \
    --glob '*.toml' \
    --glob '!deps/src/**' \
    --glob '!third_party/**' \
    --glob '!build/**' \
    --glob '!tooling/tree-sitter-omni/node_modules/**' || true)"
  [[ -z "$api_key_matches" ]] || fail "TOML files must not contain checked-in API keys:
$api_key_matches"
}

validate_no_checked_in_toml_api_keys

validate_boundary_sensitive_files_tracked() {
  local sensitive_file_list="scripts/boundary_sensitive_files.txt"
  require_file "$sensitive_file_list"

  local missing_or_untracked=()
  local raw file
  while IFS= read -r raw || [[ -n "$raw" ]]; do
    file="${raw%%#*}"
    file="${file#"${file%%[![:space:]]*}"}"
    file="${file%"${file##*[![:space:]]}"}"
    [[ -z "$file" ]] && continue
    if [[ ! -e "$file" ]] || ! git ls-files --error-unmatch "$file" >/dev/null 2>&1; then
      missing_or_untracked+=("$file")
    fi
  done < "$sensitive_file_list"

  if (( ${#missing_or_untracked[@]} > 0 )); then
    printf -v missing_list '%s\n' "${missing_or_untracked[@]}"
    fail "$sensitive_file_list entries must exist and be tracked:
$missing_list"
  fi
}

validate_boundary_sensitive_files_tracked

validate_tree_sitter_docs_use_npm_scripts() {
  local direct_cli_docs
  direct_cli_docs="$(rg -n '^[[:space:]]*tree-sitter[[:space:]]+(generate|parse|test|query)' README.md docs tooling --glob '*.md' --glob '!docs/archive/**' || true)"
  [[ -z "$direct_cli_docs" ]] || fail "Tree-sitter docs must use pinned npm scripts instead of a global tree-sitter CLI:
$direct_cli_docs"
}

validate_tree_sitter_docs_use_npm_scripts

validate_tree_sitter_generated_parser_split() {
  local package_file="tooling/tree-sitter-omni/package.json"
  local parser_file="tooling/tree-sitter-omni/src/parser.c"
  local split_script="scripts/split_tree_sitter_parser.sh"

  require_file "$package_file"
  require_file "$parser_file"
  require_file "$split_script"
  [[ -x "$split_script" ]] || fail "$split_script must be executable"

  grep -q -F "split_tree_sitter_parser.sh" "$package_file" ||
    fail "$package_file generate script must run $split_script after tree-sitter generate"

  local parser_body
  parser_body="$(sed -n '/^#include "parser_part_[0-9][0-9]*\.c\.inc"$/p' "$parser_file")"
  [[ -n "$parser_body" ]] || fail "$parser_file must remain a split parser wrapper"
  if grep -q -v '^#include "parser_part_[0-9][0-9]*\.c\.inc"$' "$parser_file"; then
    fail "$parser_file must contain only parser_part include lines"
  fi

  while IFS= read -r include_file; do
    local part_path="tooling/tree-sitter-omni/src/$include_file"
    require_file "$part_path"
  done < <(sed -n 's/^#include "\(parser_part_[0-9][0-9]*\.c\.inc\)"$/\1/p' "$parser_file")
}

validate_tree_sitter_generated_parser_split

validate_validation_status_summary_cli() {
  local summary_script="scripts/run_validation_status_summary.sh"
  require_file "$summary_script"

  local help_output
  help_output="$("$summary_script" --help)"
  [[ "$help_output" == Usage:* ]] ||
    fail "$summary_script --help must print usage without running validation"

  local invalid_rc
  set +e
  "$summary_script" --definitely-invalid-option >/dev/null 2>&1
  invalid_rc=$?
  set -e
  [[ "$invalid_rc" == "2" ]] ||
    fail "$summary_script must reject unknown options before running validation (got rc=$invalid_rc)"
}

validate_validation_status_summary_cli

if raw_tensor_dtors="$(rg -n '(main::)?scope_register_dtor(_escape)?[[:space:]]*\([^;]*&scope_dtor_value' src/lisp/prim_tensor*.c3 src/lisp/value_tensor*.c3 src/lisp/prim_ml_*.c3 || true)"; then
  if [[ -n "$raw_tensor_dtors" ]]; then
    fail "tensor/ML result constructors must use checked destructor registration helpers:
$raw_tensor_dtors"
  fi
fi

while IFS= read -r area_doc; do
  area_name="${area_doc#docs/areas/}"
  [[ "$area_name" == "README.md" ]] && continue
  grep -q -F -- "- \`$area_name\`" "$areas_index" ||
    fail "$areas_index omits area document $area_name"
done < <(find docs/areas -maxdepth 1 -type f -name '*.md' | sort)

validate_plan_status_consistency() {
  local plans_readme="docs/plans/README.md"
  [[ -f "$plans_readme" ]] || return 0
  while IFS= read -r line; do
    local plan_file status
    plan_file="$(echo "$line" | sed -n 's/.*`\([^`]*\.md\)`.*/\1/p')"
    status="$(echo "$line" | sed -n 's/.*`[^`]*\.md`:[[:space:]]*\([^[:space:]]*\).*/\1/p')"
    if [[ -n "$plan_file" && -n "$status" ]]; then
      local plan_path="docs/plans/$plan_file"
      if [[ -f "$plan_path" ]]; then
        local plan_internal_status
        plan_internal_status="$(grep -i '^[[:space:]]*- status:' "$plan_path" | head -n 1 | sed 's/.*status:[[:space:]]*//i' || true)"
        if [[ -n "$plan_internal_status" ]]; then
          if [[ "$status" == "completed" && ! "$plan_internal_status" =~ completed|closed|done ]]; then
            fail "$plans_readme lists $plan_file as completed but its internal status is '$plan_internal_status'"
          fi
        fi
      fi
    fi
  done < <(grep -E '^- `' "$plans_readme" || true)
}

validate_plan_status_consistency

echo "OK: status consistency checks passed."
echo "  latest changelog date: $latest_changelog_date"
echo "  TODO actionable count: $todo_count"
echo "  memory runtime status: $memory_status"
echo "  types dispatch status: $types_status"
echo "  ffi foreign runtime status: $ffi_status"
echo "  validation status: $validation_status"
