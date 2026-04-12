#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
source scripts/c3c_limits.sh

out_json="${1:-build/validation_status_summary.json}"
log_dir="${OMNI_VALIDATION_STATUS_LOG_DIR:-build/validation_status_logs}"
toolchain_root="${OMNI_VALIDATION_TOOLCHAIN_ROOT:-/usr/local}"

mkdir -p "$(dirname "$out_json")"
mkdir -p "$log_dir"

manifest="$(mktemp)"
trap 'rm -f "$manifest"; omni_validation_lock_release' EXIT
omni_validation_lock_acquire

if [[ -z "${OMNI_VALIDATION_EXTRA_ARGS:-}" && -e /usr/lib/libreplxx.so.0 ]]; then
  export OMNI_VALIDATION_EXTRA_ARGS="--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly"
fi

run_case() {
  local name="$1"
  local subsystem="$2"
  local kind="$3"
  local note="$4"
  shift 4

  local log_path="${log_dir}/${name}.log"
  local cmd_path="${log_dir}/${name}.cmd"
  local started_at
  local finished_at
  local rc

  started_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
  printf "%q " "$@" > "$cmd_path"
  sed -i 's/ $//' "$cmd_path"

  set +e
  "$@" >"$log_path" 2>&1
  rc=$?
  set -e

  finished_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
  printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" \
    "$name" \
    "$subsystem" \
    "$kind" \
    "$note" \
    "$rc" \
    "$started_at" \
    "$finished_at" \
    "$log_path" \
    "$cmd_path" >> "$manifest"
}

run_bounded_slice() {
  local name="$1"
  local subsystem="$2"
  local slice="$3"
  run_case \
    "$name" \
    "$subsystem" \
    "bounded_slice" \
    "bounded validation container slice ${slice}" \
    env OMNI_VALIDATION_TOOLCHAIN_ROOT="$toolchain_root" \
    scripts/run_validation_container.sh \
    env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib \
    OMNI_TEST_QUIET=1 \
    OMNI_TEST_SUMMARY=1 \
    OMNI_SKIP_TLS_INTEGRATION=1 \
    OMNI_LISP_TEST_SLICE="$slice" \
    ./build/main --test-suite lisp
}

run_case \
  "build" \
  "integration" \
  "build" \
  "host-local repo integration build" \
  c3c build

run_case \
  "status_consistency" \
  "status" \
  "policy" \
  "status/docs consistency policy" \
  scripts/check_status_consistency.sh

run_case \
  "e2e_live" \
  "integration" \
  "e2e" \
  "local entrypoint, Docker-bound e2e compiler execution" \
  scripts/run_e2e.sh

run_case \
  "e2e_baseline_policy" \
  "types-dispatch" \
  "policy" \
  "e2e baseline manifest/owner policy" \
  scripts/check_e2e_baseline_policy.sh

run_case \
  "ftxui_smoke" \
  "ui" \
  "smoke" \
  "host-local FTXUI example smoke gate" \
  scripts/run_ftxui_smoke.sh

run_case \
  "bounded_container_build" \
  "integration" \
  "build" \
  "bounded validation container build for runtime slices" \
  scripts/run_validation_container.sh c3c build

run_bounded_slice "jit_policy" "memory-runtime" "jit-policy"
run_bounded_slice "scheduler" "scheduler" "scheduler"
run_bounded_slice "deduce" "deduce" "deduce"
run_bounded_slice "compiler" "compiler" "compiler"
run_bounded_slice "memory_lifetime_smoke" "memory-runtime" "memory-lifetime-smoke"
run_bounded_slice "advanced" "memory-runtime" "advanced"

python3 - "$manifest" "$out_json" <<'PY'
import json
import sys
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

manifest_path = Path(sys.argv[1])
out_path = Path(sys.argv[2])


def coerce(value: str):
    if value.isdigit():
        return int(value)
    if value.startswith("-") and value[1:].isdigit():
        return int(value)
    if value in ("true", "false"):
        return value == "true"
    return value


def parse_summary_lines(log_path: Path):
    rows = []
    if not log_path.exists():
        return rows
    for line in log_path.read_text(encoding="utf-8", errors="replace").splitlines():
        if not line.startswith("OMNI_TEST_SUMMARY "):
            continue
        row = {}
        for token in line.split()[1:]:
            if "=" not in token:
                continue
            key, value = token.split("=", 1)
            row[key] = coerce(value)
        rows.append(row)
    return rows


runs = []
for raw in manifest_path.read_text(encoding="utf-8").splitlines():
    if not raw.strip():
        continue
    name, subsystem, kind, note, rc, started_at, finished_at, log_path, cmd_path = raw.split("\t")
    log = Path(log_path)
    cmd = Path(cmd_path)
    status = "pass" if int(rc) == 0 else "fail"
    runs.append(
        {
            "name": name,
            "subsystem": subsystem,
            "kind": kind,
            "note": note,
            "command": cmd.read_text(encoding="utf-8").strip(),
            "status": status,
            "exit_code": int(rc),
            "started_at_utc": started_at,
            "finished_at_utc": finished_at,
            "log_path": log_path,
            "summaries": parse_summary_lines(log),
        }
    )

status_counts = Counter(run["status"] for run in runs)
artifact = {
    "artifact": "validation_status_summary",
    "artifact_version": 2,
    "generated_at_utc": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
    "overall_status": "pass" if status_counts.get("fail", 0) == 0 else "fail",
    "run_counts": {
        "total": len(runs),
        "pass": status_counts.get("pass", 0),
        "fail": status_counts.get("fail", 0),
    },
    "runs": runs,
}

out_path.write_text(json.dumps(artifact, indent=2) + "\n", encoding="utf-8")
PY

echo "Validation status summary: $out_json"

python3 - "$out_json" <<'PY'
import json
import sys
from pathlib import Path

data = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
if data.get("overall_status") != "pass":
    sys.exit(1)

missing_summary_telemetry = [
    run["name"]
    for run in data.get("runs", [])
    if run.get("kind") == "bounded_slice" and not run.get("summaries")
]
if missing_summary_telemetry:
    print(
        "missing OMNI_TEST_SUMMARY telemetry for bounded slices: "
        + ", ".join(missing_summary_telemetry),
        file=sys.stderr,
    )
    sys.exit(1)

live_e2e_runs = [
    run for run in data.get("runs", [])
    if run.get("kind") == "e2e"
]
if len(live_e2e_runs) != 1:
    print(
        "missing live e2e execution evidence in validation summary",
        file=sys.stderr,
    )
    sys.exit(1)

live_e2e_run = live_e2e_runs[0]
if live_e2e_run.get("status") != "pass":
    print(
        f"live e2e execution failed: {live_e2e_run.get('name')}",
        file=sys.stderr,
    )
    sys.exit(1)

log_text = Path(live_e2e_run["log_path"]).read_text(encoding="utf-8", errors="replace")
if "=== Stage 4: Running e2e tests ===" not in log_text:
    print(
        "live e2e execution did not reach Stage 4",
        file=sys.stderr,
    )
    sys.exit(1)
if "ALL " not in log_text and "E2E baseline matched expected diff manifest." not in log_text:
    print(
        "live e2e execution did not emit a passing Stage 5 result",
        file=sys.stderr,
    )
    sys.exit(1)
PY
