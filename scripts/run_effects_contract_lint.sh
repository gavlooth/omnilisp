#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

out_json="${1:-build/effects_contract_lint_summary.json}"
mkdir -p "$(dirname "$out_json")"

policy_log="build/effects_contract_policy.log"
libuv_policy_log="build/libuv_surface_policy.log"
docs_log="build/primitive_docs_parity.log"

policy_ok=1
libuv_policy_ok=1
docs_ok=1

if ! scripts/check_effects_contract_policy.sh >"$policy_log" 2>&1; then
  policy_ok=0
fi

if ! scripts/check_libuv_surface_policy.sh >"$libuv_policy_log" 2>&1; then
  libuv_policy_ok=0
fi

if ! scripts/check_primitive_docs_parity.sh >"$docs_log" 2>&1; then
  docs_ok=0
fi

policy_bool="true"
libuv_policy_bool="true"
docs_bool="true"
if [[ "$policy_ok" != "1" ]]; then policy_bool="false"; fi
if [[ "$libuv_policy_ok" != "1" ]]; then libuv_policy_bool="false"; fi
if [[ "$docs_ok" != "1" ]]; then docs_bool="false"; fi

generated_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
diff_range="${OMNI_EFFECTS_POLICY_RANGE:-${OMNI_BOUNDARY_POLICY_RANGE:-}}"

cat > "$out_json" <<EOF
{
  "generated_at_utc": "${generated_at}",
  "diff_range": "${diff_range}",
  "checks": {
    "effects_contract_policy": {
      "passed": ${policy_bool},
      "log": "${policy_log}"
    },
    "libuv_surface_policy": {
      "passed": ${libuv_policy_bool},
      "log": "${libuv_policy_log}"
    },
    "primitive_docs_parity": {
      "passed": ${docs_bool},
      "log": "${docs_log}"
    }
  }
}
EOF

if [[ "$policy_ok" == "1" && "$libuv_policy_ok" == "1" && "$docs_ok" == "1" ]]; then
  echo "OK: effects contract lint passed."
  echo "  summary: $out_json"
  echo "  policy log: $policy_log"
  echo "  libuv policy log: $libuv_policy_log"
  echo "  docs log: $docs_log"
  exit 0
fi

echo "FAIL: effects contract lint failed."
echo "  summary: $out_json"
echo "  policy log: $policy_log"
echo "  libuv policy log: $libuv_policy_log"
echo "  docs log: $docs_log"
echo ""
if [[ "$policy_ok" != "1" ]]; then
  echo "=== effects-contract policy log ==="
  cat "$policy_log"
fi
if [[ "$libuv_policy_ok" != "1" ]]; then
  echo "=== libuv surface policy log ==="
  cat "$libuv_policy_log"
fi
if [[ "$docs_ok" != "1" ]]; then
  echo "=== primitive docs parity log ==="
  cat "$docs_log"
fi
exit 1
