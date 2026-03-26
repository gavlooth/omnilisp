#!/usr/bin/env bash
set -euo pipefail

log_file="${1:-build/boundary_profile_memory_lifetime_bench.log}"
out_json="${2:-build/boundary_profile_summary.json}"

extract_line() {
  local pattern="$1"
  grep -aE "$pattern" "$log_file" | tail -n 1 || true
}

extract_field_from_line() {
  local line="$1"
  local key="$2"
  echo "$line" | tr ' ' '\n' | awk -F= -v k="$key" '$1 == k { print $2; exit }'
}

json_num_or_null() {
  local val="$1"
  if [[ -n "$val" ]]; then
    printf "%s" "$val"
  else
    printf "null"
  fi
}

json_str_or_null() {
  local val="$1"
  if [[ -n "$val" ]]; then
    printf '"%s"' "$val"
  else
    printf "null"
  fi
}

ratio_or_null() {
  local numerator="$1"
  local denominator="$2"
  if [[ -z "$numerator" || -z "$denominator" || "$denominator" == "0" ]]; then
    printf "null"
    return
  fi
  awk -v n="$numerator" -v d="$denominator" 'BEGIN { printf "%.6f", n / d }'
}

percent_string_or_null() {
  local ratio="$1"
  if [[ -z "$ratio" || "$ratio" == "null" ]]; then
    printf "null"
    return
  fi
  awk -v r="$ratio" 'BEGIN { printf "\"%.2f%%\"", r * 100.0 }'
}

display_or_na() {
  local val="$1"
  if [[ -z "$val" || "$val" == "null" ]]; then
    printf "n/a"
  else
    printf "%s" "$val"
  fi
}

if [[ ! -f "$log_file" ]]; then
  echo "missing profile log: $log_file" >&2
  exit 1
fi

decision_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=boundary_decision_cost( |$)')"
partial_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=boundary_destination_routed_escape( |$)')"
tail_line="$(extract_line '^OMNI_BENCH_SUMMARY suite=scope_splice_tail( |$)')"
boundary_line="$(extract_line '^OMNI_TEST_SUMMARY suite=boundary_decisions( |$)')"
traversal_line="$(extract_line '^OMNI_TEST_SUMMARY suite=boundary_traversal( |$)')"
telemetry_line="$(extract_line '^\[boundary\]\[telemetry\]\[tests\] splice_attempted=')"

decision_iters="$(extract_field_from_line "$decision_line" "iters")"
decision_splice_ms="$(extract_field_from_line "$decision_line" "splice_ms")"
decision_disallowed_ms="$(extract_field_from_line "$decision_line" "disallowed_ms")"
decision_reuse_ms="$(extract_field_from_line "$decision_line" "reuse_ms")"
decision_splice_ok="$(extract_field_from_line "$decision_line" "splice_ok")"
decision_disallowed_ok="$(extract_field_from_line "$decision_line" "disallowed_ok")"
decision_reuse_ok="$(extract_field_from_line "$decision_line" "reuse_ok")"

partial_iters="$(extract_field_from_line "$partial_line" "iters")"
partial_ms="$(extract_field_from_line "$partial_line" "partial_ms")"
partial_ok="$(extract_field_from_line "$partial_line" "partial_ok")"

tail_iters="$(extract_field_from_line "$tail_line" "iters")"
tail_escapes_per_iter="$(extract_field_from_line "$tail_line" "escapes_per_iter")"
tail_splice_ms="$(extract_field_from_line "$tail_line" "splice_ms")"
tail_splice_ok="$(extract_field_from_line "$tail_line" "splice_ok")"

boundary_splice_attempted="$(extract_field_from_line "$boundary_line" "splice_attempted")"
boundary_splice_succeeded="$(extract_field_from_line "$boundary_line" "splice_succeeded")"
boundary_splice_fail_total="$(extract_field_from_line "$boundary_line" "splice_fail_total")"
boundary_promotion_attempted="$(extract_field_from_line "$boundary_line" "promotion_attempted")"
boundary_promotion_aborted_budget="$(extract_field_from_line "$boundary_line" "promotion_aborted_budget")"
boundary_promotion_aborted_pre="$(extract_field_from_line "$boundary_line" "promotion_aborted_pre_aborted")"
boundary_copy_fallback_total="$(extract_field_from_line "$boundary_line" "copy_fallback_total")"

telemetry_scope_chain_total="$(extract_field_from_line "$telemetry_line" "scope_chain_scan_total")"
telemetry_scope_chain_with_hint="$(extract_field_from_line "$telemetry_line" "scope_chain_scan_with_hint")"
telemetry_scope_chain_fallback="$(extract_field_from_line "$telemetry_line" "scope_chain_scan_fallback")"
telemetry_scope_chain_suppressed="$(extract_field_from_line "$telemetry_line" "scope_chain_scan_suppressed")"
telemetry_graph_audit_invoked="$(extract_field_from_line "$telemetry_line" "graph_audit_invoked")"
telemetry_graph_audit_skipped_rate="$(extract_field_from_line "$telemetry_line" "graph_audit_skipped_rate")"
telemetry_graph_audit_skipped_max_roots="$(extract_field_from_line "$telemetry_line" "graph_audit_skipped_max_roots")"

traversal_copy_total="$(extract_field_from_line "$traversal_line" "copy_total")"
traversal_copy_fast_reuse="$(extract_field_from_line "$traversal_line" "copy_fast_reuse")"
traversal_copy_defensive="$(extract_field_from_line "$traversal_line" "copy_defensive")"
traversal_promoted_then_spliced="$(extract_field_from_line "$traversal_line" "promoted_then_spliced")"
traversal_promoted_then_fallback_copied="$(extract_field_from_line "$traversal_line" "promoted_then_fallback_copied")"

hint_hit_count=""
if [[ -n "$telemetry_scope_chain_with_hint" && -n "$telemetry_scope_chain_fallback" ]]; then
  hint_hit_count="$((telemetry_scope_chain_with_hint - telemetry_scope_chain_fallback))"
fi
hint_hit_ratio="$(ratio_or_null "$hint_hit_count" "$telemetry_scope_chain_with_hint")"
hint_miss_ratio="$(ratio_or_null "$telemetry_scope_chain_fallback" "$telemetry_scope_chain_with_hint")"
scan_suppression_ratio="$(ratio_or_null "$telemetry_scope_chain_suppressed" "$telemetry_scope_chain_total")"
hint_hit_ratio_display="$(percent_string_or_null "$hint_hit_ratio")"
hint_miss_ratio_display="$(percent_string_or_null "$hint_miss_ratio")"
scan_suppression_ratio_display="$(percent_string_or_null "$scan_suppression_ratio")"

outcome_splice_commit="$decision_splice_ok"
outcome_disallowed="$decision_disallowed_ok"
outcome_reuse="$decision_reuse_ok"
outcome_destination_built="$partial_ok"
outcome_direct_promoted=""
measured_total=""
dominant_name=""
dominant_count=""
dominant_ties=0

if [[ -n "$outcome_splice_commit" && -n "$outcome_disallowed" && -n "$outcome_reuse" && -n "$outcome_destination_built" ]]; then
  measured_total="$((outcome_splice_commit + outcome_disallowed + outcome_reuse + outcome_destination_built))"
  while IFS=':' read -r name count; do
    if [[ -z "$dominant_name" || "$count" -gt "$dominant_count" ]]; then
      dominant_name="$name"
      dominant_count="$count"
      dominant_ties=0
    elif [[ "$count" -eq "$dominant_count" ]]; then
      dominant_ties=1
    fi
  done <<EOF
splice-commit-success:${outcome_splice_commit}
disallowed-fallback:${outcome_disallowed}
reuse-in-target-chain:${outcome_reuse}
destination-built-partial:${outcome_destination_built}
EOF
fi

dominant_label="$dominant_name"
if [[ -n "$dominant_name" && "$dominant_ties" == "1" ]]; then
  dominant_label="balanced"
fi
dominant_share="$(ratio_or_null "$dominant_count" "$measured_total")"
dominant_share_display="$(percent_string_or_null "$dominant_share")"

scope_chain_pressure_summary="total=$(display_or_na "$telemetry_scope_chain_total") with_hint=$(display_or_na "$telemetry_scope_chain_with_hint") hint_hits=$(display_or_na "$hint_hit_count") fallback=$(display_or_na "$telemetry_scope_chain_fallback") suppressed=$(display_or_na "$telemetry_scope_chain_suppressed") hit_ratio=$(display_or_na "$hint_hit_ratio_display") miss_ratio=$(display_or_na "$hint_miss_ratio_display") suppression_ratio=$(display_or_na "$scan_suppression_ratio_display")"
dominant_return_path_summary="outcome=$(display_or_na "$dominant_label") share=$(display_or_na "$dominant_share_display") count=$(display_or_na "$dominant_count") total=$(display_or_na "$measured_total")"

generated_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

mkdir -p "$(dirname "$out_json")"
cat > "$out_json" <<EOF
{
  "generated_at_utc": "${generated_at}",
  "source_log": "${log_file}",
  "benchmarks": {
    "boundary_decision_cost": {
      "iters": $(json_num_or_null "$decision_iters"),
      "splice_ms": $(json_num_or_null "$decision_splice_ms"),
      "disallowed_ms": $(json_num_or_null "$decision_disallowed_ms"),
      "reuse_ms": $(json_num_or_null "$decision_reuse_ms"),
      "splice_ok": $(json_num_or_null "$decision_splice_ok"),
      "disallowed_ok": $(json_num_or_null "$decision_disallowed_ok"),
      "reuse_ok": $(json_num_or_null "$decision_reuse_ok")
    },
    "boundary_destination_routed_escape": {
      "iters": $(json_num_or_null "$partial_iters"),
      "partial_ms": $(json_num_or_null "$partial_ms"),
      "partial_ok": $(json_num_or_null "$partial_ok")
    },
    "scope_splice_tail": {
      "iters": $(json_num_or_null "$tail_iters"),
      "escapes_per_iter": $(json_num_or_null "$tail_escapes_per_iter"),
      "splice_ms": $(json_num_or_null "$tail_splice_ms"),
      "splice_ok": $(json_num_or_null "$tail_splice_ok")
    }
  },
  "boundary_decisions": {
    "splice_attempted": $(json_num_or_null "$boundary_splice_attempted"),
    "splice_succeeded": $(json_num_or_null "$boundary_splice_succeeded"),
    "splice_fail_total": $(json_num_or_null "$boundary_splice_fail_total"),
    "promotion_attempted": $(json_num_or_null "$boundary_promotion_attempted"),
    "promotion_aborted_budget": $(json_num_or_null "$boundary_promotion_aborted_budget"),
    "promotion_aborted_pre_aborted": $(json_num_or_null "$boundary_promotion_aborted_pre"),
    "copy_fallback_total": $(json_num_or_null "$boundary_copy_fallback_total")
  },
  "boundary_traversal": {
    "copy_total": $(json_num_or_null "$traversal_copy_total"),
    "copy_fast_reuse": $(json_num_or_null "$traversal_copy_fast_reuse"),
    "copy_defensive": $(json_num_or_null "$traversal_copy_defensive"),
    "promoted_then_spliced": $(json_num_or_null "$traversal_promoted_then_spliced"),
    "promoted_then_fallback_copied": $(json_num_or_null "$traversal_promoted_then_fallback_copied")
  },
  "boundary_telemetry": {
    "scope_chain_scan_total": $(json_num_or_null "$telemetry_scope_chain_total"),
    "scope_chain_scan_with_hint": $(json_num_or_null "$telemetry_scope_chain_with_hint"),
    "scope_chain_scan_fallback": $(json_num_or_null "$telemetry_scope_chain_fallback"),
    "scope_chain_scan_suppressed": $(json_num_or_null "$telemetry_scope_chain_suppressed"),
    "graph_audit_invoked": $(json_num_or_null "$telemetry_graph_audit_invoked"),
    "graph_audit_skipped_rate": $(json_num_or_null "$telemetry_graph_audit_skipped_rate"),
    "graph_audit_skipped_max_roots": $(json_num_or_null "$telemetry_graph_audit_skipped_max_roots")
  },
  "derived": {
    "scope_chain_pressure": {
      "hint_hit_count": $(json_num_or_null "$hint_hit_count"),
      "hint_hit_ratio": $(json_num_or_null "$hint_hit_ratio"),
      "hint_hit_ratio_display": $hint_hit_ratio_display,
      "hint_miss_ratio": $(json_num_or_null "$hint_miss_ratio"),
      "hint_miss_ratio_display": $hint_miss_ratio_display,
      "scan_suppression_ratio": $(json_num_or_null "$scan_suppression_ratio"),
      "scan_suppression_ratio_display": $scan_suppression_ratio_display
    },
    "return_path_outcome_mix": {
      "splice_commit_success": $(json_num_or_null "$outcome_splice_commit"),
      "disallowed_fallback": $(json_num_or_null "$outcome_disallowed"),
      "reuse_in_target_chain": $(json_num_or_null "$outcome_reuse"),
      "destination_built_partial": $(json_num_or_null "$outcome_destination_built"),
      "direct_promoted": $(json_num_or_null "$outcome_direct_promoted"),
      "measured_total": $(json_num_or_null "$measured_total"),
      "dominant_outcome": $(json_str_or_null "$dominant_label"),
      "dominant_count": $(json_num_or_null "$dominant_count"),
      "dominant_share": $(json_num_or_null "$dominant_share"),
      "dominant_share_display": $dominant_share_display
    }
  },
  "summary": {
    "scope_chain_pressure": $(json_str_or_null "$scope_chain_pressure_summary"),
    "dominant_return_path_outcome": $(json_str_or_null "$dominant_return_path_summary")
  },
  "accepted_regression_envelope": {
    "correctness": {
      "splice_ok": 2048,
      "disallowed_ok": 2048,
      "reuse_ok": 2048,
      "partial_ok": 2048,
      "copy_fallback_total": 0,
      "splice_fail_total": 0
    },
    "latency_ms_max": {
      "boundary_decision_cost_splice_ms": 10,
      "boundary_decision_cost_disallowed_ms": 12,
      "boundary_decision_cost_reuse_ms": 6,
      "boundary_destination_routed_escape_partial_ms": 8,
      "scope_splice_tail_splice_ms": 6
    },
    "ratios": {
      "scope_chain_scan_suppressed": 0,
      "hint_miss_ratio_max": 0.75
    }
  }
}
EOF

echo "Wrote boundary profile summary: $out_json"
echo "  scope-chain pressure: $scope_chain_pressure_summary"
echo "  dominant return-path: $dominant_return_path_summary"
