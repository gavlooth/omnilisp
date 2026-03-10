#!/usr/bin/env bash
set -euo pipefail

normal_log="${1:-build/boundary_hardening_normal.log}"
asan_log="${2:-build/boundary_hardening_asan.log}"
out_json="${3:-build/boundary_hardening_summary.json}"

extract_summary_line() {
  local log_file="$1"
  local suite="$2"
  grep -aE "OMNI_TEST_SUMMARY suite=${suite}( |$)" "$log_file" | tail -n 1 || true
}

extract_summary_field_from_line() {
  local line="$1"
  local key="$2"
  echo "$line" | tr ' ' '\n' | awk -F= -v k="$key" '$1 == k { print $2; exit }'
}

extract_summary_field() {
  local log_file="$1"
  local suite="$2"
  local key="$3"
  local line
  line="$(extract_summary_line "$log_file" "$suite")"
  if [[ -z "$line" ]]; then
    return 0
  fi
  extract_summary_field_from_line "$line" "$key"
}

json_num_or_null() {
  local val="$1"
  if [[ -n "$val" ]]; then
    printf "%s" "$val"
  else
    printf "null"
  fi
}

json_bool_from_presence() {
  local val="$1"
  if [[ -n "$val" ]]; then
    printf "true"
  else
    printf "false"
  fi
}

emit_stage_json() {
  local log_file="$1"

  local stack_engine_line
  local scope_region_line
  local unified_line
  local compiler_line
  local harness_line
  local fiber_line
  local boundary_decisions_line

  stack_engine_line="$(extract_summary_line "$log_file" "stack_engine")"
  scope_region_line="$(extract_summary_line "$log_file" "scope_region")"
  unified_line="$(extract_summary_line "$log_file" "unified")"
  compiler_line="$(extract_summary_line "$log_file" "compiler")"
  harness_line="$(extract_summary_line "$log_file" "stack_affinity_harness")"
  fiber_line="$(extract_summary_line "$log_file" "fiber_temp_pool")"
  boundary_decisions_line="$(extract_summary_line "$log_file" "boundary_decisions")"

  local se_pass se_fail sr_pass sr_fail un_pass un_fail cc_pass cc_fail
  local ah_pass ah_fail
  local ft_enabled ft_hits ft_misses ft_returns ft_drops ft_pooled ft_peak
  local ft_ctx_hits ft_ctx_returns ft_ctx_pools
  local ft_lc_clone ft_lc_destroy ft_lc_defer ft_lc_flush
  local ft_eligible_slow ft_bypass_large ft_bypass_escape
  local bd_splice_attempted bd_splice_succeeded bd_splice_fail_total
  local bd_splice_fail_null_parent bd_splice_fail_null_child bd_splice_fail_same_scope bd_splice_fail_not_immediate_child
  local bd_splice_fail_refcount bd_splice_fail_owner_thread bd_splice_fail_parent_lane bd_splice_fail_child_temp_lane bd_splice_fail_child_escape_lane
  local bd_promotion_attempted bd_promotion_aborted_budget bd_promotion_aborted_pre
  local bd_copy_fallback_total bd_copy_fallback_budget bd_copy_fallback_splice_rejected bd_copy_fallback_releasing_scope bd_copy_fallback_mixed_uncertain
  local bd_copy_fallback_splice_reason_ok bd_copy_fallback_splice_reason_null_parent bd_copy_fallback_splice_reason_null_child
  local bd_copy_fallback_splice_reason_same_scope bd_copy_fallback_splice_reason_not_immediate_child bd_copy_fallback_splice_reason_refcount
  local bd_copy_fallback_splice_reason_owner_thread bd_copy_fallback_splice_reason_parent_lane
  local bd_copy_fallback_splice_reason_child_temp_lane bd_copy_fallback_splice_reason_child_escape_lane
  local bd_copy_fallback_tag_cons bd_copy_fallback_tag_error bd_copy_fallback_tag_other
  local bd_copy_fallback_last_tag bd_copy_fallback_last_site bd_copy_fallback_other_last_tag bd_copy_fallback_other_last_site
  local bd_copy_site_generic bd_copy_site_run_jit bd_copy_site_jit_single bd_copy_site_jit_call bd_copy_site_tco
  local bd_copy_site_repl bd_copy_site_prim_resume
  local bd_copy_site_jit_eval_set bd_copy_site_jit_eval_define bd_copy_site_jit_env_extend_root

  se_pass="$(extract_summary_field_from_line "$stack_engine_line" "pass")"
  se_fail="$(extract_summary_field_from_line "$stack_engine_line" "fail")"
  sr_pass="$(extract_summary_field_from_line "$scope_region_line" "pass")"
  sr_fail="$(extract_summary_field_from_line "$scope_region_line" "fail")"
  un_pass="$(extract_summary_field_from_line "$unified_line" "pass")"
  un_fail="$(extract_summary_field_from_line "$unified_line" "fail")"
  cc_pass="$(extract_summary_field_from_line "$compiler_line" "pass")"
  cc_fail="$(extract_summary_field_from_line "$compiler_line" "fail")"
  ah_pass="$(extract_summary_field_from_line "$harness_line" "pass")"
  ah_fail="$(extract_summary_field_from_line "$harness_line" "fail")"

  ft_enabled="$(extract_summary_field_from_line "$fiber_line" "enabled")"
  ft_hits="$(extract_summary_field_from_line "$fiber_line" "hits")"
  ft_misses="$(extract_summary_field_from_line "$fiber_line" "misses")"
  ft_returns="$(extract_summary_field_from_line "$fiber_line" "returns")"
  ft_drops="$(extract_summary_field_from_line "$fiber_line" "drop_frees")"
  ft_pooled="$(extract_summary_field_from_line "$fiber_line" "pooled")"
  ft_peak="$(extract_summary_field_from_line "$fiber_line" "peak")"
  ft_ctx_hits="$(extract_summary_field_from_line "$fiber_line" "ctx_hits")"
  ft_ctx_returns="$(extract_summary_field_from_line "$fiber_line" "ctx_returns")"
  ft_ctx_pools="$(extract_summary_field_from_line "$fiber_line" "ctx_pools")"
  ft_lc_clone="$(extract_summary_field_from_line "$fiber_line" "lc_clone")"
  ft_lc_destroy="$(extract_summary_field_from_line "$fiber_line" "lc_destroy")"
  ft_lc_defer="$(extract_summary_field_from_line "$fiber_line" "lc_defer")"
  ft_lc_flush="$(extract_summary_field_from_line "$fiber_line" "lc_flush")"
  ft_eligible_slow="$(extract_summary_field_from_line "$fiber_line" "eligible_slow")"
  ft_bypass_large="$(extract_summary_field_from_line "$fiber_line" "bypass_large")"
  ft_bypass_escape="$(extract_summary_field_from_line "$fiber_line" "bypass_escape")"

  bd_splice_attempted="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_attempted")"
  bd_splice_succeeded="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_succeeded")"
  bd_splice_fail_total="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_total")"
  bd_splice_fail_null_parent="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_null_parent")"
  bd_splice_fail_null_child="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_null_child")"
  bd_splice_fail_same_scope="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_same_scope")"
  bd_splice_fail_not_immediate_child="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_not_immediate_child")"
  bd_splice_fail_refcount="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_refcount")"
  bd_splice_fail_owner_thread="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_owner_thread")"
  bd_splice_fail_parent_lane="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_parent_lane")"
  bd_splice_fail_child_temp_lane="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_child_temp_lane")"
  bd_splice_fail_child_escape_lane="$(extract_summary_field_from_line "$boundary_decisions_line" "splice_fail_child_escape_lane")"
  bd_promotion_attempted="$(extract_summary_field_from_line "$boundary_decisions_line" "promotion_attempted")"
  bd_promotion_aborted_budget="$(extract_summary_field_from_line "$boundary_decisions_line" "promotion_aborted_budget")"
  bd_promotion_aborted_pre="$(extract_summary_field_from_line "$boundary_decisions_line" "promotion_aborted_pre_aborted")"
  bd_copy_fallback_total="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_total")"
  bd_copy_fallback_budget="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_budget")"
  bd_copy_fallback_splice_rejected="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_rejected")"
  bd_copy_fallback_releasing_scope="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_releasing_scope")"
  bd_copy_fallback_mixed_uncertain="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_mixed_uncertain")"
  bd_copy_fallback_splice_reason_ok="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_ok")"
  bd_copy_fallback_splice_reason_null_parent="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_null_parent")"
  bd_copy_fallback_splice_reason_null_child="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_null_child")"
  bd_copy_fallback_splice_reason_same_scope="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_same_scope")"
  bd_copy_fallback_splice_reason_not_immediate_child="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_not_immediate_child")"
  bd_copy_fallback_splice_reason_refcount="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_refcount")"
  bd_copy_fallback_splice_reason_owner_thread="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_owner_thread")"
  bd_copy_fallback_splice_reason_parent_lane="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_parent_lane")"
  bd_copy_fallback_splice_reason_child_temp_lane="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_child_temp_lane")"
  bd_copy_fallback_splice_reason_child_escape_lane="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_splice_reason_child_escape_lane")"
  bd_copy_fallback_tag_cons="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_tag_cons")"
  bd_copy_fallback_tag_error="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_tag_error")"
  bd_copy_fallback_tag_other="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_tag_other")"
  bd_copy_fallback_last_tag="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_last_tag")"
  bd_copy_fallback_last_site="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_last_site")"
  bd_copy_fallback_other_last_tag="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_other_last_tag")"
  bd_copy_fallback_other_last_site="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_other_last_site")"
  bd_copy_site_generic="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_generic")"
  bd_copy_site_run_jit="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_run_jit")"
  bd_copy_site_jit_single="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_jit_single")"
  bd_copy_site_jit_call="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_jit_call")"
  bd_copy_site_tco="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_tco")"
  bd_copy_site_repl="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_repl")"
  bd_copy_site_prim_resume="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_prim_resume")"
  bd_copy_site_jit_eval_set="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_jit_eval_set")"
  bd_copy_site_jit_eval_define="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_jit_eval_define")"
  bd_copy_site_jit_env_extend_root="$(extract_summary_field_from_line "$boundary_decisions_line" "copy_fallback_site_jit_env_extend_root")"

  cat <<EOF
{
  "stack_engine": {
    "present": $(json_bool_from_presence "$stack_engine_line"),
    "pass": $(json_num_or_null "$se_pass"),
    "fail": $(json_num_or_null "$se_fail")
  },
  "scope_region": {
    "present": $(json_bool_from_presence "$scope_region_line"),
    "pass": $(json_num_or_null "$sr_pass"),
    "fail": $(json_num_or_null "$sr_fail")
  },
  "unified": {
    "present": $(json_bool_from_presence "$unified_line"),
    "pass": $(json_num_or_null "$un_pass"),
    "fail": $(json_num_or_null "$un_fail")
  },
  "compiler": {
    "present": $(json_bool_from_presence "$compiler_line"),
    "pass": $(json_num_or_null "$cc_pass"),
    "fail": $(json_num_or_null "$cc_fail")
  },
  "stack_affinity_harness": {
    "present": $(json_bool_from_presence "$harness_line"),
    "pass": $(json_num_or_null "$ah_pass"),
    "fail": $(json_num_or_null "$ah_fail")
  },
  "fiber_temp_pool": {
    "present": $(json_bool_from_presence "$fiber_line"),
    "enabled": $(json_num_or_null "$ft_enabled"),
    "hits": $(json_num_or_null "$ft_hits"),
    "misses": $(json_num_or_null "$ft_misses"),
    "returns": $(json_num_or_null "$ft_returns"),
    "drop_frees": $(json_num_or_null "$ft_drops"),
    "pooled": $(json_num_or_null "$ft_pooled"),
    "peak": $(json_num_or_null "$ft_peak"),
    "ctx_hits": $(json_num_or_null "$ft_ctx_hits"),
    "ctx_returns": $(json_num_or_null "$ft_ctx_returns"),
    "ctx_pools": $(json_num_or_null "$ft_ctx_pools"),
    "lc_clone": $(json_num_or_null "$ft_lc_clone"),
    "lc_destroy": $(json_num_or_null "$ft_lc_destroy"),
    "lc_defer": $(json_num_or_null "$ft_lc_defer"),
    "lc_flush": $(json_num_or_null "$ft_lc_flush"),
    "eligible_slow": $(json_num_or_null "$ft_eligible_slow"),
    "bypass_large": $(json_num_or_null "$ft_bypass_large"),
    "bypass_escape": $(json_num_or_null "$ft_bypass_escape")
  },
  "boundary_decisions": {
    "present": $(json_bool_from_presence "$boundary_decisions_line"),
    "splice_attempted": $(json_num_or_null "$bd_splice_attempted"),
    "splice_succeeded": $(json_num_or_null "$bd_splice_succeeded"),
    "splice_fail_total": $(json_num_or_null "$bd_splice_fail_total"),
    "splice_fail_null_parent": $(json_num_or_null "$bd_splice_fail_null_parent"),
    "splice_fail_null_child": $(json_num_or_null "$bd_splice_fail_null_child"),
    "splice_fail_same_scope": $(json_num_or_null "$bd_splice_fail_same_scope"),
    "splice_fail_not_immediate_child": $(json_num_or_null "$bd_splice_fail_not_immediate_child"),
    "splice_fail_refcount": $(json_num_or_null "$bd_splice_fail_refcount"),
    "splice_fail_owner_thread": $(json_num_or_null "$bd_splice_fail_owner_thread"),
    "splice_fail_parent_lane": $(json_num_or_null "$bd_splice_fail_parent_lane"),
    "splice_fail_child_temp_lane": $(json_num_or_null "$bd_splice_fail_child_temp_lane"),
    "splice_fail_child_escape_lane": $(json_num_or_null "$bd_splice_fail_child_escape_lane"),
    "promotion_attempted": $(json_num_or_null "$bd_promotion_attempted"),
    "promotion_aborted_budget": $(json_num_or_null "$bd_promotion_aborted_budget"),
    "promotion_aborted_pre_aborted": $(json_num_or_null "$bd_promotion_aborted_pre"),
    "copy_fallback_total": $(json_num_or_null "$bd_copy_fallback_total"),
    "copy_fallback_budget": $(json_num_or_null "$bd_copy_fallback_budget"),
    "copy_fallback_splice_rejected": $(json_num_or_null "$bd_copy_fallback_splice_rejected"),
    "copy_fallback_releasing_scope": $(json_num_or_null "$bd_copy_fallback_releasing_scope"),
    "copy_fallback_mixed_uncertain": $(json_num_or_null "$bd_copy_fallback_mixed_uncertain"),
    "copy_fallback_splice_reason_ok": $(json_num_or_null "$bd_copy_fallback_splice_reason_ok"),
    "copy_fallback_splice_reason_null_parent": $(json_num_or_null "$bd_copy_fallback_splice_reason_null_parent"),
    "copy_fallback_splice_reason_null_child": $(json_num_or_null "$bd_copy_fallback_splice_reason_null_child"),
    "copy_fallback_splice_reason_same_scope": $(json_num_or_null "$bd_copy_fallback_splice_reason_same_scope"),
    "copy_fallback_splice_reason_not_immediate_child": $(json_num_or_null "$bd_copy_fallback_splice_reason_not_immediate_child"),
    "copy_fallback_splice_reason_refcount": $(json_num_or_null "$bd_copy_fallback_splice_reason_refcount"),
    "copy_fallback_splice_reason_owner_thread": $(json_num_or_null "$bd_copy_fallback_splice_reason_owner_thread"),
    "copy_fallback_splice_reason_parent_lane": $(json_num_or_null "$bd_copy_fallback_splice_reason_parent_lane"),
    "copy_fallback_splice_reason_child_temp_lane": $(json_num_or_null "$bd_copy_fallback_splice_reason_child_temp_lane"),
    "copy_fallback_splice_reason_child_escape_lane": $(json_num_or_null "$bd_copy_fallback_splice_reason_child_escape_lane"),
    "copy_fallback_tag_cons": $(json_num_or_null "$bd_copy_fallback_tag_cons"),
    "copy_fallback_tag_error": $(json_num_or_null "$bd_copy_fallback_tag_error"),
    "copy_fallback_tag_other": $(json_num_or_null "$bd_copy_fallback_tag_other"),
    "copy_fallback_last_tag": $(json_num_or_null "$bd_copy_fallback_last_tag"),
    "copy_fallback_last_site": $(json_num_or_null "$bd_copy_fallback_last_site"),
    "copy_fallback_other_last_tag": $(json_num_or_null "$bd_copy_fallback_other_last_tag"),
    "copy_fallback_other_last_site": $(json_num_or_null "$bd_copy_fallback_other_last_site"),
    "copy_fallback_site_generic": $(json_num_or_null "$bd_copy_site_generic"),
    "copy_fallback_site_run_jit": $(json_num_or_null "$bd_copy_site_run_jit"),
    "copy_fallback_site_jit_single": $(json_num_or_null "$bd_copy_site_jit_single"),
    "copy_fallback_site_jit_call": $(json_num_or_null "$bd_copy_site_jit_call"),
    "copy_fallback_site_tco": $(json_num_or_null "$bd_copy_site_tco"),
    "copy_fallback_site_repl": $(json_num_or_null "$bd_copy_site_repl"),
    "copy_fallback_site_prim_resume": $(json_num_or_null "$bd_copy_site_prim_resume"),
    "copy_fallback_site_jit_eval_set": $(json_num_or_null "$bd_copy_site_jit_eval_set"),
    "copy_fallback_site_jit_eval_define": $(json_num_or_null "$bd_copy_site_jit_eval_define"),
    "copy_fallback_site_jit_env_extend_root": $(json_num_or_null "$bd_copy_site_jit_env_extend_root")
  }
}
EOF
}

if [[ ! -f "$normal_log" ]]; then
  echo "missing normal log: $normal_log" >&2
  exit 1
fi
if [[ ! -f "$asan_log" ]]; then
  echo "missing asan log: $asan_log" >&2
  exit 1
fi

generated_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

mkdir -p "$(dirname "$out_json")"
cat > "$out_json" <<EOF
{
  "generated_at_utc": "${generated_at}",
  "normal": $(emit_stage_json "$normal_log"),
  "asan": $(emit_stage_json "$asan_log")
}
EOF

echo "Wrote boundary summary: $out_json"
