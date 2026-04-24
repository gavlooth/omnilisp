# Memory Boundary Telemetry Benchmark Baseline - 2026-04-24

Purpose: close `MEM-BENCH-OBSERVE-004` by recording the first bounded
counter-oriented baseline for the memory-boundary telemetry lane.

## Run Profile

- Date: 2026-04-24
- Workload slice: `memory-lifetime-bench`
- Runtime flags:
  - `OMNI_TEST_QUIET=1`
  - `OMNI_TEST_SUMMARY=1`
  - `OMNI_BOUNDARY_BENCH=1`
  - `OMNI_BOUNDARY_INSTR_COUNTERS=1`
- Build command:
  - `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`
- Container-capped runner:
  - `scripts/run_validation_container.sh`
- Runtime log:
  - `.agents/memory-boundary-telemetry-baseline-2026-04-24.log`

Command used:

```bash
scripts/run_validation_container.sh env \
  LD_LIBRARY_PATH=/usr/local/lib \
  OMNI_TEST_QUIET=1 \
  OMNI_TEST_SUMMARY=1 \
  OMNI_BOUNDARY_BENCH=1 \
  OMNI_BOUNDARY_INSTR_COUNTERS=1 \
  OMNI_LISP_TEST_SLICE=memory-lifetime-bench \
  ./build/main --test-suite lisp
```

## Captured Summary

```text
OMNI_BENCH_SUMMARY suite=boundary_decision_cost iters=2048 splice_ms=39 disallowed_ms=28 reuse_ms=5 splice_ok=2048 disallowed_ok=2048 reuse_ok=2048
OMNI_BENCH_SUMMARY suite=boundary_destination_routed_escape iters=2048 partial_ms=3 partial_ok=2048
OMNI_BENCH_SUMMARY suite=scope_splice_tail iters=2048 escapes_per_iter=64 splice_ms=3 splice_ok=2048
OMNI_BENCH_SUMMARY suite=equality_nested_workspace iters=2048 fixture_ok=1 list_depth=1024 array_len=1024 list_true_ms=2128 list_true_ok=2048 list_false_ms=2127 list_false_ok=2048 array_true_ms=48 array_true_ok=2048 array_false_ms=47 array_false_ok=2048 mixed_true_ms=607 mixed_true_ok=2048 mixed_false_ms=577 mixed_false_ok=2048 stack_growth=0 seen_growth=0 stack_bytes=0 seen_bytes=0
OMNI_BENCH_SUMMARY suite=boundary_value_shape_counters iters=128 shape_ms=45 shape_ok=128 closure_env_ok=32 stable_passport_ok=1 temp_slow_delta=209 escape_slow_delta=416 temp_slow_requested_delta=1064760 escape_slow_requested_delta=555136 temp_selected_chunk_delta=3219456 escape_selected_chunk_delta=1339392 temp_reset_slack_delta=53248 escape_reset_slack_delta=118784 temp_destroy_slack_delta=209536 escape_destroy_slack_delta=603776 scope_fresh_delta=30 scope_recycle_hit_delta=418 array_construct_delta=128 array_growth_delta=128 hashmap_construct_delta=256 hashmap_growth_delta=1024 set_construct_delta=128 set_growth_delta=512 closure_env_frame_delta=64 closure_env_binding_delta=64 string_payload_bytes_delta=7168 error_payload_bytes_delta=7040 big_integer_wrappers_delta=128 big_integer_input_bytes_delta=6400 tensor_payload_bytes_delta=65536 ffi_wrappers_delta=128 ffi_releasable_delta=128 stable_stale_delta=1 selected_transplant_delta=32 materialization_copy_bytes_delta=0
OMNI_TEST_SUMMARY suite=boundary_decisions splice_attempted=4128 splice_succeeded=4128 splice_fail_total=0 splice_fail_null_parent=0 splice_fail_null_child=0 splice_fail_same_scope=0 splice_fail_not_immediate_child=0 splice_fail_refcount=0 splice_fail_owner_thread=0 splice_fail_parent_lane=0 splice_fail_child_temp_lane=0 splice_fail_child_escape_lane=0 promotion_attempted=4128 promotion_aborted_budget=0 promotion_aborted_pre_aborted=2048 copy_fallback_total=187 planned_stable_publish=2220 planned_stable_materialize=4128 planned_transplant=0 selected_stable_publish=2220 selected_stable_materialize=0 selected_transplant=2080 selected_fail_closed=2048 route_fail_transplant_rejected=0 materialization_node_count=0 materialization_copy_bytes=0 materialization_copy_bytes_optimizer=0 materialization_copy_bytes_forced_no_splice=0 selected_stable_materialize_cons=0 selected_stable_materialize_array=0 selected_stable_materialize_hashmap=0 selected_stable_materialize_set=0 selected_stable_materialize_closure=0 selected_stable_materialize_big_integer=0 materialization_copy_bytes_cons=0 materialization_copy_bytes_array=0 materialization_copy_bytes_hashmap=0 materialization_copy_bytes_set=0 materialization_copy_bytes_closure=0 materialization_copy_bytes_big_integer=0 materialization_copy_bytes_optimizer_closure=0 materialization_copy_bytes_forced_no_splice_closure=0
```

## Interpretation

- Correctness counters are fully passing for current benchmark lanes:
  `splice_ok=2048`, `disallowed_ok=2048`, `reuse_ok=2048`, `partial_ok=2048`,
  `shape_ok=128`, `closure_env_ok=32`, and `stable_passport_ok=1`.
- Boundary materialization copy debt is zero in this benchmark profile:
  `materialization_copy_bytes_delta=0` in the value-shape workload and
  `materialization_copy_bytes_optimizer=0` in the boundary-decision summary.
  Forced no-splice materialization is also zero in this benchmark profile, so
  no new copy-reduction optimization is justified from this baseline alone.
- The most active non-copy signals are allocator and collection shape counters:
  `escape_slow_delta=416`, `temp_slow_delta=209`,
  `hashmap_growth_delta=1024`, `set_growth_delta=512`, and
  `array_growth_delta=128`.
- Slack is visible and should be tracked by the regression envelope:
  `temp_reset_slack_delta=53248`, `escape_reset_slack_delta=118784`,
  `temp_destroy_slack_delta=209536`, and
  `escape_destroy_slack_delta=603776`.
- Wall-clock numbers are advisory only. The next gate should prioritize
  correctness and counter deltas, then report wide timing warnings rather than
  strict timing failures.

## Recommended Regression Envelope Inputs

Implemented by `scripts/check_memory_telemetry_benchmark_envelope.sh`:

- Correctness:
  - `splice_ok == 2048`
  - `disallowed_ok == 2048`
  - `reuse_ok == 2048`
  - `partial_ok == 2048`
  - `shape_ok == 128`
  - `closure_env_ok == 32`
  - `stable_passport_ok == 1`
  - `splice_fail_total == 0`
- Counter presence:
  - `temp_slow_delta > 0`
  - `escape_slow_delta > 0`
  - `array_growth_delta > 0`
  - `hashmap_growth_delta > 0`
  - `set_growth_delta > 0`
  - `ffi_releasable_delta > 0`
  - `stable_stale_delta > 0`
- Advisory counters:
  - `materialization_copy_bytes_delta == 0` should warn if it rises, not fail,
    until repeated baselines prove the expected workload mix is stable.
  - `materialization_copy_bytes_optimizer == 0` should warn if it rises; forced
    no-splice rollback coverage is tracked separately and is not optimizer
    copy debt.
  - Timing fields should warn only.
