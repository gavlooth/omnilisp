# Largest Runtime Files Pass (2026-03-19)

Status: `superseded`
As of: 2026-04-21
Owner: Codex workflow

## Purpose

Track the current largest-first runtime split pass after the earlier focused
cleanup queues were closed.

This lane is explicitly size-driven again, but only for consequential non-test
runtime files. The goal is to reduce the remaining large ownership surfaces
without reopening the small-file churn that was already closed elsewhere.

## Superseded Checkpoint (2026-04-21)

This plan is no longer active under the owner rule from 2026-04-21: do not
split files unless they exceed 1000 LOC.

The former `Next Queue` is stale. Current counts are:

- `src/lisp/aot_runtime_bridge.c3`: `126`
- `src/lisp/async_tcp_transport_core.c3`: `126`
- `src/lisp/eval_promotion_copy.c3`: `284`
- `src/lisp/eval_promotion_escape.c3`: `196`
- `src/lisp/primitives_meta_types.c3`: `169`
- `src/lisp/prim_string_format.c3`: `59`

The current source inventory has no `src/` or `csrc/` code file above 1000
LOC; the largest current code/test source file is
`src/lisp/tests_runtime_feature_jit_groups_failures.c3` at `762` LOC. Future
work should reopen file splitting only when the 1000 LOC threshold is exceeded
or when a semantic ownership boundary independently requires a split.

## Rules

1. Split the largest consequential runtime files first.
2. Keep write scopes disjoint when parallelizing.
3. Validate landed batches centrally with:
   - `c3c build`
   - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
4. Record landed slices in `memory/CHANGELOG.md`.

## Landed Batch (2026-03-19)

- `src/lisp/schema.c3` → `src/lisp/schema_explain_helpers.c3`
  - `schema.c3`: `45`
  - `schema_explain_helpers.c3`: `418`
- `src/lisp/macros_expr_conversion.c3` →
  `src/lisp/macros_expr_conversion_value_to_expr.c3`
  - `macros_expr_conversion.c3`: `118`
  - `macros_expr_conversion_value_to_expr.c3`: `339`
- `src/lisp/jit_apply_multi_prims.c3` →
  `src/lisp/jit_apply_multi_prims_tail.c3`
  - `jit_apply_multi_prims.c3`: `230`
  - `jit_apply_multi_prims_tail.c3`: `201`
- `src/lisp/deduce_relation_scan_helpers.c3` →
  `src/lisp/deduce_relation_scan_helpers_join.c3`
  - `deduce_relation_scan_helpers.c3`: `159`
  - `deduce_relation_scan_helpers_join.c3`: `320`
- `src/lisp/aot.c3` → `src/lisp/aot_type_definitions.c3`
  - `aot.c3`: `84`
  - `aot_type_definitions.c3`: `375`
- `src/lisp/async_process_signal_dns.c3` →
  `src/lisp/async_process_signal_dns_process.c3`
  - `async_process_signal_dns.c3`: `197`
  - `async_process_signal_dns_process.c3`: `269`
- `src/lisp/scheduler_primitives.c3` →
  `src/lisp/scheduler_primitives_run_loop.c3`
  - `scheduler_primitives.c3`: `257`
  - `scheduler_primitives_run_loop.c3`: `218`
- `src/lisp/schema_explain_effect.c3` →
  `src/lisp/schema_explain_effect_helpers.c3` +
  `src/lisp/schema_explain_effect_runtime.c3`
  - `schema_explain_effect.c3`: `42`
  - `schema_explain_effect_helpers.c3`: `189`
  - `schema_explain_effect_runtime.c3`: `247`
- `src/lisp/deduce_db_handles_mutation.c3` →
  `src/lisp/deduce_db_handles_mutation_tracking.c3`
  - `deduce_db_handles_mutation.c3`: `260`
  - `deduce_db_handles_mutation_tracking.c3`: `209`
- `src/lisp/scheduler_primitives_task_wait_join.c3` →
  `src/lisp/scheduler_primitives_task_wait_join_args.c3`
  - `scheduler_primitives_task_wait_join.c3`: `344`
  - `scheduler_primitives_task_wait_join_args.c3`: `120`
- `src/lisp/libclang_bind.c3` → `src/lisp/libclang_bind_parse.c3`
  - `libclang_bind.c3`: `176`
  - `libclang_bind_parse.c3`: `275`
- `src/lisp/jit_handle_signal_helpers.c3` →
  `src/lisp/jit_handle_signal_helpers_continuation_scan.c3` +
  `src/lisp/jit_handle_signal_helpers_runtime_effects.c3`
  - `jit_handle_signal_helpers.c3`: `188`
  - `jit_handle_signal_helpers_continuation_scan.c3`: `143`
  - `jit_handle_signal_helpers_runtime_effects.c3`: `116`
- `src/lisp/prim_io.c3` → `src/lisp/prim_io_file.c3`
  - `prim_io.c3`: `125`
  - `prim_io_file.c3`: `311`
- `src/lisp/runtime_backend_hooks.c3` →
  `src/lisp/runtime_backend_hooks_cache.c3`
  - `runtime_backend_hooks.c3`: `333`
  - `runtime_backend_hooks_cache.c3`: `113`

## Landed Batch (2026-03-24)

- `src/lisp/deduce_rule_eval.c3` →
  `src/lisp/deduce_rule_eval_validation.c3`
  - `deduce_rule_eval.c3`: `28`
  - `deduce_rule_eval_validation.c3`: `275`
- `src/lisp/deduce_relation_ops.c3` →
  `src/lisp/deduce_relation_ops_mutations.c3`
  - `deduce_relation_ops.c3`: `4`
  - `deduce_relation_ops_mutations.c3`: `403`
- `src/lisp/eval_env_copy.c3` →
  `src/lisp/eval_env_copy_helpers.c3`
  - `eval_env_copy.c3`: `4`
  - `eval_env_copy_helpers.c3`: `440`
- `src/lisp/eval_pattern_support.c3` →
  `src/lisp/eval_pattern_support_helpers.c3`
  - `eval_pattern_support.c3`: `3`
  - `eval_pattern_support_helpers.c3`: `434`

Validation:
- `c3c build`
- `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
  (`6/9 runs passed`; current blockers: `status_consistency`, `jit_policy`, `deduce`)

## Next Queue

Superseded by the 2026-04-21 checkpoint above. Kept only as historical context.

1. `src/lisp/aot_runtime_bridge.c3` (`433`)
2. `src/lisp/async_tcp_transport_core.c3` (`431`)
3. `src/lisp/eval_promotion_copy.c3` (`181`)
4. `src/lisp/eval_promotion_escape.c3` (`168`)
5. `src/lisp/primitives_meta_types.c3` (`147`)
6. `src/lisp/prim_string_format.c3` (`51`)

## Validation

- `c3c build`
- `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
