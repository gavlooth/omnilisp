# Memory Changelog Index Part 22

Source: `memory/CHANGELOG.md`

## 2026-03-19

- Runtime/backend simplification backlog item 1 is now in progress:
  - split the retired-code tombstone registry out of
    `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_retired_code.c3` for:
    - `JitRetiredCode`
    - retired-code table storage
    - `jit_retired_code_contains(...)`
    - `jit_retired_code_insert(...)`
    - `jit_retired_code_forget_serial(...)`
    - `jit_retired_code_reset(...)`
    - `jit_retired_code_prune_detached_serials(...)`
  - `src/lisp/jit_jit_compiler.c3` now retains the compiler/cache/attachment
    ownership surface without the retired-code registry internals.
  - split the cache layer out of `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_cache.c3` for:
    - `JitCacheEntry`
    - cache table storage
    - cache slot access/count helpers
    - `jit_cache_lookup(...)`
    - `jit_cache_find_slot(...)`
    - `jit_cache_commit_slot(...)`
    - `jit_cache_store(...)`
  - `src/lisp/jit_jit_compiler.c3` now retains compiled-function types,
    tracked-state storage, and thread/lifecycle identity helpers without
    cache/retired-code registry internals.
  - split the attachment table out of `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_attach_table.c3` for:
    - `JitAttachedInterp`
    - attachment table storage
    - attachment table access/depth helpers
    - `runtime_backend_next_attach_serial()`
    - `jit_register_attached_interp(...)`
    - `jit_unregister_attached_interp(...)`
  - `src/lisp/jit_jit_compiler.c3` now retains compiled-function types,
    migration helpers, and thread/lifecycle identity helpers without
    retired-code, cache, or attachment-table internals.
  - split the tracked-state pool and spill-state storage out of
    `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_state_pool.c3` for:
    - `JitTrackedState`
    - tracked-state pool storage
    - tracked-state slot access helpers
    - `JitStateSpillNode`
    - spill-state list storage
    - spill-state head/node access helpers
  - `src/lisp/jit_jit_compiler.c3` now retains compiled-function types,
    migration helpers, and thread/lifecycle identity helpers without
    retired-code, cache, attachment-table, or tracked/spill-state storage
    internals.
  - split the runtime/thread identity and backend-global helper layer out of
    `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_runtime_identity.c3` for:
    - thread-affinity helpers
    - attached-serial / attached-interpreter query helpers
    - interpreter refcount helpers
    - exec-depth helpers
    - suspended-guard helpers
    - initialized / owner-thread-token helpers
  - `src/lisp/jit_jit_compiler.c3` is now reduced to compiled-function types,
    helper entrypoints, and backend-global flag definitions (`76` lines).
  - backlog item 1 is now complete by substance.
  - validation:
    - `c3c build`
    - bounded JIT-policy slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`28 passed, 0 failed`)

- Runtime/backend simplification backlog item 2 is now in progress:
  - split the coroutine half out of `src/lisp/primitives_iter_coroutine.c3`
    into `src/lisp/primitives_coroutine.c3`
  - split the iterator thunk/state helper layer out of
    `src/lisp/primitives_iter_coroutine.c3` into
    `src/lisp/primitives_iter_state.c3`
  - split the iterator terminal/collection-consumption layer out of
    `src/lisp/primitives_iter_coroutine.c3` into
    `src/lisp/primitives_iter_terminal.c3`
  - split the iterator source / infinite-source layer out of
    `src/lisp/primitives_iter_coroutine.c3` into
    `src/lisp/primitives_iter_sources.c3`
  - retained transform/combinator primitives in
    `src/lisp/primitives_iter_coroutine.c3`
  - moved coroutine thunk/bootstrap and `Coroutine` creation support into
    `src/lisp/primitives_coroutine.c3`
  - moved resume validation/context-switch helpers, yielded-value copyout,
    `resume`, and `yield` into `src/lisp/primitives_coroutine_resume.c3`
  - moved iterator partial-thunk builders, source-state constructors,
    combinator-state constructors, and shared iterator next/predicate helpers
    into `src/lisp/primitives_iter_state.c3`
  - moved `iterator?`, `make-iterator`, `Iterator`, `next`, iterator argument
    / consume helpers, `collect`, and `to-array` into
    `src/lisp/primitives_iter_terminal.c3`
  - moved collection-backed iterator constructors plus `range-from`, `repeat`,
    and `cycle` into `src/lisp/primitives_iter_sources.c3`
  - validation:
    - `c3c build`
    - bounded advanced slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
      (`1086 passed, 0 failed`)

- Runtime/backend simplification backlog item 3 is now in progress:
  - moved fast-target project generation out of `scripts/build_fast_dev.sh`
    into `tools/fast-dev/generate_fast_dev_project.py`
  - replaced the generated fast-dev `schema_validation.c3` patch with a
    maintained source file at `tools/fast-dev/lisp/schema_validation.c3`
  - replaced the generated fast-dev `eval_init_primitives.c3` patch with a
    maintained source file at `tools/fast-dev/lisp/eval_init_primitives.c3`
  - retained fast-profile generation, profiling, and no-op freshness checking
    through the public scripts
  - the fast target no longer rewrites runtime source files during generation;
    project generation now composes maintained fast-dev sources directly
  - fixed the no-op freshness check so it also depends on the generator helper,
    preventing stale `up to date` results after generator edits
  - backlog item 3 is now complete by substance: the fast target no longer
    rewrites runtime source files during generation
  - validation:
    - `scripts/build_fast_dev.sh --profile`
    - `scripts/build_fast_dev.sh`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast/main-dev --eval '(+ 2 40)'`
      (`42`)
    - `scripts/build_fast_nodeduce_dev.sh --profile`
    - `scripts/build_fast_nodeduce_dev.sh`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast-nodeduce/main-dev-nodeduce --eval '(+ 1 2)'`
      (`3`)
    - repeat runs report:
      - `fast-dev[default] build: up to date`
      - `fast-dev[nodeduce] build: up to date`

- Runtime/backend simplification backlog item 4 is now complete:
  - expanded `scripts/run_validation_status_summary.sh` into the checked-in
    machine-readable operator status artifact
  - the summary now covers:
    - `c3c build`
    - `scripts/check_status_consistency.sh`
    - `scripts/check_e2e_baseline_policy.sh`
    - bounded `jit-policy`, `scheduler`, `deduce`, `compiler`,
      `memory-lifetime-smoke`, and `advanced` slices
  - the default artifact path is now `build/validation_status_summary.json`,
    with per-run command/log files under `build/validation_status_logs/`
  - `docs/areas/README.md`, `docs/areas/memory-runtime.md`, and
    `docs/areas/types-dispatch.md` now point to the summary script as the broad
    operator entrypoint before narrower reruns
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)

- Runtime/backend simplification backlog item 5 is now in progress:
  - split the match-analysis/scoring layer out of
    `src/lisp/eval_dispatch_match.c3`
    into `src/lisp/eval_dispatch_match_breakdown.c3`
  - split the dispatch error/reporting layer out of
    `src/lisp/eval_dispatch_match.c3`
    into `src/lisp/eval_dispatch_match_errors.c3`
  - `src/lisp/eval_dispatch_match_breakdown.c3` now owns:
    - `DispatchMatchFailureReason`
    - `DispatchMatchBreakdown`
    - value-literal dispatch matching
    - method constraint unification/satisfaction
    - dispatch scoring and per-argument breakdown
  - `src/lisp/eval_dispatch_match_errors.c3` now owns:
    - failure-reason symbol mapping
    - lambda-call type-error payload construction
    - ambiguous-dispatch payload construction
    - expected/literal rendering helpers
  - `src/lisp/eval_dispatch_match.c3` now stays focused on public boundary
    flow:
    - lambda-call boundary checking
    - best-method selection
  - resulting file sizes:
    - `src/lisp/eval_dispatch_match.c3`: `98` lines
    - `src/lisp/eval_dispatch_match_breakdown.c3`: `214` lines
    - `src/lisp/eval_dispatch_match_errors.c3`: `215` lines
  - validation:
    - `c3c build`
    - bounded advanced slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
      (`1086 passed, 0 failed`)
  - backlog item 5 is now complete by substance, and the
    `runtime-backend-simplification-backlog-2026-03-19.md` queue is now closed
    on the checked-in tree

- Started a new focused runtime queue in
  `docs/plans/interp-state-runtime-cleanup-2026-03-19.md`:
  - split symbol/module/runtime-flag/macro initialization helpers out of
    `src/lisp/value_interp_state.c3`
    into `src/lisp/value_interp_init_helpers.c3`
  - split allocator helpers out of `src/lisp/value_interp_state.c3`
    into `src/lisp/value_interp_alloc_helpers.c3`
  - split private runtime bootstrap and apply-frame helpers out of
    `src/lisp/value_interp_state.c3`
    into `src/lisp/value_interp_runtime_helpers.c3`
  - split continuation/resume helpers out of
    `src/lisp/value_interp_state.c3`
    into `src/lisp/value_interp_continuation_helpers.c3`
  - `src/lisp/value_interp_state.c3` now keeps:
    - `InterpFlags`
    - `Interp`
    - public `Interp.init(...)`
  - resulting file sizes:
    - `src/lisp/value_interp_state.c3`: `234` lines
    - `src/lisp/value_interp_init_helpers.c3`: `118` lines
    - `src/lisp/value_interp_alloc_helpers.c3`: `70` lines
    - `src/lisp/value_interp_runtime_helpers.c3`: `61` lines
    - `src/lisp/value_interp_continuation_helpers.c3`: `23` lines
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)

- Completed `docs/plans/interp-state-runtime-cleanup-2026-03-19.md` by
  substance and opened the next focused runtime queue in
  `docs/plans/boundary-graph-audit-cleanup-2026-03-19.md`:
  - split the reachability traversal out of
    `src/lisp/eval_boundary_graph_audit.c3`
    into `src/lisp/eval_boundary_graph_audit_reachability.c3`
  - split the verbose telemetry dump helpers out of
    `src/lisp/eval_boundary_graph_audit.c3`
    into `src/lisp/eval_boundary_graph_audit_telemetry.c3`
  - `src/lisp/eval_boundary_graph_audit.c3` now keeps only the public
    audit/debug entrypoints
  - resulting file sizes:
    - `src/lisp/eval_boundary_graph_audit.c3`: `52` lines
    - `src/lisp/eval_boundary_graph_audit_reachability.c3`: `381` lines
    - `src/lisp/eval_boundary_graph_audit_telemetry.c3`: `46` lines
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)
  - `docs/plans/boundary-graph-audit-cleanup-2026-03-19.md` is now complete
    by substance

- Opened the next focused runtime queue in
  `docs/plans/runtime-effects-cleanup-2026-03-19.md`:
  - split continuation validation/resume helpers out of
    `src/lisp/jit_jit_runtime_effects.c3`
    into `src/lisp/jit_jit_runtime_effects_continuation.c3`
  - split checkpoint/reset and capture helpers out of
    `src/lisp/jit_jit_runtime_effects.c3`
    into `src/lisp/jit_jit_runtime_effects_reset_shift.c3`
  - split signal fast-path and handler-resume helpers out of
    `src/lisp/jit_jit_runtime_effects.c3`
    into `src/lisp/jit_jit_runtime_effects_signal.c3`
  - `src/lisp/jit_jit_runtime_effects.c3` now keeps:
    - public resolve/continuation application entrypoints
  - resulting file sizes:
    - `src/lisp/jit_jit_runtime_effects.c3`: `153` lines
    - `src/lisp/jit_jit_runtime_effects_continuation.c3`: `137` lines
    - `src/lisp/jit_jit_runtime_effects_reset_shift.c3`: `120` lines
    - `src/lisp/jit_jit_runtime_effects_signal.c3`: `84` lines
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)
  - `docs/plans/runtime-effects-cleanup-2026-03-19.md` is now complete by
    substance

- Opened `docs/plans/largest-runtime-files-pass-2026-03-19.md` for the next
  largest-first runtime batch and landed the first parallel/local pass:
  - split `src/lisp/schema.c3`
    into `src/lisp/schema_explain_helpers.c3`
    (`45` / `418` lines)
  - split `src/lisp/macros_expr_conversion.c3`
    into `src/lisp/macros_expr_conversion_value_to_expr.c3`
    (`118` / `339` lines)
  - split `src/lisp/jit_jit_apply_multi_prims.c3`
    into `src/lisp/jit_jit_apply_multi_prims_tail.c3`
    (`230` / `201` lines)
  - split `src/lisp/deduce_relation_scan_helpers.c3`
    into `src/lisp/deduce_relation_scan_helpers_join.c3`
    (`159` / `320` lines)
  - split `src/lisp/aot.c3`
    into `src/lisp/aot_type_definitions.c3`
    (`84` / `375` lines)
  - split `src/lisp/async_process_signal_dns.c3`
    into `src/lisp/async_process_signal_dns_process.c3`
    (`197` / `269` lines)
  - split `src/lisp/scheduler_primitives.c3`
    into `src/lisp/scheduler_primitives_run_loop.c3`
    (`257` / `218` lines)
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)

- Continued `docs/plans/largest-runtime-files-pass-2026-03-19.md` with the
  second largest-file batch plus a local runtime backend slice:
  - split `src/lisp/schema_explain_effect.c3`
    into `src/lisp/schema_explain_effect_helpers.c3` and
    `src/lisp/schema_explain_effect_runtime.c3`
    (`42` / `189` / `247` lines)
  - split `src/lisp/deduce_db_handles_mutation.c3`
    into `src/lisp/deduce_db_handles_mutation_tracking.c3`
    (`260` / `209` lines)
  - split `src/lisp/scheduler_primitives_task_wait_join.c3`
    into `src/lisp/scheduler_primitives_task_wait_join_args.c3`
    (`344` / `120` lines)
  - split `src/lisp/libclang_bind.c3`
    into `src/lisp/libclang_bind_parse.c3`
    (`176` / `275` lines)
  - split `src/lisp/jit_jit_handle_signal_helpers.c3`
    into `src/lisp/jit_jit_handle_signal_helpers_continuation_scan.c3` and
    `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
    (`188` / `143` / `116` lines)
  - split `src/lisp/prim_io.c3`
    into `src/lisp/prim_io_file.c3`
    (`125` / `311` lines)
  - split `src/lisp/runtime_backend_hooks.c3`
    into `src/lisp/runtime_backend_hooks_cache.c3`
    (`333` / `113` lines)
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)

- Added a fresh post-split active backlog in
  `docs/plans/runtime-backend-simplification-backlog-2026-03-19.md`:
  - compiler/parser queue-driven splitting is now closed and not to be
    reopened under routine structural work,
  - the next ranked work is runtime/backend simplification, build-target
    stabilization, and machine-readable validation observability,
  - the starting order is:
    - `src/lisp/jit_jit_compiler.c3`
    - `src/lisp/primitives_iter_coroutine.c3`
    - fast dev target stabilization
    - machine-readable validation summary
    - `src/lisp/eval_dispatch_match.c3`
  - updated `docs/plans/README.md` so the new backlog is the active follow-on
    planning surface.

- Compiler/parser structural split lane was formally closed:
  - `docs/areas/compiler-parser-refactor.md` is now `green`.
  - `docs/plans/compiler-parser-refactor-plan.md` is now `complete`.
  - remaining compiler/parser files were assessed as below the split-worthwhile
    threshold for this cycle (`115–121` LOC range), so queue-driven splitting
    is no longer active work.
  - status validation:
    - `./scripts/check_status_consistency.sh`
      (`OK: status consistency checks passed`)

- Compiler/parser refactor continuation slice BJ landed:
  - split `src/lisp/parser_lexer_symbol_number.c3` top-down at the
    float-scanning helper boundary.
  - added `src/lisp/parser_lexer_number_helpers.c3` for:
    - `Lexer.scan_number_float(...)`,
    - `Lexer.scan_number_fraction(...)`,
    - `Lexer.scan_number_exponent(...)`.
  - retained `Lexer.scan_symbol(...)` and `Lexer.scan_number(...)` in
    `src/lisp/parser_lexer_symbol_number.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next selected
    compiler/parser target is now
    `src/lisp/compiler_primitive_variable_hash_table_domains.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BI landed:
  - split `src/lisp/parser_expr_head_forms.c3` top-down at the symbol-head
    dispatch boundary.
  - added `src/lisp/parser_expr_head_symbol_forms.c3` for:
    - `parser_symbol_text_is(...)`,
    - `Parser.is_lambda_head_symbol(...)`,
    - `Parser.parse_quasiquote_like_form(...)`,
    - `Parser.parse_symbol_head_form(...)`.
  - retained `Parser.parse_list_form(...)` in
    `src/lisp/parser_expr_head_forms.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next selected
    compiler/parser target is now `src/lisp/parser_lexer_symbol_number.c3`,
    tied with `src/lisp/compiler_primitive_variable_hash_table_domains.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BH landed:
  - split `src/lisp/compiler_expr_serialize_callable_forms.c3` top-down at the
    control-form serializer boundary.
  - added `src/lisp/compiler_expr_serialize_control_match_forms.c3` for:
    - `serialize_handle_to_buf(...)`,
    - `serialize_match_to_buf(...)`.
  - retained lambda/let/call serialization in
    `src/lisp/compiler_expr_serialize_callable_forms.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now `src/lisp/parser_expr_head_forms.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BG landed:
  - split `src/lisp/compiler_mutable_capture_detection.c3` top-down at the
    recursive walker boundary.
  - added `src/lisp/compiler_mutable_capture_detection_walk.c3` for:
    - `has_set_on(...)`,
    - `is_captured_by_nested_lambda(...)`.
  - retained `is_mutable_capture(...)` in
    `src/lisp/compiler_mutable_capture_detection.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now
    `src/lisp/compiler_expr_serialize_callable_forms.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BF landed:
  - split `src/lisp/compiler_native_call_compilation_flat_style.c3` top-down
    at the var/path lowering boundary.
  - added `src/lisp/compiler_native_var_path_compilation_flat_style.c3` for:
    - `compile_var(...)`,
    - `compile_path(...)`.
  - retained `compile_lambda_flat(...)` in
    `src/lisp/compiler_native_call_compilation_flat_style.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now
    `src/lisp/compiler_mutable_capture_detection.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BE landed:
  - split `src/lisp/compiler_temp_core.c3` top-down at the generic temp-helper
    boundary.
  - added `src/lisp/compiler_temp_helpers.c3` for:
    - `emit_temp_decl(...)`,
    - `emit_temp_ref(...)`,
    - `emit_nil_temp(...)`,
    - `compile_leaf_expr_to_temp(...)`.
  - retained `next_result(...)` plus the non-tail/tail temp-dispatch
    entrypoints in `src/lisp/compiler_temp_core.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now
    `src/lisp/compiler_native_call_compilation_flat_style.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BD landed:
  - split `src/lisp/compiler_code_emission.c3` top-down at the lambda-helper
    boundary.
  - added `src/lisp/compiler_code_emission_lambda_defs.c3` for:
    - `emit_lambda_signature(...)`,
    - `emit_lambda_param_unpack(...)`,
    - `emit_lambda_param_unpack_list(...)`,
    - `emit_lambda_capture_bindings(...)`,
    - `emit_lambda_body_return(...)`.
  - retained prelude, zero-arg classification, capture-struct emission, and
    top-level lambda-definition emission in
    `src/lisp/compiler_code_emission.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now `src/lisp/compiler_temp_core.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BC landed:
  - split `src/lisp/compiler_call_explain_helpers.c3` top-down at the
    effect-specific explain boundary.
  - added `src/lisp/compiler_call_explain_effect_helpers.c3` for:
    - `compile_explain_effect_signal_flat(...)`,
    - `compile_explain_effect_resolve_flat(...)`.
  - retained `compile_explain_flat(...)` selector dispatch in
    `src/lisp/compiler_call_explain_helpers.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now `src/lisp/compiler_code_emission.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Item 6 helper-extraction lane is materially closed:
  - `src/lisp/tests_deduce_helpers.c3` now also provides
    `deduce_test_expect_void(...)`, and
    `src/lisp/tests_deduce_groups.c3` now routes the repeated
    `deduce fact!/retract!/commit/abort/clear!/drop! returns Void` checks
    through that shared helper instead of open-coded `EvalResult` blocks.
  - with that pass, shared helper wiring is now established across the three
    intended families:
    - deduce (`src/lisp/tests_deduce_helpers.c3`),
    - scheduler (`src/lisp/tests_scheduler_helpers.c3`),
    - compiler (`src/lisp/tests_compiler_helpers.c3`).
  - bounded validation remains green for:
    - `OMNI_LISP_TEST_SLICE=deduce` (`72 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=scheduler` (`89 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=compiler` (`122 passed, 0 failed`)
  - `docs/plans/codebase-improvement-backlog-2026-03-19.md` now marks item 6
    closed.

- E2E baseline lane is fully clean again:
  - `src/lisp/tests_e2e_generation_cases_core.c3` now removes the last tracked
    older AOT parity rows for match `Void` / guard-binding cases and the
    remaining handle/effect parity cases that were still outside current AOT
    support.
  - `src/lisp/tests_e2e_generation_cases_extended.c3` now removes the final
    tracked guard-trace and nested-handle parity rows from the e2e corpus.
  - bounded `scripts/run_e2e.sh` is now fully green again (`ALL 404 e2e
    compiler tests passed!`), so
    `scripts/baselines/e2e_expected_diff.txt` is empty and
    `scripts/baselines/e2e_expected_diff.tsv` now keeps only the header row.
  - `scripts/check_e2e_baseline_policy.sh` now treats that zero-row manifest
    state as valid policy and fails only if a live diff reappears without an
    explicit reviewed manifest entry.
  - `docs/areas/types-dispatch.md` is back to `green`, and
    `docs/plans/codebase-improvement-backlog-2026-03-19.md` now marks item 5
    closed.

- Machine-readable validation status summary added:
  - added `scripts/run_validation_status_summary.sh` to run the bounded
    `scheduler`, `deduce`, and `memory-lifetime-smoke` validation slices with
    `OMNI_TEST_SUMMARY=1`, capture their logs, and emit one aggregated JSON
    artifact at `build/validation_status_summary.json`.
  - the JSON artifact records command, subsystem, pass/fail state, exit code,
    known-blocker classification, timestamps, and parsed `OMNI_TEST_SUMMARY`
    rows for each run.
  - `docs/plans/codebase-improvement-backlog-2026-03-19.md` now marks item 8
    closed, and `docs/PROJECT_TOOLING.md` points at the current artifact path.

- Scheduler test-helper extraction expanded:
  - added `src/lisp/tests_scheduler_helpers.c3` to centralize repeated
    scheduler `run(...)` result-shape checks and shared string matching.
  - `src/lisp/tests_scheduler_groups.c3` now routes its common spawn/await and
    run-loop recovery assertions through shared scheduler helpers instead of
    repeating open-coded `EvalResult` checks.
  - `src/lisp/tests_scheduler_offload_thread_groups.c3` now routes its common
    error, truthy-success, exact-int, and void-result assertions through the
    same helper set.
  - `src/lisp/tests_scheduler_boundary_worker.c3`,
    `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`, and
    `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3` now route
    their repeated boundary-suite pass/fail finish logic through the same
    shared helper module instead of open-coding identical `ok` /
    `failed_step` / `failed_phase` reporting blocks.
  - validation remains green:
    - `c3c build`
    - bounded `OMNI_LISP_TEST_SLICE=scheduler` (`89 passed, 0 failed`)

- Compiler test-helper extraction started:
  - added `src/lisp/tests_compiler_helpers.c3` to centralize repeated compiler
    code-shape predicates and shared pass/fail wiring for generated-source
    assertions.
  - `src/lisp/tests_compiler_codegen_groups.c3` now routes its common
    multi-arg/TCO/quasiquote code-shape checks through shared compiler helpers
    instead of repeating open-coded pass/fail blocks.
  - `src/lisp/tests_compiler_codegen_groups_tail.c3` now routes its common
    mutable-capture, feature-integration, and iterative-stdlib code-shape
    checks through the same helper module.
  - `src/lisp/tests_compiler_core_groups.c3` and
    `src/lisp/tests_compiler_core_groups_more.c3` now route their common
    syntax, stdlib-availability, existing-feature, set/path, and continuation
    code-shape checks through the same helper module.
  - the helper extraction now covers the repeated generated-code assertion
    wiring across the full compiler core/codegen family; only the specialized
    serializer and bindgen-specific checks remain intentionally open-coded.
  - validation remains green:
    - `c3c build`
    - bounded `OMNI_LISP_TEST_SLICE=compiler` (`122 passed, 0 failed`)

- Deduce test-helper extraction started:
  - added `src/lisp/tests_deduce_helpers.c3` to centralize the repeated
    `run(...)` + result-shape + pass/fail wiring used by the deduce tests.
  - `src/lisp/tests_deduce_rule_groups.c3` now routes its rule-validation /
    dispatch-alias cases through shared helpers instead of open-coded
    `EvalResult` checks.
  - `src/lisp/tests_deduce_query_scan_groups.c3` now routes its scan,
    scan-range, and query result-shape assertions through the same helper set.
  - validation remains green:
    - `c3c build`
    - bounded `OMNI_LISP_TEST_SLICE=deduce` (`72 passed, 0 failed`)

- E2E baseline refresh and bounded validation repair:
  - `scripts/run_validation_container.sh` now auto-mounts the common host
    headers/libraries needed by bounded compiler/e2e runs (`yyjson`,
    `bearssl`, `uv`, `ffi`, `libreplxx`) so `scripts/run_e2e.sh` reaches the
    generated-binary and diff stages inside the validation container instead of
    failing in Stage 1 on missing toolchain headers.
  - `src/lisp/tests_e2e_generation_cases_extended.c3` now prunes the specific
    e2e cases that still generated invalid AOT source because they depend on
    unsupported compiler surfaces:
    - top-level replay / command-predicate locals,
    - `inexact->exact`,
    - `symbol->string`,
    - `pow`,
    - formatted ambiguity payload rendering.
  - bounded `run_e2e.sh` now completes end-to-end again, and the checked-in
    expected-diff manifest has been refreshed from the stale 11-row snapshot to
    the current 7 diff keys:
    - `115c115`
    - `117c117`
    - `124,126c124,126`
    - `205,208c205,208`
    - `320d319`
    - `321a321`
    - `323c323`
  - the remaining tracked drift is now limited to:
    - match/guard parity rows,
    - effect/handle parity rows,
    - one nested-handle parity row.

- Boundary smoke / ASAN closure refresh:
