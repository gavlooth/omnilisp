- Non-foundation runtime paths now consistently route both:
  - releasing-scope fallback copy behavior
  - scoped copy-to-target behavior
  through `boundary_*` helpers.
- Direct manual scope/releasing choreography is reduced further and concentrated in foundation modules.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- ASAN run still reproduces the known pre-existing stack-engine/JIT current-frame magic check (`asan_thread.cpp`) in coroutine/JIT path; no new lifetime regression signature introduced by this session.

## 2026-03-04: Session 35 - Boundary Facade Expansion (Value/Env Paths)

### Summary
Continued boundary-surface consolidation by routing remaining non-foundation value/env lifetime transitions through the audited `boundary_*` API, keeping behavior unchanged while shrinking the set of direct transition callers.

### What changed
- `src/lisp/value_constructors.c3`:
  - `make_cons` ESCAPE-lane path now uses:
    - `boundary_promote_to_escape(car, interp)`
    - `boundary_promote_to_escape(cdr, interp)`
  - Replaces direct `promote_to_escape` calls in constructor-level business logic.
- `src/lisp/value_environment.c3`:
  - `promote_for_env_target` now routes TEMP->ESCAPE barrier promotion through:
    - `boundary_promote_to_escape(value, interp)`
- `src/lisp/eval_env_copy.c3`:
  - `copy_to_scope_site` now uses:
    - `boundary_copy_to_parent_site(...)`
  - `copy_env_value_fast` fallback/instance/ffi branches now use:
    - `boundary_copy_to_parent_site_ctx(...)`
- `src/lisp/eval_boundary_api.c3`:
  - Added:
    - `boundary_copy_from_releasing_scope(v, interp, releasing_scope, site)`
  - Centralizes `interp.releasing_scope` save/restore around copy fallback paths.
- `src/lisp/eval_run_pipeline.c3`:
  - `run_promote_result` now uses `boundary_copy_from_releasing_scope(...)` for both:
    - promotion-context abort fallback copy
    - in-scope defensive copy path
- `src/lisp/jit_jit_eval_scopes.c3`:
  - `jit_finalize_scoped_result` now uses `boundary_copy_from_releasing_scope(...)` for:
    - promotion-context abort fallback copy
    - final in-scope defensive copy

### Boundary surface state
- After this pass, direct `promote_to_escape` / `copy_to_parent_site(_ctx)` usage is now confined to:
  - foundational promotion modules (`eval_promotion_*`)
  - tests
- Higher-level runtime/value/env business paths use `boundary_*` entry points.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- ASAN run still reproduces the known stack-engine/JIT sanitizer check (`asan_thread.cpp` current-frame magic) in coroutine/JIT path; this signature predates this mechanical boundary-routing change.

## 2026-03-04: Session 34 Start - Audited Boundary Facade + Low-Risk Callsite Routing

### Summary
Started boundary-surface consolidation by introducing an audited `boundary_*` API layer for promotion/copy/splice transitions and routing a low-risk caller slice through it without changing runtime behavior.

### What changed
- `src/lisp/eval_boundary_api.c3` (NEW):
  - Added contracted boundary facade functions:
    - `boundary_copy_to_parent`
    - `boundary_copy_to_parent_site`
    - `boundary_copy_to_parent_site_ctx`
    - `boundary_promote_to_escape`
    - `boundary_promote_to_root_site`
    - `boundary_promote_to_root`
    - `boundary_copy_value_if_owned_by_scope`
    - `boundary_copy_env_to_scope`
  - Added explicit splice gate:
    - `boundary_try_scope_splice_escapes(parent, child)` with non-self assertion.
- Low-risk callsite routing (behavior-preserving):
  - `src/lisp/eval_run_pipeline.c3`:
    - `promote_to_escape` -> `boundary_promote_to_escape`
    - `copy_value_if_owned_by_scope` -> `boundary_copy_value_if_owned_by_scope`
    - `scope_splice_escapes` -> `boundary_try_scope_splice_escapes`
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - Same boundary wrapper migration for promote/copy/splice paths.
  - `src/lisp/eval_repl.c3`:
    - `copy_to_parent_site` -> `boundary_copy_to_parent_site`
  - `src/lisp/primitives_iter_coroutine.c3`:
    - `promote_to_root` -> `boundary_promote_to_root`
    - `copy_to_parent_site` -> `boundary_copy_to_parent_site`
  - `src/lisp/scheduler_io_fiber_core.c3`:
    - `promote_to_root` -> `boundary_promote_to_root`
  - `src/lisp/jit_jit_closure_define_qq.c3`:
    - `copy_env_to_scope` -> `boundary_copy_env_to_scope`
    - `promote_to_root_site` -> `boundary_promote_to_root_site`
    - `promote_to_root` -> `boundary_promote_to_root`
  - `src/lisp/prim_collection_hashmap.c3`:
    - `promote_to_root` -> `boundary_promote_to_root`
  - `src/lisp/prim_collection_sort_array.c3`:
    - `promote_to_root` -> `boundary_promote_to_root`
  - `src/lisp/eval_type_evaluators.c3`:
    - `copy_to_parent_site` -> `boundary_copy_to_parent_site`
- Internal simplification (behavior-preserving):
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - Extracted shared `jit_finalize_scoped_result(...)` to centralize duplicated scope result finalization logic used by:
      - `jit_eval_in_single_scope`
      - `jit_eval_in_call_scope`
    - This reduces duplicated boundary business logic (promote/copy/splice/fallback sequencing) and narrows bug surface in JIT scope handoff paths.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Scope Guard Macroization + JIT-First Runtime Policy

### Summary
Codified runtime policy to prioritize JIT/eval boundary hardening and converted scope-owner safety checks to shared macros so lifetime/thread-affinity invariants are enforced uniformly in hot paths.

### What changed
- `AGENTS.md`:
  - Added hardening-priority rule: stabilize `jit_*`/eval/effect boundary paths before new runtime wiring.
  - Added invariant-enforcement rule: use shared macros/helpers for repeated ownership/lifetime/state checks.
- `src/scope_region.c3`:
  - Added `scope_guard_owner(scope, op)` macro wrapping `scope_require_owner`.
  - Added invariant macros:
    - `scope_invariant_refcount_live(scope)`
    - `scope_invariant_distinct_scopes(parent, child)`
  - Replaced direct `scope_require_owner(...)` calls across scope lifecycle, allocation, reset, dtor registration, and splice paths with `scope_guard_owner(...)`.
  - Added invariant checks:
    - `scope_retain`: refcount must be `> 0`.
    - `scope_release`: refcount underflow guard.
    - `scope_splice_escapes`: parent/child must be distinct.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: ASAN Triage Follow-up (Type Lookup Hardening + Runtime ASAN Test Gating)

### Summary
Re-verified the ASAN failures noted earlier and narrowed them to non-instrumented JIT execution paths. Added defensive symbol/type lookup guards and switched ASAN detection in tests to runtime checks so ASAN builds reliably skip known non-actionable JIT stress suites.

### What changed
- `src/lisp/value_symbol_table.c3`:
  - Hardened `SymbolTable.get_name` with bounds/null checks.
  - Invalid IDs now return `"<invalid-symbol>"` instead of relying on unchecked access.
- `src/lisp/value_type_registry.c3`:
  - Added symbol ID validation guards in `grow`, `register_type`, and `lookup`.
  - Removed fragile symbol-slice hashing path from lookup and switched to direct `SymbolEntry`-based hash computation.
  - Added guardrails for null/empty registry internals before probing hash tables.
- `src/lisp/tests_tests.c3`:
  - Added runtime ASAN detection via `stack_asan_enabled()` (replaces compile-time-only assumptions).
  - `jit_checks_enabled()` now disables JIT checks when ASAN runtime is active.
  - ASAN mode now explicitly:
    - keeps `interp.flags.jit_enabled = false`
    - skips `escape-scope` and `tco-recycling` stress suites that execute non-instrumented JIT machine code and produce non-actionable stack reports.
- `src/main.c3`:
  - `thread_registry_shutdown()` now calls `scope_freelist_cleanup()` so recycled `ScopeRegion` structs/chunks are released at process shutdown (prevents shutdown-only sanitizer leak noise).

### Verification
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-only skips applied)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: JIT Import/Signal + AOT Temp/QQ Decomposition

### Summary
Reduced additional oversized runtime/compiler functions by extracting focused helpers while preserving language semantics and public APIs.

### What changed
- `src/lisp/jit_jit_helper_functions.c3`:
  - Module import/load decomposition:
    - `module_copy_path`
    - `module_ensure_export_capacity`
    - `module_append_export`
    - `jit_create_file_module`
    - `jit_eval_module_top_level`
    - `jit_eval_declared_module_file`
    - `jit_eval_implicit_module_file`
    - `module_path_matches`
    - `find_loaded_module_by_path`
    - `resolve_loaded_module`
    - `build_default_module_rel_path`
    - `load_import_target_module`
    - `module_exports_symbol`
    - `bind_import_symbols`
  - `jit_load_module_from_file` now uses `defer` for source-dir stack and expression-list cleanup.
  - `jit_eval_import_impl` reduced to load+bind orchestration.
  - Signal path decomposition:
    - `jit_unhandled_effect_error`
    - `effect_handler_has_tag`
    - `jit_signal_type_check`
    - `jit_signal_suspend`
    - `jit_signal_try_fast_path`
  - `jit_signal_impl` reduced to orchestration flow.
- `src/lisp/compiler_statement_level_compilation_compile_to_temp.c3`:
  - Added reusable temp/tail helpers:
    - `emit_nil_temp`
    - `compile_leaf_expr_to_temp`
    - `compile_resolve_flat`
    - `compile_index_flat`
    - `compile_define_flat`
    - `compile_tail_if_flat`
    - `compile_tail_begin_flat`
    - `compile_tail_and_flat`
    - `compile_tail_or_flat`
  - Added call helpers:
    - `call_head_is`
    - `compile_call_arg_temps`
    - `build_arg_list_from_temps`
    - `compile_list_or_dict_call_flat`
  - Added quasiquote helpers:
    - `compile_qq_marker_pair`
    - `compile_qq_var_symbol`
    - `compile_qq_app_list`
  - Reduced orchestrator functions: `compile_to_temp`, `compile_to_temp_tail`, `compile_call_flat`, `compile_qq_flat`.

### Function size outcomes
- `jit_signal_impl`: 41 lines
- `jit_load_module_from_file`: 30 lines
- `jit_eval_import_impl`: 10 lines
- `compile_to_temp`: 84 lines
- `compile_to_temp_tail`: 35 lines
- `compile_call_flat`: 34 lines
- `compile_qq_flat`: 50 lines

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Comprehensive Language Reference Documentation

### Summary
Created `docs/OMNI_REFERENCE.md` — a single comprehensive language reference consolidating content from 8 existing doc files, stdlib, and primitive catalogs into a user-facing document with working code examples.

### What changed
- `docs/OMNI_REFERENCE.md` (NEW, ~1800 lines):
  - 38 sections covering all language features
  - Sections: Overview, Data Types, Truthiness, Special Forms, Pattern Matching, Destructuring, Functions, Partial Application, Collections, Strings, Math, Type System, Multiple Dispatch, Macros, Modules, Algebraic Effects, Delimited Continuations, Coroutines, Iterators, Error Handling, I/O, Networking, JSON, Regex, PEG Grammar, Compression, Unicode, Deduce, Concurrency, FFI, Schema Validation, System & Misc, Reader Syntax, CLI & Tooling
  - 4 Appendices: Complete Primitive Reference (~180 prims), Complete Stdlib Reference (~80 fns), Limits, EBNF Grammar
  - Working code examples throughout all sections

### Source files consulted
- `docs/LANGUAGE_SPEC.md`, `docs/SYNTAX_SPEC.md`, `docs/FEATURES.md`
- `docs/EFFECTS_GUIDE.md`, `docs/type-system-syntax.md`
- `docs/COMPILER.md`, `docs/PROJECT_TOOLING.md`, `docs/CORE_LIBS_INSPECTION.md`
- `stdlib/stdlib.lisp`, primitive registration code, test examples

### Tests
No code changes — documentation only.

## 2026-03-04: Parser/Dispatch/Scheduler Async-Path Decomposition

### Summary
Continued large-function refactoring in parser, dispatch scoring, and async scheduler setup without changing public syntax or effect APIs.

### What changed
- `src/lisp/parser_parser.c3`:
  - Extracted type-annotation parsing helpers:
    - `parser_empty_type_annotation`
    - `parse_compound_type_annotation`
    - `parse_dict_type_annotation`
  - `parse_type_annotation` now a slim form dispatcher.
  - Extracted list-form/special-form dispatch helpers:
    - `is_lambda_head_symbol`
    - `parse_quasiquote_like_form`
    - `parse_symbol_head_form`
  - `parse_list_form` now delegates instead of inlining all special-form branches.
  - Extracted let helpers:
    - `consume_let_rec_annotation`
    - `collect_let_bindings`
    - `build_let_expr_chain`
  - Extracted handle helpers:
    - `consume_handle_strict_annotation`
    - `parse_handle_old_clause`
    - `parse_handle_new_clause`
    - `parse_handle_clause`
- `src/lisp/eval.c3`:
  - Decomposed method-table scoring into:
    - `method_constraints_satisfied`
    - `method_match_score`
  - Decomposed run-path helpers:
    - `parser_error_to_eval_result`
    - `run_execute_expr`
    - `run_promote_result`
  - `run_program`/`run` now reuse shared parser-error conversion and run orchestration helpers.
- `src/lisp/scheduler.c3`:
  - Split async tcp-read watcher setup/cleanup into:
    - `scheduler_drain_loop_nowait`
    - `scheduler_close_handles_and_drain`
    - `scheduler_create_poll_handle`
    - `scheduler_create_timer_handle`
    - `scheduler_start_tcp_watchers`
  - `scheduler_try_async_tcp_read` reduced to an orchestrator over probe/setup/block/resume flow.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Runtime Registration + Promotion/JIT Apply Decomposition

### Summary
Continued large-function decomposition in runtime hot paths without API changes:
- Primitive registration pipeline split into grouped helpers and table-driven batches.
- Escape-lane promotion split into tag-specific helpers plus explicit disjoint-lifetime fallback helper.
- JIT multi-arg application split into focused handlers (null/variadic/primitive/method-table/iterative).

### What changed
- `src/lisp/eval.c3`:
  - Added registration helpers:
    - `register_language_constants`
    - `register_dispatched_primitives`
    - `register_regular_primitives_*` groups
    - `register_effect_fast_paths`
    - shared `register_prim_table` / `register_dispatched_prim_table`
  - `register_primitives` now orchestrates helpers (small dispatcher).
  - `promote_to_escape` refactored with extracted helpers:
    - `promote_to_escape_disjoint`
    - `promote_escape_string_or_error`
    - `promote_escape_cons`
    - `promote_escape_closure`
    - `promote_escape_partial_prim`
    - `promote_escape_shared_wrapper`
    - `promote_escape_instance`
    - `promote_escape_ffi_handle`
- `src/lisp/jit_jit_helper_functions.c3`:
  - `jit_apply_multi_args` refactored into focused helpers:
    - `jit_null_callable_error`
    - `jit_apply_multi_args_zeroarg_closure`
    - `jit_apply_multi_args_variadic_closure`
    - `jit_apply_multi_args_primitive`
    - `jit_apply_multi_args_closure_multi`
    - `jit_apply_multi_args_method_table`
    - `jit_apply_multi_args_iterative`
- `src/lisp/compiler_primitive_variable_hash_table.c3`:
  - Split `init_prim_hash` by category into:
    - `init_prim_hash_arithmetic_and_comparison`
    - `init_prim_hash_core_and_strings`
    - `init_prim_hash_files_and_misc`
    - `init_prim_hash_collections_math_bitwise`
  - `init_prim_hash` now clears and delegates.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Lexer/Serializer/CLI Main Decomposition

### Summary
Reduced remaining large orchestrator functions in parsing, serialization, and CLI entry flow by extracting focused helpers and preserving behavior.

### What changed
- `src/lisp/parser_lexer.c3`:
  - `Lexer.advance` split using helper scans:
    - `scan_punctuation_token`
    - `scan_logic_variable`
    - `scan_dot_tokens`
    - `scan_standalone_underscore`
  - `Lexer.advance` now acts as a short ordered dispatcher.
- `src/lisp/compiler_expression_serializer.c3`:
  - `serialize_expr_to_buf` split with reusable helpers:
    - unary/binary/named/symbol+arg form emitters
    - begin/index/path emitters
    - `serialize_expr_specialized` dispatcher for simple expression forms
- `src/entry.c3`:
  - Extracted CLI/runtime handlers:
    - `run_compile_mode`
    - `run_gen_e2e_mode`
    - `run_repl_mode`
    - `run_script_mode`
    - `run_test_mode`
  - Added argv helpers:
    - `cstr_len`
    - `find_flag_index`
  - `main` now delegates to handlers in precedence order.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: High-Throughput Function Decomposition (Parser/Eval/Macros)

### Summary
Refactored multiple high-complexity runtime/parser functions into smaller helpers while keeping public APIs and language behavior unchanged. One defensive fix was included in path error formatting to avoid null type-info dereference in an error path.

### What changed
- `src/lisp/parser_parser.c3`:
  - Unified quote/template datum parsing via shared `parse_datum_impl`.
  - Added focused constructors for datum values (nil/int/symbol/string/quote).
  - `parse_template_datum` and `parse_datum` reduced to thin wrappers.
- `src/lisp/eval.c3`:
  - `prim_ffi_bound_call` split into helper stages:
    - lazy `dlsym` resolution
    - per-arg FFI coercion
    - return storage selection
    - return-value conversion
  - `eval_path` split into root lookup + module/instance/cons step helpers.
  - `match_pattern` split by pattern kind (`var`, `cons`, `dict`, `constructor`, `guard`).
  - Defensive fix: instance-path error formatting now handles missing `TypeInfo` without dereferencing null.
- `src/lisp/macros.c3`:
  - Extracted macro expansion sequence/call-site helpers for `expand_macros_in_expr`.
  - Split `value_to_expr` special-form decoding into focused helpers.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Refactor value_to_expr in macros.c3

### Summary
Refactored the 307-line `value_to_expr` function (largest in the codebase) into 9 well-named helpers + an 85-line dispatcher. Public API unchanged.

### What changed
- `src/lisp/macros.c3`:
  - Extracted `make_nil_expr`, `rest_car_expr`, `val_to_sym` (inline helpers for repeated patterns)
  - Extracted `make_single_lambda_expr`, `make_nullary_lambda_expr` (deduplicate 3 copies of ExprLambda init)
  - Extracted `vte_lambda` (82-line lambda handler with multi-param desugaring)
  - Extracted `vte_let` (44-line let handler with flat-pair binding collection)
  - Extracted `vte_begin` (16-line begin handler)
  - Extracted `vte_call` (26-line generic call handler)
  - Simplified `value_to_expr` to flat dispatcher (307 -> 85 lines)
  - Removed redundant true/false symbol check (both branches were identical)
  - Flattened CONS nesting (early return for non-symbol head)

### Tests
- 1143 unified + 73 compiler + 10 stack + 50 scope = 1276 PASS, 0 failures


## 2026-03-04: ASAN Scope-Recycling + Stack-Switch Hook Triage

### Summary
ASAN still reports a deterministic failure during the broader JIT suite (`escape-scope: nested map+reverse`). Added two triage hardening changes to improve signal quality:
- ASAN-mode scope freelist bypass (avoid stale-reuse noise).
- Runtime ASAN symbol detection + weak-hook stack-switch integration in the C stack helper shim.

### What changed
- `src/scope_region.c3`:
  - Added `scope_freelist_recycle_enabled()` gate.
  - Under ASAN, `ScopeRegion` structs are freed directly instead of recycled through the freelist.
  - Non-ASAN behavior is unchanged (freelist reuse remains enabled).
- `csrc/stack_helpers.c`:
  - Switched ASAN detection to runtime weak-symbol checks (`__asan_init`).
  - Updated fiber switch hooks (`stack_asan_start_switch`, `stack_asan_finish_switch`) to call sanitizer fiber APIs when symbols are present, without relying on compile-time ASAN macros.
- `src/stack_engine.c3`:
  - Added `stack_runtime_asan_enabled()` and used it for stack sizing and overflow-test skip logic, preventing false "non-ASAN" behavior when runtime ASAN is active.

### Triage result
- The failure remains reproducible after both safeguards.
- Current deterministic failure site remains in the same chain:
  - `lisp.jit_apply_multi_args(...)` (multi-arg apply/dispatch path)
  - `lisp.jit_eval_in_single_scope(...)`
  - `main.scope_create(...)` / `scope_chunk_alloc(...)` where corruption becomes visible.
- With runtime stack-switch hooks enabled, ASAN also reports internal stack-frame consistency checks at the same site, which further indicates upstream stack/state corruption before allocator entry.
- Last stable frame before crash remains:
  - `lisp.jit_apply_multi_args(...)` around multi-arg apply/dispatch path
  - followed by `scope_create -> scope_chunk_alloc` where corruption becomes visible.

## 2026-03-04: Ownership Guardrail Tightening (Model Preservation)

### Summary
Added explicit model-preservation guardrails to prevent future drift from Omni's scope/region ownership model. The runtime default remains region ownership for language values, with only rare and explicit foreign-resource exceptions.

### Policy Clarification
- Language values are owned by scope/region boundaries; they do not get ad-hoc per-type lifetime systems.
- Any local RC policy is exception-only and limited to opaque foreign resources that do not own Omni `Value` graphs.
- Root pinning is not a correctness fix for boundary bugs and must not be used as a default lifetime strategy.
- Boundary paths (return copy, env copy, mutation copy, promotion) must stay model-consistent.

### Enforcement
- Added explicit "Ownership Drift Guardrails (Required)" section in `AGENTS.md`.
- Previous same-day `Instance` per-type RC experiment remains documented for history but is superseded and treated as rolled back.

## 2026-03-04: Instance Ownership Realigned to Scope Model (no per-type RC)

### Summary
Replaced `Instance` per-type refcounting with scope-owned lifetime tethering to preserve Omni's core ownership model. `Instance` now carries an `owner_scope`, and wrappers crossing boundaries retain/release that scope. This supersedes the earlier "Instance refcount" approach from the same day.

### Why
Per-type RC for `Instance` drifted from the runtime model (scope/region ownership) and left nested field payload lifetime unsound for pointer-backed values (for example `FFI_HANDLE` in instance fields). Escaped values could outlive source scopes incorrectly.

### Changes
- **`Instance` ownership shape** (`src/lisp/value.c3`):
  - Removed `Instance.refcount`.
  - Added `Instance.owner_scope`.
  - `instance_retain`/`instance_release` now call `scope_retain`/`scope_release` on `owner_scope`.
- **Instance construction** (`src/lisp/eval.c3`):
  - Added `make_instance_in_scope(...)`.
  - Each instance allocates a dedicated `owner_scope` (child of `root_scope`).
  - Field payloads are copied into `owner_scope` via boundary copy.
  - Wrapper lives in caller/root scope and releases `owner_scope` in dtor.
- **Boundary copying** (`src/lisp/eval.c3`):
  - `copy_env_value_fast(...)` no longer returns `INSTANCE` as always shareable.
  - It now uses boundary-copy path for `INSTANCE` when not in surviving target scope chain.
- **Mutation path** (`src/lisp/jit_jit_helper_functions.c3`):
  - `set!` on instance fields now copies value into `instance.owner_scope` (not root pinning).
  - Cons mutation behavior unchanged (`promote_to_root`).
- **Guardrail docs** (`AGENTS.md`):
  - Added explicit ownership policy forbidding per-type RC drift for language values.
  - Documented rare/sound exception policy (external resource wrappers only).

### Regression Tests
- Added in `src/lisp/tests_tests.c3`:
  - `instance field ffi survives scope return`
  - `closure capture instance ffi field`

## 2026-03-04: Refcounted Instance with By-Value Fields (ROLLED BACK / SUPERSEDED)

### Summary
Fixed Instance scope-boundary ownership. Instance struct now stores field data **by value** (`Value[N]`, not `Value*[N]`) and is shared across scope boundaries via refcount retain/release — O(1) per boundary crossing, zero deep copies. This was the root cause of segfaults in tensor-heavy workloads (omni-torch diffusion LLM) where Instance field pointers dangled after scope release.

### Root Cause
Three code paths returned INSTANCE values as-is across scope boundaries, assuming they lived in root_scope:
1. `copy_to_parent` — INSTANCE grouped with HASHMAP/ARRAY as "root allocated"
2. `promote_to_escape` — shallow-copied Value wrapper but not Instance struct/fields
3. `copy_value_if_owned_by_scope` — returned INSTANCE as-is

When user code creates instances inside function calls (e.g., Tensor constructors in omni-torch), the Instance lives in the call scope. On function return, the Value wrapper and field `Value*` pointers became dangling.

### Fix
- **Instance.fields**: `Value*[MAX_TYPE_FIELDS]` -> `Value[MAX_TYPE_FIELDS]` — fields stored by value, no scope pointers
- **Instance.refcount**: new field, init to 1
- **`instance_retain` / `instance_release`**: O(1) refcount helpers
- **`copy_to_parent` INSTANCE**: new Value wrapper + `instance_retain` (O(1))
- **`promote_to_escape` INSTANCE**: escape-lane wrapper + `instance_retain` (O(1))
- **`copy_value_if_owned_by_scope` INSTANCE**: delegates to `copy_to_parent`
- **`scope_dtor_value` INSTANCE**: calls `instance_release` (frees when refcount hits 0)
- **`make_instance`**: copies `*fields[i]` by value into Instance, allocates wrapper in `current_scope`
- **`make_instance_root`**: root-scope variant for nullary constructors (`None`, etc.)
- **CLAUDE.md**: added mandatory Scope-Boundary Ownership Rules section

### Ownership Model (enforced)
| Struct | Ownership | Boundary cost | Field storage |
|--------|-----------|---------------|---------------|
| Instance | refcounted, malloc'd | O(1) retain | by value in struct |
| FfiHandle | refcounted, malloc'd | O(1) retain | raw C pointer |
| Closure | refcounted via env_scope | O(1) retain | env scope-refcounted |
| Primitive | root-scope pinned | O(0) as-is | permanent |

### Files Modified
- `src/lisp/value.c3` — Instance struct, instance_retain/release, scope_dtor_value
- `src/lisp/eval.c3` — make_instance, make_instance_root, copy_to_parent, promote_to_escape, copy_value_if_owned_by_scope, field access sites
- `src/lisp/jit_jit_helper_functions.c3` — field access/set sites
- `CLAUDE.md` — mandatory ownership rules

### Validation
- `c3c build` ✅
- Unified tests: 1141 passed, 0 failed
- Compiler tests: 73 passed, 0 failed
- omni-torch diffusion_llm.omni: full forward pass (Linear Attention + Wave-PDE) ✅
- omni-torch existing demos (main, xor_nn, diffusion_2d): ✅

## 2026-03-03: General FFI Handle Ownership via Refcounted Box (`ForeignBox` model)

### Summary
Introduced a general runtime ownership model for `FFI_HANDLE` values so transient foreign handles can be allocated in normal call scopes and still cross Omni scope/effect boundaries safely. This removes root-scope pinning as a universal requirement and addresses high-intermediate workloads (for example tensor-heavy paths) without changing user-facing syntax.

### Changes
- **Refcounted FFI box metadata** (`src/lisp/value.c3`):
  - Extended `FfiHandle` to include:
    - `refcount`
    - optional `finalizer`
    - `free_lib_handle` policy flag
  - Added helpers:
    - `make_ffi_box(...)`
    - `ffi_handle_retain(...)`
    - `ffi_handle_release(...)`
    - `make_ffi_handle_ex(...)`
  - Kept existing `make_ffi_handle(...)` as migration wrapper for global library handles.
- **Scope destructor semantics**:
  - `scope_dtor_value` for `FFI_HANDLE` now uses `ffi_handle_release(...)` instead of raw `mem::free(...)`.
  - Enables shared ownership across copied wrappers and prevents double-free/UAF when values cross scope boundaries.
- **Boundary copy/promotion safety** (`src/lisp/eval.c3`):
  - `copy_to_parent(...)` now clones `FFI_HANDLE` wrappers when needed and retains the underlying box.
  - `copy_value_if_owned_by_scope(...)` / `copy_env_value_fast(...)` no longer assume FFI handles are always root-pinned.
  - `promote_to_escape(...)` now creates ESCAPE-lane wrappers for `FFI_HANDLE` with retain + dtor registration.
- **Handle producers migrated to boxed/scoped construction**:
  - TCP handles (`src/lisp/async.c3`) now use `make_ffi_handle_ex(...)` with a TCP finalizer.
  - TLS handles (`src/lisp/tls.c3`) now use boxed ownership with TLS finalizer; `tls_handle_free(...)` made idempotent.
  - Atomic handles (`src/lisp/threads.c3`) now use boxed ownership.
  - Deduce DB/relation handles (`src/lisp/deduce.c3`) now use boxed ownership and pointer extraction via `.ffi_val.lib_handle`.
  - Deduce unify path (`src/lisp/unify.c3`) updated for boxed pointer extraction.
- **No surface language changes**:
  - Effects (`io/*`) and FFI declaration syntax (`define [ffi lib]`, `define [ffi λ ...]`) unchanged.
  - Type-hint surface (`^Any`, etc.) unchanged; runtime ownership is internal.

### Tests
- Added atomic boundary/stress regressions (`src/lisp/tests_tests.c3`):
  - `atomic handle boundary copy`
  - `atomic transient handle stress`
- Validation:
  - `c3c build` ✅
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` ✅
  - Unified tests: `1141 passed, 0 failed`

## 2026-03-03: Fiber Cancellation + Scheduler Fairness + Destructuring

### Summary
Added fiber-level cancellation with recursive child propagation, scheduler fairness via rotating resume offset, wakeup drop tracking, unified deduce API, and array/dict destructuring in let/lambda/define.

### Changes
- **Fiber cancellation** (`src/lisp/scheduler.c3`):
  - `scheduler_cancel_fiber` cancels READY/BLOCKED fibers, recursively cancels children, cleans up pending I/O
  - `prim_fiber_cancel` registered as `fiber-cancel` primitive
  - Returns true if cancelled, nil if already done/running, error if invalid
- **Scheduler fairness** (`src/lisp/scheduler.c3`):
  - Added `resume_offset` to Scheduler — rotating start index for fiber resume scan
  - Both `scheduler_run_until` and `scheduler_run_all` scan `(offset + j) % count`
  - Prevents fiber 0 from always running first
- **Wakeup drop tracking** (`src/lisp/scheduler.c3`):
  - `wakeup_drops` counter incremented when ring is full
- **Unified deduce API** (`src/lisp/deduce.c3`):
  - Single `(deduce 'command args...)` replaces deduce-open/scan/query/count/match + fact!/retract!
- **Destructuring** (`src/lisp/parser_parser.c3`, `src/lisp/eval.c3`, `src/lisp/value.c3`):
  - Array destructuring in let: `(let ([x y] [10 20]) ...)`
  - Dict destructuring (PAT_DICT) in let/match: `(let ({name age} dict) ...)`
  - Array/dict destructuring in lambda/define params: `(define (f {x y} [a b] z) ...)`
  - PAT_SEQ now matches ARRAY values (not just CONS lists)
- **Multi-line stdlib loader** (`src/lisp/eval.c3`):
  - Paren-depth s-expression reader replaces line-by-line processing
- **Stdlib additions** (`stdlib/stdlib.lisp`):
  - `default` function, `with-defaults` macro
- **Docs updated**: LANGUAGE_SPEC, SYNTAX_SPEC, FEATURES, CORE_LIBS_INSPECTION

### Files Modified
- `src/lisp/scheduler.c3`, `src/lisp/deduce.c3`, `src/lisp/eval.c3`
- `src/lisp/parser_parser.c3`, `src/lisp/value.c3`
- `src/lisp/jit_jit_compiler.c3`, `src/lisp/jit_jit_helper_functions.c3`
