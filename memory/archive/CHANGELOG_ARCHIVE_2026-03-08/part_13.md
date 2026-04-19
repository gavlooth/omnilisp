  - Preserved:
    - `true`/`false` symbol special handling to JSON booleans
    - list/array emission as JSON arrays
    - hashmap key coercion behavior (`string`/`symbol` else `"?"`)

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 83 - Split JIT Handle Control-Flow Helpers

### Summary
Refactored `jit_handle_impl(...)` by extracting body-switch, signal-dispatch loop, and no-signal finalization helpers, reducing function size and preserving handle/signal behavior.

### What changed
- `src/lisp/jit_jit_handle_signal.c3`:
  - Added:
    - `jit_handle_switch_to_body(ctx, interp)`
    - `jit_handle_dispatch_signals(state, expr, env, interp, ctx, out_result)`
    - `jit_handle_finish_no_signal(expr, env, interp, ctx)`
  - Refactored:
    - `jit_handle_impl(...)` now composes the helper phases above
  - Preserved:
    - stack-context switch/restore behavior and stack-overflow error path
    - signal dispatch loop semantics (resume-completed, re-signal, abort/unmatched return)
    - pending-raise handling on normal completion

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 82 - Split Native Effect Emission Helpers

### Summary
Refactored native-effect flat compilation helpers by extracting focused emit utilities from `compile_handle_flat(...)` and `compile_pattern_bindings(...)`, keeping generated code shape and runtime behavior unchanged.

### What changed
- `src/lisp/compiler_native_effect_compilation_flat_style.c3`:
  - Added:
    - `Compiler.compile_handle_effect_tag(effect_tag)`
    - `Compiler.emit_handle_arrays(clause_count, tags_arr_out, hdlrs_arr_out)`
    - `Compiler.emit_handle_array_slot(name, arr_id, idx, value_r)`
    - `Compiler.emit_pattern_var_assign(name, rhs)`
    - `Compiler.emit_pattern_seq_elem_access(pat, val_name, idx)`
    - `Compiler.emit_pattern_seq_elem_binding(pat, idx)`
  - Refactored:
    - `Compiler.compile_handle_flat(...)` now composes the new handle/tag/array helpers
    - `Compiler.compile_pattern_bindings(...)` now delegates sequence element access and PAT_VAR assignment helpers
  - Preserved:
    - max compiled handle clause emission behavior (`i < 8`)
    - clause/body compilation order and emitted `compiled_handle` call shape
    - pattern binding semantics for `PAT_VAR`, `PAT_SEQ`, and `REST_MIDDLE`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 81 - Split JIT Eval Cache/TCO Scope Helpers

### Summary
Refactored `jit_eval(...)` to extract cache-or-compile lookup and TCO recycle-scope preparation into dedicated helpers, keeping JIT trampoline behavior and lifetime boundaries unchanged.

### What changed
- `src/lisp/jit_jit_eval_scopes.c3`:
  - Added:
    - `jit_lookup_or_compile(expr, interp)`
    - `jit_prepare_tco_recycle(env_io, saved_env, interp)`
  - Refactored:
    - `jit_eval(...)` now delegates cache/compile and TCO recycle preparation logic to these helpers
  - Preserved:
    - `jit_env` save/restore behavior on all return paths
    - JIT compilation failure behavior (`"JIT compilation failed"`)
    - TCO recycle fast-path/fallback semantics and error behavior on scope-allocation failure

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 80 - Split Quasiquote Datum Parse Helpers

### Summary
Refactored `Parser.parse_datum_impl(...)` into focused helper functions for template-only tokens, string literals, quote forms, and list datum parsing while preserving quasiquote datum behavior.

### What changed
- `src/lisp/parser_quasiquote_datum.c3`:
  - Added:
    - `Parser.parse_datum_template_only(template_mode, sym)`
    - `Parser.parse_datum_string()`
    - `Parser.parse_datum_quote(template_mode)`
    - `Parser.parse_datum_list(template_mode)`
  - Refactored:
    - `Parser.parse_datum_impl(...)` now delegates per-token branches to the helpers above
  - Preserved:
    - template-mode handling for `..` and `_`
    - list datum construction and `')'` consumption behavior
    - default fallback to nil datum on unsupported tokens

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 79 - Split Shorthand-Define Parse Pipeline

### Summary
Refactored `Parser.parse_shorthand_define(...)` into focused helpers for optional rest-parameter parsing, body parsing/destructuring, and lambda construction, preserving shorthand-define behavior.

### What changed
- `src/lisp/parser_define_core.c3`:
  - Added:
    - `Parser.parse_shorthand_rest_param(has_rest_out, rest_param_out)`
    - `Parser.parse_shorthand_define_body(e, destr_patterns, destr_param_names, destr_count)`
    - `Parser.build_shorthand_define_lambda(e, params, define_param_anns, define_has_typed, define_has_rest, define_rest_param, body)`
  - Refactored:
    - `Parser.parse_shorthand_define(...)` now composes those helpers
  - Preserved:
    - same error text for missing shorthand function name
    - same error text for invalid `..` rest param syntax
    - same shorthand desugaring to `(define name (lambda ...))`

### Verification
- `c3c build` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 78 - Split Named-Let Parse/Build Phases

### Summary
Refactored `Parser.parse_named_let(...)` into helper phases for bindings parsing, lambda construction, and recursive call construction, preserving named-let desugaring behavior.

### What changed
- `src/lisp/parser_define_core.c3`:
  - Added:
    - `Parser.parse_named_let_bindings(params, inits)`
    - `Parser.build_named_let_lambda(e, params, body)`
    - `Parser.build_named_let_call(e, loop_name, inits)`
  - Refactored:
    - `Parser.parse_named_let(...)` now delegates to the helpers above
  - Preserved:
    - same flat binding-pair parse shape (`name init ...`)
    - same error text for empty named-let binding list
    - same desugared form `(let ^rec (name lambda) (name init...))`

### Verification
- `c3c build` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 77 - Split `parse_define` Annotation and Normal Paths

### Summary
Refactored `Parser.parse_define(...)` into focused helpers for attribute parsing, annotation dispatch, and normal `(define name value)` handling. Parsing behavior and error messages remain unchanged.

### What changed
- `src/lisp/parser_define_core.c3`:
  - Added:
    - `Parser.parse_define_attrs(attrs, attr_count_out)`
    - `Parser.parse_define_with_annotation(e, attrs, attr_count)`
    - `Parser.parse_normal_define(e)`
  - Refactored:
    - `Parser.parse_define(...)` now delegates bracket-annotation and normal-define paths to helpers
  - Preserved:
    - same bracket-attribute parsing constraints (`max 8`)
    - same error text for missing attributes and missing define name
    - same type/ffi/special dispatch behavior

### Verification
- `c3c build` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 76 - Decompose Coroutine Yield Path

### Summary
Refactored `prim_yield(...)` into focused helpers for context validation, suspend/resume state transition, and resume-value return mapping, with no behavior changes.

### What changed
- `src/lisp/primitives_iter_coroutine.c3`:
  - Added:
    - `prim_yield_require_context(current_out, interp)`
    - `prim_yield_suspend_and_restore(current, interp)`
    - `prim_yield_resume_result(interp)`
  - Refactored:
    - `prim_yield(...)` now delegates to the helpers above
  - Preserved:
    - same `"yield: not inside a coroutine"` error behavior
    - same save/suspend/restore/unpin sequence
    - same returned value (`resume_value` or `nil`)

### Verification
- `c3c build` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 75 - Decompose Coroutine Creation Path

### Summary
Refactored `prim_coroutine(...)` into helper stages for thunk validation, thunk preparation, and context/state creation, keeping coroutine semantics unchanged.

### What changed
- `src/lisp/primitives_iter_coroutine.c3`:
  - Added:
    - `prim_coroutine_require_thunk(args, thunk_out, interp)`
    - `prim_coroutine_prepare_thunk(thunk, interp)`
    - `prim_coroutine_create_ctx(thunk, ctx_out, interp)`
  - Refactored:
    - `prim_coroutine(...)` now delegates to the helpers above before `make_coroutine(...)`
  - Preserved:
    - same error strings for missing/invalid thunk
    - same root-promotion + `jit_warm_expr_cache(...)` behavior
    - same stack-context allocation and OOM cleanup behavior

### Verification
- `c3c build` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 74 - Split Coroutine Resume Validation/Switch/Post-Switch

### Summary
Further decomposed `prim_resume(...)` into focused helpers for argument/context validation, stack-context switch, and post-switch status dispatch, while preserving runtime behavior and error messages.

### What changed
- `src/lisp/primitives_iter_coroutine.c3`:
  - Added:
    - `prim_resume_require_ctx(args, coroutine_out, ctx_out, interp)`
    - `prim_resume_switch_context(ctx, interp)`
    - `prim_resume_post_switch(coroutine_val, ctx, interp)`
  - Refactored:
    - `prim_resume(...)` now delegates:
      - coroutine argument + context extraction
      - context switch/resume + interpreter state save/restore
      - post-switch handling (dead/completed/yielded)
  - Preserved:
    - all existing `resume:` error strings
    - pre-switch completed/dead cleanup behavior
    - stack overflow cleanup path
    - yielded-value boundary copy semantics via `prim_resume_yield_result(...)`

### Verification
- `c3c build` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 73 - Coroutine Thunk-State Allocation Helper

### Summary
Refactored coroutine thunk-state setup into a dedicated allocator helper and added explicit OOM handling in `prim_coroutine(...)`.

### What changed
- `src/lisp/primitives_iter_coroutine.c3`:
  - Added:
    - `coroutine_alloc_thunk_state(thunk, interp)`
  - Refactored:
    - `prim_coroutine(...)` now delegates user-data state allocation to helper
  - Hardening:
    - if thunk-state allocation fails, coroutine context is destroyed and a deterministic `"coroutine: out of memory"` error is returned
    - avoids null dereference on allocation failure

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 72 - Share Iterator Argument Validation

### Summary
Extracted shared iterator argument validation used by `next`, `collect`, and `to-array`, removing repeated type/arity checks while preserving per-primitive error messages.

### What changed
- `src/lisp/primitives_iter_coroutine.c3`:
  - Added:
    - `iterator_require_arg(args, expected_msg, iterator_out, interp)`
  - Refactored:
    - `prim_next(...)` now validates via helper
    - `prim_collect(...)` now validates via helper
    - `prim_to_array(...)` now validates via helper
  - Preserved:
    - same error strings (`next|collect|to-array: expected iterator`)
    - same iterator execution/consumption semantics

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 71 - Consolidate Iterator Consumption Loop

### Summary
Refactored duplicated iterator-consumption logic shared by `prim_collect(...)` and `prim_to_array(...)` into one helper, preserving iteration/error semantics.

### What changed
- `src/lisp/primitives_iter_coroutine.c3`:
  - Added:
    - `ITER_BUFFER_MAX` constant (replaces duplicated `4096` literal)
    - `iterator_consume_items(iterator, items, count_out, interp)`
  - Refactored:
    - `prim_collect(...)` now delegates item consumption to helper
    - `prim_to_array(...)` now delegates item consumption to helper
  - Preserved:
    - early stop semantics on `nil` / non-cons / non-iterator rest
    - error propagation when iterator thunk returns `ERROR`
    - output construction behavior for list and array paths

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 70 - Decompose Coroutine `resume` Lifecycle Handling

### Summary
Refactored `prim_resume(...)` by extracting coroutine-context cleanup and terminal/yield result handling into focused helpers, reducing repeated cleanup branches while preserving behavior.

### What changed
- `src/lisp/primitives_iter_coroutine.c3`:
  - Added:
    - `coroutine_ctx_cleanup(coroutine_val, ctx, interp)`
    - `prim_resume_error_and_cleanup(coroutine_val, ctx, interp, msg)`
    - `prim_resume_complete(coroutine_val, ctx, interp)`
    - `prim_resume_yield_result(interp)`
  - `prim_resume(...)` now delegates:
    - already-complete / dead pre-check cleanup
    - stack-overflow path cleanup
    - completed-result return
    - yielded-value copy-to-parent path
  - Preserved:
    - all error strings
    - stack context destroy semantics
    - yielded value copy behavior via `boundary_copy_to_parent_site(...)`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 69 - Decompose Fiber Cancellation Internals

### Summary
Refactored `scheduler_cancel_fiber(...)` by extracting cancellability and child-cancellation helpers, reducing in-function branching while preserving recursive cancellation semantics.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_fiber_is_cancellable(f)`
    - `scheduler_cancel_fiber_children(fiber_id, interp)`
  - `scheduler_cancel_fiber(...)` now:
    - delegates state check to `scheduler_fiber_is_cancellable(...)`
    - delegates recursive child traversal to `scheduler_cancel_fiber_children(...)`
  - Preserved:
    - only READY/BLOCKED fibers cancelable
    - recursive child cancellation before marking parent done
    - same cancellation error payload and done transition

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 68 - Extract TCP-Read Result Mapping Helper

### Summary
Refactored `scheduler_consume_pending_tcp_read(...)` by extracting TCP-read completion-to-Value mapping into a dedicated helper, reducing local branching while preserving behavior.

### What changed
- `src/lisp/scheduler_wakeup_io.c3`:
  - Added:
    - `scheduler_value_from_pending_tcp_read(op, interp)`
  - `scheduler_consume_pending_tcp_read(...)` now delegates result construction to helper.
  - Preserved:
    - timeout/error/empty/non-empty mapping semantics
    - pending read cleanup and UV nowait drain behavior

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 67 - Decompose Wakeup Drain Event Handlers

### Summary
Refactored `drain_wakeups()` by extracting per-event handlers (timer, poll-error, poll-readable, offload-ready) and invalid-fiber cleanup, reducing branching complexity while preserving behavior.

### What changed
- `src/lisp/scheduler_wakeup_io.c3`:
  - Added helpers:
    - `scheduler_handle_invalid_wakeup(ev)`
    - `scheduler_handle_wakeup_timer_expired(fid)`
    - `scheduler_handle_wakeup_poll_error(fid, status)`
    - `scheduler_handle_wakeup_poll_readable(fid)`
    - `scheduler_handle_wakeup_offload_ready(fid, payload)`
  - `drain_wakeups()` now dispatches to helpers instead of inlining all state transitions.
  - Preserved:
    - offload completion cleanup when wakeup targets invalid/inactive fiber
    - TCP read completion and error semantics
    - wakeup tail/head draining behavior

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 66 - Share Thread Task-ID Validation

### Summary
Extracted shared thread task-id validation (negative/range checks) and reused it in join/cancel paths to reduce duplicate bounds checks.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_validate_thread_task_id(raw_id, invalid_msg, task_id_out, interp)`
  - Refactored:
    - `scheduler_thread_join_impl(...)` now uses shared task-id validator
    - `prim_thread_cancel(...)` now uses shared task-id validator after integer parsing
  - Preserved:
    - operation-specific invalid-id messages
    - same control flow for join/cancel behavior

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 65 - Consolidate Existing Fiber-ID Parsing

### Summary
Extracted shared fiber-id parsing/validation for scheduler primitives that require an existing fiber id, reducing duplication between `prim_fiber_cancel(...)` and `prim_await(...)`.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_parse_existing_fiber_arg(args, expected_msg, invalid_msg, fiber_id_out, interp)`
  - Refactored:
    - `prim_fiber_cancel(...)` now uses shared existing-fiber parser
    - `prim_await(...)` now uses shared existing-fiber parser
  - Preserved:
    - same expected/invalid error messages per primitive
    - same out-of-range behavior (`fiber_id >= fiber_count`)
    - same cancellation/await control-flow behavior

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 64 - Decompose Thread-Spawn Queue Setup

### Summary
Extracted thread-spawn task allocation and enqueue setup into a dedicated helper to simplify `prim_thread_spawn(...)` and centralize cleanup/error handling.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_prepare_thread_spawn(work, task_id_out, interp)`
  - Refactored:
    - `prim_thread_spawn(...)` now delegates task allocation/enqueue logic to helper
  - Preserved:
    - task table full handling
    - offload queue full handling
    - blob release and task table rollback behavior on enqueue failure

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 63 - Consolidate Offload Job Parsing

### Summary
Extracted shared offload-job argument parsing used by `prim_offload(...)` and `prim_thread_spawn(...)` to reduce duplication while preserving operation-specific error messages.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_parse_offload_job(args, expected_msg, work, interp)`
  - Refactored:
    - `prim_offload(...)` now uses shared parser helper
    - `prim_thread_spawn(...)` now uses shared parser helper
  - Preserved:
    - distinct `expected job list` messages per primitive
    - same downstream parse/validation behavior from `scheduler_build_offload_work(...)`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 62 - Decompose `prim_offload` Sync/Fiber Paths

### Summary
Split `prim_offload(...)` into focused helpers for synchronous execution and fiber-offload setup, reducing inline branching and queue-setup duplication while preserving behavior.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_run_offload_sync(work, interp)`
    - `scheduler_begin_fiber_offload(work, fiber_id, interp)`
  - `prim_offload(...)` now:
    - delegates non-fiber path to `scheduler_run_offload_sync(...)`
    - delegates pending/queue/block setup to `scheduler_begin_fiber_offload(...)`
  - Preserved existing error semantics:
    - missing job list
    - pending offload already active
    - queue full
    - worker completion allocation failure

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 61 - Decompose Cancel Argument Parsing

### Summary
Extracted shared non-negative integer argument parsing and true-value lookup helpers for scheduler cancel primitives to reduce duplicate validation code.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_parse_nonnegative_int_arg(args, expected_msg, invalid_msg, raw_id_out, interp)`
    - `scheduler_true_value(interp)`
  - Refactored:
    - `prim_thread_cancel(...)` now uses shared arg parser and true-value helper
    - `prim_fiber_cancel(...)` now uses shared arg parser and true-value helper
  - Preserved behavior and messages for:
    - missing/wrong arg type
    - negative id
    - out-of-range id checks
    - cancellation success/done semantics

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 60 - Decompose Thread-Join Validation Paths

### Summary
Extracted shared thread-join context validation and timeout-pair parsing helpers to reduce duplication in scheduler join primitives while preserving error behavior.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_validate_thread_join_context(op_name, interp)`
    - `scheduler_parse_thread_join_timeout_pair(pair, task_id_out, timeout_ms_out, interp)`
  - `prim_thread_join(...)` now uses shared join-context validation helper.
