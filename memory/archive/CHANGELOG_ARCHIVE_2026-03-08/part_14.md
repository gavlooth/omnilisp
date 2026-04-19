  - `prim_thread_join_timeout(...)` now uses:
    - shared join-context validation helper
    - shared timeout-pair parser helper
  - Existing messages preserved for:
    - mutex unavailable
    - in-fiber prohibition
    - malformed timeout pair
    - invalid timeout value

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 59 - Decompose Scheduler Run-Loop Step Logic

### Summary
Extracted the shared scheduler progress step used by both `scheduler_run_until(...)` and `scheduler_run_all(...)` to reduce duplication while preserving scheduling behavior.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_advance_round(interp, target, stop_on_target, target_done_out = null)`
  - `scheduler_run_until(...)` now delegates per-round wakeup/resume/block handling to the helper.
  - `scheduler_run_all(...)` now delegates per-round wakeup/resume/block handling to the helper.
  - Preserved:
    - round limits
    - target-done break behavior
    - UV nowait drain at end of `run_until`
    - reset semantics in `run_all`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 58 - Decompose Scheduler `await` Path

### Summary
Refactored `prim_await(...)` into focused helpers to reduce branching complexity while preserving runtime semantics and error behavior.

### What changed
- `src/lisp/scheduler_primitives.c3`:
  - Added:
    - `scheduler_result_or_nil(...)`
    - `scheduler_await_in_fiber_context(...)`
  - Simplified `prim_await(...)` by delegating:
    - in-fiber await checks/yield/resume handling
    - done-result extraction via shared helper
  - Error messages and await constraints remain unchanged:
    - self-await forbidden
    - only direct-child await inside fibers
    - resumed-before-done guard preserved

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 57 - Decompose `eval_defunion` Registration Path

### Summary
Split `eval_defunion(...)` into focused helpers for union type info initialization, variant type registration, and variant binding while preserving behavior and public API.

### What changed
- `src/lisp/eval_type_evaluators.c3`:
  - Added:
    - `eval_defunion_init_union_info(...)`
    - `eval_defunion_register_variant_type(...)`
    - `eval_defunion_bind_variant(...)`
  - Simplified `eval_defunion(...)` by delegating:
    - union `TypeInfo` construction
    - per-variant type registration
    - per-variant constructor/constant binding
  - Existing error behavior for nullary variant allocation failure retained.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 56 - Decompose JIT Cache-Warm Traversal

### Summary
Refactored `jit_warm_expr_cache(...)` into smaller focused helpers for cache insertion and per-node collection traversal, preserving traversal behavior and API.

### What changed
- `src/lisp/jit_jit_apply_eval.c3`:
  - Added helper functions:
    - `jit_cache_expr(...)`
    - `jit_warm_handle_clauses(...)`
    - `jit_warm_match_clauses(...)`
    - `jit_warm_call_args(...)`
    - `jit_warm_begin_exprs(...)`
    - `jit_warm_module_body(...)`
  - Simplified `jit_warm_expr_cache(...)` to:
    - call `jit_cache_expr(...)` once
    - delegate list/clause loops to helpers
  - Public API and warmup traversal semantics unchanged.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 55 - Decompose JIT Dot-Path `set!` Helper

### Summary
Refactored `jit_eval_set_path(...)` into focused helpers for root lookup, segment traversal, and tail mutation while preserving behavior and error messages.

### What changed
- `src/lisp/jit_jit_closure_define_qq.c3`:
  - Added helper functions:
    - `jit_resolve_set_path_root(...)`
    - `jit_set_path_step_instance(...)`
    - `jit_set_path_step_cons(...)`
    - `jit_set_path_step(...)`
    - `jit_set_path_mutate_cons(...)`
    - `jit_set_path_mutate_instance(...)`
  - Simplified `jit_eval_set_path(...)` to:
    - resolve root
    - traverse intermediate path via `jit_set_path_step(...)`
    - dispatch last-segment mutation to cons/instance helper
  - Public API unchanged; behavior equivalent, including existing error text.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 54 - Type Constructor Allocation Failure Hardening

### Summary
Hardened type/union constructor runtime paths against allocation failure to avoid null dereference and silent invalid global bindings.

### What changed
- `src/lisp/eval_type_evaluators.c3`:
  - `make_instance_in_scope(...)`:
    - checks wrapper allocation result from `boundary_alloc_value_in_scope(...)`
    - releases `owner_scope` and returns `null` on wrapper allocation failure
  - `prim_type_constructor(...)`:
    - checks `make_instance(...)` result for null
    - returns explicit runtime error on allocation failure
  - `eval_defunion(...)` (nullary variants):
    - checks `make_instance_root(...)` result
    - returns explicit `eval_error(...)` when constant instance allocation fails

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 53 - Run Result Promotion Boundary Scope Discipline

### Summary
Removed direct scope mutation in `run_promote_result(...)` and routed promotion-time scope transitions through boundary enter/leave helpers with `defer`.

### What changed
- `src/lisp/eval_run_pipeline.c3`:
  - `run_promote_result(...)` now:
    - enters `saved_scope` via `boundary_enter_scope(...)` and restores with `defer boundary_leave_scope(...)`
    - enters `child_scope` during escape promotion via `boundary_enter_scope(...)` and restores via `boundary_leave_scope(...)`
  - removed direct manual assignments:
    - `interp.current_scope = saved_scope`
    - `interp.current_scope = child_scope`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 52 - Root Promotion + JIT Closure Env-Scope Guards

### Summary
Removed another manual scope/releasing-scope swap in root-promotion and hardened JIT closure env-scope allocation with explicit failure handling.

### What changed
- `src/lisp/eval_promotion_escape.c3`:
  - `promote_to_root_site(...)` now delegates to:
    - `boundary_copy_to_scope_site(v, interp, interp.root_scope, releasing_scope, site)`
  - removed direct manual mutation/restoration of:
    - `interp.current_scope`
    - `interp.releasing_scope`
- `src/lisp/jit_jit_closure_define_qq.c3`:
  - `jit_copy_closure_env_if_needed(...)` now returns `bool` and validates:
    - detached env-scope allocation success
    - copied env success
    - releases detached env scope on copy failure
  - `jit_make_closure_from_expr(...)` now raises explicit runtime error when env-scope copy setup fails (zero-arg and regular lambda paths)

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 51 - JIT Scoped-Eval Boundary Hardening

### Summary
Reduced manual scope mutation in JIT scoped-eval paths by reusing boundary helpers and added explicit allocation-failure guards for recyclable call-scope creation.

### What changed
- `src/lisp/jit_jit_eval_scopes.c3`:
  - `jit_finalize_scoped_result(...)`:
    - replaced direct `current_scope` save/switch/restore around `boundary_promote_to_escape(...)` with:
      - `boundary_enter_scope(...)`
      - `boundary_leave_scope(...)`
  - `jit_eval_in_single_scope(...)`:
    - switched child-scope setup to:
      - `boundary_push_child_scope(...)`
    - added explicit error return when scope allocation fails
  - `jit_eval_in_call_scope(...)`:
    - switched child-scope setup to:
      - `boundary_push_child_scope(...)`
    - added explicit error return when scope allocation fails
  - `jit_eval(...)` TCO fallback:
    - added null-check for fresh recycle scope allocation
    - returns explicit runtime error instead of proceeding with null scope

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 50 - Child-Scope Boundary Push/Pop

### Summary
Added dedicated boundary helpers for child-scope lifecycle and migrated `run`/REPL eval paths to use them, removing duplicated manual scope push/pop code and centralizing failure handling.

### What changed
- `src/lisp/eval_boundary_api.c3`:
  - Added:
    - `boundary_push_child_scope(interp, saved_scope_out = null)`
    - `boundary_pop_child_scope(interp, saved_scope, child_scope)`
  - Hardened `boundary_push_child_scope(...)`:
    - does not mutate `interp.current_scope` when `scope_create` fails
    - still returns saved scope through `saved_scope_out` for callers
- `src/lisp/eval_run_pipeline.c3`:
  - `run(...)` now uses boundary child-scope helpers with `defer`
  - removed manual `scope_create/scope_release` and duplicated unwind paths
- `src/lisp/eval_repl.c3`:
  - `repl_eval_line(...)` now uses boundary child-scope helpers with `defer`
  - added explicit error return on child-scope allocation failure

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 49 - Scope Enter/Leave Boundary Helpers

### Summary
Standardized temporary scope switching with dedicated boundary helpers and adopted them in macro hygiene capture path.

### What changed
- `src/lisp/eval_boundary_api.c3`:
  - Added:
    - `boundary_enter_scope(interp, target_scope)` → returns prior scope
    - `boundary_leave_scope(interp, saved_scope)`
- `src/lisp/macros_expansion.c3`:
  - `capture_template_bindings_in_root_scope(...)` now uses:
    - `boundary_enter_scope(...)`
    - `boundary_leave_scope(...)` with `defer`
  - Replaces direct manual scope assignment/restoration.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 48 - Instance Wrapper Allocation Cleanup

### Summary
Removed the remaining wrapper-scope `current_scope` mutation in instance construction by routing wrapper allocation through boundary alloc helpers.

### What changed
- `src/lisp/eval_type_evaluators.c3`:
  - `make_instance_in_scope(...)` now allocates wrapper `Value` with:
    - `boundary_alloc_value_in_scope(interp, wrapper_scope, true)`
  - Removed explicit:
    - `interp.current_scope = wrapper_scope`
    - wrapper `alloc_value` + manual dtor registration
    - `interp.current_scope = saved_scope`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 47 - Macro Hygiene Capture Scope Helper

### Summary
Refactored macro hygiene definition-time capture to a dedicated helper with scoped restoration via `defer`, reducing inline scope-switch boilerplate in `eval_define_macro`.

### What changed
- `src/lisp/macros_expansion.c3`:
  - Added:
    - `capture_template_bindings_in_root_scope(...)`
  - `eval_define_macro(...)` now calls this helper per clause instead of open-coded:
    - save current scope
    - switch to root scope
    - capture template bindings
    - restore scope

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 46 - JIT Closure Env-Copy Simplification Follow-up

### Summary
Simplified JIT closure env-copy plumbing further by removing redundant scope parameter/state restoration after introducing target-scope env-copy boundary helper.

### What changed
- `src/lisp/jit_jit_closure_define_qq.c3`:
  - `jit_copy_closure_env_if_needed(...)`:
    - removed redundant `creation_scope` parameter
    - now only takes `need_env_copy` toggle
  - `jit_make_closure_from_expr(...)`:
    - removed temporary `creation_scope` local
    - computes `need_env_copy` directly from `interp.current_scope`
  - Call sites updated accordingly.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 45 - Scoped Env-Copy Boundary Helper

### Summary
Added a dedicated boundary helper for env-chain copy into an explicit target scope and migrated JIT closure env-copy path to use it.

### What changed
- `src/lisp/eval_boundary_api.c3`:
  - Added:
    - `boundary_copy_env_to_target_scope(env, interp, target_scope, depth = 0, ctx = null)`
  - Encapsulates target-scope save/switch/restore around env-copy.
- `src/lisp/jit_jit_closure_define_qq.c3`:
  - `jit_copy_closure_env_if_needed(...)` now uses:
    - `boundary_copy_env_to_target_scope(...)`
  - Removed direct env-copy scope switching in that path.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 44 - Recursive Closure Patch Path Scope Cleanup

### Summary
Removed manual `current_scope` switching in recursive closure patching by allocating directly in the closure env scope through boundary/value-scope alloc paths.

### What changed
- `src/lisp/jit_jit_closure_define_qq.c3`:
  - In `jit_patch_rec_closure_in_env_scope(...)`:
    - replaced direct `interp.current_scope` mutation with:
      - `boundary_alloc_value_in_scope(interp, env_scope)`
      - direct `env_scope.alloc(...)` for closure payload/params/type_sig
    - preserves existing behavior while reducing implicit scope-state mutation.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 43 - Root Env-Extend Boundary Helper

### Summary
Consolidated root-scope env extension into boundary helpers and removed direct `current_scope` mutation from the JIT root-env extension path.

### What changed
- `src/lisp/eval_boundary_api.c3`:
  - Added:
    - `boundary_env_extend_in_scope(interp, target_scope, env, name, value)`
    - `boundary_env_extend_in_root(interp, env, name, value)`
- `src/lisp/jit_jit_closure_define_qq.c3`:
  - `jit_env_extend_root(...)` now uses:
    - `boundary_env_extend_in_root(...)`
  - Removed manual `current_scope` save/switch/restore logic from this path.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 42 - Module Env Root Allocation via Boundary Helper

### Summary
Migrated file/module-load env creation paths from manual root-scope switching to the audited boundary env allocator helper.

### What changed
- `src/lisp/eval_boundary_api.c3`:
  - Added:
    - `boundary_make_env_in_scope(interp, target_scope, parent)`
    - `boundary_make_env_in_root(interp, parent)`
- `src/lisp/jit_jit_module_import.c3`:
  - Replaced two manual root-scope `make_env(...)` blocks with:
    - `boundary_make_env_in_root(interp, interp.global_env)`
  - Affects:
    - declared module evaluation path
    - file-module creation path

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 41 - JIT Method-Table Wrapper Allocation Cleanup

### Summary
Removed another manual root-scope allocation block in the JIT define path by routing method-table wrapper creation through boundary alloc helpers.

### What changed
- `src/lisp/jit_jit_closure_define_qq.c3`:
  - `jit_make_method_table_value(...)` now allocates via:
    - `boundary_alloc_value_in_root(interp, true)`
  - Removed local root-scope save/restore + explicit dtor registration block.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 40 - Additional Root Wrapper Sites Migrated

### Summary
Continued root-lifetime boundary cleanup by migrating additional wrapper allocation sites from manual root-scope switching to boundary allocator helpers.

### What changed
- `src/lisp/prim_collection_hashmap.c3`:
  - `make_hashmap(...)` now allocates wrapper via:
    - `boundary_alloc_value_in_root(interp, true)`
- `src/lisp/eval_init_primitives.c3`:
  - `register_dispatched_prim(...)` now allocates method-table wrapper via:
    - `boundary_alloc_value_in_root(interp, true)`
  - Removed local root-scope save/restore block.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 39 - Value Constructor Root/Scoped Allocation Cleanup

### Summary
Applied boundary allocator helpers to core value constructors so root/scoped wrapper allocation no longer manually mutates `interp.current_scope` in those paths.

### What changed
- `src/lisp/value_constructors.c3`:
  - `make_primitive(...)` now allocates via:
    - `boundary_alloc_value_in_root(interp, true)`
  - `make_ffi_handle_ex(...)` now allocates via:
    - `boundary_alloc_value_in_scope(interp, target_scope, true)`
  - Removed local save/restore `current_scope` choreography in both constructors.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1105 passed, 0 failed (ASAN-mode skips active)
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 38 - Root-Scope Value Allocation via Boundary Helpers

### Summary
Reduced repeated root-scope scope-switch choreography by introducing boundary allocation helpers and migrating core wrapper constructors to use them.

### What changed
- `src/lisp/eval_boundary_api.c3`:
  - Added:
    - `boundary_alloc_value_in_scope(interp, target_scope, register_dtor = false)`
    - `boundary_alloc_value_in_root(interp, register_dtor = false)`
  - Helpers centralize save/restore of `interp.current_scope` for value-wrapper allocation in target/root scope.
- `src/lisp/value_predicates_accessors_core.c3`:
  - Migrated constructors:
    - `make_array(...)`
    - `make_module(...)`
    - `make_coroutine(...)`
  - Replaced inline root-scope switching with boundary allocator calls.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 37 - Remove Local Scoped-Copy Shim + TCO Recycle Guard Helper

### Summary
Continued internal simplification by removing one remaining local scoped-copy shim and encapsulating TCO recycle fallback boundary state handling in a focused helper.

### What changed
- `src/lisp/jit_jit_closure_define_qq.c3`:
  - Replaced local call through removed shim with direct audited boundary API call:
    - `boundary_copy_to_scope_site(...)`
  - Applies to instance field mutation path in `jit_eval_set(...)`.
- `src/lisp/eval_env_copy.c3`:
  - Removed now-unused local `copy_to_scope_site(...)` shim.
  - Env-copy paths now rely on boundary APIs directly.
- `src/lisp/jit_jit_eval_scopes.c3`:
  - Added helper:
    - `jit_copy_tco_env_chain_for_recycle(...)`
  - Encapsulates:
    - temporary `releasing_scope` installation
    - promotion-context begin/end for TCO bounce copy
    - restoration of prior `releasing_scope`
  - `jit_eval(...)` TCO fallback now delegates to this helper.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 36 - Scoped Copy Boundary Consolidation

### Summary
Continued ownership-boundary hardening by centralizing scoped copy state transitions (`current_scope` + `releasing_scope`) into the audited boundary surface and removing duplicated manual choreography from runtime business paths.

### What changed
- `src/lisp/eval_boundary_api.c3`:
  - Added:
    - `boundary_copy_to_scope_site(v, interp, target_scope, releasing_scope, site)`
  - This helper performs:
    - save/restore `interp.current_scope`
    - save/restore `interp.releasing_scope`
    - scoped `copy_to_parent_site` through the boundary API
- `src/lisp/eval_env_copy.c3`:
  - `copy_to_scope_site(...)` now delegates to:
    - `boundary_copy_to_scope_site(...)`
  - Eliminates duplicated local scope/releasing save/restore implementation in env-copy path.
- `src/lisp/eval_type_evaluators.c3`:
  - `make_instance_in_scope(...)` field copy loop now uses:
    - `boundary_copy_to_scope_site(field, ..., owner_scope, saved_scope, COPY_SITE_GENERIC)`
  - Removed direct `releasing_scope` manipulation around instance field copy.
- Previously added Session 35 helper remains in use:
  - `boundary_copy_from_releasing_scope(...)` in:
    - `eval_run_pipeline.c3`
    - `jit_jit_eval_scopes.c3`

### Boundary surface state
