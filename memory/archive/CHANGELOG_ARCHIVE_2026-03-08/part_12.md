### Summary
Completed defer-substrate wiring for `jit_eval_in_call_scope(...)` and added explicit defer-argument retargeting so TCO scope replacement keeps destroy/clone cleanup attached to the currently active call scope.

### What changed
- `src/stack_engine.c3`:
  - Added `stack_ctx_defer_update_arg(StackCtx*, usz idx, void* arg)`:
    - updates an existing defer entry payload without changing callback ops.
    - used when ownership target moves during execution.
  - Added stack test: `test_stack_ctx_defer_update_arg`.
  - Updated stack-engine test runner to include new test.
- `src/lisp/jit_jit_eval_scopes.c3`:
  - `jit_eval_in_call_scope(...)` now:
    - registers call-scope cleanup defer when running on a stack context,
    - saves the defer slot id in interpreter runtime state,
    - pops defer entry on normal completion after scoped finalization.
  - `jit_prepare_tco_recycle(...)` now:
    - retargets defer payload from old scope to fresh recycled scope before releasing old scope.
    - raises error if retargeting fails instead of continuing with stale cleanup metadata.
- `src/lisp/value_interp_state.c3`:
  - Added runtime fields to track active call-scope defer metadata:
    - `tco_scope_defer_ctx`
    - `tco_scope_defer_slot`
    - `tco_scope_defer_active`
  - Initialized and cleared these fields in init/destroy paths.
- `src/lisp/jit_common.c3`:
  - Extended `SavedInterpState` save/restore to include new defer-tracking fields.

### Invariants preserved
- Stack engine remains generic/opaque (callbacks + payload only).
- No direct `ScopeRegion` policy was introduced into stack core.
- Scope/region ownership remains authoritative; no per-type RC ownership model was added.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Stack engine: `14 passed, 0 failed`
  - Unified: `1144 passed, 0 failed`
  - Compiler: `73 passed, 0 failed`
- `c3c build --sanitize=address` passes.
- ASAN run with leak detection still reports known JIT allocator leaks:
  - `2880 bytes` in `36 allocations` from `jit_alloc` (`/tmp/lightning-2.2.3/lib/jit_memory.c:85`).
  - No new scope-teardown leak signature introduced by this session.

## 2026-03-05: Session 104 - JIT Single-Scope Boundary Wiring via Defer Substrate

### Summary
Integrated the new stack defer substrate into `jit_eval_in_single_scope(...)` so child call-scope cleanup is registered on active stack contexts and remains safe under suspend/clone destroy paths.

### What changed
- `src/lisp/jit_jit_eval_scopes.c3`:
  - Added generic boundary defer callbacks:
    - `jit_scope_release_defer_destroy`
    - `jit_scope_release_defer_clone`
  - In `jit_eval_in_single_scope(...)`:
    - registers call-scope cleanup via `main::stack_ctx_defer(...)` when executing on a stack context,
    - pops defer entry via `main::stack_ctx_undefer(...)` on normal completion after scoped-finalization,
    - preserves existing promotion/finalization behavior.

### Why this is scoped
- Applied only to `jit_eval_in_single_scope` for now (non-TCO closure path).
- `jit_eval_in_call_scope` remains unchanged in this session because TCO recycle can replace/release scopes mid-loop and needs dedicated defer argument rebasing rules.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Stack engine: 13 passed, 0 failed
  - Unified: 1144 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- ASAN run with leak detection (`detect_leaks=1`) still fails overall due known JIT allocation leaks (`jit_alloc`), but leak profile improved:
  - Previous run: `3528` bytes in `38` allocations
  - Current run: `2880` bytes in `36` allocations
  - Prior scope leak signature (`scope_chunk_alloc` from `jit_eval_in_single_scope`) no longer appears in tail leak summary.

## 2026-03-05: Session 103 - Stack Engine Generic Defer Substrate (Phase 1 Foundation)

### Summary
Implemented a generic stack-context defer substrate in `stack_engine.c3` to centralize destroy-time cleanup and clone-time semantic ownership hooks without introducing Lisp-specific ownership logic in the stack engine.

### What changed
- `src/stack_engine.c3`:
  - Added generic defer types:
    - `StackCtxDeferFn`
    - `StackCtxDeferOps { destroy_fn, clone_fn }`
    - `StackCtxDeferEntry`
  - Added `StackCtx` defer storage:
    - inline fast-path slots (`STACK_CTX_DEFER_INLINE_CAP`)
    - heap overflow buffer (`defer_heap`)
    - counters (`defer_count`, `defer_capacity`)
  - Added defer API:
    - `stack_ctx_defer(...)`
    - `stack_ctx_undefer(...)`
  - Added internals:
    - reserve/copy helpers
    - destroy-time callback drain (LIFO)
    - defer storage cleanup
  - Added clone integration:
    - clone duplicates defer metadata
    - clone invokes `ops.clone(arg)` hooks for semantic ownership duplication
  - Updated pool shutdown/destroy to free defer heap storage safely.
  - Added new stack engine tests:
    - defer destroy order (LIFO)
    - undefer pop behavior
    - clone hook invocation on clone path

### Invariants preserved
- Stack engine remains generic (opaque callback+arg only).
- No direct `ScopeRegion` operations were added to defer substrate APIs.
- No per-type RC lifetime system was introduced for language values.

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Stack engine: 13 passed, 0 failed (new defer tests included)
  - Unified: 1144 passed, 0 failed
  - Compiler: 73 passed, 0 failed
- `c3c build --sanitize=address` passes.
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`:
  - Runtime tests pass functionally.
  - LeakSanitizer reports pre-existing leaks in JIT/scope paths (`jit_alloc` + known scope leak signatures), not introduced by defer-substrate tests.

## 2026-03-04: Session 102 - Split Parser Defunion Header/Variant Helpers

### Summary
Refactored `Parser.parse_defunion(...)` into focused helpers for union-header parsing and variant parsing, preserving accepted union syntax and existing error behavior.

### What changed
- `src/lisp/parser_type_defs.c3`:
  - Added:
    - `Parser.init_defunion_expr(e)`
    - `Parser.parse_defunion_name_compound(e)`
    - `Parser.parse_defunion_name(e)`
    - `Parser.parse_defunion_variant_compound(v)`
    - `Parser.parse_defunion_variant(e)`
  - Refactored:
    - `Parser.parse_defunion(...)` now delegates header and variant loops to helpers above
  - Preserved:
    - simple and parenthesized union-name forms
    - variant forms (`Variant` and `(Variant fields...)`)
    - variant/type-parameter limits
    - error strings:
      - `expected union name`
      - `expected union name after [union]`
      - `expected variant name`
      - `too many union variants`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1144 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 101 - Split Parser Deftype Header/Field Helpers

### Summary
Refactored `Parser.parse_deftype(...)` into focused helpers for header parsing and field append logic, preserving accepted syntax and existing error messages.

### What changed
- `src/lisp/parser_type_defs.c3`:
  - Added:
    - `Parser.init_deftype_expr(e)`
    - `Parser.parse_deftype_name_compound(e)`
    - `Parser.parse_deftype_name(e)`
    - `Parser.parse_deftype_typed_field(e)`
    - `Parser.parse_deftype_bare_field(e)`
  - Refactored:
    - `Parser.parse_deftype(...)` now delegates name/header and typed/bare field loops to the helpers above
  - Preserved:
    - simple and parenthesized type-name forms
    - typed field and bare symbol field parsing behavior
    - error strings:
      - `expected type name`
      - `expected type name after [type]`
      - `expected field name`
      - `too many type fields`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1144 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 100 - Split Parser Export-From Helpers

### Summary
Refactored `Parser.parse_export_from(...)` into focused helper functions for initialization, source-module parsing, specifier parsing, list growth, and list element append semantics.

### What changed
- `src/lisp/parser_import_export.c3`:
  - Added:
    - `Parser.init_export_from_expr(e)`
    - `Parser.parse_export_from_source_module(e)`
    - `Parser.ensure_export_from_name_capacity(e)`
    - `Parser.parse_export_from_name_list(e)`
    - `Parser.parse_export_from_specifiers(e)`
  - Refactored:
    - `Parser.parse_export_from(...)` now delegates to the helpers above
  - Preserved:
    - accepted forms: `(export-from mod :all)` and `(export-from mod (sym1 sym2 ...))`
    - existing error strings and parse order
    - dynamic growth behavior for export-from names

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1144 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 99 - Fix ASAN JIT Escape-Scope Stack Overflow + Re-enable Coverage

### Summary
Reproduced the ASAN crash on the previously skipped JIT escape-scope path, fixed the root cause in env-copy boundary context handling, and re-enabled ASAN execution of JIT + escape-scope/TCO suites as regression coverage.

### Reproduction (before fix)
```bash
c3c build --sanitize=address
ASAN_OPTIONS=detect_leaks=0:abort_on_error=1:halt_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main
```

### Failing ASAN stack trace (before fix)
```
ERROR: AddressSanitizer: stack-buffer-overflow
WRITE of size 40 at ... in lisp.copy_env_to_scope
  #1 lisp.copy_env_to_scope src/lisp/eval_env_copy.c3:159
  #2 lisp.boundary_copy_env_to_scope src/lisp/eval_boundary_api.c3:210
  #3 lisp.boundary_copy_env_to_target_scope src/lisp/eval_boundary_api.c3:226
  #4 lisp.jit_copy_closure_env_if_needed src/lisp/jit_jit_closure_define_qq.c3:46
  #5 lisp.jit_make_closure_from_expr src/lisp/jit_jit_closure_define_qq.c3:137
```

### Root cause
- `copy_env_to_scope(...)` created a stack-local `PromotionContext` (`local_ctx`) for top-level boundary copies.
- Under the JIT closure env-copy call chain, ASAN consistently flagged a stack redzone overflow at that stack-local context write path.
- This boundary context is logically boundary-owned metadata and does not need stack storage.

### Fix
- `src/lisp/eval_env_copy.c3`:
  - Replaced stack-local `PromotionContext local_ctx = {}` with scope-allocated boundary context:
    - `PromotionContext* local_ctx = (PromotionContext*)interp.current_scope.alloc(PromotionContext.sizeof);`
    - `promotion_context_begin(interp, local_ctx)` / `promotion_context_end(...)` unchanged semantically.
  - This keeps ownership aligned with Omni’s region model and removes stack-frame coupling in JIT-driven boundary-copy paths.

### Regression coverage changes
- `src/lisp/tests_tests.c3`:
  - Removed ASAN-only JIT disabling and suite skips in `run_lisp_tests()`.
  - ASAN now runs:
    - JIT checks in unified helper assertions
    - escape-scope optimization suite
    - TCO scope recycling suite
- `src/lisp/tests_escape_scope_tests.c3`:
  - Added targeted regression:
    - `escape-scope: captured env map+reverse`
    - `(let (m 10) (car (reverse (map (lambda (x) (* x m)) (quote (1 2 3))))))`

### Verification
- ASAN build + full suite:
  - `c3c build --sanitize=address` passes
  - `ASAN_OPTIONS=detect_leaks=0:abort_on_error=1:halt_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes
  - Unified: 1144 passed, 0 failed
  - Compiler: 73 passed, 0 failed
  - No ASAN errors; no ASAN skip banners for escape-scope/TCO suites
- Normal build + full suite:
  - `c3c build` passes
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes
  - Unified: 1144 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 98 - Split Parser Module-Decl Allocation/Append Helpers

### Summary
Refactored `Parser.parse_module(...)` into focused helpers for module allocation, export-header parsing, capacity growth, and append operations while preserving parse flow and error messages.

### What changed
- `src/lisp/parser_module_decl.c3`:
  - Added:
    - `parser_module_ensure_export_capacity(module_expr)`
    - `parser_module_ensure_body_capacity(module_expr)`
    - `Parser.alloc_module_expr(name)`
    - `Parser.parse_module_export_keyword()`
    - `Parser.module_append_export(module_expr)`
    - `Parser.module_append_body_expr(module_expr)`
  - Refactored:
    - `Parser.parse_module(...)` now delegates allocation, export parsing, and body append/capacity paths to helpers above
  - Preserved:
    - required `(export ...)` contract and existing error strings
    - export/body growth semantics and append order
    - closing-paren expectations for export list and module

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 97 - Split Lexer Advance Literal/Symbol Helpers

### Summary
Refactored `Lexer.advance(...)` by extracting number-start detection and literal/symbol scan helpers, preserving tokenization behavior while reducing branching in the main advance path.

### What changed
- `src/lisp/parser_lexer.c3`:
  - Added:
    - `Lexer.is_number_start(c)`
    - `Lexer.scan_literal_or_symbol(c)`
    - `Lexer.set_error_token()`
  - Refactored:
    - `Lexer.advance(...)` now delegates literal/symbol handling and error token setup to the helpers above
  - Preserved:
    - hash-reader dispatch fallback behavior
    - string, number (including leading `-`), dot token, underscore, and symbol/path scanning semantics
    - unknown-character fallback to `T_ERROR`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 96 - Split REPL Session/Loop Orchestration Helpers

### Summary
Refactored `repl(...)` into focused setup/shutdown/banner/step helpers so the top-level REPL entry point stays a thin orchestrator with unchanged behavior.

### What changed
- `src/lisp/eval_repl.c3`:
  - Added:
    - `repl_init_session(interp, rx_out, history_file)`
    - `repl_shutdown_session(rx, history_file)`
    - `repl_print_banner()`
    - `repl_step(rx, interp, buf, buf_len, ansi_red, ansi_green, ansi_reset)`
  - Refactored:
    - `repl(...)` now delegates initialization, one-iteration processing, and shutdown to helper functions above
  - Preserved:
    - SIGINT handler setup timing
    - replxx initialization/configuration/history lifecycle
    - `quit`/`exit`, EOF, multiline buffering, and eval-output behavior
    - per-iteration prompt and eval flow

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 95 - Split REPL Parenthesis-Depth State Machine Helpers

### Summary
Refactored `count_paren_depth(...)` into an explicit state struct plus per-mode helper steps (string/comment/normal), preserving delimiter-depth behavior while reducing control-flow nesting.

### What changed
- `src/lisp/eval_repl.c3`:
  - Added:
    - `ReplParenDepthState` struct
    - `repl_paren_depth_step_string(c, state)`
    - `repl_paren_depth_step_comment(c, state)`
    - `repl_paren_depth_step_normal(c, state)`
  - Refactored:
    - `count_paren_depth(...)` now loops over input and delegates mode transitions to helper steps
  - Preserved:
    - escaped-quote handling inside strings
    - comment-to-newline behavior
    - net `(`/`)` depth accounting

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 94 - Split REPL Completion Helpers

### Summary
Refactored `lisp_completion(...)` by extracting focused helpers for C-string length, word-break detection, prefix start scanning, prefix matching, and completion emission.

### What changed
- `src/lisp/eval_repl.c3`:
  - Added:
    - `repl_cstr_len_ptr(s)`
    - `repl_is_completion_word_break(c)`
    - `repl_completion_find_word_start(input, len)`
    - `repl_completion_matches_prefix(name, prefix, prefix_len)`
    - `repl_completion_emit(completions, name)`
  - Refactored:
    - `lisp_completion(...)` now delegates scanning/matching/emission to the helpers above
  - Preserved:
    - current-word boundary semantics
    - context length calculation for replxx
    - global-env symbol completion behavior
    - 255-byte truncation behavior for completion strings

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 93 - Split REPL Highlighter State/Step Helpers

### Summary
Refactored `lisp_highlighter(...)` by extracting explicit highlighter state and focused per-mode helpers (comment/string/normal), preserving REPL syntax-coloring behavior while reducing branch density in the main loop.

### What changed
- `src/lisp/eval_repl.c3`:
  - Added:
    - `ReplHighlighterState` struct
    - `REPL_PAREN_COLORS` constant palette
    - `repl_highlighter_is_whitespace(c)`
    - `repl_highlighter_is_quote_prefix(input, i)`
    - `repl_highlighter_flush_symbol(input, colors, state, pos)`
    - `repl_highlighter_step_comment(c, colors, i, state)`
    - `repl_highlighter_step_string(c, colors, i, state)`
    - `repl_highlighter_color_paren(colors, i, c, state)`
    - `repl_highlighter_step_normal(input, colors, i, c, state)`
  - Refactored:
    - `lisp_highlighter(...)` now loops and delegates to the step helpers above
  - Preserved:
    - comment/string/escape handling
    - paren-depth color cycling semantics
    - symbol flush behavior and keyword/constant coloring
    - quote-prefix, digit, and bracket color rules

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 92 - Split JIT Shift-Detection Traversal Helpers

### Summary
Refactored `expr_contains_shift(...)` by extracting shared traversal helpers for expression slices, match-clause results, and binary expression branches, preserving shift-detection semantics.

### What changed
- `src/lisp/jit_jit_apply_eval.c3`:
  - Added:
    - `expr_slice_contains_shift(exprs, count)`
    - `match_results_contain_shift(clauses, clause_count)`
    - `expr_contains_shift_binary(left, right)`
  - Refactored:
    - `expr_contains_shift(...)` now delegates repeated loop/branch patterns to the helpers above
  - Preserved:
    - `E_SHIFT` direct detection
    - lambda/reset/handle boundary behavior (`false` in those delimited cases)
    - recursive traversal coverage for `if/let/begin/call/app/match/and/or/...`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 91 - Split JIT Index Dispatch Helpers

### Summary
Refactored `jit_do_index(...)` into focused per-collection helpers (list/string/array/dict/instance) plus a shared type-error builder, preserving index behavior and error messages.

### What changed
- `src/lisp/jit_jit_module_import.c3`:
  - Added:
    - `jit_try_index_list(interp, collection, index, out)`
    - `jit_try_index_string(interp, collection, index, out)`
    - `jit_try_index_array(interp, collection, index, out)`
    - `jit_try_index_dict(interp, collection, index, out)`
    - `jit_try_index_instance(interp, collection, index, out)`
    - `jit_index_type_error(interp, collection)`
  - Refactored:
    - `jit_do_index(...)` now performs ordered helper dispatch and returns shared fallback type errors
  - Preserved:
    - negative indexing behavior for lists/strings/arrays
    - UTF-8 codepoint indexing behavior for strings
    - dict missing-key behavior (`nil`)
    - instance `ref` method-table dispatch behavior
    - index/type error strings

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 90 - Split Mutable-Capture Prescan Helpers

### Summary
Refactored `prescan_mutable_captures(...)` by extracting dedup/scan helpers for let nodes and repeated child-list traversal, preserving mutable-capture discovery behavior.

### What changed
- `src/lisp/compiler_mutable_capture_prescan.c3`:
  - Added:
    - `Compiler.mutable_capture_contains(sym)`
    - `Compiler.add_mutable_capture_if_absent(sym)`
    - `Compiler.prescan_let_expr(expr)`
    - `Compiler.prescan_expr_list(exprs, count)`
    - `Compiler.prescan_match_clause_results(expr)`
    - `Compiler.prescan_handle_clause_bodies(expr)`
  - Refactored:
    - `Compiler.prescan_mutable_captures(...)` now delegates repeated loops and let-specific logic to helpers above
  - Preserved:
    - mutable-capture detection criteria (`is_mutable_capture(let_name, let_body)`)
    - recursive traversal coverage across expression variants
    - dedup semantics for `mutable_captures`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 89 - Split Deduce Relation Define Pipeline

### Summary
Refactored `prim_define_relation(...)` into focused helpers for spec extraction, relation allocation, column collection, and LMDB open flow, preserving behavior and error messages.

### What changed
- `src/lisp/deduce_schema_query.c3`:
  - Added:
    - `deduce_relation_extract_spec(pair, interp, db_out, rel_name_out, cols_out)`
    - `deduce_relation_alloc(db, rel_name)`
    - `deduce_relation_collect_columns(rel, cols)`
    - `deduce_relation_open_dbi(rel, interp)`
    - `DeduceRelationOpenStatus` enum for open failure classification
  - Refactored:
    - `prim_define_relation(...)` now composes the helpers above
  - Preserved:
    - argument/type validation semantics
    - LMDB open behavior and relation handle wrapping
    - distinct write-failure messages:
      - `__define-relation: txn begin failed`
      - `__define-relation: dbi open failed`

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 88 - Split Expr Serializer Specialized Dispatch

### Summary
Refactored `serialize_expr_specialized(...)` into two focused helpers (`core forms` and `reader shorthand forms`) while preserving serialized output shape for all specialized expression tags.

### What changed
- `src/lisp/compiler_expr_serialize_exprs.c3`:
  - Added:
    - `Compiler.serialize_expr_specialized_core(expr, buf)`
    - `Compiler.serialize_expr_specialized_reader(expr, buf)`
  - Refactored:
    - `Compiler.serialize_expr_specialized(expr, buf)` now delegates to the two helpers above
  - Preserved:
    - specialized handling for app/if/define/quote/reset/shift/perform/resolve/and/or
    - begin/set/index/path/quasiquote/unquote/unquote-splicing serialization forms
    - fallback behavior unchanged when a tag is not specialized

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 87 - Split Escape-Promotion Dispatch/Memo Helpers

### Summary
Refactored `promote_to_escape(...)` by extracting tag-based dispatch and memoization helpers, preserving dual-lane promotion behavior while reducing control-flow density in the main function.

### What changed
- `src/lisp/eval_promotion_escape.c3`:
  - Added:
    - `promote_to_escape_by_tag(v, interp, ctx)`
    - `promote_to_escape_memo(old_v, new_v, interp, ctx)`
  - Refactored:
    - `promote_to_escape(...)` now delegates value-tag dispatch to `promote_to_escape_by_tag(...)`
    - promotion-context memo registration moved to `promote_to_escape_memo(...)`
  - Preserved:
    - all existing fast-path checks (`escape_generation`, current generation, target-scope-chain)
    - per-tag promotion behavior and shared-wrapper semantics
    - active promotion-context memo behavior for newly promoted values

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 86 - Split Env-Copy Wrapper/Core Helpers

### Summary
Refactored `copy_env_to_scope(...)` into a thin context-owning wrapper and a recursive core helper, plus extracted frame-allocation and binding-copy helpers, preserving env-copy and promotion behavior.

### What changed
- `src/lisp/eval_env_copy.c3`:
  - Added:
    - `copy_env_binding_value(val, interp, ctx)`
    - `copy_env_alloc_frame(src, interp)`
    - `copy_env_copy_bindings(src, dst, interp, ctx)`
    - `copy_env_to_scope_inner(env, interp, depth, active_ctx)`
  - Refactored:
    - `copy_env_to_scope(...)` now owns optional local promotion-context setup/teardown
    - recursive env walking now runs through `copy_env_to_scope_inner(...)`
  - Preserved:
    - recursion depth cap (`>= 256 → null`)
    - persistent-env parent fix-up behavior
    - closure/iterator/value copy semantics and scope destructor registration

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 85 - Split Quasiquote Template Token Handlers

### Summary
Refactored `Parser.parse_qq_template(...)` into per-token helper functions, preserving quasiquote parsing behavior while reducing branching and per-case boilerplate.

### What changed
- `src/lisp/parser_quasiquote_datum.c3`:
  - Added:
    - `Parser.parse_qq_backtick()`
    - `Parser.parse_qq_unquote()`
    - `Parser.parse_qq_unquote_splicing()`
    - `Parser.parse_qq_quote_datum()`
    - `Parser.parse_qq_int_literal()`
    - `Parser.parse_qq_var_symbol(sym)`
  - Refactored:
    - `Parser.parse_qq_template(...)` now dispatches to those helpers by token type
  - Preserved:
    - error text for unexpected quasiquote tokens
    - parse flow for nested quasiquote/unquote/unquote-splicing
    - symbol/underscore variable behavior and literal/int handling

### Verification
- `c3c build` passes.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passes:
  - Unified: 1143 passed, 0 failed
  - Compiler: 73 passed, 0 failed

## 2026-03-04: Session 84 - Split JSON Emit Conversion Helpers

### Summary
Refactored `omni_to_json_val(...)` by extracting symbol/list/array/hashmap conversion helpers, preserving JSON emission semantics while reducing branch complexity in the main conversion function.

### What changed
- `src/lisp/json.c3`:
  - Added:
    - `omni_symbol_to_json(sym, doc, interp)`
    - `omni_cons_list_to_json(list, doc, interp)`
    - `omni_array_to_json(arr_v, doc, interp)`
    - `omni_hash_key_to_json(key_v, doc, interp)`
    - `omni_hashmap_to_json(map_v, doc, interp)`
  - Refactored:
    - `omni_to_json_val(...)` now delegates `SYMBOL`, `CONS`, `ARRAY`, and `HASHMAP` cases to helpers above
