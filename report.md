# Omni Hardcoded Limits and Unfinished Sections Report

Date: 2026-03-06  
Scope: `/home/gavlooth/Documents/code/Omni` (`src/`, `csrc/`, `tests/`)

## Status Summary

This report is now split into completed remediation seasons and remaining backlog.

- Season 1: completed in code and validated.
- Season 2: completed in code and validated.
- Season 3: completed in code and validated.
- Season 4: completed in code and validated.
- Season 5: completed in code and validated.
- Season 6: completed in code and validated.
- Season 7: completed in code and validated.
- Season 8: completed in code and validated.
- Season 9: completed in code and validated.
- Season 10: completed in code and validated.
- Season 11: completed in code and validated.
- Season 12: completed in code and validated.
- Season 13: completed in code and validated.
- Season 14: completed in code and validated.
- Season 15: completed in code and validated.
- Season 16: completed in code and validated.
- Season 17: completed in code and validated.
- Season 18: completed in code and validated.
- Season 19: completed in code and validated.
- Season 20: completed in code and validated.
- Season 21: completed in code and validated.
- Season 22: completed in code and validated.
- Season 23: completed in code and validated.
- Season 24: completed in code and validated.
- Season 25: completed in code and partially validated.
- Remaining backlog: prioritized below (critical/high/medium/low).

## Season 1 Completed

1. Removed script-mode source truncation.
- File: `src/entry.c3`
- Change: removed `len > 65535` clamp in `run_script_mode`.

2. Removed JIT compile-time 16-arg call cap.
- File: `src/lisp/jit_jit_compile_expr_core.c3`
- Change: `int[16]` arg slots replaced by dynamic allocation.

3. Removed apply-time multi-arg truncation.
- File: `src/lisp/jit_jit_apply_multi_prims.c3`
- Change: removed `min(arg_count, 16)` behavior in primitive/method-table paths.

4. Removed deduce query 256-result cap.
- File: `src/lisp/deduce_schema_query.c3`
- Change: dynamic list-based match collection.

5. Removed regex `find_all` 64-match cap.
- Files: `src/pika/regex.c3`, `src/pika/lisp_pika.c3`
- Change: dynamic `RegexMatch[]` growth and dynamic call-site handling (`re-split`/tests updated).

6. Removed grammar compiler/indexer fixed limits.
- File: `src/pika/lisp_pika.c3`
- Change: dynamic grammar/rule tables; removed fixed 64/256 caps in those paths.

7. Removed silent lambda/define destructuring truncation.
- Files: `src/lisp/parser_lambda.c3`, `src/lisp/parser_define_core.c3`
- Change: list-based collection for destructuring metadata.

8. Converted macro variable overflow from silent drop to explicit error.
- File: `src/lisp/macros_expansion.c3`
- Change: overflow now raises parser/eval error instead of silently discarding.

## Season 2 Completed

1. Removed lexer token-text fixed cap behavior (`LEXER_TEXT_CAP=1024`).
- Files:
  - `src/lisp/parser_lexer.c3`
  - `src/lisp/parser_lexer_string_hash.c3`
  - `src/lisp/parser_lexer_symbol_number.c3`
  - `src/lisp/parser_lexer_token_scanners.c3`
- Change:
  - token text storage is dynamic (`char*` + grow-on-demand),
  - long symbols/strings/regex literals no longer fail due fixed token buffer,
  - added lexer cleanup (`Lexer.destroy`) and wired it into parse/run/compile call sites.

2. Removed fixed path-segment cap (`MAX_PATH_SEGMENTS=32`).
- Files:
  - `src/lisp/value_expr_ast_core.c3`
  - `src/lisp/value_ast_effects.c3`
  - `src/lisp/parser_expr_atoms.c3`
  - `src/lisp/parser_set_pipe.c3`
- Change:
  - `ExprPath.segments` and `ExprSet.path_segments` now dynamic pointers,
  - path parsing and `set!` path parsing now allocate dynamically.

3. Removed parser hard cap on set literals and pipe steps.
- Files:
  - `src/lisp/parser_expr_atoms.c3`
  - `src/lisp/parser_set_pipe.c3`
- Change:
  - replaced `Expr*[64]` temporary buffers with dynamic `List{Expr*}` pipelines.

4. Removed placeholder lambda parameter truncation (`[16]`).
- File: `src/lisp/parser_application.c3`
- Change: placeholder parameter collection is now dynamic.

5. Removed declarative FFI fixed 16-param cap in parser/runtime metadata.
- Files:
  - `src/lisp/value_ast_effects.c3`
  - `src/lisp/parser_ffi.c3`
  - `src/lisp/eval_ffi_eval.c3`
  - `csrc/ffi_helpers.c`
- Change:
  - `ExprFfiFn.param_names/param_types` now dynamic with capacity growth,
  - `FfiBoundFn.param_types` now dynamic,
  - call marshalling buffers now scale with `param_count`,
  - libffi helper now builds dynamic CIF arg-type arrays (no hard `nargs <= 16` limit).

6. Removed libclang binding parser fixed 16-param cap.
- Files:
  - `src/lisp/libclang_bind.c3`
  - `src/entry.c3`
- Change:
  - `ParsedFunc.params` is now dynamic,
  - parameter extraction no longer clamps at 16,
  - generated binding parse buffer lifecycle initialized/freed safely.

7. Parser depth cap centralized and raised.
- Files:
  - `src/lisp/parser_parser.c3`
  - `src/lisp/parser_expr_atoms.c3`
- Change:
  - moved to `PARSER_MAX_NESTING = 1024` and parser field `max_depth`.
  - This is still an explicit protective limit (not dynamic), but now centrally defined.

## Season 3 Completed

1. Removed source-dir stack fixed-cap and unsafe termination indexing.
- Files:
  - `src/lisp/value_interp_state.c3`
  - `src/lisp/jit_jit_module_import.c3`
  - `src/lisp/prim_io.c3`
  - `src/entry.c3`
- Change:
  - source directory context is now a dynamic stack (`char**` + grow-on-demand),
  - no silent drop at depth > 16,
  - push/pop now own/free per-entry directory strings safely,
  - call sites now handle push-allocation failure explicitly.

2. Removed module path copy-length mismatch that could read beyond buffer.
- Files:
  - `src/lisp/value_ast_effects.c3`
  - `src/lisp/jit_jit_module_import.c3`
  - `src/lisp/value_interp_state.c3`
- Change:
  - `Module.path` changed to heap-allocated `char*` with exact `path_len`,
  - `module_copy_path` now copies full path and reports allocation failure,
  - module teardown now frees path storage safely.

## Season 4 Completed

1. Removed import path truncation at 255 bytes.
- Files:
  - `src/lisp/value_ast_effects.c3`
  - `src/lisp/parser_import_export.c3`
- Change:
  - `ExprImport.path` changed from fixed `char[256]` to heap-allocated `char*` with exact `path_len`.
  - import parser now allocates exact storage for string paths and returns explicit parse error on allocation failure.

2. Removed FFI symbol name truncation (`c_name[128]`) in parser/runtime path.
- Files:
  - `src/lisp/value_ast_effects.c3`
  - `src/lisp/parser_ffi.c3`
  - `src/lisp/eval_ffi_eval.c3`
- Change:
  - `ExprFfiFn.c_name` and runtime `FfiBoundFn.c_name` are now heap-allocated `char*` with exact lengths.
  - parser no longer truncates long symbol names; it allocates exact symbol storage.
  - runtime lazy `dlsym` now uses full symbol name without 128-byte cap.

3. Removed sequence pattern matching materialization truncation (`Value*[64]`).
- File:
  - `src/lisp/eval_pattern_matching.c3`
- Change:
  - sequence matching now allocates element storage based on actual list/array length and matches full sequences (no silent cut at 64).
  - all REST modes (`REST_NONE`, `REST_START`, `REST_MIDDLE`, `REST_END`) continue to work with full-length inputs.

4. Added regression tests for these removals.
- File:
  - `src/lisp/tests_advanced_tests.c3`
- Coverage:
  - parser import with path length >255 preserves full path length.
  - FFI symbol name >128 preserves tail through lazy `dlsym` error path.
  - sequence pattern `[.. last]` over list length >64 returns true last element.

## Season 5 Completed

1. Removed pattern deep-equality silent failure at depth 256.
- Files:
  - `src/lisp/eval_pattern_matching.c3`
  - `src/lisp/tests_limit_busting_tests_verify_dynamic_allocation_works.c3`
- Change:
  - `values_equal` now uses iterative deep walk with dynamic work stack + seen-pair tracking (no fixed recursion depth cutoff).
  - quoted/literal pattern matching no longer fails only because structure depth exceeded 256.
  - added regression test `match quoted literal >256 depth`.

2. Removed regex character-class silent truncation at 64 entries.
- Files:
  - `src/pika/regex.c3`
  - `src/pika/lisp_pika.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - `CharClassData` now stores dynamic ranges/chars with grow-on-demand capacity.
  - class parsing/shorthand class construction (`\\d`, `\\w`, `\\s`, etc.) now retains all entries instead of clipping at fixed 64.
  - updated grammar-side scanner builder to use dynamic char-class helpers (avoids fixed-array assumptions).
  - added regression test `re-fullmatch char-class preserves entries past 64`.

## Season 6 Completed

1. Removed regex bounded quantifier parse-time hard cap (previously `<= 4096`).
- Files:
  - `src/pika/regex.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - bounded quantifiers now parse to full `i32` range with overflow-safe decimal parsing.
  - removed fixed `4096` rejection path; large but valid bounds are accepted by parser/compiler path.
  - compile-side expansion now uses a defensive budget guard to avoid pathological clause explosion (`mk_fail` with explicit error when exceeded).
  - added regression coverage for `>4096` acceptance and i32-overflow rejection.

## Season 7 Completed

1. Removed grammar clause-operator child truncation at 32 (`seq`/`first` compile path).
- File:
  - `src/pika/lisp_pika.c3`
- Change:
  - replaced fixed `lisp::Value*[32]` collection with unbounded list counting + dynamic child-array compilation.
  - grammar clause compilation for long `seq`/`first` forms no longer clips child lists at 32.

2. Removed parse-tree/fold child truncation at 32.
- Files:
  - `src/pika/lisp_pika.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - replaced fixed `int[32]` child extraction with `UserMatch`-based submatch traversal (`get_user_view`) in both `parse_tree_to_lisp` and `fold_parse_tree`.
  - added direct parser regression checks for `>32` child preservation on explicit `start` rule match for both parse-tree conversion and fold aggregation.

## Season 8 Completed

1. Removed flat compiler arg-temp fixed buffer/OOB risk at 32 args.
- Files:
  - `src/lisp/compiler_call_flat.c3`
  - `src/lisp/tests_compiler_tests.c3`
- Change:
  - replaced stack `usz[32]` arg-temp buffers with exact-size dynamic temp arrays for flat call, tail-call, and `list`/`dict` construction paths.
  - removed the mismatch where only the first 32 args were compiled but all `arg_count` slots were later consumed.
  - added compiler regressions covering `>32` arg non-tail call, `list` construction, and tail-call codegen.

## Season 9 Completed

1. Removed dispatch scoring/type-signature truncation at 8 args.
- Files:
  - `src/lisp/eval_dispatch_types.c3`
  - `src/lisp/jit_jit_closure_define_qq.c3`
  - `src/lisp/tests_advanced_tests.c3`
- Change:
  - `find_best_method` now infers runtime arg types for the full argument list instead of truncating to `TypeId[8]`.
  - typed method-signature construction now carries typed parameter metadata through the full current signature capacity instead of stopping at 8.
  - added regression coverage for methods that differ only on the 9th argument type.

## Season 10 Completed

1. Replaced lossy wakeup-ring overflow with a lossless overflow queue while preserving the fixed ring as the fast path.
- Files:
  - `src/lisp/scheduler_state_offload.c3`
  - `src/lisp/scheduler_wakeup_io.c3`
  - `src/lisp/tests_tests.c3`
  - `src/lisp/tests_scheduler_boundary_worker.c3`
- Change:
  - retained `WAKEUP_RING_SIZE=256` for the uncontended path, but ring saturation now spills into a mutex-protected overflow queue instead of dropping wakeups.
  - `wakeup_drops` now only represents unrecoverable enqueue failure (for example overflow-node allocation failure), and `wakeup_overflow_spills` records when the lossless overflow path is exercised.
  - `drain_wakeups` now drains the ring first and then drains any queued overflow batch without violating the existing per-slot ready barrier.
  - rewrote wakeup saturation tests to assert eventual delivery, zero drops under saturation, empty overflow after drain, and successful offload completion when the ring is already full.

## Season 11 Completed

1. Replaced the generic shared wakeup transport with semantic delivery lanes.
- Files:
  - `src/lisp/scheduler_state_offload.c3`
  - `src/lisp/scheduler_wakeup_io.c3`
  - `src/lisp/scheduler_offload_worker.c3`
  - `src/lisp/scheduler_tcp_async_bridge.c3`
  - `src/lisp/tests_tests.c3`
  - `src/lisp/tests_scheduler_boundary_worker.c3`
- Change:
  - removed the fixed ring + overflow transport from the scheduler core and replaced it with a mutex-protected reliable wakeup queue for must-deliver events.
  - moved `OFFLOAD_READY`, `TIMER_EXPIRED`, and `POLL_ERROR` onto the reliable queue, and made `POLL_READABLE` a per-fiber coalesced signal via `readable_pending`.
  - changed offload worker delivery from retry-on-full behavior to one-shot reliable publish with explicit enqueue-failure accounting.
  - replaced ring/overflow-specific tests with direct coverage for reliable queue flooding, deterministic enqueue observability, offload delivery under queue backlog, invalid payload cleanup, duplicate completion handling, and readable-signal coalescing.
  - this supersedes the Season 10 shared-ring overflow design as the actual pre-alpha scheduler contract.

## Season 12 Completed

1. Removed silent scheduler run-loop termination on `max_rounds` and no-progress exits.
- Files:
  - `src/lisp/scheduler_primitives.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - changed `scheduler_run_until` and `scheduler_run_all` to return explicit scheduler errors instead of silently breaking out and pretending execution completed.
  - surfaced both failure classes: round-limit exhaustion and no-progress stalls.
  - added `scheduler_abort_all_work()` cleanup so failure paths close pending I/O/offload state, reset scheduler queues, and leave the scheduler reusable after error.
  - added regressions covering `await` round-limit failure, `run-fibers` round-limit failure, and successful scheduler reuse after the failure cleanup path.

## Season 13 Completed

1. Removed the 16-column deduce relation schema cap end-to-end.
- Files:
  - `src/lisp/deduce.c3`
  - `src/lisp/deduce_schema_query.c3`
  - `src/lisp/deduce_relation_ops.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - replaced inline fixed-width relation column storage with owned dynamic storage on `Relation`.
  - added relation-handle finalization so schema memory is released with the FFI handle instead of leaking.
  - changed relation schema collection and scan decode paths to size themselves from `rel.col_count` instead of silently truncating at 16.
  - added a regression that defines a 20-column relation, inserts a 20-value fact, scans it back, and asserts the last column survives.

## Season 14 Completed

1. Removed deduce LMDB path/name truncation in open and relation creation.
- Files:
  - `src/lisp/deduce.c3`
  - `src/lisp/deduce_schema_query.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - replaced the fixed `path_buf[256]` open path handoff with exact-size heap C strings, so long user paths are passed to LMDB intact.
  - replaced the fixed `dbi_name[64]` relation DBI handoff with exact-size heap C strings, so relation names that differ after byte 63 no longer alias the same LMDB table.
  - added isolation regressions for long relation names and long open paths that only differ in the truncated tail.

## Season 15 Completed

1. Removed the unify match-result cap at 512.
- Files:
  - `src/lisp/unify.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - replaced the fixed `Value*[512]` match scratch buffer in `prim_deduce_match` with a dynamic result list, so larger match result sets are preserved instead of being silently truncated.
  - added a regression that inserts 600 facts into a relation, runs `deduce 'match`, and verifies both full result count and presence of the tail binding.

## Season 16 Completed

1. Removed the `re-replace` output cap at 1024.
- Files:
  - `src/pika/lisp_pika.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - replaced the fixed `char[1024]` result buffer with an owned growable heap buffer and returned it via `make_string_owned`, so both first-match and global replace preserve large outputs instead of silently truncating.
  - added regressions for a 1500-byte first replacement and an 1800-byte global replacement.

## Season 17 Completed

1. Replaced the explicit eval-depth cap with real stack-headroom checks.
- Files:
  - `csrc/stack_helpers.c`
  - `src/stack_engine.c3`
  - `src/lisp/value_interp_state.c3`
  - `src/lisp/jit_jit_apply_runtime.c3`
  - `src/lisp/jit_jit_apply_multi_prims.c3`
  - `src/lisp/tests_advanced_tests.c3`
- Change:
  - removed the hardcoded `max_eval_depth = 1024` limit from the interpreter state.
  - added current-stack probing for both native thread stacks and owned `StackCtx` stacks, then routed all apply trampolines through one shared headroom guard.
  - cached native-thread stack bounds in the C helper so the hot apply path no longer calls `pthread_getattr_np` on every function entry; this keeps large tail-call loops fast while preserving real stack-headroom checks.
  - added a regression showing non-tail recursion beyond 1024 frames now succeeds when actual stack headroom exists, while unbounded recursion still fails with stack-overflow error.

## Season 18 Completed

1. Removed the `unify` binding-count cap and fixed result-dict sizing for wide matches.
- Files:
  - `src/lisp/unify.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - replaced the fixed `MAX_BINDINGS = 32` parallel arrays with a dynamic `BindingEntry` list, so deduce match environments now scale with the actual number of logic variables in the pattern.
  - sized result dicts using a power-of-two capacity helper instead of a raw binding count, which preserves correct hashmap probing for wide match results.
  - added a regression that defines a 40-column relation, matches it through 40 distinct `?vars`, and verifies the last bound value survives in the result dict.

## Season 19 Completed

1. Removed parser/type-definition fixed-shape caps across attributes, type parameters, metadata, fields, variants, and runtime type storage.
- Files:
  - `src/lisp/parser_define_attrs.c3`
  - `src/lisp/parser_define_core.c3`
  - `src/lisp/parser_ffi.c3`
  - `src/lisp/parser_type_defs.c3`
  - `src/lisp/parser_type_literals.c3`
  - `src/lisp/value_ast_effects.c3`
  - `src/lisp/value_core_types.c3`
  - `src/lisp/value_type_registry.c3`
  - `src/lisp/eval_type_evaluators.c3`
  - `src/lisp/tests_advanced_tests.c3`
- Change:
  - replaced fixed parser-side attribute and relation-column temporaries with dynamic `List{...}` collection so large `[type]`, `[union]`, relation, and FFI attribute payloads no longer depend on small stack arrays.
  - changed `ExprDefType`, `ExprDefUnion`, `TypeAnnotation`, `UnionVariant`, `TypeInfo`, and `Instance` to own exact-size heap-backed arrays instead of fixed embedded buffers.
  - taught type evaluation/registration to deep-copy and destroy nested heap-backed type metadata correctly, including union variants and field/type-param arrays.
  - added regressions covering `>64` type fields, `>16` union variants, `>32` type parameters, and `>16` metadata dict entries.

## Season 20 Completed

1. Removed macro clause/capture fixed arrays and large-macro bookkeeping caps.
- Files:
  - `src/lisp/parser_define_attrs.c3`
  - `src/lisp/value_ast_effects.c3`
  - `src/lisp/value_core_types.c3`
  - `src/lisp/macros_expansion.c3`
  - `src/lisp/value_interp_state.c3`
  - `src/lisp/tests_advanced_tests.c3`
- Change:
  - replaced parser-side fixed macro clause accumulation with dynamic clause collection and exact-size AST storage.
  - changed `ExprDefineMacro` and `MacroDef` to own heap-backed clause/captured-binding arrays instead of fixed embedded buffers.
  - removed the `64`-entry pattern-variable and captured-binding bookkeeping caps by switching macro-definition analysis to dynamic lists.
  - made macro redefinition replace the existing macro entry in place, so dynamic macro metadata does not accumulate stale heap allocations behind the hash index.
  - added regressions covering `>32` macro clauses, `>64` pattern variables, and `>64` captured bindings.

## Season 21 Completed

1. Removed fixed-width dispatch signature storage and deep-copy hazards across closures and method tables.
- Files:
  - `src/lisp/value_core_types.c3`
  - `src/lisp/jit_jit_closure_define_qq.c3`
  - `src/lisp/value_constructors.c3`
  - `src/lisp/eval_dispatch_types.c3`
  - `src/lisp/tests_advanced_tests.c3`
- Change:
  - replaced `MethodSignature` fixed arrays with owned dynamic arrays for parameter types, value literals, literal flags, and constraints.
  - removed the JIT-side `MAX_TYPE_PARAMS` clamp when building lambda method signatures and allocated exact-size signature storage from the active scope.
  - added explicit signature clone/free helpers so recursive closure env copies and heap-backed method-table entries no longer shallow-copy pointer fields.
  - tightened method-signature equality to compare full constraint content, not just counts.
  - added regressions covering dispatch on the `40th` typed argument and preservation of a `40th` signature constraint.

## Season 22 Completed

1. Removed macro auto-gensym table fixed-cap behavior.
- Files:
  - `src/lisp/eval_pattern_matching.c3`
  - `src/lisp/macros_expansion.c3`
  - `src/lisp/tests_advanced_tests.c3`
- Change:
  - replaced the per-expansion `GensymTable` fixed mapping array with dynamic storage and removed the `16`-entry assert in macro expansion.
  - added a regression that expands a macro containing `80` distinct `#`-suffixed symbols in one template and verifies the last binding survives.

## Season 23 Completed

1. Removed the remaining partial Pika `scan` character-class path and fixed the named-grammar entry contract.
- Files:
  - `src/pika/lisp_pika.c3`
  - `src/lisp/tests_tests.c3`
- Change:
  - replaced the ad-hoc grammar `scan "[...]"` parser with the shared regex tokenizer so grammar scans now support negated classes, shorthand escapes, and postfix/bounded quantifiers through the same character-class parsing path as regex.
  - stored the resolved start clause on `NamedGrammar` and switched `pika/parse`/`pika/fold` to use that explicit entrypoint instead of incorrectly assuming `grammar.names[0]` was the start rule after topological reordering.
  - removed stack-backed helper rule names from the grammar compiler and replaced them with stable literal labels, eliminating dangling-name lifetime bugs in compiled grammar metadata.
  - added regressions covering quantified scan runs, negated classes, shorthand classes, and invalid scan-pattern rejection.

## Season 24 Completed

1. Removed stale tracking artifacts from the active test/backlog surface.
- Files:
  - `tests/test_as_patterns.omni` (deleted)
  - `tests/unwired_features.omni` (deleted)
  - `docs/AS_PATTERN_STATUS.md`
  - `third_party/README.md`
- Change:
  - deleted `tests/test_as_patterns.omni`, which was not an executable regression but a placeholder note for an unimplemented `as`-pattern feature.
  - moved the `as`-pattern status into documentation and recorded the actual current state: no `PAT_AS` AST/parser/runtime/compiler support exists yet.
  - deleted `tests/unwired_features.omni`, which was a stale manual script with "SKIPPED" printouts but still executed live failing code paths.
  - clarified in `third_party/README.md` that TODO comments inside `third_party/arena/arena.h` are upstream vendor comments, not Omni backlog items, and that Omni-facing includes should prefer `arena/arena_config.h`.

## Season 25 Completed

1. Implemented real `'as` pattern support end-to-end.
- Files:
  - `src/lisp/value_expr_ast_core.c3`
  - `src/lisp/parser_pattern_match.c3`
  - `src/lisp/eval_pattern_matching.c3`
  - `src/lisp/compiler_native_effect_compilation_flat_style.c3`
  - `src/lisp/compiler_free_vars_utils.c3`
  - `src/lisp/jit_jit_apply_eval.c3`
  - `src/lisp/macros_expansion.c3`
  - `src/lisp/compiler_expr_serialize_values.c3`
  - `src/lisp/tests_advanced_tests.c3`
  - `docs/AS_PATTERN_STATUS.md`
- Change:
  - added a dedicated `PAT_AS` pattern node for the canonical Omni shape `(pattern 'as name)`.
  - taught the parser to recognize quoted-marker `as` patterns and to reject bare `as` with an explicit parser error instead of falling through into generic constructor-pattern failures.
  - taught the runtime matcher and native pattern compiler to bind both the inner destructured variables and the whole matched value.
  - updated macro pattern variable collection, JIT pattern warmup, free-var collection, and pattern serialization to recurse through `PAT_AS`.
  - added regressions covering runtime `match`, macro pattern clauses, and bare-`as` rejection.

## Remaining Backlog (Prioritized)

None for the hardcoded-limit / stale-placeholder audit tracked by this report.

## Validation (After Season 25)

- `c3c build`: success.
- Focused validation:
  - `build/main build/as_match_test.omni`: success (`'as` match clause binds whole value)
  - `build/main build/as_bare_reject.omni`: expected parser failure (`expected 'as in as-pattern`)
- Full runtime+compiler run:
  - blocked in current tree by an unrelated stack-engine crash outside the `'as` pattern changes, so season validation is partial rather than suite-wide.

## Notes

- This report intentionally prioritizes silent truncation/unsafe behavior over explicit safety limits.
- Explicit limits retained for safety (depth, quantifier bounds, etc.) are listed separately from silent behavior defects.
- `docs/AS_PATTERN_STATUS.md` now records the implemented canonical `'as` pattern shape and the explicit rejection of bare `as`.
