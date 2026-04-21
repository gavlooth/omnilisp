# Defect Remediation Plan (2026-04-09)

Status: executed on 2026-04-09 (historical remediation record)

## Objective

Correct the high-severity defects found in the 2026-04-09 audit while keeping
Omni runtime invariants intact:

- strict parser progress and explicit syntax errors,
- deterministic TEMP/ESCAPE ownership behavior,
- AOT/JIT semantic parity,
- deduce refresh/transaction correctness,
- async/scheduler resource cleanup symmetry.

## Scope

This plan covered defects reported in:

- parser/grammar lane,
- async/scheduler/IO lane,
- compiler/AOT lane,
- deduce lane,
- runtime lifetime lane,
- build/smoke validation lane.

Tracking IDs were recorded in `TODO.md` as `AUDIT-*` lanes and are now closed.

## Workstreams

### WS1: Parser Progress and Error Semantics

Defects:

- `...` can produce non-progress lexing/parsing.
- Unterminated nested block comments are accepted silently.
- Single-expression path does not enforce trailing-token exhaustion.
- Pika/core parser contract drift (`a[0]` vs `a.[0]`, UTF-8 symbol handling).

Primary files:

- `src/lisp/parser_lexer_token_scanners_dot.c3`
- `src/lisp/parser_lexer_symbol_number.c3`
- `src/lisp/parser_lexer_whitespace.c3`
- `src/lisp/parser_top_level_parse.c3`
- `src/lisp/eval_run_pipeline.c3`
- `src/pika/lisp_grammar_build.c3`
- `src/pika/lisp_grammar_scanners.c3`

Implementation notes:

- guarantee lexer forward progress for every token path,
- emit explicit lexer/parser error for unclosed block comments,
- enforce end-of-input in single-expression parse API (or split API into strict
  and permissive variants with explicit naming),
- normalize Pika scanner/grammar contracts to core parser surface.

Required tests:

- add regression for `...` no-hang behavior,
- add regression for unclosed `#| ...` error,
- add regression for trailing-token rejection in strict single-expr path,
- add Pika parity tests for postfix index contract and UTF-8 symbols.

### WS2: Async/Scheduler/IO Cleanup Symmetry

Defects:

- pending async/offload state can remain active when `prim_yield` errors,
- `write-file` heap leak on shared publication failure,
- `fs-close` invalidates handle before close success,
- possible input buffer teardown race.

Primary files:

- `src/lisp/async_tcp_transport_connect.c3`
- `src/lisp/async_process_signal_dns.c3`
- `src/lisp/async_udp_pipe.c3`
- `src/lisp/async_tcp_transport_listen.c3`
- `src/lisp/scheduler_tcp_async_bridge.c3`
- `src/lisp/prim_io_file_helpers.c3`
- `src/lisp/scheduler_primitives_offload.c3`
- `src/lisp/prim_io_file.c3`
- `src/lisp/prim_io_fs_stream.c3`
- `src/lisp/prim_io_helpers.c3`

Implementation notes:

- centralize `yield`-error teardown helper for all pending operation structs,
- ensure published/shared payload ownership is released on every early-return,
- mutate handle state only after successful close,
- keep input buffer lifecycle fully mutex-synchronized through teardown.

Required tests:

- targeted async error injection tests for pending slot cleanup,
- offload path test for `prim_yield` error cleanup,
- fd-close failure behavior test,
- concurrent input close/read stress test.

### WS3: AOT/JIT Semantic Parity

Defects:

- AOT multi-arg path drops args for non-variadic closures.
- Handle lowering truncates to 8 clauses while passing full count.
- AOT handle strict-mode semantics are dropped.
- AOT signal path bypasses normal effect-tag validation.
- Primitive apply path accepts arity-0 in single-arg apply branch.

Primary files:

- `src/lisp/aot_runtime_bridge_trampoline.c3`
- `src/lisp/compiler_native_effect_compilation_flat_style.c3`
- `src/lisp/compiler_native_effect_compilation_flat_style_helpers.c3`
- `src/lisp/aot_runtime_bridge.c3`
- `src/lisp/jit_runtime_effects_handle.c3`
- `src/lisp/jit_apply_helpers.c3`

Implementation notes:

- route non-variadic multi-arg AOT calls through shared checked apply path,
- enforce explicit hard cap or dynamic arrays for handle clause lowering (no
  partial initialization),
- thread strict-mode flag through compiled handle runtime API,
- reuse effect-tag symbol validation path used by non-AOT perform/handle flows,
- fix primitive arity predicate (`arity == 1 || arity == -1` only).

Required tests:

- AOT multi-arg closure parity tests,
- handle clause count > 8 behavior tests,
- strict unhandled effect behavior parity test (AOT vs JIT),
- invalid effect tag test through AOT path.

### WS4: Deduce Refresh and Transaction Correctness

Defects:

- recursive aggregate seminaive path commits internal txn from in-txn caller
  path,
- incremental refresh marks used when no copy happened,
- refresh payload reports stale-at-entry counts as refreshed counts,
- pre-commit estimate mutation can drift on commit failure.

Primary files:

- `src/lisp/deduce_rule_eval_fixpoint_component_eval.c3`
- `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates.c3`
- `src/lisp/deduce_schema_query_refresh_incremental_copy.c3`
- `src/lisp/deduce_schema_query_refresh_incremental_apply.c3`
- `src/lisp/deduce_schema_query_refresh_targeted.c3`
- `src/lisp/deduce_schema_query_refresh_db_wide.c3`

Implementation notes:

- ensure recursive aggregate evaluation honors caller transaction ownership
  contract (no hidden inner commit for in-txn paths),
- set `used_incremental` only when at least one tuple is actually copied,
- compute refreshed payload fields after refresh mutations are applied,
- move estimate updates after successful commit or add rollback restore.

Required tests:

- relation-scope refresh correctness with stale/ineligible source,
- ephemeral/transactional recursive aggregate non-persistence test,
- payload accuracy tests for refreshed vs remaining stale counts,
- commit-failure estimate consistency test.

### WS5: Runtime Lifetime Boundary Edge Cases

Defects:

- env-copy rollback path may destroy shared iterator-captured closure,
- coroutine wrapper copy nulls source in-place, risking alias inconsistency.

Primary files:

- `src/lisp/eval_env_copy_frame_helpers.c3`
- `src/lisp/eval_promotion_copy_route_helpers.c3`

Implementation notes:

- make rollback destructor decisions based on explicit ownership markers rather
  than shape-only heuristics,
- avoid mutating source wrapper state during copy unless alias uniqueness is
  proven or forbidden by explicit guard path.

Required tests:

- shared iterator closure rollback safety test,
- repeated-alias coroutine copy behavior test.

## Execution Order and Parallelism

1. Run WS1 first (parser progress blocker and syntax safety).
2. Run WS2 and WS3 in parallel (disjoint file ownership).
3. Run WS4 and WS5 in parallel after WS1 baseline is green.
4. Run validation gates and close items per-lane (do not hold all lanes open).

## Validation Strategy

Per lane:

- `c3c build`
- targeted test slice and new regression tests for touched behavior.

Cross-lane integration:

- `LD_LIBRARY_PATH=/usr/local/lib ./build/main` targeted slices relevant to
  touched lanes.
- heavy memory/stress/full gates through Docker-bounded scripts per repo policy.

## Closure Criteria

A lane can be closed only when:

- defect behavior is corrected,
- regression coverage exists,
- targeted validation is green,
- `TODO.md` lane entry is updated with shipped slice evidence.
