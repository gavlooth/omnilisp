# Active TODO

Last condensed: 2026-03-11

This file now tracks only active, actionable work.
Full completed history is archived at:
- `docs/plans/TODO_ARCHIVE_2026-03-11.md`

Current actionable count: 2

## Language Surface Consistency (2026-03-12)

- [x] Decide the public fate of compatibility/runtime factory aliases:
  `make-iterator`, lowercase `iterator`, lowercase `list`/`array`/`dict`/`time-point`, and similar surfaces still exist for compatibility; decide which remain public, which become deprecated, and which should disappear from docs/examples.
  decided:
  - canonical constructor surfaces for new docs/examples: `List`, `Array`, `Dictionary`, `Iterator`, `TimePoint`
  - retained public helper: `list` (idiomatic Lisp builder/conversion helper)
  - compatibility-only aliases (non-preferred): `array`, `dict`, `dictionary`, `iterator`, `time-point`, `make-iterator`
  notes: language/reference docs and primitive appendix now reflect this split explicitly; runtime alias behavior is preserved for compatibility.
- [x] Decide whether abstract/meta builtin type symbols remain type-only:
  `Any`, `Value`, `Number`, and `Collection` currently participate in type annotations and dispatch but do not have callable value-position constructor/coercion behavior.
  decided: keep them type-position-only; they remain valid annotation/dispatch surfaces, but they are not callable runtime constructors. `Value` stays reserved for value-literal dispatch inside `^(Value literal)` rather than `(Value ...)`. Added advanced regression coverage for both annotation acceptance and value-position rejection, and updated language/type-system docs.
- [x] Remove implicit dispatch-time coercion/widening and require exact-or-subtype matching:
  numeric widening like `Integer` satisfying `^Double` should be removed so dispatch no longer performs hidden conversions; cross-type conversion should happen only through explicit constructor/coercion calls such as `(Double 3)` or `(Integer 3.9)`.
- [x] Audit constructor/coercion semantics across all builtin callable type symbols:
  ensure every cross-type conversion is explicit and localized to constructors (`Integer`, `Double`, `String`, `Symbol`, `Boolean`, `Nil`, `Closure`, `List`, `Array`, `Dictionary`, `Set`, `Iterator`, `Coroutine`, `TimePoint`) rather than leaking through dispatch or other implicit compatibility rules.
  completed:
  - constructor-type annotation matching resolves aliases canonically (`Int`/`Bool`/`Dict` behave as shorthands for `Integer`/`Boolean`/`Dictionary` in constructor annotation checks),
  - advanced constructor/type-parametric conformance assertions remain aligned to canonical `type-of`/`type-args` names (`Integer`, `Boolean`),
  - callable constructor audit matrix now covers `List`/`Array`/`Dictionary`/`Set`/`Iterator`/`Coroutine`/`TimePoint` in advanced tests,
  - regression coverage now asserts no implicit dispatch conversion for non-matching collection types (`^Array`, `^Iterator`) unless explicit constructor conversion is used.
- [x] Decide whether the current `false`/`nil` collapse is a stable language rule:
  the new `Boolean`/`Bool` and `Nil` constructor contracts currently reflect the existing runtime truthiness model; if `Boolean` and `Nil` should become distinct runtime values later, constructor semantics and docs will need another pass.
  decided: keep the current collapse stable for now. `false` remains the canonical falsy alias of `nil` at value level, `Boolean`/`Bool` constructors normalize falsy inputs to `nil`, and `Nil` constructor keeps accepting both `nil` and `false`. `Void` remains distinct and truthy.
- [x] Migrate command-style no-result language surfaces from `Nil` to `Void`:
  now that `Void` is a real builtin singleton type/value, command-style success paths should stop overloading `nil`; start with obvious no-payload operations such as printing, sleeping, close/release paths, and similar effectful runtime commands.
  completed:
  - command-style runtime surfaces now use `Void` for successful completion paths (`print`/`println`/`newline`/`display`, `sleep`/`async-sleep`, socket/process/signal/tls/fs close-style commands, scheduler cancel forms, `write-file`, `unsafe-free!`, `run-fibers`, module/import/export command forms, deduce command mutations/txn-control),
  - `fiber-cancel` now aligns with other cancel surfaces and returns `Void` for valid ids even when cancellation is a no-op (already done/running),
  - query/absence surfaces remain on `nil` (`file-exists?` miss, lookup/ref misses, predicate misses), preserving the `Nil` vs `Void` semantic line.
- [x] Update command-style collection mutation tests to assert `Void` result tags for `array-set!`, `push!`, `set-add`, `set-remove`, `dict-set!`, and `remove!`.
- [x] Preserve `Nil` specifically for absence/falsey/query semantics while `Void` covers successful no-payload completion:
  document and enforce the semantic line so lookups/not-found/query-style APIs stay on `nil`, while successful command-style completion uses `Void`.
  completed:
  - `LANGUAGE_SPEC` now explicitly codifies absence-vs-command semantics and false/nil truthiness collapse as stable behavior,
  - type-system and primitive appendix docs now call out `Nil` vs `Void` and `false` alias behavior directly,
  - advanced regression coverage enforces `Void` on command-style mutations and `nil` on query misses (`has?`, `set-contains?`, missing `ref` paths, etc.).
- [x] Normalize mutating update surfaces around `Void` results:
  mutating operations should return `Void` on success rather than `nil` or ad hoc payloads unless they have a strong data-return reason.
  completed:
  - canonical mutators now return `Void` on success (`set!`, `array-set!`, `push!`, `dict-set!`, `remove!`, `set-add`, `set-remove`, deduce command mutators, close/release/cancel command mutators),
  - JIT + AOT surfaces are aligned for `set!` variable/dot-path/collection update behavior,
  - intentional exceptions with explicit data-return semantics are retained (`atomic-add!` returns prior value, `atomic-cas!` returns boolean-like result).
- [x] Replace type-specific update forms with generic multiple-dispatch `set!` where the operation is conceptually “update collection/path slot”:
  array/dictionary and similar mutation paths should converge on a generic dispatched `set!` surface rather than permanently fragmenting into type-specific mutation names; the canonical success result should still be `Void`.
  completed:
  - parser/runtime support canonical generic collection updates via `(set! collection key value)` for `Array` and `Dictionary` while preserving two-arg variable/dot-path mutation behavior,
  - compiler/JIT/AOT surfaces and regressions cover generic collection `set!` flows (`Array`/`Dictionary`) and `Void` result semantics,
  - `array-set!` and `dict-set!` remain compatibility aliases rather than primary language-facing mutation surfaces.
- [x] Finish the descriptive-name audit for remaining shorthand-heavy surfaces:
  `Ptr`, low-level namespace abbreviations like `fs-*`/`tcp-*`/`udp-*`/`tls-*`/`dns-*`, and any other public spellings that still prioritize brevity over clarity should be reviewed explicitly instead of drifting ad hoc.
  decided:
  - descriptive names are the canonical language-facing direction in docs/examples (`filesystem-*`, `transmission-control-*`, `user-datagram-*`, `domain-name-resolve`, `transport-layer-security-*`, `^Pointer`),
  - shorthand spellings remain accepted for compatibility and ergonomics (`fs-*`, `tcp-*`, `udp-*`, `dns-resolve`, `tls-*`, `^Ptr`),
  - compatibility shorthands stay documented, but are explicitly marked non-preferred for new public examples/spec text,
  - effect-tag internals remain on stable `io/*` operation symbols (`io/tcp-*`, `io/udp-*`, etc.) to preserve handler/runtime parity and existing diagnostic domains.
- [ ] Run a Docker-capped full constructor-surface conformance pass:
  validate the constructor/type-symbol behavior across the advanced/e2e gates inside the validation container, not just the host-safe build/basic-smoke path.
  progress:
  - Docker-capped advanced slice now passes clean (`1017 passed, 0 failed`) via:
    - `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
  - Host e2e parity now matches exactly (`build/e2e_diff.txt` empty) after fixing continuation resume primitive-return ABI handling in JIT apply; Docker-capped e2e re-run is still pending in this environment (no Docker in PATH).
- [x] Lock in the new direct AOT lowering for type-definition forms:
  keep `deftype`, `defabstract`, `defunion`, `defalias`, and `defeffect` on structured helper paths and prevent regressions back to `aot::eval_serialized_expr(...)`.
- [x] Add direct compiled-define regression coverage for each definition form individually:
  `deftype`, `defabstract`, `defunion`, `defalias`, `defeffect`, plus constructor/variant global sync expectations where applicable.
  completed: compiler regression group now asserts direct helper lowering for all five definition forms and absence of delegated eval fallback.
- [x] Extend direct typed-define parity coverage beyond single-method smoke cases:
  cover repeated method-table extension, fallback installation, captured globals, and direct AOT callable registration without delegated eval.
  completed: compiler regression group now asserts multi-method direct typed helper call counts, typed+untyped fallback mixes, captured-global behavior, and no delegated eval fallback.
- [x] Remove or explicitly quarantine `aot::eval_serialized_expr(...)` from normal compiler paths:
  it should not remain an accidental escape hatch for core language lowering; if it stays, it should be treated as legacy/debug-only infrastructure.
  completed: compiler type-form lowering no longer has a serialized-eval default path, compiler regressions assert no emitted `aot::eval_serialized_expr(...)` in normal lowering, and the AOT bridge is now marked legacy/debug-only via deprecation.
- [x] Keep compiled `explain 'dispatch` on the direct helper path:
  preserve current direct lowering for compiled dispatch explain output and add regression coverage for multi-arg + zero-arg call targets.
  completed: compiler regression group now asserts direct `aot::explain_dispatch(...)` lowering for baseline, multi-arg call targets, and zero-arg call targets with no serialized-eval fallback.
- [x] Expand compiled `explain 'effect` direct lowering coverage:
  cover both canonical special-form shapes and call-surface spellings (`perform`/`signal`, `resolve`) so parser shape changes do not silently fall back to generic primitive invocation.
  completed: compiler regressions now assert direct effect-helper lowering for `signal`, `perform` call-surface, and `resolve` explain targets, with no serialized-eval fallback emission.
- [x] Decide the long-term representation of AOT closures:
  today AOT closures still use primitive wrappers rather than first-class runtime `CLOSURE` values, which leaves `closure?`/`type-of` parity incomplete.
  decided: keep primitive wrapper representation for compiled AOT closures for now (`aot-closure`/`aot-closure-var`) but classify them semantically as `Closure` in `closure?`, `type-of`, and runtime type inference; parity e2e cases were added for closure factory + variadic lambda introspection.
- [x] Decide whether compiled module/import/export semantics are intentionally static:
  AOT still treats module surfaces differently from interpreter/JIT runtime handling; either align semantics or document compiled-module behavior as intentionally distinct.
  decided: keep intentionally static compiled semantics for now; compiler backend inlines module bodies and lowers `import`/`export-from` to command-style `Void` no-ops. `LANGUAGE_SPEC`/module reference docs now state this explicitly, and compiler regression coverage asserts the static lowering shape.
- [ ] Run a Docker-capped parity regression pass after the direct-lowering change:
  include compiler slice, focused explain/type-dispatch e2e checks, and at least one compiled-binary parity sample that exercises typed define + type forms together.
  progress:
  - Docker-capped compiler slice still passes with direct-lowering regressions (`101 passed, 0 failed`) via `scripts/run_validation_container.sh`.
  - Host parity compile+run now passes with zero output diff (`build/e2e_diff.txt` empty) after JIT continuation primitive-return fix; Docker-capped parity pass remains pending due unavailable Docker runtime in this session.

## Active Failure Recovery (2026-03-11)

- [x] Fix `advanced`: `non-tail recursion exceeds former 1024 eval cap` (interp + jit parity).
- [x] Fix `advanced`: `parser matrix accepts Value bool constructor`.
- [x] Fix `advanced`: `match Some` regression (`got 0`).
- [x] Fix `advanced`: `match nested Some` regression (`got 0`).
- [x] Fix `async`: `dns-resolve localhost (fiber)` (`got='?'`).
- [x] Fix `async`: `dns-resolve returns string (fiber)`.
- [x] Re-run Docker-capped `advanced` and `async` slices and record clean pass.

## Optimization Follow-Up Queue (2026-03-10)

### O1. Boundary Profiling and Evidence Capture

- [x] Run a Docker-capped profiling pass for boundary-heavy workloads and record the baseline counters/trace summary. (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)
- [x] Capture scope-chain scan pressure and hint-hit/miss ratios from the current boundary telemetry surface. (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)
- [x] Identify which return-path outcomes dominate in practice (`reused`, destination-built, direct-promoted, spliced, disallowed). (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)
- [x] Record the accepted regression envelope for boundary-heavy workloads in docs or scripts. (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)

### O2. Dispatch Hot-Path Allocation Reduction

- [x] Add focused micro-bench coverage for method dispatch and typed lambda call boundaries before and after the allocation changes. (`docs/plans/dispatch-hot-path-benchmark-baseline-2026-03-11.md`)

### O3. Structural Equality Workspace Reuse

- [x] Audit `src/lisp/eval_pattern_support.c3` deep equality workspace allocation (`stack`/`seen`) under nested list/array comparisons. (`docs/plans/equality-workspace-audit-2026-03-11.md`)
- [x] Introduce a bounded scratch or inline-first workspace strategy that preserves cycle safety and current semantics. (`src/lisp/eval_pattern_support.c3`)
- [x] Add a regression/benchmark slice for large nested equality comparisons so allocator churn is measurable. (`docs/plans/equality-nested-benchmark-baseline-2026-03-11.md`)

### O4. Scheduler and Offload Throughput

- [x] Add micro-bench coverage for scheduler + async I/O/offload interaction hotspots (`queue`, completion publish, TLS/http offload). (`docs/plans/scheduler-offload-hot-path-benchmark-baseline-2026-03-11.md`)
- [x] Audit per-request heap allocation in `src/lisp/scheduler_offload_worker.c3` and classify reusable worker-local buffers versus required owned outputs. (`docs/plans/scheduler-offload-allocation-audit-2026-03-11.md`)
- [x] Prototype a narrow pool/reuse strategy for offload request/completion scaffolding without changing boundary ownership semantics. (`docs/plans/scheduler-offload-queued-work-pool-prototype-2026-03-11.md`)
- [x] Validate that any pooling change preserves generation/task handoff correctness and offload failure cleanup. (`docs/plans/scheduler-offload-queued-work-pool-prototype-2026-03-11.md`)

### O5. Deduce Scan/Query Throughput

- [x] Add benchmarks for `deduce` scan/query/count paths at corpus sizes large enough to expose regression envelopes. (`docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`)
- [x] Measure row materialization cost in `relation_scan_range(...)`, including per-row hashmap creation and per-column symbol-key allocation. (`docs/plans/deduce-scan-range-materialization-cost-baseline-2026-03-11.md`)
- [x] Reduce avoidable scan-path allocation where semantics allow (for example cached relation key values or other stable row-shape helpers). (`docs/plans/deduce-scan-range-key-cache-optimization-2026-03-11.md`)
- [x] Evaluate whether `deduce-query` should stay as full-scan + callback filtering or gain a narrower optimization path for common predicates. (`docs/plans/deduce-query-optimization-evaluation-2026-03-11.md`)

### O6. Largest-First Runtime Modularization

- [x] Continue largest-first modularization for oversized runtime modules that still combine hot-path logic with diagnostics or explainability helpers. (`docs/plans/runtime-modularization-split-2026-03-11.md`)
- [x] Prioritize split candidates by size and hot-path relevance (`schema.c3`, `eval_dispatch_types.c3`, `scheduler_offload_worker.c3`). (`docs/plans/runtime-modularization-split-2026-03-11.md`)
- [x] Keep behavior unchanged while splitting, and pair each split slice with targeted validation for the touched subsystem. (`docs/plans/runtime-modularization-split-2026-03-11.md`)
