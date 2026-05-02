# TODO Index

`TODO.md` remains the canonical backlog entrypoint; read the indexed part files for the full live queue and recently closed history.

The historical content was split mechanically to keep the backlog index
readable. Content order is preserved in the part files. The hard 1000-line
split gate applies to code files only.

## Parts

- Part 01: [docs/todo_parts/todo_part_01.md](docs/todo_parts/todo_part_01.md) (902 lines)
- Part 02: [docs/todo_parts/todo_part_02.md](docs/todo_parts/todo_part_02.md) (675 lines)
- Part 03: [docs/todo_parts/todo_part_03.md](docs/todo_parts/todo_part_03.md) (322 lines)
- Part 04: [docs/todo_parts/todo_part_04.md](docs/todo_parts/todo_part_04.md) (432 lines)
- Part 05: [docs/todo_parts/todo_part_05.md](docs/todo_parts/todo_part_05.md) (361 lines)
- Part 06: [docs/todo_parts/todo_part_06.md](docs/todo_parts/todo_part_06.md) (317 lines)
- Part 07: [docs/todo_parts/todo_part_07.md](docs/todo_parts/todo_part_07.md) (318 lines)
- Part 08: [docs/todo_parts/todo_part_08.md](docs/todo_parts/todo_part_08.md) (319 lines)
- Part 09: [docs/todo_parts/todo_part_09.md](docs/todo_parts/todo_part_09.md) (315 lines)
- Part 10: [docs/todo_parts/todo_part_10.md](docs/todo_parts/todo_part_10.md) (328 lines)
- Part 11: [docs/todo_parts/todo_part_11.md](docs/todo_parts/todo_part_11.md) (318 lines)
- Part 12: [docs/todo_parts/todo_part_12.md](docs/todo_parts/todo_part_12.md) (337 lines)
- Part 13: [docs/todo_parts/todo_part_13.md](docs/todo_parts/todo_part_13.md) (325 lines)
- Part 14: [docs/todo_parts/todo_part_14.md](docs/todo_parts/todo_part_14.md) (1915 lines)
- Part 15: [docs/todo_parts/todo_part_15.md](docs/todo_parts/todo_part_15.md) (951 lines)
- Part 16: [docs/todo_parts/todo_part_16.md](docs/todo_parts/todo_part_16.md) (261 lines)
- Part 17: [docs/todo_parts/todo_part_17.md](docs/todo_parts/todo_part_17.md) (166 lines)
- Part 18: [docs/todo_parts/todo_part_18.md](docs/todo_parts/todo_part_18.md) (1483 lines)

## Live Queue

- Current actionable count: 11.
- `DOCS-README-001` in Part 18 is open. The root README needs a sectioned,
  tutorial-style rewrite that covers the whole language surface and
  cross-links the normative docs without becoming a second spec.
- `SYNTAX-TEMPLATE-001` through `SYNTAX-TEMPLATE-003` in Part 18 are open.
  The language surface needs a deliberate `#syntax` reader-template contract,
  parser support, and docs migration plan that keeps `quote` and hygienic
  macros intact.
- `RECURSION-004` through `RECURSION-009` and `RECURSION-POLICY-001` in Part 18
  are open. Capped recursion on user data (quasiquote, value graphs, env chains,
  parser nesting) is an anti-pattern. Each must be converted to explicit
  heap-allocated stacks, iterative loops, or two-pass dependency graphs.
  Plan: `docs/plans/eliminate-capped-recursion-anti-pattern-2026-05-01.md`.
- `RECURSION-001` is closed. JIT quasiquote `jit_qq_impl` now uses an explicit
  heap-allocated worklist; the 64-depth cap is removed. Regression test
  `quasiquote deep nesting 128 levels` passes.
- `RECURSION-002` is closed. AOT quasiquote `compile_qq_flat` now uses an
  explicit heap-allocated worklist; the 64-depth cap is removed. Compiler
  codegen tests now verify 128-deep nesting succeeds.
- `RECURSION-003` is closed. JIT splice item caps in `jit_qq_expand_elements`
  and `jit_qq_expand_call` are removed; fixed `Value*[64]` arrays are replaced
  with dynamically grown heap arrays. Regression test
  `quasiquote splice >64 items` passes.
- Live blocker queue closed by the 2026-04-30 AUDIT-252 M9 default-switch
  classification.
- `AUDIT-256-FFI-ASYNC-INVALID-ARG-ABI-TAG` in Part 18 is closed. Async FFI
  string-argument ownership and worker callback paths now validate raw
  argument ABI tags before enum conversion or native handoff, and malformed
  tags report typed errors instead of relying on C-side rejection.
- `AUDIT-257-FFI-ASYNC-VOID-RETURN-CONTRACT` in Part 18 is closed. Async FFI
  now accepts valid `^Void` returns with dummy native storage and materializes
  the language `Void` singleton through scheduler completion instead of
  rejecting the call as unsupported or mapping it to `nil`.
- `AUDIT-258-FFI-ASYNC-FLOAT32-RETURN-CONTRACT` in Part 18 is closed. Async
  FFI now preserves `^Float32` returns as Omni `Float32` values through
  scheduler completion instead of widening them to `Float64`.
- `AUDIT-259-FFI-ASYNC-BOOLEAN-RETURN-CONTRACT` in Part 18 is closed. Async
  FFI now preserves `^Boolean` returns as Omni Boolean singleton values through
  scheduler completion instead of widening them to Integer `0`/`1`.
- `AUDIT-260-FFI-STRUCT-RETURN-STORAGE` in Part 18 is closed. FFI `^Struct`
  returns now use pointer-shaped return storage before the established sync
  opaque-handle conversion or async pointer-like rejection boundary, instead of
  being rejected early as an unsupported ABI tag.
- `AUDIT-261-FFI-VOID-PARAMETER-DECLARATION` in Part 18 is closed. FFI
  `^Void` remains a return-only ABI annotation and is now rejected at
  interpreter/JIT and AOT declaration time when used as a parameter, instead of
  surviving until call packing.
- `AUDIT-262-FFI-CALLBACK-VOID-PARAMETER` in Part 18 is closed. FFI callback
  `Void` remains valid as a return type but is now rejected as a parameter type
  in both list/array and variadic callback forms.
- `AUDIT-255-FFI-INVALID-RETURN-ABI-TAG` in Part 18 is closed. FFI bound-call
  return ABI tags now fail closed before libffi preparation when malformed, and
  return-value conversion plus async FFI offload report typed errors instead of
  returning `nil`.
- `AUDIT-254-JIT-TAIL-CONSTRUCTOR-ESCAPE-OPCODE` in Part 18 is closed. The
  continuation-sensitivity scanners now explicitly classify inert atom forms
  (`E_LIT`, `E_VAR`, and `E_QUOTE`) as non-sensitive while preserving
  fail-closed unknown-tag behavior, restoring tail constructor ESCAPE lowering.
- `AUDIT-253-MACRO-HYGIENE-RECURSION-HARD-EXIT` in Part 18 is closed. The
  macro-hygiene non-tail recursion probe now uses depth `384` instead of the
  no-longer-portable `512`, and the exact subgroup plus full macro-hygiene
  filter complete with normal summaries.
- `AUDIT-252-M9-DEFAULT-SWITCH-RESIDUAL` in Part 18 is closed. The M9
  native/AOT literal-lowering, AOT helper lookup, match-guard null-result,
  serializer nil-fallback, type-metadata literal-tag, quasiquote fail-closed,
  match-pattern fail-closed, and FFI manifest invalid-tag sub-slices are
  closed. Generated-global collection now also fails closed for malformed
  expression/pattern tags and traverses inline-module `with` bodies. Lambda
  scanning and free-variable analysis now also fail closed for malformed
  expression/pattern tags; mutable-capture prescan does the same while
  preserving module traversal; quasiquote free-variable analysis also fails
  closed for malformed expression tags. AOT match binding, match guard
  scan/lowering, inline-module metadata collection, and AOT type metadata
  value-tag emission now reject malformed internal tags. Generated-global
  literal collection now also rejects malformed internal `ValueTag` values
  while preserving all valid non-container tags as no-op leaves. FFI preload and
  contract-manifest discovery now also reject malformed expression tags while
  preserving quasiquote templates as no-op forms. Runtime sequence-pattern
  matching also now rejects malformed rest-position tags, and runtime
  literal-dispatch matching now rejects malformed internal literal tags before
  fallback. Final current-source classification found the remaining
  compiler/AOT `default:` arms are explicit fail-closed diagnostics,
  parent-dispatched helper fallbacks, or benign format/classification defaults.
- `AUDIT-251-TELEMETRY-TEST-SNAPSHOT-READS` in Part 18 is closed. Remaining
  test/runtime telemetry evidence reads now use snapshot/load helpers, including
  the new fiber-temp global pool-count snapshot helper.
- `AUDIT-250-SCOPE-TELEMETRY-SATURATING-COUNTERS` in Part 18 is closed. Scope,
  transfer, and fiber-temp telemetry now use M46 saturating atomic add helpers,
  guarded CAS decrement, and focused overflow regression coverage.
- `VALIDATION-002-ALL-SLICE-BOUNDARY-JIT-BLOCKER` in Part 18 is closed.
  TLS-targeted, TLS-enabled async, and bounded TLS-enabled all-slice validation
  now pass without `OMNI_SKIP_TLS_INTEGRATION`.
- `AUDIT-247-PRIMITIVE-USER-DATA-COPY-ROLLBACK` in Part 18 is closed.
  Primitive copy, escape promotion, and root-store clone routes now prepare and
  destructor-register the destination wrapper before running `user_data_copy`,
  and null-`prim_val` primitive copies fail closed when destructor registration
  fails.
- `AUDIT-246-AOT-CLOSURE-PRIMITIVE-LIFETIME` in Part 18 is closed.
  Generated AOT closure primitives now use scoped primitive `user_data`
  copy/finalizer hooks, generated sidecar refcounts, and bounded retained-scope
  activation. Generated no-capture closures route through the generated-owned
  constructors; manual AOT closure helpers remain caller-owned and opaque across
  boundary copies.
- `AUDIT-245-AOT-CLOSURE-CAPTURE-ROOT-LIFETIME` in Part 18 is closed.
  Generated immutable AOT closure captures now retain the current temp scope
  chain when needed, preserve lexical capture identity instead of cloning, guard
  mixed-capture failure paths, and skip retention allocation for all-mutable
  captures.
- `AUDIT-244-AOT-MUTABLE-CELL-ROOT-LIFETIME` in Part 18 is closed.
  Root-owned AOT mutable cells now promote initial and assigned values through
  root-store promotion and generated code uses a checked set API.
- `AUDIT-242-AOT-MUTABLE-CAPTURE-BINDINGS` in Part 18 is closed. AOT mutable
  captured `let`/`set!` lowering now uses lexical mutable cells and per-capture
  lambda mutability instead of source-name keyed environment writes.
- `MEM-PROOF-001` in Part 18 is closed under
  `docs/plans/memory-model-proof-matrix-2026-04-26.md`. The slice added a
  memory ownership inventory guard and manifest covering memory-sensitive
  owning call sites, FFI wrapper families, dynamic FFI handle call sites, and
  tensor device finalizer authorities.
- `MEM-PROOF-002` in Part 18 is closed. ScopeRegion core proof now has
  documented evidence for TEMP/ESCAPE teardown, destructor-registration OOM,
  retain/release symmetry, owner-thread checks, invalid splice preconditions,
  and retired `scope_adopt` runtime paths.
- `MEM-PROOF-003` in Part 18 is closed. Heap-backed value constructors now
  include checked `FFI_HANDLE` destructor-registration rollback coverage in
  addition to the existing scalar/no-dtor and heap-backed constructor policy
  manifest.
- `MEM-PROOF-004` in Part 18 is closed. Env/closure lifetime proof now has
  checked closure-copy destructor-registration rollback, forced env/copy-parent
  closure dtor OOM tests, rejected-transplant compatibility retry for
  closure-backed iterator return boundaries, bounded memory/JIT smoke, graph
  audit, and Valgrind evidence.
- `MEM-PROOF-005` in Part 18 is closed. Boundary commit routes now have checked
  closure escape destructor-registration rollback, direct-promotion
  disallowance coverage, explicit positive route assertions, forced no-splice
  materialization evidence, benchmark-envelope route counters, bounded
  `memory-lifetime-smoke`, policy guards, and Valgrind evidence.
  `MEM-PROOF-006` is closed with stable escape/prepared graph/transplant proof
  plus stale-handle, mutation-drift, cyclic/shared graph, refcount-rejection,
  benchmark, and Valgrind evidence. `MEM-PROOF-007` is closed with collection
  copy/materialization, rollback, known-capacity constructor OOM, benchmark,
  and Valgrind evidence. `MEM-PROOF-008` is closed with CPU/native tensor
  constructor cleanup plus CUDA/Vulkan destructor-registration failure
  coverage and leak validation. `MEM-PROOF-009` is closed with async,
  scheduler, thread, and callback lifetime validation. `MEM-PROOF-010` is
  closed with the native wrapper-family metadata sweep, targeted Valgrind, and
  leak-free release validation.
- `MEM-MODEL-IMPROVE-002` in Part 18 is closed under
  `docs/plans/memory-model-improvement-plan-2026-04-25.md`.
  The slice added slow-path slack histograms, per-scope slow-allocation
  sequence telemetry, request/unused buckets, and source/site attribution.
  Direct ESCAPE and direct TEMP chunk-size policy attempts are invalidated by
  measured counters; broad TEMP large-slack reduction is also invalidated.
  The final attribution run shows all remaining ESCAPE no-follow-up sequences
  come from the synthetic direct allocator probe
  (`escape_slow_sequence_no_followup_source_direct_delta=256`), not runtime
  boundary/promotion sources, so no allocator policy change is justified by
  the current profile.
- `MEM-MODEL-IMPROVE-003` in Part 18 is closed. Shared Dictionary/Set
  known-entry capacity sizing now eliminates the benchmark hashmap/set growth
  counters (`hashmap_growth_delta=0`, `set_growth_delta=0`) while keeping
  checked constructors and insertion paths.
- `MEM-MODEL-IMPROVE-004` in Part 18 is closed. Boundary value policy coverage
  now has a manifest-backed guard wired into `check_boundary_change_policy.sh`
  so every `ValueTag` must declare ownership, edge, copy-route,
  materialization, graph-audit, destructor, native/FFI, and rollback policy.
- `MEM-MODEL-IMPROVE-005` in Part 18 is closed. `atomic-ref` is the first
  explicit FFI bridge keepalive family, FFI wrapper copy now fails closed for
  declared traversal/unsafe modes, and public FFI metadata validation is green.
- `MEM-MODEL-IMPROVE-006` in Part 18 is closed. Product-style Finwatch,
  closure-heavy iterator pipeline, tensor-metadata crossing, and nested-module
  return benchmark slices are landed and envelope-checked. The nested-module
  slice also fixed stable materialization fallback after transplant proof
  rejection for stable graph returns.
- `AUDIT-238-CONTINUATION-IGNORE-K-TEMP-EDGE` in Part 18 is closed; the
  ignore-k continuation retention failure is fixed and `memory-lifetime-smoke`
  is green again.
- `AUDIT-239-ENV-COPY-DTOR-REGISTRATION` in Part 18 is closed; env-copy frame
  destructor-registration OOM now fails closed with a typed boundary fault
  instead of returning an unmanaged copied frame.
- `AUDIT-240-DESTINATION-ERROR-ESCAPE-DTOR` in Part 18 is closed; destination
  error escape building now fails closed if ESCAPE destructor registration
  cannot be recorded after allocating the copied error string.
- `AUDIT-241-STABLE-MATERIALIZED-CLOSURE-DTOR` in Part 18 is closed; stable
  destination materialized closures now fail closed if their closure-specific
  env-scope destructor cannot be recorded.
- The `MEM-BENCH-OBSERVE-001` through `MEM-BENCH-OBSERVE-005` memory-boundary
  telemetry evidence lane in Part 18 is closed. The lane produced a signal
  inventory, runtime counter coverage, benchmark workload coverage, first
  counter baseline, and regression-envelope parser.
- The memory boundary architecture verification blockers from
  `docs/plans/memory-boundary-architecture-spec-2026-04-24.md` are closed in
  Part 18.
- The proof-driven memory-boundary optimizer roadmap in
  `docs/plans/memory-boundary-proof-planner-roadmap-2026-04-24.md` is closed in
  Part 18 through planner-owned commit migration, tag attribution, `CONS`
  copy-debt reduction, closure copy-debt reduction, array copy-debt reduction,
  closure residual classification, and BigInteger copy-debt reduction.
  Counters-enabled `memory-lifetime-smoke` now reports no
  optimizer-addressable stable-materialization copy bucket; the remaining
  `208` copied bytes are expected no-splice closure rollback coverage.
- No open stable-escape prepared-materialization rollout items remain in Part 16.
- Recent closed stable-escape rollout history in Part 16 includes prepared
  `CONS`/`ARRAY`/dictionary/set/closure graph metadata, cyclic container
  back-edges, prepared-publication fallback observability, mutation-drift
  invalidation, explicit commit-route flags, first TEMP cons stable
  materialization, prepared root/container materialization for arrays,
  dictionaries, sets, closures, and cloned heap-backed scalar/signature payloads.
- Vulkan/CUDA/ML audit residuals from
  `AUDIT_REPORT_VULKAN_CUDA_ML_2026-04-23.md` are closed in Part 17.
