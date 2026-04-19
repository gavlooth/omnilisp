# Memory Changelog Index Part 17

Source: `memory/CHANGELOG.md`

## 2026-03-31

- Fixed the Finwatch normal boot path in two places: the background poller no
  longer blows the coroutine stack during startup because
  `examples/finwatch/server.omni` now hoists the poll-result store helpers out
  of `server-poll-once!`, and the script entrypoint
  `examples/finwatch/main.omni` now loads `server.omni` directly instead of
  relying on the top-level script-mode `import ... 'all` path that returned to
  the CLI instead of keeping the normal server boot resident. The HTTP runtime
  tests stub the feed fetchers and assert the real `server-poll-once!` helper
  can run inside a spawned fiber without overflowing.
- Hardened stack-guard setup and recovery so thread-affine stack pools keep
  guard bookkeeping and protected-switch recovery state thread-local, fail
  closed when `mprotect(...)` or guard registration cannot be installed, and no
  longer silently lose overflow protection after a fixed 256-stack cap.
- Fixed HTTP/HTTPS scheduler offload reads to accumulate full responses instead
  of silently truncating at 128 KiB, distinguish read failures from EOF, and
  publish owned response buffers only after complete read success.
- Cached the default BearSSL trust store in `csrc/tls_helpers.c` so repeated
  HTTPS offload and default TLS-connect paths stop reparsing the system CA
  bundle on every request.
- Preserved actionable loader and TLS failure causes in FFI/TLS user-facing
  errors so `dlopen`/`dlsym`, trust-store loading, socket read/write, and
  BearSSL engine/reset failures now retain the underlying OS/library/TLS cause
  instead of collapsing to generic text.
- Made top-level CLI mode dispatch command-local in `src/entry.c3`, so later
  script/submode arguments no longer hijack execution through global flag
  scanning.
- Made the FTXUI backend truthful about `handle_piped_input`: the runtime now
  disables it by default and the backend returns `not_supported` instead of
  silently claiming success.
- Hardened formatter depth handling so `omni --fmt` fails explicitly when the
  structural frame stack overflows instead of continuing with incomplete state.
- Normalized parser delimiter failures so raw bare `(` / `)` / `[` / `]` / `{`
  / `}` messages are translated into explicit expected-token diagnostics, and
  tightened `handle` parsing so empty or nested clause shapes fail as syntax
  errors before runtime allocation paths can misreport them.
- Reconciled shipped docs/introspection wording for deterministic memory,
  canonical `Dictionary`/`Dict` naming, `false` as a `nil` alias, canonical
  `signal` terminology, and `read-file`'s error behavior.
- Added per-argument primitive/type mismatch diagnostics so hot primitives now
  report the failing argument position and a rendered value shape when they
  reject non-matching inputs instead of collapsing to generic
  `expected number argument(s)` messages.
- Added path-specific file-read reporting for script and check entrypoints so
  missing-file, permission-denied, and invalid-path failures preserve the
  concrete path in both text and JSON surfaces while exposing a more specific
  IO error code.
- Closed four 2026-03-31 audit follow-up items by tightening three shipped
  surfaces: `tls-read` now rejects non-integer and non-positive `max-bytes`
  hints, `tls-server-wrap` no longer leaks its native handle when FFI handle
  boxing fails, `inflate` now rejects ignored extra args and invalid
  `original-size` hints instead of silently defaulting, and the AOT `--build`
  backend now excludes `src/lisp/tests*` while the tooling/docs describe the
  real temp-artifact and runtime-link contract.
- Reused the shared file-read classifier for text REPL preload and REPL-server
  preload/load-file paths so missing-file, permission, invalid-path, and
  generic read failures now keep their concrete IO code/reason instead of
  collapsing to `file not found` or a generic read error. Reconciled the REPL
  transport docs with the shipped per-clone `--project` preload behavior, and
  corrected stale runtime docs to describe dual-lane `TEMP`/`ESCAPE`
  ownership, value-typed `Dictionary` keys, and the live `Dictionary`/`Dict`
  collection surface instead of the older RC-centric wording.
- Hardened `--bind` header parsing so the CLI now fails closed instead of
  emitting partial FFI modules when the parser surface overruns local fixed
  limits:
  - `src/lisp/libclang_bind_parse.c3` now rejects overlong header paths before
    copying into the fixed 512-byte libclang path scratch buffer and reports
    when function discovery hits the caller-provided output cap,
  - `src/entry_bind_parse_helpers.c3` and
    `src/entry_bind_dep_generation.c3` now propagate those conditions as
    explicit dependency failures instead of silently truncating the path or
    generating a partial module from the first `256` parsed declarations,
  - `src/lisp/tests_compiler_codegen_groups_tail.c3` now covers both the
    overlong-path guard and the parse-capacity guard alongside the existing
    bindgen overflow regression.
- Tightened FFI binding type validation so unsupported signatures fail closed
  instead of silently degrading to pointer-shaped ABI metadata:
  - `src/lisp/libclang_bind_parse.c3` now aborts header parsing when parameter
    metadata allocation fails or when libclang surfaces unsupported by-value C
    types such as structs,
  - `src/entry_bind_parse_helpers.c3` and
    `src/entry_bind_dep_generation.c3` now surface those conditions as explicit
    `--bind` dependency failures instead of emitting malformed zero-argument or
    pointer-coerced bindings,
  - declarative `ffi λ` definitions now accept only canonical
    `^Integer`/`^Double`/`^String`/`^Pointer`/`^Boolean`/`^Void` annotations and
    raise a definition-time error for unsupported annotations instead of
    defaulting unknown types to raw-pointer ABI metadata,
  - added compiler/runtime regressions in
    `src/lisp/tests_compiler_codegen_groups_tail.c3` and
    `src/lisp/tests_advanced_io_effect_ffi_groups.c3` for allocation-failure,
    unsupported-C-type, and unsupported-annotation paths.
- Hardened `--init` scaffold failure handling so project creation now fails
  truthfully and rolls back partial trees instead of leaving misleading success
  state behind:
  - `src/entry_project_init_files.c3` now treats inner `mkdir(...)=EEXIST` as
    success only when the existing path is actually a directory, reports an
    explicit non-directory collision otherwise, and provides narrow test seams
    to force write-failure and collision paths,
  - `src/entry_project_init_bind.c3` now cleans up the freshly created project
    root on any later scaffold failure so mid-write errors do not leave a
    half-created tree behind,
  - added `tooling/tests/omni_init_smoke.py` to prove both rollback on
    `build/project.json` write failure and truthful failure on an inner
    non-directory collision.
- Hardened the single-worker REPL-server execution lane so live session state
  stays stable across clone growth and interrupts only target active work:
  - `src/lisp/eval_repl_server.c3` now rejects `clone` with
    `protocol/server-busy` before allocating a new session slot, so session
    array growth cannot `realloc(...)` away raw `ReplServerSession*` pointers
    still held by the worker for the active or pending command,
  - REPL-server `interrupt` now matches only the currently running request; a
    queued command is no longer acknowledged and then silently cleared before
    execution starts,
  - added `src/lisp/tests_runtime_async_repl_server_groups.c3` and wired it
    into the async slice, plus `tooling/tests/omni_repl_server_smoke.py` to
    prove `clone` is rejected while `(read-line)` is in flight and succeeds
    again after the request completes.
- Made structured REPL startup failures machine-readable and hardened shared CLI
  JSON escaping for raw argv/path bytes:
  - `src/entry_runtime_modes.c3` now reports `--repl --json` preflight and
    bootstrap failures as one JSON object on stdout instead of plaintext text,
  - `src/entry_repl_server_mode.c3` and `src/lisp/eval_repl_server.c3` now do
    the same for `--repl-server` usage, listen, accept, and TCP discovery-file
    startup failures with stable codes such as `cli/usage`,
    `io/listen-failed`, `io/accept-failed`, and `io/write-failed`,
  - `src/entry_check_reporting.c3` now escapes non-ASCII bytes as `\u00XX`
    so JSON output stays ASCII-safe even when argv/path bytes are not valid
    UTF-8,
  - added `tooling/tests/omni_cli_json_smoke.py` to cover raw-byte JSON
    escaping across `--check --json`, `--eval --json`, and
    `--describe --json`, plus REPL JSON preflight and REPL-server startup
    failures.
- Fixed primitive-name storage to use the full in-struct buffer width instead
  of an obsolete 31-byte cap, so generated/internal primitive names no longer
  emit spurious truncation warnings while still truncating safely at the real
  `Primitive.name` boundary.
- Aligned the AOT entrypoints so `--compile`, `--check`, and `--build` now fail
  before writing or handing off generated C3 when it still contains the
  internal `unsupported expr type` marker, instead of emitting partial C3 with
  a silent `aot::make_nil()` fallback; also removed the stale machine-local
  absolute `TODO.md` path from `docs/plans/README.md`, and made unsupported FFI
  lowering diagnostics refer to AOT lowering generically instead of naming only
  `--compile`; the AOT compiler now also passes its concrete lowering error back
  to the entrypoints so they stop printing a second generic wrapper on the same
  failure, and the old CLI-side scan for the emitted `unsupported expr type`
  marker has been retired in favor of that direct compiler error propagation;
  the compiler helper itself now stays silent unless a caller explicitly
  surfaces the returned lowering error. Compiler syntax failures also no longer
  report prelude-relative line numbers: the prepended stdlib now starts the user
  source on a fresh line and compiler syntax errors are translated back to
  source-relative line/column coordinates before the entrypoints print them.
  Dynamic compiler error strings are now copied out of the compiler frame and
  printed slice-safely by the AOT entrypoints, so malformed-source `--compile`
  and `--build` failures no longer collapse to a blank `Error:` line. AOT file
  loading also now uses the same path classification surface as `--check`, so
  missing-file `--compile` and `--build` failures preserve `file not found`
  instead of falling back to a generic read error. `--compile` output writes now
  also preserve concrete path/write causes such as `invalid path` instead of
  collapsing every output failure into a generic write error, and `--compile`
  no longer prints a misleading `Compiling ...` banner before failing an unreadable
  input path; obvious invalid or non-writable compile output targets are now
  also preflighted locally, so `--compile` fails before compilation starts on
  `invalid path` and `permission denied` output targets. AOT temp C3 staging now also preserves concrete write failures,
  so `--build` reports the actual `build/_aot_temp_...c3` path and reason
  instead of collapsing temp-file failures into a generic `cannot write unique
  AOT temp file in build/` message. `--build -o` also now preflights obviously
  invalid output targets, so missing-parent paths fail immediately as
  `cannot write output binary ...: invalid path` instead of spawning `c3c` and
  then wrapping the backend failure generically; existing-directory output
  targets now fail through that same local invalid-path surface too. Non-writable
  output parents are also preflighted locally now, so `--build -o blocked/outbin`
  fails as `permission denied` instead of reaching backend compile first.
  Backend handoff failures now also stop at the real backend stderr surface, so
  `--build` no longer prints a second generic `c3c compilation failed while
  compiling ...` wrapper after the backend has already emitted the concrete
  spawn or compile failure. Slash-qualified `C3C` overrides are now also
  preflighted as concrete backend compiler paths, so `C3C=/missing/c3c
  omni --build ...` fails early as `missing AOT backend compiler` instead of
  deferring to `/usr/bin/env` for the first actionable error.

## 2026-03-26

- Validated the scheduler offload reuse regression in
  `src/lisp/tests_scheduler_boundary_worker.c3`, proving recycled queued-work
  nodes are reset and the free-list bound stays intact across a reuse cycle.
- Hardened `src/lisp/deduce_relation_scan_helpers_more.c3` so scan-range row
  materialization fails through the native deduce OOM path when row-dict
  allocation is unavailable instead of dereferencing a null hashmap.
- Hardened method-table growth and first-define allocation in
  `src/lisp/jit_jit_define_method_table.c3` by checking `mem::malloc` before
  dereference and by propagating redefine/append failures instead of treating
  them as success.
- Validated the harness-only teardown regression lane with
  `OMNI_LISP_TEARDOWN_REGRESSION=1` and validated the isolated stdlib-loaded
  redefine/replacement regression with
  `OMNI_JIT_POLICY_STDLIB_TEARDOWN_REDEFINITION=1`.
- Made filtered test-group dispatch fail loudly when `OMNI_DEDUCE_GROUP_FILTER`
  or `OMNI_ADVANCED_GROUP_FILTER` matches no tests, so targeted validation no
  longer exits green on a mistyped filter.
- Guarded the queued-work allocation path in `src/lisp/scheduler_offload_worker.c3`
  so a null `mem::malloc` result returns failure instead of dereferencing the
  reset helper on an invalid buffer.
- Added explicit allocation checks to interpreter module/macro initialization in
  `src/lisp/value_interp_init_helpers.c3` so startup fails fast with a clear
  assertion instead of walking a null table during the fill loops.
- Hardened the environment hash-table rebuild and binding-growth path in
  `src/lisp/value_environment.c3` by keeping the old hash table live until a
  replacement is fully allocated and initialized, and by skipping binding
  growth when the expansion buffer allocation fails instead of dereferencing
  null.
- Shipped boundary regression wrapper to enforce the boundary fallback contract for
  return/boundary handoffs and prevent escaped-state regressions.
- Extended equality audit benchmarks to capture the new boundary wrapper behavior
  and include parity coverage for regression-sensitive cases.
- Added offload allocation counters on the boundary/offload path to track allocation
  pressure and publish the new counters in the shipped admin surface.
- Re-aligned deduce benchmark documentation with shipped behavior after the audit
  pass, including scope and truth labels for current benchmark contracts.
- Surfaced the boundary telemetry parser summary directly in
  `scripts/parse_boundary_profile_summary.sh` so the scope-chain pressure and
  dominant return-path outcomes appear in the regression output without digging
  through the nested JSON payload.
- Hardened the offload pool/reuse path by centralizing queued-work reset logic
  in `src/lisp/scheduler_offload_worker.c3` and strengthening the offload
  boundary worker retry coverage to assert pooled reuse is actually happening.
- Moved `prim_schema_explain(...)` into `src/lisp/schema_validation.c3` so the
  validation-facing entrypoints live together and `src/lisp/schema.c3` stays
  focused on the explain selector dispatcher.
- Split the dispatch/match diagnostics formatting helpers out of
  `src/lisp/eval_dispatch_types.c3` into the existing
  `src/lisp/eval_dispatch_match_errors.c3` module so the type registry and
  type-query surface stay focused in the original file.
- Extracted the `deduce_why_result_*` explainability/path-building block and
  `prim_deduce_why_result` out of `src/lisp/deduce_schema_query.c3` into a new
  `src/lisp/deduce_why_result.c3` sibling so the query/execution hot path can
  stay focused in the original file.
- Fixed worker-scratch component-pass recursive delta serialization by carrying
  same-pass scratch-visible additions through
  `src/lisp/deduce_rule_eval_exec_seminaive.c3`, so later recursive atoms in the
  component can observe earlier same-component additions before payload
  serialization.
- Revalidated the deduce worker-scratch wiring path (`c3c build` passes), but the
  bounded deduce lane currently aborts with `exit 139` before suite summary; the
  worker-scratch blocker remains open until that crash is resolved and the
  serialized recursive-delta test can be rechecked.
- Added a per-row materialization cost metric to the deduce scan/query/count
  benchmark summary and reconciled the dispatch hot-path benchmark plan with
  the actual in-tree benchmark file so the shipped coverage is documented at
  the right entrypoint.
- Expanded the equality inline-first workspace caps to keep the common nested
  comparison benchmark on-stack longer, and hoisted the deduce in-txn scan-row
  dict capacity computation out of the per-row loop to remove a small avoidable
  hot-path cost.

## 2026-03-25

- Closed Deduce TODO item `B6.9b2b2b2b2b2b2b2b2` with path-level mixed-context
  goal-directed provenance lists across support-frame relations:
  - proof paths now expose `goal-directed-read-contexts` when matching
    support-frame contexts come from different relations, instead of keeping
    the path-level surface blank or forcing a fake singular merge
  - same-relation support paths still use the singular
    `goal-directed-read-context` field
- Closed Deduce TODO item `B6.9b2b2b2b2b2b2b2b1` with common path-level
  goal-directed context propagation across same-relation support frames:
  - paths now inherit `goal-directed-read-context` when all matching
    support-frame contexts come from the same relation-local last-read state,
    including multi-frame fact support paths on the same relation
  - kept mixed-relation support paths unmerged and split the remaining
    provenance tail to that broader unresolved merge space
- Closed Deduce TODO item `B6.9b2b2b2b2b2b2b2a` with unique path-level
  goal-directed context propagation from matching support frames:
  - when the root tuple did not match but exactly one support frame in the
    chosen proof path carried bounded `goal-directed-read-context`, the path
    now inherits that same context
  - added regression coverage proving unique support-frame propagation
    succeeds while multi-frame fact support paths still stay unmerged
- Closed Deduce TODO item `B6.9b2b2b2b2b2b2b1` with bounded proof-path
  goal-directed context for matching fact support frames:
  - added a regression proving `deduce/why-result` attaches
    `goal-directed-read-context` to matching fact support frames inside a
    chosen derived proof path, not just the root path or derived
    `rule-step` frames
  - split the remaining provenance tail honestly to broader proof-path
    integration beyond the current root/fact-frame/rule-step row-matching
    surface
- Closed Deduce TODO item `B6.11b2` with explicit runtime/admin truth for
  the current parallel execution lane:
  - `deduce/analyze`, relation-local `deduce/stats`, and selector-scoped
    `deduce/explain` now expose `parallel-runtime-mode`
  - the shipped public value is `metadata-only`, which matches the real
    boundary: parallel topology and internal worker-scratch/apply seams
    exist, but the public execution engine is still not a parallel runtime
- Closed Deduce TODO item `B6.11b1b2b` with broader worker-scratch recursive
  closure for positive multi-atom SCC rule shapes:
  - worker-scratch closure no longer stops at the old
    `single-recursive-atom` guard
  - non-anchor recursive atoms now read from `LMDB + prior-iteration
    worker-visible additions`, while same-iteration scratch outputs remain
    invisible until the iteration closes
  - added a direct C-level Deduce regression proving a transitive
    multi-atom recursive SCC emits the full serialized fixpoint delta payload
- Closed Deduce TODO item `B6.9b2b2b2b2b2b2a` with truthful selector-scoped
  path-local parity across the currently shipped row-read shapes:
  - added a mixed-shape regression proving selector-scoped `match` and `scan`
    keep bounded path-local context on `no-op`, while selector-scoped
    `query` and `scan-range` keep bounded path-local context on their shipped
    ephemeral demand paths
  - removed the earlier misleading negative assumption that selector-scoped
    no-op row reads were categorically outside the bounded path-local surface
  - narrowed the remaining provenance backlog honestly back to broader
    proof-path integration beyond the current bounded root/support-frame row
    matching surface
- Closed Deduce TODO item `B6.9b2b2b2b2b2b1` with plain no-op `match`
  parity for bounded root-path goal-directed context:
  - the plain non-ephemeral `deduce/match` path now uses the txn-based scan
    helper when recording bounded subject keys, matching the shipped row-key
    capture behavior used by the goal-directed path
  - added a Deduce regression proving `why-result` attaches
    `goal-directed-read-context` on the matching root proof path after a
    plain no-op `deduce/match` read
  - narrowed the remaining provenance backlog honestly to broader
    proof-path integration beyond the current bounded root/support-frame row
    matching surface
- Closed Deduce TODO item `B6.9b2b2b2b2b2a` with the next truthful
  proof-path provenance slice:
  - plain no-op `deduce/query` and `deduce/scan-range` reads now preserve the
    bounded complete subject set for later `why-result` path matching instead
    of dropping it at the plain read boundary
  - `deduce/why-result` now also attaches path-local
    `goal-directed-read-context` on matching derived support frames, not only
    on the root proof path, when the support frame’s `(relation, tuple)` pair
    matches that bounded complete last-read subject set
  - added Deduce regressions for direct no-op `query` path-local attachment
    and derived support-frame attachment after a plain no-op `scan-range`
  - narrowed the remaining provenance backlog honestly to broader
    proof-path integration beyond the current bounded root/support-frame row
    matching surface
- Closed Deduce TODO item `B6.11b1c` with the main-thread publish/apply path
  for worker-computed recursive deltas:
  - `src/lisp/deduce_rule_eval_exec.c3` now validates and applies a
    serialized component-delta payload back through the existing relation
    integrity and LMDB write path on the main thread
  - added a direct C-level Deduce regression proving a worker-computed
    recursive closure payload can be deserialized, published, and observed as
    durable relation rows with updated schema estimates
  - narrowed the remaining parallel-runtime backlog honestly to broader
    worker-scratch closure (`B6.11b1b2b`) and runtime/admin truth (`B6.11b2`)
- Closed Deduce TODO item `B6.9b2b2b2b2b1` as a shipped selector-scan parity
  closure rather than leaving it implied:
  - selector-scoped `deduce/scan` was already participating in the bounded
    complete row-set path-local `goal-directed-read-context` surface through
    the shared capture path
  - added an explicit Deduce regression and updated the docs so `scan` no
    longer appears plain-only in the bounded-complete provenance slice
- Closed Deduce TODO item `B6.9b2b2b2b2a` with the first bounded multi-row
  proof-path goal-directed provenance slice:
  - replaced the old exact-one-only stored subject key with a bounded complete
    subject-set capture for the last goal-directed row read
  - `deduce/why-result` path-local `goal-directed-read-context` now attaches
    when the traced row belongs to that stored set and the full read stayed
    within the current `8`-row capture limit
  - this widened slice now applies to goal-directed `query`, `match`,
    `scan-range`, and plain `scan`; `query`/`match`/`scan-range` keep
    selector-scoped parity
  - when the last row-read matched more than the bounded capture limit, the
    top-level relation-read context remains available but path-local
    attachment is suppressed truthfully
  - added Deduce regressions for bounded-complete multi-row path attachment
    and overflow fallback
- Pruned the broad roadmap/milestone markers out of `TODO.md` so the live
  backlog keeps only concrete actionable items:
  - removed the `Deduce Product Roadmap Follow-up` section (`F0` through
    `V2-5`) from the active queue
  - kept the concrete open Deduce execution items plus the older runtime and
    validation follow-up items that are still directly actionable
  - corrected the live actionable count accordingly
- Closed Deduce TODO item `B6.11b1b2a` with the first multi-iteration
  worker-scratch recursive closure slice:
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3` now iterates the scratch
    seminaive path to a fixpoint for positive recursive components whose
    rules each have at most one positive recursive body atom
  - the closure path keeps a worker-local visible tuple set for duplicate
    suppression across iterations and returns the accumulated additions
    through the serialized component-delta payload format
  - added a direct C-level Deduce regression that proves the scratch closure
    computes a transitive `path` fixpoint payload containing `(1,2)`,
    `(2,3)`, and `(1,3)` without LMDB publish
  - split the remaining worker-closure lane honestly into
    `B6.11b1b2b` for broader multi-atom recursive shapes
- Closed Deduce TODO item `B6.11b1b1` with the first actual worker-scratch
  recursive compute slice:
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3` now exposes a read-only
    seminaive scratch-pass helper for positive non-aggregate recursive
    components
  - the helper seeds from the current component snapshot, evaluates one
    seminaive pass without LMDB publish, and returns serialized component
    deltas through the `B6.11b1a` payload format
  - added a direct C-level Deduce regression that proves a recursive rule can
    emit current-snapshot-derived tuples into that serialized payload
  - split the remaining worker-compute lane honestly into multi-iteration
    scratch closure under `B6.11b1b2`
- Closed Deduce TODO item `B6.11b1a` by shipping the first internal handoff
  seam for parallel recursive worker-scratch batches:
  - `src/lisp/deduce_rule_eval_exec.c3` now defines a versioned serialized
    component-delta payload for one SCC component's signed deltas
  - the payload carries component id, per-predicate identity, and opaque
    encoded tuple additions/removals through owned bytes compatible with the
    existing scheduler `OFFLOAD_RES_BYTES` result seam
  - added a direct C-level Deduce regression that roundtrips the payload and
    proves out-of-component predicates are excluded from the serialized batch
  - split the remaining runtime work honestly into `B6.11b1b`
    (worker-scratch compute) and `B6.11b1c` (main-thread publish/apply)
- Tightened the open `B6.11b1` lane to a concrete first runtime shape instead
  of leaving “parallel recursive execution” vague:
  - added `docs/plans/deduce-parallel-recursive-first-runtime-shape-decision-2026-03-25.md`
  - chose worker-local scratch computation plus main-thread publish as the
  first honest runtime widening
  - explicitly rejected direct worker-owned LMDB publish as the initial
    parallel path
- Closed Deduce TODO item `B6.11a2` by pinning the current fallback/admin
  truth for parallel recursion metadata:
  - selector-scoped `deduce/explain` now has explicit regression coverage
    proving that parallel batch metadata does not imply a separate runtime
    engine today
  - the current truthful surface keeps `execution-engine = 'semi-naive-scc`
    and `goal-directed-execution-path = 'selected-component-closure`
    alongside the parallel topology fields
- Closed Deduce TODO item `B6.11a1` by pinning the current parallel-recursion
  scheduling contract:
  - the parallel SCC metadata is now documented as a deterministic,
    metadata-only scheduling surface
  - recursive components batch only within the same stratum, grouped by the
    same computed `wave`
  - `wave = 1` means no same-stratum recursive dependency, and higher waves
    reflect the one-based longest same-stratum dependency distance from that
    boundary
  - added regression coverage that pins the current `stratum` / `wave` /
    `batch-size` topology on representative recursive SCC batches
- Closed Deduce TODO item `B6.10b2` by shipping dedicated admin counters and
  summary truth for `check` constraints:
  - relation-local `deduce/stats` now exposes
    `check-integrity-violation-count`
  - DB-wide `deduce/analyze` now exposes the same dedicated
    `check-integrity-violation-count` aggregate
  - the existing integrity-reporting regressions now pin `0` for non-check
    classes and `1` for check violations on the current `check` failure path
- Closed Deduce TODO item `B6.10b1` with the first shipped write-enforcement
  slice for canonical `check` constraints:
  - immediate `fact!`, derived rule-head publish, and deferred
    `write-deferred` commit-time snapshot validation now enforce declared
    unary column checks
  - failed checks raise deterministic machine-checkable payloads with
    `deduce/integrity-check-failed`, while missing, non-callable, and raised
    predicates reject with `deduce/check-predicate-missing`,
    `deduce/check-predicate-not-callable`, and
    `deduce/check-predicate-raised`
  - generic relation and DB integrity history now surface
    `violation-class = 'check` for those failures
  - the remaining `check` follow-up is narrowed to dedicated per-class admin
    counters and summary truth under `B6.10b2`
- Closed Deduce TODO item `B6.10a2` with the first shipped schema/admin
  baseline for canonical `check` constraints:
  - relation declarations now accept unary column checks in the form
    `(check predicate column)`
  - relation schemas persist declared `check` metadata, `deduce/schema`
    exposes `kind = 'check`, `predicate`, `columns`, and `enforced = false`,
    and `deduce/analyze` now reports DB-wide `check-constraint-count`
  - integrity totals now include declared `check` constraints on the admin
    surfaces without claiming write-side enforcement
  - added parser/storage and schema/analyze regression coverage for the new
    `check` payload surface
  - write-side `check` enforcement was the next follow-up slice and is now
    closed under `B6.10b1`
- Closed Deduce TODO item `B6.9b2b2b2b1` with exact-one goal-directed
  `deduce/scan` path context on matching `why-result` paths:
  - full-relation `deduce/scan` now captures the stored row key when the scan
    result has exactly one row
  - `deduce/why-result` now attaches path-local
    `goal-directed-read-context` for exact-one goal-directed `deduce/scan`
    subjects alongside the shipped `query`, `match`, and `scan-range` slices
  - that shipped `scan` slice is pinned for plain reads only; selector-scoped
    `scan` is still outside the current exact-one path-local slice
  - the remaining provenance item is now narrowed to `B6.9b2b2b2b2`
- Closed Deduce TODO item `B6.9b2b2b2a` by pinning selector-scoped parity for
  the shipped exact-one row-read path-local provenance slice:
  - exact-one path-local `goal-directed-read-context` now has explicit
    regression coverage for selector-scoped goal-directed `deduce/query`,
    `deduce/match`, and `deduce/scan-range`
  - selector-scoped payloads keep their concrete `selector-rule-index` on the
    matching proof path, while non-matching rows in the same relation keep
    that path-local field `nil`
  - the remaining provenance item is now narrowed to `B6.9b2b2b2b`
- Closed Deduce TODO item `B6.9b2b2b1` with exact-one goal-directed
  `deduce/scan-range` path context on matching `why-result` paths:
  - transactional goal-directed `deduce/scan-range` scans now capture the
    stored row key when exactly one tuple is returned
  - `deduce/why-result` now attaches path-local
    `goal-directed-read-context` for exact-one goal-directed
    `deduce/scan-range` subjects alongside the shipped `query` and `match`
    slices
  - the remaining provenance item is now narrowed to `B6.9b2b2b2`
- Closed Deduce TODO item `B6.9b2b2a` with exact-one goal-directed
  `deduce/match` path context on matching `why-result` paths:
  - transactional goal-directed `deduce/match` scans now capture the matched
    stored row key when exactly one tuple matches
  - `deduce/why-result` now attaches path-local
    `goal-directed-read-context` for exact-one goal-directed `deduce/match`
    subjects, just like the existing exact-one `query` slice
  - the remaining provenance item is now narrowed to `B6.9b2b2b`
- Closed Deduce TODO item `B6.9b2b1` with the first truthful
  proof-path-integrated goal-directed provenance slice:
  - `deduce/why-result` now attaches optional path-local
    `goal-directed-read-context` metadata when the relation's last
    goal-directed `deduce/query` observed exactly one row and that exact-one
    row matches the traced tuple
  - the broader provenance item is now narrowed to `B6.9b2b2`
  - relation-level top-level `goal-directed-read-context` remains unchanged
- Closed Deduce TODO item `B6.10a1` by fixing the next widened integrity
  class as canonical `check`:
  - the remaining backlog now names `check` constraints directly instead of
    the vague “widened integrity class” wording
  - `check` is the selected next integrity lane after the shipped
    `key` / `unique` / `ref` surface
  - rejected aliases for that lane are `assert`, `predicate`, and `guard`
- Closed Deduce TODO item `B6.4f5c2b2b2b` as stale backlog drift:
  - the transformed recursive query lane is now fully described by shipped
    support/fallback slices:
    same-index SCC support, one-carried-position transformed SCC relaxation,
    truthful permutation fallback, and truthful transformed multi-atom
    fallback
  - no separate transformed residual remained beyond those now-pinned
    boundaries
- Closed Deduce TODO item `B6.4f5c2b2b2a` by restoring truthful fallback for
  transformed recursive multi-atom `deduce/query` demands:
  - transformed recursive single-branch and disjunctive shapes whose
    uncarried demand is still distributed across multiple recursive body atoms
    now stay on `selected-component-closure` instead of reporting
    `ephemeral-head-demand-query` with incomplete rows
  - the remaining transformed recursive query residual stays open as
    `B6.4f5c2b2b2b`
- Closed Deduce TODO item `B6.4f5c2b2b1` by pinning multi-hop transformed SCC
  one-carried-position support:
  - transformed recursive pair and pair-disjunction demand relaxation now has
    explicit regression coverage for multi-hop SCC cycles, not just reordered
    two-relation mutual recursion
  - the remaining transformed recursive query residual stays open as
    `B6.4f5c2b2b2`
- Closed Deduce TODO item `B6.4f5c2b2a` by pinning transformed recursive
  disjunctive one-carried-position relaxation:
  - transformed reordered mutual-recursive pair disjunctions now have
    explicit regression coverage on `ephemeral-head-demand-query` with
    requested positions `(0 1)` and one applied carried position
  - pure recursive permutation disjunctions still stay on truthful fallback
  - the remaining transformed recursive query residual stays open as
    `B6.4f5c2b2b`
- Closed Deduce TODO item `B6.4f5c2b1` by widening recursive transformed
  `deduce/query` support to one-carried-position relaxation:
  - transformed recursive SCC pair demands may now stay on
    `ephemeral-head-demand-query` when the queried head predicate still has
    some same-index recursive carrier for one requested position
  - that shipped slice reports requested positions `(0 1)` with one applied
    carried position, while pure permutation shapes still fall back truthfully
  - the remaining transformed recursive query residual stays open as
    `B6.4f5c2b2`
- Closed Deduce TODO item `B6.4f5c2a` by pinning multi-hop same-index SCC
  `deduce/query` support:
  - same-index recursive pair demands now have explicit regression coverage
    for positive three-relation SCC cycles, not just self-recursive or
    two-relation mutual-recursive shapes
  - the remaining recursive query residual stays open as `B6.4f5c2b`
- Closed Deduce TODO item `B6.4f5c1` by pinning same-index mutual-recursive
  disjunctive `deduce/query` support:
  - same-index mutual-recursive SCC pair disjunctions now have explicit
    regression coverage on `ephemeral-head-demand-query` with requested and
    applied positions `(0 1)`
  - the remaining recursive query residual stays open as `B6.4f5c2`
- Closed Deduce TODO item `B6.4f5b` by widening recursive multi-position
  `deduce/query` support to same-index mutual-recursive SCC shapes:
  - fully-bound pair demands may now stay on `ephemeral-head-demand-query`
    not only for same-index self-recursive rules, but also for SCC-local
    mutual recursion where each recursive rule has some positive recursive
    body atom preserving all requested positions together at the same indices
  - permutation shapes and multi-atom distributed-demand shapes still fall
    back truthfully to `selected-component-closure`
  - the remaining recursive query residual stays open as `B6.4f5c`
- Closed Deduce TODO item `B6.9b2a` by exposing optional why-result
  goal-directed read context metadata:
  - `deduce/why-result` now adds top-level `goal-directed-read-context` when
    the relation has existing last goal-directed read state
  - that metadata mirrors the shipped `last-goal-directed-read-*` admin truth
    and may appear even when the row subject is still `missing`
  - proof-path-integrated goal-directed provenance now continues as the
    narrower residual `B6.9b2b2`
- Closed Deduce TODO item `B6.9b1b` by widening recursive `deduce/why-result`
  payloads beyond flattened fact-only closure support:
  - recursive closure paths now append a `rule-step` support frame for the
    derived child subject used in the chosen proof chain
  - recursive closure `max-depth` now reflects that deeper derived step,
    moving the first shipped recursive payloads beyond flat fact-only support
  - goal-directed provenance semantics remain open as `B6.9b2`
- Closed Deduce TODO item `B6.9b1a` by shipping the first positive recursive
  `deduce/why-result` closure lineage:
  - stored rows in positive recursive closure relations now compose support
    through the shipped row-subject provenance helper surface while guarding
    against revisiting the same `(predicate, tuple)` subject
  - one recursive support chain returns `status = ok`, and multiple recursive
    support chains return `status = partial` with the first deterministic path
  - broader recursive lineage beyond the current flattened fact-support surface
    remains open as `B6.9b1b`
- Closed Deduce TODO item `B6.9a8` by shipping the first multi-rule
  non-recursive `deduce/why-result` lineage:
  - stored rows in multi-rule non-recursive derived relations now aggregate
    support paths across the already-shipped non-recursive provenance helper
    surface
  - a single supported path across matching rules returns `status = ok`, and
    multiple supported paths return `status = partial` with the first
    deterministic path
