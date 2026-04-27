# Agent Plan Index

`.agents/PLAN.md` remains the operational plan entrypoint; read the indexed part files for the full plan state.

The historical content was split mechanically to keep individual files below the former 700-line repository limit. Content order is preserved in the part files. Current code-file split threshold is 1000 LOC from 2026-04-21 onward.

## Parts

- Part 01: [.agents/plan_parts/plan_part_01.md](plan_parts/plan_part_01.md) (341 lines)
- Part 02: [.agents/plan_parts/plan_part_02.md](plan_parts/plan_part_02.md) (629 lines)
- Part 03: [.agents/plan_parts/plan_part_03.md](plan_parts/plan_part_03.md) (353 lines)
- Part 04: [.agents/plan_parts/plan_part_04.md](plan_parts/plan_part_04.md) (395 lines)
- Part 05: [.agents/plan_parts/plan_part_05.md](plan_parts/plan_part_05.md) (758 lines)

## Current Checkpoint

Date: 2026-04-30

- Active hypothesis:
  - The canonical queue is `TODO.md`, and it reports
    `Current actionable count: 0`.
  - `AUDIT-251-TELEMETRY-TEST-SNAPSHOT-READS` is closed; M46/AUDIT-250 is closed.
  - `AUDIT_2.md` M30 is closed: `test_error()` now requires a non-empty
    non-allocation error payload, selected stable callers use
    `test_error_contains()`.
  - `AUDIT-253-MACRO-HYGIENE-RECURSION-HARD-EXIT` is closed: the
    macro-hygiene non-tail recursion headroom fixture now uses depth `384`
    instead of the no-longer-portable depth `512`, and exact/full
    macro-hygiene filters complete with normal summaries.
  - `AUDIT-254-JIT-TAIL-CONSTRUCTOR-ESCAPE-OPCODE` is closed. The JIT
    continuation-sensitivity scanners now preserve fail-closed unknown-tag
    behavior while explicitly classifying `E_LIT`, `E_VAR`, and `E_QUOTE` as
    inert, restoring tail constructor ESCAPE helper lowering.
  - The next `AUDIT-252` M9 residual is closed: generated-global literal
    collection now rejects malformed internal `ValueTag` values instead of
    treating them as no-op leaves, while valid non-container tags remain
    explicit no-op cases.
  - `AUDIT-252-M9-DEFAULT-SWITCH-RESIDUAL` is closed. Final current-source
    classification found the remaining compiler/AOT `default:` arms are
    explicit fail-closed diagnostics, parent-dispatched helper fallbacks, or
    benign format/classification defaults.
  - `AUDIT-255-FFI-INVALID-RETURN-ABI-TAG` is closed. Malformed internal FFI
    return ABI tags now fail closed before libffi preparation and return-value
    conversion plus async FFI offload report typed errors instead of `nil`.
  - `AUDIT-256-FFI-ASYNC-INVALID-ARG-ABI-TAG` is closed. Async FFI now
    validates raw argument ABI tags before string-ownership helper conversion
    and before worker callback/native handoff; malformed async call contexts
    fail closed with typed errors.
  - `AUDIT-257-FFI-ASYNC-VOID-RETURN-CONTRACT` is closed. Async FFI now uses
    valid dummy storage for `FFI_TYPE_VOID` returns and scheduler wakeup
    materializes `OFFLOAD_RES_VOID` as the language `Void` singleton rather
    than `nil`.
  - `AUDIT-258-FFI-ASYNC-FLOAT32-RETURN-CONTRACT` is closed. Async FFI now
    publishes `FFI_TYPE_FLOAT32` returns as `OFFLOAD_RES_FLOAT32`, and
    scheduler wakeup materializes them with `make_float32` instead of widening
    to Float64.
  - `AUDIT-259-FFI-ASYNC-BOOLEAN-RETURN-CONTRACT` is closed. Async FFI now
    publishes `FFI_TYPE_BOOL` returns as `OFFLOAD_RES_BOOL`, and scheduler
    wakeup materializes them as Omni Boolean singleton symbols instead of
    Integer `0`/`1`.
  - `AUDIT-260-FFI-STRUCT-RETURN-STORAGE` is closed. Sync and async FFI
    return-storage selection now treats `FFI_TYPE_STRUCT` as pointer-shaped,
    so sync reaches opaque-handle conversion and async reaches the pointer-like
    return rejection boundary.
  - `AUDIT-261-FFI-VOID-PARAMETER-DECLARATION` is closed. FFI `^Void` remains
    return-only: interpreter/JIT FFI declarations reject it as a parameter at
    definition time, and the AOT runtime bridge rejects raw `FFI_TYPE_VOID`
    parameter tags before publishing the primitive.
  - `AUDIT-262-FFI-CALLBACK-VOID-PARAMETER` is closed. FFI callbacks now reject
    `Void` parameter type metadata in both list/array and variadic forms,
    while preserving `Void` as a valid callback return type.
  - Repository status checks are green for validation, memory runtime,
    types/dispatch, and FFI foreign runtime after the bounded TLS-enabled
    all-slice pass closed `VALIDATION-002-ALL-SLICE-BOUNDARY-JIT-BLOCKER`.
  - The 2026-04-28 audit pass found and closed literal-singleton shorthand,
    non-constructor descriptor publication, inline-module helper binding,
    block-scoped effect declaration, and nested block type-form global sync
    regressions, plus tail-position block FFI binding, branch-local FFI binding,
    match-clause FFI binding, and generated FFI contract const-name
    regressions. The FFI direct-result binding patches were consolidated into
    the FFI helper lowering boundary so all generated-code paths share one
    binding rule.
  - The same audit pass also closed generated e2e fail-fast hardening and
    handler `resolve` terminal-control parity across JIT/AOT lowering. Terminal
    `resolve` guards are now distinct from ordinary `ERROR` propagation, and
    generated top-level `main` no longer receives invalid `return Value*`
    guard statements.
  - Follow-on multi-agent audit closed e2e Stage 2 diagnostic preservation,
    removed dead skip-count reporting, replaced duplicate noncanonical
    generated e2e shorthand rows with real `Dict` shorthand coverage, and
    aligned generated inline modules with interpreter module isolation through
    a temporary AOT runtime module env plus `defer` restoration.
  - The next multi-agent audit closed e2e policy enforcement gaps, Stage 4
    e2e diagnostic preservation, inline module duplicate-name fail-closed
    behavior, module-body `with`/`import` private-local discovery, imported
    module-private backing sync, and lexical `[effect]` AOT leakage by
    failing closed only for unsupported lexical effect publication.
  - The follow-on audit closed quiet-mode generated e2e count preservation,
    narrowed stale skip-count policy checks, inline `export-from` re-export
    metadata, unsupported runtime export enumeration fail-closed behavior,
    literal-dead inline module branch parity, and remaining lexical effect
    publication leaks in inline modules, `shift`, match guards, and handler
    clause bodies.
  - The next audit closed boolean-name primitive-shadow AOT divergence,
    inline-module lambda-body export metadata leakage, match-guard helper
    private-backing discovery, nested executable inline-module `export-from`
    source registration, conditional export fail-closed behavior, and
    reset/handle module-export fail-closed behavior.
  - The follow-on multi-agent audit closed generated C declaration-scope
    regressions by backing `let` locals with generated names, restoring
    declaration state across branch/match scopes, and preserving nested closure
    captures of primitive-shadowed names.
  - The next audit closed AOT module-binding versus lexical-alias precedence:
    module-local `define`/`set!`/type exports now beat older outer lexical
    aliases, inner lexical aliases inside `with` beat imported module bindings,
    and outer lexical aliases restore after `with`. Generated e2e now enforces
    423 rows with these module-collision cases.
  - The follow-on audit closed
    `AUDIT-242-AOT-MUTABLE-CAPTURE-BINDINGS`: mutable captured `let`/`set!`
    lowering now uses lexical `AotMutableCell` aliases and per-capture lambda
    mutability rather than source-name keyed environment writes.
  - The current audit closed two AUDIT-243 AOT mutable-capture residuals:
    recursive mutable self-capture now allocates the mutable cell before
    initializer lowering, and `set!` inside module/`with` scope now mirrors
    read-side module precedence instead of writing an older outer mutable cell.
  - `AUDIT-244-AOT-MUTABLE-CELL-ROOT-LIFETIME` is closed: root-owned
    `AotMutableCell.value` constructor seeding and updates now promote retained
    values through root-store promotion, generated mutable-capture lowering uses
    checked cell APIs, and compiler plus bounded memory-lifetime validation
    passed.
  - `AUDIT-245-AOT-CLOSURE-CAPTURE-ROOT-LIFETIME` is closed: generated
    immutable AOT closure captures now retain temp source scopes through
    closure-data teardown without clone-on-capture semantics drift; mixed
    capture failure paths are guarded; all-mutable captured closures skip
    retention allocation.
  - `AUDIT-246-AOT-CLOSURE-PRIMITIVE-LIFETIME` is closed: the AUDIT-245
    retention model no longer depends on root-owned generated closure primitive
    teardown.
    The first AUDIT-246 sub-slice landed: capture-retention graph-audit
    infrastructure failures now fail closed instead of silently retaining.
    The second sub-slice landed: generated captured closures call explicit
    generated-owned constructor APIs instead of lower-level constructors with a
    trailing ownership boolean.
    The final sub-slice landed: generated AOT closure primitives use scoped
    primitive `user_data` copy/finalizer hooks, generated sidecar refcounts, and
    bounded retained-scope activation; manual AOT closures remain caller-owned
    and opaque across boundary copies.
  - `AUDIT-247-PRIMITIVE-USER-DATA-COPY-ROLLBACK` is closed: primitive
    parent-boundary copy, escape promotion, and root-store clone now prepare and
    destructor-register destination wrappers before invoking `user_data_copy`,
    and null-`prim_val` copy branches fail closed on destructor-registration
    failure.
  - The current audit closed aggregate wrapper destructor-registration
    fail-closed gaps across parent copy, ESCAPE promotion, and root-store clone,
    including array, hashmap/set, method-table, and continuation routes.
  - The same audit closed two lexical-shadowing regressions: AOT `set!` now
    gives the innermost lexical alias priority over older mutable-cell aliases,
    and JIT mutability scans account for same-name `let` initializers being
    evaluated in the enclosing scope.
  - Generated e2e now enforces 431 rows, including same-name `set!`
    shadowing, same-name initializer mutation, checkpoint/capture mutable
    capture regressions, and continuation snapshot regressions where a
    `capture` body mutates a boxed cell before invoking `k`.
  - `AUDIT-248-TENSOR-DEVICE-DTOR-REGISTRATION` is closed: tensor result
    constructors under `src/lisp/prim_tensor*.c3` and
    `src/lisp/value_tensor*.c3` plus ML tensor result constructors under
    `src/lisp/prim_ml_*.c3` now use checked destructor registration, and the
    status guard rejects raw tensor/ML result `scope_register_dtor` calls.
  - Follow-up tensor hardening added tensor-specific boundary registration
    helpers so copied lazy tensor expression edges are cleaned before the
    copied tensor payload is destroyed on destination dtor-registration OOM.
  - Follow-up AOT mutable-cell snapshot replay ownership hardening made the
    registry thread-local, added an active reset snapshot frame, and replays
    that frame before terminal continuation resume without freeing the
    frame-owned snapshot; `compiled_reset` remains the sole owner/free path.
  - Follow-up fail-closed lifecycle hardening makes value/tensor destructor
    registration helpers clean already-materialized values even when called
    with a null scope, and a memory-lifetime regression covers the shared
    null-scope cleanup contract.
  - The latest follow-up closed the escaped generated-AOT multi-shot
    continuation snapshot regression: captured continuations now own a
    mutable-cell snapshot taken under the active compiled reset, replay it only
    after stack clone success, and release it through continuation teardown.
    `compiled_reset` restores/frees the grown active frame pointer/count, active
    frame append rolls back partial nested-frame entries, and AOT snapshot hooks
    install only after successful bootstrap.
  - Lifecycle cleanup coverage now also covers owned AOT closure data on
    destructor-registration failure and FFI handle constructor raw-payload
    release when the target scope is null.
  - The 2026-04-29 structural audit closed the oversized C3 file-size gate by
    splitting compiler, memory-lifetime, and JIT test/source files into
    same-module helpers without intentional behavior changes. File-size gate,
    C3 diagnostics, host build, focused compiler/JIT/memory slices, and bounded
    TLS-enabled all-slice validation are green.
  - The 2026-04-29 quiet-output follow-up closed the remaining compiler bindgen
    diagnostic leak under `OMNI_TEST_QUIET`; normal non-quiet bindgen
    diagnostics remain intact.
  - The 2026-04-29 manifest/facade audit closed an AOT runtime manifest source
    parity gap introduced by the compiler module/TCO helper split and restored
    public stdlib facades for the batch scheduler fast-path effects.
    The sibling libuv and primitive-doc parity guards now also read the split
    raw primitive registration inventory. Generated e2e, scheduler, I/O
    facade, libuv surface, primitive-doc parity, I/O status-map, file-size,
    status, and JIT env/scope guards are green at this checkpoint.
  - The 2026-04-29 boundary inventory audit closed stale `MEM-PROOF-010`
    manifest coverage for FFI test payload families and the dynamic
    foreign-runtime buffer wrapper. Boundary hardening now handles unsupported
    ASAN by requiring normal stack/scope/compiler evidence plus bounded
    Valgrind `memory-lifetime-smoke`; the gate is green with that fallback.
  - The 2026-04-29 boundary telemetry/profile audit closed stale validation
    tooling regressions: the default telemetry baseline artifact matches the
    current expanded `memory-lifetime-bench` workload, boundary profile
    regression now runs the lisp test suite and keeps stdout/stderr records
    parseable, and boundary decision thresholds understand Valgrind fallback
    logs.
  - The 2026-04-29 tooling audit closed the Tree-sitter npm-script regression:
    grammar parse/test/query commands now use the pinned `tree-sitter-cli`
    through `npx`, so they pass without a globally installed `tree-sitter`
    binary.
  - Follow-up tooling/status audits closed root and non-archive markdown link
    validation, stale workspace-path guards, Tree-sitter generated-parser split
    enforcement, splitter malformed-wrapper and zero-output fail-closed
    behavior, top-level script executable-mode enforcement, and first-party
    shell-script, Python, JSON, and TOML syntax enforcement. The same tooling
    audit also redacted a checked-in Codegraph TOML API key and added a TOML
    API-key regression guard. The only non-executable top-level shell helper
    under `scripts/` is
    `scripts/c3c_limits.sh`, which is sourced rather than run directly.
  - The 2026-04-29 ignored-policy-script audit repaired boundary guard
    discoverability: `scripts/check_boundary_value_policy_coverage.py` is now
    explicitly unignored/tracked, `.gitignore` also unignores the paired memory
    ownership inventory guard, and `scripts/check_status_consistency.sh`
    verifies every `scripts/boundary_sensitive_files.txt` entry exists and is
    tracked.
  - The 2026-04-29 Bash strict-mode audit tightened `scripts/run_e2e.sh` to
    `set -euo pipefail` and made `scripts/check_status_consistency.sh` enforce
    the same strict-mode header for first-party Bash scripts under `scripts/`,
    with `scripts/c3c_limits.sh` preserved as the sourced-helper exception.
  - The 2026-04-29 dependency Bash strict-mode audit tightened
    `deps/build_static.sh` to `set -euo pipefail` and expanded the status guard
    to the full first-party Bash syntax surface (`scripts`, `deps`, and
    `tooling`, excluding vendored dependency trees and Tree-sitter node
    modules), with `scripts/c3c_limits.sh` still the sourced-helper exception.
  - The 2026-04-29 tracked script executable-mode audit fixed tracked mode for
    `scripts/check_boundary_profile_thresholds.sh`,
    `scripts/parse_boundary_profile_summary.sh`, and
    `scripts/split_tree_sitter_parser.sh`, and made status consistency verify
    tracked executable state via jj in jj workspaces or Git in Git-only
    checkouts.
  - The 2026-04-29 interpreter-specific shell syntax audit made
    `scripts/check_status_consistency.sh` parse Bash shebang scripts with
    `bash -n` and POSIX `sh` shebang scripts with `sh -n`.
  - The 2026-04-29 POSIX shell strict-mode audit made
    `scripts/check_status_consistency.sh` require `set -eu` for first-party
    POSIX `sh` shebang scripts.
  - The 2026-04-29 native tensor audit closed `AUDIT_2.md` M50 and M51:
    BLAS/LAPACK dynamic resolver publication now uses `pthread_once`, publishes
    handles only after accepted function pointers are assigned, and uses relaxed
    atomics for shared counters and LAPACK test-disable flags.
  - The 2026-04-29 Vulkan native audit closed `AUDIT_2.md` M52: Vulkan dynamic
    resolver publication and the adjacent availability/feature probe now use
    `pthread_once`, and stale exported attempted/cache declarations were
    removed from the Vulkan runtime declarations header.
  - The 2026-04-29 stack native audit closed `AUDIT_2.md` M49: SIGSEGV handler
    recovery depth/count now use signal-safe scalar publication, guard entries
    are published as address ranges before count updates, and stale `AUDIT_2.md`
    M53 was invalidated because current libuv helpers already chunk bounded
    `uv_buf_init` calls.
  - The 2026-04-29 test harness audit closed `AUDIT_2.md` M55 and M58:
    `setup()` now aborts after setup-failure diagnostics, and the legacy
    `test_eq_double()` wrapper delegates to the existing tolerance-aware
    `test_double()` path. `AUDIT_2.md` M54 was invalidated because
    `scripts/c3c_limits.sh` is a sourced helper that inherits caller strict
    mode and is already an explicit status-guard exception.
  - The 2026-04-29 IO/e2e audit invalidated stale `AUDIT_2.md` M56 and M57:
    `scripts/check_io_parity_status_map.sh` exists and passes under the IO
    parity workflow contract, and generated-e2e failures now propagate to
    nonzero `--gen-e2e` status with runner-enforced 431-row corpus checks.
  - The 2026-04-29 eval/type audit invalidated stale `AUDIT_2.md` M59 and M60:
    JIT compile-failure paths reach `exprs.free()` before returning, and
    no-argument `defeffect` explicitly publishes `field_count = 0`.
  - The 2026-04-29 JIT method-table audit closed `AUDIT_2.md` M61-M63:
    typed method-table implementations now require a valid closure payload and
    non-null type signature through shared helpers, and `jit_eval_define()`
    fails closed before routing or name-tagging malformed closure values.
  - The 2026-04-29 dispatch/JIT audit closed `AUDIT_2.md` M64-M66:
    ambiguous dispatch candidate scratch now uses native index storage until
    the ambiguity payload is required, and JIT expression-family / env-copy
    fault switches have deterministic default fallbacks.
  - The 2026-04-29 fast-dev generator visibility audit unignored/tracked
    `tools/fast-dev/generate_fast_dev_project.py`, fixed its `is-up-to-date`
    helper-archive argument mapping, and made status consistency include `tools`
    Python plus a tracked-generator guard for `scripts/build_fast_dev.sh`.
  - The 2026-04-29 fast-dev freshness predicate hardening made
    `generate_fast_dev_project.py is-up-to-date` return quiet status `1` for
    missing manifest/dependency inputs instead of raising Python tracebacks.
  - The 2026-04-29 fast-dev generator CLI/link audit made malformed
    `generate`, `profile`, and `is-up-to-date` calls return concise usage with
    status `2`, and the generated fast-dev project now preserves linked-library
    order while removing duplicate entries such as `omni_chelpers`.
  - The 2026-04-29 migration helper CLI audit made the path-taking
    `scripts/migrate_*.py` helpers fail closed with usage and status `2` on
    malformed invocation, explicitly unignored them as first-party scripts,
    added a shared `scripts/migrate_cli.py` argument helper, and made status
    consistency reject raw direct `sys.argv[N]` indexing in first-party Python.
  - The 2026-04-29 Python smoke visibility audit explicitly unignored/tracked
    `scripts/dialectic_mcp_single.py` and `tooling/tests/*.py`, added a status
    guard for those entrypoints/tests, restored ASCII-safe high-byte escaping
    for CLI/REPL JSON strings, fixed REPL-server active-worker busy detection,
    and refreshed the init smoke for the single-directory-name `--init`
    contract.
- Current approach:
  - Treat `.agents/PLAN.md` and the part files as historical handoff context,
    not as a parallel backlog.
  - With TODO actionable count `0`, start future audit work from a fresh
    source/status scan rather than reopening closed residual umbrellas.
  - For native dynamic library resolution, prefer synchronized one-time
    publication over unsynchronized attempted flags.
  - For FFI ABI role validation, reject return-only metadata at declaration
    boundaries rather than allowing delayed call-packing failures.
  - For callback C `void` parameter lists, use an empty callback parameter
    list, not a value-bearing `Void` parameter.
- Validation path:
  - `scripts/check_status_consistency.sh` is the planning/status consistency
    gate.
  - Use targeted tests plus bounded container validation for future runtime,
    memory, FFI, CUDA/Vulkan, or high-memory work.
- Next checkpoint:
  - Continue from `TODO.md` count `0`; choose the next audit target from a
    fresh status/TODO scan rather than reopening the all-slice validation
    blocker, quiet-output bindgen diagnostics, the oversized-file gate, the AOT
    manifest source parity gap, the I/O batch facade mismatch, the boundary
    ownership inventory classifications, the boundary telemetry/profile
    baseline drift, Tree-sitter global-CLI/splitter defects, stale docs links,
    stale workspace paths, script executable-mode drift including tracked mode,
    first-party Bash strict-mode drift across the shell-syntax guard surface,
    first-party POSIX shell strict-mode drift, first-party shell syntax
    interpreter drift, first-party Python, JSON, TOML syntax drift, or
    fast-dev generator visibility/up-to-date argument or missing-input drift, or
    checked-in TOML API-key drift closed on
    2026-04-29. Do not reopen the ignored-boundary-policy-script drift closed
    on 2026-04-29 unless the boundary-sensitive manifest has an untracked or
    missing path again.
  - No live TODO-backed blocker remains. The next checkpoint is a fresh audit
    scan that opens a new concrete TODO only if it finds a current,
    reproducible defect or regression.
- Negative-memory constraints:
  - Do not treat historical plan text, roadmap notes, or old `.agents` entries
    as active work unless `TODO.md` explicitly reopens them.
  - Do not reuse the failed first BLAS/LAPACK stress harness variant as product
    evidence; it used stale ABI prototypes and crashed before exercising the
    intended resolver contract.
  - Do not treat the Vulkan resolver attempted flag as the whole M52 boundary;
    the adjacent availability/feature probe had the same racy one-shot cache
    pattern and must stay under synchronized publication.
  - Do not treat the current `advanced-effect-continuation` failure as M49
    evidence; its failing cases are IO/effect checks, not stack guard
    signal-state publication.
  - Do not reopen Python/Julia adapters or polyglot/plugin runtime support for
    the FFI foreign-runtime area without a new product decision.
  - Do not reopen allocator policy tuning from old aggregate slack counters;
    require a non-synthetic bounded benchmark signal first.
  - Do not use a combined terminal/error sequence-stop predicate as the shared
    contract for generated parent-work guards. `resolve_terminal_pending` is
    abortive control; ordinary `ERROR` values remain first-class data in
    argument/binding positions where existing semantics allow them.
  - Do not rely on inline module private C backings alone for helper-published
    declaration isolation; helper side effects must execute under a temporary
    module env so unexported names do not leak into the AOT global env.
  - Do not skip generated-global collection for entire lexical subtrees as a
    fix for effect leakage; that regresses legal FFI/type helper publication.
    Continue collecting legal helpers and fail closed only for lexical
    `[effect]` declarations until lexical helper envs are implemented.
  - Do not treat runtime module export enumeration as available during AOT
    inline-module metadata collection. Runtime `export-from` and module-body
    `import 'all` must fail closed until a real compile-time export inventory
    exists.
  - Do not use `declared_vars` as a lexical C-scope authority for primitive
    shadowing; it persists beyond branch/match blocks. Use the active
    primitive-shadow stack for generated locals/parameters that shadow
    primitive names.
  - Do not let synthetic reset/handle wrapper bodies publish inline module
    exports unless those wrappers carry the module-private backing context.
  - Do not use source symbol names as generated C locals for lexical `let`
    bindings. C3 rejects same-name shadow declarations even in nested blocks;
    `let` bodies must use generated backing aliases for lexical references and
    assignments.
  - Do not repair mutable captured AOT lets by adding more source-name
    membership checks around `mutable_captures`; source `SymbolId` is not a
    lexical binding identity and preserves same-name clobber/leak behavior.
  - Do not assume direct generated-main AOT scope behavior applies to AOT
    closures invoked from JIT/interpreter child scopes.
  - Do not blindly promote every immutable AOT closure capture at capture
    emission time; the broad clone-on-capture attempt regressed existing
    compiler capture behavior for shadowed primitive names and match guards.
  - Do not treat same-name `let` bodies and initializers as the same lexical
    scope for mutability scans. Initializers run before the new binding exists;
    bodies are shadowed by the new binding.
  - Do not replay continuation-owned AOT mutable-cell snapshots before stack
    clone succeeds; clone/OOM failure must not mutate the live AOT registry.
  - Do not use `eval_init_primitives.c3` alone as the authoritative raw
    primitive registration inventory. Runtime raw primitive registration is
    split into `eval_init_primitive_tables.c3`, so policy guards must read both
    surfaces.
  - Do not use full `--test-suite all` as the Valgrind fallback for boundary
    hardening on this stack-engine surface. Use normal stack/scope/compiler
    summaries plus bounded Valgrind `memory-lifetime-smoke` when ASAN is
    unavailable.
  - Do not use the pre-workload-expansion March/April boundary telemetry
    baselines as current threshold authority. The accepted profile now includes
    nested-module stable-materialization copy debt and Valgrind fallback
    decision counters, so gates should cap and monitor the current profile.
  - Do not assume contributor tooling has a globally installed `tree-sitter`
    binary. Use the pinned `npx --yes tree-sitter-cli@0.25.10` path or the npm
    scripts under `tooling/tree-sitter-omni`.
  - Do not treat raw `tree-sitter-cli generate` output as a repo-ready artifact.
    Use `npm run generate` so the checked-in parser remains split below the
    code-file size gate, and keep splitter failure paths preserving existing
    parts until a complete replacement exists.
  - Do not make `scripts/c3c_limits.sh` executable. It is the sourced helper
    exception; top-level shell runners under `scripts/` should remain directly
    executable.
  - Do not add `set -euo pipefail` inside `scripts/c3c_limits.sh` as an M54
    fix. The file is sourced by strict-mode callers, and setting shell options
    inside the helper would mutate caller policy rather than repair a
    standalone gate.
  - Do not reuse the old `advanced-collections-module` Vulkan softmax failure
    as evidence of a runtime Vulkan defect. It was a test contract bug that
    used unsupported 3-argument `+`; the assertion now uses nested binary
    addition and the broad advanced collections slice passes.
  - Do not recreate `scripts/check_io_parity_status_map.sh`; M56 is stale
    because the script exists and passes. Keep the workflow running the IO
    boundary facade, parity status-map, and async fallback guards together.
  - Do not treat generated-e2e errors as silently skipped without checking the
    current `--gen-e2e` exit contract. M57 is stale because generation failures
    return `false`, become exit `1`, and are backed by exact 431-row runner and
    baseline-policy checks.
  - Do not reopen M59 without a new direct-return path before `exprs.free()`;
    current program and single-expression JIT failure paths clean up the parsed
    expression list before returning.
  - Do not reopen M60 without a current code diff showing
    `eval_defeffect()` no longer sets `field_count = 0` in the no-argument
    branch.
  - Do not harden only the signature-specific method-table helpers if the
    closure-payload contract regresses; `jit_eval_define()` must also fail
    closed before typed routing or closure-name tagging when a value is tagged
    `CLOSURE` but has no payload.
  - Do not model M64 as an individually freeable list leak. Dispatch scratch
    values are region-owned; avoid scan-time Lisp scratch or isolate it in a
    releasable scope.
  - Do not use the current targeted Valgrind type-dispatch run as M64 evidence;
    it is blocked by unrelated type-error formatting and `prim_format`
    diagnostics.
  - The previous `advanced-collections-module` softmax blocker is closed as a
    test assertion bug, not a module/runtime Vulkan blocker. Future failures in
    that group should be triaged from fresh output rather than the stale
    3-argument `+` diagnosis.
  - Do not use a blanket all-tracked-JSON syntax guard. Some tracked
    `.claude/runs/*.json` operational transcripts are empty historical artifacts;
    first-party JSON status checks should exclude operational, vendored,
    generated, and third-party trees.
  - Do not store non-empty API keys in tracked TOML files. Keep local credentials
    in environment/user-local config and rotate any credential that was already
    committed.
  - Do not assume a boundary-sensitive policy script is covered by validation
    just because another script references it. The boundary-sensitive manifest
    itself must list only existing tracked paths, and status consistency now
    enforces that.
  - Do not use `deps/build_static.sh --help` as a harmless probe. The script has
    no help path and runs the full static dependency build.
  - Do not use `[ffi lib]` as an LSP JSON lowering-failure fixture. Declarative
    FFI library forms are now supported by AOT lowering; use the current
    unsupported declarative variadic FFI contract when testing
    `compiler/lowering-error` JSON reporting.
  - Do not use a fixed sleep as the readiness contract for the one-shot TLS
    server harness. The server must publish an explicit readiness file after
    `tcp-listen`, all TLS validation clients must wait for that signal without
    consuming the only accepted connection, and compiled TLS integration cases
    must unlink readiness files before the post-run `/tmp/omni-tls-ready-*.txt`
    cleanup check.
  - Do not assume `--help` is a harmless probe for validation gate scripts.
    Help and malformed-option handling must run before validation lock
    acquisition, build setup, Docker/container execution, or any expensive
    gate command.
  - Do not eagerly import executable Python submodules from package `__init__`
    files when those submodules are expected to support `python -m`; keep
    convenience exports lazy and test the exact module execution path.
  - Do not leave cheap first-party Python package entrypoint regressions as
    one-off manual validation. If the regression protects a documented
    executable surface, wire it into `scripts/check_status_consistency.sh`
    quietly.
  - Do not let Python server/smoke entrypoints silently ignore `--help` or
    malformed options. Add explicit argparse handling before starting servers,
    spawning subprocesses, or running smoke workloads, and keep cheap help-path
    checks in status consistency.
  - Do not construct CLI engines, stores, sockets, subprocesses, or provider
    clients before validating malformed/no-command paths. Usage-returning
    paths must stay side-effect free and artifact-free.
  - Do not rely on Python syntax checks to cover `--help` behavior. First-party
    policy/generator scripts need explicit cheap help paths before running
    scans, generation, or validation work.
  - Do not treat ignored ad hoc helper scripts as durable tooling once status
    gates depend on them. Unignore and track helper entrypoints before wiring
    them into `scripts/check_status_consistency.sh`.
  - Do not remove generated helper functions by skipping a fixed number of
    following lines after a signature. Match the helper block shape and replace
    targets only after a complete successful rewrite.
  - Do not rely on OS socket read/write return values being small enough for a
    callback ABI that returns `int`. Bound native I/O request sizes before
    casting counts back to callback result types.
  - Do not describe `AUDIT_2.md` M47 as a proven live runtime race without
    accounting for the production teardown order. Current teardown joins the
    worker thread before queue cleanup; the shipped change hardens the helper
    invariant by locking internally.
  - Current checkpoint: `AUDIT_2.md` H2 JIT effect fast-path primitive-entry
    guard is closed. Both current and legacy fast-path signal helpers now
    validate matched table entries before reading `prim_val`; malformed entries
    return `runtime/invalid-fast-path-primitive`, and the current dispatch path
    marks the malformed entry handled. JIT policy coverage corrupts a matched
    fast-path entry and verifies both paths fail closed.
  - Related reframe: `AUDIT_2.md` L42 is stale as a live correctness bug in the
    store path. `jit_cache_find_slot()` still has a 16-probe loop, but current
    stores use `runtime_cache_store_expr()`, which clears and retries on
    16-slot saturation. The remaining issue is cache-retention/performance
    policy, not silent store loss.
  - Previous checkpoint: `AUDIT_2.md` C3/H6 Vulkan shared-context teardown is
    closed after subagent reframe. The stale dangling-global part was already
    fixed in current code because release clears `omni_tensor_vulkan_shared_context`
    under the context mutex and acquisition retains only under that mutex. The
    live defect was final release unlocking before device/instance destruction
    and context free. Final release now holds `omni_tensor_vulkan_context_mutex`
    through the full teardown, and native regression coverage verifies stub
    destroy callbacks run while the mutex is held.
  - Previous checkpoint: `AUDIT_2.md` L41 is closed after subagent reframe. The
    compiler symbol-emission defect was broader than long identifiers:
    generated C identifiers were lossy, unbounded, and shared a namespace with
    user-authored raw `_omni_*` symbols. `Compiler.emit_symbol_name()` now keeps
    only simple C3-safe raw symbols in the raw namespace and routes lossy,
    generated-prefix, reserved, `_omni_*`, and over-budget symbols through a
    bounded `_omni_sym_..._h<hash>_l<len>` namespace. Compiler regression
    coverage checks length caps, punctuation escaping, generated namespace
    collisions, reserved-name collisions, and digit-leading prefix collisions.
  - Previous checkpoint: `AUDIT_2.md` L40 is closed after subagent reframe. The
    original null-deref concern was overstated, but path-step helper
    precondition ownership had drifted. `eval_path_step()` now dispatches by
    tag and lets module/instance/dict/cons helpers own payload validation; root
    lookup tolerates null env/global-env inputs; malformed MODULE/INSTANCE/
    HASHMAP payload regressions pass in the basic slice.
  - Previous checkpoint: `AUDIT_2.md` L37-L39 are closed. Runtime bootstrap now
    propagates stdlib `run()` failures, which exposed and fixed a malformed
    trailing `)` in `stdlib/stdlib.lisp`; FFI metadata symbol matching rejects
    null interpreters; and FFI library declarations fail closed when
    `interp.global_env` is unavailable before path evaluation or `dlopen`.
    The iterator malformed-null-thunk regression now follows the normalized
    JIT null-apply contract instead of the older iterator-pair wording.
  - Previous checkpoint: `AUDIT_2.md` M72 is closed. Blocked-fiber propagation
    now returns `JitBlockedFiberDriveStatus` and is bounded by the scheduler
    round limit, with a test-only low-limit override. Continuation, resolve,
    checkpoint, value-handle, and legacy handle-body callers distinguish yield
    failure from propagation-limit failure while preserving their cleanup and
    interpreter-state restoration obligations. The regression drives a
    re-suspending target stack under a synthetic blocked scheduler fiber and
    proves the helper exits with `JIT_BLOCKED_FIBER_DRIVE_LIMIT_EXCEEDED`.
  - Latest checkpoint: `AUDIT_2.md` H3 is closed. `jit_apply_continuation()`
    now rejects null and non-continuation values before reading `cont_val`, and
    direct helper regression coverage passes in the JIT policy slice. `L43` was
    reframed as stale for current production correctness because retired-code
    mutation is owner-thread confined; future true multi-threaded JIT work must
    synchronize or isolate that table before enabling the mode.
  - Latest checkpoint: `AUDIT_2.md` H4 is closed at the kernel contract layer.
    CUDA Complex128/Complex64 diagonal kernels now guard against inconsistent
    `diagonal_count` by checking `index` against `rows` and `cols`, and embedded
    PTX was regenerated. Public wrappers already passed `min(rows, cols)`, so
    this hardens direct/future callers without changing valid public behavior.
    The later advanced collections validation blocker was closed as a stale
    test assertion bug, and the group now passes.
  - Latest checkpoint: `AUDIT_2.md` L44 is closed. AOT no-interpreter error
    construction now records an out-of-band diagnostic instead of silently
    collapsing context to raw null, while preserving the no-orphan-`Value*`
    return contract when no `Interp*` exists.
  - Latest checkpoint: `AUDIT_2.md` L45 and L46 are closed. JIT local-env
    capture now returns a direct OOM error when emitted `jit_env_extend` fails
    for non-mutable local bindings, malformed lambda ASTs fail closed before
    closure construction, and closure constructor results are validated before
    payload access. The previously unrelated Vulkan softmax advanced-suite
    blocker is also closed as a test contract bug: the assertion now uses
    nested binary `+`, matching the language arity contract.
  - Latest checkpoint: `AUDIT_2.md` L47 is closed. Float literal parsing now
    bounds long fraction contribution, fails closed on missing/oversized
    literal exponents, and rejects non-finite literal results; `parse-number`
    uses the same bounded fraction/exponent policy while preserving
    Float64-overflow promotion to `BigFloat`.
  - Latest checkpoint: `AUDIT_2.md` L48 is invalidated as a correctness bug
    because C `0` is a null pointer constant. The native helper was still
    cleaned up to use `NULL`, and DNS render coverage now verifies too-small
    buffers return `OMNI_ADDRINFO_RENDER_FAILED`.
  - Latest checkpoint: `AUDIT_2.md` C19 is closed under `MEM-PROOF-002
    ScopeRegion Core`. Scope owner validation now has a boolean helper and the
    public allocator, destructor-registration, and reset entrypoints fail closed
    or no-op on null scopes instead of relying on `scope_guard_owner` to return
    from the caller. ScopeRegion regressions cover null allocator, dtor
    registration, and reset contracts. `AUDIT_2.md` C20-C26 are invalidated as
    stale C-style switch-fallthrough findings; C3 switches do not implicitly
    fall through.
  - Latest checkpoint: `AUDIT_2.md` C15 and C6 are invalidated. C15 repeats the
    stale C-style switch fallthrough assumption for scheduler wakeup dispatch;
    C6 cites an older `make_error` body and current code already falls back to a
    static OOM error when value allocation fails. `AUDIT_2.md` C7 is closed at
    the unauthenticated Unix-socket transport boundary: TCP REPL is token-gated,
    stdio is caller-owned, and filesystem Unix sockets are now chmodded to
    owner-only (`0600`) before listening, with async REPL-server coverage.
  - Latest checkpoint: `AUDIT_2.md` C3 and C4 are revalidated as already fixed,
    and C5 is invalidated as another stale C-style switch-fallthrough finding.
    Vulkan shared context teardown remains serialized under one mutex through
    final destruction/free, the e2e baseline policy predicate accepts either
    approved review document without false-positive metadata failures, and
    parser string escapes emit exactly one decoded character under C3 switch
    semantics.
  - Latest checkpoint: `AUDIT_2.md` H1 is invalidated as a stale/incorrect
    ownership finding. Closure stable materialization allocates payloads inside
    a staged ESCAPE build scope, failure aborts that scope, and success splices
    it into the destination; ad-hoc per-object frees would violate the
    ScopeRegion-owned value contract. `AUDIT_2.md` L8 is also invalidated under
    the current JIT fast-path dispatch contract: matched entries are validated
    by the shared primitive-entry guard before helper dereference, and the
    targeted malformed-entry regression passes.
  - Latest checkpoint: `AUDIT_2.md` C8-C10 are closed. Method-table dispatch now
    fails closed on null `MethodTable*` and null `METHOD_TABLE` payloads across
    direct matching, dispatch-error formatting, single-arg JIT apply, multi-arg
    JIT apply, tail multi-arg JIT apply, and `jit_eval_define` fallback
    updates. `AUDIT_2.md` C11-C12 are invalidated as stale C-style switch
    fallthrough claims; the adjacent live M32 destructor policy gap is closed
    by enumerating every current scalar/no-op `ValueTag` in `scope_dtor_value`
    and removing the silent default.
  - Latest checkpoint: `AUDIT_2.md` C13 is closed. The shared import/load
    source-relative resolver now rejects leading `/`, embedded NUL bytes, and
    exact `..` path segments while preserving nested relative paths and default
    dotted-module paths. `AUDIT_2.md` C14 is invalidated as a
    contract-inverted false positive: normative effects semantics define
    `handle ^strict` as a boundary that blocks outward search for otherwise
    unhandled effects.
  - Latest checkpoint: `AUDIT_2.md` C16-C18 are closed. The x86_64 `fpu_save`
    helper now preserves the non-x86 optional-output contract, BLAS `dger` now
    rejects padded `lda` values unsupported by its output-buffer ABI before
    resolving BLAS or writing output, and JIT multi-arg primitive apply now
    fails closed on malformed primitive payloads across direct, routed, and
    tail multi-arg routes. `AUDIT_2.md` H7-H8 are invalidated as stale
    C-style switch-fallthrough findings under C3 semantics, and H9 is
    invalidated because the current lexer/parser exponent policy is already
    bounded.
  - Latest checkpoint: `AUDIT_2.md` H10-H11 are closed by centralizing parser
    root allocation through `Parser.alloc_root_value_or_error` and converting
    datum/list/template construction to propagate null with parser error state
    set. The repair also hardened adjacent parser root allocations in atom,
    collection literal, pattern, quote/quasiquote, head-form, control-effect,
    and relation-attribute paths. `AUDIT_2.md` H22 is invalidated as stale
    because current HTTP header slicing no longer uses the cited underflowing
    expression. `AUDIT_2.md` H23 is invalidated under the current raw
    file-backed Deduce storage contract; sandboxed storage would need a
    separate product-level surface.
  - Latest checkpoint: `AUDIT_2.md` H12 is closed at the raw primitive-body
    boundary. Public fixed-arity calls were already metadata-guarded, but direct
    C3 primitive-body calls now also reject oversized slices for fixed-arity
    core/math bodies; ranged `+`/`-` behavior is preserved. `AUDIT_2.md` H30 is
    closed by bounding `Env.hash_lookup`/`Env.hash_insert` probes by
    `hash_capacity`, with a full-table malformed-state regression. `AUDIT_2.md`
    H13, H26, and H27-H29 are invalidated as stale/already-fixed; `AUDIT_2.md`
    H24-H25 remain live JIT follow-up targets.
  - Latest checkpoint: `AUDIT_2.md` H24-H25 are closed. JIT spill-state
    tracking now has `JIT_STATE_SPILL_MAX` and fails compile tracking before
    allocation once reached, relying on the existing `jit_compile` cleanup path.
    Retired-code tombstone saturation now fails closed for legacy pointer-only
    liveness by treating any non-null code/serial as retired after saturation;
    nonce-bearing compiled-function liveness remains the normal authority.
  - Latest checkpoint: `AUDIT_2.md` H34-H36 are closed. Malformed closure
    payload/body apply paths now fail closed through a shared closure
    payload/body validator, and env-copy reports a named invalid-closure-state
    fault for null payloads while preserving existing null-body env-copy
    fixtures. `AUDIT_2.md` H31, H32, H33, and H38 are invalidated as stale.
  - Latest checkpoint: `AUDIT_2.md` H16 is closed. Module publication now
    preserves path/name/hash invariants across duplicate file-module creation,
    unloaded same-name replacement, module-table growth, default imports, and
    path-backed resolution.
  - Latest checkpoint: `AUDIT_2.md` H17 is invalidated. Current symbol
    interning already guards `self.count >= INVALID_SYMBOL_ID` before casting
    to `SymbolId`, and the basic hardening group has a direct ceiling
    regression.
  - Next target recommendation: continue with a fresh audit target after a
    status scan. Current live candidate is H37 as AOT tail back-edge
    interrupt/budget parity, not an arbitrary depth cap.
- Agent assignments:
  - Integration owner: Codex GPT-5.
  - Completed explorers for the latest checkpoint: Kierkegaard the 2nd audited
    M54 and found it stale because `c3c_limits.sh` is a sourced helper; Fermat
    the 2nd audited M55 and confirmed `setup()` still fell through after
    failure diagnostics before this patch.
  - Completed explorers for the IO/e2e checkpoint: Averroes the 2nd audited
    M56 and found the missing-script claim stale; Hilbert the 2nd audited M57
    and found current generated-e2e failure propagation fail-closed.
  - Completed explorers for the eval/type checkpoint: Dirac the 2nd audited
    M59 and found current cleanup reaches `exprs.free()`; Carson the 2nd
    audited M60 and found no-arg `defeffect` initialization already explicit.
  - Completed explorer for the JIT method-table checkpoint: Noether the 2nd
    audited M61-M63 and identified the adjacent `jit_eval_define()` closure
    payload precondition, which is included in the shipped fix.
  - Completed explorers for the dispatch/JIT switch checkpoint: Popper the 2nd
    audited M64 and clarified the region-owned scratch contract; Avicenna the
    2nd audited M65/M66 and confirmed latent default/fail-closed gaps.
  - Completed explorers for the parser/import/warm-cache checkpoint: Halley
    the 2nd audited M67 and confirmed the duplicated compiler parse loop; Dewey
    the 2nd audited M68 and narrowed it to buffer-capacity coupling plus
    imprecise diagnostics; Harvey the 2nd audited M69 and reframed it as a
    pointer/count traversal contract rather than a null element contract.
  - Completed explorers for the handler/multi-arg/continuation checkpoint:
    Pauli the 2nd confirmed M70 and identified the correct rollback primitive;
    Nietzsche the 2nd found M71 partly stale but still open at null-result
    normalization; Locke the 2nd confirmed M72 and recommended a bounded
    propagation limit.
  - Completed explorers for the M71-M73 follow-up: McClintock the 2nd confirmed
    the broader apply-boundary shape for M71; Raman the 2nd confirmed M72 and
    the adjacent handle-body propagation loop; Leibniz the 2nd found M73 stale
    as a live null-deref path.
  - Completed explorers for the M72 implementation: Goodall the 2nd verified
    the enum-result cleanup/restoration obligations across continuation,
    resolve, checkpoint, value-handle, and handle-body call sites; Chandrasekhar
    the 2nd recommended the direct re-suspending stack-context regression now
    registered in the JIT policy slice.
  - Completed explorers for the L37-L39 checkpoint: Jason the 2nd confirmed
    stdlib bootstrap still discarded `run()` failures; Herschel the 2nd
    confirmed the FFI symbol-name helper null-interpreter guard gap; Bacon the
    2nd confirmed the FFI lib global-env guard shape after integration; Carver
    the 2nd audited the iterator fold mismatch and confirmed the test should
    follow the normalized JIT null-apply contract.
  - Completed explorer for the L40 checkpoint: Galileo the 2nd reframed the
    item from a null-deref bug into path-step helper precondition ownership
    drift and recommended direct malformed-payload basic coverage.
  - Completed explorers for the L41 checkpoint: Heisenberg the 2nd reframed
    the defect from long identifiers into lossy/unbounded identifier mangling;
    Planck the 2nd caught the generated-namespace and prefix collision
    regression; Parfit the 2nd proposed Vulkan context teardown and JIT
    effect-fast-path guard candidates for the next slice.
  - Completed explorers/workers for the Vulkan teardown checkpoint: Descartes
    the 2nd confirmed acquisition no longer observes stale global state but
    final destruction still occurred after unlock; James the 2nd confirmed the
    native test needed a runtime-side hook to observe the static mutex.
  - Completed explorers for the JIT cache/fast-path checkpoint: Godel the 2nd
    reframed L42 as stale for correctness but still bounded-window policy;
    Volta the 2nd confirmed malformed fast-path primitive entries could
    dereference `prim_val` in both current and legacy paths.
  - Completed explorers for the continuation/retired-code checkpoint: Ptolemy
    the 2nd confirmed H3 as a live direct-helper fail-closed bug; Einstein the
    2nd reframed L43 as stale under the current owner-thread JIT contract.
  - Completed explorers for the CUDA/AOT checkpoint: Zeno the 2nd confirmed H4
    as live at the CUDA kernel contract despite public wrapper mitigation;
    Gibbs the 2nd reframed L44 as a live AOT diagnostic gap outside normal
    generated-main startup.
  - Completed explorers for the ScopeRegion/switch-fallthrough checkpoint:
    Epicurus the 2nd confirmed C19 as a live ScopeRegion null-entry fail-closed
    defect and mapped it to `MEM-PROOF-002`; Rawls the 2nd invalidated C20-C26
    as stale C3 switch-fallthrough assumptions.
  - Completed explorers for the scheduler/error/REPL checkpoint: Laplace the
    2nd invalidated C15 as another stale C3 switch-fallthrough claim; Sagan the
    2nd invalidated C6 as already fixed and confirmed C7 as live for
    unauthenticated Unix-socket transport access before the owner-only socket
    hardening.
  - Completed explorers for the Vulkan/e2e/parser checkpoint: Hypatia the 2nd
    revalidated C3 and C4 as already fixed; Schrodinger the 2nd invalidated C5
    as a stale C3 switch-fallthrough claim and verified the string escape
    contract with a runtime probe.
  - Completed explorers for the closure/JIT-fast-path checkpoint: Erdos the 2nd
    invalidated H1 as stale under staged ESCAPE build-scope ownership; Darwin
    the 2nd invalidated L8 as stale under the H2 shared fast-path primitive
    validation contract.
  - Completed explorers for the method-table/destructor checkpoint: Hooke the
    2nd confirmed C8-C10 live and broader than written across method-table
    payload boundaries; Bernoulli the 2nd invalidated C11-C12 under C3 switch
    semantics and identified M32 as the live destructor-policy hardening item.
  - Completed explorers for the import/effects checkpoint: Turing the 2nd
    confirmed C13 live and broader than import-only because `(load path)` uses
    the same resolver; Lorentz the 2nd invalidated C14 as a strict-handler
    contract inversion.
  - Completed explorers for the stack/BLAS/JIT checkpoint: Faraday the 2nd
    confirmed C16 as a live exported helper-contract bug; Anscombe the 2nd
    confirmed C17 as a live native helper ABI defect but not reachable through
    the public dense tensor path; Newton the 2nd confirmed C18 live and
    invalidated H7-H9 as stale under current C3 switch and bounded lexer
    contracts.
  - Completed explorers for the parser/HTTP/Deduce checkpoint: Huygens the 2nd
    confirmed H10-H11 as live and broader than datum helpers; Russell the 2nd
    invalidated H22 as stale under current HTTP response parsing but recorded
    unrelated broad HTTP slice failures; Banach the 2nd invalidated H23 under
    the current raw caller-chosen Deduce path contract and recommended a
    separate sandboxed-storage design if that capability is desired.
  - Completed explorers for the primitive/async/JIT checkpoint: Poincare the
    2nd found H12 public-call arity stale but raw primitive-body arity live;
    Kepler the 2nd invalidated H13 under centralized FFI handle cleanup;
    Mencius the 2nd confirmed H24 and H25 live and invalidated H26 as a stale
    truncation claim under current bounded CLI diagnostics.
  - Completed explorer for the H31-H38 checkpoint: Aristotle the 2nd
    invalidated H31, H33, and H38; confirmed H34-H36 as live malformed closure
    payload/body fail-closed defects; classified H32 as lower-risk enum metadata
    fallback work; and flagged H37 as policy-sensitive AOT tail-budget work.
  - Completed explorers for the H34-H38 closure/AOT follow-up: Hume the 2nd
    invalidated H32 under C3 exhaustive enum-switch semantics; Kuhn the 2nd
    reframed H37 as shared interrupt/budget polling on AOT tail back-edges
    rather than a recursion-depth cap; Gauss the 2nd confirmed H16 as the next
    independent high-priority module cache/index candidate.
  - Completed explorers for the H16 module cache/index checkpoint: Copernicus
    the 2nd identified the advanced runtime module path edge tests as the right
    regression location; Bohr the 2nd widened the fix boundary to rollback
    stale unloaded modules, skip retired hash slots during growth, dedupe
    file-module paths, and resolve path-backed imports path-first.
  - Completed explorer for the post-H37 checkpoint: Wegener the 2nd identified
    H18 as the next bounded live memory/lifetime candidate, specifically exact
    duplicate `(ptr, func)` ScopeRegion destructor registrations.
  - Closed H37 by sharing `runtime_eval_backedge_poll` across runtime/JIT eval
    and AOT tail redispatch loops. Validation passed: C3 diagnostics,
    `c3c build main`, compiler slice (`pass=416 fail=0`), file-size gate, and
    `git diff --check`.
  - Next target recommendation: H18 exact duplicate destructor registration in
    `src/scope_region_chunk_helpers.c3`, preserving same-pointer/different
    destructor registrations and allocation-failure behavior.
  - Completed explorer for H18: Euler the 2nd confirmed the right regression
    targets are TEMP coverage in `scope_region_run_refcount_destructor_tests`
    and ESCAPE coverage in `run_scope_region_alloc_lifecycle_tests`, with
    forced dtor-metadata OOM on exact duplicate registration.
  - Closed H18 by making exact duplicate `(ptr, func)` destructor registration
    idempotent before allocation for TEMP and ESCAPE lanes while preserving
    same-pointer/different-function registrations. Validation passed: C3
    diagnostics, `c3c build main`, ScopeRegion suite (`pass=67 fail=0`),
    Valgrind ScopeRegion (`ERROR SUMMARY: 0 errors`), file-size gate, and
    `git diff --check`.
  - Broad memory-lifetime-smoke was attempted only through the bounded
    container path as required, but that container run was killed with exit 137
    before a final summary; do not count it as passing H18 evidence.
  - Next target recommendation: continue with a fresh scan, with H19-H21 still
    visible in `AUDIT_2.md` after H18 closure.
  - Completed explorers for H19-H21: Ramanujan the 2nd confirmed H19 as a
    bounded live BigInteger backend-null defect; Hubble the 2nd confirmed
    H19-H21 are all live and prioritized H21 as the next higher-blast compiler
    generated-name bounds fix.
  - Closed H19 by guarding malformed `BIG_INTEGER` null payloads before
    BigInteger binary backend calls, plus adjacent unary negation and
    bitwise-not null-payload guards. Regression coverage constructs malformed
    internal BigInteger values and verifies binary helper routes fail closed.
    Validation passed: C3 diagnostics, `c3c build main`, advanced stdlib
    numeric filter (`pass=437 fail=0`), file-size gate, and `git diff --check`.
  - Closed H21 by making all generated-name helpers capacity-checked and
    non-truncating, routing compiler call sites through `Compiler.generated_name_*`
    wrappers that set a compile diagnostic on overflow. Regression coverage
    verifies exact-capacity success, overflow failure for id/index/suffix/pair,
    and compiler wrapper diagnostics. Validation passed: C3 diagnostics,
    `c3c build main`, compiler slice (`pass=417 fail=0`), e2e compiler
    generation (`431` tests), file-size gate, and `git diff --check`.
  - Closed H20 by replacing the serializer `default -> nil` fallback with an
    explicit source/value contract: reconstructible arrays, dictionaries, sets,
    `Void`, `TimePoint`, and big numeric values serialize to parser-compatible
    source; opaque runtime/resource values set a compiler diagnostic. Regression
    coverage verifies collection, BigInteger, TimePoint, and opaque-value
    fail-closed behavior. Validation passed: C3 diagnostics, `c3c build main`,
    compiler slice (`pass=423 fail=0`), e2e compiler generation (`431` tests),
    file-size gate, and `git diff --check`.
  - Completed explorers for M33: Linnaeus the 2nd and Lovelace the 2nd
    confirmed public `lshift`/`rshift` already enforce the shift cap, but the
    shared `big_integer_shift_value` helper still needed cap and malformed
    payload guards.
  - Closed M33 by enforcing `BIG_INTEGER_MAX_SHIFT_BITS` and null/malformed
    operand guards in `big_integer_shift_value`. Regression coverage directly
    exercises over-cap integer and BigInteger helper calls plus malformed
    BigInteger payload shifts. Validation passed: C3 diagnostics,
    `c3c build main`, advanced numeric sort/bitwise/HOF filter
    (`pass=41 fail=0`), full advanced stdlib numeric filter
    (`pass=438 fail=0`), file-size gate, and `git diff --check`.
  - Completed read-only explorers for M34/M35: Feynman the 2nd and Boole the
    2nd confirmed the defects were live as missing C3-side guards, while
    current C++ backends already tolerate the null handles.
  - Closed M34/M35 by adding malformed null-payload guards to BigFloat and
    BigComplex raw-handle/component helpers. Regression coverage directly
    constructs malformed BigInteger, BigFloat, and BigComplex values and
    verifies BigFloat conversion/comparison, BigComplex part construction,
    real/imag extraction, and conjugation fail closed. Validation passed:
    C3 diagnostics, `c3c build main`, full advanced stdlib numeric filter
    (`pass=439 fail=0`), file-size gate, and `git diff --check`.
  - Closed M36 by adding shared malformed backing-storage guards for HashMap
    direct helpers, public dict/set wrappers, canonical keys/values iteration,
    length/mutation helpers, and print paths. Regression coverage directly
    constructs malformed internal HashMap/set values in the advanced collections
    module and verifies helper, primitive, and print fail-closed behavior.
    Validation passed: C3 diagnostics, `c3c build main`, advanced collections
    module filter (`pass=2144 fail=0`), file-size gate, and `git diff --check`.
  - `[INVALIDATED]` M36 over-strict storage validation that also required
    `map.mask == map.capacity - 1` rejected environment hash tables and caused
    `WARNING: env hash table build failed; falling back to linear lookup`. Keep
    the shared malformed-storage guard to non-null map, non-null entries, and
    power-of-two capacity unless a separate normalized-map contract is proven.
  - `[FAILED]` M36 memory-lifetime checked-collections placement did not yield
    useful validation evidence here. The bounded `memory-lifetime-smoke`
    container path timed out/killed with exit 137 twice, so do not cite it as
    passing M36 evidence. The effective regression is the focused advanced
    collections module case.
  - Completed read-only explorers for M37: Meitner the 2nd confirmed natural
    rehash probe exhaustion is unreachable under current valid-map invariants
    but the grow path still lacked an atomic impossible-state guard; Ampere the
    2nd recommended focused advanced collections coverage because it is
    host-runnable and avoids the current memory-lifetime smoke timeout path.
  - Closed M37 by rehashing into local new storage before publishing any grow
    metadata. If forced rehash insertion fails, `hashmap_grow_checked` frees
    the new storage and preserves the original entries pointer, capacity, mask,
    count, and lookups. Regression coverage forces that impossible-state path.
    Validation passed: C3 diagnostics, `c3c build main`, advanced collections
    module filter (`pass=2145 fail=0`), file-size gate, and `git diff --check`.
  - Closed M38 scheduler parent admission hardening. `scheduler_add_fiber`
    now rejects stale, completed, out-of-range, and cross-owner parents before
    publication; `prim_spawn` reports invalid-parent failures and cleans the
    just-created coroutine context on failed admission; scheduler abort/idle
    reset releases discarded fiber coroutine contexts before clearing slots.
    Regression coverage pins completed/stale parent rejection and nested
    blocking scheduler operations outside the fiber root context. Validation
    passed: C3 diagnostics, `c3c build main`, scheduler slice
    (`pass=147 fail=0`), `scripts/check_scheduler_state_guards.sh`, file-size
    gate, and `git diff --check`.
  - `[INVALIDATED]` The original M38 public-spawn framing was too broad:
    normal public `spawn` only inherits a parent while a fiber is actively
    running. Treat M38 as an internal scheduler helper admission and cleanup
    contract closure, not proof of a natural completed-parent public spawn path.
  - `[INVALIDATED]` Do not use a language-level checkpoint test that spawns a
    child from inside the checkpoint as the nested scheduler-op regression. It
    depends on stack-context allocation/cache state instead of the scheduler
    root-context guard. The accepted regression directly simulates a nested
    stack context and calls `await`.
  - Completed read-only M39 exploration: Singer the 2nd confirmed
    `SymbolTable.intern` can publish an `entries[id]` without a matching
    `hash_index` slot if insertion cannot find an empty probe slot. Next target
    recommendation: continue with `AUDIT_2.md` M39 symbol-table hash insertion
    atomicity.
  - Closed M39 by routing both new-symbol intern insertion and grow rehashing
    through a checked hash-index insertion helper. Intern now claims a hash
    slot before publishing `entries[id]` or incrementing `count`; insertion
    failure frees the allocated name and returns `INVALID_SYMBOL_ID` with no
    table side effects. Grow aborts before publishing new storage if any old
    entry cannot be rehashed. Regression coverage forces a synthetic full
    hash-index table and verifies repeated insert attempts preserve `count` and
    the existing symbol. Validation passed: C3 diagnostics, `c3c build main`,
    basic slice (`pass=175 fail=0`), file-size gate, and `git diff --check`.
  - Next target recommendation: continue with `AUDIT_2.md` M40 nested-pattern
    match exhaustiveness unless a fresh scan identifies a higher-priority live
    regression.
  - Closed M40 by making match diagnostic union-variant coverage recurse
    through constructor subpatterns, guards, as-patterns, cons patterns, and
    sequence patterns while preserving top-level wildcard coverage and the
    existing nullary-constructor `PAT_VAR` path. Diagnostics coverage now
    asserts a guarded nested constructor clause that structurally covers a
    variant no longer produces a false `missing variants` diagnostic.
    Validation passed: C3 diagnostics, `c3c build main`, diagnostics slice
    (`pass=11 fail=0`), file-size gate, and `git diff --check`.
  - Closed M41 by rejecting already-published but not-yet-loaded declared
    modules in `jit_eval_declared_module_file` as circular imports. Regression
    coverage seeds an in-progress declared file module, re-enters the declared
    file evaluator, verifies `circular import detected`, and rolls back the
    seeded loading module. A top-down helper split moved the Float64 dictionary
    regression helper into a companion part file so the module test file stays
    below the tracked-code 1000 LOC gate. Validation passed: C3 diagnostics,
    `c3c build main`, advanced collections module filter (`pass=2146 fail=0`),
    file-size gate, and `git diff --check`.
  - `[FAILED]` The first two M41 advanced-filter attempts used nonexistent
    group names (`advanced-stdlib-module`, `advanced-module-system`) and
    matched no tests. The correct reachable group key is
    `advanced-collections-module`.
  - `[FAILED]` A full unfiltered advanced slice crashed before module tests
    while entering `advanced-logic-tco`, so it is not M41 validation evidence.
    Use the filtered `OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module`
    command for this closure unless that earlier slice failure is separately
    fixed.
  - Next target recommendation: continue with `AUDIT_2.md` M42 AOT
    quasiquote nesting depth unless a fresh scan identifies a higher-priority
    live regression. Read-only exploration confirmed M42 is real, but the
    `emit_error_temp` fix sketch is stale; use `Compiler.set_compile_error`
    plus `usz.max` propagation.
  - Closed M42 by adding an AOT quasiquote max-depth guard matching JIT's
    64-level limit and by propagating `has_error`/`usz.max` before quasiquote
    app/list and call-splice lowering emit dependent temporaries. Regression
    coverage verifies over-nested quasiquote compilation fails closed and that
    the failure sentinel does not leak into generated C temp references.
    Validation passed: C3 diagnostics, `c3c build main`, compiler slice
    (`pass=425 fail=0`), file-size gate, and `git diff --check`.
  - `[INVALIDATED]` M42's suggested `emit_error_temp` API is stale; current
    compiler failure reporting uses `Compiler.set_compile_error(...)` plus
    `usz.max` result propagation.
  - Next target recommendation: continue with `AUDIT_2.md` M43 multi-shot
    continuation handler-state refcount unless a fresh scan identifies a
    higher-priority live regression.
  - `[INVALIDATED]` M43's proposed decrement is wrong for the current
    multi-shot ownership model. `jit_apply_handle_continuation_value` clones
    and consumes only the resumed stack clone; it must not release the original
    continuation wrapper's retained handler-state reference. That reference is
    released by wrapper/escape cleanup or single-shot `resolve` consumption.
    Regression coverage constructs a retained active handle continuation over a
    suspended stack context and verifies multi-shot application preserves
    `handle_retained` and `continuation_refcount == 1`. Validation passed:
    C3 diagnostics, `c3c build main`, focused JIT policy filter (`pass=1
    fail=0`), full `jit-policy` slice (`pass=79 fail=0`), file-size gate, and
    `git diff --check`.
  - `[FAILED]` The first M43 focused runtime command used nonexistent
    `OMNI_LISP_TEST_SLICE=jit`; the correct slice is `jit-policy`.
  - Next target recommendation: continue with `AUDIT_2.md` M44 unbounded
    perform/resume dispatch loops unless a fresh scan identifies a
    higher-priority live regression.
  - Closed M44 by replacing the stale fixed-cycle-cap recommendation with the
    existing interrupt/backedge-poll contract. Both handle redispatch loops now
    call `runtime_eval_backedge_poll(interp)` before clearing
    `state.signaled` or allocating/binding another continuation. Regression
    coverage directly exercises `jit_handle_run_signals` and
    `jit_handle_dispatch_signals`, verifies `evaluation interrupted`, and
    verifies no continuation is allocated on the interrupted backedge.
    Validation passed: C3 diagnostics, `c3c build main`, focused JIT policy
    filter (`pass=2 fail=0`), full `jit-policy` slice (`pass=81 fail=0`),
    file-size gate, and `git diff --check`.
  - `[INVALIDATED]` M44's suggested hard `10,000` perform/resume dispatch cap
    is not the current semantic contract. Large finite effect loops remain
    valid; resumptive dispatch backedges must be interruptible through
    `runtime_eval_backedge_poll`.
  - Next target recommendation: continue with `AUDIT_2.md` M45 memory
    telemetry counter atomicity unless a fresh scan identifies a higher-priority
    live regression.
  - Closed M45 for scope/fiber/transfer telemetry by routing process-wide
    counters through shared relaxed-atomic helpers and replacing raw stats
    copies with field-by-field snapshot helpers. Allocation, lifecycle, reset,
    transfer, fiber-temp, runtime-memory-stats, and benchmark snapshot readers
    now share the same race-free aggregate contract. Regression coverage adds a
    threaded scope telemetry test that creates independent scopes across worker
    threads and checks create/release/destroy/TEMP/ESCAPE allocation deltas.
    Validation passed: C3 diagnostics, `c3c build main`, scope suite
    (`pass=68 fail=0`), counters-enabled build plus scope suite, basic Lisp
    slice (`pass=175 fail=0`), memory telemetry benchmark envelope check,
    file-size gate, and `git diff --check`.
  - `[FAILED]` Local ThreadSanitizer evidence is currently unavailable for
    M45. `c3c build main --sanitize=thread -D OMNI_BOUNDARY_INSTR_COUNTERS`
    linked, but the resulting binary aborted before executing tests with
    `FATAL: ThreadSanitizer: unexpected memory mapping ...`.
  - Active residual: `AUDIT_2.md` M45A tracks the adjacent boundary
    route/value-shape telemetry atomicity gap in
    `src/lisp/eval_boundary_telemetry.c3`. M45 intentionally closed only the
    scope/fiber/transfer aggregate counters; boundary telemetry still needs the
    same helper/snapshot treatment, preferably combined with M46 saturating-add
    helpers so atomicity and overflow are handled at one boundary.
  - Next target recommendation: continue with `AUDIT_2.md` M45A/M46 boundary
    telemetry atomicity and counter overflow unless a fresh scan identifies a
    higher-priority live regression.
  - Closed M45A by moving boundary route/value-shape telemetry to relaxed
    atomic load/store, saturating CAS-add/inc, and atomic max helpers.
    Decision and value-shape snapshots/restores are now field-wise, and
    scope-chain/audit-budget readers no longer read raw global counters. The
    new `boundary-telemetry` slice runs focused saturation and threaded counter
    regressions without invoking the full memory-lifetime smoke suite.
    Validation passed: C3 diagnostics, counters-enabled `c3c build main -D
    OMNI_BOUNDARY_INSTR_COUNTERS`, `boundary-telemetry` slice (`pass=2
    fail=0`), normal `c3c build main`, basic Lisp slice (`pass=175 fail=0`),
    file-size gate, and `git diff --check`.
  - `[FAILED]` A Docker-bound `memory-lifetime-smoke` validation with a
    counters-enabled rebuild entered the slice but was killed by the configured
    validation timeout/resource wrapper before producing a pass/fail result.
    Treat the new focused `boundary-telemetry` slice as the targeted M45A
    signal; rerun broader smoke with a larger bounded timeout only when that
    broader suite is the objective.
  - Active residual: M46 remains open for scope/fiber/transfer telemetry
    overflow. M45's helper family in `src/scope_region_temp_pool_stats.c3`
    is atomic but still non-saturating; prefer replacing it with the same
    CAS-loop saturating add pattern used for boundary telemetry.
  - Next target recommendation: continue with `AUDIT-250` /
    `AUDIT_2.md` M46 scope/fiber/transfer saturating counters unless a fresh
    scan identifies a higher-priority live regression.
  - Closed M46/AUDIT-250 by replacing scope/fiber/transfer telemetry add with
    a saturating CAS-loop helper, moving guarded decrement to CAS, and
    saturating local staging/aggregation paths (`scope_chunk_bytes`,
    destructor counting, slow-sequence follow-up staging) before publication.
    Added `run_scope_region_telemetry_saturation_test()` for global scope,
    transfer, fiber-temp, local chunk-byte, destructor-count, and slow-sequence
    overflow coverage. Validation passed: C3 diagnostics, normal
    `c3c build main`, default scope suite (`scope_region pass=69 fail=0`),
    counters-enabled `c3c build main -D OMNI_BOUNDARY_INSTR_COUNTERS`,
    counters-enabled fiber-temp scope suite (`scope_region pass=69 fail=0`),
    restored normal `c3c build main`, basic Lisp slice (`pass=175 fail=0`),
    file-size gate, and `git diff --check`.
  - `[FAILED]` The first M46 saturation regression asserted feature-gated
    live/peak scope-shape counters in default builds and failed the default
    scope suite. That test assumption was invalidated; direct helper coverage
    and `scope_memory_shape_telemetry_enabled()`-guarded staging assertions are
    the accepted coverage shape.
  - Resolved residual: `AUDIT-251-TELEMETRY-TEST-SNAPSHOT-READS` tracked older
    test files that still raw-read process-wide telemetry structs instead of
    using helper snapshots. This was evidence hygiene, not an open production
    overflow defect.
  - The remaining raw global telemetry references are production helper
    implementations, protected pool mutations, and the focused M46 saturation
    helper probe.
  - Closed `AUDIT-251` by migrating raw test/runtime telemetry evidence reads
    to `scope_memory_telemetry_stats_snapshot()`,
    `fiber_temp_pool_stats_snapshot()`, and the new
    `fiber_temp_chunk_pool_count_snapshot()` helper. Remaining raw global
    references are production helper implementations, protected pool mutations,
    and the focused M46 saturation helper probe. Validation passed: C3
    diagnostics for touched files, `c3c build main`, basic Lisp slice
    (`pass=175 fail=0`), scheduler slice with `OMNI_FIBER_TEMP=1`
    (`pass=147 fail=0`), stack suite with `OMNI_FIBER_TEMP=1`
    (`pass=26 fail=0`), scope suite with `OMNI_FIBER_TEMP=1`
    (`scope_region pass=69 fail=0`), advanced collections module with
    `OMNI_ML_BENCH=1` (`pass=2146 fail=0`), file-size gate, and
    `git diff --check`.
  - Next target recommendation: do a fresh prioritized scan of `AUDIT_2.md`
    now that the telemetry helper/evidence queue is closed.
  - Closed `AUDIT_2.md` L27 by initializing `Env.is_inline = false` in
    `Interp.alloc_env()` and `Interp.alloc_env_escape()`. Added deterministic
    basic native coverage that dirties reused TEMP/ESCAPE scope memory before
    direct allocator calls. Validation passed: C3 diagnostics, `c3c build
    main`, basic Lisp slice (`pass=176 fail=0`), restored normal build,
    file-size gate, and `git diff --check`.
  - `[FAILED]` ASAN/Valgrind signal remains unavailable for this slice:
    `c3c build main --sanitize=address` reports sanitizer support unavailable
    in this toolchain configuration, and targeted Valgrind reaches the basic
    slice pass summary but exits on existing custom stack/continuation reports.
  - Next target recommendation: continue with `AUDIT_2.md` L28 unless a fresh
    scan finds a higher-priority live regression.
  - Closed `AUDIT_2.md` L28 by replacing the contract-only hash-table
    precondition on `Env.hash_lookup()` and `Env.hash_insert()` with runtime
    fail-closed guards for null env, null hash table, and zero capacity. Added
    a basic native malformed-env regression that keeps nonzero hash capacity
    with null `hash_table` and verifies lookup/insert do not crash or mutate
    the malformed hash state. Validation passed: C3 diagnostics, `c3c build
    main`, basic Lisp slice (`pass=177 fail=0`), file-size gate, and
    `git diff --check`.
  - Next target recommendation: continue with `AUDIT_2.md` L29 unless a fresh
    scan finds a higher-priority live regression.
  - Closed `AUDIT_2.md` L29 by making `hashmap_sorted_slots()` return empty
    before allocation for invalid backing storage or empty maps, and by making
    compiler dictionary/set serialization reject malformed backing storage
    before consuming sorted slot indexes. Added a compiler serializer
    regression for a malformed non-null dictionary backing with nonzero
    count/capacity and null entries. Validation passed: C3 diagnostics,
    `c3c build main`, compiler slice (`pass=426 fail=0`), advanced collections
    module filter (`pass=2146 fail=0`), file-size gate, and `git diff --check`.
  - `[FAILED]` Validation command correction: the bad filter
    `advanced-stdlib-module-generic-ops` matches no advanced tests; use
    `advanced-collections-module`.
  - Next target recommendation: continue with `AUDIT_2.md` L30 unless a fresh
    scan finds a higher-priority live regression.
  - Closed `AUDIT_2.md` L30 by adding a default `copy_env_value_fast()` switch
    branch that returns `null` for unknown value tags, preserving env-copy
    rollback semantics. Added a basic native malformed-value regression that
    corrupts a zeroed `Value` tag byte and verifies the helper rejects it.
    Validation passed: C3 diagnostics, `c3c build main`, basic Lisp slice
    (`pass=178 fail=0`), file-size gate, and `git diff --check`.
  - Next target recommendation: continue with `AUDIT_2.md` L31 unless a fresh
    scan finds a higher-priority live regression.
  - Closed `AUDIT_2.md` L31 by making `Parser.parse_dict_pattern()` reject
    duplicate symbol keys during key collection. Added a basic match/parser
    regression for `({name name} ...)` expecting `duplicate key in dict
    pattern`. Validation passed: C3 diagnostics, `c3c build main`, basic Lisp
    slice (`pass=179 fail=0`), file-size gate, and `git diff --check`.
  - Next target recommendation: continue with `AUDIT_2.md` L32 unless a fresh
    scan finds a higher-priority live regression.
  - Closed `AUDIT_2.md` L32 by making `match_pattern()` return a
    `runtime/invalid-state` error for unknown pattern tags instead of
    `match_fail()`. Added a basic native malformed-pattern regression that
    corrupts a zeroed `Pattern` tag byte and verifies the unknown-tag
    diagnostic. Validation passed: C3 diagnostics, `c3c build main`, basic
    Lisp slice (`pass=180 fail=0`), file-size gate, and `git diff --check`.
  - Next target recommendation: continue with `AUDIT_2.md` L33 unless a fresh
    scan finds a higher-priority live regression.
  - Closed `AUDIT_2.md` L33 by making `Compiler.serialize_pattern_to_buf()` set
    a compiler error for unknown pattern tags instead of emitting `_`. Added
    compiler serializer coverage that corrupts a zeroed `Pattern` tag byte and
    verifies the error plus non-wildcard output. Validation passed: C3
    diagnostics, `c3c build main`, compiler slice (`pass=427 fail=0`),
    file-size gate, and `git diff --check`.
  - Next target recommendation: continue with `AUDIT_2.md` L34 unless a fresh
    scan finds a higher-priority live regression.
  - Closed `AUDIT_2.md` L34 by normalizing file-module cache paths lexically at
    storage and lookup comparison time. The helper collapses repeated `/` and
    `.` segments while leaving `..` unresolved so the existing import safety
    checks remain authoritative. Added advanced module runtime coverage that
    verifies equivalent normalized file-module paths reuse the original module
    entry. Validation passed: C3 diagnostics, `c3c build main`, advanced
    collections module filter (`pass=2147 fail=0`), file-size gate, and
    `git diff --check`.
  - Next target recommendation: skip duplicate/status-only L35 and L36 in this
    audit section, then continue with the next live low/medium item or a fresh
    prioritized scan.
  - Closed fourth-pass script items L34-L36. `run_e2e.sh` now quotes the Stage
    3 source-array expansion, `run_deduce_perf_envelope.sh` now mirrors the
    e2e validation mount bridge into `OMNI_DOCKER_EXTRA_ARGS` with a self-test,
    and L36 was recorded as already fixed because `build_omni_chelpers.sh`
    currently uses `"${CC:-cc}"`. Validation passed: `bash -n` for the touched
    scripts, both mount-bridge self-tests, file-size gate, and
    `git diff --check`.
  - Next target recommendation: continue with the next live audit item after
    the closed script cluster, prioritizing correctness/runtime findings over
    purely cosmetic cleanup.
  - Closed prior-audit M18 by correcting boundary graph audit logging and
    verbose telemetry format strings to `%ld` for explicit `long` casts, and by
    formatting scope transfer `usz` counters through a bounded decimal helper
    before printing with `%s`. Validation passed: C3 diagnostics, `c3c build
    main`, scope suite (`pass=69 fail=0`), file-size gate, and
    `git diff --check`.
  - `[INVALIDATED]` C-style `%zu`, `%llu`, and `%u` are not valid C3 format
    strings in this toolchain; future width fixes should not reintroduce them.
  - Next target recommendation: continue with the remaining prior-audit live
    items L1-L6, starting with slice-string `%s` diagnostics or scope contract
    hardening depending on which has the clearest runtime validation path.
  - Closed prior-audit L1 by changing scope owner guard operation names and
    boundary reason/provenance helper return types to `ZString` where values are
    backed by string literals. Also invalidated/reframed stale prior-audit L2-L4
    and L6 based on current fail-closed source behavior. Validation passed: C3
    diagnostics, `c3c build main`, scope suite (`pass=69 fail=0`), file-size
    gate, and `git diff --check`.
  - Next target recommendation: continue with prior-audit L5 generation-counter
    wraparound or L7 integer-serialization consolidation, after checking whether
    current source already closed either item.
  - Closed prior-audit L5 by widening scope generation stamps to `ulong` across
    `ScopeRegion`, `Value`, closures, envs, stable escape passports/store
    entries, and promotion scope-chain cache keys. Added a basic Lisp regression
    that forces the counter above `uint.max` and verifies TEMP/ESCAPE value
    stamps preserve the widened generations. Validation passed: `c3c build
    main`, scope suite (`pass=69 fail=0`), host basic Lisp slice (`pass=181
    fail=0`), bounded-container basic Lisp slice (`pass=181 fail=0`),
    file-size gate, and `git diff --check`.
  - `[FAILED]` Broad Docker-bound `memory-lifetime-smoke` entered the slice but
    was killed by the validation wrapper with exit 137 before summary; do not
    cite it as L5 closure evidence. This matches prior broad-smoke wrapper
    failures recorded in the changelog.
  - Next target recommendation: continue with prior-audit L7 integer
    serialization consolidation or run a fresh audit scan for a higher-priority
    live correctness issue.
  - Closed prior-audit L7. The original `long.min` behavioral bug was stale in
    current source, but `Compiler.emit_int` still duplicated signed integer
    rendering. It now delegates to shared `int_to_string`, aligning native
    compiler emission with source/value serialization. Validation passed:
    `c3c build main`, compiler Lisp slice (`pass=427 fail=0`), file-size gate,
    and `git diff --check`.
  - Next target recommendation: run a fresh audit scan for the next live
    correctness item; remaining prior-audit entries after L7 need current-source
    verification before editing.
  - Closed prior-audit L9 by making `boundary_graph_audit_visit_env` treat null
    env pointers as OK empty terminals. Added direct regression coverage and a
    focused `boundary-graph-audit` Lisp slice; also wired the existing
    `boundary-telemetry` slice into execution in this workspace. Validation
    passed: `c3c build main`, bounded-container `boundary-graph-audit`
    (`pass=7 fail=0`), `boundary-telemetry` (`pass=2 fail=0`), file-size gate,
    and `git diff --check`.
  - Next target recommendation: continue with L10/L11 Vulkan helper checks or
    run a fresh audit scan for a higher-priority non-GPU correctness item.
  - Closed prior-audit L10 by guarding two-, three-, and four-output Vulkan
    barrier helpers before any output-handle dereference. Added native
    resource-safety coverage proving null-output barrier calls do not reach the
    Vulkan pipeline-barrier hook. Validation passed:
    `./scripts/build_omni_chelpers.sh`, native
    `vulkan_resource_safety_test`, `c3c build main`, file-size gate, and
    `git diff --check`.
  - Next target recommendation: continue with L11 Vulkan copy-range validation
    if current source still exposes a live issue, otherwise scan for the next
    non-stale correctness item.
  - Invalidated prior-audit L11 as stale in current source. Vulkan
    `copy_range_to_host` already maps the requested `(offset, byte_len)` range,
    and native resource-safety coverage verifies the map hook receives the
    subrange. No production change was needed beyond preserving the existing
    coverage during the L10 native test run.
  - Next target recommendation: continue with L12 `mktemp` portability or run a
    fresh scan for the next live correctness item.
  - Closed prior-audit L12 by replacing bare `mktemp` in
    `scripts/run_global_gates.sh` with `mktemp -t omni_asan_build.XXXXXX`.
    Validation passed: `bash -n scripts/run_global_gates.sh`, direct
    `mktemp -t` probe, and `git diff --check -- scripts/run_global_gates.sh`.
  - Next target recommendation: refresh the audit queue beyond the closed
    prior-audit L9-L12 cluster and prefer live correctness items over stale
    status-table entries.
  - Closed prior-audit M8 by making Vulkan physical-device selection fail
    closed when enumerate-physical-devices or queue-family-properties callbacks
    are unavailable, and by extending native Vulkan resource-safety coverage for
    those selector null guards. Validation passed:
    `./scripts/build_omni_chelpers.sh`, native
    `vulkan_resource_safety_test`, `c3c build main`, file-size gate, and
    `git diff --check`.
  - Next target recommendation: continue current-source verification of the
    remaining prior-audit table rows, especially BLAS/LAPACK resolution, before
    editing because row IDs have proven stale or underspecified.
  - Closed prior-audit M6 by decoupling optional BLAS symbol availability from
    `cblas_dgemm`: the resolver now preserves a BLAS handle when any supported
    CBLAS symbol is present, `omni_tensor_backend_blas_available()` remains the
    `dgemm` gate, and optional `dgemv`/`ddot`/`dger` availability is reported
    independently. Added a native partial-symbol-table contract check.
    Validation passed: `./scripts/build_omni_chelpers.sh`, native
    `stack_fpu_blas_contract_test`, BLAS C syntax-only check, `c3c build main`,
    file-size gate, and `git diff --check`.
  - Next target recommendation: verify prior-audit M7 against current
    `csrc/tensor_lapack_helpers.c`; do not assume the status table is live,
    because M6/M8/L-series rows required current-source verification first.
  - Closed prior-audit M7. Current LAPACK symbol assignment already had
    per-symbol null checks, and this slice added native partial-symbol-table
    coverage plus a resolver fast path for already-published handles. The test
    proves `dgetrf` availability is independent of unrelated symbols and QR
    availability requires both `dgeqrf` and `dorgqr`. Validation passed: LAPACK
    and BLAS C syntax-only checks, `./scripts/build_omni_chelpers.sh`, native
    `stack_fpu_blas_contract_test`, `c3c build main`, file-size gate, and
    `git diff --check`.
  - Next target recommendation: continue with prior-audit M9 default-case
    verification or a fresh higher-priority audit scan; stale table rows should
    be verified against current source before editing.
  - Closed the M9 native/AOT literal-lowering sub-slice. Active hypothesis:
    `Compiler.compile_literal` was the live default-switch contract failure
    because unsupported value tags silently lowered to `aot::make_nil()` while
    stdlib macro expansion also legitimately emitted dictionary, array, set,
    primitive, and definition-time global closure literals. Current approach:
    preserve those reproducible literal values with explicit AOT constructors
    or global relinks, and fail closed only for runtime-only/opaque values.
    Validation passed: C3 diagnostics, `c3c build main`, direct macro-expansion
    compile probe, compiler Lisp slice (`pass=428 fail=0`), file-size gate, and
    `git diff --check`.
  - Negative-memory constraint for the residual M9 work: do not reapply the
    blanket "reject all non-scalar literal tags" approach. It broke stdlib
    macro AOT compilation by rejecting valid collection, primitive, and
    captured global function literals. Next checkpoint: audit the remaining
    default-switch sites as independent contracts, starting with compiler/AOT
    sites that can silently produce success-shaped output.
  - Closed another M9 residual at the AOT primitive-resolution boundary.
    `aot::lookup_prim` now returns an explicit runtime error when a generated
    primitive reference is absent from the runtime global environment instead
    of returning `nil`. Validation passed: C3 diagnostics, `c3c build main`,
    compiler Lisp slice (`pass=428 fail=0`), file-size gate, and `git diff
    --check`.
  - Closed the next M9 residual at the AOT module-export lookup boundary.
    `aot::lookup_module_export` now returns an explicit error if a module
    declares an export but the module environment does not bind it. A direct
    malformed loaded-module regression covers the exported-but-unbound state.
    Validation passed: C3 diagnostics, `c3c build main`, compiler Lisp slice
    (`pass=428 fail=0`), file-size gate, and `git diff --check`.
  - Closed the next M9 residual at the AOT variable lookup boundary.
    `aot::lookup_var` now returns an explicit unbound-variable error instead
    of `nil`, and `aot::define_var` propagates an incoming error value rather
    than publishing it as a successful binding. Validation passed: C3
    diagnostics, `c3c build main`, compiler Lisp slice (`pass=428 fail=0`),
    file-size gate, and `git diff --check`.
  - Closed another M9 residual at the AOT match-guard callback boundary.
    `aot::match_guard_eval` now returns an explicit error when a callable guard
    callback returns null instead of converting that malformed callback result
    into `nil` and silently falling through. Validation passed: C3 diagnostics,
    `c3c build main`, compiler Lisp slice (`pass=428 fail=0`), file-size gate,
    and `git diff --check`.
  - Closed another M9 residual at the compiler source-serialization boundary.
    Unknown expression tags and unsupported singleton type-literal tags now set
    explicit compiler errors instead of serializing to `nil`. Regression
    coverage lives in the serializer metadata group. Validation passed: C3
    diagnostics, `c3c build main`, compiler Lisp slice (`pass=430 fail=0`),
    file-size gate, and `git diff --check`.
  - Closed another M9 residual at the AOT type-annotation metadata emission
    boundary. Unsupported singleton literal value tags now set an explicit
    compiler error only when `has_val_literal` is true; inactive metadata
    storage fields keep the existing benign `NIL` emission. Validation passed:
    C3 diagnostics, `c3c build main`, compiler Lisp slice (`pass=431 fail=0`),
    file-size gate, and `git diff --check`.
  - Negative-memory constraint: do not make the low-level value-tag renderer
    reject unsupported tags unconditionally. That overbroad attempt poisoned
    ordinary annotations whose inactive `val_literal` storage was initialized
    with non-literal tags and failed the compiler slice (`pass=225 fail=206`).
  - Closed another M9 residual at the AOT quasiquote lowering boundary.
    Standalone unquote-splicing and unsupported internal quasiquote template
    expression tags now set explicit compiler errors instead of emitting
    success-shaped `nil` temps. Validation passed: C3 diagnostics,
    `c3c build main`, compiler Lisp slice (`pass=433 fail=0`), file-size gate,
    and `git diff --check`.
  - Closed another M9 residual at the AOT match pattern-check lowering boundary.
    Unknown internal `PatternTag` values now set an explicit compiler error and
    emit `false` for the generated boolean placeholder instead of compiling as
    catch-all matches. Validation passed: C3 diagnostics, `c3c build main`,
    compiler Lisp slice (`pass=434 fail=0`), file-size gate, and `git diff
    --check`.
  - Closed another M9 residual at the FFI contract manifest boundary. Invalid
    raw ABI type tags now set an explicit compiler error and serialize as
    `Invalid` instead of being published as `Void`. Validation passed: C3
    diagnostics, `c3c build main`, compiler Lisp slice (`pass=435 fail=0`),
    file-size gate, and `git diff --check`.
  - Negative-memory constraint: do not validate FFI manifest tags after casting
    raw integers to `FfiTypeTag`; C3 traps invalid enum conversions before the
    helper can report the manifest contract error.
  - Closed another M9 residual at the generated-global collection boundary.
    Unknown internal expression/pattern tags now set explicit compiler errors
    instead of being treated as "no generated globals needed"; the
    inline-module backing collector also now traverses `E_WITH_MODULE` bodies.
    Validation passed: C3 diagnostics, `c3c build main`, compiler Lisp slice
    (`pass=438 fail=0`), file-size gate, and `git diff --check`.
  - Negative-memory constraint: do not make inline-module backing collection
    strict without enumerating valid traversal forms. The first strict default
    attempt missed `E_WITH_MODULE` and failed existing module/private-backing
    regressions (`pass=434 fail=4`).
  - Closed another M9 residual at the lambda-scan boundary. Unknown internal
    expression and pattern tags now set explicit compiler errors instead of
    being treated as "no lambdas found"; valid leaf/declaration tags remain
    explicit no-op scan cases. Validation passed: C3 diagnostics,
    `c3c build main`, compiler Lisp slice (`pass=440 fail=0`), file-size gate,
    and `git diff --check`.
  - Closed another M9 residual at the free-variable analysis boundary. Unknown
    internal expression tags and malformed pattern-binding tags now set
    explicit compiler errors instead of producing incomplete capture metadata.
    Validation passed: C3 diagnostics, `c3c build main`, compiler Lisp slice
    (`pass=442 fail=0`), file-size gate, and `git diff --check`.
  - Closed another M9 residual at the mutable-capture prescan boundary.
    Unknown internal expression and pattern tags now set explicit compiler
    errors instead of being treated as no mutable captures; `E_MODULE` and
    `E_WITH_MODULE` bodies are traversed explicitly. Validation passed: C3
    diagnostics, `c3c build main`, compiler Lisp slice (`pass=444 fail=0`),
    file-size gate, and `git diff --check`.
  - Negative-memory constraint: do not make mutable-capture prescan strict
    without explicit module traversal. The first strict attempt missed
    `E_MODULE`/`E_WITH_MODULE` and failed existing module/private-backing
    compiler regressions (`pass=411 fail=33`).
  - Closed another M9 residual at the quasiquote free-variable analysis
    boundary. Unknown internal expression tags now set explicit compiler
    errors instead of being treated as capture-free quasiquote templates; valid
    non-unquote expression forms remain explicit capture-free template cases.
    Validation passed: C3 diagnostics, `c3c build main`, compiler Lisp slice
    (`pass=445 fail=0`), file-size gate, and `git diff --check`.
  - Closed another M9 residual at the AOT match binding lowering boundary.
    Unknown internal pattern tags now set explicit compiler errors instead of
    silently emitting no binding code. Validation passed: C3 diagnostics,
    `c3c build main`, compiler Lisp slice (`pass=446 fail=0`), file-size gate,
    and `git diff --check`.
  - Closed another M9 residual at the AOT match guard scan/lowering boundary.
    Unknown internal pattern tags now set explicit compiler errors instead of
    being treated as no guard work. Validation passed: C3 diagnostics,
    `c3c build main`, compiler Lisp slice (`pass=447 fail=0`), file-size gate,
    and `git diff --check`.
  - Closed another M9 residual at the inline-module metadata classification
    boundary. Export classification and local collection now reject unknown
    internal expression/pattern tags instead of treating malformed module
    inputs as non-exporting no-ops. Validation passed: C3 diagnostics,
    `c3c build main`, compiler Lisp slice (`pass=448 fail=0`), file-size gate,
    and `git diff --check`.
  - Closed another M9 residual at the AOT type metadata value-tag emission
    boundary. Unsupported value tags now set an explicit compiler error instead
    of silently emitting `NIL`; `NIL` itself is now an explicit valid case.
    Initial attempt incorrectly treated `NIL` as fallback-only and regressed the
    compiler slice (`pass=243 fail=206`); the corrected slice passed C3
    diagnostics, `c3c build main`, compiler Lisp slice (`pass=449 fail=0`),
    file-size gate, and `git diff --check`.
  - Closed another M9 residual at the FFI preload/manifest discovery boundary.
    Unknown internal expression tags now set explicit compiler errors instead
    of silently suppressing startup preload and contract-manifest work; valid
    quasiquote template forms remain explicit no-op cases. Initial strict
    attempt missed quasiquote/unquote no-ops and regressed the compiler slice
    (`pass=445 fail=5`); corrected validation passed C3 diagnostics,
    `c3c build main`, compiler Lisp slice (`pass=450 fail=0`), file-size gate,
    and `git diff --check`.
  - Closed another default-switch residual at the runtime sequence-pattern
    matching boundary. Unknown internal rest positions now produce
    `runtime/invalid-state` instead of ordinary non-match results. Validation
    passed C3 diagnostics, `c3c build main`, basic Lisp slice
    (`pass=182 fail=0`), compiler Lisp slice (`pass=450 fail=0`), file-size
    gate, and `git diff --check`.
  - Closed another runtime M9 residual at the literal-dispatch signature
    boundary. Malformed internal `ValueLiteralKey` tags now produce
    `runtime/invalid-state` instead of ordinary literal mismatches, nil-ish
    diagnostics, or method-table fallback. Validation passed C3 diagnostics,
    `c3c build main`, filtered advanced dispatch slice (`pass=254 fail=0`),
    file-size gate, and `git diff --check`.
  - Closed a method-dispatch diagnostic truncation defect adjacent to C8.
    `format_dispatch_error` now uses a full integer buffer for expected arity
    hints, and direct regression coverage verifies a five-digit arity is
    reported intact. Validation passed C3 diagnostics, `c3c build main`,
    filtered advanced dispatch slice (`pass=255 fail=0`), file-size gate, and
    `git diff --check`.
  - Closed `AUDIT_2.md` M22/M23 REPL buffer boundary defects. `read_line`
    now returns before zero-capacity buffer underflow, and `repl_eval_buffer`
    rejects saturated buffers before writing a terminator at `buf_len`.
    Validation passed C3 diagnostics, `c3c build main`, basic Lisp slice
    (`pass=183 fail=0`), file-size gate, and `git diff --check`.
  - Closed `AUDIT_2.md` M24 REPL-server auth downgrade. Auth-required
    connections now remain auth-required even when token material is missing or
    empty, causing protected requests to fail closed instead of opening the
    transport. Direct basic validation passed (`pass=184 fail=0`); the broader
    async slice remains blocked by unrelated existing async/file/coroutine
    failures (`pass=90 fail=10`).
  - Re-audited `AUDIT_2.md` M25 as stale-closed: the worker queue clear helper
    already locks the initialized worker mutex. Closed M26-M28 by widening the
    shared `is_number()` helper to the full numeric domain, routing
    `to_double()` through `try_numeric_to_double()`, moving Float64-only UI/test
    consumers to direct checked conversion, and caching/validating
    `interp.tid_Float` under `Number`. Validation passed C3 diagnostics,
    `c3c build main`, basic Lisp slice (`pass=185 fail=0`), advanced stdlib
    numeric filter (`pass=441 fail=0`), advanced type-dispatch filter
    (`pass=255 fail=0`), file-size gate, and `git diff --check`.
  - Closed `AUDIT_2.md` M29. Type-registry hash insertion now uses a checked
    helper, and `register_type()` rolls back the just-published type slot plus
    `type_count` if a hashable name cannot be inserted into the hash index.
    Basic native coverage forces a malformed full type-registry hash index and
    verifies the failed registration is unreachable while the prior type stays
    reachable. Validation passed C3 diagnostics, `c3c build main`, basic Lisp
    slice (`pass=186 fail=0`), file-size gate, and `git diff --check`.
