# Active TODO Index Part 15

Source: `TODO.md`

This part backfills actionable items from:

- `AUDIT_REPORT_2026-04-21.md`
- `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md`
- `AGENTS.md` backlog, memory, verification, and churn rules

## 2026-04-21 Audit Remediation Backlog

- [x] `AUDIT-2026-JIT-TAIL-ERROR-ARGS` preserve error-valued arguments in
  tail multi-argument closure calls.
  - source: focused `basic` and `advanced-collections-module` regressions.
  - files: `src/lisp/jit_apply_helpers.c3`.
  - issue: the JIT escape cons helper treated an `ERROR` value in the argument
    car slot as an argument-list construction failure, so named-let tail calls
    with error-valued but unused bindings collapsed to `arg list too short`.
  - done 2026-04-21: aligned `jit_cons_escape(...)` with `make_cons(...)` so
    promoted `ERROR` cars are valid call-argument data while real promotion
    failures still propagate.
  - validation: direct named-let probes, `c3c build --obj-out obj`, basic Lisp
    slice `pass=166 fail=0`, advanced collections module `pass=1894 fail=0`,
    code file-size gate, and targeted `git diff --check`.

- [x] `AUDIT-2026-TENSOR-CONTRACT-ALIAS-ORIGIN` preserve tensor alias lineage
  across boundary clones for contract destination checks.
  - source: focused `advanced-collections-module` realize destination
    regressions.
  - files: `src/lisp/value_runtime_types.c3`, `src/lisp/value_tensor.c3`,
    `src/lisp/prim_tensor_matrix_lu_svd_core_b.c3`, and
    `src/lisp/prim_tensor_storage.c3`.
  - issue: contract-source alias checks relied on backing-storage pointer
    equality, but tensor boundary clones can deep-copy concrete storage and
    erase pointer identity before `realize` receives the lazy contract.
  - done 2026-04-21: added a compact `TensorVal.alias_origin` lineage stamp;
    new tensors get a fresh origin, boundary clones retain it, views inherit
    source origin, and contract destination checks compare lineage before raw
    storage pointers.
  - validation: direct contract alias probes returned
    `"contract: destination aliases source tensor"`, direct map-in-place probe
    still returned `5.0`, `c3c build --obj-out obj`, advanced collections
    module `pass=1894 fail=0`, code file-size gate, and targeted
    `git diff --check`.

- [x] `AUDIT-2026-C1-ADDRINFO-ABI` replace hardcoded `struct addrinfo`
  pointer offsets with a typed ABI-safe representation.
  - source: `AUDIT_REPORT_2026-04-21.md` C1.
  - files: `src/lisp/async_tcp_transport_helpers.c3`.
  - next: define a C3 struct or C helper/header boundary for real
    `addrinfo` layout and remove raw offset reads.
  - validation: networking tests or targeted TCP resolver smoke on the current
    host, plus `c3c build`.
  - done 2026-04-21: added typed C helper access for connect/render paths,
    removed C3 raw addrinfo/sockaddr offset reads, and covered real IPv4/IPv6
    resolver payload rendering. Validated with helper rebuild, async policy
    guard, async Lisp slice `pass=65 fail=0`, `c3c build`, file-size gate, and
    `git diff --check`.

- [x] `AUDIT-2026-C2-RELATION-VALUE-LIFETIME` fix dangling `Value*` storage in
  materialized deduce relations.
  - source: `AUDIT_REPORT_2026-04-21.md` C2.
  - files: `src/lisp/deduce_relation_row_materialization.c3`.
  - next: verify actual `Relation` ownership and either copy values into the
    relation-owned allocator or enforce a documented root-scope lifetime
    invariant with tests.
  - constraint: do not introduce per-type RC for ordinary Omni values; preserve
    region-centric ownership from `AGENTS.md`.
  - validation: deduce relation lifetime regression, ASAN build preferred.
  - done 2026-04-21: relation column-key cache now stores `SymbolId` values
    instead of long-lived root `Value*` pointers; row materialization creates
    root-backed dictionary symbol keys per output row. Validated with deduce
    query slice `pass=214 fail=0`, `c3c build`, file-size gate, and
    `git diff --check`. ASAN was not run in this slice.

- [x] `AUDIT-2026-H1-FORMAT-STRINGS` audit and fix C3 format string width
  mismatches.
  - source: `AUDIT_REPORT_2026-04-21.md` H1.
  - files include: `src/lisp/eval.c3`, `src/lisp/value_print_helpers.c3`,
    `src/lisp/value_print_buf.c3`, `src/lisp/tests_scheduler_groups.c3`,
    `src/lisp/tests_memory_lifetime_env_copy_groups.c3`,
    `src/lisp/eval_repl_server_state.c3`,
    `src/scope_region_temp_pool_stats.c3`,
    `src/entry_check_reporting.c3`, `src/lisp/value_constructors.c3`, and
    `src/lisp/scheduler_offload_ops.c3`.
  - next: replace `%d`/`%x` uses with width-correct formats or explicit safe
    casts, then add a lightweight regex gate if practical.
  - validation: `c3c build`, relevant print/reporting tests, diff whitespace.
  - done 2026-04-21: fixed the audit-listed format sites using C3-supported
    `%d`/`%x` forms after the compiler rejected C-style `%ld`, `%zu`, and
    `%u`. Validated with `c3c build`, compiler slice `pass=290 fail=0`, async
    slice `pass=65 fail=0`, bounded memory-lifetime smoke `pass=231 fail=0`,
    and `git diff --check`.

- [x] `AUDIT-2026-H2-VALUETAG-EXHAUSTIVE` remove hidden `default:` arms from
  core `ValueTag` switches and make handled cases explicit.
  - source: `AUDIT_REPORT_2026-04-21.md` H2 and `AGENTS.md` C3 rules.
  - files include: `src/lisp/eval.c3`, `src/lisp/eval_dispatch_types.c3`,
    `src/lisp/eval_env_copy_values.c3`,
    `src/lisp/eval_boundary_provenance.c3`,
    `src/lisp/eval_boundary_provenance_reachability.c3`,
    `src/lisp/prim_nn_checkpoint.c3`, `src/lisp/prim_tensor_capture.c3`, and
    `src/lisp/jit_define_method_table.c3`.
  - next: split into focused slices if needed, but each touched switch must
    name all current tags or document why it is not a `ValueTag` exhaustiveness
    surface.
  - validation: `c3c build`, targeted evaluator/boundary/checkpoint/tensor
    tests.
  - done 2026-04-21: replaced the audit-listed hidden `ValueTag` default arms
    with explicit tag cases in evaluator dispatch, env-copy, boundary
    provenance/reachability, checkpoint encoding, and JIT literal comparison.
    Left `prim_tensor_capture.c3` defaults untouched where they switch over
    tensor op ids or payload kinds rather than `ValueTag`. Validated with
    `c3c build`, compiler slice `pass=290 fail=0`, bounded container
    `memory-lifetime-smoke` `pass=232 fail=0`, scoped ValueTag default scan,
    and `git diff --check`.

- [x] `AUDIT-2026-H3-IGNORED-RETURNS` audit ignored non-void returns and make
  initialization/cleanup failures explicit.
  - source: `AUDIT_REPORT_2026-04-21.md` H3.
  - files include: `src/lisp/value_interp_init_helpers.c3`,
    `src/lisp/value_environment.c3`, `src/lisp/value_constructors.c3`,
    `src/lisp/eval_repl_server_state.c3`,
    `src/lisp/prim_tensor_construct.c3`, and
    `src/lisp/async_tcp_transport_helpers.c3`.
  - next: propagate errors in init/build paths; for cleanup-only calls, log or
    document why failure is intentionally non-fatal.
  - validation: `c3c build`, init/environment/tensor/network targeted tests.
  - done 2026-04-21: removed the audit-listed ignored returns. Interp macro
    and module init now leave empty recoverable storage on allocation failure,
    env hash-table construction/rebuild explicitly documents the linear-lookup
    fallback, FFI handle and REPL session cleanup failures log warnings, tensor
    default dtype inference now fails closed for invalid inferred data, and TCP
    listen setup closes/fails when `SO_REUSEADDR` setup fails. Validated with
    `c3c build`, basic slice `pass=161 fail=0`, async slice `pass=65 fail=0`,
    file-size gate, and `git diff --check`.

- [x] `AUDIT-2026-H4-REQUIRE-CONTRACTS` add `@require` preconditions to
  high-risk internal helpers.
  - source: `AUDIT_REPORT_2026-04-21.md` H4 and `AGENTS.md` C3 rules.
  - initial files: `src/lisp/prim_kernel_source.c3`,
    `src/lisp/prim_nn_checkpoint.c3`,
    `src/lisp/eval_boundary_provenance.c3`, and `src/lisp/eval.c3`.
  - next: start with non-null `Interp*`/`Value*` assumptions in boundary and
    source validation helpers; avoid broad mechanical churn without validation.
  - validation: `c3c build` and touched-area tests.
  - done 2026-04-21: added focused `@require` contracts to high-risk
    dereference helpers in evaluator falsiness, boundary alias traversal,
    kernel source validation/copy/runner helpers, and neural checkpoint
    encode/decode/load/save helpers. Kept null-tolerant boundary transfer
    probes contract-free where tests intentionally verify fail-closed null
    behavior. Validated with compile-only touched-file checks, `c3c build`,
    focused advanced collections `pass=1892 fail=0`, bounded container
    `memory-lifetime-smoke` `pass=232 fail=0`, and `git diff --check`.

- [x] `AUDIT-MEM-P1-SPLICE-REACHABILITY` add debug reachability validation for
  `scope_splice_escapes`.
  - source: `AUDIT_REPORT_2026-04-21.md` H5 and
    `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 3.3.
  - files: `src/scope_region_reset_adopt.c3` plus existing boundary graph walk
    helpers.
  - next: after ESCAPE chunks are spliced and before TEMP teardown, verify in
    debug builds that committed ESCAPE roots do not retain reachable TEMP
    edges.
  - validation: boundary-hardening tests and ASAN build preferred.
  - done 2026-04-21: added a policy-gated pre-splice graph audit at the
    root-aware boundary splice layer, immediately before the raw
    `scope_splice_escapes` transfer. The raw `main` scope layer has no Lisp
    root graph to traverse, so committed-root reachability validation lives at
    the boundary call site and rejects releasing-scope TEMP edges before
    adoption. Added a memory-lifetime smoke regression that enables graph
    audit, constructs a releasing ESCAPE root with a TEMP edge, and verifies
    splice rejection with `BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID`.
    Validated with `c3c build` and bounded container `memory-lifetime-smoke`
    `pass=233 fail=0`. ASAN was not run.

- [x] `AUDIT-2026-H6-BOUNDED-CSTR` replace unbounded raw C string length scans
  with bounded or slice-based helpers.
  - source: `AUDIT_REPORT_2026-04-21.md` H6.
  - files: `src/lisp/eval_repl_server_state.c3`,
    `src/entry_runtime_project_paths.c3`.
  - next: add max-length-aware helpers based on the known allocation or call
    contract.
  - validation: `c3c build`, REPL/project-path targeted smokes.
  - done 2026-04-21: project-path and REPL-server C string scans now use
    bounded helpers over fixed buffer/line limits. Unterminated or overlong
    inputs fail closed through existing project-path or auth/host rejection
    paths. Validated with `c3c build`, async slice `pass=65 fail=0`,
    project-path smoke, REPL-server stdio smoke, file-size gate, and
    `git diff --check`.

- [x] `AUDIT-2026-H7-OPTIONAL-UNWRAP` remove or justify the force unwrap in
  global scope guard initialization.
  - source: `AUDIT_REPORT_2026-04-21.md` H7.
  - file: `src/scope_region_global_guards.c3`.
  - shipped: `g_scope_global_mu.init()` now catches initialization failure and
    aborts with a named invariant failure instead of using `!!`; continuing
    without the global scope lock would make shared region state unsound.
  - validation: `c3c build`; `git diff --check` scoped to touched files.

- [x] `AUDIT-2026-M1-DUPLICATION-CONSTANTS` extract repeated constants and
  duplicate helper bodies where it reduces real drift.
  - source: `AUDIT_REPORT_2026-04-21.md` M1.
  - examples: GELU constants, optimizer epsilon defaults, eval error
    constructors, TCP/HTTP helper raises, and deduce tuple codecs.
  - next: prioritize constants used in numerical behavior before cosmetic
    helper cleanup.
  - validation: affected ML/eval/deduce/network tests.
  - done 2026-04-21: added shared ML constants for GELU approximation and
    optimizer defaults; added shared async I/O constants and fiber/loop error
    helpers for TCP/HTTP; merged `eval_error` and `eval_error_expr` through one
    location-aware constructor; moved tuple encoded-size ownership into the
    tuple codec and corrected symbol tuple sizing to match the encoder's tag
    plus 4-byte id. Validated with `c3c build`, basic slice `pass=150 fail=0`,
    async slice `pass=65 fail=0`, deduce query slice `pass=214 fail=0`,
    focused advanced collections `pass=1892 fail=0`, and `git diff --check`.

- [x] `AUDIT-MEM-P0-DEFER-MULTI-RESOURCE` refactor multi-resource allocation
  paths to use `defer` cleanup guards.
  - source: `AUDIT_REPORT_2026-04-21.md` M2 and
    `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 1.3.
  - files: `src/lisp/value_tensor_clone.c3`,
    `src/lisp/eval_promotion_copy_route_helpers.c3`, and
    `src/lisp/eval_promotion_escape_structured.c3`.
  - next: introduce small cleanup structs where needed so future edits cannot
    leak partial allocations.
  - validation: targeted tensor clone and boundary promotion tests; ASAN build
    strongly preferred.
  - done 2026-04-21: converted tensor payload cloning and shared wrapper parent
    copy/escape promotion paths to scoped `defer` cleanup guards with explicit
    commit flags. The boundary wrapper guards now track copied array items,
    processed hashmap slots, copied method-table entries, and fallback
    ownership so failed child copy/promotion and result-wrapper allocation
    paths converge through one cleanup point. This also makes method-table
    implementation-copy failures clean the already-copied signature for the
    current entry. Validated with `c3c build`, bounded container
    `memory-lifetime-smoke` `pass=233 fail=0`, and `git diff --check`.
    `c3c build --sanitize=address` was attempted but the local toolchain
    rejected sanitizer mode before building.

- [x] `AUDIT-2026-M3-FOREACH-LOOPS` convert practical manual index loops to
  `foreach`.
  - source: `AUDIT_REPORT_2026-04-21.md` M3 and `AGENTS.md` C3 rules.
  - initial files include: `src/lisp/prim_kernel_source.c3`,
    `src/lisp/prim_tensor_capture.c3`, `src/lisp/prim_nn_checkpoint.c3`,
    boundary provenance helpers, JIT method-table helpers, and
    `src/lisp/value_print_buf.c3`.
  - next: convert only loops where index semantics are not needed; keep indexed
    loops where offsets are part of the logic.
  - validation: touched-area tests and `c3c build`.
  - done 2026-04-21: converted practical non-semantic index loops to
    `foreach` in print buffering, Kernel source validation/copying, Tensor
    capture metadata scans, NN checkpoint array/dictionary/string scans, and
    closure-env self-reference scanning. Left paired-array method-signature
    logic, reverse list reconstruction, tensor shape presentation, and
    offset/count-sensitive loops indexed. Validated with `c3c build`, basic
    slice `pass=150 fail=0`, focused advanced collections module
    `pass=1892 fail=0`, a model checkpoint round-trip `--eval` probe, and
    `git diff --check`. During validation, `[..len]` pointer-slice foreach was
    found to include one extra element for these uses; the landed code uses
    count-style `[0:len]` slices.

- [x] `AUDIT-2026-M4-GLOBAL-STATE-CONTRACTS` document or synchronize global
  mutable runtime state.
  - source: `AUDIT_REPORT_2026-04-21.md` M4.
  - files include: `src/lisp/scheduler_state_offload.c3`,
    `src/lisp/jit_compiler_state_pool.c3`,
    `src/lisp/jit_compiler.c3`, symbol/type/interp fault-injection
    globals.
  - next: distinguish single-threaded-by-design globals from globals needing
    mutex/tlocal protection; make `g_jit_compile_nonce_counter` thread-safe or
    `tlocal` if appropriate.
  - validation: scheduler/JIT targeted tests and concurrency stress where
    relevant.
  - done 2026-04-21: documented scheduler global state as owner-thread
    fiber/uv-loop state with offload cross-thread handoff limited to existing
    mutex/condition-variable and wakeup queues; documented JIT runtime globals
    and state pool as owner-thread protected by `jit_require_owner_thread`;
    centralized `g_jit_compile_nonce_counter` mutation behind
    `runtime_backend_next_compile_nonce`; and documented symbol/type/interp
    allocation fault-injection globals as feature-gated, test-only,
    single-threaded controls. Validated with `c3c build`, compiler slice
    `pass=290 fail=0`, scheduler slice `pass=113 fail=0`,
    `scripts/check_jit_env_scope_guards.sh`,
    `scripts/check_scheduler_state_guards.sh`, and `git diff --check`.

- [x] `AUDIT-2026-M5-MODULE-CYCLE` break or explicitly isolate the
  `main` <-> `lisp` module cycle.
  - source: `AUDIT_REPORT_2026-04-21.md` M5.
  - next: map which `src/lisp/` imports require `main`, then extract shared
    runtime contracts into a neutral module if feasible.
  - validation: `c3c build`, manifest/source parity checks.
  - done 2026-04-21: documented the current cycle as an explicit isolation
    contract in `docs/plans/main-lisp-module-cycle-isolation-2026-04-21.md`.
    Exact mapping found 22 `module main` adapter/test files importing `lisp`
    and 413 `src/lisp` files importing `main` for runtime services. The
    allowed `main -> lisp` side is limited to entry, CLI, REPL, build, bindgen,
    source-check, and test adapters; the allowed `lisp -> main` side is
    limited to low-level runtime services still hosted in `main`. A real break
    is recorded as a separate neutral-runtime-module extraction, not a leaf
    import reshuffle. The required Stage 3 source parity check exposed two
    earlier helper files missing from the AOT Lisp runtime manifests, so
    `src/entry_build_runtime_manifest_lisp_part0.c3` now includes
    `src/lisp/async_io_shared.c3` and
    `src/entry_build_runtime_manifest_lisp_part3.c3` now includes
    `src/lisp/prim_ml_constants.c3`. Validated with `c3c build`, Stage 3
    source parity, importer scans, and `git diff --check`.

- [x] `AUDIT-2026-M6-BUILD-CONFIG` clean up build configuration gaps.
  - source: `AUDIT_REPORT_2026-04-21.md` M6.
  - file: `project.json`.
  - scope: include `third_party/ftxui/include` if required, represent helper
    archive build prerequisites, remove hardcoded `/usr/local/lib` where
    practical, and consider a separate test target.
  - validation: helper build, `c3c build`, docs/build smoke.
  - done 2026-04-21: made `project.json` complete for the current helper
    build surface by adding FTXUI include roots and the C/Vulkan ML/helper
    sources that were present in `scripts/build_omni_chelpers.sh` but absent
    from the project manifest. Added
    `scripts/check_build_config_parity.sh` to keep helper sources, FTXUI
    include dirs, and library-path override hooks from drifting. Replaced
    hardcoded e2e/AOT host library search paths with
    `OMNI_RUNTIME_TOOLCHAIN_LIB_PATH` and `OMNI_AOT_LINK_LIBRARY_PATH`
    overrides while keeping `/usr/local/lib` as the fallback for existing
    installs. Kept test-target separation in scripts/CI instead of adding a
    second C3 target because the repo currently has one executable target and
    test entry wiring is runtime-selected. Validated with JSON parsing, helper
    build, build-config parity script, `c3c build`, Stage 3 source parity,
    FTXUI smoke, CLI help smoke, and `git diff --check`. Container e2e
    compile-only was attempted but the validation image failed before repo
    build with `/usr/local/bin/c3c: cannot execute binary file: Exec format
    error`.

- [x] `AUDIT-2026-M7-FAULT-INJECTION-GLOBALS` gate test-only fault-injection
  globals out of release builds.
  - source: `AUDIT_REPORT_2026-04-21.md` M7.
  - files: `src/lisp/value_symbol_table.c3`,
    `src/lisp/value_type_registry.c3`, `src/lisp/value_interp_lifecycle.c3`.
  - next: wrap in an existing debug/test build flag or move to a test-only
    module without losing deterministic failure coverage.
  - validation: fault-injection tests and release build smoke.
  - done 2026-04-21: gated symbol-table, type-registry, and interp lifecycle
    fault-injection globals behind `OMNI_TEST_FAULT_INJECTION`; normal builds
    compile production allocation probes to inactive helpers while feature
    builds keep deterministic failure coverage. Validated with normal
    `c3c build`, normal basic slice `pass=150 fail=0`, feature build
    `c3c -D OMNI_TEST_FAULT_INJECTION --obj-out obj-fi build`, feature basic
    slice `pass=161 fail=0`, normal-binary `nm` check showing no exported
    `g_symbol_table_force_*`, `g_type_registry_force_*`, or `g_interp_force_*`
    symbols, and `git diff --check`.

- [x] `AUDIT-2026-L1-INVALID-SYMBOL-ID` replace raw `0xFFFFFFFF` symbol-id
  sentinels with `INVALID_SYMBOL_ID`.
  - source: `AUDIT_REPORT_2026-04-21.md` L1.
  - files include compiler callable/free-var/mutable-capture/JIT closure
    helpers listed in the audit.
  - validation: compiler slice and `c3c build`.
  - done 2026-04-21: replaced raw sentinel checks in the audit-listed compiler
    and JIT closure helper files with `INVALID_SYMBOL_ID`. Raw `0xFFFFFFFF`
    values outside the L1-listed symbol-id sites were left untouched. Validated
    with compiler slice `pass=290 fail=0`, `c3c build`, file-size gate, and
    `git diff --check`.

- [x] `AUDIT-2026-L2-JIT-FILE-NAMES` rename redundant double-JIT file prefixes
  if the project wants module/file naming cleanup.
  - source: `AUDIT_REPORT_2026-04-21.md` L2.
  - next: do this as a dedicated mechanical rename with manifest/build updates;
    avoid mixing with behavioral JIT changes.
  - validation: manifest source parity and `c3c build`.
  - done 2026-04-21: mechanically renamed every active double-prefix JIT C3
    file under `src/lisp/` to the single-prefix `jit_*.c3` form and updated
    active manifests, scripts, current docs/plans, and audit references. Stage
    3 source parity now names the new JIT paths and there are no remaining
    double-prefix JIT references in active source/script/planning surfaces. The
    rename also exposed two legacy `make_error` calls in migrated JIT effect
    surfaces, which were converted to canonical payloaded runtime errors.
    Validated with `c3c build`, compiler slice `pass=290 fail=0`, jit-policy
    slice `pass=51 fail=0`, Stage 3 source parity,
    `scripts/check_effects_contract_policy.sh`, and `git diff --check`.
    `scripts/check_boundary_hotpath_formatting.sh` still fails on debug
    `io::printfn` calls in `src/lisp/jit_eval_scopes.c3`; that is tracked by
    the open `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS` item.

- [x] `AUDIT-MEM-P1-STRUCT-SIZE-ASSERTS` add compile-time `$assert`s for
  interop-critical runtime struct sizes.
  - source: `AUDIT_REPORT_2026-04-21.md` L3 and
    `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 3.1.
  - structs include: `Value`, `ScopeRegion`, `Closure`, `TensorVal`,
    `HashMap`, `Array`, `Env`, `Interp`, `Primitive`, `SymbolTable`, and
    `MethodTable`.
  - next: measure current sizes first, then add assertions with comments
    explaining the ABI/interop reason.
  - validation: `c3c build`.
  - done 2026-04-21: added compile-time `$assert`s for current measured sizes:
    `Value` 56, `ScopeRegion` 144, `Closure` 88, `TensorVal` 296, `HashMap`
    24, `Array` 24, `Env` 136, `Interp` 2288, `Primitive` 88, `SymbolTable`
    40, and `MethodTable` 40. Validated with `c3c build`, file-size gate, and
    `git diff --check`.

- [x] `AUDIT-2026-L4-MAGIC-NUMBERS` replace selected hardcoded magic numbers
  with named constants.
  - source: `AUDIT_REPORT_2026-04-21.md` L4.
  - files include: `src/lisp/prim_math.c3`, `src/lisp/prim_nn_init.c3`,
    `src/stack_engine_backend_contract.c3`,
    `src/lisp/prim_collection_hashmap_key_helpers.c3`,
    `src/lisp/async_tcp_transport_helpers.c3`, and `src/lisp/http.c3`.
  - validation: touched-area tests and `c3c build`.
  - done 2026-04-21: named the remaining audit-listed raw values in
    `prim_math.c3`, `prim_nn_init.c3`,
    `stack_engine_backend_contract.c3`, and
    `prim_collection_hashmap_key_helpers.c3`. The async TCP and HTTP buffer
    sizes had already been moved to `src/lisp/async_io_shared.c3` during the
    build-manifest waves. Validated with `c3c build`, basic slice
    `pass=150 fail=0`, async slice `pass=65 fail=0`, focused advanced
    collections `pass=1892 fail=0`, stack suite `pass=24 fail=0`, and
    `git diff --check`.

- [x] `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS` verify debug-only blocks do not
  hide required release side effects.
  - source: `AUDIT_REPORT_2026-04-21.md` L5.
  - files: `src/lisp/jit_compiler_compile.c3`,
    `src/lisp/runtime_backend_hooks_cache.c3`.
  - next: inspect `$if DEBUG_BUILD:` blocks and move any required state updates
    outside debug conditionals.
  - validation: `c3c build`, JIT/cache targeted tests if behavior changes.
  - done 2026-04-21: verified the audit-listed `DEBUG_BUILD` blocks only gate
    diagnostics while required JIT GC scheduling, cache clear/retry, cache
    commit, and warning-state mutations remain in release code. Added
    `scripts/check_debug_build_side_effects.sh` to guard that invariant. Also
    moved TCO scope trace formatting out of hot `src/lisp/jit_eval_scopes.c3`
    into `src/lisp/jit_eval_scope_trace.c3`, restoring the boundary hot-path
    formatting guard without removing the opt-in trace. Validated with
    `c3c build`, jit-policy slice `pass=51 fail=0`, Stage 3 source parity,
    `scripts/check_debug_build_side_effects.sh`,
    `scripts/check_boundary_hotpath_formatting.sh`, and `git diff --check`.

## 2026-04-21 Memory Architecture Improvement Backlog

- [x] `MEMORY-P0-PROMOTION-LEAKS` fix confirmed promotion/escape leak paths
  A-D.
  - source: `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 1.1.
  - files: `src/lisp/eval_promotion_copy_wrapper_helpers.c3`,
    `src/lisp/eval_promotion_escape_structured.c3`,
    `src/lisp/eval_boundary_commit_escape_cons.c3`.
  - scope: closure wrapper clone failure, cons promotion budget abort,
    destination cons escape partial failure, and partial closure promotion
    cleanup.
  - constraint: preserve region-centric ownership; do not introduce per-type RC
    for ordinary language values.
  - validation: boundary promotion tests and ASAN build strongly preferred.
  - done 2026-04-21: added cleanup for partial closure wrapper cloning,
    promoted cons abort paths, destination cons escape aborts, and partial
    closure promotion env-scope ownership. Validated with bounded container
    `memory-lifetime-smoke` `pass=231 fail=0`, `c3c build`, file-size gate, and
    `git diff --check`. ASAN was not run in this slice.

- [x] `MEMORY-P0-SPLICE-SCOPE-GEN` fix stale `scope_gen` stamps after
  `scope_splice_escapes`.
  - source: `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 1.2.
  - files: `src/scope_region_reset_adopt.c3`,
    `src/lisp/eval_promotion_copy.c3`.
  - next: prefer rewriting stamps while walking spliced ESCAPE chunks, unless
    measurement or invariants require a splice epoch design.
  - validation: copy-site fast-reuse tests, boundary tests, ASAN build
    preferred.
  - done 2026-04-21: added a root-aware escape-splice transfer path that walks
    the committed root graph before adoption, rewrites child ESCAPE value/env
    generation stamps to the parent ESCAPE generation, updates closure/env
    owner-scope metadata, and rejects the splice if graph traversal fails.
    Commit paths now use the root-aware helper; the legacy rootless helper
    remains for direct transfer probes. Added a memory-lifetime smoke
    regression for adopted root/leaf/tail generation stamps. Validated with
    `c3c build` and bounded container `memory-lifetime-smoke` `pass=232
    fail=0`. ASAN was not run in this slice.

- [x] `MEMORY-P1-TELEMETRY` add lightweight runtime memory telemetry and user
  inspection.
  - source: `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 3.2.
  - scope: TEMP/ESCAPE allocation totals, freed/spliced bytes,
    copy-to-parent/promotion bytes, scope create/recycle counts, and fiber-temp
    hit/miss counts.
  - surface: `runtime-memory-stats` primitive and optional
    `OMNI_MEM_TELEMETRY=1` process-exit dump.
  - completed 2026-04-21: added cumulative scope telemetry counters, structured
    `runtime-memory-stats` dictionaries, `OMNI_MEM_TELEMETRY=1` JSON teardown
    output, and basic/native telemetry regressions.
  - validation: `c3c build`, direct `--eval '(runtime-memory-stats)'`,
    `OMNI_MEM_TELEMETRY=1 --eval '1'`, basic Lisp slice `pass=152 fail=0`,
    and Stage 3 source parity.

- [x] `MEMORY-P2-JIT-ESCAPE-OPCODES` add lane-specific JIT allocation helpers
  for tail-position return construction.
  - source: `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 2.1.
  - files: `src/lisp/jit_apply_helpers.c3`,
    `src/lisp/jit_compile_expr_core.c3`, and JIT tests.
  - scope: `jit_cons_escape`, `jit_make_array_escape`, and
    `jit_make_string_escape` when escape allocation is proven.
  - completed 2026-04-21: added ESCAPE-lane JIT helpers for nil, cons, array,
    and string literals; lowered tail `cons`, `List`/`list`, `Array`, and tail
    string literals to the escape variants; kept non-tail and shadowed
    constructor calls on the normal apply path.
  - validation: `c3c build`, focused
    `OMNI_JIT_POLICY_FILTER=tail-constructor-escape-opcode` JIT policy slice
    `pass=1 fail=0`, full JIT policy slice `pass=52 fail=0`, and direct
    `runtime-memory-stats` eval.

- [x] `MEMORY-P2-TCO-LANE-RESET` recycle TCO call scopes by resetting the TEMP
  lane instead of recreating scopes.
  - source: `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 2.2.
  - files: `src/lisp/jit_eval_scopes.c3`,
    `src/scope_region_reset_helpers.c3`.
  - next: add `scope_reset_temp_lane(ScopeRegion*)` and route eligible TCO
    bounces through it while preserving ESCAPE bindings.
  - completed 2026-04-21: verified the existing implementation already has
    `scope_reset_temp_lane(ScopeRegion*)` and routes eligible TCO bounces
    through `runtime_prepare_tco_recycle_env`, preserving ESCAPE lane chunks and
    dtors while resetting TEMP. Guard paths still allocate/copy into a fresh
    scope when surviving env frames, root-persistent boxes, or TEMP-reachable
    binding graphs would make a fast TEMP reset unsafe.
  - validation: `scope` suite `63 passed, 0 failed`; JIT policy slice
    `pass=52 fail=0`; bounded container `tco-recycling` slice `pass=11
    fail=0`; bounded container memory-lifetime smoke with local aarch64
    toolchain mounted: `pass=233 fail=0`.

- [x] `MEMORY-P2-TCO-ENV-REUSE` reduce hot-path `copy_to_parent` calls in TCO
  environment-copy paths.
  - source: `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 2.3.
  - files: `src/lisp/eval_env_copy_values.c3` and JIT env-copy helpers.
  - next: detect source envs already in the target scope chain or root and
    reuse directly where ownership invariants allow.
  - completed 2026-04-21: added shared env-frame reuse guards for generic
    boundary env-copy and JIT TCO env-copy. Reuse is allowed only when the
    frame is already in the target scope chain or root, the parent can be kept
    without rewrite, and every binding value is reusable under the existing
    boundary provenance rules. Root-persistent boxes keep their existing
    parent-rewrite path, and unsafe or releasing-scope frames still
    materialize a replacement frame.
  - validation: `c3c build`; bounded container memory-lifetime smoke with
    local aarch64 toolchain mounted: `pass=234 fail=0`; bounded container
    `tco-recycling` slice: `pass=11 fail=0`.

- [x] `MEMORY-P3-INLINE-SMALL-COLLECTIONS` evaluate typed arrays and inline
  capacity for small collections.
  - source: `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 4.1.
  - scope: arrays/dicts up to a small inline threshold.
  - constraint: high-risk pervasive representation change; requires design
    note, benchmarks, and broad tests before implementation.
  - validation: array/dict suite, JIT array tests, memory telemetry.
  - completed 2026-04-21: evaluated in
    `docs/plans/memory-tier3-evaluation-2026-04-21.md`. Literal inline storage
    in `Value` is rejected without benchmark evidence because it would break
    the current 56-byte `Value` ABI and pointer-backed collection payload
    contract. If future telemetry justifies code, the first implementation
    boundary is an `Array` payload-level small-buffer pilot with `Value`
    unchanged; dict/set and cons-list representations remain separate design
    items.

- [x] `MEMORY-P3-SCOPE-PAGING` evaluate TEMP lane size classes and scope paging.
  - source: `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 4.2.
  - next: use telemetry to decide whether 4KB/16KB/64KB chunk classes improve
    fragmentation for cons-heavy workloads.
  - validation: memory telemetry comparison and boundary tests.
  - completed 2026-04-21: evaluated in
    `docs/plans/memory-tier3-evaluation-2026-04-21.md`. Size-class paging is
    mechanically plausible as an internal TEMP allocator policy, but should not
    ship before class-level telemetry shows chunk capacity distribution, wasted
    bytes at release/reset, and fiber-temp pool behavior by capacity. Current
    runtime-memory telemetry is sufficient to begin measurement, not to bless a
    new paging policy.

- [x] `MEMORY-P3-PER-THREAD-SCOPE-POOLS` evaluate per-thread scope pools for
  concurrent workloads.
  - source: `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` Tier 4.3.
  - next: design thread-local scope pools and cross-thread transfer policy
    before implementation.
  - validation: scheduler/concurrency stress tests and memory telemetry.
  - completed 2026-04-21: evaluated in
    `docs/plans/memory-tier3-evaluation-2026-04-21.md`. Same-owner-thread
    thread-local recycle caches are feasible as a future performance
    experiment, but cross-thread scope mobility is blocked by the current
    owner-thread scope invariant and scheduler contract. Future work must first
    measure `scope_global_lock` contention and define explicit transfer,
    generation, teardown, ASAN, and telemetry semantics.

- [x] `AUDIT-2026-FFI-ASYNC-OFFLOAD-LIFETIME-FLOAT32` fix FFI async offload
  context ownership and Float32 return storage.
  - source: 2026-04-21 continuation audit and subagent memory/runtime review.
  - files: `src/lisp/prim_ffi_async.c3`,
    `src/lisp/eval_ffi_bound_call.c3`, scheduler offload ownership helpers,
    and `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3`.
  - completed 2026-04-21: added owned custom offload context release hooks so
    heap FFI async call contexts are released after worker execution or on
    admission/cancellation/error paths instead of being freed immediately at
    the call site while queued work still points at them.
  - completed 2026-04-21: changed sync and async FFI Float32 return storage to
    use real `float` storage instead of writing Float32 bits through a `long`
    and converting that integer value back to Float32.
  - follow-up 2026-04-21: factored sync variadic C ABI argument inference into
    `ffi_pack_variadic_call_arg(...)` and reused it from `ffi-async-call` so
    async variadic calls no longer treat extra arguments as raw pointers.
  - follow-up 2026-04-21: async FFI string returns now cross the worker
    boundary through the scheduler shared-byte lane, null string returns map to
    `nil`, and pointer-like async returns fail closed instead of leaking raw
    addresses as integers.
  - validation: `c3c build --obj-out obj`; focused
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface`
    passed with `pass=109 fail=0`; targeted `git diff --check` passed.
