# Agent Plan Index Part 05

Source: `.agents/PLAN.md`

## Current ML Validation Checkpoint

Date: 2026-04-22

- `ML-VK-090` is closed for the current Vulkan ML validation gate.
- `ML-VK-090-001` is closed for CPU finite-difference coverage on the shipped
  `ml/grad` contracts.
- `ML-VK-090-002` is closed for no-hidden-CPU-fallback diagnostic probes on
  mixed Vulkan ML trees.
- `ML-VK-090-003` is closed for the first bounded ML validation entrypoint and
  benchmark fixture.
- `ML-VK-090-004` is closed for the explicit ML Vulkan operation-family
  availability-gate regression.
- Added central finite-difference regressions for:
  - `linear-mean-squared-error` input gradients;
  - `linear-softmax-cross-entropy` weight gradients.
- The numeric oracle goes through public loss primitives (`ml/linear`,
  `ml/mean-squared-error`, and `ml/cross-entropy`) instead of reusing the
  `ml/grad` implementation path.
- Added focused mixed Vulkan/CPU error-message probes for `ml/clip-gradients`
  and `ml/optimizer-step`, so hidden CPU fallback regressions cannot collapse
  into generic backend errors.
- Added `scripts/run_ml_validation_slice.sh`; it runs the focused advanced
  collections ML/Tensor group through the Docker validation path and asserts
  `OMNI_ML_BENCH=1` benchmark summaries for ML inference and optimizer
  training-step loops.
- Added a host-path regression that exercises both sides of the Vulkan gate:
  on Vulkan-visible hosts it ties Float32 ML operation-family capability bits
  to actual Float32 placement across linear, activation, reduction,
  normalization, attention, convolution/pooling, optimizer, and gradient
  clipping families, and runs a device-resident `ml/linear` smoke; on
  Vulkan-unavailable hosts it requires false ML Vulkan capability bits and
  fail-closed `tensor/backend-unavailable` for `to-device 'vulkan`.
- Future broader validation expansion should use a new item for the specific
  operation family or benchmark dimension instead of reopening `ML-VK-090`.
- Validation checkpoint:
  - `c3c build --obj-out obj`
  - focused `advanced-collections-module` with `pass=1901 fail=0`
  - bounded `scripts/run_ml_validation_slice.sh`: `pass=1884 fail=0`,
    `ml_inference_oracle inference_ok=128`, `ml_training_step_oracle
    train_ok=64`
  - `scripts/check_file_size_gate.sh`
  - targeted `git diff --check`

## Runtime Ownership Fallback Closure Checkpoint

Date: 2026-04-22

- `FALLBACK-RUNTIME-OWNERSHIP-001` is closed for the staged runtime ownership
  fallback cleanup boundary.
- The invalidated global scan deletion remains off the table:
  `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1` previously failed bounded
  `memory-lifetime-smoke` with `pass=215 fail=9`.
- `boundary_ptr_in_target_scope_chain_with_hint(...)` no longer bypasses the
  active promotion-context route cache for `pinned_gen == 0`.
- Zero-generation hinted lookups now cache under pinned generation `0`, using
  the same pointer/target-scope/target-generation key as pinned lookups.
- Added a smoke regression proving repeated zero-generation target-chain
  lookups scan once in one active promotion context.
- Removed dead non-hinted scope-chain helper surfaces after verifying all live
  callers use the hinted/cache-aware route.
- Routed detached environment write-barrier promotion through
  `copy_to_parent_with_fault(...)` instead of calling
  `copy_to_parent_by_route(...)` directly. Env writes now share the typed
  copy-fault path and promotion-context lookup behavior used by the rest of the
  boundary copy cleanup.
- Explicit target-scope scope-chain lookups now use
  `boundary_ptr_in_scope_chain_with_hint_cached(...)`, so destination partial
  child reuse checks and `boundary_classify_return_value(...)` share the active
  promotion-context route cache even when the target scope is passed directly
  instead of coming from `interp.current_scope`.
- Added a smoke regression proving repeated explicit-target zero-generation
  lookups scan once under one active promotion context.
- Removed dead unchecked `boundary_copy_to_parent_site_ctx(...)` and
  `copy_to_parent_site(...)` helper surfaces after moving remaining test-only
  callers to the checked result API.
- Removed the plain unchecked `copy_to_parent(...)` convenience after its
  remaining test-only callers moved to typed-fault helper paths.
- Live inventory now reports no `copy_to_parent(...)`,
  `boundary_copy_to_parent_site_ctx`, or `copy_to_parent_site` symbols under
  `src/lisp`.
- `scripts/check_boundary_facade_usage.sh` now blocks production
  `copy_to_parent_by_route(...)`, `boundary_copy_to_parent_site_ctx(...)`, and
  uncached `boundary_ptr_in_scope_chain_with_hint(...)` call sites outside the
  sanctioned boundary implementation files.
- Tensor expression-edge escape promotion now uses
  `boundary_promote_to_escape(...)` instead of calling `promote_to_escape(...)`
  directly.
- Validation checkpoint:
  - `c3c build --obj-out obj`
  - bounded `memory-lifetime-smoke` with `pass=236 fail=0`
  - `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`
  - counters-enabled bounded `memory-lifetime-smoke` with `pass=236 fail=0`
  - normal rebuild `c3c build --obj-out obj`
  - env-barrier checked-copy rebuild `c3c build --obj-out obj`
  - env-barrier bounded `memory-lifetime-smoke` with `pass=236 fail=0`
  - explicit-target scope-cache bounded `memory-lifetime-smoke` with
    `pass=237 fail=0`
  - explicit-target counters build `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`
  - explicit-target counters-enabled bounded `memory-lifetime-smoke` with
    `pass=237 fail=0`
  - explicit-target normal rebuild `c3c build --obj-out obj`
  - closure guard `bash scripts/check_boundary_facade_usage.sh`
  - closure rebuild `c3c build --obj-out obj`
  - closure bounded `memory-lifetime-smoke` with `pass=237 fail=0`
  - targeted `git diff --check`
  - `scripts/check_file_size_gate.sh`

## Current FTXUI C ABI Wrapper Checkpoint

Date: 2026-04-21

- `FTXUI-C-ABI-WRAPPERS-001` is now closed for the chosen
  generic-builder-first ABI contract; advanced wrapper families are explicit
  non-goals unless a concrete Omni-facing surface needs them.
- Added `omni_ftxui_component_handle_event(...)` as the deterministic
  non-interactive event-dispatch helper for component callback delivery.
- Added the previously documented-but-missing
  `omni_ftxui_component_wrap_action(...)` action callback wrapper.
- `component_handle_event` now preserves callback-side context failures instead
  of clearing them after `OnEvent`.
- Corrected the C3 callback struct mirror to use callback aliases directly
  rather than pointer-to-function-pointer fields.
- New focused regressions cover context/error lifecycle, screen
  create/control/post-event/exit, table and canvas render conversion, and
  event-handler plus action callback delivery.
- Remaining FTXUI backlog should move to the next intentional family:
  advanced widget options/callbacks, selection-style pixel callbacks/gradient
  decorators, captured-mouse/selection runtime APIs, or explicit overload
  wrappers where the generic builder is not enough.
- Validation checkpoint:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - bounded `advanced-ffi-system-surface` with `pass=102 fail=0`
  - `scripts/run_ftxui_smoke.sh`

## Current UI Facade Checkpoint

Date: 2026-04-21

- `UI-LIB-FACADE-001` is closed for the facade/module split and shipped
  public-surface coverage.
- The canonical `examples/libraries/ftxui/ui.omni` facade now loads the dotted
  `lib/ui/*` modules as its implementation source:
  `ui.nodes`, `ui.effects`, `ui.layout`, `ui.style`, `ui.runtime`, and
  `ui.ftxui`.
- `module_direct_smoke.omni` now verifies default-path dotted imports for those
  modules and is part of `scripts/run_ftxui_smoke.sh`.
- `ui.runtime` now owns dispatch only. The stale copied static evaluator was
  removed from runtime; `lib/ui/evaluate.omni` remains the isolated static
  evaluator owner.
- Residual runtime/backend execution semantics are split into
  `UI-LIB-RUNTIME-BACKEND-001`: make declarative effect-tree dispatch drive a
  concrete FTXUI backend lifecycle path instead of only lowering to Omni
  `signal` effects for handler/test capture.
- Validation checkpoint:
  - `c3c build --obj-out obj`
  - direct loads of `examples/libraries/ftxui/lib/ui/runtime.omni` and
    `examples/libraries/ftxui/lib/ui/evaluate.omni`
  - `scripts/run_ftxui_smoke.sh`

## Current UI Runtime Backend Checkpoint

Date: 2026-04-21

- `UI-LIB-RUNTIME-BACKEND-001` is closed for the non-interactive backend
  lifecycle path.
- Added hidden primitive `__ui-ftxui-dispatch`, exposed through
  `ui.ftxui.dispatch` and routed by `ui.runtime.dispatch_to`.
- The primitive requires a root `open_tree` with backend `ftxui`, creates a
  real FTXUI context/screen, lowers render nodes to FTXUI components, maps
  invalidate/post/close effects to C ABI screen operations, and destroys all
  state before return.
- `read_event_tree` explicitly fails closed in this non-interactive path.
- Residual interactive read/update/render work is split to
  `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001`.
- Validation checkpoint:
  - `c3c build --obj-out obj`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_backend_smoke.omni`
  - `scripts/run_ftxui_smoke.sh`
  - bounded `advanced-ffi-system-surface` with `pass=102 fail=0`
  - targeted `git diff --check`
  - `scripts/check_file_size_gate.sh`

## Current FTXUI ABI Wrapper Checkpoint

Date: 2026-04-21

- `FTXUI-C-ABI-WRAPPERS-001` is closed for the chosen generic-builder-first C
  ABI contract.
- Remaining raw upstream parity families are explicit non-goals until a
  concrete Omni-facing feature needs them: arbitrary selection pixel callbacks,
  gradient decorators, animated/richer widget options, active-screen access,
  captured-mouse exposure, `WithRestoredIO`, selection API callbacks, and
  one-function-per-upstream-overload parity.
- Added focused fail-closed coverage for the unsupported piped-input path:
  `screen_create(... handle_piped_input=true ...)` and
  `screen_set_handle_piped_input(... true ...)` must return
  `OMNI_FTXUI_STATUS_NOT_SUPPORTED`.
- Validation checkpoint: `c3c build --obj-out obj`, bounded
  `advanced-ffi-system-surface` with `pass=105 fail=0`,
  `scripts/run_ftxui_smoke.sh`, targeted `git diff --check`, and
  `scripts/check_file_size_gate.sh`.
- Next UI work should continue through
  `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001`, not by reopening raw ABI overload
  parity.

## Current Vulkan Graph-Capture Checkpoint

Date: 2026-04-21

- Closed the current open `ML-VK-080-027`, `ML-VK-080-028`,
  `ML-VK-080-029`, `ML-VK-080-030`, `ML-VK-080-031`,
  `ML-VK-080-032`, `ML-VK-080-033`, `ML-VK-080-034`, and
  `ML-VK-080-035` slices on concrete shipped boundaries.
- Shipped boundaries:
  - `ML-VK-080-027`: captured all-Vulkan `Float32` contract -> scalar-map*
    selected regions now route through a native executor with C-owned scratch
    and one final transferred output handle.
  - `ML-VK-080-028`: the same contract -> scalar-map* region now records the
    contract dispatch and scalar-tail dispatches into one Vulkan command
    buffer after command-batch metadata validation.
  - `ML-VK-080-029`: direct SPIR-V source dictionaries now have an explicit
    checked ABI, `source-scale-f32-v1`; omitted ABI remains compatible with
    that same scale ABI and any other ABI fails closed.
  - `ML-VK-080-030`: source-backed unary `Float32` kernels now execute through
    checked registered and direct word-array `source-unary-f32-v1` SPIR-V
    sources. Registered source uses `name 'map-unary-f32`; direct source must
    declare `abi 'source-unary-f32-v1`.
  - `ML-VK-080-031`: captured all-Vulkan `Float32` source -> direct
    transpose-view -> scalar-map* selected regions now route through a native
    view/scalar-chain executor with C-owned scratch, view-stride input
    metadata for the first dispatch, dense intermediate/output metadata, and
    one final transferred dense output handle.
  - `ML-VK-080-032`: that same direct transpose-view -> scalar-map* family now
    lowers only after command-batch metadata validation; malformed metadata
    skips the native path and falls back to serial graph replay.
  - `ML-VK-080-033`: direct SPIR-V source dictionaries now support a checked
    binary `Float32` ABI, `source-binary-f32-v1`, for storage2-output1 shaders.
    `kernel/run` validates entry, ABI, optional `storage2-output1-f32-v1`
    metadata, input placement, input/output shapes, and empty push dictionaries
    before dispatching supplied SPIR-V words through the native Vulkan helper.
  - `ML-VK-080-034`: selected-region runtime reuse now handles the concrete
    mixed source -> transpose-view plus dense source -> tensor-map ->
    scalar-map* graph family. Native execution owns intermediates and returns
    one final dense Vulkan output handle.
  - `ML-VK-080-035`: captured command-batch metadata for that same mixed
    transpose-view/dense-source graph family is now validated before native
    command-buffer lowering; invalid metadata falls back to serial graph replay.
- Churn correction:
  - the source/custom Kernel lane, selected-region runtime reuse lane, and
    command-buffer lowering lane accumulated repeated case-specific slices while
    preserving the same broader capability as a residual. The next checkpoint is
    no longer another narrow shape/ABI case.
- Consolidated closure:
  - `ML-VK-080-036` now defines the shared Kernel source/layout contract and
    selected-region planner output that command lowering consumes.
  - Checked direct scale/unary/binary SPIR-V Kernel sources share a
    `kernel-source-layout` metadata dictionary contract.
  - `tensor/capture(source)` records top-level `selected-region-plan`
    candidates, and `tensor/run(graph)` requires a matching selected-region
    candidate plus command-batch metadata before entering native
    selected-region executors.
  - Source-language compilation, arbitrary direct-SPIR-V descriptor schemas
    beyond checked scale/unary/binary ABIs, memory-plan-backed runtime reuse,
    arbitrary mixed schedules, and fused dispatch execution remain future
    capability boundaries, not active `ML-VK-080` residual children.
- Negative memory:
  - Do not inline large source-binary direct-SPIR-V execution fixtures in the
    long advanced collections harness. A standalone `--eval` probe for the
    optimized binary-add shader returned `[vulkan 9.0 16.0]`, but putting that
    runtime fixture after the direct scale/unary SPIR-V tests corrupted the
    long harness. Keep constructor/fail-closed suite coverage and use a
    standalone probe or future non-variadic fixture builder for runtime
    source-binary validation.
- Validation checkpoint:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - focused advanced collections module with `pass=1886 fail=0`
  - standalone source-binary direct-SPIR-V `--eval` probe returned
    `[vulkan 9.0 16.0]`
  - compiler slice with `pass=290 fail=0`
  - basic Lisp slice with `pass=161 fail=0`
  - primitive docs parity
  - Stage 3 e2e source parity
  - code file-size gate
  - `git diff --check`
  - after `ML-VK-080-036`: `c3c build`, focused advanced collections module
    with `pass=1892 fail=0`, primitive docs parity, Stage 3 source parity, code
    file-size gate, `git diff --check`, and open `ML-VK-080-0xx` TODO scan
    with no unchecked child items

## Current Next Checkpoint

Date: 2026-04-21

- Audit/proposal remediation wave 1 closed five part 15 items:
  - `AUDIT-2026-C1-ADDRINFO-ABI`
  - `AUDIT-2026-C2-RELATION-VALUE-LIFETIME`
  - `AUDIT-2026-H1-FORMAT-STRINGS`
  - `AUDIT-2026-L1-INVALID-SYMBOL-ID`
  - `MEMORY-P0-PROMOTION-LEAKS`
- Validation for the wave passed:
  - helper rebuild;
  - async fallback policy;
  - `c3c build`;
  - compiler slice `pass=290 fail=0`;
  - deduce query slice `pass=214 fail=0`;
  - async slice `pass=65 fail=0`;
  - bounded container memory-lifetime smoke `pass=231 fail=0`;
  - file-size gate;
  - `git diff --check`.
- Remaining audit/proposal work is still in `docs/todo_parts/todo_part_15.md`.
  A second remediation wave closed:
  - `AUDIT-2026-H3-IGNORED-RETURNS`
  - `AUDIT-2026-H6-BOUNDED-CSTR`
  - `AUDIT-2026-H7-OPTIONAL-UNWRAP`
  - `AUDIT-MEM-P1-STRUCT-SIZE-ASSERTS`
  A third remediation wave closed:
  - `AUDIT-2026-H2-VALUETAG-EXHAUSTIVE`
  - `AUDIT-2026-H4-REQUIRE-CONTRACTS`
  - `AUDIT-2026-M7-FAULT-INJECTION-GLOBALS`
  - `MEMORY-P0-SPLICE-SCOPE-GEN`
  A fourth remediation wave closed:
  - `AUDIT-MEM-P1-SPLICE-REACHABILITY`
  A fifth remediation wave closed:
  - `AUDIT-2026-M1-DUPLICATION-CONSTANTS`
  A sixth remediation wave closed:
  - `AUDIT-MEM-P0-DEFER-MULTI-RESOURCE`
  A seventh remediation wave closed:
  - `AUDIT-2026-M3-FOREACH-LOOPS`
  An eighth remediation wave closed:
  - `AUDIT-2026-M4-GLOBAL-STATE-CONTRACTS`
  A ninth remediation wave closed:
  - `AUDIT-2026-M5-MODULE-CYCLE`
  The required Stage 3 source parity check also restored AOT runtime manifest
  coverage for `src/lisp/async_io_shared.c3` and
  `src/lisp/prim_ml_constants.c3`.
  A tenth remediation wave closed:
  - `AUDIT-2026-M6-BUILD-CONFIG`
  An eleventh remediation wave closed:
  - `AUDIT-2026-L2-JIT-FILE-NAMES`
  A twelfth remediation wave closed:
  - `AUDIT-2026-L5-DEBUG-BUILD-SIDE-EFFECTS`
  A thirteenth remediation wave closed:
  - `AUDIT-2026-L4-MAGIC-NUMBERS`
  Historical note: the old "7 open items after wave 13" count is superseded.
  The proposal-scoped memory items listed below were later backfilled and
  closed; use the current TODO scan rather than this historical wave count.
- Historical next remediation targets, now closed:
  - `MEMORY-P1-TELEMETRY` for runtime memory telemetry visibility.
  - `MEMORY-P2-JIT-ESCAPE-OPCODES` for targeted JIT escape allocation support.
  - `MEMORY-P2-TCO-LANE-RESET` for explicit TCO TEMP lane reset support.
- Negative memory:
  - Do not apply C printf width spellings to C3 `io::printf`; `%ld`, `%zu`,
    and `%u` failed `c3c build`. Use C3-supported format grammar and validate
    with the compiler.
  - Do not run `memory-lifetime-smoke` host-local; use the bounded container
    validation path.
  - Do not convert interp macro/module initial allocation failures to
    assertions. Existing storage-hardening tests force those failures and
    expect recoverable empty storage.
  - Do not try to put Lisp graph reachability validation inside raw
    `main::scope_splice_escapes`; that layer has chunks, not committed Lisp
    roots. Run committed-root graph validation at the boundary splice call site
    before the raw scope transfer.
  - Tuple symbol encoded-size accounting must stay 5 bytes: one tag byte plus
    the four-byte symbol id. The old rule-signature size helper undercounted
    symbol literals as 3 bytes.
  - C3 `[..len]` pointer slices are not count-style for the runtime foreach
    loops touched in wave 7; use `[0:len]` when iterating exactly `len`
    elements from a pointer.
  - Do not add atomics or mutexes to JIT globals under the current contract:
    the JIT runtime and state pool are owner-thread-only and guarded by
    `jit_require_owner_thread`. Multi-thread JIT compilation is a separate
    runtime contract change.
  - Do not attempt to break the `main` <-> `lisp` cycle by moving random leaf
    imports. The current closure is an explicit isolation contract; a physical
    break must start with a neutral runtime module for shared scope/stack and
    lifecycle services.
  - Do not let project/build manifests drift from `scripts/build_omni_chelpers.sh`;
    run `scripts/check_build_config_parity.sh` when adding C helper sources or
    FTXUI include dependencies.
  - The active JIT file prefix is now `src/lisp/jit_*.c3`; do not reintroduce
    redundant double-prefix JIT paths in active source, scripts, manifests, or
    planning artifacts.
  - `scripts/check_boundary_hotpath_formatting.sh` is currently the sharp
    validation signal for keeping boundary hot paths free of formatting calls;
    L5 moved TCO scope trace formatting into `src/lisp/jit_eval_scope_trace.c3`.
  - `scripts/check_debug_build_side_effects.sh` guards that the JIT/cache
    `DEBUG_BUILD` blocks stay diagnostic-only.
  - `MEMORY-P1-TELEMETRY` is closed. Use `runtime-memory-stats` for structured
    runtime inspection and `OMNI_MEM_TELEMETRY=1` for teardown JSON. The current
    counters include scope TEMP/ESCAPE bytes, chunk acquire/free bytes,
    create/release/destroy/recycle counts, splice transfer bytes, fiber-temp
    hit/miss/return/drop counters, and outermost copy-to-parent allocation
    bytes.
  - `MEMORY-P2-JIT-ESCAPE-OPCODES` is closed for the proposal-scoped tail
    constructor surface. Tail `cons`, `List`/`list`, `Array`, and string
    literals now use JIT ESCAPE allocation helpers when the constructor name is
    the unshadowed global primitive. Do not reopen this item for non-tail
    constructors or shadowed names; those intentionally stay on normal apply.
  - `MEMORY-P2-TCO-LANE-RESET` is closed. `scope_reset_temp_lane` and
    `runtime_prepare_tco_recycle_env` already route safe TCO bounces through
    TEMP-lane reset while preserving ESCAPE. In this aarch64 environment,
    container memory validation needs
    `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local` because the default
    image toolchain is x86-64.
  - `MEMORY-P2-TCO-ENV-REUSE` is closed. Generic boundary env-copy and JIT TCO
    env-copy now reuse target/root-owned env frames only when the parent can be
    kept without rewrite and every binding value is reusable under the existing
    provenance checks. Do not weaken this into value-copy skipping for
    releasing-scope bindings; materialization remains the required path when a
    frame or binding graph still depends on the releasing scope.
  - All proposal-scoped `MEMORY-*` TODO items in
    `docs/todo_parts/todo_part_15.md` are now closed. The Tier 3 memory items
    are evaluated in `docs/plans/memory-tier3-evaluation-2026-04-21.md` and
    should not be reopened as direct implementation work without measurement:
    collection length/capacity telemetry for small-buffer arrays, chunk-class
    fragmentation telemetry for scope paging, and scope global-lock contention
    telemetry plus an explicit transfer contract for per-thread scope pools.
  - `OWNERSHIP-HARDENING-001` is closed. `docs/ARCHITECTURE.md` now has the
    accepted scope/region ownership ADR, and `docs/C3_STYLE.md` has the
    ownership-boundary checklist plus durable graph, handle/ID, opaque payload,
    and static allocation-routing rules. Treat these docs as the review entry
    point before adding new graph-carrying runtime value shapes.
  - `META-PLAN-TODO-BACKFILL-001` is closed. The 2026-04-21 plan audit checked
    113 Markdown plan files under `docs/plans/` using local scans plus three
    parallel subagent review slices. New live TODO entries now track runtime
    file splitting, UI facade modules, FTXUI ABI contract/wrappers, REPL server
    multi-client queueing, the `uv` callback-handle prototype, runtime
    ownership fallback cleanup, and index/ref error-path parity. The aarch64
    stack backend plan was backfilled as closed because its plan already
    records runtime arm64 parity evidence.
  - `ACCESS-UNIFY-INDEX-REF-002` is closed. Runtime implementation already
    routes `ref` and postfix index negative paths through shared helpers; this
    pass added explicit parity and canonical postfix error-message tests in
    `src/lisp/tests_core_groups.c3`. Validation passed with
    `c3c build --obj-out obj` and direct rebuilt-binary probes. The broad basic
    slice still has unrelated tail multi-arg failures (`pass=164 fail=2`).
  - `RUNTIME-FILE-SPLIT-QUEUE-001` is closed as superseded. The old
    2026-03-19 plan queue was stale, the current owner rule is no file
    splitting below 1000 LOC, and current `src/` + `csrc/` inventory has no
    code file above 1000 LOC. Future splitting should start only from the
    threshold being exceeded or a real semantic ownership boundary, not from
    the stale largest-runtime-file plan.
  - `AUDIT_REPORT_2026-04-21.md` and
    `memory/MEMORY_IMPROVEMENTS_PROPOSAL.md` are historical audit/proposal
    inputs, not live backlog sources. Their implementation items were
    backfilled into `docs/todo_parts/todo_part_15.md`, and that part now has
    no open audit/memory-remediation checkboxes.
  - `AUDIT-2026-FFI-ASYNC-OFFLOAD-LIFETIME-FLOAT32` is closed. Async FFI call
    contexts are now scheduler-owned until worker completion/error cleanup,
    sync/async Float32 returns use real `float` return storage, async variadic
    calls reuse the sync C ABI inference helper, string returns materialize
    through the scheduler shared-byte lane, and pointer-like async returns now
    fail closed instead of leaking raw addresses as integers.
  - `ML-VK-080-HARDEN-001` is closed. Custom Kernel construction and Tensor
    graph capture/replay now fail closed for non-`main` source entries,
    both-sided scalar map metadata, and malformed map operands. Bare symbolic
    `source 'map-unary-f32` now validates and executes consistently with the
    dictionary `builtin-spirv` unary source form.
  - Current validation delta: `c3c build --obj-out obj`, focused FFI advanced
    surface `pass=109 fail=0`, advanced collections module `pass=1895 fail=0`,
    direct ML-VK fail-closed probes, code file-size
    gate, and targeted `git diff --check` passed.
  - `AUDIT-2026-JIT-TAIL-ERROR-ARGS` is closed. `jit_cons_escape(...)` now
    preserves promoted `ERROR` values as closure-call argument data when they
    occupy the cons car, matching the existing `make_cons(...)` contract.
  - `AUDIT-2026-TENSOR-CONTRACT-ALIAS-ORIGIN` is closed. Tensor payloads now
    carry `alias_origin` lineage so lazy contract destination checks survive
    boundary clones that deep-copy concrete storage.
  - Updated validation delta after the collections repair: `c3c build
    --obj-out obj`, direct named-let/error and contract-alias probes, basic
    Lisp slice `pass=166 fail=0`, focused advanced collections module
    `pass=1894 fail=0`, code file-size gate, and targeted `git diff --check`
    passed.
  - Negative-memory constraints: do not collapse closure argument lists just
    because the car is an `ERROR` value, and do not rely only on raw tensor
    storage pointer equality after region-boundary clones.
  - `FFI-CALLBACK-UV-001` is closed. The concrete `uv` timer callback-handle
    lane is already implemented and now has focused wrapper-allocation and
    finalizer teardown coverage proving retained callback owner scopes are
    released after shim registration failures and wrapper-scope destruction.
  - Current validation delta after the callback audit: `c3c build --obj-out
    obj`, focused advanced FFI surface `pass=72 fail=0`, and targeted `git
    diff --check` passed.
  - Historical audit target `FALLBACK-RUNTIME-OWNERSHIP-001` is now closed;
    future runtime ownership work should be filed as a new concrete boundary
    item with a failing signal rather than reopening the staged fallback
    umbrella.
  - `FALLBACK-RUNTIME-OWNERSHIP-001` current checkpoint: checked
    `copy_to_parent_with_fault(...)` missing-provenance calls now return a
    typed `BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE` instead of an `ERROR`
    value. Bounded `memory-lifetime-smoke` passes normally (`pass=234 fail=0`).
  - Additional route-specific fallback cleanup: scope-chain hint-miss fallback
    scans now reuse the already-approved scan gate instead of checking the scan
    budget twice in one logical lookup. Bounded `memory-lifetime-smoke` now
    passes with `pass=235 fail=0`.
  - Target-chain reuse cleanup: `boundary_classify_return_value(...)` now feeds
    its precomputed target-chain membership into
    `boundary_can_reuse_value_with_target_chain(...)`, avoiding a second
    target-chain scan inside the same classification decision while preserving
    the releasing-scope and alias-safety checks.
  - Current validation delta: normal `c3c build --obj-out obj`, counters build
    `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`, bounded
    counters-enabled `memory-lifetime-smoke` `pass=235 fail=0`, normal rebuild,
    and `scripts/check_file_size_gate.sh` passed.
  - Destination builder cleanup: unchecked
    `boundary_copy_to_parent_site_ctx(...)` calls for destination cons,
    partial, and iterator child-copy routes now use the checked facade and
    preserve destination error bubbling instead of aborting promotion before
    child-fault promotion can run. Validation passed with `c3c build --obj-out
    obj` and bounded `memory-lifetime-smoke` `pass=235 fail=0`.
  - Root clone cleanup: remaining root-store and root-clone unchecked
    `boundary_copy_to_parent_site_ctx(...)` call sites now use the checked
    facade and convert typed faults to explicit error values at the existing
    fail-closed clone boundary. The unchecked-wrapper inventory now reports
    only the wrapper definition. Validation passed with `c3c build --obj-out
    obj` and bounded `memory-lifetime-smoke` `pass=235 fail=0`.
  - Scope-chain route-specific cleanup: `boundary_can_reuse_value(...)` now
    computes target-chain membership before alias-safety traversal and passes
    that result to the shared reuse predicate, avoiding alias scans for values
    already outside the target chain. Validation passed with `c3c build
    --obj-out obj` and bounded `memory-lifetime-smoke` `pass=235 fail=0`.
  - Negative-memory constraint: do not globally bypass/delete scope-chain
    scans next. The bounded smoke run with
    `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1` failed `pass=215 fail=9`, so
    remaining scan cleanup needs route-specific replacement semantics.
  - `FTXUI-C-ABI-CONTRACT-001` is closed. The ABI shim plan now locks inbound
    string ownership, the generic-builder-first coverage target, and
    payload-free custom events. Continue FTXUI work through
    `FTXUI-C-ABI-WRAPPERS-001`, not by reopening the contract questions unless
    a concrete public API requirement invalidates them.
  - `REPL-SERVER-MULTICLIENT-001` is closed for transport concurrency. Unix
    socket and TCP listeners now hand accepted clients to detached handler
    threads and keep accepting. Residual same-stream request queueing is split
    into `REPL-SERVER-REQUEST-QUEUE-001`.
  - REPL validation delta: `c3c build --obj-out obj`, bounded `async` slice
    `pass=66 fail=0`, targeted `git diff --check`, and file-size gate passed.
  - `REPL-SERVER-REQUEST-QUEUE-001` is closed. Per-connection runtime work now
    uses an 8-entry FIFO queue with one active command, queue-full
    `protocol/server-busy` backpressure, active-only interrupt semantics, and
    stdin routing to active or queued `eval` / `load-file` request ids.
    Queued `clone` requests allocate their new session on the worker execution
    path to avoid request-thread session-table reallocation while active session
    pointers may be held.
  - Request-queue validation delta: `c3c build --obj-out obj` and bounded
    `async` slice `pass=69 fail=0`.
  - `ML-VK-060-014` is closed. `ml/sgd-step` now reuses the existing
    stateless CUDA/Vulkan dense row-major `Float32` SGD optimizer kernels for
    all-device parameter/gradient leaves, preserving device placement when
    `ml-optimizer-sgd-float32` is available and rejecting mixed placement
    without hidden transfers.
  - ML SGD validation delta: `c3c build --obj-out obj`, focused
    `advanced-collections-module` slice `pass=1897 fail=0`, compiler slice
    `pass=290 fail=0`, file-size gate, and `git diff --check` passed.
  - `ML-VK-020` is closed for the current neural elementwise, reduction,
    softmax, and loss kernel contract. The final residual was the scoped
    activation surface `ml/leaky-relu(input [negative-slope])`, now shipped for
    Float64/Float32 through composed Tensor map kernels with explicit narrow
    capability bits.
  - ML-VK-020 validation delta: `c3c build --obj-out obj`, focused
    `advanced-collections-module` slice `pass=1908 fail=0`, bounded
    `scripts/run_ml_validation_slice.sh` `pass=1891 fail=0` with
    `inference_ok=128` and `train_ok=64`, compiler slice `pass=290 fail=0`,
    primitive docs parity, file-size gate, and targeted `git diff --check`
    passed.
  - `ML-VK-050-004` is closed. `ml/grad` now supports CPU
    `linear-activation-mean-squared-error` `leaky-relu` backward for
    `Float64`/`Float32`, including default `negative-slope 0.01` and custom
    direct-spec slope validation. `nn/leaky-relu` now constructs a
    default-slope DataSpec and lowers through `ml/leaky-relu`.
  - `ML-VK-050-005` is closed. Current `ml/grad` results now include a
    versioned metadata-only `gradient-tape` dictionary owned by the normal
    scope-region value system. The tape records dtype/source/activation
    metadata and ordinary node dictionaries, sets `retains-handles` false, and
    does not own native handles or executor state.
  - `ML-VK-050-006` is closed. `ml/grad` now supports CPU dense row-major
    Tensor-expression MSE and softmax-cross-entropy specs over `map` and
    `contract` expression graphs, with single-leaf `wrt` disambiguation and
    unsupported map backward rules failing closed.
  - `ML-VK-050-007` is closed. Tensor-expression `ml/grad` now preflights
    non-CPU operands before `wrt` leaf matching, so Vulkan forward tensors
    without matching backward kernels fail closed under
    `tensor/backend-unsupported`; broad `ml-autograd` remains false.
  - Backlog reshaping checkpoint: broad parents `ML-VK-001`, `ML-VK-040`,
    `ML-VK-050`, `ML-VK-060`, `ML-VK-070`, and `ML-VK-080` are closed for
    their shipped semantic boundaries. Remaining real work is explicit under
    `ML-VK-040-FUSED-ATTENTION-001`, `ML-VK-050-VK-MAP-BWD-001`,
    and `ML-VK-060-FUSED-CUDA-001`.
  - `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001` is closed. `ui.loop` /
    `ui.ftxui.loop` now route effect trees through the real FTXUI app loop with
    exactly one render child and explicit fail-closed `read_event_tree`;
    session-owned external read/update/render semantics are split to
    `UI-LIB-RUNTIME-SESSION-001`.
  - `ML-VK-040-TRAIN-BN-001` is closed. `nn/batch-normalization` now provides
    explicit affine params and running-stat state, `nn/apply` uses running stats
    for inference/eval, and train-mode `nn/forward` returns updated ordinary
    state/model data after CPU dense current-batch stat computation.
  - `UI-LIB-RUNTIME-SESSION-001` is closed for the session-owned lifecycle
    contract. `ui.open_session`, `ui.update_session`, `ui.render_session`,
    `ui.invalidate_session`, `ui.post_event_session`, and `ui.close_session`
    now operate on an owned `ui-ftxui-session` handle backed by the FTXUI
    custom loop ABI.
  - `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001` is closed.
    `ui.read_event_session` now runs one blocking loop step and returns `nil`
    or a captured event dictionary with `kind` and optional `text` from the
    explicit FTXUI shim capture/read ABI.
  - `ML-VK-050-VK-BWD-001` is closed for the first native Vulkan backward
    kernel. `ml/grad` now supports dense row-major `Float32`
    `tensor-mean-squared-error` backward when `wrt` is the concrete prediction
    tensor; loss and input-gradient stay on Vulkan, and unsupported
    graph/map-expression backward remains fail-closed.
  - Negative constraint for the next Vulkan backward slice: do not assume the
    existing reverse graph walk can recover normal Vulkan `map` ancestry.
    Current Vulkan `map` eagerly materializes concrete device tensors and drops
    expression edges. `ML-VK-050-VK-MAP-BWD-001` should add explicit autograd
    capture/tape support or a graph-preserving `ml/grad` map path without
    changing global eager map semantics.
  - Validation delta: `c3c build --obj-out obj`; focused
    `advanced-collections-module` slice `pass=1924 fail=0`; FTXUI session
    smoke and `scripts/run_ftxui_smoke.sh`; file-size gate; `git diff --check`.
  - Current recommended next work: choose among
    `ML-VK-050-VK-MAP-BWD-001`, `ML-VK-040-FUSED-ATTENTION-001`, and
    `ML-VK-060-FUSED-CUDA-001`. Do not reopen closed ML/UI umbrella parents.

## Active CUDA Stateful Optimizer Closure Checkpoint

Date: 2026-04-22

- Active hypothesis:
  - The open CUDA optimizer residual was not a missing public optimizer
    contract; it was an execution-regime gap where Adam/AdamW/RMSProp validated
    CUDA placement but still decomposed state updates into multiple map-backed
    kernels.
- Current approach:
  - Close the residual at the native fused leaf boundary. Keep backend-neutral
    `ml/optimizer-step` unchanged, keep tree/schema validation in C3, and use
    native CUDA only after all operands are concrete dense row-major `Float32`
    CUDA tensors.
- Implemented checkpoint:
  - `ML-VK-060-FUSED-CUDA-STATEFUL-001` is closed.
  - `csrc/tensor_cuda_ml_optimizer.cu` now owns the CUDA optimizer PTX source.
  - The CUDA optimizer module resolves fused SGD, Adam/AdamW, and RMSProp
    kernels together.
  - Adam/AdamW now compute updated parameters plus first- and second-moment
    state in one native CUDA launch.
  - RMSProp now computes updated parameters, square-average state, and optional
    velocity state in one native CUDA launch.
- Validation path:
  - CUDA PTX generation and `ptxas` assembly passed.
  - C helper syntax check, helper-library rebuild, C3 build, file-size gate,
    direct CUDA optimizer probes, and focused advanced collections passed.
- Negative-memory constraints:
  - Do not silently fall back from a resolved native fused optimizer launch or
    allocation failure to map-backed execution. Map fallback is valid only when
    the native optimizer module is unavailable.
  - Do not reintroduce hand-written CUDA optimizer PTX as the source of truth;
    update `csrc/tensor_cuda_ml_optimizer.cu` and regenerate the PTX include.
- Next checkpoint:
  - Continue with the remaining explicit TODO items. The stateful CUDA
    optimizer residual should not be reopened unless a new failing runtime
    signal appears.

## Active Vulkan Math Performance Measurement Checkpoint

Date: 2026-04-22

- Active hypothesis:
  - The open `TENSOR-100F` SVD/eigen performance residuals should not trigger
    a tiled or staged Jacobi rewrite until in-process measurements show the
    current serial Jacobi/storage-scratch shape is the dominant cost.
- Current approach:
  - Use `scripts/run_vulkan_math_perf_probe.sh` as the reproducible local
    probe. It measures operations with Omni `time-ms` inside the runtime
    process and emits `OMNI_BENCH_SUMMARY` lines for 64/65 Vulkan fixtures.
- Validation path:
  - `bash -n scripts/run_vulkan_math_perf_probe.sh`
  - `git diff --check -- scripts/run_vulkan_math_perf_probe.sh`
  - `scripts/run_vulkan_math_perf_probe.sh`
- Checkpoint result:
  - The current default plus opt-in scale probe does not justify an algorithm
    rewrite. SVD measured 310-615 ms across 64/65 fixtures plus 96/128/192
    identity and 128 all-ones scale probes. Eigen measured 304-494 ms across
    64/65 fixtures plus 128 identity/all-ones scale probes.
  - Staged solve measured 303-326 ms across 65/128/192 identity and
    `I + ones` probes, so blocked trailing-update LU is not justified by the
    current data.
  - The measurement artifact is
    `docs/plans/vulkan-math-performance-measurements-2026-04-22.md`.
- Next checkpoint:
  - Treat the SVD/eigen and LU blocked-update performance residuals as closed
    for the current measured boundary. Open a new item only if larger repeated
    measurements at a named size show the current algorithms are the
    bottleneck.
- Negative-memory constraints:
  - Do not use process-per-expression `/usr/bin/time` results as crossover
    evidence for a rewrite; those timings include runtime startup. Use
    in-process `time-ms` summaries for the performance decision.

## Older Vulkan/Math Checkpoint

Use the integrated Tensor plan, `TODO.md`, and
`docs/plans/vulkan-math-library-roadmap-2026-04-17.md` as source of truth
before changing code.

1. Continue broader Vulkan math-library work. `TENSOR-100G` now has measured
   routing for the current staged `Float64` parallel solve path; full blocked
   trailing-update LU is a future performance lane only if later measurements
   justify it.
2. Keep the current serial Vulkan solve/LU/inverse shaders as
   correctness-preserving small-system/backend paths. The panel-factor staged
   solve path is the current large-system candidate at `n >= 65`.
3. Do not continue from the rolled-back generic `map` mode-3 unary branch:
   GLSL double transcendental builtins failed to compile for that path, and an
   arithmetic-only unary variant compiled but still failed at runtime. If unary
   Vulkan map is resumed, prefer a separately debugged unary shader/helper
   entrypoint or a deeper descriptor/dispatch diagnosis.
4. Treat mature serial helper plumbing as closed for now: compute-pipeline,
   descriptor, one-time command, and compatible `Float64` status-readback
   boilerplate are shared. Do not reopen those as the neutral baseline unless
   new duplication appears in a different helper shape.
5. Treat Vulkan `Float32` as partially active, not as a `Float64` fallback.
   Native CPU/runtime `Tensor Float32` exists, and Vulkan `Float32`
   placement/copyback, destination `realize`, dense row-major `map`, unary
   helpers, direct `min`/`max`, rank-N dense row-major `contract`, structural
   matrix kernels (`transpose`, `diagonal`, `diagonal-matrix`, `trace`), direct
   `matrix/rank`, direct `matrix/norm` selectors, `matrix/singular-values`,
   `matrix/svd`, serial factor/solve surfaces (`matrix/determinant`,
   `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and
   `matrix/qr`), staged parallel `matrix/solve`, capability reporting, and
   no-downcast tests, large-dense SVD robustness, and CPU `Float32`
   factor/SVD surfaces have landed. CUDA `Float32` placement/copyback,
   destination `realize`, eligible cuBLAS contract routing, supported
   zero-size CUDA contract identity/fill, CUDA rank-1/rank-1 dot, and
   zero-offset CUDA transpose-view operands for binary `map` have also landed.
   CPU fixed-width complex scalar and Tensor storage for `Complex128`/
   `Complex64` has landed. Remaining nonzero-offset/general view-backed
   Vulkan/CUDA coverage, CUDA unary/scientific view-backed `map`,
   CUDA/Vulkan fixed-width complex kernels, and direct Vulkan general
   `matrix/eigenpairs` still require dedicated contracts and validation before
   they can leave fail-closed behavior. Do not lower
   `BigInteger`, `BigFloat`, or `BigComplex` to Vulkan, and do not treat real
   `float32`/`float64` backend capability bits as complex capability.
6. Preserve `Tensor`, `map`, `contract`, `matrix/*`, `to-device`, `device`,
   and `tensor-backends` as the public surface. Do not add `VulkanTensor`,
   `GpuTensor`, `CudaTensor`, backend-flavored math names, hidden CPU/GPU
   transfers, or boundary cloning of opaque non-CPU handles without explicit
   copy ownership.
6. Extend Boost.Math or scalar precision lanes only when there is a concrete
   next scientific function, distribution family, or precision contract.

Do not start by binding GSL. Do not implement `linalg/matmul` as canonical.
