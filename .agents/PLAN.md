# Agent Plan Index

`.agents/PLAN.md` remains the operational plan entrypoint; read the indexed part files for the full plan state.

The historical content was split mechanically to keep individual files below the former 700-line repository limit. Content order is preserved in the part files. Current code-file split threshold is 1000 LOC from 2026-04-21 onward.

## Parts

- Part 01: [.agents/plan_parts/plan_part_01.md](plan_parts/plan_part_01.md) (341 lines)
- Part 02: [.agents/plan_parts/plan_part_02.md](plan_parts/plan_part_02.md) (624 lines)
- Part 03: [.agents/plan_parts/plan_part_03.md](plan_parts/plan_part_03.md) (353 lines)
- Part 04: [.agents/plan_parts/plan_part_04.md](plan_parts/plan_part_04.md) (395 lines)
- Part 05: [.agents/plan_parts/plan_part_05.md](plan_parts/plan_part_05.md) (758 lines)

## Current Checkpoint

Date: 2026-04-24 15:17 CEST

- Active hypothesis:
  - The memory model migration is complete; the next useful memory work is
    evidence collection, not more architecture migration.
- Current approach:
  - Continue the memory-boundary telemetry/benchmark evidence lane in
    `TODO.md` Part 18 as `MEM-BENCH-OBSERVE-003` through
    `MEM-BENCH-OBSERVE-005`.
  - Use `docs/plans/memory-boundary-telemetry-benchmark-plan-2026-04-24.md`
    as the active plan for maximizing observability before further
    optimization.
  - `MEM-BENCH-OBSERVE-001` is closed with the current signal inventory in
    `docs/plans/memory-boundary-telemetry-signal-inventory-2026-04-24.md`.
  - `MEM-BENCH-OBSERVE-002` is closed with runtime counter coverage exposed
    through `runtime-memory-stats` and `OMNI_MEM_TELEMETRY`.
  - The Part 18 memory-boundary proof-planner queue is closed through
    planner-owned commit migration, tag attribution, `CONS`/closure/array/
    BigInteger copy-debt reduction, and closure residual classification.
- Validation path:
  - Use `c3c build --obj-out obj` for runtime instrumentation changes.
  - Use bounded `memory-lifetime-bench` with `OMNI_BOUNDARY_BENCH=1` and
    counters enabled for benchmark evidence.
  - Use bounded `memory-lifetime-smoke` after runtime memory-accounting
    changes.
  - Keep `scripts/check_status_consistency.sh` green after any planning or
    backlog change.
- Next checkpoint:
  - Complete `MEM-BENCH-OBSERVE-003` by expanding `memory-lifetime-bench`
    workloads so the new allocator/value-shape counters report meaningful
    `OMNI_BENCH_SUMMARY` deltas.
- Negative-memory constraints:
  - Do not reopen closed memory-boundary copy-debt work to chase the expected
    no-splice closure rollback coverage bucket.
  - Do not weaken TEMP-edge proof rejection, mutation-epoch invalidation, or
    fail-closed planner behavior to improve benchmark numbers.
  - Do not add strict wall-clock gates until repeated bounded-container runs
    prove a stable timing envelope.
- Agent assignments:
  - Integration owner: GPT-5 Codex in this session; no active subagents.

## Previous Closure Checkpoint

Date: 2026-04-23

- Live queue is closed: `TODO.md` reports `Current actionable count: 0` and
  `scripts/check_status_consistency.sh` is green against changelog date
  2026-04-23.
- The Vulkan general eigen closure is fully validated:
  `TENSOR-100H-VK-REAL-GENERAL-EIGEN-001`,
  `TENSOR-100H-VK-COMPLEX-GENERAL-EIGEN-HARDENING-001`, and the promoted
  `TENSOR-100H-VK-GENERAL-EIGEN-DEFLATION-001` residual are closed.
- Native Vulkan `matrix/eigenpairs` now covers dense row-major `Float64`,
  `Float32`, `Complex128`, and `Complex64` with Vulkan-placed fixed-width
  complex `values` and `vectors`, exact 2x2 complex-shift handling, and
  active-submatrix deflation for mixed-block 3x3 spectra.
- Follow-up global-gate blockers are closed:
  - handled raise payload string-allocation failure now propagates the original
    allocation `ERROR`;
  - deduce no-data raises use an explicit payloadless fallback under
    payload-map OOM while generic/JIT payload construction remains strict;
  - FTXUI smoke examples consume structured raise payload messages.
- Latest validation:
  - `c3c build --obj-out obj`;
  - bounded `memory-lifetime-smoke` `pass=237 fail=0`;
  - bounded `deduce` `pass=392 fail=0`;
  - bounded `jit-policy` `pass=52 fail=0`;
  - bounded `scripts/run_ftxui_smoke.sh`;
  - bounded `scripts/run_global_gates.sh`, passing file-size gate, normal
    build, all configured normal Lisp slices, compiler slice, and FTXUI
    smokes. ASAN is explicitly skipped because the current C3 toolchain reports
    address sanitizer unsupported for this target.

## Follow-Up Audit Checkpoint

Date: 2026-04-19

- Re-ran the split gates after the all-file documentation split.
- Fixed mechanical Markdown split hygiene: stripped trailing whitespace and
  extra blank EOFs from generated part files so `git diff --check` is clean.
- Fast subagent audit results:
  - earlier docs/tooling audit found vendor/generated residuals; those have
    since been split rather than kept as LOC exceptions;
  - source/build-wiring audit found no missing helper build entries, PTX
    include mismatches, or duplicate moved Tensor ownership definitions.
- Latest validation:
  - raw tracked-text LOC gate prints no rows above 700;
  - generated Markdown index-link audit passed;
  - LSP smoke and Python syntax checks passed;
  - `scripts/build_omni_chelpers.sh` passed;
  - `c3c build --obj-out obj` linked `build/main`.

## Enforced File Size Gate Checkpoint

Date: 2026-04-19

- Added `scripts/check_file_size_gate.sh` and wired it into
  `scripts/run_global_gates.sh` as Stage 0.
- Gate contract: tracked text files must be at or below 700 LOC. The gate no
  longer exempts vendored/dependency text, generated SPIR-V/PTX text, or
  generated tree-sitter text. Only repo-state artifacts, backup/disabled
  snapshots, bytecode caches, and non-text binaries remain outside the text
  LOC contract.
- Artifact cleanup from audit:
  - untracked generated `.codegraph/logs/*` files and `.codegraph/daemon.pid`;
  - untracked generated Python bytecode under `scripts/__pycache__` and
    `tooling/omni-lsp/**/__pycache__`;
  - added `*.pyc` and `__pycache__/` ignore rules.
- `plan.jsonl` was split into a one-line JSONL index plus ordered
  `plan_jsonl_parts/*.jsonl` shards. Shards are valid JSONL, under 700 LOC,
  and below the jj new-file snapshot-size cap.
- `test_aot` was untracked and ignored as a top-level ELF test binary.
- `scripts/check_file_size_gate.sh` now uses `jj file list` when available so
  newly added jj-tracked files are checked before Git export.
- Generated Vulkan SPIR-V C embeddings that were still above 700 lines were
  split into wrapper-plus-`*_spv_part_*.inc` chunks.
- The repeated owner request extended the split policy to vendor/generated text
  too. Remaining oversized vendor C/C++/header files and the generated
  tree-sitter parser were split into include-only wrappers plus under-700
  `.inc` chunks. The oversized FTXUI doc/config files were split into stubs
  plus under-700 part files.
- `scripts/check_file_size_gate.sh` no longer exempts third-party, generated
  SPIR-V, generated PTX, or generated tree-sitter text files. Final raw
  `jj file list` text inventory reports zero files above 700 LOC; the largest
  tracked text file is exactly 700 lines.

## Active Vulkan ML Suite Planning Checkpoint

Date: 2026-04-19

- Owner direction: build toward a complete ML suite on Vulkan, not just
  individual math kernels.
- Added active roadmap:
  - `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- Added live TODO lanes:
  - `ML-VK-001` capability matrix and backend-neutral contract;
  - `ML-VK-010` batched linear algebra foundation;
  - `ML-VK-020` neural elementwise/reduction/softmax/loss kernels;
  - `ML-VK-030` convolution, pooling, and image tensor kernels;
  - `ML-VK-040` normalization and attention primitives;
  - `ML-VK-050` reverse-mode Tensor autograd with explicit Vulkan semantics;
  - `ML-VK-060` Vulkan-capable optimizer suite;
  - `ML-VK-070` model/layer library and serialization;
  - `ML-VK-080` graph capture, fusion, and memory planning;
  - `ML-VK-090` ML validation and benchmark suite.
- Active hypothesis:
  - complete ML support should preserve the current Tensor/Vulkan contract:
    backend-neutral public names, explicit placement/copyback, capability
    reporting, and no hidden CPU fallback for Vulkan operands.
- Next checkpoint:
  - start with `ML-VK-001` and freeze the public operation names plus
    `tensor-backends` capability keys before adding kernels.

## Active Vulkan ML Contract Audit Checkpoint

Date: 2026-04-19

- Audit findings from the ML-VK-001 pass:
  - `tensor-backends` had no backend-neutral ML operation-family keys, so the
    Vulkan ML roadmap had no runtime-discoverable contract.
  - Vulkan Float64 `stats/normal-*` support was documented and tested, but
    discoverability was blurred by the coarse `scientific-map-float64` key.
- Implemented checkpoint:
  - `tensor-backends` now exposes explicit false ML capability keys for every
    backend entry: `ml-linear`, `ml-convolution`, `ml-neural-map`,
    `ml-normalization`, `ml-attention`, `ml-autograd`, `ml-optimizer`, and
    `ml-graph-execution`.
  - `tensor-backends` now exposes `stats-normal-float64` and
    `stats-normal-float32` separately from broad `scientific-map-*` coverage.
    Vulkan keeps `scientific-map-float64` false while reporting
    `stats-normal-float64` from Vulkan Float64 availability.
- Validation:
  - `git diff --check`
  - `scripts/check_file_size_gate.sh`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- Next checkpoint:
  - continue `ML-VK-001` by freezing the first public ML operation names and
    adding fail-closed tests for unsupported Vulkan ML operations before
    implementing kernels.

## Active Vulkan ML First Operation Checkpoint

Date: 2026-04-19

- Audit finding:
  - `ML-VK-001` could not honestly claim fail-closed coverage while no public
    ML operation name was frozen.
- Implemented checkpoint:
  - froze `ml/linear` as the first backend-neutral public ML operation;
  - implemented CPU dense `Float64`/`Float32` affine projection with optional
    bias;
  - registered runtime and AOT primitive lookup for `ml/linear`;
  - kept GPU `ml-linear` capability false and added Vulkan fail-closed tests
    that reject concrete and lazy Vulkan operands before CPU realization.
- Validation:
  - `c3c build`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - focused advanced collections suite: `pass=1606 fail=0`
- Next checkpoint:
  - start `ML-VK-010` by deciding whether Vulkan `ml/linear` should lower
    through existing `contract` kernels or get a dedicated batched GEMM path.

## Active ML Linear Audit Hardening Checkpoint

Date: 2026-04-20

- Audit result:
  - subagent review found the `ml/linear` implementation honest after hard
    CPU-only resolution, but the test surface needed broader operand coverage.
  - `ML-VK-010` was still too broad to execute directly.
- Implemented checkpoint:
  - added `ml/linear` tests for lazy CPU input, rank-3 batch projection, dtype
    mismatch, Vulkan weights fail-closed, and Vulkan bias fail-closed.
  - split `ML-VK-010` into concrete `ML-VK-010-001` through
    `ML-VK-010-003` subitems.
- Validation:
  - `c3c build`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
  - focused advanced collections suite: `pass=1611 fail=0`
- Next checkpoint:
  - implement `ML-VK-010-001`, choosing the Vulkan `ml/linear` lowering path
    explicitly before changing backend capability reporting.

## Active Vulkan ML Linear No-Bias Checkpoint

Date: 2026-04-20

- Audit finding:
  - `ml/linear` could not reach Vulkan because the primitive resolved operands
    through the CPU-only Tensor expression path before any backend dispatch.
  - The existing Vulkan `contract` helper already matches no-bias
    `ml/linear` semantics when contracting input axis `rank - 1` with weights
    axis `1`.
- Implemented checkpoint:
  - added a pre-CPU-resolution Vulkan `Float32` no-bias branch for direct
    concrete Vulkan input and weights;
  - kept mixed-device operands, Vulkan bias, and view-backed operands
    fail-closed;
  - kept broad Vulkan `ml-linear` capability false until the remaining family
    support lands.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1618 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
- Next checkpoint:
  - continue `ML-VK-010-003` for Vulkan bias-add/reduction coverage, or decide
    `ML-VK-010-004` for expression-backed/view-backed `ml/linear` lowering.

## Active Vulkan ML Linear Bias Checkpoint

Date: 2026-04-20

- Audit finding:
  - `ML-VK-010-003` conflated bias-add and batched reductions; the shipped
    boundary is bias-add only, while reductions need their own semantic item.
  - Existing Vulkan `map` already provides the needed trailing-axis broadcast
    for `ml/linear` bias vectors.
- Implemented checkpoint:
  - direct concrete Vulkan `Float32` `ml/linear` now supports optional
    concrete Vulkan `Float32` bias through `contract` followed by broadcast
    `map +`;
  - mixed CPU/Vulkan bias and view-backed operands still fail closed;
  - `ML-VK-010-004` now tracks batched reductions separately and
    `ML-VK-010-005` tracks broader expression/view lowering.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1620 fail=0`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
- Next checkpoint:
  - continue `ML-VK-010-004` by defining the public batched-reduction
    operation surface and backend capability boundary before adding kernels.

## Active Vulkan ML Direct Capability Checkpoint

Date: 2026-04-20

- Audit finding:
  - broad Vulkan `ml-linear` must stay false, but the already-shipped direct
    concrete `Float32` route needed a truthful narrow capability bit.
  - `ML-VK-010-004` was a naming/surface decision, not implementation work.
- Implemented checkpoint:
  - added `ml-linear-direct-float32` to `tensor-backends`;
  - kept broad Vulkan `ml-linear` false while reporting the narrow Vulkan bit
    from the existing Vulkan `Float32` placement capability;
  - froze `ml/linear-batched-reduce` as the public batched-reduction surface.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1622 fail=0`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Next checkpoint:
  - implement `ML-VK-010-004-001`: Vulkan `Float32`
    `ml/linear-batched-reduce` coverage with no-hidden-CPU-fallback
    regressions.

## Active Vulkan ML Batched Reduce Checkpoint

Date: 2026-04-20

- Audit finding:
  - `ml/linear-batched-reduce` was frozen in planning but not registered as a
    runtime or AOT primitive.
  - Public docs and TODO acceptance did not spell out rank, capability, and
    no-fallback diagnostics for the new surface.
- Implemented checkpoint:
  - added `ml/linear-batched-reduce` as a callable primitive and AOT lookup;
  - shared the existing CPU dense `Float64`/`Float32` implementation and narrow
    direct concrete Vulkan `Float32` `contract` plus bias `map` path;
  - required input rank >= 2 for the batched surface while leaving `ml/linear`
    vector support unchanged;
  - closed `ML-VK-010-004-001` with public docs and diagnostics coverage.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1632 fail=0`
- Next checkpoint:
  - continue `ML-VK-010-005`: decide whether expression/view-backed Vulkan
    `ml/linear` lowering should expand beyond direct materialized map results
    or remain permanently fail-closed.

## Active Vulkan ML Narrow Expression Checkpoint

Date: 2026-04-20

- Audit finding:
  - broad arbitrary expression/view lowering would be unsafe without a larger
    recursive Vulkan realization contract;
  - the current Tensor realization path already supports a narrow safe lane for
    Vulkan-only expressions that materialize to concrete dense Vulkan storage.
- Implemented checkpoint:
  - `ml/linear` and `ml/linear-batched-reduce` now resolve Vulkan-only
    expressions before dispatching the existing Vulkan `Float32` contract path;
  - supported scope is direct concrete tensors, supported Vulkan map/contract
    materialization, and Vulkan transpose views that realize to dense Vulkan
    storage;
  - CPU/Vulkan mixes and non-Vulkan views are rejected before CPU fallback;
  - `ML-VK-010-005` is closed and `ML-VK-010-006` tracks the remaining
    Vulkan `Float64` linear/batched-reduce lane.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1637 fail=0`
- Next checkpoint:
  - implement `ML-VK-010-006`: Vulkan `Float64` `ml/linear` and
    `ml/linear-batched-reduce`, or record a concrete blocker with fail-closed
    tests.

## Active Vulkan ML Float64 Linear Checkpoint

Date: 2026-04-20

- Audit finding:
  - `ML-VK-010-006` was feasible; the blocker was local sloppiness, not a
    missing kernel. The Vulkan `ml/linear` path hard-gated Float32 even though
    existing Vulkan `contract` and broadcast `map +` support Float64.
  - `tensor-backends` only exposed `ml-linear-direct-float32`, leaving Float64
    discoverability underspecified.
- Implemented checkpoint:
  - widened the narrow Vulkan `ml/linear`/`ml/linear-batched-reduce` path to
    same-dtype `Float64` and `Float32`;
  - changed optional bias add to pass the actual result dtype into Vulkan map;
  - added `ml-linear-direct-float64` while keeping broad Vulkan `ml-linear`
    false;
  - closed `ML-VK-010` and `ML-VK-010-006` with positive Float64 direct,
    bias, mapped-bias, mapped-source, transpose-view, and batched-reduce tests.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1652 fail=0`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Next checkpoint:
  - continue strict audit from `ML-VK-020`: Vulkan neural elementwise,
    reductions, softmax, and loss kernels.

## Active Vulkan ML ReLU Checkpoint

Date: 2026-04-20

- Audit finding:
  - `ML-VK-020` was still an umbrella item. It mixed public naming,
    activation surfaces, reductions, softmax, and loss kernels without a
    reviewable first semantic boundary.
  - The backend already had enough Tensor `map max` support to ship a narrow
    Tensor ReLU surface honestly, while reductions and stable softmax need a
    real axis-reduction substrate.
- Implemented checkpoint:
  - froze `ml/relu` as the first public neural Tensor activation surface;
  - implemented `Float64` and `Float32` Tensor ReLU through existing `map max`
    semantics, preserving CPU/CUDA/Vulkan placement behavior and fail-closed
    unsupported dtype handling;
  - added narrow `tensor-backends` capability bits
    `ml-neural-relu-float64` and `ml-neural-relu-float32`;
  - split `ML-VK-020` into shipped ReLU items and remaining activation,
    reduction, softmax, and loss follow-ups.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1658 fail=0`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Next checkpoint:
  - continue `ML-VK-020-004`: remaining Float32 activation surfaces with
    explicit no-hidden-CPU-fallback tests, then do `ML-VK-020-006` before
    softmax/loss work.

## Active Vulkan ML Float32 Activations Checkpoint

Date: 2026-04-20

- Audit finding:
  - `ML-VK-020-004` was still vague about whether `tanh` should be a bare math
    alias or a canonical ML activation. The surface decision is now explicit:
    `ml/sigmoid`, `ml/tanh`, and `ml/gelu` are canonical `ml/*` activation
    names, not compatibility aliases for bare scientific primitives.
  - Exact GELU would need an `erf` Vulkan lowering that does not exist in the
    current map allowlist; tanh-approx GELU is the honest portable backend
    slice.
- Implemented checkpoint:
  - added runtime and AOT primitives for `ml/sigmoid`, `ml/tanh`, and
    tanh-approximation `ml/gelu`;
  - implemented the Float32 activation slice through composed Tensor `map`
    kernels, preserving CPU/CUDA/Vulkan placement and failing closed on
    unsupported dtypes;
  - added narrow activation capability bits for sigmoid/tanh/GELU Float64 and
    Float32, with Float64 transcendental ML activation bits left false;
  - closed `ML-VK-020-004`; `ML-VK-020-005` remains the Float64
    transcendental policy item.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1665 fail=0`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Next checkpoint:
  - continue `ML-VK-020-005` for Float64 transcendental activation policy or
    jump to `ML-VK-020-006` for real axis reductions before softmax/loss.

## Active Vulkan ML Float64 Activation Policy Checkpoint

Date: 2026-04-20

- Audit finding:
  - Float64 `ml/sigmoid`, `ml/tanh`, and `ml/gelu` must not quietly inherit
    bare scientific math or CPU fallback behavior. Prior Vulkan shader probes
    invalidated simple GLSL double transcendental assumptions, and exact GELU
    has no current Vulkan `erf` lowering.
- Implemented checkpoint:
  - closed `ML-VK-020-005` as explicit fail-closed behavior for Float64
    sigmoid/tanh/GELU ML activations;
  - kept the Float64 sigmoid/tanh/GELU backend capability bits false;
  - added an explicit Vulkan Float64 execution regression for all three
    activations so unsupported GPU inputs raise `tensor/backend-unsupported`;
  - updated TODO, roadmap, language spec, and reference docs with the
    fail-closed contract.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1666 fail=0`
- Next checkpoint:
  - continue `ML-VK-020-006`: real axis `sum`, `mean`, and `variance`
    reductions before softmax/loss work.

## Active Vulkan ML Axis Reduction Checkpoint

Date: 2026-04-20

- Audit finding:
  - `ml/sum`, `ml/mean`, and `ml/variance` must be a real ML reduction layer,
    not a softmax/loss shortcut through matrix `contract`.
  - The current Vulkan helper layer lacks a one-input axis reducer that
    preserves free axes; pretending `contract` is that substrate would repeat
    the known-bad design.
  - The just-landed Float32 reduction overflow path incorrectly reported
    `tensor/backend-unsupported`; overflow is a numeric/representation failure,
    not a backend capability failure.
  - Stable `logsumexp`/`softmax` needs an axis max path. A sum/mean/variance
    layer alone is not enough to implement the next loss/normalization slice
    without numerically sloppy shortcuts.
- Current approach:
  - ship CPU `Float64`/`Float32` axis reductions with truthful capability bits;
  - add CPU `ml/max` as the canonical axis maximum reducer before exposing
    stable `ml/logsumexp` or `ml/softmax`;
  - ship CPU `ml/logsumexp(input axes)` and `ml/softmax(input axis)` with
    max-shifted numerics, while leaving loss primitives split into separate
    TODO items;
  - ship CPU `ml/mean-squared-error(predictions targets)` as the unambiguous
    scalar loss item before the still-underdefined cross-entropy surface;
  - decide cross-entropy targets are same-shape probability/one-hot tensors and
    ship CPU `ml/cross-entropy(logits targets axis)` with max-shifted
    log-softmax over the explicit class axis;
  - keep Float32 reduction overflow under `tensor/numeric-overflow`;
  - fail closed for CUDA/Vulkan until backend-specific axis kernels exist;
  - keep `ML-VK-020-006-VK` open for the real Vulkan reduction substrate.
- Current test-plan delta:
  - guarded Vulkan Float64/Float32 parity tests for `ml/sum`, `ml/mean`, and
    `ml/variance` now assert the result stays on Vulkan and matches CPU after
    an explicit `to-device 'cpu` check;
  - rank-3 middle-axis coverage now checks shared reduction indexing across
    `ml/sum`, `ml/logsumexp`, and `ml/softmax`;
  - Vulkan Float64/Float32 `ml/max` is now routed through the same axis
    reduction helper and covered by device-preserving parity tests;
  - Vulkan Float32 `ml/logsumexp` now uses the same helper with a stable
    max-shifted shader and device-preserving parity tests;
  - Vulkan Float32 `ml/softmax` now uses a dedicated same-shape
    axis-normalization shader and keeps the result on Vulkan;
  - Vulkan Float64/Float32 `ml/mean-squared-error` now uses a dedicated
    two-input scalar loss shader and keeps the scalar result on Vulkan;
  - Vulkan Float32 `ml/mean-squared-error` now checks the device scalar for
    non-finite output and maps overflow to `tensor/numeric-overflow`, matching
    the CPU Float32 write contract instead of returning an `inf` tensor;
  - Vulkan Float64/Float32 `ml/mean-squared-error` now stages large inputs
    through chunked partial-sum kernels and follow-up reduction passes instead
    of using one device invocation for the whole scalar loss;
  - guarded staged MSE tests now cover a >256-element Float64 path, a
    >65k-element recursive Float32 path, and staged Float32 overflow mapping;
  - Vulkan Float32 `ml/cross-entropy` now uses a dedicated fused loss shader
    plus staged status-preserving reduction passes that preserve Vulkan
    placement, target probability diagnostics, and fail-closed Float64
    behavior;
  - `AUDIT-AOT-RUNTIME-MANIFEST-061` regenerated the manifest-backed AOT
    non-test Lisp source set into four under-700 parts and strengthened the
    Stage 3 parity checker to compare the full source set;
  - unsupported dtype coverage remains, while Vulkan Float64 `ml/logsumexp`
    and Vulkan Float64 `ml/softmax` stay backend-unsupported.
- Next checkpoint:
  - `ML-VK-030-001` through `ML-VK-030-003` are implemented for explicit dense
    NCW `ml/conv1d`, NCHW `ml/conv2d`, and NCHW max/average pool2d with CPU
    `Float64`/`Float32` and direct Vulkan `Float32`. Broad `ml-convolution` is
    now true for CPU and for Vulkan when Float32 is available.
  - `ML-VK-070-001` is implemented for Omni Neural DataSpec schema validation:
    data-only `nn/*` constructors are registered, `nn/validate` returns valid
    specs, and invalid specs raise `nn/invalid-spec` diagnostics before any
    parameter allocation.
  - `ML-VK-070-002` is implemented for deterministic parameter initialization:
    `nn/init(spec [options])` returns transparent model bundles with explicit
    `params`, `state`, `mode`, `dtype`, `device`, and `metadata`; dense,
    conv1d, and conv2d parameter tensors honor seeded initializer options and
    explicit CPU/CUDA/Vulkan placement.
  - Superseded by the later Omni Neural DataSpec inference checkpoint below:
    `ML-VK-070-003` is now implemented, so the next Vulkan ML work is
    `ML-VK-070-004` checkpoint serialization and restore.
  - Runtime validation for helper changes should put `build` before
    `/usr/local/lib` in `LD_LIBRARY_PATH`; otherwise local smokes can load a
    stale installed `libomni_chelpers`.

## Active Omni Neural DataSpec Inference Checkpoint

Date: 2026-04-20

- Implemented checkpoint:
  - closed `ML-VK-070-003` as the first model-bundle inference and inspection
    slice;
  - added `nn/apply`, `nn/predict`, `nn/summary`, `nn/spec`,
    `nn/parameters`, `nn/state`, and `nn/mode`;
  - lowered sequential, dense, conv1d, conv2d, max/avg pool2d, activation,
    softmax, and CPU flatten specs through the existing `ml/*` primitives;
  - made non-empty explicit `nn/apply` options fail closed instead of being
    accepted as ignored no-ops;
  - made malformed `nn/summary` parameter trees report `nn/invalid-spec`.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1748 fail=0`
  - basic Lisp slice: `pass=160 fail=0`
  - primitive docs parity, file-size gate, and `git diff --check`
- Next checkpoint:
  - Superseded by the later Omni Neural DataSpec checkpoint entry below:
    `ML-VK-070-004` is now implemented, so the next Vulkan ML work is
    `ML-VK-070-005` after `ML-VK-050` autograd and `ML-VK-060` optimizers, or
    the next lower-level Vulkan ML kernel lane if training prerequisites remain
    blocked.

## Active Omni Neural DataSpec Checkpoint Round Trip

Date: 2026-04-20

- Implemented checkpoint:
  - closed `ML-VK-070-004` as the first spec/model checkpoint slice;
  - added `nn/save-spec`, `nn/load-spec`, `nn/save`, and `nn/load`;
  - supported checkpoint JSON strings and explicit file paths;
  - serialized transparent model bundle data, including `spec`, `params`,
    `state`, `mode`, `dtype`, `device`, `metadata`, tensor dtype, tensor shape,
    flat tensor data, and recorded tensor placement;
  - restored non-CPU tensors through explicit `to-device` placement instead of
    hidden fallback;
  - made mismatched payload families and malformed checkpoint envelopes fail
    closed with `nn/invalid-spec`.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1753 fail=0`
  - basic Lisp slice: `pass=160 fail=0`
  - primitive docs parity, file-size gate, and `git diff --check`
- Next checkpoint:
  - continue Vulkan ML prerequisites for `ML-VK-050` autograd and `ML-VK-060`
    optimizers before opening `ML-VK-070-005` training facade work.

## Active Vulkan ML Layer Normalization Checkpoint

Date: 2026-04-20

- Implemented checkpoint:
  - closed `ML-VK-040-001` as the first normalization primitive slice;
  - added `ml/layer-normalization(input axis [epsilon])` with default
    epsilon `1e-5` and positive finite epsilon validation;
  - shipped CPU `Float64`/`Float32` and direct Vulkan `Float32` execution;
  - added a dedicated same-shape Vulkan layer-normalization helper/shader and
    embedded SPIR-V source;
  - exposed `ml-layer-normalization-float64`,
    `ml-layer-normalization-float32`, and broad `ml-normalization` capability
    reporting;
  - kept Vulkan Float64/CUDA fail-closed instead of routing through CPU.
- Validation:
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - direct CPU and Vulkan `--eval` smokes
  - focused advanced collections suite: `pass=1758 fail=0`
  - basic Lisp slice: `pass=160 fail=0`
  - primitive docs parity, file-size gate, and `git diff --check`
- Next checkpoint:
  - continue `ML-VK-040` with explicit-stat batch normalization; affine
    gamma/beta layer-normalization should be a separate multi-buffer Vulkan
    kernel slice rather than a hidden CPU fallback.

## Completed Vulkan ML Batch Normalization Checkpoint

Date: 2026-04-20

- Implemented checkpoint:
  - closed `ML-VK-040-002` as
    `ml/batch-normalization(input scale bias mean variance channel-axis [epsilon])`;
  - chose explicit inference/data-path batch normalization with
    rank-1 scale, bias, mean, and variance tensors matching the channel axis;
  - CPU route supports `Float64`/`Float32`; Vulkan route supports direct dense
    row-major `Float32` with no hidden CPU fallback.
- Validation:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for
    `csrc/tensor_vulkan_ml_batch_norm_f32.comp`;
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - direct CPU/Vulkan `--eval` smokes
  - focused advanced collections suite: `pass=1764 fail=0`
  - basic Lisp slice: `pass=160 fail=0`
  - primitive docs parity, file-size gate, and `git diff --check`

## Completed Vulkan ML Scaled Dot-Product Attention Checkpoint

Date: 2026-04-20

- Implemented checkpoint:
  - closed `ML-VK-040-003` as
    `ml/scaled-dot-product-attention(query key value [mask] [scale])`;
  - CPU route supports `Float64`/`Float32`;
  - Vulkan route supports direct dense row-major `Float32` through a dedicated
    five-buffer helper/shader path;
  - accepted optional additive `[Q K]` or batched masks and positive finite
    scale, with default scale `1 / sqrt(head-dim)`;
  - added `ml-scaled-dot-product-attention-float64`,
    `ml-scaled-dot-product-attention-float32`, and broad `ml-attention`
    capability reporting;
  - kept mixed CPU/Vulkan operands and Vulkan Float64 fail-closed before CPU
    fallback.
- Validation:
  - `glslangValidator -V --target-env vulkan1.0` and `spirv-val` for
    `csrc/tensor_vulkan_ml_attention_f32.comp`
  - `scripts/build_omni_chelpers.sh`
  - `c3c build`
  - direct CPU/Vulkan `--eval` smokes
  - focused advanced collections suite: `pass=1769 fail=0`
  - basic Lisp slice: `pass=160 fail=0`
  - primitive docs parity, Stage 3 source parity, file-size gate, and
    `git diff --check`
- Next checkpoint:
  - continue `ML-VK-040` only for fused attention/dropout/matmul after the
    unfused oracle remains stable, or move to `ML-VK-050` autograd prerequisites.

## Completed ML-VK-050-001 Data Gradient Checkpoint

Date: 2026-04-20

- Implemented checkpoint:
  - added canonical `ml/grad` as a data-oriented gradient spec evaluator;
  - first supported spec kind is `linear-mean-squared-error`;
  - CPU route supports `Float64`/`Float32` dense row-major tensors;
  - returns ordinary data: scalar loss tensor, forward output tensor,
    input-gradient tensor, and `parameter-gradients` with weights/bias tensors;
  - keeps Vulkan backward fail-closed until real Vulkan gradient kernels land;
  - keeps broad `ml-autograd` false because tape-backed reverse-mode over the
    full supported training path is still open.
- Validation:
  - `c3c build`
  - direct CPU `--eval` gradient smokes
  - focused advanced collections suite: `pass=1772 fail=0`
  - basic Lisp slice: `pass=160 fail=0`
  - primitive docs parity, Stage 3 source parity, file-size gate, and
    `git diff --check`
- Next checkpoint:
  - continue `ML-VK-050` with tape-backed composition or add explicit backward
    rules for activations and softmax/loss before exposing `nn/grad`.

## Completed ML-VK-050-002 Activation Gradient Checkpoint

Date: 2026-04-20

- Implemented checkpoint:
  - extended `ml/grad` with `linear-activation-mean-squared-error`;
  - preserved the same data-oriented spec shape as the linear MSE gradient path;
  - added `relu` backward for CPU `Float64`/`Float32`;
  - added `sigmoid`, `tanh`, and tanh-approximation `gelu` backward for CPU
    `Float32`, matching the existing forward activation dtype policy;
  - kept CUDA/Vulkan backward fail-closed before CPU fallback and did not flip
    broad `ml-autograd`.
- Validation path:
  - run `c3c build`;
  - run direct CPU `--eval` activation-gradient smokes;
  - run focused advanced collections and basic Lisp slices;
  - run primitive docs parity, Stage 3 source parity, file-size gate, and
    `git diff --check`.
- Next checkpoint:
  - continue `ML-VK-050` with softmax/loss backward rules or tape-backed
    composition before exposing `nn/grad` or `nn/train-step`.

## Completed ML-VK-050-003 Softmax CE Gradient Checkpoint
Date: 2026-04-20
- Implemented `linear-softmax-cross-entropy` for `ml/grad`.
- Contract: CPU dense row-major `Float64`/`Float32` linear logits, last-axis
  stable softmax cross-entropy, probability-target validation, and data output.
- Device semantics: CUDA/Vulkan backward remains fail-closed; `ml-autograd` false.
- Validation: build, direct smokes, focused suites, parity, source, size, and diff gates passed.
- Next checkpoint: tape-backed composition or optimizer prerequisites before `nn/grad`.
## Active ML-VK-060-001 SGD Step Checkpoint
Date: 2026-04-20 - Implemented CPU `ml/sgd-step` for data-oriented parameter trees; validation passed.

## Active ML-VK-060-002 Optimizer Step Checkpoint
Date: 2026-04-20 - Implemented CPU `ml/optimizer-step` for SGD momentum state; validation passed.

## Active ML-VK-060-003 Adam And AdamW Optimizer Step Checkpoint
Date: 2026-04-20 - Implemented CPU `ml/optimizer-step` for Adam and AdamW explicit moment state; validation passed.

## Active ML-VK-060-004 RMSProp Optimizer Step Checkpoint
Date: 2026-04-20 - Implemented CPU `ml/optimizer-step` for RMSProp explicit square-average and optional velocity state; validation passed.

## Active ML-VK-060-005 Gradient Clipping Checkpoint
Date: 2026-04-20 - Implemented CPU `ml/clip-gradients` and optimizer `clip-norm` max-norm clipping; validation passed.

## Active ML-VK-060-006 Optimizer Checkpoint Helpers
Date: 2026-04-20 - Implemented CPU `ml/save-optimizer` and `ml/load-optimizer` for explicit optimizer spec/state checkpoints; validation passed.

## Active ML-VK-060-007 Vulkan Float32 SGD Optimizer
Date: 2026-04-20 - Implemented Vulkan dense row-major `Float32`
`ml/optimizer-step` SGD for all-Vulkan parameter/gradient/velocity leaves.

- Stateless SGD, initial velocity creation, and momentum velocity consumption
  preserve Vulkan placement and explicit optimizer state.
- `tensor-backends` now reports narrow `ml-optimizer-sgd-float32`; broad
  `ml-optimizer` remains false.
- Adam, AdamW, RMSProp, clipping kernels, CUDA optimizer kernels, and
  `nn/train-step` remain open.

## Active ML-VK-060-008 Vulkan Float32 Adam And AdamW Optimizer
Date: 2026-04-20 - Implemented Vulkan dense row-major `Float32`
`ml/optimizer-step` Adam and AdamW for all-Vulkan parameter/gradient/moment
leaves.

- Initial first/second moment creation and prior-moment continuation preserve
  Vulkan placement and explicit integer `step` state.
- Adam keeps coupled weight decay; AdamW keeps decoupled weight decay.
- `tensor-backends` now reports narrow `ml-optimizer-adam-float32` and
  `ml-optimizer-adamw-float32`; broad `ml-optimizer` remains false.
- Remaining optimizer work: Vulkan RMSProp and clipping, CUDA optimizer
  kernels, and train-step integration after autograd is sufficient.

## Active ML-VK-060-009 Vulkan Float32 RMSProp Optimizer
Date: 2026-04-20 - Implemented Vulkan dense row-major `Float32`
`ml/optimizer-step` RMSProp for all-Vulkan parameter/gradient/square-average
and velocity leaves.

- Missing square-average or velocity state initializes from zero, matching the
  CPU explicit-state contract.
- Output square-average and optional velocity state preserve Vulkan placement;
  integer `step` remains ordinary state data.
- `tensor-backends` now reports narrow `ml-optimizer-rmsprop-float32`; broad
  `ml-optimizer` remains false.
- Remaining optimizer work: Vulkan clipping, CUDA optimizer kernels, and
  train-step integration after autograd is sufficient.

## Active ML-VK-060-010 Vulkan Float32 Gradient Clipping
Date: 2026-04-20 - Implemented Vulkan dense row-major `Float32`
`ml/clip-gradients` for all-Vulkan gradient trees and optimizer `clip-norm`.

- Added Vulkan sum-squares and scale kernels for clipping norm computation and
  scaled gradient output.
- Mixed CPU/Vulkan gradient trees and non-`Float32` Vulkan leaves fail closed.
- `tensor-backends` now reports narrow `ml-clip-gradients-float32`; broad
  `ml-optimizer` remains false.
- Remaining optimizer work: CUDA optimizer kernels and `nn/train-step`
  integration after autograd is sufficient.

## Active ML-VK-060-011 CUDA Float32 SGD Optimizer
Date: 2026-04-20 - Added CUDA dense row-major `Float32` `ml/optimizer-step`
SGD execution backed by the existing CUDA elementwise map kernels.

- Added runtime dispatch and behavior coverage for stateless and optional
  momentum `ml/optimizer-step` on all-CUDA `Float32` dense row-major
  parameter/gradient trees, including state initialization/consumption.
- Documented that `tensor-backends` exposes narrow `ml-optimizer-sgd-float32`
  when CUDA elementwise `Float32` map kernels are available and that broad
  `ml-optimizer` remains false.
- Runtime route is not fused: it composes existing CUDA `map` kernels for
  weight decay, momentum, learning-rate scaling, and parameter subtraction.
- Remaining optimizer work: fused CUDA optimizer kernels beyond map-backed
  implementations and `nn/train-step` integration after autograd is sufficient.

## Active ML-VK-060-012/013 CUDA Float32 Map-Backed Adam/AdamW/RMSProp Optimizer
Date: 2026-04-20 - Implemented map-backed CUDA `Float32` optimizer execution in
`ml/optimizer-step` and expanded mixed-device fail-closed diagnostics for Adam,
AdamW, and RMSProp with explicit state.

- Added runtime dispatch and test coverage for CUDA dense row-major `Float32`
  map-backed:
  - Adam with initial and continued moment state;
  - AdamW with decoupled weight-decay state updates;
  - RMSProp with square-average initialization and momentum continuation.
- Kept `tensor-backends` checks updated so CUDA narrow keys for
  `ml-optimizer-adam-float32`, `ml-optimizer-adamw-float32`, and
  `ml-optimizer-rmsprop-float32` align with `elementwise-map-float32`.
- `ml-optimizer` broad key remains false while fused CUDA optimizer kernels and
  train-step integration remain open.

## Active Custom Kernel Surface Decision
Date: 2026-04-20 - Owner approved `Kernel` as a real type/value, possibly with special runtime/compiler support, for user-defined backend kernels.

- Canonical base form: `(define name (Kernel spec))`.
- Owner revision on 2026-04-21: do not implement `(define [kernel] ...)`.
  Kernel declarations should remain ordinary bindings to `Kernel` values, for
  example `(define name (Kernel spec))`.
- Specs use ordinary Omni data with quoted symbol keys, `Dictionary`, arrays,
  path access, and postfix index access. Example inspection:
  `k.inputs`, `k.inputs.[0]`, `k.inputs.[0].name`.
- Use `ref` only for dynamic lookup, not execution.
- Execution must be explicit, for example `(kernel/run k inputs push)`.
- Do not make `Kernel` pretend to be `Lambda`; do not overload ordinary calls,
  path access, postfix indexing, or `ref` into kernel execution.

## Active ML-VK-080-002 Checked Vulkan Kernel Runner
Date: 2026-04-21 - Implemented the first real `kernel/run` backend route.

- Shipped operation: `operation 'scale-f32` with `backend 'vulkan`.
- Contract:
  - one input descriptor and one output descriptor;
  - both descriptors must declare `dtype 'Float32`;
  - descriptor shapes must match the realized tensor shape, with symbol
    dimensions accepted as wildcards;
  - `push` must include `scale` representable as `Float32`;
  - input storage must be dense row-major Vulkan `Float32`.
- The runner uses the existing checked Vulkan Float32 scale helper and returns
  an ordinary dictionary keyed by the output descriptor name.
- Still open under `ML-VK-080`: arbitrary user source compilation, graph capture,
  command-buffer batching, fusion, device-buffer reuse/lifetime planning, and
  deterministic invalidation.
- Negative constraint remains active: do not implement `(define [kernel] ...)`.

## Completed ML-VK-070-005 Dense Training Facade Checkpoint
Date: 2026-04-21 - Implemented the first data-oriented NN training facade.

- Added `nn/forward`, `nn/grad`, and `nn/train-step`.
- `nn/forward` reuses `nn/apply` model/explicit-data execution arities without
  requiring eval mode.
- `nn/grad` requires train-mode model bundles and supports direct dense or
  sequential dense-plus-activation specs by lowering to the shipped CPU
  `ml/grad` linear MSE and linear softmax cross-entropy contracts.
- `nn/train-step` composes `nn/grad` with `ml/optimizer-step` and returns
  updated model data, optimizer state, gradients, loss, output, and nested
  gradient/optimizer results without hidden mutation.
- CUDA/Vulkan backward remains fail-closed through `ml/grad`; no hidden CPU
  fallback was added.

## Completed ML-VK-070-006 Optimizer Spec Constructors
Date: 2026-04-21 - Added data-first NN optimizer constructors.

- Added `nn/sgd`, `nn/adam`, `nn/adamw`, and `nn/rmsprop`.
- Constructors validate required non-negative finite learning rates, supported
  option keys, and optimizer-specific hyperparameter ranges before returning
  ordinary optimizer spec dictionaries.
- Returned specs compose directly with `nn/train-step` and `ml/optimizer-step`;
  constructors do not mutate or own hidden optimizer state.
- Next checkpoint: continue broader ML suite work from remaining TODO entries,
  with tape-backed recursive autograd and fused CUDA optimizer kernels still
  open.

## Completed ML-VK-080-001 Kernel Value Surface
Date: 2026-04-21 - Implemented the first data-oriented custom `Kernel` value
surface and kept execution explicit.

- Active hypothesis validated: `Kernel` can be a real type/value without
  becoming an opaque object. The runtime value remains a dictionary-shaped data
  spec, while `type-of` / `is?` expose `Kernel` for introspection.
- Current approach shipped:
  - `Kernel(spec)` validates backend, operation, IO descriptors, optional
    push constants, optional workgroup, and recognized keys.
  - Constructed values normalize `kind` to `'kernel` and preserve ordinary
    path/index/ref data access.
  - `kernel/run(kernel inputs push)` is explicit. The initial value-surface
    slice failed closed; later checked runners added Vulkan `scale-f32` and the
    binary `Float32` family.
  - Parser postfix chaining now supports `rows.[0].name` /
    `k.inputs.[0].name` without reviving removed leading-dot accessors.
- Validation path completed: `c3c build`, direct eval smokes, focused advanced
  collections exit 0, compiler slice, basic slice, primitive docs parity,
  Stage 3 source parity, code file-size gate, and `git diff --check`.
- Next checkpoint: implement graph-capture command batching or a third checked
  helper-backed Kernel operation from the remaining `ML-VK-080` TODOs.

## Active ML-VK-080-003 Checked Vulkan Add Kernel Runner
Date: 2026-04-21 - Implemented the second real `kernel/run` backend route.

- Shipped operation: `operation 'add-f32` with `backend 'vulkan`.
- Contract:
  - two input descriptors and one output descriptor;
  - all descriptors must declare `dtype 'Float32`;
  - input tensors and output descriptor shape must match;
  - `push` must be empty and `push` specs must be absent or empty;
  - runtime inputs must be dense row-major Vulkan `Float32`.
- The runner uses the existing Vulkan Float32 map helper with binary add and
  returns an ordinary dictionary keyed by the output descriptor name.
- Still open under `ML-VK-080`: arbitrary user source compilation, graph capture,
  command-buffer batching, fusion, device-buffer reuse/lifetime planning, and
  deterministic invalidation.
- Negative constraint remains active: do not implement `(define [kernel] ...)`.

## Active ML-VK-080-004 Checked Vulkan Binary Float32 Kernel Family
Date: 2026-04-21 - Generalized the helper-backed binary Kernel runner.

- Shipped operations: `add-f32`, `sub-f32`, `mul-f32`, `div-f32`, `min-f32`,
  and `max-f32` with `backend 'vulkan`.
- Contract:
  - two input descriptors and one output descriptor;
  - all descriptors must declare `dtype 'Float32`;
  - input tensors and output descriptor shape must match;
  - `push` must be empty and `push` specs must be absent or empty;
  - runtime inputs must be dense row-major Vulkan `Float32`.
- The runner uses the existing Vulkan Float32 map helper opcodes and returns an
  ordinary dictionary keyed by the output descriptor name.
- Validation completed: `c3c build`, direct `mul-f32` smoke, focused advanced
  collections `pass=1840 fail=0`, basic Lisp `pass=161 fail=0`, compiler slice
  `pass=287 fail=0`, primitive docs parity, Stage 3 source parity, code
  file-size gate, and `git diff --check`.
- Next checkpoint: commit and push this checked binary family slice, then
  continue toward graph/DAG capture or another real backend execution boundary.

## Active ML-VK-080-005 Checked Vulkan Scalar Float32 Kernel Family
Date: 2026-04-21 - Added scalar helper-backed Kernel execution.

- Shipped tensor-scalar operations: `add-scalar-f32`, `sub-scalar-f32`,
  `mul-scalar-f32`, `div-scalar-f32`, `min-scalar-f32`, and `max-scalar-f32`.
- Shipped scalar-left operations: `scalar-sub-f32` and `scalar-div-f32`.
- Contract:
  - one input descriptor and one output descriptor;
  - both descriptors must declare `dtype 'Float32`;
  - descriptor and tensor shapes must match;
  - the Kernel spec push dictionary must contain only `scalar 'Float32`;
  - runtime push must contain exactly one `scalar` value representable as
    `Float32`;
  - runtime input must be dense row-major Vulkan `Float32`.
- The runner uses the existing Vulkan Float32 map helper scalar modes and
  returns an ordinary dictionary keyed by the output descriptor name.
- Validation completed: `c3c build`, focused advanced collections
  `pass=1842 fail=0`, basic Lisp `pass=161 fail=0`, compiler slice
  `pass=287 fail=0`, primitive docs parity, Stage 3 source parity, code
  file-size gate, and `git diff --check`.
- Next checkpoint: commit and push this scalar family slice, then continue
  toward graph/DAG capture or another real backend execution boundary.

## Active ML-VK-080-006 Checked Vulkan Unary Float32 Kernel Family
Date: 2026-04-21 - Added unary helper-backed Kernel execution.

- Shipped unary operations: `abs-f32`, `neg-f32`, `sqrt-f32`,
  `identity-f32`, `zero-f32`, `sin-f32`, `cos-f32`, `tan-f32`, `asin-f32`,
  `acos-f32`, `atan-f32`, `sinh-f32`, `cosh-f32`, `tanh-f32`, `exp-f32`,
  `log-f32`, `log10-f32`, and `normal-cdf-f32`.
- Contract:
  - one input descriptor and one output descriptor;
  - both descriptors must declare `dtype 'Float32`;
  - descriptor and tensor shapes must match;
  - the Kernel spec push dictionary must be absent, nil, or empty;
  - runtime push must be empty;
  - runtime input must be dense row-major Vulkan `Float32`.
- The runner uses the existing Vulkan Float32 unary map helper and returns an
  ordinary dictionary keyed by the output descriptor name.
- Validation completed: `c3c build`, focused advanced collections
  `pass=1844 fail=0`, basic Lisp `pass=161 fail=0`, compiler slice
  `pass=287 fail=0`, primitive docs parity, Stage 3 source parity, code
  file-size gate, and `git diff --check`.
- Next checkpoint: commit and push this unary family slice, then continue
  toward graph/DAG capture, source compilation, command-buffer batching, fusion,
  or device-buffer lifetime planning.

## Active ML-VK-080-007 Checked Vulkan Single-Node Kernel Capture
Date: 2026-04-21 - Added the first checked Kernel graph-capture surface.

- Shipped `kernel/capture(kernel inputs push)` for the checked direct-helper
  Vulkan `Float32` Kernel families already supported by `kernel/run`.
- Contract:
  - accepts the same explicit Kernel, input dictionary, and push dictionary
    boundary as `kernel/run`;
  - validates backend, operation family, descriptor dtype/arity, runtime
    placement, push contract, and concrete runtime shape;
  - returns ordinary data with `kind 'kernel-graph`, a single `kernel-node`,
    backend, operation, family, dtype, device, direct-helper execution,
    input/output names, concrete runtime shape, push data, and invalidation key;
  - does not launch the kernel.
- Validation completed: `c3c build`, focused advanced collections
  `pass=1846 fail=0`, compiler slice `pass=288 fail=0`, basic Lisp
  `pass=161 fail=0`, primitive docs parity, Stage 3 source parity, code
  file-size gate, and `git diff --check`.
- Next checkpoint: commit and push this single-node capture slice, then continue
  toward multi-node Tensor expression DAG capture, command-buffer batching,
  fusion, or device-buffer lifetime planning.

## Active ML-VK-080-008 Vulkan Tensor Map Graph Capture
Date: 2026-04-21 - Added the first multi-node Tensor graph-capture boundary.

- Shipped `tensor/capture(source)` for supported all-Vulkan `Float32`
  concrete/map Tensor expression graphs.
- Contract:
  - accepts one Tensor source;
  - requires `Float32` dtype and all leaves/device references to be Vulkan;
  - supports concrete source nodes and map nodes only;
  - records source/map nodes, node ids, input edges, scalar operands, output
    node id, shape, dtype/device/backend, family `map-expression`, and an
    invalidation key;
  - does not launch or realize the graph.
- Added explicit graph preservation for supported CPU lazy `Float32` map
  expressions under `to-device 'vulkan`, so user code can build a capturable
  Vulkan map DAG without changing direct Vulkan map execution.
- Still open under `ML-VK-080`: contract/view graph capture, command-buffer
  batching, fusion, source compilation, buffer reuse/lifetime planning, and
  broader invalidation/capability planning.
- Validation completed: `c3c build`, direct capture smoke, focused advanced
  collections `pass=1848 fail=0`, compiler slice `pass=289 fail=0`, basic Lisp
  `pass=161 fail=0`, primitive docs parity, Stage 3 source parity, code
  file-size gate, and `git diff --check`.

## Active ML-VK-080-009 Vulkan Tensor Contract Graph Capture
Date: 2026-04-21 - Extends Tensor graph capture from map DAGs to contract DAGs.

- Shipped in this slice:
  - `to-device 'vulkan` preserves supported CPU lazy `Float32` contract
    expressions as Vulkan Tensor expressions;
  - `tensor/capture(source)` counts and records all-Vulkan `Float32` contract
    expression nodes;
  - contract nodes record input ids, left/right axes, axis count, output shape,
    dtype/device/backend, and family `contract-f32`.
- Contract:
  - accepts one Tensor source;
  - requires `Float32` dtype and all leaves/device references to be Vulkan;
  - supports concrete source, map, and contract nodes;
  - does not launch, fuse, batch commands, or embed the source Tensor in the
    returned plan.
- Still open under `ML-VK-080`: view graph capture, command-buffer batching,
  fusion, source compilation, buffer reuse/lifetime planning, and broader
  invalidation/capability planning.
- Validation completed: `c3c build`, direct contract-capture eval smoke,
  focused advanced collections `pass=1849 fail=0`, compiler slice
  `pass=289 fail=0`, basic Lisp `pass=161 fail=0`, primitive docs parity,
  Stage 3 source parity, code file-size gate, and `git diff --check`.

## Active ML-VK-080-010 Vulkan Tensor Direct View Graph Capture
Date: 2026-04-21 - Extends Tensor graph capture to the existing transpose-view boundary.

- Shipped in this slice:
  - `tensor/capture(source)` counts and records direct all-Vulkan `Float32`
    rank-2 `matrix/transpose-view` nodes over concrete dense Vulkan backing
    tensors;
  - view nodes record input id, output shape, strides, storage offset,
    dtype/device/backend, operation `transpose-view`, and family
    `transpose-view-f32`.
- Contract:
  - accepts one Tensor source;
  - requires `Float32` dtype and all leaves/device references to be Vulkan;
  - supports concrete source, map, contract, and direct transpose-view nodes;
  - does not launch, fuse, batch commands, consume arbitrary strided views, or
    embed the source Tensor in the returned plan.
- Still open under `ML-VK-080`: arbitrary view graph capture, command-buffer
  batching, fusion, source compilation, buffer reuse/lifetime planning, and
  broader invalidation/capability planning.
- Validation completed: `c3c build`, direct view-capture eval smoke, focused
  advanced collections `pass=1850 fail=0`, compiler slice `pass=289 fail=0`,
  basic Lisp `pass=161 fail=0`, primitive docs parity, Stage 3 source parity,
  code file-size gate, and `git diff --check`.

## Active ML-VK-080-011 Tensor Graph Schedule Metadata
Date: 2026-04-21 - Adds non-executing schedule metadata to captured Tensor graphs.

- Shipped in this slice:
  - every captured Tensor node now records an execution class:
    `external-buffer`, `direct-helper`, or `metadata-only`;
  - `tensor/capture(source)` now returns a topological `schedule` array with
    step, node id, kind, execution class, dependencies, and launch flag;
  - graph plans now report `launch-count`, `execution 'not-launched`,
    and `fusion 'none`.
- `ML-VK-080-012`: `tensor/capture(source)` now records `command-batching
  'metadata` plus a serial `command-buffer-candidate` descriptor when the graph
  contains launchable `direct-helper` nodes; non-launching graphs keep
  `command-batching 'none`. This is descriptive only; it does not allocate,
  record, submit, or execute Vulkan command buffers, and it does not fuse graph
  nodes.
- `ML-VK-080-013`: `tensor/capture(source)` now records a nested
  metadata-only `memory-plan` dictionary with kind `tensor-memory-plan`,
  `version`, `backend`, `dtype`, `policy`, `allocates`, `retains-handles`,
  `external-bytes`, `transient-bytes`, and `node-memory`. Capture nodes record
  `element-count`, `byte-length`, `storage-offset`, `storage-elements`,
  `storage-bytes`, `allocation`, `owner`, and `write-policy`. This slice is
  descriptive only; it does not allocate, retain handles, or reuse runtime
  buffers, and the top-level capture result remains a `tensor-graph`.
- `ML-VK-080-014`: `tensor/capture(source)` now records a nested
  metadata-only `fusion-plan` dictionary with kind `tensor-fusion-plan`, policy
  `eligibility-only`, map-chain candidates for direct Vulkan Float32 map
  chains, and barrier records for contract/view nodes. Top-level `fusion`
  remains `none`; this does not compile fused shaders, record command buffers,
  execute fused dispatch, or reuse runtime buffers.
- `ML-VK-080-015`: `realize` now executes supported Vulkan `Float32`
  two-scalar-map expression chains as one native command-buffer batch. The
  executor copies the CPU expression source into Vulkan through the existing
  explicit `to-device 'vulkan` graph route, records two map dispatches into one
  command buffer, inserts one shader write/read barrier between the intermediate
  and output dispatch, and submits once. This is not arbitrary
  `tensor/capture` dictionary execution.
- `ML-VK-080-016`: `kernel/run` now has an executable registered-source path
  for Vulkan `Float32` `source-scale-f32` kernels. `Kernel.source` accepts the
  registered source `ml-clip-scale-f32`, `entry` must be `main`, and execution
  goes through the generic Vulkan shader-module dispatch helper. Arbitrary text
  compilation remains split into `ML-VK-080-022`.
- `ML-VK-080-017`: nested Tensor capture `memory-plan` is now version 2 and
  records metadata-only transient reuse groups, reuse policy, runtime
  ownership, and `executes-reuse false`. This does not alias, allocate, retain,
  transfer ownership, or reuse runtime buffers; real runtime reuse is split
  into `ML-VK-080-020`.
- `ML-VK-080-018`: added `tensor/run(graph)` for captured all-Vulkan `Float32`
  source/map graph dictionaries. The executor consumes captured source-node
  tensors and map nodes and replays them through existing Vulkan map helpers.
- `ML-VK-080-019`: `Kernel.source` now accepts checked direct SPIR-V source
  dictionaries as data. The validator accepts unsigned 32-bit word arrays with
  a valid SPIR-V header, registered-source dictionaries, and the
  `ml-clip-scale-f32` symbol form; direct SPIR-V runtime execution fails closed
  until a backend compile/pipeline entrypoint exists.
- `ML-VK-080-021`: `tensor/run(graph)` now also replays captured all-Vulkan
  `Float32` contract nodes and direct `matrix/transpose-view` nodes through
  existing Vulkan helpers. Fused dispatch and command-buffer lowering are split
  into `ML-VK-080-023`.
- `ML-VK-080-020`: `tensor/run(graph)` now detects the captured all-Vulkan
  `Float32` source -> scalar map -> scalar map graph shape and executes it via
  the native Vulkan command-buffer batch helper. This is the first
  runtime reuse/ownership boundary for captured graph execution: scratch
  ownership stays native and one final output handle is transferred to a fresh
  Tensor.
- `ML-VK-080-024`: the runtime reuse path now handles captured linear
  all-Vulkan `Float32` scalar-map chains with two or more map nodes. C3
  validates source -> scalar-map* graph shape and passes scalar/op/mode arrays
  to a native Vulkan scalar-chain executor that owns intermediates and returns
  one final output handle.
- `ML-VK-080-023`: the scalar-chain route now validates captured
  `command-buffer-candidate` metadata before lowering to the native executor.
  Invalid or missing command-batch metadata falls back to serial helper replay,
  so command-buffer execution is no longer inferred from node shape alone for
  this path.
- `ML-VK-080-025`: selected-region runtime reuse now extends beyond linear
  scalar-map chains for captured all-Vulkan `Float32` tensor/tensor map ->
  scalar-map* regions with two dense same-shape concrete source tensors. C3
  validates graph shape plus `command-buffer-candidate` metadata and passes the
  source handles, scalar/op/mode arrays, shape, and strides to a native Vulkan
  selected-region executor that owns intermediates and returns one final output
  handle.
- `ML-VK-080-026`: command-buffer lowering beyond scalar-map metadata is now
  closed for the same dense tensor/tensor map -> scalar-map* selected-region
  boundary. The route validates captured source dependencies, dispatch ids,
  operations, and batch shape before recording the native command buffer.
- Follow-up refactor: `tensor/run` chain detection and selected-region native
  routing helpers were split from `prim_tensor_graph_run.c3` into
  `prim_tensor_graph_run_chains.c3`, leaving the main graph runner at 533 LOC
  and the chain helper module at 146 LOC. This keeps the next runtime-reuse
  slice below the hard code-file gate.
- `ML-VK-080-022`: direct SPIR-V word-array Kernel sources are now executable
  for the checked Vulkan `source-scale-f32` ABI. `kernel/run` copies validated
  `Kernel.source.words` into native scratch and the backend creates a compute
  pipeline from those words for the two-buffer scale dispatch.
- `ML-VK-080-029`: direct SPIR-V source dictionaries now carry an explicit
  checked ABI contract. `source-scale-f32-v1` is accepted for the scale source
  ABI, omitted ABI remains compatible with that scale ABI, and unsupported
  direct-SPIR-V ABI names fail closed during `Kernel` validation.
- `ML-VK-080-030`: source-backed unary `Float32` kernels now execute through
  checked registered and direct word-array `source-unary-f32-v1` SPIR-V
  sources. Registered source uses `name 'map-unary-f32`; direct source must
  declare `abi 'source-unary-f32-v1`.
- `ML-VK-080-033`: direct SPIR-V source dictionaries now support a checked
  binary `Float32` ABI, `source-binary-f32-v1`, for storage2-output1 shaders.
  `kernel/run` validates entry, ABI, optional `storage2-output1-f32-v1`
  metadata, input placement, input/output shapes, and empty push dictionaries
  before dispatching supplied SPIR-V words through the native Vulkan helper.
- `ML-VK-080-034`: selected-region runtime reuse now handles the concrete mixed
  source -> transpose-view plus dense source -> tensor-map -> scalar-map* graph
  family. Native execution owns intermediates and returns one final dense
  Vulkan output handle.
- `ML-VK-080-035`: captured command-batch metadata for that same mixed
  transpose-view/dense-source graph family is now validated before native
  command-buffer lowering; invalid metadata falls back to serial graph replay.
- Contract:
  - `tensor/capture` scheduling metadata is still descriptive only;
  - `direct-helper` means an existing helper-backed node would require a launch
    in a future graph executor, not that capture launches it;
  - executable command-buffer batching currently exists for the narrow Vulkan
    `Float32` two-scalar-map `realize` path and captured `tensor/run` linear
    scalar-map graph paths, dense tensor/tensor map -> scalar-map* graph
    paths, contract -> scalar-map* graph paths, direct transpose-view ->
    scalar-map* graph paths, and the concrete transpose-view/dense-source
    mixed graph path;
  - no fused execution, arbitrary source-language compilation, arbitrary
    direct-SPIR-V descriptor layouts beyond the checked scale/unary/binary
    ABIs, broader mixed DAG region reuse, memory-plan-backed runtime reuse, or
    broad runtime buffer reuse is implemented in this slice.
- Churn correction:
  - the source/custom Kernel lane, selected-region runtime reuse lane, and
    command-buffer lowering lane accumulated repeated case-specific slices while
    preserving the same broader capability as a residual. The case-by-case
    recognizer/ABI/lowering approach is no longer the neutral next step.
- Closed under `ML-VK-080` in this continuation:
  - `ML-VK-080-036` now defines the shared Kernel source/layout contract and
    selected-region planner output that command lowering consumes.
  - Checked direct scale/unary/binary SPIR-V Kernel sources share a
    `kernel-source-layout` metadata dictionary contract with ABI, descriptor
    layout, dtype, input/output count, and optional push-layout validation.
  - `tensor/capture(source)` records a top-level `selected-region-plan` for the
    native scalar-map, tensor-map, direct-view, contract, and concrete mixed
    view/dense-source regions. `tensor/run(graph)` requires a matching
    selected-region candidate and command-batch metadata before entering native
    selected-region executors.
- Future capability boundaries, not active `ML-VK-080` residual children:
  - broader invalidation/capability planning;
  - source-language parsing/compilation, reflection, arbitrary direct-SPIR-V
    descriptor schemas beyond checked scale/unary/binary ABIs,
    memory-plan-backed runtime reuse, arbitrary mixed schedules, and fused
    dispatch execution.
- Negative memory:
  - Do not inline large source-binary direct-SPIR-V execution fixtures in the
    long advanced collections harness. A standalone `--eval` probe for the
    optimized binary-add shader returned `[vulkan 9.0 16.0]`, but putting that
    runtime fixture after the direct scale/unary SPIR-V tests corrupted the
    long harness. Keep constructor/fail-closed suite coverage and use a
    standalone probe or future non-variadic fixture builder for runtime
    source-binary validation.
- Validation completed after the parallel continuation:
  `scripts/build_omni_chelpers.sh`, `c3c build`, focused advanced collections
  `pass=1869 fail=0`, compiler slice `pass=290 fail=0`, basic Lisp slice
  `pass=161 fail=0`, primitive docs parity, Stage 3 source parity, code
  file-size gate, and `git diff --check`. After the graph-run split, `c3c
  build`, focused advanced collections `pass=1869 fail=0`, code file-size gate,
  and `git diff --check` also passed. After direct-SPIR-V execution,
  `scripts/build_omni_chelpers.sh`, `c3c build`, focused advanced collections
  `pass=1870 fail=0`, compiler slice `pass=290 fail=0`, basic Lisp slice
  `pass=161 fail=0`, primitive docs parity, Stage 3 source parity, code
  file-size gate, and `git diff --check` passed. After the checked unary
  source ABI, `scripts/build_omni_chelpers.sh`, `c3c build`, focused advanced
  collections module `pass=1875 fail=0`, compiler slice `pass=290 fail=0`,
  basic Lisp slice `pass=161 fail=0`, primitive docs parity, Stage 3 source
  parity, code file-size gate, and `git diff --check` passed. After the
  direct-view scalar-chain runtime/lowering slice,
  `scripts/build_omni_chelpers.sh`, `c3c build`, focused advanced collections
  module `pass=1879 fail=0`, compiler slice `pass=290 fail=0`, basic Lisp
  slice `pass=161 fail=0`, primitive docs parity, Stage 3 source parity, code
  file-size gate, and `git diff --check` passed. After the source-binary ABI
  and concrete mixed DAG runtime/lowering slice, `scripts/build_omni_chelpers.sh`,
  `c3c build`, focused advanced collections module `pass=1886 fail=0`, and a
  standalone source-binary direct-SPIR-V `--eval` probe returning
  `[vulkan 9.0 16.0]` passed. After the `ML-VK-080-036` shared source-layout
  and selected-region planner closure, `c3c build`, focused advanced
  collections module `pass=1892 fail=0`, primitive docs parity, Stage 3 source
  parity, code file-size gate, `git diff --check`, and the open
  `ML-VK-080-0xx` TODO scan passed with no unchecked child items.

## 2026-04-22 10:47 CEST - Active Audit Checkpoint

- Active hypothesis: backend helper contract bugs are most likely where public
  Tensor wrappers admit device views or optional-state values that lower-level
  helpers interpret as concrete storage or malformed state.
- Current approach: close concrete mismatches with fail-closed front-door
  guards and focused regressions, then continue scanning adjacent helper
  preconditions and ownership cleanup paths.
- Validation path: file-size gate, targeted `git diff --check`, `c3c build
  --obj-out obj`, and the focused `advanced-collections-module` slice.
- Latest checkpoint: `matrix/transpose-view` now enforces concrete
  zero-offset dense row-major Vulkan sources before constructing a Vulkan view;
  nested Vulkan transpose views fail closed with `tensor/backend-unsupported`.
- Negative-memory constraints: do not treat a valid Vulkan device handle as
  proof of concrete zero-offset storage, and do not pass view-backed Vulkan
  tensors to helper ABIs that do not receive full view offset/backing metadata.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 22:55 CEST - Vulkan General Eigen Checkpoint

- Active hypothesis: native Vulkan general `matrix/eigenpairs` is shipped for
  real and fixed-width complex tensors at the covered diagonal, triangular,
  lazy, no-LAPACK, exact 2x2 complex-shift, and mixed-block deflation
  boundaries; the durable TODO live queue is closed.
- Current approach: close `TENSOR-100H-VK-REAL-GENERAL-EIGEN-001`,
  `TENSOR-100H-VK-COMPLEX-GENERAL-EIGEN-HARDENING-001`, and the promoted
  `TENSOR-100H-VK-GENERAL-EIGEN-DEFLATION-001` residual.
- Validation path: shader validation, helper rebuild, `c3c build --obj-out obj`,
  direct exact-shift residual probes, `scripts/run_vulkan_math_perf_probe.sh`,
  focused advanced collection run, file-size gate, status consistency gate, and
  diff whitespace checks.
- Latest checkpoint: exact 2x2 complex-shift and mixed-block deflation
  residual probes pass for `Float64`, `Float32`, `Complex128`, and `Complex64`;
  focused advanced collections are at `pass=2008 fail=17` with eigenpair
  failures cleared and unrelated ML/Vulkan fallback failures remaining.
- Negative-memory constraints: do not treat the old real Vulkan fail-closed
  tests as authoritative, do not keep widening QR shift perturbation for
  mixed-block 3x3 spectra, and do not reconstruct leading 2x2 vectors directly
  into the public output/accumulated-basis buffer before all leading columns are
  computed.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 15:17 CEST - TENSOR-100H Vulkan General Eigen Checkpoint

- Active hypothesis: general non-Hermitian Vulkan `matrix/eigenpairs` needs a
  backend-native solver family; fixed-width CPU result storage is already
  solved, so BigComplex-era blocker wording is stale.
- Current approach: lock the shared ABI/design first, keep capability bits
  false, and advance only to a fail-closed helper boundary until a
  correctness-first Hessenberg/QR shader can be validated with residual checks
  and no-LAPACK counter guards.
- Validation path: targeted diff whitespace check, C helper syntax check,
  helper rebuild, `c3c build --obj-out obj`, bounded focused advanced
  collections, file-size gate, and status consistency.
- Latest checkpoint: fail-closed C helper declarations/stubs and C3 externs now
  exist for `omni_tensor_backend_vulkan_general_eigen_complex128` and
  `omni_tensor_backend_vulkan_general_eigen_complex64`; docs/TODO now name the
  next boundary as the Hessenberg/QR shader implementation rather than the ABI
  stub. The shipped Vulkan Hermitian fixed-width complex eigen tests now also
  guard `zgeev`/`cgeev` call counts, so that backend-native path cannot silently
  regress to LAPACK while general `matrix/eigenpairs` remains fail-closed.
- Negative-memory constraints: no 2x2-only shortcut, realification, Hermitian
  Jacobi reuse, or hidden CPU/LAPACK fallback for Vulkan general eigenpairs.
- Agent assignments: GPT-5 Codex is integration owner; read-only explorers
  Nash and Laplace supplied CPU contract and helper ABI constraints.

## 2026-04-22 15:49 CEST - TENSOR-100H Vulkan General Eigen Closure

- Active hypothesis: the fixed-width complex Vulkan general eigenpair capability
  can ship as a backend-native serial shifted-QR solver with explicit
  no-convergence status. Full Hessenberg staging is now a future numerical
  hardening/performance boundary, not the open capability blocker.
- Current approach: route only `Complex128`/`Complex64` Vulkan
  `matrix/eigenpairs`; keep real-valued Vulkan `Float64`/`Float32`
  `matrix/eigenpairs` fail-closed and keep CUDA/cuBLAS capability bits false.
- Validation path: shader compile/`spirv-val`, C helper syntax checks, helper
  rebuild, C3 build, direct Vulkan probes, bounded focused
  `advanced-collections-module`, file-size gate, and status consistency.
- Latest checkpoint: `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL` is closed on
  disk. Native Vulkan Complex128/Complex64 `matrix/eigenpairs` returns
  Vulkan-placed fixed-width complex `values` and `vectors`, validates residuals
  in focused tests, and does not move LAPACK `zgeev`/`cgeev` counters.
- Negative-memory constraints: continue forbidding hidden CPU/LAPACK fallback,
  arbitrary realification, Hermitian-kernel reuse for non-Hermitian inputs, and
  2x2-only closures.
- Agent assignments: GPT-5 Codex is integration owner; no active subagents.

## 2026-04-22 15:01 CEST - Active Audit Checkpoint

- Active hypothesis: the `TENSOR-100F` parent is closed because its remaining
  Vulkan math baseline work has either shipped, been measured closed, or been
  promoted to a separately named residual. The visible graph-audit diagnostics
  in `memory-lifetime-smoke` are classified expected output from a passing
  negative root-splice regression, not a Tensor/Vulkan blocker.
- Current approach: close `TENSOR-100F-BROAD-VALIDATION-001` with bounded global
  validation evidence, record the runtime fixes that were required for that
  gate to converge, and close `MEM-GRAPH-AUDIT-DIAGNOSTIC-20260422-001` as
  deliberately emitted negative-regression output.
- Validation path: `bash -n scripts/run_global_gates.sh`, targeted
  `git diff --check`, `c3c build --obj-out obj`, bounded
  `allocator-validation`, bounded `advanced`, bounded global gates, file-size
  gate, and status consistency gate.
- Latest checkpoint: bounded global gates passed normal build/tests and FTXUI
  smoke; ASAN is explicitly skipped because the current C3 toolchain reports it
  unsupported for this target. `ast_arena_alloc` now fails closed on corrupt
  current chunks, and JIT tail `List` / `Array` fast paths no longer bypass
  one-argument conversion validation. The narrowed graph-audit probe showed the
  diagnostic lines immediately followed by `[PASS] lifetime: root splice debug
  audit rejects releasing temp edge`.
- Negative-memory constraints: do not use the JIT tail constructor shortcut for
  one-argument `List` / `Array` calls; do not continue AST arena allocation after
  `used > capacity`; do not interpret current `memory-lifetime-smoke`
  graph-audit output as a lazy Tensor return regression unless the neighboring
  pass line or tag pattern changes.
- Agent assignments: main agent owns integration and file edits. Explorer
  subagent `019db548-1039-7391-8234-9ff0ca187168` completed read-only
  isolation of the graph-audit diagnostic source and smallest repro command.

## 2026-04-22 15:18 CEST - Active Audit Checkpoint

- Active hypothesis: the remaining actionable TODO is no longer a vague
  `TENSOR-100F` parent; it is specifically
  `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL`, and the next required boundary is
  a native Vulkan non-Hermitian complex solver ABI before public routing.
- Current approach: record the shared solver design in the eigensolver plan,
  reject case-specific 2x2/triangular shortcuts, and make the next code slice
  fail-closed helper ABI declarations plus validation before any capability bit
  flips.
- Validation path: plan/TODO diff hygiene, status consistency, file-size gate,
  then helper ABI compile checks once code is added.
- Latest checkpoint: `docs/plans/vulkan-eigensolver-plan-2026-04-17.md` now
  chooses staged complex Hessenberg reduction plus implicit shifted QR as the
  shared solver family, with public Vulkan `Complex128`/`Complex64`
  values/vectors outputs and private status buffers.
- Negative-memory constraints: no hidden CPU/LAPACK fallback for Vulkan
  operands; do not realify arbitrary complex matrices; do not reuse the
  Hermitian Jacobi shader as a general eigensolver; do not flip
  `matrix-eigenpairs-complex128/64` capability bits before residual and
  no-LAPACK-counter validation.
- Agent assignments: main agent owns plan integration. Explorer subagents
  `019db54f-969a-7bb0-b5b4-859ca53a6e2d` and
  `019db54f-b85f-7333-8c95-cd78500ff889` are read-only and assigned to confirm
  Vulkan helper/routing constraints and CPU/test contract constraints.

## 2026-04-22 14:10 CEST - CUDA Zero-Offset View Map Checkpoint

- Active hypothesis: the overlapping `TENSOR-100F` stride-aware helper and
  CUDA map residuals should close at a shared semantic boundary instead of
  another case-specific dense-map tweak.
- Current approach: ship zero-offset CUDA transpose views plus stride-offset
  binary map execution while leaving dense-only unary/scientific helpers
  fail-closed for views.
- Validation path: helper rebuild, targeted CUDA layout/map probes, `c3c build
  --obj-out obj`, focused `advanced-collections-module`, bounded-container
  focused validation, file-size gate, and status consistency gate.
- Latest checkpoint: CUDA `matrix/transpose-view` now preserves CUDA read-only
  view metadata for supported dtypes, CUDA binary map builds operand offset
  tables when operand strides differ from dense output strides, and lazy CUDA
  binary map realization keeps zero-offset transpose-view operands on device.
- Negative-memory constraints: do not skip CUDA binary map operand offset
  tables solely because operand and output shapes match; stride equality is
  required. Do not route CUDA unary/scientific map over strided views until
  those helper ABIs accept explicit stride metadata.
- Agent assignments: integration owner is GPT-5 Codex. Read-only sidecar
  `Tesla` reviewed the CUDA view/map slice for dense/unary regressions; final
  integration decisions remained local.

## 2026-04-22 13:51 CEST - Active Audit Checkpoint

- Active hypothesis: after the CUDA SVD adapter boundary shipped, the remaining
  fixed-width complex CUDA SVD TODO could close by adding one cuSOLVER
  execution helper that returns CUDA-owned `u/s/v` device buffers and routing
  all public SVD/norm surfaces through it.
- Current approach: use cuSOLVER `gesvd` only for CUDA Complex128/Complex64
  tensors, keep results on CUDA, and keep the capability fields operation
  specific instead of claiming broad CUDA complex numerical matrix support.
- Validation path: C helper syntax check, helper archive rebuild, `c3c build
  --obj-out obj`, direct CUDA probes, focused `advanced-collections-module`,
  file-size gate, and status consistency gate.
- Latest checkpoint: `TENSOR-100H-CUDA-SVD-NORMS-EXEC` is closed. CUDA
  `matrix/singular-values`, spectral/nuclear `matrix/norm`, and reduced
  `matrix/svd` now route through cuSOLVER for fixed-width complex CUDA tensors.
  Focused advanced collections passed with `pass=1999 fail=0`.
- Negative-memory constraints: no CPU/LAPACK fallback for CUDA operands, do not
  realify fixed-width complex CUDA SVD through doubled real matrices, and do not
  broaden `matrix-numerical-complex128` / `matrix-numerical-complex64` for this
  narrow operation family.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 13:39 CEST - Active Audit Checkpoint

- Active hypothesis: the open CUDA fixed-width complex SVD adapter lane is no
  longer blocked by missing CUDA tools; checked-in adapter PTX can now be
  generated and wired into the existing complex matrix CUDA module.
- Current approach: close the adapter boundary before public routing by adding
  deterministic Omni row-major to cuSOLVER column-major input conversion,
  wide-matrix adjoint-input preparation, cuSOLVER column-major `U` to public
  row-major `u`, and cuSOLVER `VT = V^H` to public row-major `v` conversion
  helpers for Complex128 and Complex64.
- Validation path: CUDA PTX generation and `ptxas`, C helper syntax check,
  helper archive rebuild, `c3c build --obj-out obj`, focused
  `advanced-collections-module`, diff whitespace check, file-size gate, and
  status consistency gate.
- Latest checkpoint: `TENSOR-100H-CUDA-SVD-NORMS-ADAPTERS` is closed. The
  existing CUDA complex matrix PTX module now exposes SVD layout adapter
  kernels and C helper entry points. The generated PTX exceeded 1000 LOC as one
  include and was split into
  `csrc/tensor_cuda_complex_matrix_ptx_part_00.inc` and
  `csrc/tensor_cuda_complex_matrix_ptx_part_01.inc`.
- Negative-memory constraints: do not realify fixed-width complex CUDA SVD
  through doubled real matrices, do not publish cuSOLVER `VT` as public `v`,
  and do not silently copy CUDA operands to CPU/LAPACK. Adapter shape/byte
  guards must run before CUDA availability, allocation, or kernel launch.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 10:56 CEST - Active Audit Checkpoint

- Active hypothesis: backend helper contract bugs are most likely where public
  Tensor wrappers admit layouts or optional state that lower-level helpers
  interpret as plain dense storage.
- Current approach: close concrete helper precondition mismatches with
  fail-closed front-door guards, then continue scanning adjacent device helper
  cleanup and open TODO boundaries.
- Validation path: targeted `git diff --check`, code file-size gate,
  `c3c build --obj-out obj`, and the focused `advanced-collections-module`
  slice.
- Latest checkpoint: CUDA `ml/optimizer-step` SGD now explicitly requires
  dense row-major parameter, gradient, and velocity tensors before fused or
  map-backed CUDA execution, matching CUDA Adam/RMSProp and Vulkan SGD. CUDA
  `tensor-backends` capability reporting now advertises
  `ml-optimizer-sgd-float32` when either fused SGD or map-backed Float32
  execution is available.
- Negative-memory constraints: do not treat a valid CUDA device handle as proof
  that a tensor is safe for byte-linear helper ABIs; future CUDA view/strided
  helpers must accept explicit layout metadata instead of relying on raw
  `byte_len` storage.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 11:09 CEST - Active Audit Checkpoint

- Active hypothesis: backend helper capability bugs are most likely where
  cached native module state is treated as live backend availability.
- Current approach: harden capability probes so they check current backend
  availability before returning cached helper handles, then continue scanning
  adjacent device-helper capability and cleanup paths.
- Validation path: targeted `git diff --check`, code file-size gate,
  `c3c build --obj-out obj`, and the focused `advanced-collections-module`
  slice.
- Latest checkpoint: CUDA map, scientific map, complex map, complex matrix,
  rounding, and ML optimizer resolvers now return unavailable when CUDA is
  disabled for tests, even if their modules were resolved earlier. The
  `tensor-backends` row now reports those operation capabilities false under
  `omni_tensor_backend_cuda_disable_for_tests(1)`.
- Negative-memory constraints: do not treat cached CUDA PTX function pointers
  as live availability authority; public capability probes must honor the
  backend availability/test-disable gate before advertising operation support.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 11:19 CEST - Active Audit Checkpoint

- Active hypothesis: raw helper ABI bugs are most likely where public tensor
  paths resolve views or lazy tensors before dispatching helpers that consume
  linear `byte_len` storage.
- Current approach: preserve public logical view semantics through resolver
  materialization, and add helper-boundary layout guards where raw native
  helpers are not stride-aware.
- Validation path: targeted `git diff --check`, code file-size gate,
  `c3c build --obj-out obj`, and the focused `advanced-collections-module`
  slice.
- Latest checkpoint: Vulkan `ml/mean-squared-error` now has an internal dense
  row-major precondition before raw linear helper dispatch, and the advanced
  regression confirms public MSE materializes Vulkan transpose views logically
  before helper execution.
- Negative-memory constraints: do not assume public MSE passes Vulkan view
  payloads directly to the raw helper; concrete resolution materializes them.
  Conversely, do not call raw linear Vulkan helper boundaries with strided or
  offset tensors unless the helper ABI explicitly accepts layout metadata.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 11:28 CEST - Active Audit Checkpoint

- Active hypothesis: backend helper bugs are most likely where internal
  autograd paths retain device storage and re-label it with new shape metadata
  without revalidating storage layout.
- Current approach: fail closed at internal helper boundaries unless retained
  Vulkan storage and shape metadata are dense row-major and valid for linear
  helper dispatch; keep public semantics covered with focused regressions.
- Validation path: targeted `git diff --check`, code file-size gate,
  `c3c build --obj-out obj`, and the focused `advanced-collections-module`
  slice.
- Latest checkpoint: Vulkan `ml/grad` broadcast-reduction aliasing now requires
  valid dense row-major Vulkan storage before retaining a reduced gradient
  buffer under the child gradient shape. A focused regression confirms the
  aliased singleton broadcast gradient remains a Vulkan dense concrete tensor
  with correct values. The adjacent tensor validation helper source also had a
  duplicated `module`/import prologue removed as non-behavioral source cleanup;
  the same duplicate-prologue pattern was removed from tensor map callable
  helpers, and the `src/lisp` duplicate-module scan is now clean. The
  fixed-width complex plan was also corrected so closed CPU eigen/capability
  slices are marked closed and the CUDA SVD execution order points at the
  concrete adapter/execution TODOs. `scripts/check_status_consistency.sh` now
  understands split TODO, changelog, and area-doc indexes again, and the status
  consistency gate passes with 11 actionable TODOs.
- Negative-memory constraints: do not treat a non-null Vulkan device handle or
  retained buffer as sufficient authority to re-label storage under dense
  tensor metadata. Shape aliasing is only valid with matching element/byte
  counts and dense row-major source/target layout.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 11:51 CEST - Active Audit Checkpoint

- Active hypothesis: device-handle equality is not enough authority for
  autograd tensor identity when future device concrete tensors may carry
  offset/stride metadata over shared storage.
- Current approach: require valid dense zero-offset device storage before
  treating matching non-CPU device handles as the same `wrt` leaf, then keep
  auditing adjacent identity/layout boundaries without crossing the 1000 LOC
  split threshold.
- Validation path: targeted `git diff --check`, code file-size gate,
  `c3c build --obj-out obj`, and focused `advanced-collections-module`.
- Latest checkpoint: `ml_grad_expr_same_tensor` now validates both non-CPU
  concrete tensors with `tensor_has_valid_device_storage` before handle-based
  identity succeeds. `src/lisp/prim_ml_autograd_tensor_expr.c3` is exactly
  1000 LOC after the guard and must not receive further line additions unless
  it is split or equivalent lines are removed.
- Negative-memory constraints: do not treat opaque CUDA/Vulkan handle equality
  as sufficient identity proof for autograd leaf matching; storage validity and
  dense zero-offset layout must be part of the identity precondition.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:00 CEST - Active Audit Checkpoint

- Active hypothesis: split TODO files can preserve live anonymous work rows
  unless status tooling enforces the repo's unique task-ID rule for unchecked
  entries.
- Current approach: assign concrete IDs to all anonymous open nested TODO rows,
  then make `scripts/check_status_consistency.sh` fail future unchecked rows
  that do not start with a backtick task ID.
- Validation path: shell syntax, status consistency gate, anonymous-open-row
  scan, targeted `git diff --check`, and file-size gate.
- Latest checkpoint: all 11 unchecked TODO rows now start with backtick task
  IDs. The checker guard uses portable `rg | awk` filtering rather than
  unsupported regex lookahead.
- Negative-memory constraints: do not add live unchecked TODO bullets under
  `docs/todo_parts/` without a concrete task ID; broad parent items may remain
  open only when their residual child rows are also explicitly ID'd.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:06 CEST - Active Audit Checkpoint

- Active hypothesis: optional backend operands should cross native helper ABIs
  as explicit absence (`null`/`0`), not as unrelated placeholder buffers that
  only satisfy a fixed descriptor layout.
- Current approach: keep public C3 ABI semantics honest and let C helpers map
  absent optional operands to internal descriptor placeholders only after
  validating the operation mode.
- Validation path: targeted diff check, C syntax check, file-size gate, helper
  rebuild, `c3c build --obj-out obj`, and focused
  `advanced-collections-module`.
- Latest checkpoint: Vulkan `ml/scaled-dot-product-attention` no-mask calls now
  pass `null`/`0` for the mask. The native helper accepts that only when
  `mask_kind == 0` and internally reuses the query buffer as the unused mask
  descriptor binding required by the shader layout.
- Negative-memory constraints: do not encode absent optional device operands
  by passing an unrelated live buffer at the language/native boundary; if a
  shader needs a dummy binding, create that dummy mapping inside the native
  helper after mode validation.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:14 CEST - Active Audit Checkpoint

- Active hypothesis: same-device Tensor dtype mismatches are contract errors,
  not backend capability misses, and Vulkan map diagnostics should match the
  already-hardened CUDA path.
- Current approach: keep unsupported dtype families as `tensor/backend-unsupported`
  but classify Vulkan map operands with mismatched Tensor storage dtypes as
  `tensor/dtype-mismatch` before helper dispatch or lazy realization.
- Validation path: targeted diff check, file-size gate, status consistency
  gate, `c3c build --obj-out obj`, and focused `advanced-collections-module`.
- Latest checkpoint: Vulkan direct map and Vulkan lazy-expression map paths now
  report `map: Vulkan tensor dtype mismatch` with code
  `tensor/dtype-mismatch`; a guarded advanced regression covers mixed
  Float64/Float32 Vulkan map operands when both placement dtypes are available.
- Negative-memory constraints: do not hide same-device dtype mismatches behind
  backend-unsupported diagnostics. Reserve backend-unsupported for unavailable
  operation families, unsupported dtypes/layouts, or mixed-device placement
  contracts.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:24 CEST - Active Audit Checkpoint

- Active hypothesis: matrix factorization helper boundaries should fail closed
  on malformed non-empty CPU Tensor storage instead of relying solely on public
  call-site `tensor_has_valid_storage` checks before allocating and copying
  work buffers.
- Current approach: harden the shared workspace/copy helpers used by complex
  Hermitian eigen, general eigen fallback, SVD, LU, determinant, inverse, and
  solve paths so `byte_len`/element-count copies reject null backing storage
  before allocation or `mem::copy`.
- Validation path: targeted diff whitespace check, code file-size gate,
  `c3c build --obj-out obj`, and focused `advanced-collections-module`.
- Latest checkpoint: `src/lisp/prim_tensor_matrix_eigen_primitives.c3`,
  `src/lisp/prim_tensor_matrix_lu_svd_core_b.c3`, and
  `src/lisp/prim_tensor_matrix_lu_cpu_solve.c3` now add explicit malformed
  storage guards at the helper boundary. The touched files remain below the
  1000 LOC split threshold.
- Negative-memory constraints: do not assume a non-zero Tensor byte length or
  shape-derived element count proves a CPU backing pointer exists. Shared
  matrix helpers must check storage before allocating/copying because future
  internal callers may bypass the public entry-point guard.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:32 CEST - Active Audit Checkpoint

- Active hypothesis: CUDA helper launch dimensions must be range-checked before
  converting `size_t` element counts to driver `unsigned int` grid dimensions,
  otherwise oversized tensors can silently under-launch and leave output
  buffers partially written.
- Current approach: replace repeated 1-D launch-grid casts in CUDA map,
  rounding, complex-matrix, and fused-optimizer helpers with a shared checked
  `omni_tensor_cuda_grid_dim_1d` helper, and expose a lightweight native probe
  for regression coverage.
- Validation path: C syntax check for `csrc/tensor_cuda_helpers.c`, targeted
  diff whitespace check, code file-size gate, `c3c build --obj-out obj`, and
  focused `advanced-collections-module`.
- Latest checkpoint: CUDA 1-D helper launches now return
  `OMNI_TENSOR_CUDA_INVALID_ARGUMENT` when the required grid dimension exceeds
  `UINT_MAX`; the advanced collections module includes a probe-backed
  oversized-grid regression.
- Negative-memory constraints: do not calculate CUDA launch grids with
  `(unsigned int)((count + block - 1) / block)` or other unchecked casts. Use
  the shared helper so overflow and driver grid-limit violations fail closed
  before allocation results are exposed as valid outputs.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:36 CEST - Active Audit Checkpoint

- Active hypothesis: CUDA public memory-copy helpers must validate non-empty
  pointer arguments before applying no-op alias shortcuts, otherwise malformed
  null source/destination pairs can be reported as successful copies.
- Current approach: reorder device-to-device copy preconditions so zero-length
  copies remain no-ops, null non-empty pointers fail closed, and only valid
  identical device pointers take the alias no-op path.
- Validation path: C syntax check for `csrc/tensor_cuda_helpers.c`, targeted
  diff whitespace check, code file-size gate, `c3c build --obj-out obj`, and
  focused `advanced-collections-module`.
- Latest checkpoint: `omni_tensor_backend_cuda_copy_device_to_existing_device`
  now rejects `(NULL, nonzero, NULL)` with `OMNI_TENSOR_CUDA_INVALID`; a native
  probe and advanced regression cover the null-alias boundary.
- Negative-memory constraints: do not check source/destination pointer equality
  before validating non-empty CUDA copy pointers. Alias no-ops are valid only
  for non-null device pointers or zero-length copies.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:41 CEST - Active Audit Checkpoint

- Active hypothesis: CUDA map-helper failure paths must release every temporary
  device allocation made before launch-grid validation. The new checked grid
  helper turns previously unreachable oversize paths into explicit early
  returns, so cleanup on those returns is now part of the contract.
- Current approach: audit all CUDA map `grid_status` exits and patch the
  cleanup branches that were returning after allocating broadcast-offset or
  status buffers.
- Validation path: C syntax check for `csrc/tensor_cuda_helpers.c`, targeted
  diff whitespace check, code file-size gate, `c3c build --obj-out obj`, and
  focused `advanced-collections-module`.
- Latest checkpoint: `csrc/tensor_cuda_helpers_map_binary.inc` now frees
  broadcast-offset buffers on scalar/binary grid failure and frees
  offsets/status/output on complex-map grid failure;
  `csrc/tensor_cuda_helpers_map_unary_round.inc` now frees status/output
  buffers on scientific, complex, and round-to-int grid failure. Plain unary
  grid failure still frees only the output buffer because it allocates no
  status buffer.
- Negative-memory constraints: do not add a fail-closed validation branch after
  helper allocations without auditing all already-allocated temporaries on the
  new return path. For CUDA map helpers, grid-size errors are cleanup paths,
  not just argument-validation paths.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:46 CEST - Active Audit Checkpoint

- Active hypothesis: CUDA optimizer native helpers must prove byte-length
  multiplication cannot overflow before comparing `byte_len` to
  `element_count * sizeof(float)`. A wrapped product can otherwise make an
  oversized tensor look like a tiny valid buffer.
- Current approach: harden the fused CUDA SGD helper precondition and expose a
  native no-device probe that exercises the overflow path before CUDA
  availability or allocation is consulted.
- Validation path: C syntax check for `csrc/tensor_cuda_helpers.c`, targeted
  diff whitespace check, code file-size gate, status consistency gate,
  `c3c build --obj-out obj`, and focused `advanced-collections-module`.
- Latest checkpoint: `csrc/tensor_cuda_helpers_ml_optimizer.inc` now rejects
  `element_count > SIZE_MAX / sizeof(float)` before byte-length equality
  checks. `src/lisp/tensor_cuda_backend.c3` exposes
  `omni_tensor_backend_cuda_ml_sgd_byte_len_overflow_guard_for_tests`, and the
  advanced collections module verifies the overflow case returns
  `OMNI_TENSOR_CUDA_INVALID`.
- Negative-memory constraints: do not compare a user-provided byte length to a
  size-derived multiplication until the multiplication has been bounded. In
  native CUDA optimizer helpers, overflow must fail closed before backend
  availability, allocation, or launch logic.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:51 CEST - Active Audit Checkpoint

- Active hypothesis: Vulkan map-chain dispatch helpers must share the same
  checked size-arithmetic rule as CUDA optimizer helpers: validate
  `element_count` bounds before comparing `byte_len` to
  `element_count * sizeof(float)`.
- Current approach: factor the Vulkan Float32 map-chain byte-length/count
  validation into one helper, use it in both scalar-chain and
  tensor-scalar-chain dispatch entry points, and expose a native no-device
  regression probe.
- Validation path: C syntax check for
  `csrc/tensor_vulkan_helpers_dispatch_batch.c`, targeted diff whitespace
  check, code file-size gate, status consistency gate, `c3c build --obj-out
  obj`, and focused `advanced-collections-module`.
- Latest checkpoint: `csrc/tensor_vulkan_helpers_dispatch_batch.c` now rejects
  oversized map-chain Float32 element counts before byte-length multiplication.
  `src/lisp/tensor_vulkan_backend_batch.c3` exposes
  `omni_tensor_backend_vulkan_map_chain_f32_byte_len_overflow_guard_for_tests`,
  and the advanced collections module verifies the guard returns
  `OMNI_TENSOR_VULKAN_UNSUPPORTED`.
- Negative-memory constraints: do not combine `byte_len != count * sizeof(T)`
  and `count` bounds in an order where the multiplication is evaluated first.
  Place size/count guards in a shared helper when multiple Vulkan/CUDA entry
  points enforce the same byte-length contract.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 12:58 CEST - Active Audit Checkpoint

- Active hypothesis: the native libffi shim must fail closed before
  dereferencing caller-supplied argument metadata, and closure allocation must
  never leave stale output pointers on failure. These are native boundary
  contracts independent of higher-level FFI argument validation.
- Current approach: harden `csrc/ffi_helpers.c` preconditions for fixed,
  variadic, and callback-closure calls; expose no-crash native probes through
  the existing FFI callback extern surface; validate them in the advanced FFI
  system surface group.
- Validation path: C syntax check for `csrc/ffi_helpers.c`, targeted diff
  whitespace check, helper archive rebuild, `c3c build --obj-out obj`, focused
  `advanced-ffi-system-surface`, file-size gate, and status consistency gate.
- Latest checkpoint: `omni_ffi_call` and `omni_ffi_call_var` now reject null
  argument type/value vectors when `nargs > 0`; `omni_ffi_closure_alloc`
  clears `out_closure`/`out_code` on entry and clears `out_code` again if
  `ffi_prep_closure_loc` fails after libffi allocated executable code.
  `src/lisp/prim_ffi_callback.c3` also bounds callback parameter counts before
  allocating type tables or narrowing counts into the native libffi `int` ABI,
  exposes three native guard probes, and the advanced FFI surface group
  verifies all three. The surrounding declarative FFI setup paths now mirror
  that contract: interpreter FFI binding construction rejects oversized
  parameter metadata before allocation, and the AOT bridge guards both ABI-tag
  and handle-policy tables.
- Negative-memory constraints: do not assume C3-level FFI call preparation is a
  sufficient guard for the C shim. Native helper entry points must defend their
  own pointer-vector and output-parameter contracts because tests, async paths,
  callback paths, and future AOT/native callers can reach them directly.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 13:11 CEST - Active Audit Checkpoint

- Active hypothesis: ML optimizer backend wrappers must not manufacture
  concrete device tensors when a native helper returns a null output handle.
  Empty optimizer tensors are already rejected before helper dispatch, so a
  null output handle after a successful Vulkan optimizer helper is a broken
  backend contract, not a valid zero-byte tensor.
- Current approach: compare the CUDA and Vulkan optimizer output wrappers,
  align Vulkan with CUDA's null source/output guard, and verify the focused ML
  optimizer test surface.
- Validation path: targeted diff whitespace check, `c3c build --obj-out obj`,
  focused `advanced-collections-module`, file-size gate, and status
  consistency gate.
- Latest checkpoint: `src/lisp/prim_ml_optimizer_vulkan.c3` now rejects
  `source == null` or `device_handle == null` in
  `ml_optimizer_vulkan_tensor_value`, frees any supplied handle on metadata
  allocation failure, and assigns `tensor_vulkan_device_finalizer` only after
  the output handle has been proven non-null.
- Negative-memory constraints: do not treat a null Vulkan optimizer output
  handle as an acceptable concrete tensor state. Optional state outputs should
  be skipped by the caller before wrapping; required optimizer outputs must
  fail closed if the helper did not produce device storage.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 13:17 CEST - Active Audit Checkpoint

- Active hypothesis: Vulkan ML loss wrappers have the same required-output
  invariant as optimizer wrappers. A successful scalar loss helper must return
  real Vulkan result storage; a null output handle is a backend contract
  failure, not a valid concrete Tensor.
- Current approach: harden the MSE and cross-entropy Vulkan loss wrappers after
  successful native status returns, before installing concrete Tensor metadata.
- Validation path: targeted diff whitespace check, `c3c build --obj-out obj`,
  focused `advanced-collections-module`, file-size gate, and status
  consistency gate.
- Latest checkpoint: `src/lisp/prim_ml_vulkan_losses.c3` now rejects successful
  `ml/mean-squared-error` or `ml/cross-entropy` Vulkan helper returns with
  `out_device == null`, frees the partially allocated Tensor payload, and only
  installs `tensor_vulkan_device_finalizer` after required result storage has
  been proven non-null.
- Negative-memory constraints: do not represent successful Vulkan scalar loss
  results as `TENSOR_PAYLOAD_CONCRETE` with `device_handle == null`. The native
  helpers reject empty element counts, so null required output storage after a
  successful status is invalid and must fail closed.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 13:26 CEST - Active Audit Checkpoint

- Active hypothesis: Vulkan scaled-dot-product attention is another required
  non-empty ML output wrapper. Its validation rejects zero query/key/head/value
  dimensions, so a successful native helper return with a null output handle is
  invalid concrete Tensor metadata.
- Current approach: harden the attention Vulkan wrapper at the same post-status
  boundary as optimizer and loss wrappers, without touching generic Tensor paths
  where zero-byte device handles may still be valid.
- Validation path: targeted diff whitespace check, `c3c build --obj-out obj`,
  focused `advanced-collections-module`, file-size gate, and status
  consistency gate.
- Latest checkpoint: `src/lisp/prim_ml_attention.c3` now rejects a successful
  Vulkan attention helper return with `out_device == null`, frees the partial
  Tensor payload, and only installs `tensor_vulkan_device_finalizer` after
  required result storage has been proven non-null.
- Negative-memory constraints: do not blanket-rewrite conditional finalizers in
  generic Tensor helpers without first proving zero-sized output semantics. For
  ML attention, the output is required non-empty after validation, so null
  result storage after success must fail closed.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-22 16:38 CEST - Vulkan Eigen Follow-Up TODO Reopen

- Active hypothesis: the shipped Vulkan fixed-width complex general
  `matrix/eigenpairs` path is closed, but two residual risks from the final
  handoff need explicit backlog ownership: real-valued general eigenpairs still
  fail closed, and the serial complex QR path needs measurement-gated hardening
  for larger or difficult spectra.
- Current approach: reopen the TODO live queue with two scoped items rather
  than reopening `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL`.
- Validation path: status consistency gate, TODO checkbox scan, and targeted
  diff whitespace checks for the touched planning artifacts.
- Next checkpoint: either implement
  `TENSOR-100H-VK-REAL-GENERAL-EIGEN-001`, or run/extend the Vulkan math probe
  for `TENSOR-100H-VK-COMPLEX-GENERAL-EIGEN-HARDENING-001` and close or
  escalate based on measured evidence.
- Negative-memory constraints: do not use hidden CPU/LAPACK fallback, Hermitian
  reuse, 2x2 shortcuts, or capability-bit flips before native non-Hermitian
  real eigenpairs pass residual/no-LAPACK tests. Do not add another
  case-specific complex QR tweak before measurements show a shared bottleneck.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request only asked to record TODO items.

## 2026-04-22 13:36 CEST - Active Audit Checkpoint

- Active hypothesis: Vulkan conv1d, conv2d, and pool2d wrappers also produce
  required non-empty ML outputs after validation. A successful native helper
  return with a null output handle would create invalid concrete Tensor
  metadata.
- Current approach: harden only the convolution/pooling ML wrappers whose
  validation rejects zero batch/channel/spatial/output dimensions. Leave
  reduction and normalization conditional-finalizer paths unchanged until a
  dedicated zero-element semantics review proves which outputs are required.
- Validation path: targeted diff whitespace check, `c3c build --obj-out obj`,
  focused `advanced-collections-module`, file-size gate, and status
  consistency gate.
- Latest checkpoint: `src/lisp/prim_ml_conv.c3`,
  `src/lisp/prim_ml_conv2d.c3`, and `src/lisp/prim_ml_pool2d.c3` now reject
  successful Vulkan helper returns with `out_device == null`, free partial
  Tensor payloads, and install `tensor_vulkan_device_finalizer` only after
  required result storage has been proven non-null.
- Negative-memory constraints: do not extend the required-output fail-closed
  rule to `prim_ml_reduction.c3`, `prim_ml_stable_reduction.c3`, or
  `prim_ml_normalization.c3` without first proving zero-element semantics for
  those public operations.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request did not explicitly ask for
  subagents in this turn.

## 2026-04-23 06:50 CEST - Slash Surface Naming Audit Plan

- Active hypothesis: slash names should stay for compact, always-present core
  families, but broad surfaces need either explicit exceptions or real module
  boundaries.
- Current approach: track the naming cleanup as
  `docs/plans/slash-surface-naming-audit-plan-2026-04-23.md` with TODO-backed
  items instead of performing broad primitive renames in this planning turn.
- Validation path: documentation-only plan checks use `git diff --check` and
  `scripts/check_status_consistency.sh`; primitive renames require build,
  compiler slice, affected runtime slice, primitive-doc parity, status
  consistency, and file-size gate.
- Next checkpoint: resolve `SURFACE-NAMING-001` and `SURFACE-NAMING-002` first:
  document Pika as language-core and write the Deduce module-boundary decision.
- Negative-memory constraints: do not treat slash as module lookup; do not add
  deep slash pseudo-paths to avoid real module boundaries; do not convert Pika
  into an optional module because the owner clarified Pika is part of the
  language substrate.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request asked for a plan, not parallel
  implementation.

## 2026-04-23 07:25 CEST - Slash Surface Naming Integration

- Active hypothesis: the naming audit should close every resolved design slice
  and leave only product-level math special-function naming open.
- Current approach: integrate the three parallel documentation workers and the
  local `ml/linear-batched-reduce` runtime rename under one final state update.
- Validation path: `c3c build --obj-out obj`, focused advanced
  collections-module slice, compiler slice, direct eval smoke, primitive-doc
  parity, status consistency, file-size gate, and `git diff --check`.
- Latest checkpoint: Pika is documented as language-core; Deduce has an
  accepted module/facade decision; ML has an accepted split between core
  `ml/...` tensor primitives and future `ml.visualization.*` /
  `ml.optimizers.*` facades; `ml/linear-batched-reduce` is now the only public
  batched linear symbol in runtime registration, AOT lookup, tests, docs, and
  diagnostics; ML/NN activation docs distinguish eager tensor operations from
  layer-spec constructors.
- Negative-memory constraints: do not reintroduce
  `ml/linear/batched-reduce`; do not add compatibility aliases without an
  owner-approved migration window; do not treat the ML visualization/optimizer
  decisions as implemented runtime facades yet.
- Agent assignments: main Codex thread is integration owner. Kierkegaard owned
  Pika docs (`SURFACE-NAMING-001`), Chandrasekhar owned the Deduce decision
  (`SURFACE-NAMING-002`), Dewey owned the ML split decision
  (`SURFACE-NAMING-003`), and the main thread owned `SURFACE-NAMING-004`,
  `SURFACE-NAMING-006`, final state, and validation.

## 2026-04-23 07:45 CEST - Math/Stats Scientific Module Plan

- Active hypothesis: because `math` and `stats` will grow into core
  scientific surfaces, the final canonical API should be real module/facade
  access rather than global special-function names or permanent slash
  primitive families.
- Current approach: close `SURFACE-NAMING-005` as a design decision and track
  implementation under
  `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`.
- Validation path: documentation-only checks for this slice are
  `git diff --check` and `scripts/check_status_consistency.sh`; future runtime
  facade slices need build, compiler lookup/codegen tests, focused scientific
  Tensor runtime tests, parity/status/file-size gates, and Vulkan shader/helper
  validation where backend behavior changes.
- Latest checkpoint: current bare elementary/rounding/complex/integer helpers
  map to target `math.*`; current `math/lgamma`, `math/erf`, and `math/erfc`
  map to `math.lgamma`, `math.erf`, and `math.erfc`; current
  `stats/normal-cdf` and `stats/normal-quantile` map to
  `stats.normal-cdf` and `stats.normal-quantile`. TODO now tracks module
  facade implementation, docs/tests migration, old-callable removal/explicit
  prelude decision, Vulkan math special functions, and Vulkan stats
  distribution alignment.
- Negative-memory constraints: do not add global scientific special-function
  names; do not add deeper slash pseudo-paths for scientific modules; do not
  expose backend-specific Vulkan call names; do not treat broad
  `scientific-map-*` capability as proof of every `math.*`/`stats.*` function.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request asked for planning, not explicit
  parallel implementation.

## 2026-04-23 07:57 CEST - Core Math/Stats Module Facades

- Active hypothesis: `math` and `stats` should be true core module values,
  not source-relative `.omni` wrappers or deeper slash pseudo-paths.
- Current approach: create root-owned module records during primitive
  initialization, bind module exports directly to existing primitive values,
  and keep slash special/stat primitive names as transitional migration inputs.
- Validation path: `c3c build --obj-out obj`, direct prebound/import smokes,
  `/tmp` script import smoke, focused advanced collections-module slice,
  status/manifest/docs gates, and diff whitespace check.
- Latest checkpoint: `math` and `stats` are prebound module values. `math`
  exports elementary math, rounding, complex helpers, integer helpers,
  `lgamma`, `erf`, and `erfc`; `stats` exports `normal-cdf` and
  `normal-quantile`. Runtime tests cover direct access, import rebinding,
  selective import, Tensor behavior, map-callable behavior, and that importing
  `math` does not replace global `abs`.
- Negative-memory constraints: do not implement core scientific facades by
  loading `lib/math.omni` / `lib/stats.omni` self-alias wrappers; that approach
  triggered global method-table fallback poisoning through module-body `define`.
  Method-table fallback replacement must check the current frame only.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the latest owner request was a continuation and the immediate
  blocking work was local integration/validation.

## 2026-04-23 08:20 CEST - Math/Stats Docs and Tests Migration

- Active hypothesis: after prebinding `math` and `stats`, active tests and docs
  should exercise the canonical dotted module surface before slash-callable
  removal is attempted.
- Current approach: migrate active scientific tests/docs and diagnostic text to
  `math.*` / `stats.*`; keep slash registrations and backend callable keys as
  compatibility internals for `MATHSTATS-MODULE-003`.
- Validation path: build, focused advanced collections-module, focused
  advanced stdlib numeric, focused compiler slice, primitive docs parity, E2E
  baseline policy, file-size gate, status consistency, and diff whitespace.
- Latest checkpoint: `MATHSTATS-MODULE-002` is closed. Active runtime tests,
  numeric tests, CUDA/Vulkan scientific map docs, scientific Tensor docs, and
  diagnostics now use dotted module names. Active docs retain only explicit
  compatibility notes for old slash spellings.
- Negative-memory constraints: do not remove slash registrations without
  preserving backend callable recognition for module-exported primitive values
  and adding negative tests for removed public slash spellings.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because the work was a contiguous migration over the same surface.

## 2026-04-23 08:35 CEST - Math/Stats Slash Callable Removal

- Active hypothesis: after tests/docs moved to dotted module calls, the old
  slash scientific special/stat primitives should be removed rather than kept
  as aliases.
- Current approach: remove public slash primitive registrations and AOT lookup
  entries; make `math` / `stats` module exports own dotted-name primitive
  values; update backend callable recognition to the dotted primitive names.
- Validation path: build, direct dotted/removed-slash smokes, focused advanced
  collections-module, focused advanced stdlib numeric, focused compiler slice,
  primitive docs parity, E2E baseline policy, file-size gate, status
  consistency, and diff whitespace.
- Latest checkpoint: `MATHSTATS-MODULE-003` is closed. Old public slash
  spellings fail, module calls and `map math.erf` continue to work, and backend
  callable recognition no longer depends on public slash bindings.
- Negative-memory constraints: do not reintroduce the old slash special/stat
  callables as compatibility aliases without an explicit owner-approved
  migration window.
- Agent assignments: direct execution in this continuation; no sub-agent was
  spawned because this was a tightly coupled follow-on to the same migration.

## 2026-04-23 09:20 CEST - Vulkan Float32 Math Erf/Erfc

- Active hypothesis: the first Vulkan special-function math boundary should be
  dense row-major `Float32` `math.erf` / `math.erfc`, using the existing
  backend-neutral `math.*` surface and fail-closed `Float64` behavior.
- Current approach: add op ids `17` / `18` to the Vulkan Float32 unary shader,
  regenerate SPIR-V, widen native Float32 unary helper whitelists through op
  `19`, and route module primitive callables plus direct Tensor unary math to
  those op ids. The checked `kernel/run` unary helper also exposes `erf-f32`
  and `erfc-f32` for the same shader path.
- Validation path: shader compile and `spirv-val`, helper rebuild, `c3c build`,
  direct Vulkan smokes, focused advanced collections-module, focused compiler,
  docs/status/file-size/static gates, and diff whitespace.
- Latest checkpoint: `MATHSTATS-VK-001` is closed for the Float32 semantic
  boundary. Vulkan `Float64` `math.erf` / `math.erfc` remains fail-closed
  pending a documented double approximation policy. Remaining active math/stats
  work is `MATHSTATS-VK-002`: granular capability reporting while preserving
  the existing `stats.*` behavior and diagnostics.
- Negative-memory constraints: do not infer Vulkan `Float64` error-function
  support from the Float32 shader; do not add backend-named public calls; do
  not treat broad `scientific-map-*` as proof that every `math.*` / `stats.*`
  function is supported.
- Agent assignments: direct integration owner was this session. No sub-agent
  was spawned for this continuation because the immediate blocking work was
  shader/helper/test integration in a tightly coupled surface.

## 2026-04-23 09:45 CEST - Granular Math/Stats Backend Capabilities

- Active hypothesis: broad `scientific-map-*` and legacy `stats-normal-*`
  fields are too coarse for the new `math` / `stats` module surface; callers
  need separate fields for elementary math, error functions, and stats
  distributions.
- Current approach: keep the old fields stable for compatibility, add
  `math-elementary-*`, `math-error-function-*`, and
  `stats-distribution-*` fields to every `tensor-backends` entry, and make
  Vulkan report only the support it actually has: Float32 elementary and
  error-function support, Float64/Float32 stats distribution support, and no
  Float64 elementary/error-function claim.
- Validation path: build, focused advanced collections-module capability
  tests, docs/status/file-size/static gates, and diff whitespace.
- Latest checkpoint: `MATHSTATS-VK-003` is closed as a policy decision.
  Vulkan `Float64` `math.erf` / `math.erfc` remain fail-closed until a
  validated double approximation contract exists, and Vulkan `math.lgamma`
  remains a separate hardening item with its own policy.
- Negative-memory constraints: do not name the field `math-special-*`
  because that implies `math.lgamma`; use `math-error-function-*` for
  `math.erf` / `math.erfc` coverage. Do not treat `scientific-map-*` as
  complete coverage. Do not infer Vulkan `Float64` error-function support from
  the `Float32` shader path.
- Runtime note: `stats.normal-quantile` Vulkan `Float64` was initially
  blocked by a helper whitelist mismatch. The public mapping uses op `20`, so
  `omni_tensor_backend_vulkan_map_unary_f64` now permits op `20` while
  keeping `math.erf` / `math.erfc` fail-closed on `Float64`.
- Agent assignments: direct integration owner was this session. No sub-agent
  was spawned for this tightly coupled continuation.

## 2026-04-23 10:15 CEST - Tagged Switch Exhaustiveness Remediation

- Active hypothesis: the pane-captured defaults are the remaining audit-class
  exhaustiveness gaps, not independent one-off bugs. The fix should close the
  whole switch/default class across the captured compiler/parser/tensor files.
- Current approach: treat the queue as all-or-nothing. Remove every hidden
  `default:` in the listed audit files or move the unknown branch behind a
  named fail-closed helper with tests; do not preserve silent catch-alls.
- Validation path: `c3c build --obj-out obj`, targeted compiler/parsing/tensor
  regression slices, `rg -n "default:"` over the audited files, and
  `git diff --check`.
- Latest checkpoint: the remediation plan and TODO queue are now recorded in
  `docs/plans/tagged-switch-exhaustiveness-remediation-plan-2026-04-23.md`
  and `docs/todo_parts/todo_part_15.md`.
- Negative-memory constraints: do not treat a visible `default:` as
  acceptable unless it has been converted into a named, tested fail-closed
  helper. Do not leave any of the audited files in a partially cleaned state.
- Agent assignments: direct integration owner was this session.
