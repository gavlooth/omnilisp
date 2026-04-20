# Agent Plan Index

`.agents/PLAN.md` remains the operational plan entrypoint; read the indexed part files for the full plan state.

The historical content was split mechanically to keep individual files below the 700-line repository limit. Content order is preserved in the part files.

## Parts

- Part 01: [.agents/plan_parts/plan_part_01.md](plan_parts/plan_part_01.md) (341 lines)
- Part 02: [.agents/plan_parts/plan_part_02.md](plan_parts/plan_part_02.md) (624 lines)
- Part 03: [.agents/plan_parts/plan_part_03.md](plan_parts/plan_part_03.md) (353 lines)
- Part 04: [.agents/plan_parts/plan_part_04.md](plan_parts/plan_part_04.md) (395 lines)
- Part 05: [.agents/plan_parts/plan_part_05.md](plan_parts/plan_part_05.md) (55 lines)

## Current Checkpoint

Date: 2026-04-19

- All jj-tracked text files above 700 LOC have been split into indexed part
  files, include-only wrappers, or ordered data shards. The root entrypoints
  remain under the cap.
- The current raw jj-tracked text inventory reports zero files above 700 LOC.
  This includes vendored and generated text that earlier checkpoints treated
  as exclusions.
- Audit fixes landed during this continuation:
  - corrected index links so part links resolve relative to each stub file;
  - restored the LSP smoke extended-formatting assertion loop after a split had
    reduced it to `pass`;
  - preserved the Tensor `realize` evaluated-error propagation fix from the
    prior source audit.
- Validation path for this checkpoint:
  - raw jj-tracked text LOC inventory with no vendor/generated exclusions;
  - generated index-link existence check;
  - `git diff --check`;
  - targeted build/smoke gates for the earlier source changes.

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
  - froze `ml/linear/batched-reduce` as the public batched-reduction surface.
- Validation:
  - `c3c build`
  - focused advanced collections suite: `pass=1622 fail=0`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
  - `scripts/check_file_size_gate.sh`
- Next checkpoint:
  - implement `ML-VK-010-004-001`: Vulkan `Float32`
    `ml/linear/batched-reduce` coverage with no-hidden-CPU-fallback
    regressions.

## Active Vulkan ML Batched Reduce Checkpoint

Date: 2026-04-20

- Audit finding:
  - `ml/linear/batched-reduce` was frozen in planning but not registered as a
    runtime or AOT primitive.
  - Public docs and TODO acceptance did not spell out rank, capability, and
    no-fallback diagnostics for the new surface.
- Implemented checkpoint:
  - added `ml/linear/batched-reduce` as a callable primitive and AOT lookup;
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
  - `ml/linear` and `ml/linear/batched-reduce` now resolve Vulkan-only
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
    `ml/linear/batched-reduce`, or record a concrete blocker with fail-closed
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
  - widened the narrow Vulkan `ml/linear`/`ml/linear/batched-reduce` path to
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
- Current approach:
  - ship CPU `Float64`/`Float32` axis reductions with truthful capability bits;
  - fail closed for CUDA/Vulkan until backend-specific axis kernels exist;
  - keep `ML-VK-020-006-VK` open for the real Vulkan reduction substrate.
- Validation path:
  - `c3c build`
  - focused advanced collections suite
  - docs parity, whitespace, and file-size gates before commit/push.
- Next checkpoint:
  - use the landed CPU reducer for `ML-VK-020-007` softmax/loss CPU semantics,
    or implement `ML-VK-020-006-VK` first if claiming Vulkan reduction
    capability.
