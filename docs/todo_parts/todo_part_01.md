# Active TODO Index Part 01

Source: `TODO.md`

# Active TODO

Last condensed: 2026-04-12

This file is now the sole live backlog.
The Live Queue lists only still-open work.

Current live parent count: 2
Explicit deferred subtasks are listed under their active parent items.

Completed backlog snapshots:

- `docs/TODO-backup-2026-03-26.md`
- `docs/TODO-backup-2026-03-31.md`
- `docs/TODO-backup-2026-04-01.md`
- `docs/TODO-backup-2026-04-08.md`

Recently Closed is retained below as a short audit trail.

## Live Queue

- [ ] `OWNERSHIP-HARDENING-001` codify and harden the completed
  TEMP/ESCAPE ownership model
  - objective: treat the scope/region dual-lane model as architecturally
    complete, then reduce future drift by making the contract easier to find,
    review, test, and observe.
  - authoritative baseline: `memory/DESTINATION_ARENA_PLAN.md` Revision XXI/XXII
    plus `memory/CHANGELOG.md`; the root `discussion` file is design
    brainstorm context, not implementation truth.
  - guardrail: do not add a new user-visible ownership syntax, per-object RC
    for ordinary language values, generalized root pinning, or a pure static
    escape-analysis correctness model while working this parent.
  - deferred TODOs under this parent:
    - [ ] Add a concise accepted memory-model ADR to `docs/ARCHITECTURE.md`.
      - blocker/defer reason: the current truth is distributed across
        `memory/DESTINATION_ARENA_PLAN.md`, `memory/CHANGELOG.md`,
        `docs/LANGUAGE_SPEC.md`, tests, and implementation files; future
        agents should not need to reconstruct the contract from history.
      - concrete next step: add an ADR that states `ScopeRegion` is the
        ownership unit, each region has `TEMP` and `ESCAPE` lanes, `TEMP` is
        transient, `ESCAPE` is for boundary survivors, committed ESCAPE roots
        must not retain Omni-owned TEMP edges, boundary copy/promotion uses a
        shared promotion context, `scope_adopt` is retired from normal return
        flow, ordinary language values must not gain per-type RC, and
        foreign-resource handles are the narrow exception.
      - validation: `git diff --check -- docs/ARCHITECTURE.md`; if wording
        changes public contracts, also run docs/spec parity checks.
    - [ ] Add an ownership-boundary checklist to `docs/C3_STYLE.md`.
      - blocker/defer reason: new graph-carrying tags, wrappers, Tensor
        payloads, iterators, closures, and FFI objects can accidentally bypass
        copy/promotion/destruction/audit rules unless review has a concrete
        checklist.
      - concrete next step: add a checklist for new owned-edge runtime types:
        identify every `Value*`/`Env*`/Omni-owned child, boundary-copy route,
        ESCAPE-promotion route, root-store clone route, graph-audit traversal
        or opaque exclusion, destructor authority, rollback behavior, and
        return/env/capture/destruction regression coverage.
      - validation: `git diff --check -- docs/C3_STYLE.md`.
    - [ ] Expose or print first-class memory boundary statistics for debug
      builds.
      - blocker/defer reason: correctness is tested, but performance drift can
        silently convert O(1) return paths into fallback copies without a
        compact runtime summary.
      - concrete next step: add a debug-only primitive or CLI/debug dump for
        copy-site counts, destination ESCAPE commit outcomes, splice
        success/reject counts, promotion abort counts, max/budget hits, TEMP
        bytes allocated, ESCAPE bytes spliced, fallback-copy count,
        graph-audit rejection count, and fiber-TEMP eligible/bypass counts.
      - validation: targeted memory-lifetime stats tests, `c3c build`, focused
        memory-lifetime suite, and `git diff --check`.
    - [ ] Codify the durable-graph rule: temporary graphs may be cyclic;
      durable published forms should trend tree/DAG plus interned atoms.
      - blocker/defer reason: the root discussion captured this useful design
        rule, but it is not yet a documented project-facing review constraint
        for new long-lived structures.
      - concrete next step: add a short rule to the architecture/style docs
        stating that arbitrary durable cycles require explicit justification
        and that published AST/IR/query-plan/library state should prefer
        indices, handles, arrays, compact records, interned symbols/shapes, or
        immutable atoms when practical.
      - validation: `git diff --check` on the touched docs.
    - [ ] Prefer handles/IDs at durable subsystem API boundaries and document
      the policy.
      - blocker/defer reason: the core runtime should not be rewritten around
        handles, but durable subsystem APIs still need a consistent policy for
        Tensor backend buffers, FFI handles, scheduler/server handles, query DB
        handles, and long-lived mutable graph stores.
      - concrete next step: document the rule that ordinary `Value*` and
        runtime pointers are fine within a scope/lane, while cross-subsystem or
        cross-lifetime APIs should prefer stable handles/IDs with one explicit
        release/finalizer authority.
      - validation: docs `git diff --check`; for any implementation slice,
        add handle lifetime/finalizer regression coverage.
    - [ ] Require boundary regression tests for every new graph-carrying
      `ValueTag` or owned-edge wrapper.
      - blocker/defer reason: the biggest remaining ownership risk is future
        drift from new tags or wrappers that are not covered by return,
        closure/env, root-store, ESCAPE-promotion, rollback, destructor, and
        graph-audit tests.
      - concrete next step: add the requirement to docs and, if useful, create
        a reusable test checklist/template under the memory-lifetime test
        groups for return survival, closure/env capture, root/global store,
        no TEMP edge under committed ESCAPE roots, allocation-failure rollback,
        destructor safety, and audit traversal/opaque exclusion.
      - validation: focused memory-lifetime tests for the template/example,
        `c3c build`, and `git diff --check`.
    - [ ] Keep static allocation routing as an optimization layer, not the
      correctness foundation.
      - blocker/defer reason: compile-time escape analysis can reduce
        promotion work, but Omni's dynamic Lisp surfaces, continuations,
        effects, mutation, FFI, and root stores still require runtime boundary
        enforcement.
      - concrete next step: document this in the memory ADR or C3 style guide,
        then only add static/JIT allocation-intent improvements behind the
        existing runtime boundary invariants and tests.
      - validation: docs `git diff --check`; for implementation slices,
        compare copy-site/commit counters before and after plus focused
        memory-lifetime tests.
    - [ ] Normalize boundary vocabulary in docs without broad symbol churn.
      - blocker/defer reason: names such as copy, promotion, destination,
        escape, commit, and splice still carry historical baggage even though
        the implemented model is coherent.
      - concrete next step: add a glossary covering boundary commit,
        destination ESCAPE build, promotion, splice, fallback copy, reusable
        target-chain value, releasing TEMP, releasing ESCAPE, and
        mixed/uncertain provenance.
      - validation: docs `git diff --check`; avoid renaming code symbols unless
        a separate implementation task proves it is worth the churn.
    - [ ] Make opaque-payload ownership policy explicit.
      - blocker/defer reason: opaque wrappers are safe only when they either
        own no Omni values, expose explicit traversal/copy/promotion/destructor
        hooks, or fail closed at boundary promotion.
      - concrete next step: document the policy and audit existing opaque
        payload families (`FFI_HANDLE`, backend/device handles, primitive
        payloads, and Tensor backend payloads) against it.
      - validation: docs `git diff --check`; for any audit fixes, focused
        boundary/root-store/promotion tests plus `c3c build`.
    - [ ] Add ownership-model examples for common runtime shapes.
      - blocker/defer reason: abstract invariants are present, but future
        implementation work would benefit from concrete examples showing where
        allocation happens and what boundary commit must prove.
      - concrete next step: document examples for returning a list, returning
        an iterator, closure capture, root/global define, Tensor lazy
        expression, FFI handle, opaque foreign resource, mutable collection
        update, and failure during ESCAPE publication.
      - validation: docs `git diff --check`; link examples from the new ADR or
        `docs/C3_STYLE.md` checklist.
- [ ] `TENSOR-100F` continue the Vulkan math library baseline
  - objective: grow the current correctness-first Vulkan backend into an
    Omni-owned portable math library while preserving the existing
    backend-neutral public surface.
  - plan: `docs/plans/vulkan-math-library-roadmap-2026-04-17.md`.
  - public surface must remain `Tensor`, `to-device`, `device`,
    `tensor-backends`, `map`, `contract`, and existing `matrix/*` operations.
    Do not add `VulkanTensor`, `vulkan/solve`, backend-named math APIs, hidden
    CPU/GPU transfers, or silent dtype downcasts.
  - non-Docker execution order:
    1. landed: harden Vulkan `map` and map-backed `min`/`max` preflight
       ordering before any new Vulkan callable broadening;
    2. landed: define and prove a shared CUDA probability-domain status ABI
       for data-dependent unary math through `stats/normal-quantile`;
    3. landed: implement CUDA `stats/normal-quantile` for dense row-major
       `Float64`/`Float32` Tensor storage;
    4. landed: implement Vulkan `Float32` `stats/normal-quantile` with a
       status-bearing Vulkan unary helper;
    5. landed: resolve Vulkan `Float64` `stats/normal-cdf` through a
       documented double approximation or an explicit fail-closed policy;
    6. landed: resolve Vulkan `Float64` `stats/normal-quantile` through
       a documented double inverse-CDF approximation/status helper;
    7. landed: implement CUDA-first dtype-changing Tensor rounding to
       `Tensor BigInteger` through a CUDA status/copyback helper and a
       `rounding-big-integer` backend capability;
    8. landed: implement Vulkan dtype-changing Tensor rounding to
       `Tensor BigInteger` through `shaderInt64`-gated Vulkan status/copyback
       helpers and a `rounding-big-integer` backend capability;
    9. landed: add Tensor view/layout metadata through the public
       `tensor-layout` introspection primitive;
    10. landed: define the explicit read-only Tensor transpose-view contract
       through `matrix/transpose-view`, with CPU read/materialize support and
       fail-closed GPU/copy-kernel posture;
    11. landed: define CPU fixed-width complex scalar and Tensor execution
       semantics for `Complex128` and `Complex64`, plus CUDA/Vulkan raw
       storage placement, CUDA/Vulkan fixed-width complex elementwise `map`,
       CUDA/Vulkan fixed-width complex `contract`, and Vulkan fixed-width
       complex structural matrix kernels behind explicit operation capability
       bits; CUDA fixed-width complex structural kernels now support the same
       public structural family behind those operation bits. Keep fixed-width
       complex numerical matrix kernels beyond landed Vulkan
       LU/determinant/solve/inverse/rank/norm/singular-values/QR/Cholesky and direct Vulkan general
       `matrix/eigenpairs` behind explicit backend contracts;
    12. use measurements, not speculation, for large SVD/eigen performance
       rewrites.
  - `TENSOR-100F` tracks the broader Vulkan math library:
    1. factor shared Vulkan descriptor/pipeline/status helpers so new kernels
       do not duplicate boilerplate;
    2. keep dense row-major `Float64` as the first library tier;
    3. add more backend-neutral matrix kernels only when they compute on
       Vulkan and preserve the CPU Tensor oracle;
    4. add `Float32` only after native `Float32` Tensor storage exists;
    5. add remaining GPU fixed-width complex numerical matrix kernels only
       after the shipped storage, elementwise-map, contract, CUDA/Vulkan
       structural matrix operation families, and landed Vulkan
      LU/determinant/solve/inverse/rank/norm/singular-values/QR/Cholesky
       lane have explicit capability reporting, helper ABIs, and validation
       coverage for the chosen next matrix family;
    6. add stride-aware GPU dispatch only after a specific view-aware helper
       ABI and validation plan exists; `matrix/transpose-view` alone is a
       CPU-readable view contract, not GPU view execution.
  - deferred TODOs that remain open under this parent:
    - [x] Split the largest Vulkan helper file top-down through the
      loader/context/buffer lifecycle boundary.
      - closed: `csrc/tensor_vulkan_helpers.c` now includes shared private
        Vulkan helper declarations from `csrc/tensor_vulkan_helpers_internal.h`
        and the dynamic loader, availability probe, shared context, buffer
        ownership, host/device copy, fill, retain, and free paths live in
        `csrc/tensor_vulkan_helpers_core.c`.
      - validation: `scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`;
        `git diff --check`; bounded-container `basic` slice passed `160/0`.
      - unresolved validation signal: focused `advanced-collections-module`
        is not green after this pass. Container run reported `1580/1` with
        `realize propagates single-arg source errors`; host run reported
        `1596/2` with the same error plus `realize vulkan unsupported callable
        fails closed`. The changed files are C helper factoring/build wiring,
        so treat this as an unresolved Tensor validation signal, not proof of
        the split itself passing the advanced gate.
    - [x] Split the Vulkan scalar/complex map plus contract family out of the
      remaining helper file.
      - closed: `csrc/tensor_vulkan_helpers_map_contract.c` now owns
        `map_add_scalar_f64`, dense `map` for `Float64`/`Float32`,
        fixed-width complex map launch, and dense `contract` for
        `Float64`/`Float32`/`Complex128`/`Complex64`.
      - closed: shared descriptor-layout, descriptor-set, compute-pipeline,
        and single-dispatch helpers are declared in
        `csrc/tensor_vulkan_helpers_internal.h` and exported from
        `csrc/tensor_vulkan_helpers.c` so the split file does not rely on
        implicit C declarations.
      - validation: `scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`;
        `git diff --check`; bounded-container `basic` slice passed `160/0`.
      - unresolved validation signal: focused bounded-container
        `advanced-collections-module` still fails `1580/1` on
        `realize propagates single-arg source errors`.
    - [x] Split the Tensor `map` primitive/runtime family out of
      `src/lisp/prim_tensor.c3`.
      - closed: `src/lisp/prim_tensor_map.c3` now owns the `map` callable
        classifier helpers, CUDA/Vulkan direct map routing, CPU map
        evaluation, lazy map expression construction, and `prim_tensor_map`.
      - closed: no build manifest update was needed because `project.json`
        sources all of `src`, and Stage 3/AOT policy already includes
        `src/lisp/*.c3`.
      - validation: `c3c build --obj-out obj`; `git diff --check`;
        bounded-container `basic` slice passed `160/0`.
      - unresolved validation signal: focused bounded-container
        `advanced-collections-module` still fails `1580/1` on
        `realize propagates single-arg source errors`.
    - [ ] Continue largest-first source splitting from
      `src/lisp/prim_tensor_matrix.c3`.
      - blocker/defer reason: after the Tensor `map` split, current source
        line counts put `src/lisp/prim_tensor_matrix.c3` at about 8.2k LOC,
        ahead of `csrc/tensor_vulkan_helpers.c` at about 7.3k LOC,
        `src/lisp/prim_tensor.c3` at about 6.8k LOC, and
        `csrc/tensor_cuda_helpers.c` at about 6.8k LOC.
      - concrete next step: inspect `src/lisp/prim_tensor_matrix.c3` top-down
        and split the largest coherent matrix primitive/helper family without
        changing runtime semantics.
      - prerequisite state: preserve current Tensor public surface decisions
        in `.agents/PLAN.md`, avoid reviving stale `linalg/matmul` direction,
        and keep new C3 sources wired through the repo's existing build/module
        conventions.
      - validation: `c3c build --obj-out obj`; `git diff --check`;
        bounded-container `basic`; focused Tensor validation for the touched
        primitive family. Keep tracking the current `realize` failure as an
        unresolved pre-existing signal unless the next split touches it.
    - [x] Harden Vulkan `map` preflight ordering before additional callable
      broadening.
      - closed: Vulkan public `map` lowering and lazy map realization now
        perform callable, recursive device-placement, dtype-family, and exact
        dtype preflight before resolving concrete Tensor storage. Mixed
        CPU/Vulkan lazy operands fail with
        `map: Vulkan operands must remain Vulkan-placed` before CPU lazy
        materialization can run.
      - adjacent closure: direct Tensor `min` / `max` now applies the same
        recursive Vulkan placement preflight before resolving operands, so the
        map-backed min/max Vulkan path also rejects mixed CPU/Vulkan lazy
        operands before CPU materialization.
      - preserved contract: no hidden CPU/GPU transfers, no mixed-device
        execution support, no generic Vulkan unary mode-3 revival, and no new
        Vulkan callable coverage.
      - validation: `c3c build --obj-out obj`; direct Vulkan smokes for mixed
        CPU/Vulkan lazy `map`, unsupported binary Vulkan callable preflight,
        unsupported unary callable preflight, and mixed CPU/Vulkan lazy `min`;
        bounded-container focused `advanced-collections-module`
        `pass=1287 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; targeted
        `git diff --check`.
    - [x] Investigate `handle` interaction around unsupported Vulkan lazy map
      construction errors.
      - closed 2026-04-18: unsupported Vulkan `map` callable errors now
        propagate through `handle` as structured `tensor/backend-unsupported`
        raises for direct `map`, lazy Vulkan map sources, and one-argument
        `realize` of unsupported lazy Vulkan map expressions.
      - root fix: payloaded recoverable raises with no `data` now construct
        the canonical payload directly in root scope before setting
        `raise_pending`, avoiding stack-heavy boundary promotion inside the
        constrained handle body stack. Compiled one-argument and multi-argument
        primitive apply paths also propagate evaluated `ERROR` arguments before
        invoking the callee, and normal StackCtx budget is now 256KB so handled
        lazy Tensor map realization does not trip the guard on small tensors.
      - preserved contract: no Vulkan `floor`/rounding support was widened
        through generic `map`; unsupported callables remain outside the Vulkan
        kernel whitelist and no hidden CPU/GPU fallback was added.
      - validation: `c3c build --obj-out obj`; direct `--eval` smokes for
        handled direct `map`, handled lazy-source `map`, direct `realize`,
        handled `realize`, handled CPU lazy-map `realize`, and destination
        `realize` error propagation; host focused `advanced-collections-module`
        `pass=1352 fail=0`; host `advanced-effect-continuation`
        `pass=56 fail=0`; host `advanced-effect-union-limit`
        `pass=68 fail=0`; bounded-container focused
        `advanced-collections-module` `pass=1335 fail=0`; stack suite
        `pass=23 fail=0`; targeted `git diff --check`.
    - [ ] Measure and optimize the storage-backed large-`k` Vulkan
      `matrix/singular-values` / `matrix/svd` path with tiled or
      multi-dispatch execution if runtime measurements justify it.
      - blocker/defer reason: the semantic `k > 64` cap removal now uses
        hidden storage-buffer Gram scratch behind the public Vulkan outputs,
        but it intentionally preserves the existing correctness-first serial
        Jacobi execution shape.
      - next step: collect runtime measurements for representative `65+`
        dense, diagonal, and rank-deficient inputs; only then design a tiled
        Gram build or staged Jacobi helper that keeps `Float64` semantics,
        Vulkan result placement, and no hidden LAPACK/CPU fallback.
      - validation: shader compile/`spirv-val`, helper rebuild,
        `c3c build --obj-out obj`, reconstruction/value checks,
        no-`LAPACKE_dgesvd` counter checks, focused host plus
        bounded-container advanced collections tests.
    - [ ] Measure and optimize the storage-backed large-`n` Vulkan symmetric
      `matrix/eigenvalues` / `matrix/eigenvectors` path with tiled or staged
      execution if runtime measurements justify it.
      - blocker/defer reason: the semantic `n > 64` cap removal now uses
        storage-buffer scratch behind the public Vulkan outputs, but it keeps
        the correctness-first Jacobi iteration shape and remains bounded by
        helper resource limits, 32-bit shader index guards, and the Jacobi
        iteration guard.
      - next step: collect runtime measurements for representative `65+`
        dense symmetric, diagonal, repeated-value, and near-degenerate inputs;
        only then design a tiled scratch layout or staged Jacobi helper that
        preserves `tensor/not-symmetric`, `tensor/no-convergence`, Vulkan
        output placement, and no hidden `LAPACKE_dsyev` fallback.
      - validation: shader compile/`spirv-val`, helper rebuild,
        `c3c build --obj-out obj`, residual/vector-placement checks,
        no-`LAPACKE_dsyev` counter checks, focused host plus
        bounded-container advanced collections tests.
    - [ ] Add direct Vulkan general `matrix/eigenpairs`.
      - blocker/defer reason: the public `matrix/eigenpairs` result contract
        is still pointer-backed `BigComplex`, which must not be lowered into
        fixed-width Vulkan buffers or host-built behind a Vulkan claim.
      - next step: choose whether the public Vulkan-ready eigenpairs contract
        returns native `Complex128`/`Complex64` tensors or an explicitly
        approved replacement result contract; then implement a real Vulkan
        Hessenberg/QR or real-Schur lane.
      - validation: fixed-width complex CPU dtype/oracle coverage must stay
        green, then Vulkan shader compile/`spirv-val`, helper rebuild,
        residual checks, no-`LAPACKE_dgeev` counter checks, and focused host
        plus bounded-container tests.
    - [x] Make Vulkan `Float32` large-dense SVD robust beyond the previous
      dense all-ones / rank-deficient `65x65` failure.
      - closed: scale-aware eigenvalue tolerance plus orthonormal completion
        in the `Float32` singular-values/SVD shaders now handles the
        previously failing all-ones `65x65` case while preserving Vulkan
        placement and `Float32` Tensor output dtype.
      - validation: shader compile/`spirv-val`, helper rebuild,
        `c3c build --obj-out obj`, direct zero/identity/all-ones `65x65`
        value and reconstruction checks, no-`LAPACKE_dgesvd` checks, host
        `advanced-collections-module` `pass=1177 fail=0`, and
        bounded-container `advanced-collections-module` `pass=1164 fail=0`.
    - [x] Define CPU fixed-width complex Tensor dtype/layout before Vulkan
      complex kernels.
      - closed: `Complex128` and `Complex64` now have scalar constructors,
        stdlib predicates, native CPU Tensor storage, construction/ref/flat
        conversion, scalar fill/realize, component helpers, arithmetic `map`,
        `contract`, and structural matrix oracle coverage.
      - preserved contract: `BigComplex` remains high-precision
        pointer-backed storage and must not be approximated by fixed-width GPU
        buffers.
      - validation: `c3c build`; focused scalar and Tensor advanced tests;
        bounded-container focused advanced collections tests; targeted
        `git diff --check`.
    - [x] Add CUDA/Vulkan fixed-width complex storage and copy round-trips.
      - closed: CUDA and Vulkan `to-device` placement now accept concrete
        zero-offset dense row-major `Complex128` and `Complex64` Tensor storage
        when `tensor-backends` reports the matching `complex128` or
        `complex64` storage capability. Explicit `to-device 'cpu` copyback
        preserves dtype and values, and destination `realize` can copy matching
        CPU or same-device complex storage into existing complex device
        tensors.
      - historical preserved contract: at the storage-only checkpoint,
        CUDA/Vulkan complex `map`, `contract`, and matrix kernels still failed
        closed. Superseded by the completed Vulkan and CUDA complex map items
        below; complex `contract`/matrix kernels still require explicit
        operation bits and remain fail-closed.
      - validation: `c3c build`; focused advanced collections tests; backend
        availability-gated round-trip and fail-closed compute tests; targeted
        `git diff --check`.
    - [x] Add Vulkan fixed-width complex elementwise `map`.
      - closed: Vulkan `Complex128` and `Complex64` dense row-major tensors
        now support elementwise `map` for `+`, `-`, `*`, `/`, unary `+`,
        `abs`, unary `-`, `real-part`, `imag-part`, and `conjugate` when
        `tensor-backends` reports `elementwise-map-complex128` or
        `elementwise-map-complex64`.
      - closed: direct Vulkan complex `abs`, `real-part`, and `imag-part`
        return component-width real Vulkan tensors; direct unary minus and
        `conjugate` preserve complex dtype and Vulkan placement.
      - preserved contract: division uses shader status readback for
        zero-denominator and non-representable-result failure; unsupported
        complex `min`/`max`, CPU destinations, complex `contract`, and matrix
        kernels remain fail-closed without hidden CPU fallback. Historical
        note: CUDA complex `map` was not landed at this Vulkan checkpoint and
        is superseded by the completed CUDA map item below.
      - validation: shader compile/`spirv-val`, helper rebuild, `c3c build`,
        host focused advanced collections `pass=1415 fail=0`; bounded
        container focused advanced collections `pass=1398 fail=0`; primitive
        docs parity; targeted diff hygiene.
    - [x] Add CUDA fixed-width complex elementwise `map`.
      - closed: CUDA `Complex128` and `Complex64` dense row-major tensors now
        support elementwise `map` behind `elementwise-map-complex128` and
        `elementwise-map-complex64` capability bits.
      - supported ops: binary `+`, `-`, `*`, `/`; unary `abs`, unary `-`,
        identity/`+`, `real-part`, `imag-part`, and `conjugate`.
      - direct helpers: CUDA complex `real-part`, `imag-part`, and `abs`
        return component-width real CUDA tensors (`Float64` for `Complex128`,
        `Float32` for `Complex64`); direct unary `-` and `conjugate` preserve
        complex dtype.
      - status contract: division by zero maps through native status to
        `tensor/domain-error`; nonrepresentable complex results map to invalid
        argument.
      - preserved contract: CPU fallback remains forbidden; CUDA/Vulkan
        complex `contract` and matrix kernels remain fail-closed.
      - validation passed: `./scripts/build_omni_chelpers.sh`; `c3c build`;
        host focused `advanced-collections-module` `1433 passed, 0 failed`;
        bounded container focused `advanced-collections-module`
        `1416 passed, 0 failed`;
        CUDA PTX generation and `ptxas` for `csrc/tensor_cuda_complex_map.cu`;
        C helper syntax check; primitive docs parity; Stage 3 source parity;
        targeted diff check.
    - [x] Add CUDA/Vulkan fixed-width complex `contract` kernels.
      - closed: CUDA and Vulkan dense row-major `Complex128` and `Complex64`
        tensors now support backend `contract` behind explicit
        `contract-complex128` and `contract-complex64` operation capability
        bits. Supported contractions compute on the selected device, preserve
        fixed-width complex dtype and placement, and require explicit
        `to-device 'cpu` for CPU inspection.
      - preserved boundary: storage capability (`complex128`/`complex64`) and
        elementwise-map capability do not imply complex contraction support.
        Unsupported layouts, mixed devices, mixed dtypes, unavailable helpers,
        and complex matrix kernels still fail closed without hidden CPU
        fallback or lowering through `Float64`, `Float32`, or `BigComplex`.
      - validation passed: complex Vulkan shaders compiled and passed
        `spirv-val`; CUDA/Vulkan helper syntax checks; helper archive rebuild;
        `c3c build`; host focused `advanced-collections-module`
        `1438 passed, 0 failed`.
    - [x] Add Vulkan fixed-width complex structural matrix kernels.
      - closed: Vulkan dense row-major zero-offset `Complex128` and
        `Complex64` tensors now support `matrix/transpose`,
        `matrix/diagonal`, `matrix/diagonal-matrix`, and `matrix/trace`
        behind `matrix-structural-complex128` and
        `matrix-structural-complex64` operation capability bits. Tensor
        returning structural operations compute on Vulkan, preserve
        fixed-width complex dtype and placement, and require explicit
        `to-device 'cpu` for CPU inspection. `matrix/trace` computes on
        Vulkan and reads back only the fixed-width complex scalar required by
        the public scalar return contract.
      - preserved boundary: this closed only the Vulkan structural fixed-width
        complex matrix kernels. CUDA structural kernels later landed as their
        own explicit PTX/helper/routing slice; complex solver, factorization,
        decomposition, singular-value/SVD, spectral/nuclear norm, and eigen
        kernels still require their own helper/shader/PTX ABI, capability bit,
        status/error contract, and oracle coverage. Do not lower through `Float64`,
        `Float32`, pointer-backed `BigComplex`, or hidden CPU fallback.
      - validation passed: complex Vulkan structural shaders compiled and
        passed `spirv-val`; Vulkan helper syntax check; helper archive
        rebuild; `c3c build`; host focused `advanced-collections-module`
        `1449 passed, 0 failed`; bounded-container focused
        `advanced-collections-module` `1432 passed, 0 failed`; docs parity and
        Stage 3 source parity plus targeted diff hygiene passed.
    - [x] Land Vulkan fixed-width complex LU, determinant, and solve kernels.
      - closed: Vulkan dense row-major zero-offset `Complex128`/`Complex64`
        operands now support `matrix/lu`, `matrix/determinant`, and
        `matrix/solve` behind `matrix-numerical-complex128` and
        `matrix-numerical-complex64` capability bits.
      - shipped contract: `matrix/lu` keeps `lu` as a Vulkan-placed fixed-width
        complex Tensor and returns host `pivots`/`swap-count` metadata;
        `matrix/determinant` computes on Vulkan and reads back only the public
        fixed-width complex scalar; `matrix/solve` returns a Vulkan-placed
        Tensor preserving RHS rank and dtype. Singular systems raise
        `tensor/singular-matrix`; unsupported layouts/dtypes/devices fail
        closed without hidden CPU fallback.
      - validation: complex LU/determinant/solve shaders compiled and passed
        `spirv-val`; Vulkan helper rebuild passed; `c3c build --obj-out obj`
        passed; direct CPU/Vulkan smokes passed; host focused
        `advanced-collections-module` passed `1475/0`; bounded-container
        focused `advanced-collections-module` passed `1458/0`; targeted
        diff hygiene passed.
    - [x] Add CUDA fixed-width complex structural matrix kernels.
      - closed: CUDA dense row-major zero-offset `Complex128` and `Complex64`
        tensors now support `matrix/transpose`, `matrix/diagonal`,
        `matrix/diagonal-matrix`, and `matrix/trace` behind
        `matrix-structural-complex128` and
        `matrix-structural-complex64` operation capability bits.
      - shipped contract: tensor-returning structural operations compute
        through CUDA PTX/helper exports, preserve fixed-width complex dtype and
        CUDA placement, and require explicit `to-device 'cpu` for inspection.
        `matrix/trace` computes on CUDA and reads back only the public
        fixed-width complex scalar.
      - validation: CUDA PTX generation and `ptxas`; CUDA helper syntax and
        symbol checks; `./scripts/build_omni_chelpers.sh`;
        `c3c build --obj-out obj`; direct CUDA smokes for transpose,
        diagonal-matrix, and trace; host focused `advanced-collections-module`
        `1482 passed, 0 failed`; bounded-container focused
        `advanced-collections-module` `1465 passed, 0 failed`.
    - [x] Add Vulkan fixed-width complex `matrix/inverse`.
      - closed: CPU `Complex128` and `Complex64` now provide the
        `matrix/inverse` oracle, and Vulkan dense row-major zero-offset
        `Complex128`/`Complex64` operands route through native SPIR-V
        Gauss-Jordan inverse kernels behind `matrix-numerical-complex128` and
        `matrix-numerical-complex64`.
      - shipped contract: Tensor results preserve fixed-width complex dtype
        and remain Vulkan-placed for Vulkan inputs. Singular matrices raise
        `tensor/singular-matrix`; unsupported layouts/dtypes/devices fail
        closed without hidden CPU fallback or lowering through real tensors.
      - validation: complex inverse shaders compiled and passed `spirv-val`;
        helper archive rebuild passed; `c3c build --obj-out obj` passed;
        direct CPU/Vulkan Complex128 and Complex64 inverse smokes passed;
        singular diagnostics mapped to `tensor/singular-matrix`; host focused
        `advanced-collections-module` passed `1496/0`; bounded-container
        focused `advanced-collections-module` passed `1479/0`; docs parity,
        Stage 3 source parity, and targeted diff hygiene passed.
    - [x] Add Vulkan fixed-width complex `matrix/rank` and direct
      `matrix/norm` reducers.
      - closed: CPU `Complex128` and `Complex64` now provide `matrix/rank`
        and direct `matrix/norm` oracle behavior for default/`'frobenius`,
        `'one`, `'infinity`, and `'max` over complex magnitudes. Vulkan dense
        row-major zero-offset `Complex128`/`Complex64` operands route through
        native SPIR-V rank and norm kernels behind
        `matrix-numerical-complex128` and `matrix-numerical-complex64`.
      - shipped contract: rank reads back only the public `Integer` result;
        direct norm selectors read back only the public `Float64` scalar
        result. Spectral/nuclear norms are now covered by the later
        fixed-complex singular-values slice. Unsupported layouts/dtypes/devices
        fail closed without hidden CPU fallback or lowering through real
        tensors.
      - validation: complex norm/rank shaders compiled and passed
        `spirv-val`; helper archive rebuild passed; `c3c build --obj-out obj`
        passed; direct CPU/Vulkan Complex128 and Complex64 rank/norm smokes
        passed; host focused `advanced-collections-module` passed `1540/0`;
        bounded-container focused `advanced-collections-module` passed
        `1523/0`.
    - [x] Add Vulkan fixed-width complex `matrix/qr` and `matrix/cholesky`.
      - closed: CPU `Complex128` and `Complex64` now provide `matrix/qr` and
        `matrix/cholesky` oracle behavior. Complex QR uses the Hermitian
        inner product and returns dtype-preserving `q`/`r`; Complex Cholesky
        accepts Hermitian positive-definite inputs and returns dtype-preserving
        lower factors. Vulkan dense row-major zero-offset
        `Complex128`/`Complex64` operands route through native SPIR-V QR and
        Cholesky kernels behind `matrix-numerical-complex128` and
        `matrix-numerical-complex64`.
      - shipped contract: QR rank-deficient input maps to
        `tensor/singular-matrix`; Cholesky non-Hermitian or non-HPD input maps
        to `tensor/not-positive-definite`; Tensor results preserve fixed-width
        complex dtype and Vulkan placement. CPU and Vulkan fixed-complex
        tolerance thresholds are aligned for near-rank-deficient QR and
        near-Hermitian Cholesky inputs.
      - validation: complex QR/Cholesky shaders compiled and passed
        `spirv-val`; helper archive rebuild passed; `c3c build --obj-out obj`
        passed; direct CPU/Vulkan Complex128 and Complex64 QR/Cholesky smokes
        passed; host focused `advanced-collections-module` passed `1570/0`;
        bounded-container focused `advanced-collections-module` passed
        `1553/0`; docs parity, Stage 3 source parity, and targeted diff
        hygiene passed.
    - [x] Add CPU/Vulkan fixed-width complex `matrix/singular-values` and
      spectral/nuclear `matrix/norm` selectors.
      - closed: CPU `Complex128`/`Complex64` `matrix/singular-values` now use
        realification and collapse duplicate real singular-value pairs,
        returning component-width real tensors (`Float64` for `Complex128`,
        `Float32` for `Complex64`). CPU spectral/nuclear norms route through
        the same fixed-complex singular-value oracle and return `Float64`
        scalars.
      - closed: Vulkan dense row-major zero-offset `Complex128`/`Complex64`
        operands route through native SPIR-V complex singular-value kernels
        behind `matrix-numerical-complex128` and
        `matrix-numerical-complex64`. Tensor singular-value results stay
        Vulkan-placed with component-width real dtype, while spectral/nuclear
        norms read back only the public `Float64` scalar.
      - validation: complex singular-value shaders compiled and passed
        `spirv-val`; helper archive rebuild passed; `c3c build --obj-out obj`
        passed; direct Vulkan Complex128/Complex64 singular-value and norm
        smokes passed; host focused `advanced-collections-module` passed
        `1598/0`; bounded-container focused `advanced-collections-module`
        passed `1581/0`; primitive docs parity, Stage 3 source parity, and
        targeted diff hygiene passed. Review follow-up added a native guard for
        fixed-complex realified Jacobi iteration-count overflow and covered
        non-diagonal lazy, wide, zero-size, and guard paths.
    - [ ] `TENSOR-100H-SVD-FACTORS` add full fixed-width complex
      `matrix/svd` factor output.
      - blocker/defer reason: fixed-complex singular values are landed, but
        full SVD remains a separate result contract because `u` and `v` must
        preserve `Complex128`/`Complex64` while `s` must be component-width
        real. Realification is valid for singular values only; it is not a
        reliable source of complex SVD factors because duplicated real
        singular subspaces can rotate arbitrarily.
      - next step: land CPU `Complex128`/`Complex64` oracle behavior first,
        then Vulkan dense row-major zero-offset execution. Use Hermitian
        Gram/Jacobi or optional complex LAPACK for CPU; use native complex
        Hermitian Gram/Jacobi shaders for Vulkan. Do not include CUDA in this
        lane.
      - validation: complex factor reconstruction via `U * diag(S) * V^H`,
        dtype/shape/device checks, rank-deficient and empty-axis cases,
        no-hidden-fallback tests, shader/helper validation where applicable,
        focused host and bounded-container advanced tests, docs parity, Stage
        3 source parity, and `git diff --check`.
    - [ ] `TENSOR-100H-CUDA-SVD-NORMS` add CUDA fixed-width complex
      `matrix/singular-values`, spectral/nuclear `matrix/norm`, and
