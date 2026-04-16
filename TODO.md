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
      `matrix/svd`.
      - blocker/defer reason: CUDA has fixed-complex storage, map, contract,
        and structural matrix support, but no cuSOLVER runtime loader or SVD
        ABI. cuBLAS is not an SVD backend, and custom PTX should be limited to
        deterministic layout adapters.
      - next step: add runtime-loaded cuSOLVER DN support for
        `cusolverDnZgesvd` / `cusolverDnCgesvd`, adapter PTX for Omni
        row-major to cuSOLVER column-major storage and `VT` to row-major `V`,
        operation-specific capability bits, and CUDA routing for
        singular-values/norms before full factor output.
      - validation: CUDA PTX generation plus `ptxas`, cuSOLVER symbol probes
        and call counters, helper archive rebuild, CUDA direct smokes, no
        LAPACK/CPU fallback counters, focused host and bounded-container
        advanced tests, docs parity, Stage 3 source parity, and
        `git diff --check`.
    - [ ] `TENSOR-100H-COMPLEX-EIGEN` settle and implement fixed-width
      complex eigen/eigenpair contracts.
      - blocker/defer reason: native fixed-width complex Tensor storage now
        exists, but the public eigen result contract still has a `BigComplex`
        general `matrix/eigenpairs` legacy. Backend support must not fake
        support by lowering pointer-backed `BigComplex` or host-building it
        behind a GPU result.
      - next step: freeze the fixed-width contract first. Recommended
        direction: `matrix/eigenvalues` / `matrix/eigenvectors` cover
        symmetric/Hermitian inputs with component-width real values and
        dtype-preserving vectors; `matrix/eigenpairs` becomes the general
        fixed-width eigenpair surface returning fixed-width complex
        values/vectors for fixed-width numeric inputs. CUDA remains false
        unless cuSOLVER eigen support is introduced later.
      - validation: residual checks `A * v ~= lambda * v`, Hermitian and
        general cases, dtype/device/capability truth tables, no-hidden-fallback
        tests, focused host and bounded-container advanced tests, docs parity,
        Stage 3 source parity, and `git diff --check`.
    - [x] Add explicit Tensor view/layout representation metadata before
      enabling view-backed GPU dispatch.
      - closed: `tensor-layout` now returns a metadata `Dictionary` covering
        dtype, device, payload, layout, dense-row-major status, shape, strides,
        rank, element count, logical byte length, storage offset, concrete
        backing storage element/byte extent, view flags, owner, and write
        policy. Lazy expression payloads report zero storage extent until
        realization.
      - preserved contract: this is metadata-only. It does not add a public
        view constructor, recursive/view GPU kernels, or stride-aware helper
        ABI consumption. Dense CUDA/Vulkan kernels still require zero-offset
        dense row-major storage.
      - prerequisite state: dense row-major contiguous CPU/CUDA/Vulkan Tensor
        paths are shipped; unsupported layouts still fail closed.
      - negative-memory constraint: do not resume by only passing strides into
        Vulkan helpers. Strides without offset, backing extent, and alias owner
        metadata are not a safe view contract.
      - validation: implementation validation was owned by the runtime slice;
        this documentation update was checked with targeted diff hygiene.
    - [x] Define the first read-only Tensor view construction/operation
      contract.
      - closed: `matrix/transpose-view` is the explicit read-only rank-2
        transpose view constructor. It swaps logical shape and strides, keeps
        the source Tensor as owner, reports `tensor-layout` payload `view`,
        owner `view-source`, `owns-storage` false, and write-policy
        `read-only-view`.
      - shipped CPU surface: `ref`, flat `(Array view)`, flat `(List view)`,
        and CPU `realize` materialization observe the transposed logical
        indexing. Writes through the view fail closed.
      - preserved contract: existing `matrix/transpose` remains materializing
        for concrete tensors, but composes structurally when its input is
        already a transpose view.
      - backend boundary: this original view slice remained CPU-readable only
        until the follow-on Vulkan materialization item below landed. Broader
        arbitrary view-backed CUDA/Vulkan kernels still fail closed instead of
        unsafe offset/stride interpretation.
      - validation: original CPU view behavior and the follow-on Vulkan
        materialization boundary are covered by the current focused host and
        bounded-container advanced collections runs.
    - [x] Land Vulkan transpose-view materialization before broader
      view-aware GPU execution.
      - closed: direct rank-2 `matrix/transpose-view` values over dense
        zero-offset Vulkan storage can now materialize to dense Vulkan tensors
        through `realize`, `to-device 'vulkan`, and `to-device 'cpu` copyback.
      - shipped contract: this is explicit materialization only. It preserves
        logical transpose indexing, dtype, and placement for Tensor outputs,
        and it does not make arbitrary strided/view-backed Vulkan kernels
        eligible.
      - validation: direct Vulkan transpose-view smokes passed; host focused
        `advanced-collections-module` passed `1475/0`; bounded-container
        focused `advanced-collections-module` passed `1458/0`; targeted diff
        hygiene passed.
    - [ ] Add explicit view-aware Vulkan/copy-kernel support after the
      read-only CPU transpose-view and Vulkan materialization contracts are
      validated.
      - blocker/defer reason: `matrix/transpose-view` ships with CPU
        read/materialize support, and direct rank-2 Vulkan transpose-view
        materialization is landed. That materialization boundary is not broad
        view-aware kernel execution. Vulkan/CUDA helpers still require
        zero-offset dense row-major storage unless a specific helper ABI accepts
        offset/stride metadata.
      - concrete next step: choose one structural GPU operation, define the
        helper ABI for source offset/strides/backing extent, add alias/bounds
        tests, then route that operation without hidden CPU/GPU copies.
      - prerequisite state: direct rank-2 Vulkan transpose-view materialization
        validation is recorded above. Future view-aware execution still needs
        new offset/stride/backing-extent validation, including alias/source
        ownership and fail-closed unsupported-view cases.
      - negative-memory constraint: do not pass strides into Vulkan helpers
        opportunistically. View-aware backend execution requires offset,
        backing extent, owner/alias, and write-policy validation together.
      - validation: shader compile/`spirv-val` if a shader changes, helper
        rebuild, `c3c build --obj-out obj`, focused CPU/GPU parity smokes,
        host and bounded-container advanced collections, docs parity, Stage 3
        parity, and targeted `git diff --check`.
    - [x] Expand `Float32` scientific unary Vulkan coverage through dedicated
      helper opcodes, not the invalidated generic unary `map` mode-3 branch.
      - closed: dense row-major Vulkan `Float32` tensors now support `sin`,
        `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `exp`,
        `log`, and `log10` through the separate unary helper ABI for both
        public `map` and direct Tensor unary math.
      - validation: shader compile/`spirv-val`, regenerated SPIR-V C embed,
        helper rebuild, `c3c build --obj-out obj`, direct Vulkan smokes, and
        focused host `advanced-collections-module` `pass=1198 fail=0`.
      - negative constraint: do not implement Vulkan `Float64` scientific
        unary math by downcasting; Vulkan GLSL rejected the double
        transcendental builtins under the current Vulkan 1.0 validation path.
    - [x] Define and implement scalar `Float32` runtime values.
      - closed: scalar `Float32` now has a runtime value tag/payload,
        constructor, type id, `Number` parent, `type-of` / `is?` support,
        stdlib `float32?`, copy/promotion/env-copy leaf handling,
        print/string/format conversion, numeric comparison/equality/order
        integration, basic arithmetic policy, schema/tuple/JSON/CSV/AOT
        surfaces, and Tensor `Float32` `ref` / `Array` / `List` / `Iterator`
        / contract extraction now returns scalar `Float32` instead of
        widening to `Float64`.
      - validation: `c3c build --obj-out obj`; direct runtime smokes for
        constructor/type/predicate, arithmetic, closure capture, schema,
        format/sort-by, Tensor extraction, structural matrix extraction,
        contract extraction, and `matrix/trace`; focused
        `advanced-core-semantics` `pass=71 fail=0`; arithmetic-comparison
        `pass=47 fail=0`; bounded-container focused
        `advanced-collections-module` `pass=1205 fail=0`; primitive docs
        parity and Stage 3 source parity.
    - [x] Add CUDA `Float32` placement, destination `realize`, copyback, and
      cuBLAS contract support.
      - closed: CUDA helpers now expose `fill_f32`, `sgemm`, and `sgemv`;
        public placement, CPU copyback, destination `realize`, scalar fills,
        lazy contract destination writes, `tensor-backends` capability
        reporting, and matching `Float32` cuBLAS contract layouts are routed
        through the existing explicit-device Tensor surfaces.
      - preserved contract: CUDA `map`, CPU-destination-from-device behavior,
        and mixed CUDA dtypes remain fail-closed/deferred. Scalar `Float32`
        runtime values are covered by the completed scalar checkpoint above;
        zero-size CUDA contract identity/fill is covered by the completed
        CUDA zero-size checkpoint below.
      - validation: helper rebuild, `c3c build --obj-out obj`, focused host
        `advanced-collections-module` `pass=1218 fail=0`, bounded-container
        focused `advanced-collections-module` `pass=1205 fail=0`, primitive
        docs parity, Stage 3 source parity, and targeted `git diff --check`.
    - [x] Add CUDA elementwise `map` kernels for the explicit dense row-major
      binary scope.
      - closed: CUDA now has embedded-PTX CUDA Driver API elementwise binary
        map kernels for dense row-major `Float64` and `Float32` tensors.
        Supported operations are `+`, `-`, `*`, `/`, `min`, and `max`.
        Supported operand shapes are tensor/scalar, scalar/tensor, exact-shape
        tensor/tensor, and right-aligned singleton-axis tensor/tensor
        broadcasting.
      - shipped paths: direct public `map` on CUDA tensors, lazy map
        realization to CUDA, and CUDA destination realization from lazy CUDA
        maps. There is no hidden CPU fallback.
      - capability reporting: `tensor-backends` now reports
        `elementwise-map-float64` and `elementwise-map-float32` capability keys
        for CPU, Vulkan, and CUDA.
      - preserved residuals: unsupported callables, mixed CPU/CUDA operands,
        mixed dtype/device operands, unsupported layout, and scientific CUDA
        map remain explicit backend-unsupported/fail-closed cases. Arithmetic
        unary CUDA map is covered by the completed checkpoint below.
      - validation: helper compile, `c3c build --obj-out obj`, direct
        CUDA-gated smokes for scalar/exact-shape/broadcast binary map and
        fail-closed residuals, host focused `advanced-collections-module`
        `pass=1240 fail=0`; earlier exact/scalar validation also passed
        bounded-container `memory-lifetime-smoke` `pass=228 fail=0`,
        bounded-container focused `advanced-collections-module` `pass=1224
        fail=0`, primitive docs parity, and Stage 3 source parity.
    - [x] Add CUDA arithmetic unary `map` kernels for the dense row-major
      `Float64`/`Float32` helper family.
      - closed: CUDA now has separate embedded-PTX unary map kernels and C ABI
        helpers for op ids `0..4`: `abs`, unary `-`, `sqrt`, identity
        (`map +`, `real-part`, `conjugate` on real tensors), and zero-fill
        (`imag-part` on real tensors).
      - shipped paths: direct public `map`, lazy map realization, CUDA
        destination realization from lazy unary CUDA maps, and direct Tensor
        primitives `abs`, unary `-`, `sqrt`, `real-part`, `imag-part`, and
        `conjugate` preserve CUDA placement for eligible dense row-major
        `Float64`/`Float32` tensors.
      - superseded residual note: scientific CUDA map/direct unary math op ids
        `5..19` and foreign CUDA clone semantics are covered by later checkpoints
        below; unsupported callables, mixed CPU/CUDA operands, mixed dtype/device
        operands, and unsupported layouts remain explicit fail-closed cases.
      - validation: helper compile, full helper rebuild, `c3c build --obj-out
        obj`, CUDA-available direct smokes for `abs`, `sqrt`, `imag-part`,
        Float32 `sqrt`, CUDA destination realization from lazy unary map, and
        scientific fail-closed checks, host focused `advanced-collections-module`
        `pass=1256 fail=0`, bounded-container focused
        `advanced-collections-module` `pass=1243 fail=0`, primitive docs
        parity, Stage 3 source parity, and targeted `git diff --check`.
    - [x] Add CUDA scientific unary `map` kernels for dense row-major
      `Float64`/`Float32`.
      - closed: CUDA now has generated CUDA C/libdevice PTX kernels for unary
        scientific op ids `5..19`: `sin`, `cos`, `tan`, `asin`, `acos`,
        `atan`, `sinh`, `cosh`, `tanh`, `exp`, `log`, `log10`, `math/erf`,
        `math/erfc`, and `stats/normal-cdf`.
      - shipped paths: public `map`, lazy CUDA map realization, CUDA
        destination realization from lazy scientific maps, and direct Tensor
        scientific primitives preserve CUDA placement for eligible dense
        row-major `Float64`/`Float32` tensors.
      - CPU Tensor `math/erf`, `math/erfc`, and `stats/normal-cdf` are also
        implemented: `Float64` and `Float32` inputs preserve real float dtype,
        `BigInteger` inputs return `Float64`, `BigFloat` inputs preserve
        `BigFloat`, and `BigComplex` Tensor inputs fail closed because no
        complex error-function or distribution Tensor contract is shipped.
      - capability reporting: `tensor-backends` now reports
        `scientific-map-float64` and `scientific-map-float32` separately from
        the existing arithmetic `elementwise-map-*` bits.
      - preserved boundaries: unsupported layouts, mixed CPU/CUDA operands,
        mixed dtype/device operands, and unsupported callables remain explicit
        fail-closed cases.
      - validation: CUDA C source generated PTX with `/usr/local/cuda-13.0/bin/nvcc
        --ptx -arch=compute_75`; generated PTX assembled with
        `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75`; helper compile; full
        helper rebuild; `c3c build --obj-out obj`; CUDA-gated smokes for
        capability reporting, Float64 `map sin`, Float32 direct `log10`, direct
        `sin`, family `acos`/`log10`, and destination `realize`; later
        `math/erf`/`math/erfc` validation: regenerated CUDA PTX/ptxas, helper
        compile and rebuild, `c3c build --obj-out obj`, CUDA smokes, host
        focused `advanced-collections-module` `pass=1271 fail=0`, and
        bounded-container focused `advanced-collections-module` `pass=1258
        fail=0` with `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`;
        `stats/normal-cdf` validation added opcode-19 PTX regeneration,
        ptxas, helper compile/rebuild, `c3c build --obj-out obj`, direct
        CPU/CUDA/Vulkan-boundary smokes, host focused
        `advanced-collections-module` `pass=1282 fail=0`, bounded-container
        focused `advanced-collections-module` `pass=1269 fail=0`, primitive
        docs parity, Stage 3 source parity, and targeted `git diff --check`.
    - [ ] Broaden CUDA `map` beyond the landed dense row-major arithmetic and
      scientific `Float64`/`Float32` scope only after dedicated semantic and
      validation gates.
      - blocker/defer reason: unsupported layouts need explicit storage-span /
        base-offset / owner metadata before CUDA kernels can safely read
        non-dense views; unsupported callables still need explicit dispatch
        contracts. The current kernels intentionally cover dense row-major
        supported callables only.
      - closed hardening slice: mixed CPU/CUDA operands now fail with the
        explicit `map: CUDA operands must remain CUDA-placed` diagnostic, and
        mixed CUDA Tensor dtypes fail with `tensor/dtype-mismatch` instead of a
        generic backend kernel message. CUDA and Vulkan map probes now check for
        relevant device operands before realizing child expressions, so an
        unsupported mixed CPU/CUDA lazy operand is rejected before CPU lazy
        materialization can run. This preserves the no-hidden-transfer rule; it
        does not add mixed-device execution support.
      - next step: choose one residual family first: unsupported layout/view
        metadata, explicit mixed-device execution support, or callable extension
        beyond the fixed op-id table.
      - prerequisite state: keep generated CUDA scientific PTX and the existing
        embedded arithmetic PTX as separate capability-gated modules; keep
        `tensor-backends` `elementwise-map-*` and `scientific-map-*` reporting
        as the baseline for shipped kernels.
      - negative constraint: do not treat the landed CUDA map support as
        arbitrary-layout support, and do not reintroduce hidden CPU
        materialization for unsupported CUDA operands.
      - validation: CUDA direct smokes for the chosen residual family,
        no-implicit-transfer regressions, helper rebuild if helper ABI changes,
        `c3c build --obj-out obj`, focused host tests, and bounded-container
        focused advanced collections. Mixed-operand hardening validation:
        `c3c build --obj-out obj`; host focused `advanced-collections-module`
        `pass=1261 fail=0`; bounded-container focused
        `advanced-collections-module` `pass=1248 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
    - [x] Add CUDA-first Tensor rounding through a dtype-changing
      `Tensor BigInteger` result path.
      - shipped contract: direct `floor`, `ceiling`, `round`, and `truncate`
        over CUDA-placed dense row-major `Float64`/`Float32` tensors compute
        on CUDA, copy fixed-width integer results back through a status-bearing
        helper, and materialize native CPU `Tensor BigInteger` output.
      - coverage: advanced stdlib module assertions are gated by the CUDA
        `rounding-big-integer` backend capability, require `BigInteger` dtype,
        and verify values after explicit `to-device 'cpu` copyback via `ref`,
        `Array`, and `List`.
      - negative-memory constraint: do not add `floor` / `ceiling` / `round` /
        `truncate` to the CUDA unary opcode table unless the result dtype path
        changes in the same slice.
      - validation: CUDA `Float64`/`Float32` direct smokes passed for
        `floor`, `ceiling`, `round`, and `truncate`; non-finite status smoke
        returned the Tensor integer range diagnostic; `map floor` remains
        backend-unsupported; `./scripts/build_omni_chelpers.sh`; `c3c build
        --obj-out obj`; host focused `advanced-collections-module`
        `pass=1322 fail=0`; bounded-container focused
        `advanced-collections-module` `pass=1305 fail=0`; parity and targeted
        diff hygiene passed.
    - [x] Add Vulkan Tensor rounding through a dtype-changing
      `Tensor BigInteger` result path.
      - shipped contract: direct `floor`, `ceiling`, `round`, and `truncate`
        over Vulkan-placed dense row-major `Float64`/`Float32` tensors compute
        on Vulkan when `shaderInt64` is available, copy checked fixed-width
        integer results back through a status-bearing helper, and materialize
        native CPU `Tensor BigInteger` output.
      - coverage: advanced stdlib module assertions are gated by the Vulkan
        `rounding-big-integer` backend capability, require `BigInteger` dtype,
        and verify values after explicit `to-device 'cpu` copyback.
      - negative-memory constraint: do not expose Vulkan rounding as same-dtype
        float output, do not downcast or approximate `BigInteger` results, and
        do not treat generic Vulkan `available`/`float64`/`float32` as proof of
        dtype-changing rounding support.
      - validation: Vulkan rounding shader compile/`spirv-val`; helper
        rebuild; `c3c build --obj-out obj`; direct Vulkan `Float64`/`Float32`
        smokes for `floor`, `ceiling`, `round`, and `truncate`; overflow
        range diagnostic; direct `map floor` fail-closed diagnostic; host
        focused `advanced-collections-module` `pass=1326 fail=0`;
        bounded-container focused `advanced-collections-module`
        `pass=1309 fail=0`; parity and targeted diff hygiene passed.
    - [x] Add CPU Tensor `stats/normal-quantile` with an explicit
      probability-domain contract.
      - closed: CPU Tensor `stats/normal-quantile` now applies elementwise for
        `Float64`, `Float32`, and `BigFloat` tensors, fails the whole operation
        on the first invalid probability outside `0 < p < 1`, rejects
        `BigComplex`, and keeps `BigInteger` non-empty tensors fail-closed
        because integer probabilities cannot satisfy the open probability
        interval.
      - validation: direct CPU smokes and host focused
        `advanced-collections-module` `pass=1296 fail=0`.
    - [x] Define shared GPU probability-domain status handling for
      `stats/normal-quantile`.
      - closed: CUDA scientific unary op `20` now carries a device `uint32`
        status word for data-dependent probability validation. Status `0`
        means success, `1` means probability outside `0 < p < 1`, and `2`
        means non-finite input; higher status wins for mixed invalid tensors,
        so non-finite input takes priority over a domain endpoint. The helper
        maps nonzero status to the scalar-compatible whole-operation
        diagnostics before result exposure.
      - superseded boundary: Vulkan `Float32` `stats/normal-quantile` now has
        a separate status-bearing helper. Vulkan `Float64`
        `stats/normal-quantile` remains fail-closed unless a double
        inverse-CDF approximation/status helper is explicitly implemented.
      - negative-memory constraint: do not reuse the existing statusless Vulkan
        unary helper for any data-dependent quantile path; status-bearing
        helpers are required.
      - validation: CUDA valid/invalid probability smokes, `c3c build
        --obj-out obj`, focused advanced collections, and targeted
        `git diff --check`.
    - [x] Add CUDA Tensor `stats/normal-quantile` after the GPU domain-status
      ABI lands.
      - closed: CUDA `Float64`/`Float32` dense row-major Tensor
        `stats/normal-quantile` now routes through generated CUDA C/libdevice
        PTX op `20`, preserves CUDA placement and float dtype for direct and
        `map` paths, and uses libdevice `normcdfinv` after status-checked
        probability validation.
      - superseded boundary: no arbitrary GPU callable support, no hidden CPU
        fallback, and no unsupported layout support landed in this slice.
        Vulkan quantile is covered by the later Vulkan-specific
        status-bearing helpers for `Float32` and `Float64`.
      - negative-memory constraint: do not return `NaN` or backend success for
        invalid probabilities, and do not route through CPU fallback.
      - validation: `nvcc --ptx`, `ptxas`, helper C compile,
        `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
        direct CUDA valid/invalid smokes, host focused
        `advanced-collections-module` `pass=1307 fail=0`, and
        bounded-container focused `advanced-collections-module`
        `pass=1294 fail=0`.
    - [x] Add Vulkan `Float32` Tensor `stats/normal-quantile` after the shared
      GPU domain-status ABI is proven.
      - closed: dense row-major Vulkan `Float32` Tensor
        `stats/normal-quantile` now uses a dedicated inverse-CDF shader plus a
        `uint32` status buffer. Direct Tensor unary math and
        `map stats/normal-quantile` preserve Vulkan placement and `Float32`
        dtype for valid probabilities. Status `1` reports finite probability
        values outside `0 < p < 1`; status `2` reports non-finite probability,
        and `atomicMax` keeps non-finite status priority for mixed invalid
        tensors.
      - superseded boundary: Vulkan `Float64` quantile is covered by the later
        Float64-specific status-bearing helper; the existing statusless Vulkan
        unary helper still must not receive op `20`.
      - validation: shader compile/`spirv-val`, helper rebuild,
        `c3c build --obj-out obj`, direct Vulkan valid/domain/non-finite/
        mixed-invalid/Float64 fail-closed smokes, host focused
        `advanced-collections-module` `pass=1311 fail=0`, and bounded-container
        focused `advanced-collections-module` `pass=1298 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
    - [x] Resolve Vulkan `Float64` Tensor `stats/normal-quantile` with an
      explicit double inverse-CDF approximation/status helper.
      - closed: dense row-major Vulkan `Float64` tensors now route public
        `(map stats/normal-quantile ...)` and direct
        `(stats/normal-quantile ...)` through the Float64 Vulkan quantile
        helper, preserving Vulkan placement and `Float64` dtype.
      - regression coverage:
        `src/lisp/tests_advanced_stdlib_module_groups.c3` checks direct and
        `map stats/normal-quantile` placement/dtype/value preservation for
        `0.025`, `0.5`, and `0.975`, plus probability `0`/`1` and non-finite
        error messages without CPU fallback or Float32 downcast.
      - preserved contract: do not infer Float64 quantile support from Vulkan
        Float32 or CUDA, do not downcast Float64 tensors to Float32, and do not
        route through CPU fallback.
      - validation: `glslangValidator -V --target-env vulkan1.0
        csrc/tensor_vulkan_normal_quantile_f64.comp -o
        /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`; `spirv-val
        --target-env vulkan1.0 /tmp/omni_tensor_vulkan_normal_quantile_f64.spv`;
        `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
        Vulkan direct/map Float64 quantile smokes plus domain/non-finite
        smokes; host focused `advanced-collections-module` `pass=1317 fail=0`;
        bounded-container focused `advanced-collections-module`
        `pass=1300 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
    - [x] Add Vulkan `Float32` `stats/normal-cdf` through the dedicated unary
      helper.
      - closed: dense row-major Vulkan `Float32` tensors now route public
        `(map stats/normal-cdf ...)` and direct `(stats/normal-cdf ...)`
        through fixed op id `19`, preserving Vulkan placement and `Float32`
        dtype with a documented same-dtype shader approximation.
      - validation: shader compile/`spirv-val`, helper rebuild, direct Vulkan
        smokes, and host focused `advanced-collections-module`
        `pass=1296 fail=0`.
    - [x] Resolve Vulkan `Float64` `stats/normal-cdf` with an explicit double
      approximation or an explicit fail-closed policy.
      - closed: dense row-major Vulkan `Float64` tensors now route public
        `(map stats/normal-cdf ...)` and direct `(stats/normal-cdf ...)`
        through fixed op id `19` in the existing two-buffer Float64 unary
        helper, preserving Vulkan placement and `Float64` dtype.
      - implementation note: the shader uses a bounded piecewise polynomial
        approximation over `|x| < 8` and saturates outside that range. The
        fitted approximation was sampled against the CPU double oracle with
        maximum absolute error below `6e-8`; checked-in tests use a stricter
        `1e-6` tolerance around representative values.
      - preserved contract: no hidden CPU fallback and no Float64-to-Float32
        downcast. Vulkan `Float64` `stats/normal-quantile` is covered by its
        later Float64 inverse-CDF/status helper.
      - validation: `glslangValidator -V --target-env vulkan1.0
        csrc/tensor_vulkan_map_unary_f64.comp -o
        /tmp/omni_tensor_vulkan_map_unary_f64.spv`; `spirv-val --target-env
        vulkan1.0 /tmp/omni_tensor_vulkan_map_unary_f64.spv`;
        `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`; direct
        Vulkan direct/map Float64 CDF smokes; Float64 quantile fail-closed
        smoke; host focused `advanced-collections-module` `pass=1313 fail=0`;
        bounded-container focused `advanced-collections-module`
        `pass=1300 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
    - [x] Define clone semantics for arbitrary foreign CUDA tensor payloads
      before accepting them across concrete clone/copy boundaries.
      - closed: valid foreign CUDA concrete payloads now clone by allocating a
        fresh Omni-owned CUDA buffer, copying device-to-device from the source
        handle, and installing `tensor_cuda_device_finalizer` on the clone. The
        source foreign finalizer remains the sole authority for the source
        handle.
      - preserved boundary: fake, stale, malformed, or otherwise invalid CUDA
        handles still fail closed rather than being accepted as cloneable
        storage.
      - regression coverage: the memory-lifetime suite covers the invalid fake
        CUDA handle path and a real foreign-finalizer CUDA handle path that
        round-trips copied values, uses a distinct clone handle, and verifies
        source finalizer ownership.
      - validation: `c3c build --obj-out obj`; bounded-container
        `memory-lifetime-smoke` `pass=229 fail=0` with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`; host focused
        `advanced-collections-module` `pass=1258 fail=0`.
    - [x] Add explicit zero-size CUDA contract identity/fill support before
      relaxing CUDA zero-dimension contract gates.
      - closed: CUDA-placed dense row-major matching `Float64` or matching
        `Float32` rank-2/rank-2, rank-2/rank-1, and rank-1/rank-2
        single-axis contractions now preserve zero-free outputs as
        CUDA-placed zero-length tensors and fill non-empty zero-contracted
        outputs with the dtype-preserving additive identity. The zero-fill
        path skips cuBLAS and therefore also succeeds when cuBLAS is forced
        unavailable for tests.
      - validation: `c3c build --obj-out obj`; direct CUDA availability-gated
        smokes for `Float64` zero-contracted rank-2, `Float32`
        zero-contracted rank-2 with scalar dtype preservation, zero-free
        rank-2 output preservation, and rank-1/rank-2 zero-contracted fill.
        Host focused `advanced-collections-module` was attempted but remained
        idle past ten minutes and was terminated as inconclusive;
        bounded-container focused `advanced-collections-module` passed
        `pass=1210 fail=0`.
    - [x] Add CUDA rank-1/rank-1 dot support for the existing cuBLAS contract
      family.
      - closed: CUDA-placed contiguous matching `Float64` or matching
        `Float32` rank-1/rank-1 single-axis contractions now return
        CUDA-placed scalar Tensor outputs. The implementation reuses the
        existing cuBLAS GEMV helper by treating the left vector as a one-row
        row-major matrix, so no new CUDA helper ABI was required.
      - preserved contract: zero-size rank-1 dots use the CUDA additive
        identity fill path; nonzero rank-1 dots still fail unavailable when
        cuBLAS is forced unavailable; CUDA `map`, mixed devices/dtypes,
        multi-axis CUDA contract, and zero-axis rank-1 outer product remain
        outside this slice.
      - validation: `c3c build --obj-out obj`; direct CUDA availability-gated
        smokes for `Float64` rank-1 dot, explicit-axis rank-1 dot, `Float32`
        rank-1 dot with scalar dtype preservation, zero-size rank-1 dot
        identity, rank-0 destination realization for `Float64` rank-1 dot,
        and rank-0 destination realization for `Float32` rank-1 dot;
        bounded-container focused `advanced-collections-module` passed
        `pass=1217 fail=0`.
    - [x] Decide and implement any non-CPU destination `realize` /
      destination-copy contract explicitly.
      - shipped contract: destination-form `realize` stays backend-neutral and
        now supports existing dense row-major CUDA and Vulkan `Float64` or
        `Float32` destinations for matching same-backend sources, CPU sources,
        supported lazy same-backend results, and scalar fills; CPU destinations
        still require explicit `to-device 'cpu` for device sources.
      - validation: public CPU/CUDA/Vulkan destination-copy tests, direct
        smokes, helper rebuild, `c3c build --obj-out obj`, host focused
        advanced collections, docs parity, Stage 3 source parity, and targeted
        `git diff --check` passed for this checkpoint. Bounded-container
        focused advanced collections also passed after the CUDA addition.
    - [ ] Reopen full blocked/tiled trailing-update LU only if new
      measurements justify it as a performance lane.
      - blocker/defer reason: current measured parallel solve baseline routes
        `n >= 65` through staged multi-dispatch solve; blocked trailing-update
        LU is not required for correctness closure.
      - next step: collect comparative measurements showing the existing
        staged route is the bottleneck before broadening algorithmic scope.
      - validation: checked-in measurement artifact, no-LAPACK solve tests,
        focused host plus bounded-container advanced collections tests.
    - [ ] Run broad/heavy bounded-container validation before closing the whole
      `TENSOR-100F` parent.
      - blocker/defer reason: recent source-level slices ran focused host and
        bounded-container validation, not full heavy/container-only gates. The
        scalar `Float32` slice now has a bounded focused
        `advanced-collections-module` pass, but parent-level closure still
        needs the broader Docker-bound suite required by repo policy.
      - next step: when the parent is otherwise ready to close, run the
        Docker-bound suite required by repo policy rather than host-heavy
        execution.
      - validation: record the exact bounded-container command, pass/fail
        counts, and any unresolved failures in this TODO or the session report.
    - [ ] Fix the validation Docker image C3 compiler architecture on arm64
      hosts.
      - blocker/defer reason: `scripts/build_validation_image.sh` currently
        rebuilds an arm64 Ubuntu image but `docker/validation.Dockerfile`
        downloads `c3-linux.tar.gz`, whose `/opt/c3/c3c` is x86-64. Running the
        container path without a host toolchain mount fails before tests with
        `/usr/local/bin/c3c: cannot execute binary file: Exec format error`.
      - next step: update the validation Dockerfile/build script to select a
        native arm64 C3 compiler artifact or build C3 from source for arm64;
        until then run bounded validation with
        `OMNI_VALIDATION_TOOLCHAIN_ROOT=/home/christos/.local`.
      - validation: `docker run --rm --entrypoint /bin/sh
        omni-validation:2026-03-10 -lc 'uname -m; file /opt/c3/c3c'` must show
        matching arm64/aarch64, then rerun `scripts/run_validation_container.sh`
        without `OMNI_VALIDATION_TOOLCHAIN_ROOT`.
  - shipped `TENSOR-100F1` slice:
    - factored shared trailing-status copyback for serial Vulkan matrix
      helpers;
    - added dense row-major `Float64` Vulkan `matrix/cholesky`, returning a
      Vulkan-placed lower factor and raising `tensor/not-positive-definite`
      for nonsymmetric or non-SPD inputs without calling LAPACK;
    - added dense row-major `Float64` Vulkan `matrix/qr`, returning
      Vulkan-placed `q` and `r` reduced factors and raising
      `tensor/singular-matrix` for rank-deficient inputs without calling
      LAPACK;
    - added dense row-major Vulkan `Float64` `matrix/singular-values`,
      returning a Vulkan-placed rank-1 result. The helper now uses
      storage-buffer Gram scratch behind the public output payload, so
      `k = min(rows, columns) > 64` is supported without hidden CPU/LAPACK
      fallback while preserving 32-bit shader index guards;
    - routed Vulkan `matrix/norm` `'spectral` and `'nuclear` through the same
      singular-value helper, reading back only the scalar norm result;
    - fixed the CPU pure Cholesky fallback to accept empty square tensors.
  - shipped unary `+` / unary `abs` / unary `-` / unary `sqrt` Vulkan slice:
    - added a separate two-buffer `Float64` unary map shader/helper for
      identity, absolute value, negation, and square root instead of reviving the
      invalidated generic map mode-3 branch;
    - routed public `(map + <vulkan-tensor>)` through the identity opcode for
      dense row-major Vulkan-placed `Float64` tensors, and made direct
      `(+ <tensor>)` the Tensor identity form that preserves placement;
    - routed public `(map abs <vulkan-tensor>)` and direct
      `(abs <vulkan-tensor>)`, public `(map - <vulkan-tensor>)`, and direct
      `(- <vulkan-tensor>)` through that helper for dense row-major
      Vulkan-placed `Float64` tensors;
    - routed public `(map sqrt <vulkan-tensor>)` and direct
      `(sqrt <vulkan-tensor>)` through that helper for dense row-major
      Vulkan-placed `Float64` tensors;
    - routed public `(map real-part <vulkan-tensor>)`,
      `(map imag-part <vulkan-tensor>)`, `(map conjugate <vulkan-tensor>)`,
      and direct Tensor `real-part` / `imag-part` / `conjugate` through the
      helper for real-valued Vulkan `Float64` tensors;
    - routed public `(map min <vulkan-tensor> scalar)`,
      `(map max scalar <vulkan-tensor>)`, and direct Tensor `min` / `max`
      through the existing binary map helper for dense row-major Vulkan
      `Float64` tensor/scalar and tensor/tensor broadcast cases.
  - shipped Vulkan destination-form `realize` slice:
    - kept the public `(realize source destination)` API backend-neutral;
    - added existing-buffer Vulkan host-to-device, device-to-device, and
      scalar `Float64` fill helpers for destination realization;
    - supports matching dense row-major `Float64` CPU sources, Vulkan sources,
      lazy Vulkan results, and scalar fills into existing Vulkan destinations;
    - preserved CPU destination fail-closed behavior for device sources unless
      the caller explicitly copies with `to-device 'cpu`.
  - shipped scalar unary `-` surface fix:
    - fixed the documented scalar unary `-` surface by registering `-` as a
      primitive that accepts one or two arguments, while `prim_sub` still
      rejects extra arguments itself;
    - direct CPU Tensor unary `-` now preserves native Tensor dtype for
      `Float64`, `BigInteger`, `BigFloat`, and `BigComplex`;
  - shipped Vulkan helper factoring proof slice:
    - reused the existing compute-pipeline creation helper across the serial
      two-buffer, three-buffer, one-input/two-output, and one-input/three-output
      Vulkan dispatch helpers;
    - left descriptor set layout, pool allocation, command recording, and
      status-copy ABI unchanged for follow-up factoring.
    - direct Tensor unary math on Vulkan now fails closed for unsupported
      operations instead of materializing hidden CPU results;
    - results remain Vulkan-placed and require explicit `(to-device ... 'cpu)`
      for CPU inspection;
    - unsupported Vulkan unary callables such as `sin` still fail closed with
      `tensor/backend-unsupported`.
  - shipped audit-hardening slice:
    - added public zero-size Vulkan `matrix/norm` `'spectral` / `'nuclear`
      regressions for empty rows and empty columns, with unchanged `dgesvd`
      counter coverage;
    - added unchanged LAPACK counter guards for direct and lazy Vulkan
      fail-closed `matrix/eigenpairs`; earlier `matrix/svd`,
      `matrix/eigenvalues`, and `matrix/eigenvectors` fail-closed guards are
      superseded by the direct Vulkan slices below;
    - added diagnostic-order regressions proving invalid-but-Vulkan SVD and
      general eigenpair operands return `tensor/backend-unsupported` before CPU
      shape diagnostics;
    - added internal lazy-contract helper coverage for zero-axis and multi-axis
      Vulkan contractions;
    - updated the generic Vulkan contract failure context from stale
      single-axis wording to `contract: Vulkan Float64 contract kernel failed`.
  - shipped direct Vulkan `matrix/svd` factor-output slice:
    - added `csrc/tensor_vulkan_svd_f64.comp`, generated
      `csrc/tensor_vulkan_svd_f64_spv.c`, and the
      `omni_tensor_backend_vulkan_svd_f64` helper for dense row-major
      Vulkan `Float64` rank-2 inputs;
    - public `matrix/svd` now routes concrete and lazy Vulkan inputs through
      Vulkan before the CPU SVD fallback and returns Vulkan-placed `u`, `s`,
      and `v` tensors with shapes `[rows k]`, `[k]`, and `[cols k]`;
    - the Vulkan path preserves the existing reduced SVD dictionary contract,
      keeps CPU inspection explicit through `to-device 'cpu`, and asserts no
      hidden `LAPACKE_dgesvd` counter movement for direct or lazy Vulkan
      inputs;
    - the factor-output helper now uses storage-buffer Gram scratch behind the
      public `u` output payload, so `k = min(rows, columns) > 64` is supported
      without hidden CPU/LAPACK fallback while preserving 32-bit shader index
      guards; unsupported dtype/layout and unsupported non-CPU cases fail
      closed with Tensor backend diagnostics.
  - shipped direct Vulkan symmetric eigen slice:
    - added `csrc/tensor_vulkan_symmetric_eigen_f64.comp`, generated
      `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c`, and the
      `omni_tensor_backend_vulkan_symmetric_eigen_f64` helper for dense
      row-major Vulkan `Float64` square inputs;
    - public `matrix/eigenvalues` and `matrix/eigenvectors` now route concrete
      and lazy Vulkan inputs through Vulkan before the CPU resolver and return
      Vulkan-placed values and aligned vector columns;
    - the Vulkan path preserves the existing symmetric-real output contracts,
      keeps CPU inspection explicit through `to-device 'cpu`, maps
      nonsymmetric Vulkan input to `tensor/not-symmetric`, and asserts no
      hidden `LAPACKE_dsyev` counter movement;
    - the symmetric eigen helper uses storage-buffer scratch behind the public
      Vulkan outputs, so dense row-major Vulkan `Float64` square symmetric
      inputs support `n > 64` within helper resource limits, 32-bit shader
      index guards, and the Jacobi iteration guard; unsupported dtype/layout,
      unsupported shape/resource bounds, missing Vulkan/Float64 capability,
      and unsupported non-CPU cases fail closed with Tensor backend diagnostics.
  - shipped first `TENSOR-100G` slice:
    - added `omni_tensor_backend_vulkan_solve_parallel_f64`, generated
      `csrc/tensor_vulkan_solve_parallel_f64_spv.c`, and routed public
      `matrix/solve` to it for Vulkan `Float64` systems with `n >= 3`;
    - kept the serial Vulkan solve helper as the small-size path for `n < 3`;
    - implemented a 64-invocation workgroup-parallel partial-pivot Gaussian
      elimination shader with parallel pivot reduction, row swaps,
      elimination, and RHS-column back-substitution;
    - added an explicit private `uint` status buffer for the parallel helper
      instead of extending the old trailing-`double` serial status sentinel;
    - added availability-gated tests for parallel vector RHS, matrix RHS,
      rank/shape preservation, singular diagnostics, parallel-helper counter
      movement, and unchanged LAPACK `dgesv` call count.
  - shipped staged `TENSOR-100G` slice:
    - added `csrc/tensor_vulkan_solve_parallel_init_f64.comp` and generated
      `csrc/tensor_vulkan_solve_parallel_init_f64_spv.c`;
    - records init/copy and factor/solve dispatches in one command buffer with
      a `vkCmdPipelineBarrier` buffer-memory dependency between them;
    - init dispatch covers `max(n*n, n*rhs_cols)` work items, so coefficient
      and RHS copies are not capped at one 64-lane workgroup;
    - added regressions for 2x2 serial-threshold counter stability, staged
      pivoting, 9x9 staged copy beyond one workgroup, and post-elimination
      singular diagnostics.
  - shipped multi-dispatch `TENSOR-100G` slice:
    - added pivot/swap, elimination, and backsolve shaders under
      `csrc/tensor_vulkan_solve_multi_*_f64.comp`;
    - generated and wired the corresponding SPIR-V C sources;
    - reworked the parallel solve helper to record init/copy, per-pivot
      pivot/swap, multi-workgroup elimination, and descending backsolve
      dispatches into one Vulkan command buffer with buffer barriers between
      dependent stages;
    - added a private multi-dispatch helper counter and tests for serial
      threshold isolation, counter movement, no-LAPACK behavior, and dense 9x9
      elimination correctness.
  - shipped cross-workgroup pivot `TENSOR-100G` slice:
    - split pivot selection into scan, ping-pong reduce, commit, and row-swap
      shader stages;
    - added a private `double` pivot magnitude scratch buffer and expanded the
      private typed `uint` status buffer for candidate row scratch slots;
    - reworked the solve command buffer so each pivot scans candidate rows
      across workgroups, recursively reduces candidates with barriers, commits
      the selected row, then swaps rows before elimination;
    - added a private pivot-reduce counter and a 65x65 solve regression that
      proves the cross-workgroup pivot-reduction stage executes without LAPACK.
  - shipped tiled LU staging `TENSOR-100G` slice:
    - added `csrc/tensor_vulkan_solve_multi_factor_f64.comp` and generated
      SPIR-V wiring for a per-pivot panel-factor stage;
    - expanded the private solve ABI with a factor scratch buffer at binding
      `5`, a factor pipeline, and four-buffer barriers covering output,
      status, pivot scratch, and factor scratch;
    - the helper now computes `L[row,pivot]` factors in their own dispatch
      after row swap and before elimination, then elimination consumes staged
      factors instead of recomputing them in every coefficient/RHS update;
    - added a private factor-stage counter and dense 65x65 `I + J`
      regression proving the factor stage executes without LAPACK while
      preserving the public solve contract.
  - shipped measured-threshold `TENSOR-100G` slice:
    - replaced the fixed `n >= 3` bring-up route with private
      `OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_MIN_N = 65`;
    - added a private serial solve call counter so tests prove below-threshold
      systems execute the serial helper directly instead of only proving
      parallel counters stayed unchanged;
    - recorded local in-process threshold evidence under `build/`, where
      `3`, `9`, `16`, and `32` were tied or serial-favorable and `65` was the
      first tested size where the staged parallel route won across identity and
      dense fixtures;
    - added availability-gated tests proving `2x2` and `9x9` stay serial,
      `65x65` enters the parallel helper without LAPACK, pivot/factor counters
      still move, and a `65x65` singular system uses the parallel status path.
  - shipped Vulkan `Float32` staged parallel solve slice:
    - added dedicated `Float32` ports of the staged Vulkan solve shader family
      and generated SPIR-V C embeds;
    - made the staged solve helper dtype-aware and added
      `omni_tensor_backend_vulkan_solve_parallel_f32` without hidden
      widening/downcast, CPU fallback, or LAPACK routing;
    - routed public Vulkan `Float32` `matrix/solve` systems with `n >= 65`
      through the staged helper while preserving serial routing for `2x2` and
      `9x9`;
    - added dtype-specific `Float32`/`Float64` solve counters so tests prove
      the staged `Float32` path does not borrow the `Float64` helper;
    - recorded local parity timing logs under
      `build/vulkan_solve_f32_threshold_20260417_*`: on this stack the
      `65x65` staged and forced-serial `Float32` identity/dense probes were
      tied within measurement noise, so `65` is a parity threshold rather than
      a decisive speedup claim.
  - current negative constraints:
    - do not resume the rolled-back generic Vulkan unary `map` mode-3 branch;
      the shipped unary `+` / `abs` / unary `-` / unary `sqrt` support uses a
      separate unary shader/helper ABI;
    - do not treat the serial Vulkan LU/solve/inverse shaders as the
      performance solution;
    - do not lower `BigInteger`, `BigFloat`, or `BigComplex` to Vulkan;
    - do not hide unsupported Vulkan cases behind CPU fallback.
  - next checkpoint: continue broader Vulkan math-library baseline work.
    Direct Vulkan `matrix/svd` factor output has landed for dense row-major
    `Float64` and `Float32` inputs and now uses storage-backed Gram scratch for `k > 64`;
    keep the plan note as the design record and performance tracker. Direct
    Vulkan `matrix/eigenvalues` and
    `matrix/eigenvectors` have landed for dense row-major `Float64` square
    symmetric inputs, including `n > 64` through storage-buffer scratch within
    helper resource limits, 32-bit shader index guards, and the Jacobi
    iteration guard; keep the eigen plan note as the residual-boundary and
    performance tracker. Future `Float32` planning now lives in
    `docs/plans/vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`.
    Direct general `matrix/eigenpairs` remains intentionally CPU-only and
    fail-closed on Vulkan operands until its gates land. Vulkan `Float32`
    execution has landed for placement/copyback, destination `realize`,
    elementwise `map`, unary helpers, direct `min`/`max`, rank-N `contract`,
    structural matrix kernels (`transpose`, `diagonal`, `diagonal-matrix`,
    `trace`), direct `matrix/rank`, direct `matrix/norm` selectors,
    `matrix/singular-values`, `matrix/svd`, serial factor/solve surfaces,
    staged parallel Vulkan `matrix/solve`, large-dense SVD robustness, and CPU
    factor/SVD oracle surfaces. CUDA `Float32` placement/copyback,
    destination `realize`, and eligible cuBLAS contract routing have also
    landed; scalar `Float32` runtime values are closed above. Remaining CUDA
    fixed-width complex structural matrix kernels, fixed-width complex numerical
    matrix kernels, and stride/view-backed Vulkan layouts stay
    fail-closed/deferred until dedicated gates land.
    `matrix/singular-values` /
    `matrix/svd` now support `k > 64` through
    storage-backed Gram scratch, while symmetric eigen now supports `n > 64`
    through storage-buffer scratch within helper resource limits, 32-bit shader
    index guards, and the Jacobi iteration guard. Unary `+`,
    `abs`, unary `-`, and unary `sqrt` have a separate unary Vulkan helper, and
    full blocked trailing-update LU remains a future performance lane only if
    measurements justify it.

## Recently Closed

- [x] `TENSOR-100F` Vulkan helper plumbing through status-readback
  - shipped slices: the mature serial Vulkan dispatch helper family now shares
    compute-pipeline creation, sequential storage-buffer descriptor
    layout/allocation/update helpers, standard one-time command
    recording/submission, and shared `Float64` status-payload readback
    boilerplate for compatible mapped-status paths.
  - shipped contract: status-domain semantics remain separate. Generic trailing
    `double` nonzero still maps to singular; SVD/singular-values status still
    distinguishes no-convergence; symmetric eigen status still distinguishes
    not-symmetric; and parallel solve intentionally remains on its separate
    `uint32_t` status-buffer ABI.
  - proof migration: serial solve, inverse, cholesky, QR, singular-values, SVD,
    and symmetric eigen status readers use the shared mapped readback path, and
    LU metadata validation reuses the generic trailing-status mapper while
    keeping pivot and swap-count validation LU-specific.
  - validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    host focused `advanced-collections-module` `pass=995 fail=0`,
    bounded-container focused `advanced-collections-module` `pass=982 fail=0`,
    primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed.

- [x] `TENSOR-076C` native `Iterator(Tensor)` view semantics
  - shipped contract: `Iterator(^Tensor)` yields flat row-major Tensor elements,
    matching `(Array tensor)` and `(List tensor)` collection conversions.
    Lazy CPU Tensor expressions are realized once into Tensor storage before
    iteration; non-CPU device tensors fail closed until explicitly copied with
    `to-device 'cpu`.
  - validation: `c3c build --obj-out obj`, direct `Iterator(Tensor)` smokes,
    host focused `advanced-collections-module` `pass=969 fail=0`,
    bounded-container focused `advanced-collections-module` `pass=956 fail=0`,
    primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed.

- [x] `TENSOR-076B` constructor-driven iterator materialization into `Tensor`
  - shipped slice: `(Tensor iterator)`, `(Tensor iterator dtype)`, and
    `(Tensor dtype shape iterator)` now consume finite numeric iterators through
    constructor dispatch. Iterator pipelines such as
    `(Tensor (map f (Iterator xs)))` materialize through `Tensor` without
    introducing public `delay`, `force`, or a special lazy realization surface.
  - docs: `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md` now prioritize constructor-driven materialization and
    demote `realize` to low-level destination-storage use.
  - validation: `c3c build --obj-out obj`, direct iterator-to-Tensor smokes,
    host focused `advanced-collections-module` `pass=964 fail=0`,
    bounded-container focused `advanced-collections-module` `pass=951 fail=0`,
    primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed.

- [x] `TENSOR-100G` complete measured Vulkan parallel solve baseline
  - shipped solver path: public `matrix/solve` now routes Vulkan dense
    row-major `Float64` systems with `n >= 65` through the staged
    multi-dispatch parallel helper and keeps smaller systems on the serial
    Vulkan helper.
  - shipped implementation slices: separate parallel solve helper, staged
    init/copy and factor/solve dispatches, multi-dispatch pivot/swap,
    elimination and backsolve, cross-workgroup pivot reduction, per-pivot
    factor staging, private typed status buffers, no-LAPACK counter coverage,
    and measurement-backed threshold routing.
  - remaining solver direction: full blocked trailing-update LU is not current
    correctness-required work; reopen it only as a separate performance item if
    measurements justify the extra algorithmic scope.

- [x] `TENSOR-100E` complete portable Vulkan Tensor backend baseline
  - shipped baseline: runtime-optional Vulkan placement/introspection,
    explicit `to-device` roundtrips, SPIR-V `map` arithmetic and broadcasting,
    zero-or-more-axis `contract`, and baseline Vulkan matrix kernels for
    `transpose`, `diagonal`, `diagonal-matrix`, `trace`, `rank`, `lu`,
    `solve`, `determinant`, and `inverse`.
  - subsequent `TENSOR-100F1` shipped extensions added `cholesky`, `qr`,
    `matrix/singular-values`, and spectral/nuclear norm routing; keep that
    ownership under the live `TENSOR-100F` history above.
  - route forward: the remaining Vulkan math direction now lives in
    `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` as active
    `TENSOR-100F` work, with `TENSOR-100G` closed as the measured parallel
    solver baseline.

- [x] `TENSOR-100D` add cuBLAS rank-2 CUDA contract fast path
  - shipped slice: runtime-loaded cuBLAS handle lifecycle now lives in the
    optional CUDA helper; `tensor-backends` reports a structured `cublas`
    backend entry.
  - execution: CUDA-placed dense row-major `Float64` rank-2/rank-2
    single-axis contractions route to `cublasDgemm_v2` for `[1 0]`, `[0 0]`,
    `[1 1]`, and `[0 1]`; rank-2/rank-1 and rank-1/rank-2 single-axis
    contractions route to `cublasDgemv_v2`. Results are CUDA-placed Tensors;
    callers copy back explicitly with `to-device 'cpu`.
  - unsupported CUDA contract shapes, dtypes, devices, or axis pairs fail with
    Tensor backend diagnostics instead of silently copying to CPU; zero-size
    CUDA contract dimensions fail closed until an explicit CUDA zero-fill path
    exists.
  - coverage: focused regressions cover all four cuBLAS rank-2/rank-2
    single-axis layouts plus matrix-vector and vector-matrix layouts when
    available, then force cuBLAS unavailable through the private
    backend-disable hook and assert both `tensor-backends` inventory reporting
    and `tensor/backend-unavailable` contract failure.
  - validation: `c3c build --obj-out obj`; direct host CUDA/cuBLAS contract
    smoke returned `154.0`; host focused `advanced-collections-module`
    pass=646 fail=0; bounded-container focused `advanced-collections-module`
    pass=633 fail=0; bounded-container `memory-lifetime-smoke` pass=226
    fail=0; bounded-container full `advanced` pass=1950 fail=0;
    `./scripts/build_omni_chelpers.sh`, primitive docs parity, Stage 3 source
    parity, and `git diff --check` passed. ASAN build was attempted but
    blocked by the local C3 sanitizer platform guard.

- [x] `TENSOR-100C` add optional CUDA Tensor copy backend
  - shipped slice: added a runtime-loaded `libcudart` helper with no required
    CUDA link dependency. CUDA availability now requires symbol resolution,
    visible device count, and a successful allocation/free probe.
  - current `to-device` behavior: target `'cuda` realizes CPU Tensor
    expressions and copies concrete `Float64` or `Float32` storage to CUDA
    when available; target `'cpu` copies CUDA Tensor storage back to native CPU
    storage.
  - unsupported CUDA dtypes outside `Float64` and `Float32` fail with
    `tensor/backend-unsupported`; unavailable or unusable CUDA fails with
    `tensor/backend-unavailable`.
  - validation: `c3c build --obj-out obj`; host focused
    `advanced-collections-module` pass=633 fail=0; direct host CUDA
    roundtrip returned `2.0`; bounded-container focused
    `advanced-collections-module` pass=619 fail=0; bounded-container
    `memory-lifetime-smoke` pass=226 fail=0; `git diff --check` passed.

- [x] `TENSOR-100B` add Tensor placement metadata and backend inventory
  - shipped slice: `TensorVal` now carries internal placement metadata and an
    opaque device handle/finalizer slot; ordinary constructors initialize CPU
    placement, `device` reports the stored placement, and CPU Tensor kernels
    reject non-CPU storage instead of silently reading backend handles.
  - public introspection: `tensor-backends` is registered and returns a list of
    dictionaries; CPU reports `available true`, while CUDA reports
    `available false` with reason `backend-unavailable`.
  - ownership guard: boundary clones refuse opaque non-CPU Tensor payloads
    until explicit copy semantics land, and `tensor_free_payload` is the single
    release authority for opaque device handles.
  - validation: `c3c build --obj-out obj`; host focused
    `advanced-collections-module` pass=633 fail=0; bounded-container focused
    `advanced-collections-module` pass=619 fail=0; bounded-container
    `memory-lifetime-smoke` pass=226 fail=0; direct `tensor-backends` and
    `to-device 'cpu` smokes passed.

- [x] `TENSOR-100A` implement CPU-only Tensor placement/introspection surface
  - shipped slice: `device` reports `'cpu` for current Tensor values, and
    `to-device` with target `'cpu` realizes Tensor expressions to CPU Tensor
    storage.
  - CUDA behavior: `to-device` with target `'cuda` fails closed with
    `tensor/backend-unavailable`; no CUDA runtime dependency was added.
  - registration: interpreter primitive table and AOT primitive hash table.
  - validation: `c3c build --obj-out obj`; focused
    `advanced-collections-module` pass=629 fail=0; direct `device` smoke
    returned `"cpu"`; direct handled CUDA smoke returned
    `"tensor/backend-unavailable"`; bounded-container focused
    `advanced-collections-module` pass=616 fail=0; bounded-container full
    `advanced` slice pass=1933 fail=0; primitive docs parity, Stage 3 source
    parity, and `git diff --check` passed.

- [x] `TENSOR-100` close explicit-device CUDA/cuBLAS backend design
  - shipped slice: `docs/plans/cuda-cublas-backend-decision-2026-04-16.md`
    locks future GPU support behind `Tensor` with `to-device`, `device`, and
    `tensor-backends` as the first placement/introspection surface.
  - rejected first-surface forms: `GpuTensor`, `CudaTensor`,
    backend-flavored math names, `tensor-use-backend!` as first control
    surface, and implicit CPU/GPU transfer inside ordinary Tensor operations.
  - next implementation boundary: Tensor placement metadata, CPU-only
    `device`, fail-closed `to-device` diagnostics when CUDA is unavailable,
    then opaque CUDA buffer ownership tests before cuBLAS execution.
  - validation: documentation/reference grep checks and `git diff --check`.

- [x] `ADV-STACK-001` calibrate macro-hygiene non-tail recursion headroom
  - shipped slice: the `advanced-macro-hygiene-string-number` non-tail
    recursion probe now uses depth `512`, which is portable across the tighter
    ARM64 host stack and the bounded validation container.
  - invalidated assumption: depths `896` and `1200` are not portable stack
    headroom contracts on this workspace; the host crashed at `640`/`896`, and
    the bounded container crashed at `1200`.
  - validation: `c3c build --obj-out obj`; host and bounded-container
    `advanced-macro-hygiene-string-number` pass=9 fail=0; host and
    bounded-container `advanced-macro-hygiene` pass=83 fail=0; bounded
    container full `advanced` slice pass=1928 fail=0.

- [x] `TENSOR-090AQ` stabilize pure `matrix/eigenpairs` real-Schur fallback
  - shipped slice: pure no-`dgeev` fallback now treats isolated 2x2
    real-Schur blocks as converged, fixing a bounded-container-only corruption
    of the trailing real eigenvalue in a 3x3 real-plus-complex-block matrix.
  - validation: `c3c build --obj-out obj`; host focused
    `advanced-collections-module` pass=624 fail=0; bounded container focused
    `advanced-collections-module` pass=611 fail=0; direct forced no-`dgeev`
    value smoke returned `"2+0i"`; primitive docs parity, Stage 3 source
    parity, and `git diff --check` passed.

- [x] `TENSOR-090AP` add forced fallback coverage for QR LAPACK routines
  - shipped slice: a private test-only QR backend disable hook now lets focused
    tests prove `matrix/qr` preserves public results through the pure reduced
    QR fallback.
  - preserved the existing tolerance-based LAPACK QR rank guard.
  - validation: `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`;
    focused `advanced-collections-module` pass=624 fail=0; primitive docs
    parity, Stage 3 source parity, and `git diff --check` passed.

- [x] `TENSOR-090AO` add forced fallback coverage for `dpotrf` routines
  - shipped slice: a private test-only `dpotrf` disable hook now lets focused
    tests prove `matrix/cholesky` preserves public results through the pure
    lower-triangular Cholesky fallback.
  - validation: `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`;
    focused `advanced-collections-module` pass=622 fail=0; primitive docs
    parity, Stage 3 source parity, and `git diff --check` passed.

- [x] `TENSOR-090AN` add forced fallback coverage for `dgetrf` routines
  - shipped slice: a private test-only `dgetrf` disable hook now lets focused
    tests prove `matrix/lu` and `matrix/determinant` preserve public results
    through the pure partial-pivot LU fallback.
  - validation: `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`;
    focused `advanced-collections-module` pass=620 fail=0; primitive docs
    parity, Stage 3 source parity, and `git diff --check` passed.

- [x] `TENSOR-090AM` add forced fallback coverage for `dgesv` routines
  - shipped slice: a private test-only `dgesv` disable hook now lets focused
    tests prove `matrix/solve` and `matrix/inverse` preserve public results
    through the pure Gaussian solver fallback.
  - fixed backend control ownership: `dgesv` no longer checks the unrelated
    `dgeev` disable state.
  - validation: `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`;
    focused `advanced-collections-module` pass=616 fail=0; primitive docs
    parity, Stage 3 source parity, and `git diff --check` passed.

- [x] `TENSOR-090AL` add forced fallback coverage for symmetric eigen routines
  - shipped slice: a private test-only `dsyev` disable hook now lets focused
    tests prove `matrix/eigenvalues` and `matrix/eigenvectors` preserve their
    public results through the pure symmetric Jacobi fallback.
  - validation: `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`;
    focused `advanced-collections-module` pass=612 fail=0; primitive docs
    parity, Stage 3 source parity, and `git diff --check` passed.

- [x] `TENSOR-090AK` add forced fallback coverage for SVD-backed matrix routines
  - shipped slice: focused tests now force runtime `dgesvd` off and verify
    `matrix/rank`, `matrix/singular-values`, and `matrix/svd` preserve their
    public results through pure fallback paths.
  - validation: `c3c build --obj-out obj`; focused
    `advanced-collections-module` pass=606 fail=0; primitive docs parity,
    Stage 3 source parity, and `git diff --check` passed.

- [x] `TENSOR-090AJ` add `matrix/norm` SVD backend/fallback coverage
  - shipped slice: a private test-only `dgesvd` disable hook now lets focused
    tests prove `matrix/norm` `'spectral` and `'nuclear` selectors use the
    runtime LAPACK backend when available and preserve the same results through
    the pure SVD fallback.
  - validation: `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`;
    focused `advanced-collections-module` pass=598 fail=0; primitive docs
    parity, Stage 3 source parity, and `git diff --check` passed.

- [x] `TENSOR-090AI` add singular-value `matrix/norm` selectors
  - shipped slice: `matrix/norm` now accepts `'spectral` for the largest
    singular value and `'nuclear` for the sum of singular values, reusing the
    existing pure/LAPACK SVD machinery while preserving lazy input realization
    and empty axes as `0.0`.
  - validation: direct spectral/nuclear smokes returned `3.0`, `5.0`, `0.0`,
    and `0.0`; `c3c build --obj-out obj`; focused
    `advanced-collections-module` pass=592 fail=0; primitive docs parity,
    Stage 3 source parity, and `git diff --check` passed.

- [x] `TENSOR-090AH` add `matrix/norm`
  - shipped slice: `matrix/norm` returns `Float64` norms for rank-2 `Float64`
    Tensor values, defaulting to Frobenius and accepting `'frobenius`, `'one`,
    `'infinity`, or `'max` selectors.
  - validation: direct norm smokes returned `5.0`, `9.0`, `15.0`, and `5.0`;
    `c3c build --obj-out obj`; focused `advanced-collections-module` pass=590
    fail=0.

- [x] `TENSOR-090AG` add reusable `matrix/eigenpairs` residual harness
  - shipped slice: advanced collections/module tests now define reusable 3x3
    residual helpers and validate all returned vector columns for non-normal
    and real-plus-complex-block matrices under backend and forced-fallback
    execution.
  - validation: prototype backend/fallback helper smokes returned `true`;
    `c3c build --obj-out obj`; focused `advanced-collections-module` pass=580
    fail=0.

- [x] `TENSOR-090AF` add non-normal `matrix/eigenpairs` residual coverage
  - shipped slice: accelerated and forced-fallback paths now validate a
    non-normal upper-triangular matrix with a non-basis returned vector column
    satisfying `A*v ~= lambda*v`.
  - validation: direct backend and forced-fallback residual smokes returned
    `true`; `c3c build --obj-out obj`; focused `advanced-collections-module`
    pass=578 fail=0.

- [x] `TENSOR-090AE` add accelerated `matrix/eigenpairs` residual coverage
  - shipped slice: when runtime `LAPACKE_dgeev` is available, focused tests now
    validate representative real and complex returned vector columns satisfy
    `A*v ~= lambda*v`.
  - validation: direct backend residual smokes returned `true`;
    `c3c build --obj-out obj`; focused `advanced-collections-module` pass=576
    fail=0.

- [x] `TENSOR-090AD` broaden pure `matrix/eigenpairs` fallback coverage
  - shipped slice: forced no-`dgeev` tests now cover 3x3 diagonal,
    upper-triangular, and real-plus-complex-block matrices, plus residual
    checks that representative vector columns satisfy `A*v ~= lambda*v`.
  - validation: `c3c build --obj-out obj`; focused
    `advanced-collections-module` pass=574 fail=0.

- [x] `TENSOR-090AC` add a pure fallback for general `matrix/eigenpairs`
  - shipped slice: `matrix/eigenpairs` now falls back to a pure QR/nullspace
    path when runtime `LAPACKE_dgeev` is unavailable.
  - the fallback preserves the shipped contract: `BigComplex` `values` shape
    `[n]`, `BigComplex` `vectors` shape `[n n]`, sorted values, and aligned
    vector columns.
  - validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    focused `advanced-collections-module` pass=568 fail=0, forced no-dgeev CLI
    smokes, primitive docs parity, Stage 3 source parity, and `git diff --check`.

- [x] `LANG-SCALAR-BOOST-BIGINTEGER-096` add the first Boost.Multiprecision
  exact-integer scalar slice
  - shipped slice: `SCALAR-010A` Boost `BigInteger` core:
    - Added Boost.Multiprecision `cpp_int` storage behind an owned C++ helper
      archive and C ABI shim.
    - Added the language-facing `BigInteger` constructor for integers and
      decimal strings, type identity as a `Number` subtype, printing/String
      conversion, hashing/equality, and scope-boundary copy/promotion support.
    - Updated `+`, `-`, and `*` so fixed-width `Integer` overflow promotes to
      `BigInteger`; `BigInteger` combines with `Integer`/`BigInteger`, and
      mixed `Float64` arithmetic uses finite double conversion when possible.
    - Deferred `/`, `%`, ordering comparisons, bitwise operations, `gcd`/`lcm`,
      `BigFloat`/`BigComplex`, and `parse-number` arbitrary-precision parsing
      until each has an explicit surface contract.
    - Validation: `./scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
      direct BigInteger smokes, focused advanced numeric tests, full
      `advanced-stdlib-numeric` under `prlimit --stack=67108864`, and Stage 3
      e2e source parity passed.

- [x] `LANG-TENSOR-LAPACK-SOLVER-NAMING-097` record solver naming constraint
  for the next LAPACK/LAPACKE slice
  - decision note:
    - Do not expose solver/decomposition conveniences as a bare `solve`.
    - Do not lock `linalg/` as the base namespace yet.
    - Keep `tensor/lapack` as an implementation/backend ownership label until
      the public Tensor convenience namespace is explicitly chosen.

- [x] `LANG-FFI-FIRST-CLASS-GROUPED-MODULE-106` make Omni FFI grouped,
  first-class, and easier to bind
  - design note:
    `docs/plans/ffi-first-class-grouped-module-plan-2026-04-11.md`
    locks the first grouped form around `(define [ffi module] lib "path"
    (fn (^Type arg)) ^Return ...)` with no `->` syntax and no bracketed body
    entries for the first slice.
  - shipped slice: `FFI-GROUP-001` parser/lowering:
    - `(define [ffi module] lib "path" (fn (^Type arg)) ^Return ...)` lowers
      to the existing `[ffi lib]` and `[ffi lambda]` declarative FFI forms.
    - Malformed body pairs fail closed; `ForeignHandle` metadata policy,
      callable reflection, AOT preload, and manifest generation share the
      existing declarative FFI path.
  - shipped slice: `FFI-GROUP-002` grouped bindgen output option:
    - `[dependencies.ffi.NAME] raw-syntax = "grouped"` makes `--bind` emit
      grouped `[ffi module]` raw bindings.
    - The default remains `raw-syntax = "legacy"` so existing raw `[ffi lib]`
      plus `[ffi lambda]` generation stays available.
    - Unsupported `raw-syntax` values fail closed before header parsing.
  - shipped slice: `FFI-BIND-003` minimal grouped bindgen review note:
    - grouped raw output now carries a minimal syntax header note
      (`;; Raw syntax: grouped`) to make review diffs easier to scan without
      changing semantics.
  - shipped slice: `FFI-TOML-004A` facade prefix stripping:
    - `[dependencies.ffi.NAME] strip-prefixes = ["c_prefix_"]` is parsed and
      threaded into bindgen output generation.
    - Prefix stripping applies to facade/exported Omni names and raw import
      aliases only; raw binding names preserve the C-derived symbol names so
      `dlsym` stays correct without adding a separate `c-name` surface.
  - shipped slice: `FFI-TOML-004B` generated bind manifest:
    - `--bind` now writes `lib/ffi/<name>_manifest.toml` beside the raw and
      facade files.
    - The manifest records the effective shipped bindgen output config:
      dependency name, library, raw syntax, generated raw/facade paths, and
      `strip-prefixes`.
  - shipped slice: `FFI-TOML-004C` path-safe bind output dependency names:
    - `--bind` output path construction rejects empty FFI dependency names and
      names containing anything outside ASCII letters, digits, `_`, and `-`
      before writing raw, facade, or manifest files under `lib/ffi/`, keeping
      names safe as both file stems and generated Omni module names.
  - shipped slice: `FFI-TOML-004D` overlong bind dependency section names:
    - `[dependencies.ffi.NAME]` section names longer than the bind output stem
      limit now fail closed through the existing empty-name output guard instead
      of being silently truncated before raw, facade, or manifest generation.
  - shipped slice: `FFI-TOML-004E` prefix-stripped emitted name preflight:
    - bindgen validates generated Omni-facing function names before writing raw
      or facade files.
    - `strip-prefixes` fails closed if stripping would produce a name the Omni
      lexer reads as a number-leading token, such as `3d-distance`.
  - shipped slice: `FFI-TOML-004F` prefix-stripped emitted name collision
    preflight:
    - bindgen fails closed before writing raw or facade files if two generated
      Omni-facing function names collide after prefix stripping, such as
      `sqlite3_open` and `open` both emitting `open`.
  - shipped slice: `FFI-TOML-004G` strict `strip-prefixes` TOML parsing:
    - `strip-prefixes` now rejects malformed, empty, or overlong prefix entries instead
      of silently truncating them before generated-name rewriting.
    - `--bind` fails the dependency before header parsing when
      `strip-prefixes` is invalid.
  - shipped slice: `FFI-TOML-004H` strict bind dependency core TOML fields:
    - `library`, `raw-syntax`, `headers`, and `functions` now fail closed for
      missing required, malformed, empty, overlong, or incorrectly shaped values
      instead of silently truncating, accepting unquoted raw-syntax selectors,
      accepting missing required fields as no-ops, or accepting malformed arrays
      before header parsing.
    - `--bind` refuses the dependency before generating raw, facade, or
      manifest outputs when these fields are invalid.
  - shipped slice: `FFI-TOML-004I` duplicate bind dependency TOML key rejection:
    - `library`, `raw-syntax`, `headers`, `functions`, and `strip-prefixes`
      fail closed when repeated in one `[dependencies.ffi.NAME]` section instead
      of letting later keys overwrite the bind target, function filter, raw
      syntax mode, or generated-name rewrite policy.
  - shipped slice: `FFI-TOML-004J` duplicate FFI dependency section rejection:
    - Repeated `[dependencies.ffi.NAME]` sections mark both dependencies invalid
      instead of letting multiple entries target the same generated raw, facade,
      and manifest output stem.
    - `--bind` rejects invalid or empty dependency names before raw syntax
      resolution and header parsing.
  - shipped slice: `FFI-TOML-004K` inline comment handling for strict bind TOML
    values:
    - Strict FFI dependency value parsing strips `#` comments only outside
      quoted strings, keeping documented examples such as
      `library = "m" # comment` valid while preserving quoted values containing
      `#`.
  - shipped slice: `FFI-TOML-004L` dependency count overflow fail-closed:
    - `omni.toml` parsing records when `[dependencies.ffi.NAME]` sections
      exceed `TOML_MAX_DEPS`, and `--bind` exits before raw syntax resolution,
      header parsing, or output generation.
    - Overflow sections reset parser section state so their key/value lines
      cannot mutate the last accepted dependency.
  - shipped slice: `FFI-TOML-004M` malformed adjacent quoted scalar rejection:
    - strict FFI dependency scalar parsing rejects values such as
      `library = "sqlite3" "m"` instead of accepting the malformed tail as one
      library or raw-syntax string.
  - shipped slice: `FFI-TOML-004N` unknown FFI dependency key rejection:
    - unsupported keys in `[dependencies.ffi.NAME]` fail the dependency before
      libclang/header parsing, keeping typos such as `function = [...]` from
      accidentally binding all exported functions.
  - shipped slice: `FFI-TOML-004O` unsafe bind library stem rejection:
    - dependency `library` stems containing slash, backslash, quote, whitespace,
      or control characters fail before header parsing or generated
      raw/facade/manifest output.
    - Lower-level bindgen writers also reject unsafe library stems, keeping
      direct generator calls aligned with the `omni.toml` bind path.
  - shipped slice: `FFI-TOML-004P` explicit function filter match enforcement:
    - when `functions = [...]` is present, every requested C function must be
      discovered in the parsed headers.
    - Missing filter entries fail the dependency before output generation
      instead of silently producing a partial binding set or a successful no-op.
  - shipped slice: `FFI-TOML-004Q` partial header parse cleanup:
    - multi-header dependencies now release functions parsed from earlier
      headers when a later header fails to parse or trips the header-path guard.
    - Partial parse failures still fail before generated output, without
      leaving parsed parameter metadata on the error path.
  - shipped slice: `FFI-TOML-004R` section-header comment/context hardening:
    - `[dependencies.ffi.NAME] # comment` parses as the intended dependency
      section.
    - malformed section-header lines starting with `[` reset parser context;
      `FFI-TOML-004W` later tightened this into active-dependency invalidation.
  - shipped slice: `FFI-BIND-004S` raw/facade pair failure cleanup:
    - when a new raw module is written but facade generation fails, bindgen
      removes the new raw file before returning failure.
    - existing raw files are left in place on rerun failures.
  - shipped slice: `FFI-BIND-004T` manifest failure cleanup:
    - when raw/facade generation succeeds but manifest writing fails, first-time
      raw, facade, and manifest artifacts are cleaned before dependency failure.
    - existing raw, facade, or manifest artifacts are left in place on rerun
      failures.
  - shipped slice: `FFI-BIND-004U` atomic bindgen text writes:
    - raw, facade, and manifest writers now write through a sibling temp path
      and rename into place only after the full text write and close succeeds.
    - failed final renames clean temp output instead of leaving writer-level
      partial/truncated generated files at the target path.
  - shipped slice: `FFI-BIND-004V` anonymous bindgen parameter fallback naming:
    - anonymous C parameter fallback names now format the full numeric index
      (`arg123`, etc.) instead of producing invalid fallback symbols for
      three-digit parameter indexes.
  - shipped slice: `FFI-BIND-005A` bindgen return metadata fail-closed:
    - unsafe return-type metadata now causes code generation to fail before any
      raw/facade output is written.
    - runtime emission now stops on unsafe return metadata instead of partially
      writing a valid prefix and leaving a truncated module.
  - shipped slice: `FFI-BIND-005B` bindgen stale-output cleanup fail-closed:
    - cleanup helpers now fail closed when a newly written raw file cannot be
      removed after facade generation fails.
    - generated outputs from previous reruns are retained on failure.
  - shipped slice: `FFI-BIND-005C` bindgen metadata control-character hardening:
    - metadata/comment payload sanitizer now rejects all ASCII controls below `0x20`
      (including tabs and other non-newline controls) to close remaining
      generation attack surface.
    - overflow-guard bindgen test coverage includes a tab control character path
      that must fail closed with no raw/facade output.
  - shipped slice: `FFI-CALLBACK-007A` fail-closed generic callback scaffolds:
    - bindgen-generated callback wrapper helpers validate callback metadata but
      raise until the facade is edited for a concrete subsystem
      callback-handle shim.
    - Generic bindgen output no longer routes arbitrary callback parameters
      through the `uv` timer callback prototype.
  - shipped slice: `FFI-CALLBACK-007B` callback cleanup idempotence:
    - generated callback unregister helpers now keep nil cleanup as a true
      idempotent no-op while preserving fail-closed behavior for non-nil
      generic callback handles until a concrete subsystem shim is wired.
  - shipped slice: `FFI-BUFFER-009A` string-buffer manual-review fail-closed:
    - generated mutable string-buffer helpers now raise on `manual-review`
      teardown until the facade is edited with an explicit allocation and
      writeback policy.
    - `none` teardown buffers remain caller-owned pass-through values after
      validation.
  - shipped slice: `FFI-BUFFER-009B` string-buffer none-policy guards:
    - generated `none` teardown mutable string-buffer helpers now validate role
      and ownership before returning the caller-owned buffer.
  - shipped slice: `FFI-BUFFER-009C` string-buffer `inout` direction
    precedence:
    - bindgen now checks the `inout` name hint before the broader `out`
      substring, so names such as `inout_buffer` stay
      `buffer-direction=inout`.
  - shipped slice: `FFI-BIND-010A` byte-pointer classification:
    - bindgen now treats only a plain `char` token as string-shaped.
    - `signed char*`, `unsigned char*`, and their `const` variants stay opaque
      `ForeignHandle` pointers instead of string-buffer or string-return
      scaffolds.
  - shipped slice: `FFI-BIND-010B` const-pointee char-pointer classification:
    - bindgen now distinguishes pointee const from top-level pointer const for
      plain `char*` spellings.
    - `const char*` and `char const*` stay string-input shaped, while
      `char* const` remains a mutable string-buffer contract and goes through
      the fail-closed buffer wrapper path.
  - shipped slice: `FFI-BIND-010C` char-pointer depth classification:
    - bindgen now treats only single-level plain `char*` pointers as
      string-shaped.
    - `char**`, `const char**`, and related pointer-to-pointer spellings stay
      opaque `ForeignHandle` values instead of string inputs, buffers, or
      returns.
  - shipped slice: `FFI-TOML-004W` malformed dependency section fail-closed
    cleanup:
    - malformed section-header lines starting with `[` mark the currently
      active FFI dependency invalid before resetting parser context.
    - this keeps a broken section line from leaving the previous dependency
      looking valid while following keys are ignored outside any dependency.
  - shipped slice: `FFI-TOML-004X` TOML array/escape compatibility:
    - strict bind TOML string arrays now accept a trailing comma before `]`.
    - strict quoted string parsing now decodes TOML basic-string escapes
      `\b`, `\f`, `\uXXXX`, and `\UXXXXXXXX` while keeping malformed Unicode
      escapes fail-closed.
  - shipped slice: `FFI-TOML-004Y` generated manifest string escaping:
    - compile-side FFI contract JSON string emission now escapes `\b`, `\f`,
      and the remaining C0 control characters as JSON-safe `\u00XX`
      sequences.
    - bindgen generated manifest TOML string emission now uses the same
      control-character escaping for dependency/path/prefix strings.
    - validation:
      - `c3c build --warn-deprecation=no`
      - host targeted `compiler` slice: `pass=264 fail=0`
  - shipped slice: `FFI-TOML-004Z` raw control-byte rejection:
    - strict bind TOML quoted-string parsing rejects unescaped bytes below
      `0x20` instead of allowing raw tabs/carriage returns into dependency
      metadata.
    - invalid control-byte metadata fails before raw, facade, or manifest
      outputs are written.
    - validation:
      - `c3c build --warn-deprecation=no`
      - host targeted `compiler` slice: `pass=269 fail=0`
  - shipped slice: `FFI-TOML-004AA` escaped NUL rejection:
    - strict bind TOML quoted-string parsing rejects `\u0000` and
      `\U00000000` before they can materialize a C NUL and truncate dependency
      metadata.
    - invalid escaped-NUL metadata fails before raw, facade, or manifest
      outputs are written.
    - validation:
      - `c3c build --warn-deprecation=no`
      - host targeted `compiler` slice: `pass=269 fail=0`
  - shipped slice: `FFI-TOML-004AB` `exclude-functions` denylist:
    - `[dependencies.ffi.NAME] exclude-functions = [...]` is parsed as a
      strict string array and recorded in the generated bind manifest.
    - Denied C functions are omitted from generated raw and facade output.
    - Denied functions are skipped before libclang type mapping, so an excluded
      unsupported C signature cannot fail the primary bind pass.
    - Missing excluded names fail closed before output generation instead of
      silently accepting stale configuration.
    - validation:
      - `c3c build --warn-deprecation=no`
      - host targeted `compiler` slice: `pass=276 fail=0`
  - closure note: broader optional FFI lanes stay split in the plan instead of
    keeping this grouped/bindgen umbrella open.
  - follow-up lanes: opaque handle families, structs, callback handles, FFI
    error effects, optional CppInterOp-backed C++ shim generation, and a
    separate MetaFFI-inspired polyglot runtime/plugin lane.

- [x] `LANG-FOREIGN-RUNTIME-CORE-107` define a minimal common foreign runtime core
  for C, Python, Julia, CUDA/cuBLAS, and optional C++/polyglot adapters
  while keeping safety and ownership model boundaries clear
  - design note: `docs/plans/foreign-runtime-core-plan-2026-04-11.md`
    locks a narrow contract for:
    - `ForeignHandle`-centric resource representation,
    - shared runtime/capability descriptors,
    - dictionary+array reflection payloads (`'parameters [..]` canonical in metadata),
    - common conversion/release/invocation shape, and
    - adapter separation from public runtime semantics.
  - shipped slice: `FOREIGN-CORE-001` common handle descriptors and
    `foreign-describe`:
    - `FfiHandle` carries runtime kind, handle kind, and capability bits.
    - C ABI library and pointer-return handles are tagged through the common
      descriptor path.
    - `(foreign-describe handle)` returns a dictionary with quoted-symbol keys;
      `'capabilities` is an array of capability symbols.
  - shipped slice: `FOREIGN-CORE-002A` explicit release:
    - `(foreign-release handle)` closes releasable `ForeignHandle` payloads
      through the same finalizer/free path used by scope teardown.
    - release is idempotent for already-closed handles and rejected for ordinary
      non-releasable library handles.
  - shipped slice: `FOREIGN-CORE-002B` C ABI callable reflection:
    - `(foreign-describe ffi-bound-function)` returns a dictionary with
      `'type 'ForeignCallable`, `'runtime 'c-abi`, `'kind 'function`,
      `'parameters [..]`, `'returns [..]`, and call/reflect capabilities.
    - Parameter/return descriptors expose user-facing `'type` plus ABI
      `'abi-type`; `ForeignHandle` descriptors preserve name, ownership,
      nullability, and finalizer metadata.
  - shipped slice: `FOREIGN-CORE-002C` bindgen descriptor comments:
    - raw bindgen modules now emit `bindgen-descriptor` comments using the
      shared `ForeignCallable` descriptor vocabulary, while `bindgen-policy`
      comments remain explicit review hints and are not a runtime reflection
      source.
  - shipped slice: `FOREIGN-CORE-002D` internal runtime adapter dispatch:
    - `foreign-describe` and `foreign-release` now route through a minimal
      `ForeignRuntimeAdapter` operation boundary.
    - C ABI callable reflection and bound-function calls are registered through
      the C ABI adapter, while generic handle describe/release remains
      available for current runtime kinds until dedicated Python, Julia,
      CUDA/cuBLAS, optional C++ tooling, and polyglot adapters exist.
  - shipped slice: `FOREIGN-CORE-002E` explicit load/resolve adapter hooks:
    - C ABI lazy `dlopen` and `dlsym` now route through explicit `load_bound`
      and `resolve_bound` adapter slots before the C ABI `call_bound` reaches
      libffi.
    - The adapter shape also reserves a `tensor_buffer` slot behind
      `FOREIGN_CAP_TENSOR_BUFFER`; no C ABI Tensor buffer marshalling is
      registered until the BLAS/cuBLAS backend has a concrete ownership and
      layout contract.
  - shipped slice: `FOREIGN-CORE-002F` capability-gated import/member hooks:
    - `FOREIGN_CAP_IMPORT` and `FOREIGN_CAP_MEMBER` guard the new internal
      adapter behavior.
    - `import_module` and `resolve_member` were added as adapter slots, with
      `foreign_runtime_import_module` and `foreign_runtime_resolve_member`
      handling the internal dispatch.
    - No new public user primitive was added, and Python/Julia/CUDA behavior is
      still not wired yet.
  - shipped slice: `FOREIGN-CORE-002G` C ABI string-return cleanup:
    - `^String` FFI returns now copy non-null C `char*` returns into Omni
      `String` values and keep null returns as `nil`.
    - This removes the remaining raw-address integer leak from the string
      return path while preserving the documented `^String` contract.
  - shipped slice: `FOREIGN-CORE-002H` opaque handle name reflection cleanup:
    - `(foreign-describe handle)` now preserves opaque foreign-resource handle
      families such as `File` as symbols under `'name`, matching
      `^{'name File ...}` metadata.
    - C ABI library handles keep their soname/path `'name` as a string because
      that field is the `dlopen` target, not a resource-family symbol.
  - shipped slice: `FOREIGN-CORE-002I` borrowed-handle release regression
    alignment:
    - the borrowed return-handle regression now binds the real C `fopen`
      symbol with borrowed `ForeignHandle` metadata, closes it through
      `fclose`, and verifies `foreign-release` still rejects the
      non-releasable borrowed wrapper.
  - shipped slice: `FOREIGN-CORE-002J` single-release foreign handle authority:
    - mixed internal foreign handles with both a native finalizer and
      `free_lib_handle` now normalize to finalizer-owned release authority at
      construction, preventing finalizer-plus-free double release.
  - shipped slice: `FOREIGN-CORE-002K` manual return ownership preservation:
    - `ForeignHandle` return policies now preserve explicit ownership in
      returned boxes (including `manual`) instead of re-deriving ownership from
      finalizer presence.
    - `(foreign-describe)` preserves manual ownership for returned foreign
      handles.
    - manual-return handles remain non-releasable and are rejected by
      `(foreign-release ...)`.
    - AOT declaration policy rejects manual ownership combined with a
      finalizer, matching the interpreter metadata parser.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
  - shipped slice: `FOREIGN-CORE-002L` AOT finalizer ownership parity:
    - `aot_ffi_policy_from_spec` now rejects return policies that include a
      finalizer unless ownership is `owned`.
    - This closes the gap where borrowed/default finalizer metadata was accepted
      but ignored by finalizer resolution.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=263 fail=0`
    - `git diff --check`
  - shipped slice: `FOREIGN-CORE-002M` finalizer-owned foreign payload release:
    - finalizer-backed handle wrappers now free their own heap payloads after
      resource-specific teardown because finalizer presence makes the finalizer
      path the release authority.
    - covered wrappers include UV timer callback, process, signal, TCP, UDP,
      FS stream, TLS, deduce relation, deduce database, and deduce transaction
      handles.
    - deduce relation scan forced column-key-value OOM handling now fails before
      allocating the relation cache, avoiding the sanitizer-only forced-OOM leak.
  - validation:
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - ASAN targeted `deduce` slice: `pass=330 fail=0`
    - final `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - `scripts/check_status_consistency.sh`
    - `git diff --check`
  - shipped slice: `FOREIGN-CORE-002N` construction-failure ownership:
    - `make_ffi_handle_ex_with_descriptor` releases finalizer/free-backed
      non-library payloads on both box-allocation and value-allocation failures.
    - C ABI library handles remain non-releasable and stay under the existing
      `dlopen` caller cleanup path on constructor failure.
    - owned C FFI pointer returns rely on constructor cleanup when wrapping the
      returned pointer fails instead of running a second caller-side finalizer.
    - TLS client/server wrapper OOM paths use `tls_handle_finalizer` for
      connected initialized handles so graceful close/session logic runs before
      nested TLS storage is freed.
    - allocation-failure tests now pin exactly-once finalizer cleanup across
      FFI handle box-allocation and value-allocation failures.
  - shipped slice: `FOREIGN-CORE-002O` constructor-failure caller cleanup
    alignment:
    - redundant caller-side cleanup after `make_ffi_handle_ex` null returns were
      removed now that constructor failure owns finalizer/free-backed
      non-library payload release.
    - covered async TCP/UDP/process/signal/UV timer callback/FS stream handles,
      scheduler task/thread handles, atomic refs, TLS wrapper error paths,
      deduce database/transaction/relation handles, and boundary helper tests.
    - malformed C ABI library descriptors with `FOREIGN_CAP_RELEASE` now have
      direct advanced FFI coverage proving `foreign-release` still rejects them.
      Opaque malformed handles with a release bit but no finalizer/free release
      authority are also rejected, keep their payload pointer, and are not
      reflected as owned.
    - released handles no longer reflect as owned after payload cleanup clears
      the live pointer and release capability.
    - C ABI/AOT library registration failures close the raw library handle,
      clear the root-scoped value pointer, and release the constructed wrapper
      box so later teardown cannot see a freed box.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=74 fail=0`
    - host targeted `compiler` slice: `pass=263 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - ASAN targeted `deduce` slice: `pass=330 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=216 fail=0`
    - final `c3c build --warn-deprecation=no`
    - `scripts/check_status_consistency.sh`
    - `git diff --check`
  - shipped slice: `FOREIGN-CORE-002P` capability reflection authority
    alignment:
    - `foreign-describe` now omits `'release` from reflected capabilities
      unless the handle has actual release authority, matching the
      `foreign-release` gate for malformed opaque handles.
    - the malformed opaque-handle regression now checks both rejection and
      metadata reflection, so a stray release bit cannot leak into the
      describe surface.
  - shipped slice: `FOREIGN-CORE-002Q` C ABI library capability reflection
    tightening:
    - C ABI library handles now reflect `[load resolve reflect]`, not `'call`,
      because the library handle itself is not directly callable.
    - Bound C ABI functions keep reflecting `'call` through their
      `ForeignCallable` descriptors.
  - shipped slice: `FOREIGN-CORE-002R` process-spawn cleanup failure-injection
    coverage:
    - async process-spawn tests now force `stdin`, `stdout`, and `stderr`
      wrapper allocation failures after process creation.
    - each failure path proves the opened pipes and process state are cleaned
      through the shared foreign-handle cleanup authority.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=75 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=225 fail=0`
  - closure note:
    - the common-core lane is closed; non-C runtime adapters remain split under
      `FOREIGN-CORE-004`, and TLS offload/in-flight lifecycle coverage remains
      a separate runtime validation follow-up.
  - follow-up risk: avoid overloading the core with polyglot host/guest API
    semantics before C ABI path, `ForeignHandle`, and reflection payload contract
    are stable.

- [x] `LANG-TENSOR-BACKEND-BOUNDARY-092` define the optional Tensor
  library/backend boundary
  - source plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
  - closure evidence:
    - `TENSOR-080` is closed as a design/contract slice only; no backend
      runtime code landed.
    - keep BLAS/LAPACK/CUDA/cuBLAS as optional build/runtime acceleration
      behind the pure `Tensor` fallback.
    - use explicit `ForeignHandle` ownership and finalizer policy for native
      library/context handles.
    - do not add public `matmul`, backend-specific tensor types, or implicit
      CPU/GPU transfer semantics.

- [x] `LANG-TENSOR-NATIVE-BLAS-LAPACK-095` close optional native BLAS/LAPACK
  execution backend baseline behind `contract`/`realize`
  - source plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
  - shipped slice: `TENSOR-090A` added optional runtime `cblas_dgemm`
    discovery and a private rank-2 `[1 0]` `Float64` fast path.
  - shipped slice: `TENSOR-090B` extended that private `dgemm` path with
    transpose flags for all contiguous rank-2 single-axis layouts:
    `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`.
  - shipped slice: `TENSOR-090C` added optional runtime `cblas_dgemv`
    discovery and private rank-2/rank-1 plus rank-1/rank-2 `Float64` fast
    paths.
  - shipped slice: `TENSOR-090D` added optional runtime `cblas_ddot`
    discovery and a private rank-1/rank-1 `Float64` vector dot fast path.
  - shipped slice: `TENSOR-090E` added optional runtime `cblas_dger`
    discovery and a private rank-1/rank-1 `Float64` outer-product fast path.
  - closure note:
    - solver/decomposition naming and backend coverage were later resolved
      through the `matrix/*` surfaces and `TENSOR-090` follow-up slices;
    - any remaining native BLAS/LAPACK work should reopen as a concrete
      operation-specific item with focused pure-fallback regressions;
    - do not add public `matmul`, backend-specific Tensor types, or
      `ForeignHandle`-backed ordinary Tensor storage.
  - latest validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - host targeted `advanced-collections-module` group: `pass=298 fail=0`
    - bounded container targeted `advanced-collections-module` group:
      `pass=298 fail=0`
    - direct transpose-backed smokes returned `84.0`, `68.0`, and `123.0`

- [x] `LANG-TENSOR-BROADCASTING-093` decide singleton-axis Tensor
  broadcasting semantics
  - source plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
  - closure evidence:
    - `TENSOR-070` implements right-aligned singleton-axis broadcasting for
      tensor-tensor `map`.
    - rank-0 tensors broadcast as tensor scalars, and incompatible shapes fail
      with `tensor/shape-mismatch`.
    - exact-shape tensor-tensor behavior remains the primary fast path.
    - validation:
      - `c3c build --warn-deprecation=no`
      - host targeted `advanced-collections-module` group: `pass=212 fail=0`

- [x] `LANG-TENSOR-LAZY-EDGE-ROLLBACK-094` add a deterministic regression
  for lazy tensor-expression child edge rollback
  - closure evidence:
    - `src/lisp/value_tensor.c3` now exposes deterministic tensor expression
      edge fail counters for copy-to-parent and escape-promotion paths.
    - `src/lisp/tests_tests.c3` resets those counters between isolated test
      groups.
    - memory-lifetime coverage now fails the third lazy `map` child-edge copy
      after a prior tensor child copy has committed, and verifies the partial
      copied tensor edge is cleaned from the destination scope.
    - root promotion of lazy Tensor expressions now promotes tensor operand
      edges into the destination ESCAPE lane and keeps shared dispatch method
      tables out of Tensor expression ownership graph-audit checks.
    - lazy closure-mapped Tensor promotion and copy-to-parent closure env-scope
      parent chains now detach from released child scopes.
    - commit-scoped graph audit now rejects releasing-child TEMP edges and stops
      at terminal/global env frames.
  - validation:
    - bounded container graph-audit repro for lazy Tensor return: `12.0` with no
      boundary violation output
    - bounded container `memory-lifetime-smoke` slice: `pass=222 fail=0`
    - bounded ASAN container `memory-lifetime-smoke` slice: `pass=222 fail=0`

- [x] `AUDIT-ERROR-PAYLOAD-PROCESS-WAIT-112` skip unconsumed structured error
  payloads and clean process-wait partial result dictionaries
  - closure evidence:
    - `src/lisp/value_constructors.c3` now builds structured raise payloads
      only when a matching `raise` handler can consume them, avoiding
      root-scoped payload dictionaries for ordinary unhandled error returns.
    - `src/lisp/async_process_signal_dns_process.c3` now cleans the
      partially built `process-wait` result hashmap if insertion fails.
    - `src/lisp/value_constructors.c3` now also cleans a partially built
      root-scoped raise payload dictionary if payload key/value construction
      fails mid-build.
    - regression coverage forces a `process-wait` insertion failure and
      verifies root hashmap destructor count returns to the pre-call value.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=215 fail=0`

- [x] `TENSOR-060H` clean nested lazy Tensor materialization child temporaries
  - closure evidence:
    - `src/lisp/prim_tensor.c3` now tracks the concrete child values produced
      while resolving nested lazy Tensor `map`/`contract` operands and cleans
      them after evaluating the parent expression.
    - regression coverage materializes `(map + (map + tensor 1) 2)` and
      verifies only the top-level materialized Tensor remains live until
      caller cleanup.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=212 fail=0`

- [x] `TENSOR-060I` clean failed lazy Tensor materialization result
  - closure evidence:
    - `src/lisp/prim_tensor.c3` now cleans fresh concrete Tensor
      materialization results through the generic fresh-value path rather than
      treating the lazy source graph as expression edges on the concrete
      result.
    - regression coverage verifies nested lazy `contract` materialization and
      failed lazy `map` materialization leave active tensor destructor counts
      unchanged after cleanup.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=215 fail=0`

- [x] `AUDIT-HASHMAP-PROMOTION-ROLLBACK-116` clean checked hashmap promotion
  copies on failed publication
  - closure evidence:
    - `src/lisp/prim_collection_hashmap.c3` now rolls back newly promoted root
      key/value copies from `hashmap_set_checked(...)` and
      `hashmap_set_symbol_checked(...)` when insertion fails before
      publication.
    - regression coverage forces the checked no-slot failure path after child
      promotion and verifies root value destructor counts remain unchanged.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=216 fail=0`

- [x] `TENSOR-060J` stage lazy destination materialization
  - closure evidence:
    - `src/lisp/prim_tensor.c3` now evaluates lazy Tensor expressions into a
      temporary concrete Tensor before copying into an explicit destination, so
      failed `materialize` leaves the caller destination unchanged.
    - the contract destination/source alias rejection now walks lazy expression
      operands before the staged write path, ignores zero-byte tensor storage,
      and failed Tensor constructor data validation cleans the unreturned tensor
      wrapper.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-collections-module` group: `pass=207 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=216 fail=0`

- [x] `AUDIT-DICT-DESCRIPTOR-115` canonicalize `Dict` descriptor printing
  - closure evidence:
    - `src/lisp/value_print_helpers.c3` now prints `Dict` type descriptors as
      the canonical `#<type Dictionary>` while keeping `Dict` as the approved
      shorthand alias.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-unicode-iterator` group: `pass=151 fail=0`

- [x] `FFI-MANIFEST-006` harden compile-side FFI sidecar manifests
  - closure evidence:
    - `src/entry_compile_manifest.c3` now escapes every JSON C0 control byte
      in compile-side `.ffi-manifest.json` string fields.
    - compile-side manifest writes now publish through a sibling temp file and
      final rename, preserving an existing final manifest if a pre-rename
      failure occurs.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=272 fail=0`

- [x] `AUDIT-VALUE-NIL-LITERAL-113` parse `^(Value nil)` as the nil literal
  - closure evidence:
    - `src/lisp/parser_type_annotation_helpers.c3` now treats `nil` inside
      `^(Value ...)` as the NIL literal rather than the symbol named `nil`.
    - runtime dispatch coverage verifies `^(Value nil)` matches `nil` and
      does not match `'nil`.
    - compiler coverage verifies generated AOT metadata emits
      `.val_tag = lisp::ValueTag.NIL` for `^(Value nil)`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-type-dispatch-mutation-chain` group:
      `pass=240 fail=0`
    - host targeted `compiler` slice: `pass=270 fail=0`

- [x] `AUDIT-ASYNC-SIGNAL-HANDLE-OOM-111` clean `signal-handle`
  registered runtime state on wrapper allocation failure
  - closure evidence:
    - `src/lisp/async_process_signal_runtime.c3` now detaches the registered
      signal handle state and frees the `SignalHandle` if the final
      `ForeignHandle` box allocation fails.
    - value-wrapper allocation failure now releases the box, letting the
      signal-handle finalizer detach the native watcher, registry entry, and
      retained callback owner scope exactly once.
    - regression coverage forces `signal-handle` wrapper allocation failure
      and verifies root-scope refcount plus signal registry count return to
      their pre-call values.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=210 fail=0`

- [x] `TENSOR-060G` clean lazy Tensor `ref` materialization temporaries
  - closure evidence:
    - `src/lisp/prim_tensor.c3` now cancels and destroys the concrete
      temporary when `ref` forces a lazy Tensor expression.
    - regression coverage counts active tensor destructors around a lazy
      `map` `ref` and verifies no temporary materialized tensor remains live.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-collections-module` group: `pass=203 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=210 fail=0`

- [x] `AUDIT-TYPE-ANNOTATION-DICT-AOT-110` align `Dict` annotation aliasing
  and AOT dictionary metadata annotation materialization
  - closure evidence:
    - `src/lisp/parser_type_annotation_helpers.c3` now normalizes `Dict` to
      the canonical `Dictionary` type symbol for annotation parsing.
    - `src/lisp/aot_type_spec_helpers.c3` now accepts metadata-dictionary
      annotations with no base type, preserving the metadata payload for
      generated AOT type forms.
    - regression coverage now pins `^Dict` dispatch and AOT materialization of
      dict metadata annotations such as `^{'T Number}`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=267 fail=0`
    - host targeted `advanced-collections-module` group: `pass=203 fail=0`

- [x] `AUDIT-SCHEDULER-TASK-JOIN-TIMER-START-109` close failed
  non-fiber task-join timeout timers exactly once
  - closure evidence:
    - `src/lisp/scheduler_primitives_task_wait_join.c3` now clears the local
      timeout timer pointer after explicit failure-path close, so deferred
      cleanup cannot double-close the libuv handle.
    - `src/lisp/tests_scheduler_groups_more.c3` now pins the timer-start
      failure path through a narrow scheduler fault-injection seam.
  - validation:
    - bounded container `scheduler` slice: `pass=113 fail=0`

- [x] `AUDIT-SCHEDULER-OS-THREAD-ADMIT-SHARED-108` release shared payloads on
  OS-thread work admission failure exits
  - closure evidence:
    - `src/lisp/scheduler_primitives_thread_scaffold.c3` now releases
      `work.shared` before returning scheduler errors for missing handle,
      full thread table, invalid generation lookup, and OS-thread start
      failure exits.
    - `src/lisp/tests_scheduler_groups_more.c3` now pins the direct invalid
      handle path and verifies the shared registry no longer resolves the
      payload after admission failure.
    - validation:
      - bounded container `scheduler` slice: `pass=112 fail=0`

- [x] `LANG-TENSOR-SCIENTIFIC-SURFACE-091` implement the canonical Tensor
  scientific-computing surface
  - design note: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
    locks the proposed surface around `Tensor`, tensor-dispatched `map`,
    `contract`, and `materialize`.
  - shipped slice: `TENSOR-010` runtime representation, destructor, printing,
    type identity, boundary copy/promotion support, and `tensor?`, `dtype`,
    `shape`, `rank`, and `length`.
  - shipped slice: `TENSOR-020` constructor and indexing:
    `(Tensor Float64 shape data-or-scalar)` constructs native Float64 tensors
    from scalar fills or exact-length array/proper-list data, and
    `(ref tensor index-array)` indexes tensors with per-axis negative index
    support.
  - shipped slice: `TENSOR-030` concrete tensor materialization:
    `(materialize tensor)` returns the already-concrete tensor,
    `(materialize tensor out)` copies into a mutable exact-shape/dtype
    destination, and `(materialize scalar out)` fills a destination tensor.
  - shipped slice: `TENSOR-040` elementwise `map`:
    unary tensor, tensor-scalar, scalar-tensor, and exact-shape tensor-tensor
    `Float64` inputs are routed through generic `map` dispatch.
  - shipped slice: `TENSOR-050` pure `Float64` `contract`:
    `(contract a b left-axes right-axes)` supports axis-list contraction,
    rank-0 scalar results, multi-axis contractions, and deterministic
    axis/dimension diagnostics.
  - shipped slice: `TENSOR-060A` destination-fusion audit:
    true `(materialize (map ...) out)` / `(materialize (contract ...) out)`
    fusion requires a lazy tensor-expression value or a macro/special-form
    rewrite because eager evaluation materializes the source before
    `materialize` receives it.
  - shipped slice: `TENSOR-060B-prep` destination-ready kernels:
    the first pure `map` and `contract` paths were split through internal
    helpers that can evaluate into staged concrete tensor storage before
    committing successful destination materialization to caller-provided
    tensors.
  - shipped slice: `TENSOR-060B` lazy tensor-expression protocol:
    Tensor-dispatched `map` and `contract` now return lazy Tensor expression
    payloads under the existing `Tensor` value, and `materialize` can allocate
    a concrete result or stage evaluation before copying into a destination
    tensor after success.
  - shipped slice: `TENSOR-060C` tensor shape/zero-contraction hardening:
    valid non-negative Tensor dimensions no longer trip the platform-size
    overflow guard on 64-bit builds, and zero-size contracted axes produce the
    additive identity instead of reaching divide/modulo-by-zero during
    contraction materialization.
  - shipped slice: `TENSOR-060D` materialize edge hardening:
    rank-0 concrete destination materialization, zero-size destination paths,
    aliased elementwise `map` destination materialization, and duplicate-axis
    detection after negative-axis normalization are now covered.
  - shipped slice: `TENSOR-060E` boundary rollback cleanup:
    tensor expression child values copied or promoted across boundary paths are
    rolled back if a later tensor copy/promotion or wrapper allocation step
    fails; generic materialized-value cleanup also recurses through tensor
    expression edges.
  - shipped slice: `TENSOR-060F` boundary/materialize fail-closed hardening:
    tensor values now use the non-unique destination retry promotion path at
    return boundaries, and concrete `materialize` fast paths validate source
    backing storage before returning or copying malformed tensors.
  - shipped slice: `TENSOR-110` cleanup lane completion:
    - `examples/scicomp_demo.omni` now uses canonical `Tensor`, `map`,
      `contract`, and `materialize` forms; old `vec-*`, `mat-*`, and
      `mat-mul` prototype forms are no longer part of the shipped surface.
    - `map` and `contract` now return lazy Tensor expressions through the
      existing `Tensor` value, and `materialize` controls when concrete
      storage is allocated.
    - regression coverage now includes lazy-map/contract return tests and
      closure-capture behavior.
    - metadata examples intentionally keep `Array` where required (shape,
      axes, and metadata payload transport), so the cleanup does not alter
      metadata-shape conventions.
  - closure note:
    - the pure public Tensor surface was split into separate backend-boundary
      and broadcasting lanes; both are now closed above.

- [x] `AUDIT-IMMER-FFI-COMPAT-101` retire the optional Immer bridge instead
  of extending FFI around it
  - closure evidence:
    - the owner clarified that C++/Immer support was not intended as core
      language infrastructure.
    - deleted the unsupported `lib/immer.omni` wrapper and the `lib/immer/`
      C++ bridge tree, including the tracked nested `lib/immer/immer` gitlink.
    - deleted the obsolete `docs/plans/immer-ffi-compat-plan-2026-04-11.md`
      compatibility plan.
    - no `^Value`, automatic value-handle, or pointer-only rewrite was added
      for this legacy optional library.
  - validation:
    - active source/reference search confirms no supported surface references
      remain outside historical TODO/changelog/plans.
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- [x] `AUDIT-LET-BRACKET-SHORTHAND-102` remove legacy outer `let [...]`
  shorthand from live library code
  - closure evidence:
    - `lib/core.omni` macro expansions now emit flat-pair `let` and named
      `let` binding lists.
    - `lib/test-utils.omni` now uses flat-pair `let` binding syntax.
    - the remaining `let [` text matches are only the syntax decision note and
      the negative parser regression that verifies the shorthand is rejected.
  - validation:
    - `rg "\(let\s*\[" -n lib stdlib tests examples docs src` returns only
      the syntax decision note and the negative parser regression.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/test-utils.omni`
    - bounded `basic` slice: `pass=142 fail=0`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- [x] `AUDIT-IMMER-PERSISTENT-DISPATCH-088` harden persistent collection
  wrappers before adding generic dispatch
  - closure evidence:
    - `persistent-dictionary` now rejects odd key/value argument lists instead
      of silently dropping the final key.
    - persistent collection values are now tagged Omni wrapper dictionaries
      around the raw bridge handle, and public operations unwrap by expected
      family before calling the C++ bridge.
    - `persistent-array?`, `persistent-dictionary?`, and `persistent-set?`
      predicates now exist, while generic `count`/`conj`/`into` dispatch
      remains intentionally frozen.
    - residual runtime bridge compatibility is split into
      `AUDIT-IMMER-FFI-COMPAT-101`.
    - superseded by `AUDIT-IMMER-FFI-COMPAT-101`, which retires the optional
      Immer bridge entirely instead of adding FFI machinery around it.
  - historical validation before the bridge was retired:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/immer.omni`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - runtime preload remains blocked by `AUDIT-IMMER-FFI-COMPAT-101`
      because `ffi-declare` is no longer bound.
    - bridge build remains blocked locally: `make -C lib/immer test_bridge`
      fails on missing `immer/flex_vector.hpp`.

- [x] `AUDIT-LIST-HELPER-ALIAS-086` keep lowercase `list` as an approved public
  helper
  - closure evidence:
    - `List` remains the canonical constructor/conversion surface.
    - lowercase `list` is explicitly approved as an idiomatic Lisp
      list-builder/conversion helper, not a new canonical constructor family.
    - runtime primitive registration and compiler primitive hash coverage
      already route `List` and `list` through the same implementation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- [x] `AUDIT-PROCESS-WRAPPER-PAYLOAD-FALLBACK-100` preserve process
  runtime payload errors through stdlib wrappers
  - closure evidence:
    - `process-spawn` and `process-kill` now have untyped fallbacks after their
      typed wrapper methods, matching the `tcp-*`, `offload`, and
      `thread-spawn` pattern.
    - invalid command and signal arguments now reach the runtime
      `io/process-*` payload contracts instead of being intercepted by generic
      typed dispatch.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=67 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-FILESYSTEM-SURFACE-087` canonicalize filesystem wrappers on
  `fs-*`
  - closure evidence:
    - stdlib no longer exports `filesystem-*` compatibility aliases.
    - runtime, compiler primitive hash entries, docs, and tests now agree on
      the canonical `fs-*` wrapper family.
    - regression coverage verifies representative long-form filesystem aliases
      are unbound.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=65 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-COMPILER-PRIMITIVE-HASH-COVERAGE-099` complete compiler
  primitive hash coverage for public runtime primitives
  - closure evidence:
    - public runtime primitive value references now have compiler primitive hash
      entries, reducing the public-ish missing-hash audit set to zero.
    - the primitive hash table was resized from 256 to 512 to preserve a safe
      load factor after expanding coverage to 195 entries.
    - regression coverage now verifies `sin`, `string-byte-length`, and
      `sort-by` are looked up as primitives, not captured as C3 locals.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - audit script: `hash entries=195`, `focus missing public-ish hash entries=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-COMPILER-PRIMITIVE-CLASSIFICATION-098` consolidate compiler
  primitive classification around the primitive hash table
  - closure evidence:
    - free-variable and delegation primitive checks now use one shared
      hash-backed classifier instead of duplicated hardcoded name arrays.
    - legacy non-hash exceptions for `ForeignHandle` and `__ui-ftxui-run`
      remain excluded from closure capture without adding value-position
      lowering.
    - regression coverage now verifies hash-only primitives such as `Dict` and
      `json-parse` are looked up as primitives, not captured as C3 locals.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-FFI-FOREIGN-HANDLE-SURFACE-103` replace the public FFI pointer
  annotation surface with `^ForeignHandle`
  - closure evidence:
    - `docs/LANGUAGE_SPEC.md` now documents `^ForeignHandle` as the opaque
      foreign-handle annotation and only accepts live `FFI_HANDLE` values or
      `nil`.
    - `docs/reference/09-concurrency-ffi.md` now states that opaque foreign C
      values use `^ForeignHandle` rather than raw integer addresses.
    - `docs/PROJECT_TOOLING.md` now maps non-string opaque foreign bindings to
      `^ForeignHandle` in bindgen output.
    - runtime FFI call packing now rejects raw integer addresses for
      foreign-handle arguments and expects `nil` or live `FFI_HANDLE` values.
    - non-null foreign-handle returns now produce non-owning `FFI_HANDLE` values
      instead of raw address integers.
    - the audit/remediation and AOT runtime/linking notes now use
      `ForeignHandle` instead of the stale `Pointer` exception wording where
      current-state docs describe the live surface.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=43 fail=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - `git diff --check`

- [x] `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105` carry `ForeignHandle`
  metadata policy through the AOT runtime bridge
  - closure evidence:
    - AOT generated declarations now carry `ForeignHandle` policy descriptors
      into `aot::ffi_declare_fn`.
    - Parameter policy descriptors preserve handle family and nullability.
    - Return policy descriptors preserve handle name, ownership, and finalizer
      symbol, and the AOT bridge resolves owned-return finalizers before
      installing the bound primitive.
    - AOT lowering rejects owned `ForeignHandle` parameter policy fail-closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=209 fail=0`
    - `git diff --check`

- [x] `AUDIT-FFI-FOREIGN-HANDLE-METADATA-104` implement the interpreter/JIT
  ForeignHandle metadata dictionary policy
  - closure evidence:
    - `^ForeignHandle` remains the simple default foreign-handle annotation.
    - FFI-local metadata dictionaries now accept the refinements:
      `^{'name File 'ownership owned 'finalizer fclose}` implies
      `ForeignHandle`, and the explicit `^{'type ForeignHandle ...}` form is
      also accepted.
    - runtime FFI now enforces handle family and non-null policy at call
      packing, resolves owned return finalizers with `dlsym`, and wraps pointer
      returns in named `FFI_HANDLE` boxes.
    - FFI AST serialization now preserves metadata dictionary annotations for
      compiler roundtrip coverage.
    - AOT policy propagation was split into
      `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105` and is now closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=204 fail=0`

- [x] `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096` fix generated
  e2e C3 primitive capture name sanitization
  - closure evidence:
    - shared compiler symbol-value emission now routes primitive references and
      closure capture initializers through the same primitive-cache path.
    - runtime primitives exposed by the e2e corpus now have compiler primitive,
      free-variable, and hash-table coverage: `error`, `error?`,
      `error-message`, `is?`, `instance?`, and `type-args`.
    - AOT `Coroutine` now accepts `aot::make_closure` wrapper thunks without
      inspecting JIT-only closure payload fields.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct `Coroutine` type probe returns `Coroutine`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-NUMBER-PARSE-SURFACE-085` canonicalize permissive numeric parsing
  as `parse-number`
  - decision note: `docs/plans/number-parse-surface-decision-2026-04-11.md`
    selects `parse-number` and keeps `Number` non-callable.
  - closure evidence:
    - public runtime/compiler primitive surfaces now register `parse-number`
      instead of `string->number`.
    - live tests, examples, and docs now use `parse-number`.
    - `Number` remains an abstract/meta type descriptor for annotation and
      dispatch, not a value-position constructor.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `parse-number` int/double/nil results, removed
      `string->number` binding, live `parse-number` binding, and non-callable
      `Number`
    - bounded `advanced-macro-hygiene-string-number` subgroup: `pass=9 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `compiler` slice: `pass=196 fail=0`
    - `./build/main --check examples/deduce_crud_server.omni`
    - `./build/main --check examples/finwatch/smoke_test.omni`
    - broader Docker `scripts/run_e2e.sh` reached generated-source parity but
      failed the generated C3 build on primitive capture names `error` /
      `error?`; subsequently closed as
      `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096`.

- [x] `AUDIT-LIST-STRING-CONSTRUCTOR-SURFACE-084` canonicalize list/string
  conversion through constructors
  - decision note: `docs/plans/list-string-constructor-decision-2026-04-11.md`
    selects `List(String)` and `String(List)` as the canonical public surface.
  - closure evidence:
    - `List(String)` now reuses the UTF-8 codepoint splitter and returns a
      proper list of one-codepoint strings.
    - `String(List)` now concatenates proper lists of string fragments and
      treats `nil` as the empty list / empty string.
    - public `string->list` and `list->string` primitive/compiler aliases were
      removed; internal C helpers remain for runtime implementation and memory
      regression tests.
    - docs and Lisp-level tests now use constructor forms.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `List(String)`, `String(List)`, `String(nil)`,
      non-string list element rejection, and removed public arrow bindings
    - bounded `advanced-unicode-iterator` subgroup: `pass=138 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `limit-busting` slice: `pass=17 fail=0`
    - bounded `compiler` slice: `pass=196 fail=0`

- [x] `AUDIT-STRING-GENERIC-BYTE-CODEPOINT-094` decide byte versus codepoint
  semantics for generic string `length` and `ref`
  - closure evidence:
    - selected codepoint/character semantics for generic string sequence
      operations to match `string-length`, `char-at`, and `List(String)`.
    - kept byte count explicit through `string-byte-length`.
    - changed generic `ref` and postfix `.[index]` on strings to return
      single-character strings instead of byte integers.
    - added non-ASCII regressions for generic `length`, `ref`, and postfix
      indexing.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `length`, `string-length`, `string-byte-length`, `ref`,
      `char-at`, and postfix `.[index]` on non-ASCII strings
    - bounded `advanced-unicode-iterator` subgroup: `pass=136 fail=0`
    - bounded `advanced-collections-module` subgroup: `pass=142 fail=0`

- [x] `AUDIT-CONS-REF-SPEC-PARITY-095` reconcile cons/list `ref` behavior with
  the language spec
  - closure evidence:
    - kept the existing tested runtime behavior as the contract rather than
      narrowing `ref` on cons cells back to pair-only `0`/`1` access.
    - updated `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`, and
      `docs/reference/11-appendix-primitives.md` to document cons/list chain
      indexing, negative indexes, and dotted terminal tail length/indexing.
  - validation:
    - bounded `advanced-collections-module` subgroup: `pass=139 fail=0`

- [x] `AUDIT-LIST-PREDICATE-CONTRACT-093` reconcile `list?` proper-list
  semantics with the stdlib implementation
  - closure evidence:
    - registered the existing strict `prim_is_list` primitive as public `list?`.
    - removed the stdlib override that treated every pair as a list.
    - added regressions proving `(list? (cons 1 2))` is false.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `advanced-type-dispatch-mutation-chain` subgroup:
      `pass=237 fail=0`

- [x] `AUDIT-LIST-WALKER-IMPROPER-LIST-092` normalize improper-list handling
  across public list walkers
  - closure evidence:
    - added a private proper-list guard and applied it across the public stdlib
      list walkers covered by the audit: `map`, `filter`, `foldl`, `foldr`,
      `append`, `take`, `drop`, `zip`, `for-each`, `any?`, `every?`,
      `flatten`, `partition`, `remove` via `filter`, `find`, and `nth`.
    - mirrored the guard in the compiler stdlib prelude for the overlapping
      walker definitions.
    - guarded nested list traversal in `flatten` so improper inner lists no
      longer fall through to low-level `car`/`cdr` failures.
    - left pair/iterator protocols such as `stream-take` and iterator `drop`
      outside this proper-list-only lane.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup after partial-success slice:
      `pass=275 fail=0`
    - bounded `compiler` slice after partial-success slice: `pass=196 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup after remaining walker slice:
      `pass=285 fail=0`
    - bounded `compiler` slice after remaining walker slice: `pass=196 fail=0`

- [x] `AUDIT-EVAL-VALUE-TO-EXPR-FAIL-CLOSED-096` harden the runtime `eval`
  data-to-expression conversion path
  - closure evidence:
    - value-to-expression conversion now enforces structural arity for malformed
      special-form data instead of defaulting missing operands to `nil` or
      ignoring extras.
    - `define`, two-operand `set!`, `capture`, and `signal` now reject
      non-symbol names/tags instead of coercing them to symbol id `0`.
    - multi-argument `set!` data forms now lower through normal call dispatch,
      preserving parser-equivalent generic collection setter behavior.
    - lambda and let data forms now preserve parser-equivalent implicit block
      bodies, and `macroexpand` now surfaces structural conversion errors for
      malformed cons forms.
    - added eval and macroexpand regressions in the advanced stdlib numeric
      introspection group.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup after arity/name slice:
      `pass=265 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup after block/macroexpand slice:
      `pass=268 fail=0`

- [x] `AUDIT-NAMED-LET-INIT-ERROR-PROPAGATION-097` preserve initializer
  error values through named `let`/tail multi-argument calls
  - closure evidence:
    - `make_cons` now treats successfully ESCAPE-promoted `ERROR` values as
      first-class cons elements instead of always interpreting an `ERROR` tag
      result as promotion failure.
    - `append` now checks the intermediate `(reverse a)` value before entering
      its named-let loop, preserving the original proper-list error for
      improper left input.
    - added core regressions for tail multi-argument calls that ignore or
      return an error-valued argument, plus the append improper-left regression.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `c3c build --sanitize=address --warn-deprecation=no`: green
    - direct tail multi-arg/append probes: green
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup: `pass=256 fail=0`
    - bounded `tco-recycling` slice: `pass=11 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- [x] `AUDIT-PIKA-REGEX-STRESS-CACHE-090` fix repeatable bounded `pika` regex
  stress/cache failures
  - closure evidence:
    - direct Pika large-pattern controls passed, while named-let
      multi-argument recursion with string parameters reproduced the same
      `arg list too short` failure without invoking regex internals.
    - `make_cons` now permits identity promotion only for values already owned
      by the current target scope chain or ESCAPE lane, and still rejects
      identity returns for current TEMP values that failed to promote.
    - added a memory-lifetime regression covering named-let string argument-list
      promotion under the TCO/ESCAPE-lane path.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `pika` slice: `pass=83 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- [x] `AUDIT-HTTP-CRUD-DICT-PROMOTION-089` fix repeatable bounded `http` slice
  failures after constructor cleanup
  - closure evidence:
    - `pipeline/decode-request` now uses line-based request splitting so LF-only
      and CRLF requests do not treat header lines as JSON bodies.
    - duplicate-post payload construction now uses `ref` for optional response
      error fields, so success responses do not inject error values into a
      dictionary literal.
    - `prim_dict` now propagates error-valued literal key/value arguments before
      insertion instead of reporting a misleading backing-storage OOM.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `http` slice: `pass=29 fail=0`

- [x] `AUDIT-CONSTRUCTOR-DISPATCH-SURFACE-083` remove high-confidence
  noncanonical constructor/conversion aliases and vector terminology
  - closure evidence:
    - `number->string`, `symbol->string`, `string->symbol`,
      `exact->inexact`, and `inexact->exact` are no longer public primitive
      registrations; `String`, `Symbol`, `Float64`, and `Integer` are the
      canonical constructor/coercion surfaces.
    - `set-size` and `set->list` are no longer public primitive
      registrations; `length` handles set cardinality and `List(Set ...)`
      materializes deterministic canonical set element order.
    - fast-dev-only `Int`, `Bool`, and duplicate `filesystem-*` primitive
      aliases were removed.
    - schema validation now uses `array-of` instead of `vector-of`.
    - `lib/immer.omni` now exposes `persistent-array`,
      `persistent-dictionary`, and `persistent-set` names instead of
      `vector`, `hash-map`, and `hash-set`.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `string-type` slice: `pass=40 fail=0`
    - bounded `advanced` slice: `pass=1189 fail=0`
    - bounded `compiler` slice: `pass=194 fail=0`
    - quick eval confirmed removed set aliases, `length`/`List(Set ...)`, and
      `array-of` schema behavior.

- [x] `AUDIT-RUNTIME-INTERN-RAISEPAYLOAD-GUARDS-081` guard runtime intern and
  unhandled-effect raise payload paths
  - closure evidence:
    - `src/lisp/eval_init_primitive_registration.c3` now rejects failed `nil`
      symbol interning before defining the constant.
    - `src/lisp/jit_jit_closure_runtime.c3` now treats failed promise env-tag
      interning as a non-match.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now uses
      non-raising dictionary allocation for unhandled-effect payloads and
      checks payload-key interning before publication.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal `jit-policy`: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`
    - bounded normal `advanced`: `pass=1185 fail=0`
    - bounded ASAN `advanced`: `pass=1172 fail=0`

- [x] `AUDIT-RUNTIME-RESULT-KEY-INTERN-GUARDS-080` guard runtime result,
  lookup, and optional diagnostic payload symbols before dictionary use
  - closure evidence:
    - `src/lisp/eval_dispatch_error_payloads.c3` now rejects
      `INVALID_SYMBOL_ID`, null, `ERROR`, and invalid-symbol payload values in
      the shared optional dispatch payload setter.
    - `src/lisp/async_process_spawn.c3` now interns process-spawn result keys
      before constructing key symbols and closes spawned resources on key
      interning failure.
    - `src/lisp/http_url_response.c3` now rejects failed response-field key
      interning before publishing HTTP response payload dictionaries.
    - `src/lisp/prim_ui_ftxui_helpers.c3` now rejects failed lookup-key
      interning before probing UI dictionaries.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `async` slice with FTXUI smoke enabled:
      `pass=65 fail=0`
    - bounded normal `jit-policy`: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- [x] `AUDIT-DEDUCE-GOAL-DIRECTED-INTERN-GUARDS-079` guard
  goal-directed deduce explain/why-result symbols before payload publication
  or dictionary lookup
  - closure evidence:
    - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3` now
      routes goal-directed blocker and shape symbols through a helper that
      rejects `INVALID_SYMBOL_ID` with the existing explain OOM error instead
      of constructing invalid `SYMBOL` payload values.
    - `src/lisp/deduce_rule_ops_explain_snapshot.c3` now uses that helper for
      goal-directed shape and execution-path payload values.
    - `src/lisp/deduce_why_result_path_payload.c3` now rejects invalid lookup
      key interning before constructing the temporary dictionary key symbol.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `deduce` slice: `pass=330 fail=0`

- [x] `AUDIT-DEDUCE-GOAL-DIRECTED-PAYLOAD-NO-PRESEED-078` make
  goal-directed deduce diagnostic payload allocation non-raising before
  fallback deduce OOM publication
  - closure evidence:
    - `src/lisp/deduce_rule_eval_analyze_setup.c3` now uses
      `make_hashmap_no_raise(...)` for goal-directed selector analysis error
      payload dictionaries instead of the raising dictionary constructor path.
    - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
      now uses the same non-raising dictionary path for selector and relation
      surface diagnostic payload dictionaries.
    - Both files route payload field insertion through local no-raise setters
      before publishing the existing deduce out-of-memory fallback error.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=200 fail=0`

- [x] `AUDIT-DEDUCE-INTEGRITY-PAYLOAD-NO-PRESEED-077` make optional deduce
  diagnostic payload allocation non-raising under active handlers
  - closure evidence:
    - `src/lisp/deduce_relation_ops_validation_payload.c3` now uses
      `make_hashmap_no_raise(...)` for integrity/check-context diagnostic
      payload dictionaries instead of the raising dictionary constructor path.
    - `src/lisp/deduce_rule_eval_exec_component_state.c3` now uses the same
      non-raising dictionary helper for iteration-limit diagnostic payloads
      that are returned as payload-or-null before the later iteration-limit
      raise.
    - Integrity payload dictionary field insertion now routes through
      no-raise local setters that return `null` from the optional payload
      builder on allocation, interning, or promotion failure.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now installs a
      raise handler, forces deduce integrity and iteration-limit payload
      allocation failure, and verifies `raise_pending`, `raise_payload`, and
      `raise_msg_len` remain clear.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=200 fail=0`

- [x] `AUDIT-CTOR-PAYLOAD-NO-PRESEED-076` make optional constructor mismatch
  diagnostic payload allocation non-raising under active handlers
  - closure evidence:
    - `src/lisp/primitives_meta_types_ctor_helpers.c3` now uses
      `make_hashmap_no_raise(...)` for `ctor_mismatch_data(...)` instead of the
      raising dictionary constructor path.
    - Constructor mismatch payload keys are checked for `INVALID_SYMBOL_ID`
      before constructing payload key symbols.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now installs a
      raise handler, forces constructor mismatch payload allocation failure, and
      verifies `raise_pending`, `raise_payload`, and `raise_msg_len` remain
      clear.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=199 fail=0`

- [x] `AUDIT-DISPATCH-PAYLOAD-NO-PRESEED-075` make optional dispatch
  diagnostic payload allocation non-raising under active handlers
  - closure evidence:
    - `src/lisp/prim_collection_hashmap.c3` now exposes
      `make_hashmap_no_raise(...)` for callers that need optional dictionary
      payload storage without publishing a runtime raise on allocation failure.
    - `src/lisp/value_constructors.c3` and
      `src/lisp/eval_dispatch_error_payloads.c3` use the non-raising helper
      for handled raise payload and dispatch diagnostic payload construction.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3` now installs a
      raise handler, forces dispatch payload dictionary allocation failure, and
      verifies `raise_pending`, `raise_payload`, and `raise_msg_len` remain
      clear.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal `jit-policy` with FTXUI smoke enabled: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- [x] `AUDIT-JIT-MUTABLE-LOCAL-NULL-BOX-073` make JIT mutable-local helper
  paths fail closed when a root-box env allocation is missing
  - closure evidence:
    - `src/lisp/jit_jit_apply_multi_prims_tail.c3` now makes
      `jit_env_lookup_local(...)` return an explicit
      `jit: missing mutable local binding` error when the helper receives a
      null env or cannot find the requested binding.
    - `jit_env_reparent(...)` now returns the effective env and treats a null
      source env as a no-op reparent to the requested parent, so compiled env
      capture does not reload a known-null helper result.
    - `src/lisp/jit_jit_compile_expr_basic.c3` and
      `src/lisp/jit_jit_emit_helpers.c3` now use the checked helper contracts.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3` covers both helper
      contracts directly in the `jit-policy` slice.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal `jit-policy` with FTXUI smoke enabled: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- [x] `AUDIT-RAISE-PAYLOAD-NESTED-PENDING-074` make handled raise payload
  construction avoid publishing stale nested pending-raise state on allocation
  failure
  - closure evidence:
    - `src/lisp/value_constructors.c3` now builds raise payload dictionaries
      through `make_hashmap_no_raise(...)` instead of the raising
      `make_hashmap(...)` constructor path.
    - `raise_error_pending_impl(...)` receives the intended
      payload-construction failure without a pre-existing `raise_pending` side
      effect from nested dictionary construction.
    - The existing `pending-raise-payload-alloc-failure` `jit-policy`
      regression now passes in the full bounded slice.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal `jit-policy` with FTXUI smoke enabled: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- [x] `AUDIT-TCO-ENV-COPY-FAIL-CLOSED-071` make TCO env-chain copy reject
  boundary-copy failures before binding copied env frames
  - closure evidence:
    - `src/lisp/jit_jit_eval_scope_copy.c3` now uses checked boundary-copy
      results for TCO env-frame binding copy and aborts the copied frame when
      copy returns a fault, null value, or `ERROR`.
    - Parent rewrites for root-persistent env boxes now also fail closed when a
      required parent-chain copy fails.
    - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3` now covers a
      forced opaque-primitive boundary-copy failure and verifies the source env
      binding remains intact while the copied env is rejected.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=198 fail=0`

- [x] `AUDIT-TCO-RECYCLE-HOOK-FAIL-CLOSED-072` make TCO recycle preparation
  preserve the previous env and return explicit error on env-copy failure
  - closure evidence:
    - `src/lisp/runtime_backend_hooks.c3` now keeps `*env_io` unchanged when
      TCO env-chain copy fails, releases the fresh recycle scope, restores the
      prior call scope, and returns an explicit `ERROR`.
    - Active defer retargeting is restored back to the original call scope when
      env-copy fails after fresh-scope retargeting.
    - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3` now verifies the
      recycle hook returns `jit: failed to copy TCO recycle env` while
      preserving env and recycle-scope state.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=198 fail=0`

- [x] `AUDIT-JIT-MODULE-VALUE-GROWTH-069` make first-class module values
  survive module table growth
  - closure evidence:
    - `src/lisp/value_predicates_accessors_basic.c3` now snapshots the module
      descriptor into root-scope storage for first-class `MODULE` values
      instead of storing an address into the reallocating interpreter module
      table.
    - `src/lisp/eval_path.c3` now fails closed for invalid module descriptors
      before reading exports or env bindings.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now forces
      module table growth after creating a first-class module value and then
      verifies exported path access remains valid under normal and ASAN smoke.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=196 fail=0`

- [x] `AUDIT-JIT-MUTATION-PROMOTION-NULL-GUARDS-070` harden mutation/define
  paths against null promotion results
  - closure evidence:
    - `src/lisp/jit_jit_closure_let_set_helpers.c3` now treats null cons-field
      promotion and null instance-field boundary-copy results as errors before
      mutating storage.
    - `src/lisp/jit_jit_define_method_table.c3` now rejects null typed method
      implementations and null global define promotion results before
      appending method table entries or replacing fallbacks.
    - `src/lisp/aot_type_definitions.c3` now rejects null AOT typed-method
      promotion before calling into method-table publication.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now covers the
      JIT instance-field boundary-copy fault path and verifies the old field
      remains intact.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=196 fail=0`

- [x] `AUDIT-BOUNDARY-INSTANCE-MODULE-ALIAS-GRAPH-068` align boundary
  alias-reuse traversal with graph-bearing `INSTANCE` / `MODULE` wrappers
  - closure evidence:
    - `src/lisp/eval_boundary_provenance.c3` now treats `INSTANCE` and
      `MODULE` as graph-bearing alias payloads, matching the committed-root
      graph audit edge table.
    - The rare `INSTANCE` / `MODULE` path uses a heap-backed reachability scan
      instead of adding more large local arrays to the alias walker frame, so
      the FTXUI/effect stack budget remains protected.
    - The scan checks value and environment reachability into the releasing
      scope, including by-value instance fields whose stored `scope_gen` or
      nested graph still points back to the releasing scope.
    - Root-persistent env boxes are still traversed for parent/binding edges;
      they are only excluded from direct temp-frame ownership checks.
    - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` now includes
      a regression that forces an instance field graph to retain a releasing
      payload and verifies alias reuse rejects it.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=194 fail=0`

- [x] `AUDIT-BOUNDARY-DESTINATION-BUILD-SPLICE-FACADE-067` keep destination
  build-scope splice callsites policy-clean without changing commit semantics
  - closure evidence:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` no longer
      calls `main::scope_splice_escapes(...)` directly.
    - `src/lisp/eval_boundary_commit_escape_builders.c3` now owns the narrow
      allowlisted `boundary_destination_build_scope_splice(...)` shim, keeping
      the low-level splice within the existing boundary implementation file
      that the facade policy permits.
    - Destination `cons`, `partial`, `iterator`, and `error` builders retain
      the previous build-scope commit behavior, avoiding a source-wrapper
      fallback that breaks nested effect payload return boundaries.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=193 fail=0`

- [x] `AUDIT-BOUNDARY-ALIAS-WORKLIST-PERF-065` replace boundary alias linear
  visited tracking with a bounded hash/set helper
  - closure evidence:
    - `src/lisp/eval_boundary_provenance.c3` now keeps the authoritative linear
      `seen` list for no-false-negative correctness, but fronts it with a small
      bounded `ushort` index-table accelerator for common repeated composite
      alias checks.
    - Scalar/non-graph roots now return before entering the large traversal
      frame, and the large-array walker sits behind a small stack-headroom
      wrapper that fails closed to copy-required if the current stack context is
      too shallow.
    - The accelerator is deliberately small to stay under the FTXUI/effect
      resolve stack budget; it saturates into the existing linear scan rather
      than dropping entries or failing open.
    - The smoke lane now includes a shared composite cycle payload regression
      alongside the wide scalar payload and nested effect payload regressions.
    - Larger local pointer/index-table attempts regressed FTXUI smoke with a
      `smoke.omni` boundary resolve stack overflow and were not kept.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=193 fail=0`

- [x] `AUDIT-FTXUI-C-ABI-EXCEPTION-SAFETY-063` wrap exported FTXUI C ABI
  entrypoints in fail-closed exception guards
  - closure evidence:
    - `csrc/ftxui_shim.cpp` now routes status-returning backend work through
      a shared `status_try(...)` guard that maps `std::bad_alloc` to
      `OMNI_FTXUI_STATUS_OUT_OF_MEMORY` and other C++ exceptions to
      `OMNI_FTXUI_STATUS_INTERNAL_ERROR`.
    - Argument and ABI validation remains outside the guard where it was
      already fail-closed, preserving existing validation behavior.
    - Deferred graph/render/event/quit-key callback adapters catch callback
      exceptions locally and return safe fallback values rather than allowing
      callback exceptions to escape through FTXUI render/event frames.
    - A coverage check over `omni_ftxui_status` exports found no unguarded
      status-returning entrypoint except the trivial last-error accessor.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green

- [x] `AUDIT-FTXUI-SCREEN-LIFETIME-064` make quit-key wrapper lifetime explicit
  - closure evidence:
    - `csrc/ftxui_shim.cpp` now stores the underlying `ScreenInteractive` with
      shared ownership.
    - `omni_ftxui_component_wrap_quit_keys(...)` now captures the shared screen
      object instead of the raw screen handle and retains it in the wrapped
      component keep-alive list.
    - `csrc/ftxui_shim.h` and `docs/plans/ftxui-c-abi-shim.md` document that
      the quit wrapper retains the screen's underlying loop object until the
      wrapped component is destroyed.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green

- [x] `AUDIT-OFFLOAD-WIDTH-GUARDS-066` harden offload descriptor/session/temp-id
  narrowing edges
  - closure evidence:
    - `src/lisp/scheduler_offload_network.c3` now rejects listener file
      descriptors that cannot fit in the `int` `tcp_accept_fd(...)` API before
      narrowing.
    - `src/lisp/eval_repl_server_state.c3` now formats REPL session IDs from a
      guarded `long` value instead of truncating `next_session_id` through
      `int`.
    - `src/lisp/scheduler_offload_ops.c3` now formats the full guarded
      `unique_id` lane for atomic temp-path suffixes instead of truncating it to
      `uint`.
    - `src/lisp/scheduler_state_support_types.c3` now has a compile-time guard
      for the current `OffloadWork` pointer-through-`long` payload contract.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `async`: green (`pass=65 fail=0`)

- [x] `AUDIT-AOT-MODULE-DEDUCE-COLLECTION-SENTINELS-058` harden AOT/env
  sentinel handling, module load rollback, deduce restore failure propagation,
  and nullable collection backing guards
  - closure evidence:
    - `src/lisp/aot_runtime_bridge_closure.c3`,
      `src/lisp/aot_runtime_bridge_helpers.c3`,
      `src/lisp/aot_runtime_bridge_ffi.c3`, and
      `src/lisp/aot_runtime_bridge_ffi_lib.c3` now route AOT bridge symbol
      binding and lookup through checked intern helpers instead of allowing
      `INVALID_SYMBOL_ID` to reach environment lookup/define/set paths.
    - `src/lisp/value_environment_storage.c3`,
      `src/lisp/value_environment.c3`, and
      `src/lisp/value_environment_barrier.c3` now reject invalid symbol IDs
      before hash probing or binding mutation.
    - `src/lisp/jit_jit_module_setup_helpers.c3`,
      `src/lisp/jit_jit_module_import_setup.c3`,
      `src/lisp/macros_expansion.c3`, and
      `src/lisp/value_interp_lifecycle.c3` now roll back newly published
      modules on body/path/top-level load failure, rebuild module hash state,
      skip tombstones during hash rebuild, and avoid stale module pointers when
      nested loads grow the module table.
    - `src/lisp/deduce_db_storage_open.c3`,
      `src/lisp/deduce_db_rule_catalog_persistence.c3`,
      `src/lisp/deduce_db_rule_signature_restore.c3`, and
      `src/lisp/deduce_db_handles_storage.c3` now distinguish missing DBIs
      from other LMDB open failures, reject invalid interned restored symbols,
      roll back partially restored rule signatures/schemas, and fail closed on
      relation metadata policy intern failure.
    - `src/lisp/eval_promotion_root_clones.c3` now checks method-table clone
      entry allocation size before `sizeof * capacity`.
    - `src/lisp/value_constructors.c3`,
      `src/lisp/prim_collection_sort_array.c3`,
      `src/lisp/prim_collection_hashmap.c3`, and
      `src/lisp/prim_collection_generic_set.c3` now reject invalid raise
      payload symbols and nullable array/dict/set backing storage before
      dereference.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded advanced slice: `pass=1183 fail=0`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded deduce slice: `pass=330 fail=0`
      - bounded string-type slice: `pass=40 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-NUMERIC-MACRO-ASYNC-TAIL-GUARDS-057` harden numeric overflow,
  macro/parser symbol allocation, data-format option tails, and async cleanup
  paths
  - closure evidence:
    - `src/lisp/prim_math.c3`, `src/lisp/prim_math_core.c3`, and
      `src/lisp/prim_math_arithmetic.c3` now fail closed on `getrandom`
      failure/partial reads and on signed `long.min` / integer overflow edges
      in arithmetic, `abs`, `gcd`, and `lcm`.
    - `src/lisp/prim_string_format_directives.c3` now rejects overflowing
      format width/precision specifiers and `%b` no longer negates
      `long.min`.
    - `src/lisp/prim_string_ops.c3`,
      `src/lisp/prim_string_transform.c3`,
      `src/lisp/primitives_data_formats_csv.c3`, and
      `src/lisp/primitives_data_formats_toml_options.c3` now preserve
      multibyte `list->string` elements and reject improper option/list tails.
    - `src/lisp/prim_string_convert.c3`,
      `src/lisp/macros_template_expansion.c3`,
      `src/lisp/macros_expr_conversion_form_builders.c3`,
      `src/lisp/parser_parser.c3`, and
      `src/lisp/parser_application_placeholders.c3` now reject intern/allocation
      failure instead of propagating invalid symbol IDs or writing through
      unchecked macro AST allocations.
    - `src/lisp/async_tcp_transport_helpers.c3`,
      `src/lisp/async_tcp_transport_core.c3`,
      `src/lisp/async_process_signal_dns.c3`,
      `src/lisp/async_tcp_transport_connect.c3`,
      `src/lisp/async_udp_pipe.c3`,
      `src/lisp/async_tcp_transport_listen.c3`,
      `src/lisp/scheduler_state_types.c3`, and
      `src/lisp/scheduler_wakeup_queue.c3` now reject negative TCP read sizes,
      close stranded pending async state on resumed-before-completion errors,
      and track writable wakeup coalesces separately.
    - regression coverage added in:
      - `src/lisp/tests_advanced_core_unicode_groups.c3`
      - `src/lisp/tests_advanced_stdlib_numeric_groups.c3`
      - `src/lisp/tests_runtime_data_unicode_groups.c3`
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded arithmetic-comparison slice: `pass=45 fail=0`
      - bounded string-type slice: `pass=40 fail=0`
      - bounded data-format slice: `pass=62 fail=0`
      - bounded async slice: `pass=61 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded advanced numeric string/predicate/format group:
        `pass=59 fail=0`
      - bounded advanced unicode iterator group: `pass=130 fail=0`
      - bounded advanced macro hygiene group: `pass=82 fail=0`

- [x] `AUDIT-RUNTIME-REGISTRY-IO-TLS-GUARDS-056` harden registry,
  bootstrap, unicode, collection, I/O, and TLS guard paths
  - closure evidence:
    - `src/lisp/value_symbol_table.c3` and
      `src/lisp/value_type_registry.c3` now reject exhausted ID spaces before
      narrowing counts to `SymbolId` / `TypeId`; symbol probing also skips
      out-of-range stale indices, and type rollback now rebuilds the hash table
      so open-address probe chains remain sound.
    - `src/lisp/value_interp_init_helpers.c3` and
      `src/lisp/value_interp_state.c3` now fail fast if interpreter bootstrap
      symbols cannot be interned instead of publishing invalid sentinel IDs.
    - `src/lisp/unicode_case_mapping.c3` now rejects strings too large for
      `utf8proc`'s `long` length parameter before case conversion.
    - `src/lisp/scheduler_offload_network.c3` now calls `br_sslio_close(...)`
      only after `br_sslio_init(...)` has completed.
    - `src/lisp/prim_collection_hashmap.c3` now computes Dictionary initial
      capacity with checked arithmetic and allocates the hashmap payload before
      publishing the root wrapper.
    - `src/lisp/prim_io_file_helpers.c3` now rejects file sizes that cannot fit
      in `usz` before buffer allocation, and
      `src/lisp/prim_io_console_helpers.c3` now reports render/write failures
      as typed I/O errors.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`
      - bounded async slice: `pass=61 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded advanced unicode iterator slice: `pass=129 fail=0`

- [x] `AUDIT-AOT-DISPATCH-SIGNATURE-ALLOC-GUARDS-055` harden AOT/JIT
  signature allocation and type publication failure paths
  - closure evidence:
    - `src/lisp/aot_type_spec_helpers.c3` now rejects overflowing type
      annotation, method-signature parameter, and method-constraint allocation
      sizes before allocation and delays count publication until staging
      succeeds.
    - `src/lisp/jit_jit_closure_support.c3` now guards lambda method-signature
      constraint counting and parameter/constraint staging on both scope and
      heap copies.
    - `src/lisp/eval_type_declarations.c3` now checks derived type-info
      field/type-param/constraint/variant allocation sizes and rolls back a
      newly registered type if later constructor/global binding/type-value
      publication fails.
    - `src/lisp/value_type_registry.c3` now exposes a narrow
      just-added-type rollback helper for new-entry failure cleanup.
    - `src/lisp/eval_dispatch_match.c3`,
      `src/lisp/eval_dispatch_match_breakdown.c3`,
      `src/lisp/schema_explain_helpers.c3`, and
      `src/lisp/aot_runtime_bridge_helpers.c3` now reject overflowing temporary
      dispatch/schema/AOT staging buffers.
    - `src/lisp/eval_dispatch_types.c3` and
      `src/lisp/eval_init_primitives.c3` now clean up empty heap method-table
      bootstrap payloads when root-wrapper or global binding publication fails.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-JIT-DEDUCE-ALLOC-BOUNDS-054` harden JIT effect and deduce
  aggregate/materialization allocation arithmetic
  - closure evidence:
    - `src/lisp/jit_jit_handle_signal_handle.c3`,
      `src/lisp/jit_jit_runtime_effects_signal.c3`, and
      `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now reject
      overflowing effect clause and signal argument buffer sizes before
      allocation.
    - `src/lisp/deduce_rule_eval_exec_aggregate_groups.c3`,
      `src/lisp/deduce_rule_eval_exec_aggregate_helpers.c3`,
      `src/lisp/deduce_rule_ir_helpers.c3`, and
      `src/lisp/deduce_rule_ir.c3` now check aggregate group, encoded tuple,
      rule-term, and rule-atom allocation arithmetic before writing staged
      state.
    - `src/lisp/deduce_relation_row_materialization.c3` now clamps dictionary
      capacity arithmetic and rejects overflowing column-key value buffers.
    - `src/lisp/deduce_db_goal_directed_read_tracking.c3` now bounds
      read-position buffer sizing and writeback iteration from `ulong` counts.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded deduce slice: `pass=330 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-ENV-PROCESS-SPAWN-SIZE-GUARDS-053` harden environment,
  closure-wrapper, and process-spawn allocation sizing
  - closure evidence:
    - `src/lisp/value_environment_storage.c3` now rejects overflowing hash-table
      capacity and byte-size calculations before allocation.
    - `src/lisp/value_environment.c3` now guards load-factor multiplication
      before hash-table rebuild comparisons.
    - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now rejects overflowing
      closure parameter-copy allocations.
    - `src/lisp/async_process_spawn.c3` now rejects overflowing argv/env pointer
      table counts and byte sizes before process-spawn staging.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-RUNTIME-PROMOTION-ALLOC-STAGING-052` harden promotion,
  root-clone, env-copy, and pattern sequence allocation sizing and staging
  - closure evidence:
    - `src/lisp/eval_promotion_copy_route_helpers.c3`,
      `src/lisp/eval_promotion_escape_structured.c3`, and
      `src/lisp/eval_promotion_root_clone_basic.c3`
      now reject overflowing array, hashmap, method-table, signature, and
      closure-parameter allocation sizes before copying boundary-owned data.
    - `src/lisp/eval_promotion_escape_structured.c3`
      now resets staged method signatures on dependent allocation failure and
      delays closure result wrapper publication until fallible clone/env work
      succeeds.
    - `src/lisp/eval_env_copy_frame_helpers.c3`
      now allocates non-inline binding storage before publishing the copied
      frame and frees it if frame allocation fails.
    - `src/lisp/eval_pattern_matching.c3`
      now rejects overflowing sequence element collection buffers.
    - validation:
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-DEDUCE-SCC-ALLOC-BOUNDS-051` harden deduce SCC, reachability,
  proof-key, aggregate batch, component-delta, and schema-capacity arithmetic
  - closure evidence:
    - `src/lisp/deduce_rule_eval_scc_plan.c3`
      now checks SCC square matrix and stratum relaxation bound arithmetic.
    - `src/lisp/deduce_rule_eval_validation.c3`
      now checks reachability matrix square sizing before allocation.
    - `src/lisp/deduce_rule_eval_exec_seminaive.c3`,
      `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates_impl.c3`,
      and `src/lisp/deduce_rule_eval_exec_component_delta_restore.c3`
      now guard proof-key, aggregate batch, and decoded delta-entry allocation
      sizes.
    - `src/lisp/deduce_schema_query_input_shape.c3`,
      `src/lisp/deduce_schema_query_input_roles.c3`, and
      `src/lisp/deduce_schema_query_input_constraints.c3`
      now reject overflowing `count + 1` capacity requests.
    - validation:
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-AOT-FFI-ALLOC-STAGING-050` harden AOT type definition cleanup
  ownership and runtime FFI call argument staging
  - closure evidence:
    - `src/lisp/aot_type_definitions.c3`
      now initializes cleanup-owned type fields and union variants before any
      later fallible allocation can trigger deferred cleanup, and checks
      AOT field/variant/type-parameter allocation sizes before allocation.
    - `src/lisp/eval_ffi_bound_call.c3`
      now rejects unsupported/narrowing argument counts and overflowing
      libffi staging buffer sizes before preparing runtime call storage.
    - validation:
      - bounded compiler slice: `pass=191 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-PARSER-AST-ARRAY-ALLOC-HELPER-049` centralize parser AST array
  allocation overflow checks
  - closure evidence:
    - `src/lisp/parser_parser.c3`
      now exposes `Parser.alloc_ast_array_bytes(...)`, which checks
      `elem_size * count` before delegating to AST arena allocation.
    - Parser dynamic AST array allocations for calls, relation definitions,
      literals, patterns, type annotations, module bodies, lambda params,
      named-let rewrites, path expressions, macro clauses, blocks, and pipe
      rewrites now route through the checked helper.
    - `src/lisp/parser_set_pipe_helpers.c3`
      now rejects overflowing `arg_count + 1` before growing pipe call
      arguments.
    - validation:
      - bounded compiler slice: `pass=191 fail=0`
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-PARSER-AST-ARG-SIZE-GUARDS-048` harden parser call/import/FFI
  argument sizing before AST arena allocation
  - closure evidence:
    - `src/lisp/parser_define_relation_attr_helpers.c3`
      now checks relation role/count arithmetic and `Expr*` allocation sizing
      before building `__define-relation` call arguments.
    - `src/lisp/parser_application_helpers.c3`
      now checks generic call argument `Expr*` allocation sizing before
      copying argument pointers.
    - `src/lisp/parser_ffi_helpers.c3`
      now rejects overflowing FFI parameter counts before `+ 1` capacity
      checks.
    - `src/lisp/parser_import_helpers_specs.c3`
      now rejects overflowing selective-import counts before `+ 1` capacity
      checks.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-SHARED-ASYNC-STRING-SIZE-GUARDS-047` harden shared blob,
  process/TLS string duplication, filesystem result array growth, and
  compression output allocation sizes
  - closure evidence:
    - `src/lisp/scheduler_shared_handles_blob.c3`
      now rejects max-sized blob copies before `len + 1` allocation.
    - `src/lisp/async_process_signal_handles.c3`
      now rejects max-sized C-string copies, cons sequence count overflow, and
      argv/env `char*` table byte-size overflow before allocation.
    - `src/lisp/tls_offload_connect.c3`
      now rejects max-sized TLS duplicate strings before `len + 1` allocation.
    - `src/lisp/prim_io_fs_handles.c3`
      now rejects invalid/overflowing filesystem result array growth before
      replacing array backing storage.
    - `src/lisp/scheduler_offload_network.c3`
      now rejects max-sized compression bounds before allocating the output
      scratch buffer.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`

- [x] `AUDIT-ASYNC-FILE-IO-SIZE-GUARDS-046` harden async file I/O payload,
  temp-path, and read-buffer allocation sizes against `len + 1` overflow
  - closure evidence:
    - `src/lisp/prim_io_file.c3`
      now checks path/content payload length arithmetic before building the
      internal async write-file offload payload.
    - `src/lisp/scheduler_offload_ops.c3`
      now checks atomic temp-path length arithmetic before allocating the
      temporary path buffer.
    - `src/lisp/prim_io_file_helpers.c3`
      now rejects a max-sized read buffer before adding the trailing byte for
      the file-read scratch buffer.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`

- [x] `AUDIT-SCHEDULER-OFFLOAD-NIL-COMPLETION-PROJECTION-045` make
  scheduler offload `OFFLOAD_RES_NIL` projection terminal so async file
  missing-path failures are remapped to their I/O payload codes instead of
  leaking `scheduler/offload-invalid-completion-kind`
  - closure evidence:
    - `src/lisp/scheduler_wakeup_io.c3`
      now returns directly from each offload completion-kind projection,
      including the nil result path used by failed async read-file offloads.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`

- [x] `AUDIT-IO-STRING-BUFFER-GROWTH-HARDENING-044` harden console, input,
  CSV, REPL, and Unicode string-buffer growth paths against
  overflow-before-allocation and invalid capacity state
  - closure evidence:
    - `src/lisp/prim_io_console_helpers.c3`
      now checks console capture append/copy growth arithmetic before
      appending or duplicating buffered text.
    - `src/lisp/prim_io_helpers.c3`
      now rejects overflowing input-state append lengths before mutating the
      live buffer.
    - `src/lisp/primitives_data_formats_csv_parse.c3`
      now guards CSV field and row-array growth before allocating replacement
      storage.
    - `src/lisp/eval_repl_server_state.c3`
      now rejects overflowing session-string and session-capacity growth
      before publishing the replacement session state.
    - `src/lisp/unicode_case_utf8proc.c3` and
      `src/lisp/unicode_case_mapping.c3`
      now route case-mapping append growth through a checked helper so the
      UTF-8 output buffer cannot overflow or grow from invalid capacity state.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded unicode slice: `pass=25 fail=0`
      - bounded data-format slice: `pass=59 fail=0`
      - bounded async slice after scheduler nil-completion projection fix:
        `pass=61 fail=0`
      - bounded advanced unicode iterator group: `pass=129 fail=0`

- [x] `AUDIT-DEDUCE-DIRECT-ALLOC-SCHEMA-MUTATION-043` harden direct
  deduce schema/rule mutation allocation sites and transaction insert
  accounting
  - closure evidence:
    - `src/lisp/deduce_db_handles_mutation.c3`
      now guards rule-signature count increments, range checks, and direct
      `sizeof * count` array allocations before copying rule metadata.
    - `src/lisp/deduce_db_relation_schema_init.c3`,
      `src/lisp/deduce_db_handles.c3`, and
      `src/lisp/deduce_db_handles_register.c3`
      now reject overflowing schema/index sizing before allocation or count
      publication.
    - `src/lisp/deduce_db_handles_mutation_txn.c3`
      now increments transaction `inserted_count` only after tuple delta
      append succeeds.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-CORE-REGISTRY-TABLE-GROWTH-HARDENING-042` harden remaining
  core registry/table growth paths against overflow-before-allocation,
  failed-allocation state corruption, and invalid hashmap capacity state
  - closure evidence:
    - `src/lisp/value_symbol_table.c3` and
      `src/lisp/value_type_registry.c3`
      now reject overflowed init/grow byte-size arithmetic and keep previous
      table state when replacement allocation cannot be materialized.
    - `src/lisp/value_interp_init_helpers.c3`
      now checks macro/module init table/hash size multiplication before
      allocation.
    - `src/lisp/eval_pattern_match_support.c3`
      now guards `gensym` / match-binding grow loops against doubling and
      allocation-byte overflow.
    - `src/lisp/prim_collection_hashmap.c3` and
      `src/lisp/prim_collection_sort_array.c3`
      now enforce valid hashmap grow preconditions and reject overflowing grow
      arithmetic before mutating live collection state.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded allocator-validation slice: `pass=1 fail=0`
      - bounded advanced collections/module group: `pass=134 fail=0`
      - bounded advanced type-dispatch/mutation-chain group: `pass=236 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-OVERFLOW-HARDENING-BATCH-041` close the next allocator/cursor
  overflow audit batch across deduce persistence, AST/parser allocation,
  JIT/effect staging, interpreter/env table growth, and method-table growth
  - closure evidence:
    - persisted deduce rule signature/catalog record encode/restore paths now
      use checked size and cursor helpers before allocation, copy, and slice
      movement.
    - AST arena alignment/chunk accounting and parser import/module/export
      growth now reject allocation-size overflow before state mutation.
    - JIT arg-buffer, effect-handler, and handle-state copy allocation sites
      now reject oversized element counts before byte-size multiplication.
    - interpreter macro/module/handler table growth, env binding growth, and
      method-table growth now check doubling and allocation sizes before
      mutating tables.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded deduce slice: `pass=330 fail=0`
      - bounded allocator-validation slice: `pass=1 fail=0`
      - bounded advanced collections/module group: `pass=134 fail=0`
      - bounded advanced effect-continuation group: `pass=56 fail=0`
      - bounded advanced runtime-control group: `pass=22 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-RUNTIME-MODULE-EXPORT-GROWTH-FAILCLOSED-040` make module
  export-table growth fail closed instead of writing through failed
  replacement allocation
  - closure evidence:
    - `src/lisp/jit_jit_module_setup_helpers.c3`
      now routes export allocation/growth through a checked helper and
      preserves existing exports on growth failure.
    - `src/lisp/jit_jit_compile_effects_modules.c3` and
      `src/lisp/jit_jit_module_import_setup.c3`
      now propagate export growth failure from re-export and implicit module
      export paths.
    - `src/lisp/tests_advanced_stdlib_module_groups.c3`
      now pins forced module export growth allocation failure.
    - validation:
      - `c3c build`
      - bounded advanced collections/module group: `pass=134 fail=0`

- [x] `AUDIT-CAPACITY-REALLOC-BYTE-OVERFLOW-039` guard deduce/JIT
  capacity growth before `sizeof * new_cap` arithmetic can wrap and
  under-allocate
  - closure evidence:
    - deduce relation, aggregate, delta, query-demand, transaction,
      dirty-predicate, rule-signature, relation-schema, and persisted-rule
      catalog growth helpers now fail closed before allocation byte counts
      overflow.
    - `src/lisp/jit_jit_module_setup_helpers.c3`
      now rejects oversized source-dir vector growth and path length
      increments before allocation-size arithmetic can wrap.
    - `src/lisp/tests_deduce_groups_parallel.c3`
      now pins the oversized-capacity fail-closed regression.
    - validation:
      - `c3c build`
      - bounded deduce parallel group: `pass=6 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-SCHEDULER-OFFLOAD-MISSING-COMPLETION-038` make non-task
  offload worker double-allocation failure wake and fail closed instead of
  stranding a blocked waiter
  - closure evidence:
    - `src/lisp/scheduler_offload_worker.c3`
      now publishes or directly handles readiness even when both worker
      completion and fallback alloc-failure completion are null.
    - `src/lisp/scheduler_wakeup_io.c3`
      now consumes active completed-null offload slots as
      `"offload: missing completion"` and clears the pending slot.
    - `src/lisp/tests_scheduler_boundary_offload_payload_groups.c3`
      now pins the completed-null wakeup/consume path.
    - validation:
      - `c3c build`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-FORMAT-DISPLAY-TEMP-BUILDER-FAILCLOSED-037` fix `%s`
  formatting display path so plain `(format "%s" value)` does not fail with
  `"format: failed to grow temporary builder"` for ordinary values
  - closure evidence:
    - `src/lisp/prim_string_format_helpers.c3`
      now computes `StringVal` target capacity through checked overflow
      addition instead of comparing normal small appends against `usz.max`.
    - `src/lisp/tests_advanced_core_unicode_groups.c3`
      now pins both `(format "%s" nil)` and `(format "%s" (Void))`.
    - validation:
      - `c3c build`
      - bounded `advanced` slice with
        `OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator`: `pass=129 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`
      - bounded ASAN `memory-lifetime-smoke`: `pass=189 fail=0`
      - direct JSON REPL probes for `(format "%s" nil)`,
        `(format "%s" (Void))`, and a long `%s` string that requires growth

- [x] `AUDIT-RUNTIME-EFFECT-PUBLICATION-FAILCLOSED-036` make effect
  publication/dispatch fail closed when payload or continuation materialization
  fails, instead of degrading payload shape or null-dereferencing
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now makes handled raises reject payload-construction failure before
      handler bind instead of degrading to a message-only handled raise.
    - `src/lisp/value_interp_continuation_helpers.c3`
      now exposes a narrow continuation-allocation failure seam instead of
      dereferencing failed root-scope allocation.
    - `src/lisp/jit_jit_handle_signal.c3`,
      `src/lisp/jit_jit_runtime_effects_handle.c3`,
      `src/lisp/jit_jit_reset_shift.c3`, and
      `src/lisp/jit_jit_runtime_effects_reset_shift.c3`
      now fail closed on continuation allocation failure in handled effect and
      capture dispatch.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
      now returns `"runtime effect payload: out of memory"` when
      unhandled-effect diagnostic payload construction cannot complete,
      instead of silently dropping the payload.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` and
      `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now pin both handled-raise payload construction failure and handled
      effect continuation allocation failure directly in bounded slices.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=pending-raise-payload-alloc-failure,handle-continuation-alloc-failure ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-RUNTIME-LIST-MATERIALIZATION-FAILCLOSED-035` make helper-owned
  list materializers fail closed instead of continuing after cons-constructor
  faults
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now exposes a shared `make_cons_or_error(...)` helper with a narrow
      nth-failure seam for deterministic runtime list-builder tests.
    - `src/lisp/prim_string_transform.c3`
      now makes `string-split` reject internal result-list construction
      failure instead of continuing with partial/null list state.
    - `src/lisp/prim_io_file.c3`
      now makes `read-lines` reject internal result-list construction failure
      instead of continuing with partial/null list state.
    - `src/lisp/prim_collection_hashmap_key_helpers.c3`
      now makes `keys` / `values` canonical list assembly fail closed in both
      sorted and fallback paths instead of reusing raw `make_cons(...)`.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now directly pins string-split and hashmap key/value list allocation
      failure in the bounded runtime alloc lane.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-JIT-HELPER-ARG-CONSTRUCTION-FAILCLOSED-034` make JIT helper
  arg-list and variadic rest-list construction fail closed instead of passing
  raw cons-constructor faults into normal dispatch/binding flows
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now exposes a checked `make_list1_or_error(...)` helper with a narrow
      nth-failure seam for deterministic JIT variadic-rest tests.
    - `src/lisp/jit_jit_apply_helpers.c3` and
      `src/lisp/jit_jit_apply_runtime.c3`
      now reject variadic zero-fixed-arg rest-list construction failure before
      binding the rest parameter environment.
    - `src/lisp/jit_jit_dispatch_helpers.c3`
      now routes instance `ref` dispatch arg-list materialization through the
      shared checked two-item helper instead of nesting raw `make_cons(...)`.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins the variadic rest-list allocation failure in the
      `jit-policy` slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=variadic-rest-list-alloc-failure ./build/main --test-suite lisp'`

- [x] `AUDIT-JIT-QUASIQUOTE-CONS-FAILCLOSED-033` make JIT quasiquote pair
  construction fail closed instead of wrapping cons-constructor faults as
  successful quasiquote values
  - closure evidence:
    - `src/lisp/jit_jit_quasiquote_macros.c3`
      now routes all internal quasiquote pair construction through one checked
      helper with a narrow nth-failure seam, and returns
      `"quasiquote: failed to allocate pair"` on allocation failure instead of
      passing raw cons-constructor faults through `eval_ok(...)`.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins both nested quasiquote and list quasiquote pair
      materialization failure in the `jit-policy` slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=quasiquote-cons-alloc-failure ./build/main --test-suite lisp'`

- [x] `AUDIT-JIT-MULTI-ARG-LIST-FAILCLOSED-032` make JIT multi-arg call
  construction and iterative apply fail closed on malformed arg-list state
  instead of degrading to partial success
  - closure evidence:
    - `src/lisp/jit_jit_apply_runtime.c3`
      now rejects `make_cons(...)` failure while constructing continuation-safe
      multi-arg call lists instead of passing malformed arg lists downstream.
    - `src/lisp/jit_jit_apply_multi_prims.c3`
      now makes `jit_apply_multi_args_iterative(...)` return
      `"arg list too short"` when the arg list breaks before all required args
      are consumed, instead of breaking and returning the partial result.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins the malformed multi-arg list case in `jit-policy`
      using a two-arg curried closure and a one-element arg list.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=multi-arg-malformed-list-fails-closed ./build/main --test-suite lisp'`

- [x] `AUDIT-TWO-ARG-LIST-MATERIALIZATION-FAILCLOSED-031` make shared
  two-value list/arg materialization fail closed instead of publishing cons
  constructor failures as ordinary runtime data
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now exposes one checked `make_list2_or_error(...)` helper with a narrow
      nth-failure seam for deterministic runtime/JIT constructor tests.
    - `src/lisp/prim_system.c3`
      now makes `(shell cmd true)` fail closed with
      `"shell: failed to construct result list"` if the final two-item result
      list cannot be built.
    - `src/lisp/jit_jit_runtime_effects_handle.c3`
      now routes both pending-raise and normal effect-handler arg-pair
      construction through the same checked helper, so constructor failure
      propagates before handler call-through.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins the shell result-list construction seam.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins the pending-raise and signal-handler arg-pair seam in
      the `jit-policy` slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=handler-arg-list-alloc-failure ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-BATCH-RESULT-LIST-FAILCLOSED-030` make scheduler
  batch primitives fail closed when result-list cons construction fails instead
  of publishing partial success or leaking spawned task state
  - closure evidence:
    - `src/lisp/scheduler_primitives_threads.c3`
      now routes batch thread result-list assembly through one checked
      scheduler-local prepend helper with a narrow nth-failure seam.
    - `src/lisp/scheduler_primitives_offload_execute.c3`
      now makes offload batch result-list construction return a typed
      `"offload: out of memory"` error instead of publishing a partial result
      list.
    - `src/lisp/scheduler_primitives_task_spawn.c3`
      now drops already-spawned live thread-task entries if result-list
      publication fails after task creation.
    - `src/lisp/tests_scheduler_groups_more.c3`
      now pins forced result-list cons allocation failure for offload-batch,
      task-spawn-batch, and thread-spawn-batch, and proves active thread-task
      count is unchanged after the failure path.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`

- [x] `AUDIT-ITERATOR-CONS-CONSTRUCTOR-FAILCLOSED-029` make iterator
  coroutine cons construction fail closed instead of publishing constructor
  failures as data or remapping them to misleading apply errors
  - closure evidence:
    - `src/lisp/primitives_iter_coroutine.c3`
      now routes `zip` item-pair and `foldl` arg-list construction through a
      checked iterator-local cons helper with a narrow nth-failure seam.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now proves forced constructor failure makes `zip` return
      `"__iterator-zip: failed to allocate item pair"` and `foldl` return
      `"__iterator-foldl: failed to allocate call args"` instead of embedding
      `ERROR` values into iterator data or degrading to `"arg list too short"`.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-PENDING-RAISE-PAYLOAD-FAILCLOSED-028` make pending raise
  payload/message materialization fail closed instead of binding constructor
  failures as ordinary handler data
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now rejects `boundary_promote_to_root(...)` null/error results before
      publishing pending raise payload state.
    - `src/lisp/jit_jit_runtime_effects_handle.c3`
      now rejects raise fallback `make_string(...)` and arg-pair
      `make_cons(...)` failure before handler call-through.
    - `src/lisp/jit_jit_handle_signal_handle.c3`
      now rejects raise fallback string materialization failure before clause
      env extension.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now proves the surface `handle` form returns a top-level eval error
      instead of invoking the raise clause when pending raise message
      materialization fails.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now pins the same fail-closed contract in the `jit-policy` slice.
    - validation:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=pending-raise-string-alloc-failure ./build/main --test-suite lisp`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-OS-THREAD-NULL-COMPLETION-FAILCLOSED-027` make
  `scheduler_complete_os_thread(...)` drop+wake instead of stranding a running
  OS-thread entry when both completion materialization and alloc-failure
  completion materialization fail
  - closure evidence:
    - `src/lisp/scheduler_thread_task_transition_scaffold.c3`
      now exposes a narrow transition-completion allocation fail seam for
      deterministic scheduler boundary tests.
    - `src/lisp/scheduler_thread_task_transitions.c3`
      now drops the OS-thread entry on null completion plus alloc-failure
      completion OOM instead of returning with the entry still running.
    - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`
      now pins the double-failure seam and proves the blocked waiter wakes,
      join token clears, and the OS-thread entry is removed.
    - `src/lisp/tests_scheduler_groups.c3`
      now wires the new boundary regression into the bounded scheduler slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-ITERATOR-MALFORMED-TAIL-FAILCLOSED-026` make iterator
  terminal and coroutine helpers reject malformed pair tails instead of
  truncating pipelines as successful completion
  - closure evidence:
    - `src/lisp/primitives_iter_state.c3`
      now exposes one shared `iterator_tail_or_error(...)` helper for
      fail-closed iterator tail validation.
    - `src/lisp/primitives_iter_terminal.c3`
      now makes `collect` / `to-array` reject malformed iterator pairs and
      malformed iterator tails instead of silently truncating the result.
    - `src/lisp/primitives_iter_coroutine.c3`
      now makes `map`, `filter`, `take`, `zip`, and `foldl` reject malformed
      iterator tails instead of truncating or deferring broken state as normal
      completion.
    - `src/lisp/tests_advanced_core_unicode_groups.c3`
      now pins malformed-tail rejection through the surface `List`, `Array`,
      `map`, `filter`, `take`, `zip`, and `foldl` iterator pipelines.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (List (Iterator (lambda () (cons 1 2)))) (raise msg (ref msg 'message)))\""`
      - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (foldl (lambda (a x) (+ a x)) 0 (Iterator (lambda () (cons 1 9)))) (raise msg (ref msg 'message)))\""`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-WAKEUP-PUBLISH-FALLBACK-025` make scheduler timer,
  sleep, poll-error, and non-task offload wakeups fail closed when reliable
  wakeup enqueue fails instead of dropping the blocked-fiber completion
  - closure evidence:
    - `src/lisp/scheduler_wakeup_callbacks.c3`
      now makes timer, sleep, and poll-error callbacks fall back to the same
      direct wakeup handlers on reliable queue publish failure.
    - `src/lisp/scheduler_offload_worker.c3`
      now makes non-task worker completion fall back to
      `scheduler_handle_wakeup_offload_ready(...)` on publish failure instead
      of freeing the live completion payload.
    - `src/lisp/tests_scheduler_groups_more.c3`
      now pins the real enqueue-failure seam for timer, sleep, poll-error,
      and offload-after fallback.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-SHARED-PROJECTION-FAILCLOSED-024` make scheduler
  shared-handle and offload-path projection fail closed instead of publishing
  empty-string or false-success results
  - closure evidence:
    - `src/lisp/scheduler_state_shared_handles.c3`
      now makes `scheduler_project_shared_to_local_value(...)` return
      scheduler `ERROR`s for missing handle refs and shared-payload
      materialization failure instead of an empty string.
    - `src/lisp/scheduler_offload_ops.c3`
      now makes `scheduler_offload_read_file(...)` and
      `scheduler_offload_file_exists(...)` report `OFFLOAD_RES_ERROR` for
      missing/invalid projected path payloads instead of synthesizing
      `nil`/`0` success results.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins both the direct shared-handle projection failure and the
      offload-path projection failure family.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-CONS-ESCAPE-PROMOTION-FAILCLOSED-023` make escape-lane cons
  publication fail closed when string/error promotion cannot actually move the
  field into the ESCAPE lane
  - closure evidence:
    - `src/lisp/value_constructors_core.c3`
      now stages `car` / `cdr` escape promotion before pair allocation,
      rejects null or `ERROR` promotion results, rejects the string/error
      case where promotion falls back to the original non-escape value, and
      unwinds staged promoted fields if final pair allocation fails.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins the forced string/error escape-promotion seam and proves
      `make_cons(...)` returns a typed error instead of publishing an
      ESCAPE-lane cons that still points at a TEMP string.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-DATA-FORMAT-ARRAY-PROMOTION-FAILCLOSED-022` make JSON/TOML
  array assembly reject promoted boundary `ERROR` values instead of storing
  them as ordinary array elements
  - closure evidence:
    - `src/lisp/json.c3`
      now rejects `boundary_promote_to_root(...)` results that come back as
      `ERROR` values during JSON array assembly instead of publishing them
      into successful arrays.
    - `src/lisp/primitives_toml_bridge.c3`
      now applies the same fail-closed rule for TOML array element promotion.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins a TOML array-element promotion fault through the existing
      `TIME_POINT` wrapper-copy allocation seam under a non-root scope and
      proves the array conversion returns the boundary error directly.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-COROUTINE-THUNK-PROMOTION-FAILCLOSED-021` make coroutine thunk
  publication fail closed when root promotion returns an `ERROR` or invalid
  callable state instead of allocating coroutine stack context around a bad
  thunk
  - closure evidence:
    - `src/lisp/primitives_coroutine.c3`
      now makes both `prim_coroutine_prepare_thunk(...)` and
      `prim_coroutine_create_ctx(...)` reject:
      - null promotion results,
      - promoted `ERROR` values,
      - and non-closure / null-closure thunk state
      before any stack context or coroutine wrapper allocation.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins the forced closure-wrapper promotion-allocation seam and proves
      coroutine construction aborts before `stack_ctx_pool` allocation
      counters change.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-APPLY-DISPATCH-FAILCLOSED-020` make malformed primitive and
  partial-application helper paths fail closed instead of dereferencing null
  call targets or silently consuming invalid state
  - closure evidence:
    - `src/lisp/eval_apply.c3`
      now rejects invalid primitive application state when the primitive
      wrapper is null, wrongly tagged, missing `prim_val`, or missing
      `prim_val.func`, and rejects partial-application state when
      `first_arg == null`.
    - `src/lisp/jit_jit_apply_helpers.c3`
      now makes `jit_apply_value_primitive(...)` reject malformed primitive
      wrappers before any function-pointer call-through.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins invalid partial state and invalid primitive-wrapper execution in
      the direct runtime helper lane.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now pins malformed primitive wrapper rejection in the JIT helper lane.
    - validation:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=invalid-primitive-state-fails-closed ./build/main --test-suite lisp`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-APPLY-PROMOTION-FAILCLOSED-019` make partial-application state
  and root-promotion array/hashmap helper surfaces fail closed instead of
  executing impossible state or storing promoted `ERROR` values as ordinary
  data
  - closure evidence:
    - `src/lisp/eval_apply.c3`
      now rejects invalid `PARTIAL_PRIM` state before call-through:
      null/non-partial input, null function pointer, `remaining <= 0`,
      `remaining > 2`, and the inconsistent `remaining == 2` with a prefilled
      `second_arg`.
    - `src/lisp/prim_collection_hashmap.c3`
      now makes `hashmap_set_symbol_checked(...)` and
      `hashmap_set_checked(...)` reject `boundary_promote_to_root(...)`
      results that come back as `ERROR` values instead of inserting them.
    - `src/lisp/prim_io_fs_handles.c3` and
      `src/lisp/primitives_data_formats_csv_parse.c3`
      now make `fs_array_push(...)` and `csv_array_push(...)` reject promoted
      `ERROR` values instead of appending them into successful arrays.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins:
      - invalid partial-application state,
      - checked hashmap insertion under opaque primitive promotion failure,
      - and `fs` / CSV array helper pushes under the same promoted-error seam.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-COLLECTION-ARRAY-FAILCLOSED-018` make collection/apply array
  write helpers fail closed on boundary promotion faults, grow failures, and
  comparator runtime errors
  - closure evidence:
    - `src/lisp/prim_collection_sort_array.c3`
      now makes `sort` / `sort-by` list rebuilding propagate `make_cons(...)`
      failure directly, makes `sort-by` propagate comparator application
      errors instead of silently returning a partial sort, makes `array`,
      `list->array`, `set!` on arrays, and `push!` reject
      `boundary_promote_to_root(...)` failures instead of storing `ERROR`
      values as data, and makes `push!` fail closed on grow allocation
      failure instead of null-dereferencing the new item buffer.
    - `src/lisp/primitives_iter_terminal.c3`
      now makes `collect` propagate list-construction failure directly and
      makes `to-array` reject `boundary_promote_to_root(...)` failures
      instead of returning arrays populated with `ERROR` elements.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins array constructor/mutator and `to-array` boundary-promotion
      failure, `push!` grow failure, and `sort-by` comparator failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-STRING-PAYLOAD-MATERIALIZERS-017` make string-backed runtime
  payload/list helpers fail closed on string wrapper allocation failure
  - closure evidence:
    - `src/lisp/prim_system.c3`
      now makes `(shell cmd true)` propagate stdout string construction
      failure directly instead of returning a success-shaped `(ERROR
      exit-code)` list.
    - `src/lisp/prim_io_fs_handles.c3`
      now makes `fs-readdir` propagate entry-name string construction failure
      directly instead of storing `ERROR` values as directory entries in a
      successful array.
    - `src/lisp/http.c3`
      now makes `http-get` / `http-request` propagate host/request string
      materialization failure directly before transport setup/write.
    - `src/lisp/schema_validation.c3`
      now makes `schema-explain` propagate failure of its singleton message
      string instead of returning a one-element list containing `ERROR` as
      ordinary data.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins `shell`, `fs-readdir`, and `schema-explain` under forced
      `make_string(...)` allocation failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-STRING-LIST-MATERIALIZERS-016` make pure string/list helper
  surfaces fail closed on per-element string wrapper allocation failure
  - closure evidence:
    - `src/lisp/prim_string_transform.c3`
      now makes `string-upcase` / `string-downcase` return constructor
      `ERROR`s directly and makes `string-split` propagate per-part
      string-allocation failure.
    - `src/lisp/prim_string_ops.c3`
      now makes `string->list` propagate per-character string wrapper
      allocation failure instead of embedding an `ERROR` into a successful
      list.
    - `src/lisp/unicode.c3`
      now makes `string-graphemes` propagate grapheme-cluster string
      construction failure directly instead of storing `ERROR` values in the
      cluster array/list.
    - `src/lisp/prim_io_file.c3`
      now makes `read-lines` propagate per-line string-construction failure
      directly.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins `string-upcase`, `string-downcase`, `string->list`,
      `string-split`, and `string-graphemes` under forced
      `make_string(...)` allocation failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEMA-EXPLAIN-LIST-BUILDERS-015` make schema-explain list
  assembly fail closed instead of hard-aborting on internal cons allocation
  failure
  - closure evidence:
    - `src/lisp/schema_explain_payload_helpers.c3` now routes list
      accumulation/reversal through `explain_prepend_or_oom(...)` instead of
      raw `make_cons(...)`.
    - `src/lisp/schema_explain_helpers.c3`,
      `src/lisp/schema_explain_effect_helpers.c3`, and
      `src/lisp/schema_explain_effect_runtime.c3`
      now propagate that failure through dispatch candidates, handler tag
      lists, and effect candidates.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins helper-level and top-level schema-explain list-builder OOM
      seams through a dedicated local `nth` fail seam.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-COLLECTION-MUTATOR-CHECKED-RETURNS-014` make dictionary/set
  mutation fail closed on backing-storage grow failure instead of silently
  dropping writes behind void mutator wrappers
  - closure evidence:
    - `src/lisp/prim_collection_hashmap.c3` now makes
      `hashmap_set_symbol(...)`, `hashmap_grow(...)`, and `hashmap_set(...)`
      return checked `bool` results instead of discarding insertion/grow
      failure.
    - `src/lisp/prim_collection_hashmap.c3` now makes `set!` on dictionary
      targets return `runtime/out-of-memory` when backing-storage growth
      fails, instead of returning `Void` after a dropped write.
    - `src/lisp/prim_collection_generic_set.c3` now makes `set-add` follow
      the same checked mutator contract for `SET`.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves
      both mutators fail with typed errors and leave the failed key absent
      from the target collection after grow failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-DEDUCE-EXPLAIN-INSERT-FAILCLOSED-013` make deduce explain,
  analyze, schema, stats, and why-result payload builders propagate checked
  `explain_dict_set*` failures instead of returning partially populated maps
  - closure evidence:
    - `src/lisp/deduce_why_result_payload.c3`,
      `src/lisp/deduce_why_result_path_payload.c3`,
      `src/lisp/deduce_why_result_lookup.c3`, and
      `src/lisp/deduce_why_result_lookup_derived.c3`
      now return the first insertion/grow failure from path/context/payload
      attachment instead of silently dropping the failed field and returning a
      successful why-result payload.
    - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`,
      `src/lisp/deduce_rule_eval_analyze_payload_tail.c3`, and
      `src/lisp/deduce_rule_eval_analyze_payload_result.c3`
      now treat payload-field insertion failure as a first-class
      `deduce/analyze` error instead of ignoring the failed write and
      continuing with a partial result map.
    - the remaining deduce explain/schema/stats helper family now follows the
      same checked insertion contract:
      - `src/lisp/deduce_parallel_runtime_truth.c3`
      - `src/lisp/deduce_rule_ops_explain_goal_directed.c3`
      - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3`
      - `src/lisp/deduce_rule_ops_explain_plan_payload.c3`
      - `src/lisp/deduce_rule_ops_explain_plan_steps.c3`
      - `src/lisp/deduce_rule_ops_explain_projection.c3`
      - `src/lisp/deduce_rule_ops_explain_snapshot.c3`
      - `src/lisp/deduce_rule_ops_explain_step_counters.c3`
      - `src/lisp/deduce_schema_query_metadata_schema_helpers.c3`
      - `src/lisp/deduce_schema_query_metadata_schema_payloads.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_payload.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_tail.c3`
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-GUARDED-HASHMAP-CALLERS-012B` normalize the remaining already-guarded `make_hashmap(...)` caller family onto checked constructors or a shared fail-closed helper contract
  - closure evidence:
    - `src/lisp/deduce_relation_row_materialization.c3` now uses checked
      hashmap construction plus checked insertion for row-dict materialization.
    - `src/lisp/deduce_relation_ops_validation_payload.c3` now routes
      integrity payload maps through a checked helper and drops the payload
      cleanly when insertion fails instead of returning partially populated
      dicts.
    - deduce runtime helper state maps in:
      - `src/lisp/deduce_rule_eval_exec_component_state_helpers.c3`
      - `src/lisp/deduce_rule_eval_exec_component_state.c3`
      - `src/lisp/deduce_rule_eval_exec_aggregate_state.c3`
      - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
      - `src/lisp/deduce_rule_eval_scc.c3`
      - `src/lisp/deduce_relation_scan_helpers_join.c3`
      - `src/lisp/deduce_rule_eval_analyze_setup.c3`
      - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
      now all use checked constructor/insertion paths.
    - the remaining deduce explain/schema/analyze and why-result payload dict
      families no longer use raw `make_hashmap(...)` either:
      - `rg -n "make_hashmap\\(" src/lisp/deduce_* src/lisp/unify_* -S`
        returns no matches.
    - regressions now pin:
      - helper-state constructor OOM in
        `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      - integrity payload-map OOM degradation in
        `src/lisp/tests_deduce_groups_integrity.c3`
      - why-result payload/path OOM in
        `src/lisp/tests_deduce_query_groups.c3`
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-RAW-HASHMAP-CRASHERS-012A` close the direct raw-hashmap caller class that still dereferenced payloads without constructor checks
  - closure evidence:
    - `src/lisp/unify_match_helpers.c3` now routes `build_result_dict(...)`
      through checked hashmap construction and checked insertion, and returns
      `deduce/match-out-of-memory` on constructor or insertion failure.
    - `src/lisp/unify_scan_helpers.c3` now propagates that builder `ERROR`
      directly instead of embedding it into a successful result list.
    - `src/lisp/tests_deduce_query_groups.c3` now proves `deduce 'match`
      propagates result-dict constructor OOM directly.
    - the residual raw-hashmap backlog is now just the already-guarded
      normalization family:
      - `AUDIT-COLLECTION-CONSTRUCTOR-GUARDED-HASHMAP-CALLERS-012B`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-RAW-ARRAY-AOT-011A` close raw array constructor and AOT dict payload fail-closed gaps
  - closure evidence:
    - `src/lisp/value_predicates_accessors_basic.c3` now routes
      `make_array(...)` through the checked array constructor path instead of
      raw unchecked allocation.
    - `src/lisp/prim_collection_sort_array.c3` now propagates array
      constructor `ERROR`s from `array(...)` and `list->array(...)` instead of
      dereferencing a partially initialized wrapper.
    - `src/lisp/aot_runtime_bridge.c3` now routes `dict_from_args(...)`
      through checked hashmap construction and checked insertion, so bridge
      payload creation fails closed under allocation pressure.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins raw
      array-constructor OOM behavior directly.
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now pins active
      bridge-interpreter `dict_from_args(...)` hashmap-constructor OOM
      behavior directly.

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011` migrate remaining runtime/status payload builders off unchecked collection constructors
  - closure evidence:
    - `src/lisp/async_process_signal_dns_process.c3`,
      `src/lisp/async_process_spawn.c3`, and
      `src/lisp/prim_io_fs_handles.c3`
      now route runtime status payload builders through checked `HASHMAP` /
      `ARRAY` constructors and checked hashmap insertion instead of mutating
      unchecked constructor results.
    - `src/lisp/http_url_response.c3` now constructs parsed HTTP response
      payload maps through the same checked contract.
    - `process-spawn` now also closes its live process/fs handles if final
      success-payload map construction fails, so constructor OOM cannot strand
      a half-built success-shaped result with open resources.
    - `src/lisp/eval_dispatch_error_payloads.c3` now treats lambda and
      ambiguous-dispatch payload dictionaries as optional under OOM: the
      primary typed error still returns even if payload-map construction or
      insertion fails.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now applies
      the same optional-payload contract to unhandled-effect error payloads.
    - `src/lisp/primitives_meta_types_ctor_helpers.c3` now makes
      `ctor_mismatch_data(...)` fail closed by returning `null` instead of
      dereferencing unchecked hashmap payloads.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins fs,
      process-spawn, process-wait, HTTP response payload, dispatch-payload,
      runtime-effect payload, and ctor-mismatch constructor OOM paths directly.
    - no same-lane residual callsites remain from the staged runtime/status
      payload-builder family.

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-SCHEMA-EXPLAIN-010` migrate schema explain payload builders onto checked collection-constructor OOM contracts
  - closure evidence:
    - `src/lisp/schema_explain_payload_helpers.c3` now centralizes checked map
      construction and checked `explain_dict_set*` insertion through one
      explicit `"schema explain: out of memory"` contract.
    - `src/lisp/schema_explain_helpers.c3`,
      `src/lisp/schema_explain_effect.c3`,
      `src/lisp/schema_explain_effect_result_payload.c3`,
      `src/lisp/schema_explain_effect_runtime.c3`, and
      `src/lisp/schema_explain_effect_helpers.c3`
      now route entrypoint/result/candidate/source payload maps through that
      checked path instead of dereferencing unchecked `make_hashmap(...)`
      results.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves:
      - dispatch explain result construction fails closed on map OOM,
      - effect explain result construction fails closed on map OOM,
      - helper payload/source maps fail closed on map OOM.
    - the separate runtime/status payload-builder lane is now closed under
      `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-CALLSITE-MIGRATION-009` close the data-format bridge slice of internal collection-constructor OOM hardening and split the residual backlog by real callsite family
  - closure evidence:
    - `src/lisp/json.c3` and `src/lisp/primitives_toml_bridge.c3` now use
      checked `ARRAY` / `HASHMAP` constructors plus checked hashmap insertion
      in their recursive decode paths.
    - nested conversion `ERROR`s in those files now propagate directly instead
      of being embedded into partial arrays/dicts.
    - `src/lisp/primitives_data_formats_csv_parse.c3` now uses checked row and
      result-array constructors and propagates constructor/cell materialization
      errors directly.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves JSON,
      TOML, and CSV constructor OOM paths fail closed.
    - the broad umbrella item is now split into:
      - `AUDIT-COLLECTION-CONSTRUCTOR-SCHEMA-EXPLAIN-010`
      - `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`

- [x] `AUDIT-ITERATOR-TAIL-ERROR-PROPAGATION-008` stop iterator tail-construction faults from degrading into silent truncation
  - closure evidence:
    - `src/lisp/primitives_iter_state.c3` now routes `(item . next)` iterator
      pair construction through `iterator_make_pair_or_propagate(...)`, which
      returns tail `ERROR` values directly instead of wrapping them in `CONS`.
    - source iterator thunks in `src/lisp/primitives_iter_sources.c3` and
      coroutine/transform thunks in `src/lisp/primitives_iter_coroutine.c3`
      now share that helper, so tail constructor failure no longer looks like
      normal iterator completion/truncation.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves both
      a source thunk (`repeat`) and a coroutine thunk (`take`) propagate the
      tail allocation error directly.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=128 fail=0`

- [x] `AUDIT-RUNTIME-CONSTRUCTOR-OOM-SUBSTRATE-008` harden shared runtime error/collection constructor paths that live iterator/error helpers depend on
  - closure evidence:
    - `src/lisp/value_core_types.c3`,
      `src/lisp/value_interp_alloc_helpers.c3`,
      `src/lisp/value_constructors_lifecycle.c3`, and
      `src/lisp/primitives_meta_types.c3`
      now track whether `STRING` / `ERROR` chars are heap-owned, so fallback
      literal-backed error values no longer flow into invalid frees during
      normal teardown or `unsafe-free`.
    - `src/lisp/value_constructors.c3` now makes `make_error(...)` fail closed
      when its message buffer allocation fails.
    - `src/lisp/value_predicates_accessors_basic.c3` and
      `src/lisp/prim_collection_hashmap.c3`
      now expose checked `ARRAY` / `HASHMAP` / `SET` constructor and grow
      helpers, and the live runtime-dependent surfaces now use them:
      - raise payload construction
      - `Dictionary`
      - `Set`
      - `to-array`
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins the
      exact constructor seams for:
      - printable `make_error(...)` fallback
      - iterator ctor raise payload-map failure
      - `to-array` result-array failure
      - checked collection constructor/grow failure
    - residual broader internal constructor migration is now split into
      `AUDIT-COLLECTION-CONSTRUCTOR-CALLSITE-MIGRATION-009` instead of being
      left implicit.

- [x] `AUDIT-STRING-BUILDER-OOM-007` harden shared `StringVal` builder creation and growth to fail closed
  - closure evidence:
    - `src/lisp/prim_string_format_helpers.c3` now gives `StringVal` an
      explicit fail-closed contract:
      - `strval_new(...)` returns `null` instead of dereferencing failed
        allocations,
      - `strval_ensure(...)` returns `bool`, guards size overflow, and marks
        builder failure on grow failure,
      - `strval_push(...)` / `strval_append(...)` / padding helpers now stop
        writing after a failed growth attempt,
      - deterministic seams were added for initial builder allocation and
        builder growth failure.
    - parser string literal construction paths in
      `src/lisp/parser_datum_helpers.c3`,
      `src/lisp/parser_expr_atoms.c3`,
      `src/lisp/parser_patterns_values.c3`, and
      `src/lisp/parser_quasiquote_datum_helpers.c3`
      now share the checked builder path and set parser errors instead of
      dereferencing a failed builder allocation.
    - runtime string helpers in
      `src/lisp/prim_string_ops.c3`,
      `src/lisp/prim_string_format.c3`, and
      `src/lisp/prim_string_format_directives.c3`
      now fail closed on builder creation/growth failure instead of writing
      through invalid builder buffers.
    - `src/lisp/primitives_meta_types.c3` no longer uses unchecked `StringVal`
      allocation in the `unsafe-free` error path.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=127 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp'` -> `pass=191 fail=0`

- [x] `AUDIT-JIT-POLICY-FULL-SLICE-006` isolate and close the remaining non-continuation `jit-policy` slice crash
  - closure evidence:
    - the crash was isolated to the `stale-raise-scrub` JIT policy case, but
      the actual fault site was the TCO recycle TEMP-graph scan in
      `src/lisp/jit_jit_eval_scope_chain_helpers.c3`, not stale raise state.
    - `jit_graph_binding_reaches_temp_scope(...)` no longer allocates four
      `4096`-entry pointer arrays on the runtime stack; it now uses one
      heap-backed `JitTempGraphScan`, closing the entry-time stack-overflow
      crash on smaller runtime stacks.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=stale-raise-scrub ./build/main --test-suite lisp'` -> `1 passed, 0 failed`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp'` -> `pass=41 fail=0`

- [x] `AUDIT-BOUNDARY-WRAPPER-SLOT-LEAK-005` eliminate pre-dtor wrapper-slot leaks on partial-abort copy/root-store paths
  - closure evidence:
    - `src/lisp/eval_promotion_copy_route_helpers.c3`,
      `src/lisp/eval_promotion_root_clone_basic.c3`, and
      `src/lisp/eval_promotion_root_clones.c3` now allocate/register the
      destination wrapper only after all fallible child-copy and payload clone
      work succeeds.
    - wrapper-allocation failure after payload success now routes through the
      existing partial-cleanup helpers, so already-copied child retains and
      heap payloads are unwound before returning.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves repeated
      failed shared-wrapper copy attempts do not grow the surviving target
      scope allocation count.
    - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now proves the
      same invariant for repeated failed root-store method-table clone
      attempts against `root_scope`.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=106 fail=0`
      - `scripts/check_status_consistency.sh` -> pass

- [x] `AUDIT-BOUNDARY-DESTINATION-CTX-005` route direct destination escape promotion through the caller promotion context
  - closure evidence:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now routes
      direct destination escape promotion through a shared ctx-aware helper
      instead of silently falling back to `interp.active_promotion_ctx`.
    - `src/lisp/eval_boundary_commit_escape_helpers.c3` and
      `src/lisp/eval_boundary_commit_destination.c3` now use that helper for
      releasing-scope retry, mixed-destination retry, and direct destination
      promotion, so destination commit stays inside the caller-owned
      memo/budget/abort epoch.
    - destination-builder teardown now restores both `memo_head` and the
      small scope-chain cache snapshot, so temporary build-scope cache entries
      cannot survive after the builder returns or aborts.
    - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
      proves:
      - repeated builder calls do not retain builder-local scope-chain cache
        state, and
      - a non-active caller `PromotionContext` still receives the abort state
        from direct destination promotion while the unrelated active context
        remains untouched.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> pass
      - `scripts/check_status_consistency.sh` -> pass

- [x] `AUDIT-BOUNDARY-DESTINATION-MEMO-004` define correct promotion-context memo semantics for destination builders
  - closure evidence:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now makes the
      shipped contract explicit: memo entries remembered while routing nested
      children inside temporary destination build scopes are builder-local and
      are discarded when the builder returns or aborts.
    - `src/lisp/eval_boundary_commit_escape_cons.c3` and
      `src/lisp/eval_boundary_commit_escape_wrappers.c3` now route that
      save/restore policy through shared helpers instead of leaving it as an
      implicit per-builder pattern.
    - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
      proves repeated destination-builder calls in one promotion epoch do not
      retain child memo entries after return and therefore materialize fresh
      destination graphs instead of reusing transient builder-local memo state.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=90 fail=0`

- [x] `AUDIT-BOUNDARY-PROVENANCE-WRAPPER-004` re-audit target-chain wrapper reuse for nested child-owned payloads
  - closure evidence:
    - `src/lisp/eval_boundary_provenance.c3` now walks nested `ARRAY`,
      `HASHMAP` / `SET`, and `METHOD_TABLE` payload edges before admitting
      target-chain fast reuse, so the reuse classifier now agrees with the
      existing graph-audit ownership model instead of checking only the wrapper
      shell.
    - target-chain shared wrappers now fall back into the existing copy /
      ESCAPE builders whenever any nested child still lives in the releasing
      scope or outside the surviving target chain.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now pins the exact
      target-chain-wrapper regression for `ARRAY`, `HASHMAP`, `SET`, and
      `METHOD_TABLE`, and
      `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` keeps it in the
      bounded smoke lane.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=89 fail=0`

- [x] `AUDIT-COMPILER-DIAGNOSTIC-PARITY-003` unify remaining JIT/AOT diagnostic drift and cover prelude-remapped parser coordinates
  - closure evidence:
    - `src/lisp/jit_jit_apply_multi_prims.c3` now emits the same canonical under-arity text as the tail/AOT helpers for both fixed multi-arg closures and variadic multi-arg closure application.
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now directly asserts that compile-time parser failures report user-source coordinates after the stdlib prelude offset is stripped.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` -> `pass=194 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-BOUNDARY-METHOD-TABLE-FAILURE-TEST-003` add deterministic coverage for method-table partial-cleanup abort lanes
  - closure evidence:
    - `src/lisp/eval_promotion_root_clones.c3` now exposes narrow abort-cleanup telemetry for partial method-table reclamation, and `src/lisp/jit_jit_closure_support.c3` now exposes a targeted heap-signature copy failure seam.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now drives both copy-to-parent and escape-promotion abort paths through that seam and proves partially copied heap signatures are reclaimed instead of leaked.
    - `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` now includes that regression in the bounded smoke lane.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=83 fail=0`

- [x] `AUDIT-ASYNC-PROCESS-CONCURRENCY-003` define and harden shared process-handle concurrency semantics
  - closure evidence:
    - `src/lisp/async_process_signal_runtime.c3` now gives each process handle a shared in-flight guard, and `src/lisp/async_process_lifecycle.c3` preserves the closed state while wait/kill activity is serialized through the same contract.
    - `src/lisp/async_process_signal_dns_process.c3` now fails closed with `io/process-handle-busy` when `process-wait` / `process-kill` reuse the same live handle concurrently.
    - focused regression coverage now lives in `src/lisp/tests_advanced_io_effect_ffi_scheduler_boundary.c3` and `src/lisp/tests_advanced_io_effect_ffi_groups.c3`.
    - validation:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp` -> pass

- [x] `HTTP-CRUD-CONCURRENT-WRITES-001` decide and harden spawned CRUD write semantics for one in-memory Deduce DB
  - closure evidence:
    - `examples/deduce_crud_server.omni` now guards CRUD mutation entrypoints with a shared atomic write gate, preserving keyed-write duplicate-id normalization while preventing spawned overlap from surfacing raw runtime `ERROR` payloads.
    - the shipped application contract is now explicit: overlapping spawned writes over one in-memory Deduce CRUD store may resolve as either one success plus `"crud write already in progress"` or one success plus `"item already exists"`, but never as an unnormalized runtime error and never with more than one persisted row for the shared id.
    - the spawned concurrency probe now runs in the focused `http-crud` slice instead of the broad `http` slice, so deterministic HTTP regressions stay stable while the concurrency lane remains exercised.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` -> `pass=29 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http-crud ./build/main --test-suite lisp` -> `pass=1 fail=0`

- [x] `AUDIT-VALIDATION-INTEGRITY-002` repair status/validation tooling contracts
  - closure evidence:
    - `scripts/check_status_consistency.sh` now accepts the current zero-item TODO wording instead of hard-requiring one historical sentinel string.
    - `scripts/run_validation_container.sh` now serializes bounded validation runs with a repo-local lock so overlapping host/container/ASAN jobs do not corrupt the shared build tree.
    - `scripts/run_validation_status_summary.sh` now treats missing required `OMNI_TEST_SUMMARY` telemetry as a validation failure instead of trusting exit status alone.
    - `scripts/c3c_limits.sh` now preserves quoted extra Docker args instead of re-splitting them unsafely.
    - `scripts/run_e2e.sh` and `scripts/check_e2e_baseline_policy.sh` now guard Stage 3 compile-source parity explicitly against entry-build drift.
    - validation:
      - `scripts/check_status_consistency.sh` -> pass
      - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity` -> pass
      - `bash -n scripts/check_status_consistency.sh scripts/run_validation_container.sh scripts/run_validation_status_summary.sh scripts/c3c_limits.sh scripts/run_e2e.sh scripts/check_e2e_baseline_policy.sh` -> pass

- [x] `AUDIT-BOUNDARY-SHARED-WRAPPER-003` re-audit shared-wrapper boundary aliasing after fail-closed propagation landed
  - closure evidence:
    - `src/lisp/eval_promotion_copy.c3`, `src/lisp/eval_promotion_copy_route_helpers.c3`, `src/lisp/eval_promotion_escape_leaf.c3`, and `src/lisp/eval_promotion_escape_structured.c3` no longer return disjoint `ARRAY` / `HASHMAP` / `SET` / `METHOD_TABLE` wrappers by pointer identity once fast reuse is declined; they now defensively clone those wrappers and recurse through nested payload edges.
    - `METHOD_TABLE` shared-wrapper clones now keep signature arrays heap-backed so the existing value destructor contract remains sound during scope teardown.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now covers both defensive clone behavior for shared wrappers and nested fail-closed boundary-copy behavior.
    - `src/lisp/tests_memory_lifetime_finalize_groups.c3` also stops reading detached-scope env memory after release, removing the finalize-lane UAF that surfaced while validating the integrated smoke suite.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=81 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=0:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=81 fail=0`

- [x] `AUDIT-COMPILER-PARITY-002` harden JIT multi-arg allocation failures and restore AOT/JIT parity
  - closure evidence:
    - `src/lisp/jit_jit_apply_multi_prims.c3` and `src/lisp/jit_jit_apply_multi_prims_tail.c3` now null-guard multi-arg argument-buffer allocation for primitive and method-table dispatch, with a focused test seam that fails closed instead of dereferencing `null`.
    - `src/lisp/parser_top_level_parse.c3` and `src/lisp/compiler_program_pipeline_helpers.c3` now fail closed on malformed trailing forms; they no longer return a silently truncated prefix program after parser error.
    - `src/lisp/aot.c3` now exposes shared AOT arg-list counting and arity-error helpers, and `src/lisp/compiler_code_emission_lambda_defs.c3` uses them so generated multi-arg lambdas reject under-application, preserve JIT-style over-application chaining through `aot::apply_multi(...)`, and reject malformed arg lists explicitly.
    - `src/lisp/compiler_native_call_compilation_flat_style.c3` now guards generated closure-capture allocation in flat expression lowering without emitting invalid raw `return` statements into non-`Value*` contexts.
    - focused regressions landed in:
      - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
      - `src/lisp/tests_compiler_core_groups.c3`
    - validation:
      - `c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp` -> `pass=35 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` -> `pass=193 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `HTTP-CRUD-DUPLICATE-POST-001` restore a deterministic green broad HTTP slice after the parser regressions landed
  - closure evidence:
    - `examples/deduce_crud_server.omni` no longer performs a check-then-insert race in `repo/create`; it now treats keyed `deduce 'fact!` as the atomic source of truth and maps `deduce/integrity-key-conflict` to the existing `"item already exists"` API result.
    - `src/lisp/tests_runtime_feature_http_groups.c3` now keeps deterministic broad-slice coverage for duplicate-id rejection using two POSTs with the same id but different payloads, rather than the order-sensitive spawned race probe.
    - the HTTP helper-method fiber smoke remains isolated from the long-lived group interpreter so the broad `http` slice no longer inherits unrelated state from earlier cases.
    - validation:
      - `c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` -> `pass=29 fail=0`

- [x] `AUDIT-BOUNDARY-FAILCLOSED-002` make nested boundary copy/promotion fail closed for opaque primitive payloads
  - closure evidence:
    - `src/lisp/eval_promotion_copy.c3`, `src/lisp/eval_promotion_copy_route_helpers.c3`, and `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now propagate `BoundaryCopyFault` through nested `CONS` / `PARTIAL_PRIM` / `ITERATOR` copy paths, so nested opaque primitive rejection aborts transitively instead of embedding null/error payloads into rebuilt wrappers.
    - `src/lisp/eval_promotion_escape_leaf.c3` and `src/lisp/eval_promotion_escape_structured.c3` now fail closed transitively through the same wrapper shapes during ESCAPE promotion.
    - primitive-copy rejection now validates opaque payload legality before allocating the destination wrapper where practical, removing the target-scope garbage-allocation path from rejected primitive copies.
    - focused regressions landed in `src/lisp/tests_memory_lifetime_boundary_groups.c3` and `src/lisp/tests_memory_lifetime_promotion_context_groups.c3`, and the smoke suite wires them through `src/lisp/tests_memory_lifetime_groups.c3`.
    - validation:
      - `c3c build` -> pass
      - `rm -rf build/obj/linux-x64 build/main && mkdir -p build/obj/linux-x64/tmp_c_compile && c3c build --sanitize=address` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=80 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-ASYNC-FFI-SAFETY-002` harden REPL worker lifecycle and handle-kind validation
  - closure evidence:
    - `src/lisp/eval_repl_server_worker.c3` now refuses to start the REPL worker thread unless both mutex and condition-variable initialization succeeded, so partial-init paths fail closed before the worker can touch uninitialized sync state.
    - `src/lisp/async_socket_handle_runtime.c3`, `src/lisp/tls_handle_lifecycle.c3`, and `src/lisp/prim_io_fs_stream.c3` now validate exact FFI handle names before casting TCP/UDP/TLS/FS payloads, closing the type-confusion path where unrelated `FFI_HANDLE` boxes could be reinterpreted as transport state.
    - `src/lisp/async_process_spawn.c3` now treats constructor-returned error values from `make_process_handle(...)` and `make_fs_handle(...)` as hard failures instead of packaging them into a success-shaped spawn result.
    - `src/lisp/http_url_response.c3` now rejects malformed `:port` suffixes with trailing garbage or missing digits, and trims HTTP response header slices so the exposed header string no longer retains delimiter residue.
    - focused regressions landed in:
      - `src/lisp/tests_runtime_async_repl_server_groups.c3`
      - `src/lisp/tests_advanced_io_effect_ffi_groups.c3`
      - `src/lisp/tests_runtime_feature_http_groups.c3`
    - validation:
      - `c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp` -> `pass=61 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system ./build/main --test-suite lisp` -> `pass=42 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_VERBOSE=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` now passes the new parser regression coverage but still reports one pre-existing unrelated failure:
        - `crud pipeline duplicate-post race (error: unexpected token in expression)`
        - tracked separately as `HTTP-CRUD-DUPLICATE-POST-001`

- [x] `REF-SPLIT-INFRA-002` continue runtime large-file decomposition (largest-first, ownership-preserving)
  - closure evidence:
    - completed largest-first structural splits in the remaining targeted runtime files:
      - `src/lisp/eval_run_pipeline.c3` (`274 -> 164`) with `src/lisp/eval_run_pipeline_helpers.c3` (`117`)
      - `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates.c3` (`275 -> 64`) with `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates_impl.c3` (`236`)
    - preserved runtime contracts by keeping coordinator entrypoints and moving internal helpers only.
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=schema OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=42 fail=0`

- [x] `REF-SPLIT-TESTS-002` split oversized test files by existing feature/group seams
  - closure evidence:
    - completed largest-first splits for all oversized files listed in this lane:
      - `src/lisp/tests_deduce_rule_groups_more_tail.c3` (`1902 -> 310`) + extracted seam files
      - `src/lisp/tests_deduce_query_bench_groups.c3` (`1684 -> 117`) + extracted seam files
      - `src/lisp/tests_deduce_rule_groups_explain.c3` (`1471 -> 54`) + extracted seam files
      - `src/lisp/tests_deduce_durability_groups.c3` (`1403 -> 493`) + extracted seam files
    - preserved test harness behavior by keeping coordinator runners and existing test names/filters.
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=324 fail=0`

- [x] `ERR-MODEL-STRICT-001` reduce `!!` usage in non-test runtime/compiler paths per `docs/C3_STYLE.md`
  - closure evidence:
    - removed the remaining non-test `!!` sites from runtime/compiler paths, including:
      - `src/lisp/parser_callable_helpers_params.c3`
      - `src/lisp/jit_jit_compile_let_set_helpers.c3`
      - `src/lisp/aot_runtime_bridge_helpers.c3`
      - `src/lisp/eval_dispatch_types.c3`
      - `src/lisp/prim_ui_ftxui_helpers.c3`
      - `src/lisp/primitives_meta_types_ctor.c3`
      - `src/lisp/compiler_temp_type_forms_defs_misc.c3`
      - `src/lisp/async_tcp_transport_helpers.c3`
      - `src/lisp/eval_type_evaluators.c3`
    - repo re-audit now shows zero non-test hits:
      - `rg -n "\)!!|!!;|!!," src/lisp --glob '!**/tests*'` -> no matches
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=189 fail=0`
      - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=60 fail=0`


- [x] `AUDIT-PARSER-SAFETY-001` fix parser progress/error defects and parser-surface drift
  - closure evidence:
    - lexer/parser fail-closed fixes landed in:
      - `src/lisp/parser_lexer_token_scanners_dot.c3`
      - `src/lisp/parser_lexer_whitespace.c3`
      - `src/lisp/parser_lexer.c3`
      - `src/lisp/parser_lexer_core_api.c3`
      - `src/lisp/parser_top_level_parse.c3`
      - `src/lisp/eval_run_pipeline.c3`
    - Pika/core surface parity fixes landed in:
      - `src/pika/lisp_grammar_build.c3`
      - `src/pika/lisp_grammar_scanners.c3`
    - targeted regressions added in:
      - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
      - `src/lisp/tests_runtime_feature_schema_reader_groups.c3`
      - `src/lisp/tests_runtime_feature_pika_groups.c3`
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=183 fail=0`
      - `OMNI_LISP_TEST_SLICE=reader-dispatch OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=12 fail=0`
      - `OMNI_LISP_TEST_SLICE=schema OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=42 fail=0`
      - `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=78 fail=0`

- [x] `VCS-JJ-CHECKOUT-ACL-001` restore `jj` checkout-state readability and bookmark workflow
  - closure evidence:
    - recovered broken checkout metadata non-destructively by rebuilding `.jj/working_copy` from readable repository state and preserving backups:
      - `.jj/working_copy.recover.bak.20260409-022334`
      - `.jj/repo/op_heads/heads.recover.bak.20260409-022441`
      - `.jj/repo/store/extra/heads.recover.bak.20260409-022625`
      - `.jj/repo/index.recover.bak.20260409-022536`
    - restored command health:
      - `jj status` -> exit `0`
      - `jj bookmark list` -> exit `0`
    - corrected bookmark workflow for current `jj 0.39.0` surface:
      - create/update by name: `jj bookmark set <name> -r <revset>`
      - move existing bookmarks: `jj bookmark move <name> --to <revset>`
      - advance closest bookmarks: `jj bookmark advance --to <revset>`
    - note:
      - `jj bookmark update` is not a valid subcommand in this CLI version.

- [x] `AUDIT-DEDUCE-SLICE-RED-001` investigate and reduce current deduce-lane failures
  - closure evidence:
    - deduce lane is now green end-to-end:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=378 fail=0`
    - repaired high-signal root-cause clusters in materialization metadata, selector disjunctive-demand fallback, and why-result payload shape alignment:
      - `src/lisp/deduce_schema_query_metadata_schema_helpers.c3`
      - `src/lisp/deduce_schema_query_execution_goal_directed_selector_disjunction_projected.c3`
      - `src/lisp/deduce_why_result_payload.c3`
      - `src/lisp/deduce_why_result_path_payload.c3`
    - targeted admin-surface lane is also green after fixes:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query OMNI_DEDUCE_QUERY_FILTER=admin-surface OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=184 fail=0`

- [x] `AUDIT-ADVANCED-SLICE-RED-001` restore advanced-lane green without relaxing strict contracts
  - closure evidence:
    - fixed parser macro-pattern zero-length allocation bug that broke `syntax-match` empty-sequence clauses (`[]`) and silently prevented stdlib/test macro definitions:
      - `src/lisp/parser_patterns_values.c3`
      - avoided zero-byte AST arena allocations for empty pattern buffers in sequence/dict pattern parsing.
    - stdlib macro/predicate lane restored:
      - `stdlib/stdlib.lisp`
      - `branch` macro now loads during bootstrap again.
      - `boolean?` contract aligned to strict boolean values (`true`/`false`) while preserving existing truthiness semantics (`nil`/`false` falsy).
    - strict higher-order arity lane restored:
      - `src/lisp/primitives_meta_predicates.c3`
      - `src/lisp/eval_init_primitive_tables.c3`
      - `stdlib/stdlib.lisp`
      - introduced primitive `error?` and used it in stdlib `map` to propagate callback runtime errors instead of returning lists of embedded `ERROR` values.
    - validation:
      - `OMNI_LISP_TEST_SLICE=advanced OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=1156 fail=0`
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=182 fail=0`

- [x] `META-STATS-001` remove duplicate SCC/parallel topology planner work from `deduce/stats`
  - closure evidence:
    - removed dead duplicate topology-planning wrapper that rebuilt SCC + batch metadata independently of the stats path:
      - deleted `deduce_parallel_batch_topology_counts(...)` from `src/lisp/deduce_schema_query_metadata_parallel_topology.c3`
    - the live stats path remains on a single metadata-build lane:
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
      - `src/lisp/deduce_schema_query_metadata_parallel_topology.c3`
    - no residual callsites remain for the deleted duplicate planner wrapper:
      - `rg -n "deduce_parallel_batch_topology_counts\\(" src/lisp` -> no matches
    - integration safety:
      - `c3c build` passes and links `build/main`
  - validation note:
    - the broader `OMNI_LISP_TEST_SLICE=deduce` lane is currently green in this workspace:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=324 fail=0`

- [x] `REF-SPLIT-INFRA-REPL-001` split REPL server runtime surfaces by ownership lane
  - closure evidence:
    - split `src/lisp/eval_repl_server.c3` top-down into focused runtime surfaces without behavioral contract changes:
      - request auth/dispatch lane extracted to `src/lisp/eval_repl_server_request.c3`
      - unix/tcp listener lane extracted to `src/lisp/eval_repl_server_listeners.c3`
      - stream/session orchestration retained in `src/lisp/eval_repl_server.c3`
    - primary runtime file was reduced from `332` lines to `67` lines:
      - `wc -l src/lisp/eval_repl_server.c3 src/lisp/eval_repl_server_request.c3 src/lisp/eval_repl_server_listeners.c3`
    - integration + async REPL validation remain green after the split:
      - `c3c build`
      - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=59 fail=0`

- [x] `AUDIT-BINDGEN-DEFERRED-001` resolve generated bindgen TODO ownership/teardown policy gaps
  - closure evidence:
    - bindgen wrappers now enforce concrete ownership/role/teardown guards and fail closed where policy is unresolved:
      - `src/lisp/bindgen.c3`
      - added explicit raise paths:
        - `bindgen/opaque-arg-role-mismatch`
        - `bindgen/opaque-arg-ownership-mismatch`
        - `bindgen/opaque-arg-unsupported-teardown`
        - `bindgen/opaque-arg-nil`
        - `bindgen/string-return-unknown-ownership`
        - `bindgen/opaque-return-manual-review`
    - staged comment marker switched from `TODO(bindgen)` to `REVIEW(bindgen)`:
      - `src/lisp/bindgen.c3`
      - `src/lisp/tests_compiler_codegen_groups_tail.c3`
    - compiler bindgen coverage remains green:
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=189 fail=0`

- [x] `AUDIT-DEDUCE-WHY-DERIVED-001` implement missing derived-subject why-result support
  - closure evidence:
    - removed explicit unsupported derived-subject error path from why-result lookup:
      - `src/lisp/deduce_why_result_lookup_derived.c3`
      - `rg -n "why-result-derived-subject-not-yet-supported|not-yet-supported" src/lisp/deduce_why_result_* src/lisp/deduce_*` returns no matches
    - derived why-result now returns structured provenance payloads instead of a legacy unsupported error for supported read shapes:
      - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block ... (deduce/query reach (lambda (row) (= (ref row 'src) 8))) (deduce/why-result reach 8 10))"`
      - observed payload includes:
        - `kind why-result`
        - `path-kind derived`
        - `status partial`
        - `paths` with structured support frames

- [x] `AUDIT-BUILD-IMPORT-001` restore green build by fixing invalid allocator import paths
  - closure evidence:
    - updated allocator import/module references in:
      - `src/lisp/scheduler_thread_tasks.c3`
      - `src/lisp/eval_repl_server_worker.c3`
    - `c3c build` now completes and links `build/main`.

- [x] `AUDIT-REPL-SECURITY-001` lock down unauthenticated remote REPL execution surfaces
  - closure evidence:
    - TCP REPL now enforces loopback bind (`localhost`/`127.0.0.1`/`::1`) and requires `OMNI_REPL_TCP_AUTH_TOKEN` at startup:
      - `src/lisp/eval_repl_server.c3`
    - per-request authorization gate added for non-`describe` operations with `auth` token matching:
      - `src/lisp/eval_repl_server.c3`
      - `src/lisp/eval_repl_server_protocol.c3`
      - `src/lisp/eval_repl_server_protocol_parse.c3`
      - `src/lisp/eval_repl_server_state.c3`
      - `src/lisp/eval_repl_server_output.c3`
    - regression coverage added:
      - `src/lisp/tests_runtime_async_repl_server_groups.c3`
    - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=59 fail=0`.

- [x] `AUDIT-DEDUCE-PARALLEL-RUNTIME-001` replace metadata-only parallel mode with truthful runtime state
  - closure evidence:
    - added shared runtime-truth field helper with explicit serial runtime counters:
      - `src/lisp/deduce_parallel_runtime_truth.c3`
    - updated analyze/explain/stats payloads to emit truthful runtime mode/counters:
      - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`
      - `src/lisp/deduce_rule_ops_explain_snapshot.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
    - updated assertions:
      - `src/lisp/tests_deduce_query_admin_surface_tail.c3`
      - `src/lisp/tests_deduce_rule_groups_explain.c3`
    - targeted eval checks for analyze/stats runtime mode now return `true`.

- [x] `AUDIT-DEDUCE-NAIVE-FALLBACK-001` reduce recursive component fallback to naive execution
  - closure evidence:
    - seminaive recursive aggregate path is now selected directly when seminaive recursive mode is enabled:
      - `src/lisp/deduce_rule_eval_fixpoint_component_eval_non_txn.c3`
      - `src/lisp/deduce_rule_eval_fixpoint_component_eval.c3`
    - analyze/explain runtime-truth payloads remain aligned with serial execution counters.

- [x] `AUDIT-NET-IPv6-001` remove IPv4-only DNS resolution limitation
  - closure evidence:
    - DNS address rendering now supports both `AF_INET` and `AF_INET6` and reports unsupported families explicitly:
      - `src/lisp/async_tcp_transport_helpers.c3`
      - `src/lisp/async_runtime_base.c3`
      - `src/lisp/async_process_signal_dns.c3`
    - deterministic coverage added for IPv4 + IPv6 addrinfo rendering:
      - `src/lisp/tests_runtime_async_io_tls_groups.c3`
    - `OMNI_LISP_TEST_SLICE=async ... --test-suite lisp` remains green (`pass=59 fail=0`).

- [x] `STACK-AARCH64-CONT-001` arm64 language-level continuation multi-shot parity
  - closure evidence:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` -> `Stack engine: 23 passed, 0 failed`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (with-continuation k (k 41))))"` -> `42`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(checkpoint (+ 1 (capture k (k 10))))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block (define replay-set-counter 0) (define replay-set-r (checkpoint (+ (capture k (+ (k 1) (k 1))) (block (set! replay-set-counter (+ replay-set-counter 1)) replay-set-counter)))) (+ (* 10 replay-set-r) replay-set-counter))"` -> `52`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=56 fail=0`
    - `OMNI_JIT_POLICY_FILTER=multishot-capture-scope-guard-clone OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=1 fail=0`
  - implementation note:
    - arm64 GNU lightning register IDs were corrected in `src/lisp/jit_lightning_constants.c3`.
    - effect fast-path primitive dispatch now preserves primitive error payloads and supports dotted cons payloads for fixed-arity wrappers in:
      - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
      - `src/lisp/jit_jit_runtime_effects_signal.c3`
- [x] `AUDIT-AOT-PRINT-DEDUCE-SENTINELS-059` close AOT/type-spec, print, and deduce persistence soundness defects
  - closure evidence:
    - AOT generated type/type-spec surfaces now reject `INVALID_SYMBOL_ID`
      before constructing type definitions, aliases, effects, method
      signatures, match helpers, dictionary symbol lookup, and effect explain
      payloads:
      - `src/lisp/aot.c3`
      - `src/lisp/aot_runtime_bridge.c3`
      - `src/lisp/aot_runtime_bridge_helpers.c3`
      - `src/lisp/aot_runtime_match_helpers.c3`
      - `src/lisp/aot_type_definitions.c3`
      - `src/lisp/aot_type_spec_helpers.c3`
    - AOT compiled list helpers now reject negative `long` indexes before
      converting to `usz`.
    - direct and buffered value printers now tolerate nullable dictionary/set
      backing storage, and `print_value_to_buf` now rejects null/zero-capacity
      buffers before writing.
    - constructor type constraint diagnostics now use guarded type-registry
      lookups, and instance type inference rejects invalid type IDs.
    - deduce tuple persistence now stores full 32-bit `SymbolId` values and
      rejects invalid/out-of-range decoded symbols.
    - deduce materialized metadata delete now distinguishes missing metadata DBI
      from real DBI-open errors.
    - deduce DBI name/path copy helpers now use checked addition before
      allocation.
    - deduce relation/rule install failure paths now roll back newly appended
      in-memory schemas/rule signatures when later fallible persistence or
      handle-publication steps fail.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `compiler`: green (`pass=191 fail=0`)
    - bounded `deduce`: green (`pass=330 fail=0`)
    - bounded `advanced`: green (`pass=1183 fail=0`)
    - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)

- [x] `AUDIT-PARSER-JIT-ASYNC-BOUNDARY-060` close parser/compiler, JIT boundary, macro splice, and async/TLS soundness defects
  - closure evidence:
    - parser language-surface interning now fails closed through a parser-local
      checked helper before publishing symbols into import paths, path
      segments, type annotations, collection constructors, explain selectors,
      relation definition rewrites, quasiquote/template underscores, and
      special-form comparisons.
    - compiler lambda effect-wrapper scanning now uses checked AST arena
      allocation and invalid-symbol guards before publishing synthetic wrapper
      bodies or handler bodies.
    - primitive variable hash bootstrap now rejects `INVALID_SYMBOL_ID` keys
      and reports initialization failure instead of treating the sentinel as an
      empty slot.
    - compiler integer emission now avoids `long.min` negation and avoids
      `usz -> long` narrowing for unsigned decimal output.
    - macro splice append now rejects improper splice tails and recursion-limit
      exhaustion instead of silently truncating to the rest/nil.
    - runtime boundary string/error copying now guards `len + 1` allocation
      arithmetic, and boundary policy integer parsing now rejects overflow.
    - JIT resolve/continuation yield-failure paths restore saved interpreter
      state before returning; pending raise handler staging now clears
      `raise_pending` only after payload/env/list construction succeeds.
    - runtime handle entry points now reject null tags/closures arrays when
      `count > 0`.
    - TLS offload yield-error paths now close pending offload state before
      returning, TCP/UDP ports and signal numbers are range-checked before
      narrowing to `int`, and file-read close failure now fails the read.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `compiler`: green (`pass=191 fail=0`)
    - bounded `async`: green (`pass=61 fail=0`)
    - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
    - bounded `advanced` macro hygiene group: green (`pass=82 fail=0`)

- [x] `AUDIT-SCHEMA-LIFETIME-WIDTH-061` close schema/deduce payload and lifetime-width soundness defects
  - closure evidence:
    - schema explain payload setters now reject failed key/value symbol
      interning before publishing dictionary keys or symbol payload values.
    - JSON pointer symbol-key fallback now treats failed token interning as a
      miss instead of constructing an invalid symbol key.
    - filesystem/process payload helpers now check result-key interning and
      file-size/handle-key bounds before constructing keys or integer payloads.
    - user-facing `exit`, `TimePoint`, Unicode codepoint predicates, `fs-open`,
      `tcp-listen`, and zlib original-size paths now validate integer ranges
      before narrowing to C `int`/`usz`/external API widths.
    - deduce materialize now rejects failed `"manual"` policy interning before
      persisting relation/schema state.
    - deduce integrity payload construction now propagates actual allocation/
      intern/set errors instead of collapsing them to `null`, and list payload
      builders now stop on cons allocation errors.
    - materialized-stale and integrity-violation payload symbols now reject
      failed interning before `make_symbol`.
    - primitive name matching now rejects null primitive backing pointers and
      overlong expected names before reading the fixed primitive name buffer.
    - checked array construction now allocates backing payloads before
      publishing the root wrapper, mirroring the hashmap constructor pattern.
    - closure escape promotion now releases any retained/detached env scope if
      final wrapper allocation fails.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `data-format`: green (`pass=64 fail=0`)
    - bounded normal+ASAN `unicode`: green (`pass=27 fail=0`)
    - bounded normal+ASAN `compression`: green (`pass=27 fail=0`)
    - bounded normal+ASAN `async`: green (`pass=65 fail=0`)
    - bounded normal+ASAN `compiler`: green (`pass=194 fail=0`)
    - bounded normal+ASAN `memory-lifetime-smoke`: green (`pass=190 fail=0`)
    - bounded normal `advanced`: green (`pass=1185 fail=0`)
    - bounded ASAN `advanced`: green (`pass=1172 fail=0`)
    - bounded normal+ASAN `deduce`: green (`pass=330 fail=0`)

- [x] `AUDIT-FTXUI-SMOKE-SEGFAULT-062` investigate FTXUI smoke crash outside the Lisp slice set
  - observed during bounded validation after all targeted Lisp slices had
    passed:
    - `scripts/run_ftxui_smoke.sh` `smoke.omni` exited with SIGSEGV.
  - closed:
    - root cause was a runtime lifetime-boundary provenance/reuse walk over a
      nested effect payload graph, not the FTXUI lowering path.
    - `src/lisp/eval_boundary_provenance.c3` now uses a bounded iterative
      alias-safety worklist with visited tracking for nested arrays, dicts,
      sets, method tables, partials, iterators, and cons payloads, avoiding the
      recursive stack overflow that surfaced through FTXUI `ui.graph` payloads.
    - `src/lisp/eval_boundary_provenance.c3` now skips scalar leaves before
      consuming alias worklist/visited capacity, so wide scalar-only payloads do
      not fail closed as if they were unsafe graph overflows.
    - `src/lisp/prim_ui_ftxui.c3` now checks child component count arithmetic
      before allocating the FTXUI child pointer array.
    - `src/lisp/prim_ui_ftxui_helpers.c3` now guards helper-array capacity
      growth and graph-series allocation size math before `realloc`/`malloc`.
    - `src/lisp/prim_ui_ftxui_lowering.c3` now rejects menu item counts that
      cannot be represented by the FTXUI `int` selected-index state.
    - `csrc/ftxui_shim.cpp` now declares `keep_alive` before `component`, so
      component teardown runs before retained borrowed backing data is released.
    - `csrc/ftxui_shim.cpp` now rejects nonzero child counts with null child
      arrays, checks table `rows * cols` overflow before comparing against the
      child count, and rejects table row/column selectors that cannot fit in
      FTXUI `int` APIs.
    - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` adds a minimal
      nested effect-payload regression matching the failing view shape plus a
      wide scalar payload regression, and
      `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` keeps both in the
      bounded smoke lane.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      green (`pass=192 fail=0`)
