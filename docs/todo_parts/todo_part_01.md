# Active TODO Index Part 01

Source: `TODO.md`

# Active TODO

Last condensed: 2026-04-12

This file is now the sole live backlog.
The Live Queue lists only still-open work.

Current live parent count: 0
Live blocker queue is closed as of 2026-04-22.

Completed backlog snapshots:

- `docs/TODO-backup-2026-03-26.md`
- `docs/TODO-backup-2026-03-31.md`
- `docs/TODO-backup-2026-04-01.md`
- `docs/TODO-backup-2026-04-08.md`

Recently Closed is retained below as a short audit trail.

## Live Queue

- none currently; live queue closed as of 2026-04-22.

## Recently Closed

- [x] `TENSOR-100H-VK-GENERAL-EIGEN-DEFLATION-001` implement active-submatrix
  deflation for mixed-block Vulkan general eigen spectra.
  - classification: Runtime / Targeted.
  - task: extended the native Vulkan general eigensolver to deflate converged
    trailing scalar blocks and continue QR on the remaining active leading
    submatrix, so mixed spectra such as a 2x2 rotation block plus a trailing
    real scalar no longer fail closed.
  - shipped: added trailing scalar deflation before the full-matrix QR loop,
    leading active 2x2 analytic solve through the accumulated basis, scratch
    output staging to preserve basis columns while reconstructing vectors,
    regenerated SPIR-V, and added mixed-block residual regressions/probes for
    `Float64`, `Float32`, `Complex128`, and `Complex64`.
  - validation: `glslangValidator`/`spirv-val`, `scripts/build_omni_chelpers.sh`,
    `c3c build --obj-out obj`, and direct mixed-block residual probes for all
    four Vulkan general eigen dtypes returned `true`.
  - negative-memory closure: the previous shift-perturbation-only approach is
    still invalidated; active-submatrix deflation is the shipped fix.
  - references: `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`,
    `csrc/tensor_vulkan_general_eigen_complex128.comp`,
    `csrc/tensor_vulkan_general_eigen_f64.comp`.

- [x] `TENSOR-100H-VK-REAL-GENERAL-EIGEN-001` implement native Vulkan
  real-valued general `matrix/eigenpairs` for dense row-major `Float64` and
  `Float32`.
  - classification: Runtime / Targeted.
  - shipped: added checked-in real general Vulkan shaders/SPIR-V for `Float64`
    and `Float32`, helper ABI declarations/build-manifest wiring, public
    `matrix/eigenpairs` routing to backend-native complex-valued output tensors,
    availability bits, real diagonal/lazy/no-LAPACK tests, and exact 2x2
    complex-shift residual coverage.
  - validation: `glslangValidator`/`spirv-val` during SPIR-V regeneration,
    `scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`, direct
    Float64/Float32 exact-shift residual probes, and focused advanced
    collections improved to `pass=2004 fail=17` with only unrelated ML/Vulkan
    contract failures remaining.
  - references: `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`,
    `src/lisp/prim_tensor_matrix_eigen_primitives.c3`,
    `csrc/tensor_vulkan_helpers_matrix_ops_eigen_complex.c`.
- [x] `TENSOR-100H-VK-COMPLEX-GENERAL-EIGEN-HARDENING-001` measure and harden
  the shipped Vulkan fixed-width complex general eigensolver for larger or
  difficult spectra.
  - classification: Runtime / Targeted.
  - shipped: added shift regularization plus direct 2x2 analytic eigenpair
    handling for exact complex-shift cases in `Complex128`/`Complex64` shaders,
    added exact-shift residual tests, and extended
    `scripts/run_vulkan_math_perf_probe.sh` with named general eigenpair
    fixtures for real and complex Vulkan routes.
  - validation: direct `Complex128`/`Complex64` exact-shift residual probes,
    `scripts/run_vulkan_math_perf_probe.sh` completed the new general
    eigenpair fixtures, and focused advanced collections no longer report
    eigenpair failures.
  - promoted residual: `TENSOR-100H-VK-GENERAL-EIGEN-DEFLATION-001` tracks the
    separately discovered active-submatrix deflation gap.
  - references: `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`,
  `scripts/run_vulkan_math_perf_probe.sh`,
    `csrc/tensor_vulkan_general_eigen_complex128.comp`,
    `csrc/tensor_vulkan_general_eigen_complex64.comp`.

- [x] `OWNERSHIP-HARDENING-001` codify and harden the completed
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
    - [x] Add a concise accepted memory-model ADR to `docs/ARCHITECTURE.md`.
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
    - [x] Add an ownership-boundary checklist to `docs/C3_STYLE.md`.
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
    - [x] Expose or print first-class memory boundary statistics for debug
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
    - [x] Codify the durable-graph rule: temporary graphs may be cyclic;
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
    - [x] Prefer handles/IDs at durable subsystem API boundaries and document
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
    - [x] Require boundary regression tests for every new graph-carrying
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
    - [x] Keep static allocation routing as an optimization layer, not the
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
    - [x] Normalize boundary vocabulary in docs without broad symbol churn.
      - blocker/defer reason: names such as copy, promotion, destination,
        escape, commit, and splice still carry historical baggage even though
        the implemented model is coherent.
      - concrete next step: add a glossary covering boundary commit,
        destination ESCAPE build, promotion, splice, fallback copy, reusable
        target-chain value, releasing TEMP, releasing ESCAPE, and
        mixed/uncertain provenance.
      - validation: docs `git diff --check`; avoid renaming code symbols unless
        a separate implementation task proves it is worth the churn.
    - [x] Make opaque-payload ownership policy explicit.
      - blocker/defer reason: opaque wrappers are safe only when they either
        own no Omni values, expose explicit traversal/copy/promotion/destructor
        hooks, or fail closed at boundary promotion.
      - concrete next step: document the policy and audit existing opaque
        payload families (`FFI_HANDLE`, backend/device handles, primitive
        payloads, and Tensor backend payloads) against it.
      - validation: docs `git diff --check`; for any audit fixes, focused
        boundary/root-store/promotion tests plus `c3c build`.
    - [x] Add ownership-model examples for common runtime shapes.
      - blocker/defer reason: abstract invariants are present, but future
        implementation work would benefit from concrete examples showing where
        allocation happens and what boundary commit must prove.
      - concrete next step: document examples for returning a list, returning
        an iterator, closure capture, root/global define, Tensor lazy
        expression, FFI handle, opaque foreign resource, mutable collection
        update, and failure during ESCAPE publication.
      - validation: docs `git diff --check`; link examples from the new ADR or
        `docs/C3_STYLE.md` checklist.
  - completed 2026-04-21:
    - Added accepted ADR `ADR-2026-04-21-A: Scope/Region Ownership Contract` to
      `docs/ARCHITECTURE.md`.
    - Added the ownership-boundary checklist, durable graph/handle policy, and
      static allocation-routing rule to `docs/C3_STYLE.md`.
    - ADR covers `ScopeRegion` as owner, TEMP/ESCAPE lanes, the committed
      ESCAPE no-TEMP-edge invariant, shared boundary promotion context,
      retired `scope_adopt` return flow, ordinary-value no-per-type-RC policy,
      opaque payload exceptions, durable graph policy, handle/ID policy,
      static allocation as optimization, glossary, and examples.
    - Stats visibility is satisfied by the landed `runtime-memory-stats`
      primitive and `OMNI_MEM_TELEMETRY=1` teardown JSON from the memory
      telemetry wave.
  - validation: `git diff --check -- docs/ARCHITECTURE.md docs/C3_STYLE.md
    docs/todo_parts/todo_part_01.md TODO.md`; no runtime semantics changed.
- [x] `TENSOR-100F` continue the Vulkan math library baseline
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
  - closed 2026-04-22: all `TENSOR-100F` Vulkan math baseline slices are now
    either shipped, measured closed, or promoted to a separately named residual.
    The only remaining backend-native complex eigensolver work is tracked as
    `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL`, not as another open
    `TENSOR-100F` child.
  - closure validation:
    - `scripts/run_validation_container.sh scripts/run_global_gates.sh` passed
      the file-size gate, normal build, normal lisp slices, compiler slice,
      and FTXUI smokes.
    - ASAN build was explicitly skipped by the global gate because the current
      C3 toolchain reports `Address sanitizer is only supported on Linux,
      FreeBSD, NetBSD, Darwin and Windows.`
    - The broad run surfaced and fixed `ast_arena_alloc` corrupt-current-chunk
      fail-closed behavior and JIT one-argument `List` / `Array` constructor
      shortcut semantics before this parent was closed.
  - historical and promoted TODOs tracked from this parent:
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
    - [x] Continue largest-first source splitting from
      `src/lisp/prim_tensor_matrix.c3`.
      - closed 2026-04-22 as a stale residual: the matrix primitive family has
        already been split into `src/lisp/prim_tensor_matrix*.c3` files and no
        current matrix split target exceeds the 1000 LOC owner gate.
      - current measured counts: `src/lisp/prim_tensor_matrix.c3` is 173 LOC,
        `src/lisp/prim_tensor_matrix_lu_tensor_ops.c3` is 665 LOC, and
        `csrc/tensor_vulkan_helpers.c` is 265 LOC.
      - preserved contract: no runtime semantics changed; the remaining
        `TENSOR-100F` open items are runtime/performance Vulkan math work, not
        source-splitting work.
      - validation: repository code-file count scan and
        `scripts/check_file_size_gate.sh` show no tracked code files above the
        1000 LOC gate.
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
    - [x] `TENSOR-100F-VK-SVD-LARGE-K-PERF-001` measure and optimize the storage-backed large-`k` Vulkan
      `matrix/singular-values` / `matrix/svd` path with tiled or
      multi-dispatch execution if runtime measurements justify it.
      - closed 2026-04-22: the semantic `k > 64` cap removal now uses
        hidden storage-buffer Gram scratch behind the public Vulkan outputs,
        and current in-process measurements do not justify replacing the
        existing correctness-first serial Jacobi execution shape.
      - measurement checkpoint 2026-04-22:
        `docs/plans/vulkan-math-performance-measurements-2026-04-22.md`
        records the checked-in probe script and current host results. The
        default plus scale probe covers 64/65 fixtures, 96/128/192 `Float64`
        identity SVD, and 128x128 all-ones SVD; measured operation times stayed
        in the 310-615 ms range.
      - future reopen rule: open a new item, not this closed residual, if
        repeated measurements at a named larger size show the serial
        Jacobi/storage-scratch shape is the bottleneck. Any new rewrite must
        preserve `Float64`/`Float32` semantics, Vulkan result placement, and no
        hidden LAPACK/CPU fallback.
      - validation: shader compile/`spirv-val`, helper rebuild,
        `c3c build --obj-out obj`, reconstruction/value checks,
        no-`LAPACKE_dgesvd` counter checks, focused host plus
        bounded-container advanced collections tests.
    - [x] `TENSOR-100F-VK-EIGEN-LARGE-N-PERF-001` measure and optimize the storage-backed large-`n` Vulkan symmetric
      `matrix/eigenvalues` / `matrix/eigenvectors` path with tiled or staged
      execution if runtime measurements justify it.
      - closed 2026-04-22: the semantic `n > 64` cap removal now uses
        storage-buffer scratch behind the public Vulkan outputs, but it keeps
        the correctness-first Jacobi iteration shape because current
        in-process measurements do not justify a tiled or staged rewrite.
      - measurement checkpoint 2026-04-22:
        `docs/plans/vulkan-math-performance-measurements-2026-04-22.md`
        records the checked-in probe script and current host results. The
        default plus scale probe covers 64/65 fixtures and 128x128 `Float64`
        identity/all-ones eigenvalues; measured operation times stayed in the
        304-494 ms range.
      - future reopen rule: open a new item, not this closed residual, if
        repeated measurements at a named larger size show the current serial
        Jacobi/storage-scratch shape is the dominant cost. Any new rewrite must
        preserve `tensor/not-symmetric`, `tensor/no-convergence`, Vulkan output
        placement, and no hidden `LAPACKE_dsyev` fallback.
      - validation: shader compile/`spirv-val`, helper rebuild,
        `c3c build --obj-out obj`, residual/vector-placement checks,
        no-`LAPACKE_dsyev` counter checks, focused host plus
        bounded-container advanced collections tests.
    - [x] Retire stale direct Vulkan general `matrix/eigenpairs` umbrella.
      - closed/superseded: fixed-width `matrix/eigenpairs` now returns
        `Complex128` or `Complex64` result tensors on CPU, so the old
        pointer-backed `BigComplex` blocker no longer describes the active
        contract.
      - remaining work is tracked by
        `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL`, which requires a
        backend-native non-Hermitian complex eigensolver and still forbids
        hidden CPU/LAPACK fallback.
      - validation: focused Vulkan fixed-complex fail-closed tests now assert
        `zgeev`/`cgeev` counters are unchanged for Vulkan `Complex128` /
        `Complex64` `matrix/eigenpairs`.
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
    - [x] `TENSOR-100H-SVD-FACTORS-CPU` add CPU fixed-width complex
      `matrix/svd` factor output.
      - closed: CPU `matrix/svd` accepts `Complex128` and `Complex64`.
        `Complex128` returns Complex128 `u`/`v` and Float64 `s`; `Complex64`
        returns Complex64 `u`/`v` and Float32 `s`. The implementation uses a
        native complex Hermitian Gram/eigenvector path for public factors
        rather than realified `U`/`V`; Complex64 is computed through the
        Complex128 oracle and narrowed at the output boundary.
      - validation: `c3c build --obj-out obj`; focused host
        `advanced-collections-module` passed `1937/0`; code-file LOC gate was
        checked before the test run.
    - [x] `TENSOR-100H-SVD-FACTORS-VULKAN` add Vulkan fixed-width complex
      `matrix/svd` factor output.
      - closed: Vulkan dense row-major fixed-width complex `matrix/svd` now
        has native Complex128/Complex64 factor shaders and helper ABI wiring.
        Complex inputs return complex `u`/`v` tensors on Vulkan and component
        real `s` tensors (`Float64`/`Float32`) without CPU/LAPACK fallback.
      - validation: `glslangValidator` shader compilation and `spirv-val`
        validation via generated SPIR-V refresh; `c3c build --obj-out obj`;
        focused host
        `advanced-collections-module` passed `1941/0`; build-config parity,
        code-file LOC gate, and `git diff --check` passed.
    - [x] `TENSOR-100H-CUDA-SVD-NORMS-LOADER` add runtime-loaded CUDA
      cuSOLVER SVD discovery.
      - closed: CUDA now has dynamic cuSOLVER DN resolution for
        `cusolverDnZgesvd` and `cusolverDnCgesvd`, availability probes,
        a disable-for-tests hook, and `gesvd` call counters. `tensor-backends`
        exposes `cusolver` and `cusolver-complex-svd` loader booleans plus
        operation-specific `matrix-singular-values-complex128`,
        `matrix-singular-values-complex64`, `matrix-svd-complex128`, and
        `matrix-svd-complex64` fields.
      - shipped contract: operation-specific CUDA complex SVD fields remain
        `false` until the layout adapters and execution routes land; the
        loader does not make cuSOLVER a link-time dependency and does not
        claim broad CUDA complex numerical matrix support.
      - validation: `c3c build --obj-out obj`; focused host
        `advanced-collections-module` passed `1943/0`.
    - [x] `TENSOR-100H-CUDA-SVD-NORMS-ADAPTERS` add CUDA complex SVD layout
      adapters and generated PTX.
      - closed: the existing CUDA complex matrix PTX module now includes
        row-major Omni to column-major cuSOLVER input adapters, wide-matrix
        adjoint-input preparation, cuSOLVER column-major `U` to public
        row-major `u`, and cuSOLVER `VT = V^H` to public row-major `v`
        conversion kernels for Complex128 and Complex64.
      - shipped contract: helper entry points validate shape products and
        byte lengths before CUDA availability, allocation, or launch. The
        generated PTX is split into
        `csrc/tensor_cuda_complex_matrix_ptx_part_00.inc` and
        `csrc/tensor_cuda_complex_matrix_ptx_part_01.inc` to stay under the
        1000 LOC gate.
      - validation: `/usr/local/cuda-13.0/bin/nvcc --ptx -arch=compute_75`,
        `/usr/local/cuda-13.0/bin/ptxas -arch=sm_75`,
        `cc -fsyntax-only -I csrc csrc/tensor_cuda_helpers.c`,
        `scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`, focused
        host `advanced-collections-module` passed `1994/0`, and
        `scripts/check_file_size_gate.sh` passed.
      - negative-memory constraint: do not realify fixed-width complex CUDA
        SVD through doubled real matrices, do not return cuSOLVER `VT` as
        public `v`, and do not silently copy CUDA operands to CPU/LAPACK.
    - [x] `TENSOR-100H-CUDA-SVD-NORMS-EXEC` route CUDA fixed-width complex
      `matrix/singular-values`, spectral/nuclear `matrix/norm`, and
      `matrix/svd`.
      - closed: CUDA Complex128/Complex64 tensors now route
        `matrix/singular-values`, spectral/nuclear `matrix/norm`, and
        reduced-factor `matrix/svd` through cuSOLVER `gesvd` with the shipped
        adapter ABI.
      - shipped contract: CUDA results remain on CUDA as real singular-value
        tensors plus Complex128/Complex64 `u`/`v` factor tensors. Wide inputs
        use the adjoint-input path and map cuSOLVER factors back to public
        Omni row-major `u`/`v`; empty axes return zero-length CUDA result
        tensors without allocation.
      - validation: `cc -fsyntax-only -I csrc csrc/tensor_cuda_helpers.c`,
        `scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`, focused
        host `advanced-collections-module` passed `1999/0`,
        `scripts/check_file_size_gate.sh`, and targeted direct CUDA probes.
      - negative-memory constraint: no CPU/LAPACK fallback for CUDA operands
        and no broad `matrix-numerical-complex128` /
        `matrix-numerical-complex64` claim for this narrow family.
    - [x] `TENSOR-100H-COMPLEX-EIGEN-F64-EIGENPAIRS` migrate CPU Float64
      `matrix/eigenpairs` result tensors to fixed-width complex.
      - closed: CPU `Float64` `matrix/eigenpairs` now returns `Complex128`
        `values` and `vectors` tensors for both LAPACK and pure fallback
        paths, replacing the previous `BigComplex` hidden result type for this
        fixed-width contract slice.
      - shipped contract: existing public name and Float64 input acceptance are
        unchanged; BigComplex remains a separate high-precision family, not the
        backend-neutral result dtype for Float64 eigenpairs.
      - validation: `c3c build --obj-out obj`; focused host
        `advanced-collections-module` passed `1943/0`.
    - [x] `TENSOR-100H-COMPLEX-EIGEN-HERMITIAN-CPU` add CPU Hermitian
      `Complex128`/`Complex64` `matrix/eigenvalues` and `matrix/eigenvectors`.
      - closed: CPU `matrix/eigenvalues` now accepts exact-Hermitian
        `Complex128` and `Complex64` tensors and returns component-real value
        tensors (`Float64` / `Float32`).
      - closed: CPU `matrix/eigenvectors` now returns `{ values, vectors }`
        where values are component-real and vectors preserve `Complex128` or
        `Complex64` input dtype.
      - shipped contract: the correctness path uses an exact Hermitian gate and
        pure Complex128 Jacobi factorization with deterministic phase
        normalization, widening/narrowing `Complex64` around the oracle.
      - validation: `c3c build --obj-out obj`; focused host
        `advanced-collections-module` passed `1948/0`.
      - prerequisites: fixed-width eigen result builder helpers from
        `TENSOR-100H-COMPLEX-EIGEN-F64-EIGENPAIRS`.
      - negative-memory constraint: do not reintroduce BigComplex as the hidden
        backend-neutral output dtype for fixed-width CPU/GPU eigen contracts.
      - negative-memory constraint: do not reuse the SVD Hermitian power helper
        for general Hermitian eigenvalues because it clamps negative
        eigenvalues for Gram-matrix semantics.
    - [x] `TENSOR-100H-COMPLEX-EIGEN-HERMITIAN-CAPS` expose narrow Hermitian
      complex eigen capability fields.
      - closed: `tensor-backends` now reports
        `matrix-hermitian-eigen-complex128` and
        `matrix-hermitian-eigen-complex64` true only on CPU, matching the
        shipped Hermitian CPU oracle.
      - shipped contract at closure time: `matrix-eigenpairs-complex128` and
        `matrix-eigenpairs-complex64` remained false on every backend until the
        general fixed-width eigenpair contract shipped.
      - validation: `c3c build --obj-out obj`; focused host
        `advanced-collections-module` passed `1948/0`.
      - negative-memory constraint: broad `matrix-numerical-complex128` /
        `matrix-numerical-complex64` fields must not imply complex eigenpair
        support.
    - [x] `TENSOR-100H-LAPACK-DGEEV-DISABLE-GATE` make the `dgeev` disable
      hook govern execution, not only capability reporting.
      - closed: `omni_tensor_backend_lapack_dgeev` now returns unavailable when
        the test-only disable hook or `OMNI_TENSOR_DISABLE_LAPACK_DGEEV` marks
        `dgeev` disabled.
      - shipped contract: the forced pure `matrix/eigenpairs` fallback test now
        asserts the `dgeev` call counter is unchanged while disabled, so the
        no-LAPACK path cannot falsely pass by calling LAPACK anyway.
      - validation: `c3c build --obj-out obj`; focused host
        `advanced-collections-module` passed `1948/0`.
      - negative-memory constraint: do not treat backend `available()` probes as
        sufficient fallback verification unless execution counters or equivalent
        side effects prove the disabled routine was not called.
    - [x] `TENSOR-100H-COMPLEX-EIGEN-GENERAL-CPU` add CPU general
      fixed-width `matrix/eigenpairs` for Float32, Complex128, and Complex64.
      - closed: CPU `matrix/eigenpairs` now accepts square `Float32`,
        `Complex128`, and `Complex64` tensors in addition to `Float64`.
      - shipped contract: `Float32` and `Complex64` return `Complex64`
        `values`/`vectors`; `Float64` and `Complex128` return `Complex128`
        `values`/`vectors`.
      - shipped contract: Float32 routes through LAPACK `sgeev` when available
        and through the widened pure real fallback when disabled/unavailable;
        Complex128/Complex64 route through LAPACK `zgeev`/`cgeev` and fail
        closed with `tensor/backend-unavailable` when those complex general
        LAPACK entrypoints are disabled/unavailable.
      - validation: `c3c build --obj-out obj`; focused host
        `advanced-collections-module` passed `1969/0`.
      - negative-memory constraint: keep CUDA eigen false unless a separate
        cuSOLVER eigen lane is opened.
      - negative-memory constraint: do not use realification as a pure general
        complex eigensolver because it cannot distinguish a complex eigenvalue
        from its conjugate for arbitrary complex matrices.
    - [x] `TENSOR-100H-COMPLEX-EIGEN-VULKAN-HERMITIAN` add backend-native
      Vulkan Hermitian fixed-width complex `matrix/eigenvalues` and
      `matrix/eigenvectors`.
      - closed: Vulkan `Complex128`/`Complex64` Hermitian tensors now route
        through native Jacobi shaders and helper ABIs.
      - shipped contract: `matrix/eigenvalues` returns Vulkan-placed
        component-real `Float64`/`Float32` value tensors; `matrix/eigenvectors`
        returns Vulkan-placed values plus `Complex128`/`Complex64` vector
        tensors.
      - shipped contract: non-Hermitian Vulkan complex inputs fail closed with
        `tensor/not-symmetric`; no CPU/LAPACK fallback is used.
      - validation: shader compile plus `spirv-val`;
        `./scripts/build_omni_chelpers.sh`; `c3c build --obj-out obj`;
        focused host `advanced-collections-module` passed `1975/0`;
        `scripts/check_file_size_gate.sh`.
    - [x] `TENSOR-100H-COMPLEX-EIGEN-VULKAN-GENERAL` add backend-native Vulkan
      general fixed-width complex `matrix/eigenpairs`.
      - closed 2026-04-22: Vulkan `Complex128` and `Complex64` general
        `matrix/eigenpairs` now route through backend-native serial shifted-QR
        shaders with private scratch/status buffers and no CPU/LAPACK fallback.
      - shipped contract: public results are Vulkan-placed fixed-width complex
        `values` `[n]` and `vectors` `[n n]`; output columns are sorted by the
        existing magnitude/real/imag ordering and phase-normalized by the
        largest component.
      - shipped contract: real-valued Vulkan `Float64`/`Float32`
        `matrix/eigenpairs` remain fail-closed; this item only closes the
        fixed-width complex TENSOR-100H lane.
      - design checkpoint 2026-04-22: `docs/plans/vulkan-eigensolver-plan-2026-04-17.md`
        now records the chosen shared solver boundary: staged complex
        Hessenberg reduction plus implicit shifted QR, with private status
        buffer, public Vulkan `Complex128`/`Complex64` values/vectors outputs,
        no public status sentinels, and no hidden CPU/LAPACK fallback.
      - implementation checkpoint 2026-04-22: the shipped first native solver is
        a serial shifted-QR implementation over dense row-major device buffers
        with explicit no-convergence status. Full Hessenberg staging remains a
        future performance/numerical-hardening optimization, not an open
        capability blocker.
      - validation: shader compile plus `spirv-val`; helper rebuild; `c3c build
        --obj-out obj`; direct Complex128/Complex64 Vulkan triangular probes;
        bounded focused `advanced-collections-module` passed.
      - negative-memory constraint: no hidden CPU/LAPACK fallback for Vulkan
        operands; do not realify arbitrary complex matrices because that loses
        eigenvalue/eigenvector pairing semantics.
