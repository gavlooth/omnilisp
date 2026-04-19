# Session Report Index Part 28

Source: `.agents/SESSION_REPORT.md`

## 2026-04-18 13:10 CEST - CUDA Fixed-Width Complex Map Docs Checkpoint

- Objective attempted:
  - Update documentation and operational artifacts only for the landed CUDA
    fixed-width complex elementwise map work. No code/source/test files were
    edited in this documentation pass.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - fixed-width complex Tensor docs, Tensor area status, TODO, active plan,
    changelog, and this session report.
- Documentation/artifact changes made:
  - Superseded stale wording that CUDA complex `map` remains fail-closed.
  - Recorded CUDA `Complex128`/`Complex64` elementwise `map` as shipped behind
    `elementwise-map-complex128` / `elementwise-map-complex64`.
  - Recorded supported CUDA complex map ops: binary `+`, `-`, `*`, `/`; unary
    `abs`, unary `-`, identity/`+`, `real-part`, `imag-part`, and
    `conjugate`.
  - Recorded direct helper dtype behavior: CUDA complex `real-part`,
    `imag-part`, and `abs` return component-width real CUDA tensors; direct
    unary `-` and `conjugate` preserve complex dtype.
  - Kept CUDA/Vulkan complex `contract` and matrix kernels fail-closed.
- Commands verified by the implementation work:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build`: passed.
  - Host focused `advanced-collections-module`: `1433 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1416 passed, 0
    failed`.
  - CUDA PTX generation and `ptxas` for `csrc/tensor_cuda_complex_map.cu`:
    passed.
  - C helper syntax check: passed.
  - Primitive docs parity, Stage 3 source parity, and targeted diff check:
    passed.
- Current best recommendation / checkpoint:
  - Treat CUDA/Vulkan fixed-width complex elementwise `map` as landed and split
    remaining complex backend work around `contract`/matrix kernels, not a
    broad "complex GPU kernels" umbrella.
- Unresolved issues:
  - CUDA/Vulkan fixed-width complex `contract` remains fail-closed.
  - CUDA/Vulkan fixed-width complex matrix kernels remain fail-closed.
- Signature: Codex GPT-5.4

## 2026-04-18 12:13 CEST - CUDA/Vulkan Fixed-Width Complex Storage Round-Trips

- Objective attempted:
  - Continue fixed-width complex implementation by landing CUDA/Vulkan
    `Complex128`/`Complex64` raw storage placement and explicit CPU copyback,
    while preserving fail-closed complex GPU compute.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_tensor.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups.c3`
  - fixed-width complex Tensor docs, Vulkan dtype policy, TODO, and
    operational plan artifacts
- Code or configuration changes made:
  - Added separate CUDA/Vulkan storage dtype predicates for raw copy paths.
    The existing real-only predicates remain in compute gates.
  - Broadened `to-device` CPU-to-CUDA/Vulkan and CUDA/Vulkan-to-CPU copyback
    to accept `Complex128` and `Complex64` storage when the backend is
    available.
  - Broadened CUDA/Vulkan destination validation for matching complex device
    tensors and added complex scalar fill staging through host buffers plus raw
    copy-to-existing-device helpers.
  - Added `tensor-backends` `complex128` and `complex64` storage capability
    bits for CUDA/Vulkan. Historical note: complex map/contract bits were
    explicit false at this storage-only checkpoint; map bits are superseded by
    later Vulkan and CUDA complex map checkpoints.
  - Added availability-gated tests for complex device layout, placement,
    round-trip copyback, destination `realize`, scalar fill, no-hidden CPU
    destination realization, and fail-closed complex CUDA/Vulkan compute.
  - Updated docs/backlog/planning to distinguish storage/copy capability from
    complex GPU kernels.
- Commands run and key results:
  - `c3c build`: passed.
  - Host focused `advanced-collections-module`: `1399 passed, 0 failed`.
  - Bounded-container focused `advanced-collections-module`: `1382 passed,
    0 failed`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat CUDA/Vulkan fixed-width complex as placement-forbidden. It is
    now raw-storage-copyable when `tensor-backends` reports `complex128` or
    `complex64`.
  - Do not treat `complex128` or `complex64` storage capability as kernel
    capability. Historical note: CUDA/Vulkan complex `map` still intentionally
    failed closed at this checkpoint, then landed in later backend-specific map
    checkpoints. Complex `contract` and matrix kernels still intentionally fail
    closed.
- Current best recommendation / checkpoint:
  - Historical storage-only checkpoint. The first operation family,
    elementwise `map`, is now landed for Vulkan and CUDA; keep direct general
    Vulkan `matrix/eigenpairs` blocked on its public result contract.
- Unresolved issues:
  - Superseded: CUDA/Vulkan complex map kernels are now implemented in later
    checkpoints.
  - CUDA/Vulkan complex contract/matrix kernels are not implemented.
  - Fixed-width complex scientific/transcendental scalar/Tensor operations
    beyond arithmetic, component helpers, `abs`, and `conjugate` remain
    fail-closed pending a precision contract.
- Signature: Codex GPT-5.4

## 2026-04-18 11:53 CEST - CPU Fixed-Width Complex Scalar And Tensor Support

- Objective attempted:
  - Land native CPU `Complex128`/`Complex64` scalar and Tensor semantics, then
    update operational planning so the remaining fixed-width complex lane is
    clearly CUDA/Vulkan backend work.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - scalar value/runtime/type integration
  - CPU Tensor storage, map/contract/component helpers, and matrix structural
    operations
  - `TODO.md`, `.agents/PLAN.md`, `memory/CHANGELOG.md`, docs/reference, and
    Tensor/Vulkan planning docs
- Code or configuration changes made:
  - Added `Complex128` and `Complex64` scalar value families with constructors,
    stdlib predicates, dispatch/type integration, printing/String conversion,
    equality/hash, AOT/literal serialization, and boundary copy/promotion
    handling.
  - Added CPU Tensor `Complex128`/`Complex64` dtype/storage support for
    explicit and inferred construction, scalar fill, `ref`, `Array`/`List`
    conversion, `realize`, mixed fixed-complex `map` promotion, `contract`,
    `real-part`, `imag-part`, `abs`, `conjugate`, unary minus, and structural
    matrix operations (`matrix/transpose`,
    `matrix/diagonal`, `matrix/diagonal-matrix`, `matrix/identity`, and
    `matrix/trace`).
  - Kept CUDA/Vulkan fixed-width complex placement and backend execution
    fail-closed. Future GPU complex support needs explicit backend capability
    bits, device layout/copy semantics, status contracts, and kernels.
  - Updated fixed-width complex docs/backlog/planning wording so CPU support is
    no longer described as deferred.
- Commands run and key results:
  - `c3c build`: passed.
  - Direct `--eval` smokes for scalar `String`, Tensor `ref`, component dtype,
    and Complex128 `contract`: passed.
  - Host focused scalar advanced tests passed (`180 passed, 0 failed`).
  - Host focused Tensor advanced tests passed (`1383 passed, 0 failed`).
  - Bounded-container focused Tensor advanced tests passed (`1366 passed,
    0 failed`).
  - Targeted `git diff --check`: passed.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating fixed-width complex as missing at the CPU
    scalar/Tensor layer. The remaining fixed-width complex work is backend
    placement/execution and direct Vulkan general `matrix/eigenpairs` contract
    work.
  - Do not infer complex GPU support from real `float64` or `float32`
    `tensor-backends` capability bits.
- Current best recommendation / checkpoint:
  - Use the landed CPU `Complex128`/`Complex64` Tensor behavior as the oracle
    for the next CUDA/Vulkan complex ABI. Start with explicit capability bits,
    device layout/copy semantics, and a first operation family such as
    `map`/`contract` before attempting general complex eigenpairs.
- Unresolved issues:
  - Fixed-width complex scientific/transcendental operations beyond arithmetic,
    component helpers, `abs`, and `conjugate` remain fail-closed pending an
    explicit approximation/precision contract.
  - CUDA/Vulkan `Complex128`/`Complex64` placement, backend `map`, backend
    `contract`, backend matrix kernels, and direct Vulkan general
    `matrix/eigenpairs` remain unimplemented.
- Signature: Codex GPT-5.4

## 2026-04-19 21:42 CEST - FTXUI Doc And Config Split

- Objective attempted:
  - Mechanically split the remaining over-700 vendor doc/config files in
    `third_party/ftxui/doc/` into a small stub plus ordered part files, while
    avoiding C/C++ sources and preserving content order.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `third_party/ftxui/doc/Doxyfile.in`
  - `third_party/ftxui/doc/mainpage.md`
- Code or configuration changes made:
  - Replaced `third_party/ftxui/doc/Doxyfile.in` with a short config stub that
    chains ordered fragments via `@INCLUDE`, preserving the original config
    semantics as a single Doxygen config file.
  - Split the original Doxyfile content into four ordered fragment files under
    `third_party/ftxui/doc/Doxyfile.in.parts/`:
    `part-01.txt`, `part-02.txt`, `part-03.txt`, and `part-04.txt`.
  - Replaced `third_party/ftxui/doc/mainpage.md` with a short Markdown index
    that links to ordered section fragments.
  - Split the original main page content into three ordered fragment files
    under `third_party/ftxui/doc/mainpage.md.parts/`:
    `part-01.md`, `part-02.md`, and `part-03.md`.
- Commands run and key results:
  - `wc -l` inventory for the two original files and the generated fragments:
    all fragments are below 700 LOC.
  - `git diff --check -- third_party/ftxui/doc/...`: passed.
  - `doxygen --version`: not available in this environment, so the config stub
    could not be runtime-validated here.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not assume `doxygen` is installed in this workspace; config/runtime
    validation remains unverified until a machine with Doxygen is available.
  - Do not keep the Doxyfile fragments under a `.cfg` extension in this vendor
    subtree; the vendor ignore rules hide them. Use the `.txt` fragments instead
    so the split is visible without changing vendor ignore policy.
- Current best recommendation / checkpoint:
  - Treat the two requested vendor files as split and the remaining validation
    gap as purely environmental. If future work needs a runtime check, run a
    Doxygen parse/build on a machine that has the tool installed.
- Unresolved issues:
  - `Doxyfile.in` runtime parsing remains unverified on this machine because
    `doxygen` is absent.
- Signature: Codex GPT-5.4

## 2026-04-20 00:16 CEST - Vulkan ML Linear No-Bias Lowering

- Objective attempted:
  - Continue the strict ML-VK audit by implementing the first concrete Vulkan
    `ml/linear` slice after adding TODO checkboxes for the found boundary.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `src/lisp/prim_ml_linear.c3`
  - `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  - `docs/todo_parts/todo_part_14.md`
  - `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
- Code or configuration changes made:
  - Added a pre-CPU-resolution Vulkan `Float32` no-bias path for direct
    concrete `ml/linear` input and weights, lowered through the existing
    Tensor `contract` helper.
  - Added primitive-level tests for Vulkan result placement/dtype, rank-3
    batch projection, mapped-source copyback, mixed-device fail-closed
    behavior, bias fail-closed behavior, view fail-closed behavior, and shape
    diagnostics.
  - Marked `ML-VK-010-001` and `ML-VK-010-002` complete, while keeping
    `ML-VK-010-003` and `ML-VK-010-004` open for bias/reduction and broader
    expression/view handling.
- Commands run:
  - Subagent audits for routing safety, TODO readiness, and test placement.
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_file_size_gate.sh`
  - `git diff --check`
  - `scripts/check_primitive_docs_parity.sh`
- Key results:
  - Initial focused test run failed with one malformed test expression and one
    wrong lazy-source expectation; the latter invalidated the assumption that
    direct Vulkan `map` inputs must fail closed for this slice.
  - Final focused advanced collections suite passed with `pass=1618 fail=0`.
  - File-size gate, whitespace diff check, and primitive docs parity passed.
- Current best recommendation / checkpoint:
  - Continue with `ML-VK-010-003`: Vulkan `Float32` bias-add and batched
    reduction coverage. Keep broad Vulkan `ml-linear` capability false until
    the operation family is complete enough to advertise truthfully.
- Unresolved issues:
  - Full bounded-container suite was not run.
  - Vulkan `Float64` `ml/linear`, Vulkan bias, and view-backed/expression-backed
    `ml/linear` remain incomplete or explicitly fail-closed.
- Signature: Codex GPT-5.4
