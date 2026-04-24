# Plans Index

This folder now holds supporting implementation notes and design documents.
The live backlog has been consolidated into `TODO.md` at the repo root.

## Usage Rules

1. Use `memory/CHANGELOG.md` as current-state truth.
2. Keep `TODO.md` as the only live execution queue.
3. Treat files in this folder as design context or one-off implementation notes, not as parallel backlog sources.

## TODO-Linked Plan Status

- Current live queue status: memory-boundary telemetry/benchmark evidence work
  is open in `TODO.md` Part 18 as `MEM-BENCH-OBSERVE-004` and
  `MEM-BENCH-OBSERVE-005`; `MEM-BENCH-OBSERVE-001` closed with the signal
  inventory, `MEM-BENCH-OBSERVE-002` closed with runtime counter coverage, and
  `MEM-BENCH-OBSERVE-003` closed with benchmark workload coverage.

- `slash-surface-naming-audit-plan-2026-04-23.md`: closed surface naming
  audit. Closed slices document Pika as language-core, choose a Deduce module
  facade, choose the ML module split, flatten `ml/linear-batched-reduce`,
  clarify ML/NN activation naming, and move the math/stats scientific naming
  follow-up to the dedicated math/stats module plan.
- `math-stats-scientific-module-plan-2026-04-23.md`: current scientific module
  migration reference. The core `math` and `stats` module facades,
  docs/tests migration, old slash special/stat callable removal, and Vulkan
  `Float32` `math.erf` / `math.erfc` extension have shipped. Granular
  `tensor-backends` scientific capability reporting has also shipped. The
  remaining Vulkan `Float64`/harder special-function policy has been
  documented and intentionally stays fail-closed until a validated contract is
  added.
- `tagged-switch-exhaustiveness-remediation-plan-2026-04-23.md`: completed audit
  plan for fully eliminating hidden `default:` switch arms in the captured
  compiler/parser/tensor audit surface. The remediation shipped 2026-04-23;
  every listed switch was rewritten to explicit cases or moved behind a named
  fail-closed helper with tests.
- `memory-tier3-evaluation-2026-04-21.md`: closed evaluation note for the
  remaining Tier 3 memory proposal items. It rejects immediate `Value` inline
  collection rewrites, unmeasured TEMP lane paging, and cross-thread scope pool
  changes without benchmark/telemetry evidence and a transfer contract. Future
  work should start with measurement: collection length/capacity telemetry,
  chunk-class fragmentation telemetry, and scope global-lock contention
  telemetry.
- `stable-escape-graph-plan-2026-04-23.md`: finalized proposal note for a
  stable handle / prepared-graph boundary model. It recommends publishing
  prepared escape graphs through stable handles instead of moving raw
  pointers, and its TODO-backed rollout queue has now closed the shape-audit,
  store-skeleton, prepared-publication, first prepared-node data-structure,
  fallback-observability, dictionary prepared-graph, set prepared-graph,
  closure, mutation-policy, commit-route visibility, and first `CONS`
  materialization slices. Prepared `CONS` roots with scalar leaves, `ARRAY` roots
  with shared child reuse, dictionary roots with key/value child-index pairs, set
  roots with member-only child indices, cyclic container back-edges, and closure
  roots with captured binding-value edges are represented in the stable store;
  TEMP `CONS`, `ARRAY`, dictionary, set, closure, string/error payload,
  time-point, and big-number returns can now materialize from prepared metadata
  into the target build scope, prepared handles fail closed after structural
  mutation drift, and boundary commit results expose stable-publication,
  stable-materialization, and compatibility-route use. No open stable-escape
  prepared-materialization rollout item remains.
- `memory-boundary-architecture-spec-2026-04-24.md`: current implementation
  spec for the region-RC/TEMP/ESCAPE boundary contract. It records
  `ScopeRegion` as ordinary Omni value ownership authority, makes stable
  materialization the preferred TEMP-to-ESCAPE boundary path, and keeps
  `FFI_HANDLE` opaque and scope-integrated rather than graph-owned by generic
  language-value RC. `MEM-BOUNDARY-POLICY-001` and
  `MEM-BOUNDARY-VERIFY-001` are closed; the bounded memory-lifetime smoke gate
  now passes for the policy/store/rollback work, including bounded container
  Valgrind. Stable indexed publication and region transplanting remain
  constrained fast paths, not a replacement for destination materialization
  when the lifetime proof is weaker than the copy proof.
- `memory-boundary-proof-planner-roadmap-2026-04-24.md`: closed roadmap for
  maximizing stable indexed publication plus TEMP/ESCAPE boundary planning. It
  defines the `BoundaryPlanner` ladder, stable graph passports, transplant
  proofs, mutation epoch invalidation, FFI bridge declarations, copy-debt
  telemetry, and commit-path migration. The TODO Part 18 proof-planner queue is
  closed: `MEM-BOUNDARY-PLANNER-001`, `MEM-BOUNDARY-PASSPORT-001`,
  `MEM-BOUNDARY-EPOCH-001`, `MEM-BOUNDARY-TRANSPLANT-001`,
  `MEM-BOUNDARY-FFI-BRIDGE-001`, `MEM-BOUNDARY-COPY-DEBT-001`, and
  `MEM-BOUNDARY-PLAN-MIGRATE-001` are closed.
- `memory-boundary-telemetry-benchmark-plan-2026-04-24.md`: open plan for
  maximizing memory-boundary observability before further optimization. It
  tracks low-overhead counter expansion, `memory-lifetime-bench` workload
  expansion, first bounded baseline capture, and a counter-first
  regression-envelope parser. The existing-signal inventory slice is closed in
  `memory-boundary-telemetry-signal-inventory-2026-04-24.md`.
- `vulkan-backend-decision-2026-04-16.md`: completed `TENSOR-100E` baseline note for
  portable explicit GPU execution behind the existing `Tensor` surface. Vulkan
  is runtime-optional, capability-gated, and owned through an Omni C ABI
  helper. Probe, explicit CPU<->Vulkan placement/copy, dense row-major
  `Float64` `map` arithmetic for Tensor/scalar, scalar/Tensor, exact-shape
  Tensor/Tensor inputs, and right-aligned singleton-axis Tensor/Tensor
  broadcasting, generic dense row-major rank-N `Float64` multi-axis
  `contract`, and dense row-major `Float64` matrix kernels for
  `matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`,
  `matrix/trace`, `matrix/rank`, `matrix/lu`, `matrix/solve`,
  `matrix/determinant`, `matrix/inverse`, `matrix/cholesky`, `matrix/qr`, and
  `matrix/norm` are
  landed. The generic contract shader uses checked-in GLSL source and
  rank/shape/stride metadata-buffer dispatch. No public `VulkanTensor`, backend-named math API,
  implicit CPU/GPU transfer, or silent `Float64` downcast is allowed.
- `vulkan-dtype-layout-policy-2026-04-17.md`: locks the Vulkan expansion
  policy. Keep fixed-width dtype paths explicit; do not downcast between
  `Float64` and `Float32`; do not lower pointer-backed Big* dtypes to Vulkan.
  Fixed-width complex CPU storage plus CUDA/Vulkan raw storage, elementwise
  `map`, `contract`, structural matrix kernels, and the landed Vulkan
  numerical subset are now capability-gated explicitly. Remaining deferred
  boundaries are full complex SVD factor output, CUDA complex
  singular-values/norm/SVD, complex eigen result contracts, and stride-aware
  layouts. Its next-boundary section now delegates the broader library and
  parallel solver roadmap to `vulkan-math-library-roadmap-2026-04-17.md` while
  keeping dtype/layout preconditions local to the policy note.
- `fixed-width-complex-closure-plan-2026-04-18.md`: closure record for the
  fixed-width complex tensor work. `TENSOR-100H-SVD-FACTORS`,
  `TENSOR-100H-CUDA-SVD-NORMS`, and `TENSOR-100H-COMPLEX-EIGEN` are closed;
  no live TODO lane remains under this plan.
- `vulkan-math-library-roadmap-2026-04-17.md`: reference roadmap for Vulkan
  math work after the correctness-first `TENSOR-100E` baseline. It records
  the shipped `TENSOR-100F` helper/library/dtype/layout work and `TENSOR-100G`
  as the measured
  parallel solver baseline. The plan keeps the public
  surface backend-neutral, records `Float64` and `Float32` as active Vulkan
  dtype families where shipped, records `Complex128`/`Complex64` CPU storage,
  CUDA/Vulkan raw storage, CUDA/Vulkan elementwise `map`, CUDA/Vulkan
  fixed-width complex `contract`, and CUDA/Vulkan fixed-width complex
  structural matrix kernels as shipped behind explicit operation capability
  bits. It also records landed Vulkan transpose-view materialization and Vulkan
  fixed-width complex LU/determinant/solve/inverse/rank/norm/singular-values
  reducers and factor kernels.
  The roadmap rejects Big* lowering
  and hidden CPU fallback, and requires a separate measured
  performance item before extending solver algorithms again.
- `vulkan-ml-suite-roadmap-2026-04-19.md`: reference roadmap for building a
  backend-neutral ML suite on top of explicit Vulkan Tensor placement. Its
  historical `ML-VK-*` TODO lanes cover ML capability reporting,
  batched linear algebra, neural elementwise/reduction kernels, convolution
  and pooling, normalization and attention, reverse-mode autograd, optimizers,
  model/layer serialization, graph capture/fusion/memory planning, and ML
  validation/benchmarks. The roadmap keeps public APIs backend-neutral and
  rejects hidden CPU fallback for Vulkan operands.
- `omni-neural-dataspec-plan-2026-04-20.md`: accepted design direction for the
  high-level `nn/*` library. Omni Neural DataSpec keeps networks, parameters,
  state, and execution options as inspectable data while giving users
  TensorFlow/Keras-like ergonomics through transparent model bundles. Vulkan
  remains first-class for inference and training through explicit Tensor
  placement, truthful capability bits, and fail-closed behavior instead of
  hidden CPU fallback.
- `vulkan-ml-linear-direct-float32-batched-reduction-surface-2026-04-20.md`:
  accepted decision for the narrow `ml-linear-direct-float32` capability and
  public `ml/linear-batched-reduce` surface under `ML-VK-010-004`.
- `cuda-cublas-backend-decision-2026-04-16.md`: CUDA/cuBLAS stays behind the
  backend-neutral Tensor surface. Current support includes explicit
  `to-device` CPU/CUDA copies, cuBLAS rank-2/rank-1 contract fast paths,
  embedded-PTX CUDA binary elementwise `map` for dense row-major `Float64` and
  `Float32` scalar, exact-shape, and right-aligned broadcast operands, CUDA
  arithmetic/component unary `map` for eligible dense row-major real tensors,
  generated CUDA C/libdevice PTX scientific unary `map` including `math.erf`,
  `math.erfc`, and `stats.normal-cdf`, and destination-form `realize` into
  existing dense row-major `Float64` or `Float32` CUDA destinations without
  implicit CPU fallback.
  Landed slices include Vulkan `matrix/cholesky`, Vulkan `matrix/qr`, Vulkan
  `matrix/singular-values`, unary/helper expansion, zero-axis contract, and
  the staged thresholded Vulkan `matrix/solve` helper.
- `vulkan-svd-factor-output-plan-2026-04-17.md`: direct Vulkan `matrix/svd`
  planning under the existing backend-neutral surface. It preserves reduced
  `u`/`s`/`v` factor-output semantics, keeps outputs Vulkan-placed, forbids
  hidden CPU/LAPACK fallback, and tracks the storage-backed large-`k`
  factor-output helper separately from the singular-value-only helper.
- `vulkan-eigensolver-plan-2026-04-17.md`: direct Vulkan eigen closure record
  for existing `matrix/eigenvalues`, `matrix/eigenvectors`, and
  `matrix/eigenpairs` surfaces. Native Vulkan general `matrix/eigenpairs` is
  shipped for dense row-major `Float64`, `Float32`, `Complex128`, and
  `Complex64`, returning Vulkan-placed fixed-width complex `values` and
  `vectors` with exact-shift hardening and active-submatrix deflation.
- `vulkan-float32-dtype-and-kernel-plan-2026-04-17.md`: `Float32` planning
  split into the landed native `Tensor Float32` CPU storage/oracle phase, the
  landed CPU `Float32` matrix factor/SVD oracle phase, the landed Vulkan
  placement/map/unary/minmax, rank-N contract, and structural matrix slices,
  the landed reducer and SVD-backed slices (`matrix/rank`, all `matrix/norm`
  selectors, `matrix/singular-values`, and `matrix/svd`), and the landed
  Vulkan `Float32` serial factor/solve slices (`matrix/determinant`,
  `matrix/lu`, `matrix/solve`, `matrix/inverse`, `matrix/cholesky`, and
  `matrix/qr`) plus staged parallel `matrix/solve`. Large-dense SVD robustness
  is landed through scale-aware eigenvalue tolerance and orthonormal
  completion. It explicitly rejects using `Float32` as a downcast fallback for
  current `Float64` Vulkan execution.
- `fixed-width-complex-tensor-contract-2026-04-18.md`: contract for scalar and
  Tensor `Complex128`/`Complex64`. It documents constructors, stdlib
  predicates, Tensor dtype behavior, component helper result dtypes, CPU
  tensor `map`/`contract`, CUDA/Vulkan raw storage, CUDA/Vulkan fixed-width
  complex elementwise `map`, CUDA/Vulkan fixed-width complex `contract`, and
  CUDA/Vulkan fixed-width complex structural matrix kernels behind explicit
  operation capability bits. Vulkan fixed-width complex
  LU/determinant/solve/inverse/rank/norm/singular-values/QR/Cholesky is landed
  behind `matrix-numerical-complex128` and `matrix-numerical-complex64`.
  Remaining CUDA numerical complex singular-value/norm support, full complex
  SVD factor output, and complex eigen families remain explicit deferred
  operation families.

Recently closed TODO-linked plans:

- `main-lisp-module-cycle-isolation-2026-04-21.md`: closes
  `AUDIT-2026-M5-MODULE-CYCLE` by making the current `main` <-> `lisp` module
  cycle explicit and isolated. `main -> lisp` is limited to entry, CLI, REPL,
  build, bindgen, source-check, and test adapter surfaces; `lisp -> main` is
  limited to low-level runtime services still hosted in `main` such as
  scope/region, stack/coroutine, lifecycle, bindgen manifest, diagnostics, and
  tests. A real cycle break remains a separate neutral-runtime-module
  extraction, not a leaf import reshuffle.
- `ffi-first-class-grouped-module-plan-2026-04-11.md`: grouped C ABI FFI,
  grouped bindgen output, generated bind manifests, strict bind TOML hardening,
  and `exclude-functions` denylist filtering.
- `foreign-runtime-core-plan-2026-04-11.md`: common `ForeignHandle` runtime
  descriptor and adapter boundary for future C, Python, Julia, CUDA/cuBLAS,
  optional C++ tooling, and polyglot lanes; the common-core lane is closed
  through `FOREIGN-CORE-002R`.
- `tensor-scientific-computing-plan-2026-04-11.md`: canonical `Tensor`,
  tensor-dispatched `map` and `contract`, constructor-driven materialization,
  flat row-major `Iterator(Tensor)` conversion, public surface cleanup,
  singleton-axis broadcasting, and backend-boundary design are closed.
  `realize` remains only a low-level Tensor storage primitive for destination
  reuse.
- `matrix-solver-surface-decision-2026-04-16.md`: locks `matrix/` as the
  rank-2 Tensor solver/decomposition namespace, with `matrix/solve` and
  `matrix/lu` as shipped surfaces plus `matrix/determinant` as the first
  decomposition consumer, `matrix/transpose` as the first structural matrix
  transform, `matrix/diagonal` as the second structural matrix transform,
  `matrix/diagonal-matrix` as the first structural matrix constructor,
  `matrix/identity` as the first size-driven structural matrix constructor,
  `matrix/trace` as the first structural matrix reduction, `matrix/rank` as
  the first numerical structural matrix reduction, `matrix/norm` as the first
  general matrix norm reducer, `matrix/inverse` as the first solve-derived
  matrix transform, `matrix/qr` as the first non-LU
  decomposition, `matrix/cholesky` as the first symmetric positive-definite
  decomposition, `matrix/singular-values` as the direct singular-value
  extraction surface, plus `matrix/svd` as the first rectangular decomposition
  and `matrix/eigenvalues` / `matrix/eigenvectors` as symmetric-real eigen
  surfaces, `matrix/eigenpairs` as the general nonsymmetric eigen surface,
  and optional `dgesvd` backend coverage for `matrix/rank`,
  and rejects bare `solve`, `linalg/solve`, `tensor/solve`, and
  backend-flavored public solver names for the first slice.
- `cuda-cublas-backend-decision-2026-04-16.md`: locks the `TENSOR-100`
  explicit-device CUDA/cuBLAS design. Future GPU support stays behind
  `Tensor`, uses `to-device`, `device`, and `tensor-backends`, rejects public
  `GpuTensor`/`CudaTensor` and backend-flavored math names, and forbids
  implicit CPU/GPU transfer inside ordinary Tensor operations. The first
  CPU-only implementation slice, `TENSOR-100A`, registers `device` and
  `to-device` with CPU realization and fail-closed CUDA diagnostics.
  `TENSOR-100B` adds internal placement metadata, structured
  `tensor-backends` inventory, CPU-kernel rejection of non-CPU storage, and
  fake-device destruction coverage for opaque backend handles. `TENSOR-100C`
  adds optional runtime-loaded CUDA CPU<->device copy support for concrete
  `Float64` Tensor storage, later extended to `Float32`. `TENSOR-100D` adds runtime-loaded cuBLAS
  rank-2/rank-2 GEMM plus rank-2/rank-1 and rank-1/rank-2 GEMV contract
  execution for CUDA-placed dense row-major `Float64` tensors, later extended
  to matching `Float32` and rank-1/rank-1 dot. `TENSOR-100F` adds embedded-PTX
  CUDA binary elementwise map kernels for dense row-major `Float64`/`Float32`
  tensor/scalar, scalar/tensor, exact-shape tensor/tensor, and right-aligned
  singleton-axis tensor/tensor broadcast operands, then adds arithmetic and
  component unary CUDA map helpers for unary `+`, `abs`, unary `-`, `sqrt`,
  `real-part`, `imag-part`, and `conjugate`.

Historical plans in this directory remain useful context, but they are not
live backlog entries unless `TODO.md` explicitly references them.

Recently closed:

- `validation-all-slice-2026-04-11.md`: non-Tensor all-slice failure follow-up;
  final bounded all-slice-without-TLS result used
  `OMNI_SKIP_TLS_INTEGRATION=1` and is unified `pass=2798 fail=0`, compiler
  `pass=208 fail=0`; this is not a replacement for the TLS integration gate.
- `validation-all-slice-nested-let-residual-2026-04-11.md`: closed after
  balancing the malformed memory-stress nested-let fixture.
