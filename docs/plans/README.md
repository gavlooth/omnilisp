# Plans Index

This folder now holds supporting implementation notes and design documents.
The live backlog has been consolidated into `TODO.md` at the repo root.

## Usage Rules

1. Use `memory/CHANGELOG.md` as current-state truth.
2. Keep `TODO.md` as the only live execution queue.
3. Treat files in this folder as design context or one-off implementation notes, not as parallel backlog sources.

## Current Planning State

No plan file in this directory is a live execution queue by itself. `TODO.md`
is the only live queue, and it currently reports `Current actionable count: 4`
because of the README overhaul item plus the new syntax-template surface plan.
The items below are either closed plans, reference specs, or conditional
future directions that must be reopened through a new TODO entry before
implementation.

## Worth Considering, Not Backlog

These are the only inspected directions that still look technically worth
keeping visible:

- AArch64 stack backend: useful only if ARM runtime support becomes a product
  target. Reopen from `stack-aarch64-implementation-plan-2026-04-08.md` with a
  concrete validation host/toolchain.
- Neutral runtime module extraction: useful only if the current documented
  `main` <-> `lisp` cycle stops being an acceptable build boundary. Reopen from
  `main-lisp-module-cycle-isolation-2026-04-21.md`.
- Vulkan/ML expansion: useful only when backend ML product work resumes. Reopen
  from `vulkan-ml-suite-roadmap-2026-04-19.md` with specific operation slices.
- CUDA/Vulkan complex numerical gaps: useful only when complex SVD/norm or
  eigen families become product requirements. Reopen from the fixed-width
  complex and Vulkan/CUDA tensor roadmap notes.
- Memory allocator policy tuning: useful only after a non-synthetic bounded
  benchmark shows runtime boundary/promotion pressure. Do not reopen from the
  old aggregate slack counters alone.

Everything else inspected is historical, closed, superseded, or covered by the
current area-status docs. Validation is green after the bounded TLS-enabled
all-slice pass closed `VALIDATION-002-ALL-SLICE-BOUNDARY-JIT-BLOCKER`.

## TODO-Linked Plan Status

- `memory-model-proof-matrix-2026-04-26.md`: closed proof plan for applying the
  same proof/measurement/hardening treatment to the entire memory model after
  the core migration closed. TODO Part 18 lane `MEM-PROOF-001` is closed with
  a manifest-backed ownership inventory guard for memory-sensitive call sites,
  FFI wrapper families, dynamic FFI handle call sites, and tensor device
  finalizer authorities. `MEM-PROOF-002` is closed with ScopeRegion unit,
  memory-smoke, and Valgrind evidence for TEMP/ESCAPE teardown, dtor-record
  OOM, retain/release symmetry, owner guards, splice preconditions, and retired
  `scope_adopt` runtime paths. `MEM-PROOF-003` is closed with checked
  `FFI_HANDLE` constructor destructor-registration rollback and focused
  finalizer/free-owned payload OOM coverage. `MEM-PROOF-004` is closed with
  checked closure-copy destructor-registration rollback, forced env/copy-parent
  closure dtor OOM regressions, rejected-transplant compatibility retry for
  closure-backed iterator boundaries, and bounded memory/JIT/Valgrind evidence.
  `MEM-PROOF-005` is closed with checked boundary-route selected outcomes,
  direct closure escape dtor-registration rollback, direct-promotion
  disallowance coverage, forced-no-splice materialization evidence, policy
  guards, benchmark-envelope counters, and Valgrind evidence.
  `MEM-PROOF-006` is closed with stable escape/prepared graph/transplant proof,
  stale-handle invalidation, mutation-drift invalidation, cyclic/shared graph
  coverage, refcount rejection, benchmark evidence, and Valgrind evidence.
  `MEM-PROOF-007` is closed with collection copy/materialization, rollback,
  known-capacity constructor OOM, benchmark, and Valgrind evidence.
  `MEM-PROOF-008` is closed with native tensor/device cleanup proof and leak
  validation. `MEM-PROOF-009` is closed with async, scheduler, thread, and
  callback lifetime validation. `MEM-PROOF-010` is closed with native
  wrapper-family metadata coverage and isolated Valgrind on the foreign-handle
  metadata group.
- `memory-model-improvement-plan-2026-04-25.md`: completed memory-model
  improvement plan. It keeps `ScopeRegion` as ordinary Omni value ownership
  authority and treats the planner/passport/transplant work as current
  foundation, not a work item to reopen. The TODO Part 18 memory-model
  improvement queue is closed: `MEM-MODEL-IMPROVE-002` added allocator
  histograms, per-scope sequence evidence, request/unused buckets, and
  source/site attribution, then closed without a policy change because the
  remaining ESCAPE no-follow-up bucket is synthetic direct benchmark traffic.
  `MEM-MODEL-IMPROVE-003` closed the shared Dictionary/Set known-entry sizing
  slice by reducing hashmap/set growth counters to zero. `MEM-MODEL-IMPROVE-004`
  closed the boundary value policy coverage guard with a manifest-backed check
  wired into the boundary change policy script, `MEM-MODEL-IMPROVE-005` closed
  the `atomic-ref` FFI bridge keepalive slice, and `MEM-MODEL-IMPROVE-006`
  landed product-style Finwatch, closure-heavy iterator, tensor-metadata, and
  nested-module return workload slices.
- Current queue status: memory-boundary telemetry/benchmark evidence work
  in `TODO.md` Part 18 is closed through `MEM-BENCH-OBSERVE-005`. The lane
  produced the signal inventory, runtime counter coverage, benchmark workload
  coverage, first counter baseline, and
  `scripts/check_memory_telemetry_benchmark_envelope.sh`.

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
- `memory-boundary-telemetry-benchmark-plan-2026-04-24.md`: closed plan for
  memory-boundary observability before further optimization. It shipped
  low-overhead counter expansion, `memory-lifetime-bench` workload expansion,
  first bounded baseline capture, and a counter-first regression-envelope
  parser. Future memory optimization should start from the baseline and
  `scripts/check_memory_telemetry_benchmark_envelope.sh`, not from a live TODO
  under this plan.
- `readme-tutorial-overhaul-plan-2026-05-01.md`: open docs plan for
  consolidating the root README into a sectioned, tutorial-style language
  entrypoint with runnable examples and doc-map cross-links. Tracked by TODO
  Part 18 `DOCS-README-001`.
- `syntax-literal-template-surface-plan-2026-05-01.md`: completed language-surface
  plan for the `#syntax` reader-template form with `#{exp}` unquote and
  `#{.. exp}` splice. Replaces user-facing quasiquote examples while preserving
  `quote` and hygienic macro expansion. Tracked by TODO Part 18
  `SYNTAX-TEMPLATE-001` through `SYNTAX-TEMPLATE-003`.
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
  descriptor and adapter boundary for C ABI handles, CppInterop API-mode
  bindgen tooling, and CPU/CUDA tensor-buffer marshalling. Python/Julia and
  polyglot/plugin runtime lanes are out of scope; the common-core lane is
  closed through `FOREIGN-CORE-002S`.
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
