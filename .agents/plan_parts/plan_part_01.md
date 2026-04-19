# Agent Plan Index Part 01

Source: `.agents/PLAN.md`

# Scientific Numerics And Tensor Plan

Date: 2026-04-14
Workspace: `/home/christos/Omni`

## Manual Old-Master Integration Checkpoint

Date: 2026-04-19

Current state:

- Old `master` hardening changes through `37b49ff9` / `vnxlknss` have been
  manually replayed onto the merged `main` / `master` tensor line.
- The old final local plan commit is intentionally superseded by this plan's
  integrated Tensor direction. Do not revive the stale `linalg/matmul` or
  early GSL-first naming direction from old `master`.
- Allocation and env-copy hardening were reconciled against the current Tensor
  runtime APIs instead of restoring stale old-master JIT/FFI implementations.
- Env-copy self-referential closure payload support must stay narrowly gated:
  only a releasing-scope closure whose captured env directly self-references
  the same wrapper enters the memoized clone path. Do not broadly mark all
  undelimited releasing-scope captured env closures as cloneable; that masks
  invalid-alias and rollback failures.

Validation checkpoint:

- `git diff --check`
- `scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- Bounded container `basic` Lisp slice: `160/0`
- Bounded container `deduce` Lisp slice: `432/0`
- Bounded container `memory-lifetime-smoke` Lisp slice: `231/0`

## Ignored LSP Source Split And Audit Correction

Date: 2026-04-19

Current state:

- The previous over-700 inventory used `rg --files`, which obeyed ignore rules
  and missed the real Python source under `tooling/omni-lsp/`. Treat that
  inventory as superseded for file-splitting audits.
- `.gitignore` now explicitly unignores `tooling/omni-lsp/*.py` and
  `tooling/omni-lsp/tests/*.py`, and the LSP source/test files are tracked.
- `tooling/omni-lsp/omni_lsp.py` was split into focused mixin/support modules;
  the wrapper is now only the executable entrypoint and `OmniLspServer`
  composition point.
- `tooling/omni-lsp/tests/smoke_test.py` was split into smoke helpers and
  smoke scenarios. Audit tightened the split assertions back to exact current
  behavior, including the `2 refs` code-lens expectation and the restored
  extended-formatting fixture loop that had been reduced to `pass`.
- Tensor audit found a real runtime contract bug: `realize` rejected evaluated
  `ERROR` sources with a type error before preserving the original source
  error. `prim_realize` now propagates evaluated source/destination errors
  before type checks.
- The current source split gate is closed for project-owned source when
  generated SPIR-V/PTX and the generated tree-sitter parser are excluded:
  `find src csrc tooling scripts -path '*/__pycache__' -prune -o -type f \( -name '*.c3' -o -name '*.c' -o -name '*.cpp' -o -name '*.h' -o -name '*.hpp' -o -name '*.lua' -o -name '*.py' -o -name '*.sh' -o -name '*.inc' \) -print0 | xargs -0 wc -l | awk '$2 != "total" && $1 > 700 { print }' | rg -v '(_spv\.c$|_ptx\.inc$|tooling/tree-sitter-omni/src/parser\.c$)' | sort -nr`
  prints no rows.

Validation checkpoint:

- `git diff --check` passed.
- Python syntax compile for all `tooling/omni-lsp/*.py` and
  `tooling/omni-lsp/tests/*.py` passed.
- `PYTHONDONTWRITEBYTECODE=1 python3 tooling/omni-lsp/tests/smoke_test.py`
  passed.
- `scripts/build_omni_chelpers.sh` passed.
- `c3c build --obj-out obj` passed and linked `build/main`.
- Bounded container `advanced-collections-module` Lisp slice passed `1581/0`.

Open risks:

- Remaining over-700 rows in a whole-repository count are non-source
  documentation/session logs, vendored/dependency files, generated SPIR-V/PTX
  C fragments, the generated tree-sitter parser, or binary/cache artifacts.
- Full bounded-container `OMNI_LISP_TEST_SLICE=all` was not run in this pass.

## All Over-700 Source Split And Audit Checkpoint

Date: 2026-04-19

Current state:

- The repository-level maintainable source inventory is now below the
  requested 700-line ceiling. The final inventory command reported only the
  aggregate total and no individual files above 700 LOC:
  `rg --files src csrc tooling scripts | rg '\.(c3|c|cpp|h|hpp|lua|py|sh|inc)$' | rg -v '(_spv\.c$|__pycache__|tooling/tree-sitter-omni/src/parser\.c$)' | xargs wc -l | awk '$1 > 700 { print }' | sort -nr`.
- The largest-first split pass reached beyond Tensor into remaining C helper,
  C3 test/runtime manifest, eval provenance, Vulkan backend, and Neovim plugin
  files. The largest touched Neovim files are now under cap:
  `init.lua` 697 LOC, `repl/session.lua` 687 LOC, and `repl/selection.lua`
  622 LOC.
- Lua audit found real extraction regressions after the initial split:
  `selection_commands.lua` still reached for stale parent-module names and
  LSP navigation needed private helper functions threaded through its context.
  Those were fixed by exporting the necessary selector helpers, exposing
  `session.eval_once`, and passing LSP helper callbacks into
  `omni.lsp_navigation`.
- C3 audit found no compile-time extraction regression in the split manifest,
  test dispatchers, or C3 source slices. Manifest part array counts match
  their declared sizes.
- C/C++ helper split validation remains green through the helper build.

Validation checkpoint:

- `git diff --check` passed.
- `scripts/build_omni_chelpers.sh` passed.
- `nvim --headless -u NONE +'set rtp+=/home/christos/Omni/tooling/omni-nvim' +'lua require("omni")' +qa` passed.
- `c3c build --obj-out obj` passed and linked `build/main`.

Open risks:

- This checkpoint proves syntax/build/load hygiene after the split, not full
  semantic equivalence of every extracted runtime/test slice.
- Neovim REPL selection and LSP navigation were loaded headlessly but were not
  exercised interactively in a live buffer.
- Full bounded-container Lisp slice validation was not run in this pass.

## Largest-First Tensor Source Split Checkpoint

Date: 2026-04-19

Current state:

- First largest-first split target was `csrc/tensor_vulkan_helpers.c`.
- Landed boundary: dynamic Vulkan loading, availability probing, shared
  context ownership, buffer lifecycle, host/device copy, fill, retain, and
  free moved into `csrc/tensor_vulkan_helpers_core.c`.
- Shared Vulkan helper constants/types/externs/internal prototypes moved into
  `csrc/tensor_vulkan_helpers_internal.h`.
- Second landed boundary: scalar/complex map plus dense contract setup and
  launch code moved into `csrc/tensor_vulkan_helpers_map_contract.c`.
- The map/contract split owns `map_add_scalar_f64`, dense `map` for
  `Float64`/`Float32`, fixed-width complex map launch, and dense `contract`
  for `Float64`/`Float32`/`Complex128`/`Complex64`.
- Shared descriptor-layout, descriptor-set, compute-pipeline, and
  single-dispatch helpers are now explicit internal declarations exported from
  the remaining Vulkan helper file for use by split family files.
- Build wiring was updated in `scripts/build_omni_chelpers.sh` and
  `project.json`.
- `csrc/tensor_vulkan_helpers.c` dropped from roughly 12.3k LOC to roughly
  7.3k LOC.
- Third landed boundary: Tensor `map` callable classification, CUDA/Vulkan
  direct map routing, CPU map evaluation, lazy map expression construction,
  and `prim_tensor_map` moved from `src/lisp/prim_tensor.c3` into
  `src/lisp/prim_tensor_map.c3`.
- `src/lisp/prim_tensor.c3` dropped from roughly 9.0k LOC to roughly 6.8k LOC.
  Current line counts now put `src/lisp/prim_tensor_matrix.c3` at roughly
  8.2k LOC ahead of the remaining Vulkan helper file.

Validation checkpoint:

- `scripts/build_omni_chelpers.sh` passed.
- `c3c build --obj-out obj` passed.
- `git diff --check` passed.
- Bounded container `basic` Lisp slice passed `160/0`.
- Focused `advanced-collections-module` is not green: bounded container saw
  `1580/1` after the Vulkan and C3 map splits, centered on
  `realize propagates single-arg source errors`. Earlier host validation after
  the first split saw `1596/2`, adding `realize vulkan unsupported callable
  fails closed`. The splits were mechanical source decomposition and build
  wiring, so treat this as an unresolved Tensor validation signal rather than a
  proven split regression until isolated.

Next checkpoint:

- Superseded by the over-700 source split checkpoint above. Do not continue
  from the older `prim_tensor_matrix.c3` queue head as if it were still live;
  the remaining useful work is runtime validation of the split surfaces.

## Active Direction

This operational note now defers Tensor surface authority to the integrated
repo plan:

- `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
- `docs/areas/tensor-scientific.md`
- `docs/LANGUAGE_SPEC.md`
- `docs/reference/03-collections.md`

The older local `.agents/PLAN.md` statement that treated `linalg/matmul` as a
chosen public name is invalidated. The recovered and integrated decision is:

- `Tensor` is the rank-polymorphic scientific numeric aggregate.
- `map` is the canonical elementwise tensor operation, including scalar
  broadcast and right-aligned singleton-axis tensor-tensor broadcasting.
- Elementwise multiplication is `(map * a b)`, not `(* a b)`.
- `contract` is the canonical summed-axis tensor contraction operation.
- Rank-2 matrix multiplication maps to `(contract a b [1 0])`; the explicit
  `(contract a b [1] [0])` form remains accepted.
- Constructor dispatch is the canonical public materialization boundary:
  `Array`, `List`, `Dictionary`, and `Tensor` consume iterators or normalize
  compatible values through their constructors.
- `realize` is demoted to a low-level Tensor storage primitive for exact
  destination writes and compatibility with existing tests; it is not Omni's
  general lazy sequence abstraction.
- `matmul` is rejected as canonical because it is rank-2 biased and programming
  jargon; do not implement `linalg/matmul` as the normal public surface.
- Do not add public `TensorExpr`, `map-into`, backend-flavored operation names,
  `delay`/`force` promise surfaces, or implicit CPU/GPU transfers as
  first-surface semantics.

## Active Non-Docker Work Plan

The validation-image architecture fix is excluded from this plan. The current
non-Docker implementation order is:

1. Landed: Vulkan `map` and map-backed `min`/`max` preflight ordering now
   checks callable/device/dtype eligibility before concrete Tensor resolution
   can realize unsupported CPU operands. Mixed CPU/Vulkan lazy operands and
   unsupported Vulkan callables fail closed before hidden CPU materialization.
2. Landed: CUDA now proves the shared probability-domain status ABI for
   data-dependent unary math through `stats/normal-quantile`: device status
   `0` means success, `1` means probability outside `0 < p < 1`, and `2` means
   non-finite input, with whole-operation failure before result exposure.
3. Landed: CUDA `stats/normal-quantile` now uses generated CUDA C/libdevice PTX
   op `20` for dense row-major `Float64`/`Float32` Tensor storage, preserving
   CUDA placement and dtype for direct and `map` paths.
4. Landed: Vulkan `Float32` `stats/normal-quantile` now uses a separate
   status-bearing same-dtype unary helper. Vulkan `Float64` stays separate; do
   not downcast or assign op `20` to the existing statusless two-buffer Vulkan
   unary helper.
5. Landed: Vulkan `Float64` `stats/normal-cdf` now uses a documented
   piecewise polynomial double approximation in the existing Float64 unary
   shader/helper, preserving Vulkan placement and dtype without hidden CPU
   fallback or Float32 downcast.
6. Landed: Vulkan `Float64` `stats/normal-quantile` now uses a Float64
   status-bearing inverse-CDF helper. Direct and `map` paths preserve Vulkan
   placement and `Float64` dtype for valid probabilities, and invalid
   probability/non-finite status maps to the scalar-compatible diagnostics
   before output exposure.
7. Landed: CUDA-first dtype-changing Tensor rounding uses a dedicated
   status-bearing CUDA helper and a native CPU `Tensor BigInteger` result path.
   Direct `floor`, `ceiling`, `round`, and `truncate` over dense row-major
   CUDA `Float64`/`Float32` inputs are gated by `tensor-backends`
   `rounding-big-integer`; do not add these operations as same-dtype GPU unary
   map opcodes.
8. Landed: Vulkan Tensor rounding now uses dedicated `shaderInt64`-gated
   status/copyback helpers for dense row-major `Float64`/`Float32` inputs and
   materializes CPU `Tensor BigInteger` output behind the Vulkan
   `rounding-big-integer` capability.
9. Landed: add Tensor view/layout representation metadata through the public
   `tensor-layout` primitive. The returned dictionary covers storage offset,
   physical strides, concrete backing extent/capacity, owner/alias metadata,
   and write-safety policy. Lazy expression payloads report zero storage
   extent until realization.
10. Landed: define and implement the narrow public read-only transpose-view
   contract through `matrix/transpose-view`. CPU `ref`, `Array`, `List`, and
   `realize` materialization observe the view; GPU/copy-kernel paths fail
   closed for view payloads.
11. Landed: unsupported Vulkan `map` callable errors propagate through
   `handle` as structured `tensor/backend-unsupported` raises for direct
   `map`, lazy Vulkan map sources, and one-argument `realize` of unsupported
   lazy Vulkan map expressions. The fix did not widen Vulkan `floor` or
   rounding coverage through generic `map`. The same runtime pass fixed
   compiled primitive `ERROR` argument propagation and increased normal
   StackCtx budget so handled CPU lazy Tensor map realization succeeds on small
   tensors instead of reporting `stack overflow in handle body`.
12. Landed: define native CPU fixed-width complex scalar and Tensor semantics
   for `Complex128` and `Complex64`, then add CUDA/Vulkan raw storage
   placement and explicit CPU copyback behind `complex128`/`complex64`
   capability bits. Vulkan dense row-major fixed-width complex elementwise
   `map` now has explicit `elementwise-map-complex128` /
   `elementwise-map-complex64` operation bits and preserves Vulkan placement
   for supported arithmetic/component operations. CUDA dense row-major
   fixed-width complex elementwise `map` now uses the same operation bits with
   generated PTX/status helpers. CUDA/Vulkan dense row-major fixed-width
   complex `contract` now has explicit `contract-complex128` /
   `contract-complex64` operation bits. Vulkan dense row-major fixed-width
   complex structural matrix kernels now have explicit
   `matrix-structural-complex128` / `matrix-structural-complex64` operation
   bits for `matrix/transpose`, `matrix/diagonal`, `matrix/diagonal-matrix`,
   and `matrix/trace`. CUDA dense row-major fixed-width complex structural
   matrix kernels now support the same public family behind those operation
   bits. Vulkan fixed-width complex `matrix/inverse`, `matrix/rank`, direct
   `matrix/norm` reducers, `matrix/qr`, and `matrix/cholesky` also landed
   behind the same numerical capability bits. Fixed-width complex numerical
  matrix kernels beyond landed Vulkan
  LU/determinant/solve/inverse/rank/norm/singular-values/QR/Cholesky and
  direct Vulkan general `matrix/eigenpairs` remain behind explicit
  backend/result-contract gates. Full fixed-width complex `matrix/svd` factor
  output remains separate because it needs complex `u`/`v` tensors and
  component-width real `s`.
  Remaining fixed-width complex closure is now split into
  `TENSOR-100H-SVD-FACTORS`, `TENSOR-100H-CUDA-SVD-NORMS`, and
  `TENSOR-100H-COMPLEX-EIGEN`; see
  `docs/plans/fixed-width-complex-closure-plan-2026-04-18.md`.
13. Landed: Vulkan materialization for direct rank-2 `matrix/transpose-view`
   operands where explicit Vulkan placement/realization/copyback needs dense
   storage. Keep this narrower than arbitrary strided GPU execution: broader
   view-aware Vulkan kernels still require an ABI for offset, strides, backing
   extent, owner/aliasing, and write policy.
14. Landed: Vulkan fixed-width complex `matrix/lu`, `matrix/determinant`, and
   `matrix/solve` for dense row-major zero-offset `Complex128`/`Complex64`
   tensors behind `matrix-numerical-complex128` and
   `matrix-numerical-complex64`; host and bounded-container focused validation
   passed.
15. Treat large-`k` SVD and large-`n` symmetric eigen work as measurement-led
   performance lanes. Do not replace the current correctness-first Vulkan
   paths without checked-in measurements showing the bottleneck.

Negative constraints:

- Do not restart the rolled-back generic Vulkan unary `map` mode-3 branch.
- Do not implement GPU quantile by assigning an opcode without per-element
  probability-domain status propagation.
- Do not implement GPU rounding as same-dtype float map output.
- Do not regress CUDA rounding into staged-only coverage; `rounding-big-integer`
  is shipped only as a direct dtype-changing CUDA helper plus CPU
  `Tensor BigInteger` materialization.
- Do not treat generic Vulkan `available`, `float64`, or `float32` capability
  as proof of dtype-changing rounding. Vulkan direct rounding must report
  `rounding-big-integer` and return CPU `Tensor BigInteger`; same-dtype Vulkan
  float rounding remains invalid.
- Do not add stride parameters to GPU helpers before Tensor values can
  represent storage offset, backing extent, and alias ownership.
- Do not claim broad view-backed GPU support from `tensor-layout` or
  `matrix/transpose-view`; the shipped Vulkan view materialization slice is
  limited to direct rank-2 transpose views and dense GPU kernels still require
  zero-offset dense row-major storage unless a
  later helper explicitly accepts view metadata. The active Vulkan
  transpose-view materialization lane creates dense Vulkan storage for the
  explicit materialization boundary, but it must not be described as general
  stride-aware GPU kernel execution.
- Do not lower pointer-backed `BigComplex` into Vulkan storage for
  `matrix/eigenpairs`.
- Do not infer CUDA/Vulkan complex kernel support from `complex128` or
  `complex64` storage capability bits. Those bits mean raw placement/copyback
  only; operation-family bits must be reported separately. Vulkan complex map
  is now reported through `elementwise-map-complex128` /
  `elementwise-map-complex64`; CUDA complex map is now reported through the
  same bits when the generated PTX/status helper is usable. Complex
  contract bits are landed; CUDA/Vulkan structural complex matrix bits are
  landed; Vulkan complex LU/determinant/solve/inverse/rank/norm/
  singular-values/QR/Cholesky bits are landed. Remaining complex numerical
  matrix bits must stay false until real kernels land.
