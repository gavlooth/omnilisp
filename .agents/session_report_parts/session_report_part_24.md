# Session Report Index Part 24

Source: `.agents/SESSION_REPORT.md`

## 2026-04-14 11:55 CEST - Boost BigInteger First Slice And Solver Naming Checkpoint

Objective attempted:
- Land the first Boost.Multiprecision scalar integration after the owner chose
  the BigInteger path, and record the solver naming constraint that `solve`
  must not be bare while `linalg/` remains unsettled.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Installed `libboost-dev` on the host to provide Boost.Multiprecision headers.
- Added `csrc/big_integer_helpers.cpp`, a small C++17 shim around
  `boost::multiprecision::cpp_int` with a stable C ABI.
- Added C3 extern declarations and `BIG_INTEGER` value support, including
  destructor, copy-to-parent, escape/root promotion, env-copy, printing,
  hashing/equality, builtin type identity, and `Number` parent registration.
- Added `BigInteger` constructor/coercion behavior for `Integer`, decimal
  `String`, and existing `BigInteger`; invalid decimal input and out-of-range
  `Integer` narrowing fail with `type/arg-mismatch`.
- Updated `+`, `-`, and `*` so `Integer` overflow promotes to `BigInteger`
  and `BigInteger` combines with `Integer`/`BigInteger`; mixed `Double`
  arithmetic uses finite double conversion where possible.
- Added the C++ helper object to `scripts/build_omni_chelpers.sh` and linked
  the helper archive from `project.json`.
- Added focused advanced stdlib numeric coverage for constructor,
  stringification, type identity, `number?`, overflow promotion, arithmetic,
  and failure payloads.
- Updated `memory/CHANGELOG.md`, `.agents/PLAN.md`, `TODO.md`,
  `docs/LANGUAGE_SPEC.md`, `docs/areas/tensor-scientific.md`, and
  `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`.

Commands run and key results:
- `test -f /usr/include/boost/multiprecision/cpp_int.hpp`: initially absent.
- `sudo apt-get install -y libboost-dev`: installed Boost 1.83 development
  headers.
- `c++ -O2 -std=c++17 -c csrc/big_integer_helpers.cpp -o /tmp/omni_big_integer_helpers.o`:
  passed.
- `./scripts/build_omni_chelpers.sh`: passed.
- `c3c build --obj-out obj`: passed with existing entry deprecation warnings.
- Direct smokes passed for:
  - `(String (BigInteger "9223372036854775808"))`
  - `(String (+ 9223372036854775807 1))`
  - `(= (type-of (BigInteger "1")) (quote BigInteger))`
  - `(String (* (BigInteger "9223372036854775808") 2))`
  - `(Integer (BigInteger "9223372036854775807"))`
  - invalid decimal and out-of-range narrowing handlers returning
    `"type/arg-mismatch"`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=66 fail=0`.
- Remaining advanced numeric child groups passed; exact and combined TCO
  groups required `prlimit --stack=67108864` because the default-stack
  `(length (range 4000))` path crashes in deep recursive escape promotion.
- `prlimit --stack=67108864 env OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=295 fail=0`.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Final `c3c build --obj-out obj`: passed with the same existing entry
  deprecation warnings.
- Final direct smokes for overflow promotion, invalid BigInteger input, and
  `type-of BigInteger`: passed.
- `git diff --check` on the touched tracked files: passed.
- `jj status` and `jj diff --stat`: worked at the final checkpoint.

Current best recommendation / checkpoint:
- Treat `BigInteger` as the first Boost.Multiprecision scalar slice only.
  Keep `/`, `%`, ordering comparisons, bitwise operations, `gcd`/`lcm`,
  `BigFloat`/`BigComplex`, and `parse-number` arbitrary-precision parsing as
  explicit follow-up slices.
- Do not publish a bare `solve` for LAPACK/LAPACKE solver conveniences.
  `linalg/` is not accepted yet as the base namespace; keep the public
  qualifier unresolved until the Tensor convenience layer is named.

Unresolved issues:
- The normal-stack advanced numeric TCO headroom case crashes in recursive
  escape promotion for `(length (range 4000))`; the same test passes with a
  64MB stack limit. This looks like a pre-existing deep-promotion headroom
  issue, not a BigInteger arithmetic failure, but it remains a validation
  caveat.
- The earlier corrupt-object `jj` failure was not reproduced at this checkpoint:
  `jj status` and `jj diff --stat` both worked after the BigInteger slice.

Signature: Codex GPT-5

## 2026-04-14 08:20 CEST - Tensor Native BLAS Backend First Slices

Objective attempted:
- Write and start implementing the plan to keep ordinary Tensor storage
  C3-native/scoped while integrating OpenBLAS/LAPACK-style acceleration
  directly at the Tensor backend layer instead of through user-visible
  `ForeignHandle` Tensor storage.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added private native BLAS shim `csrc/tensor_blas_helpers.c` with optional
  runtime `cblas_dgemm` discovery via `dlopen`/`dlsym`.
- Added C3 extern declarations in `src/lisp/tensor_blas_backend.c3`.
- Wired `tensor_contract_eval_into(...)` to use the native `dgemm` fast path
  only for contiguous rank-2 row-major `Double` contraction equivalent to
  `(contract a b [1 0])`; unsupported cases still fall back to the pure C3
  contraction kernel.
- 09:40 CEST follow-up: extended the same private `dgemm` path with CBLAS
  transpose flags so all contiguous rank-2 single-axis contractions can route
  through BLAS when available: `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`.
- Added an internal path-sensitive call counter so the targeted regression can
  prove BLAS branch use when a compatible BLAS symbol is available.
- Added focused regressions for the transpose-backed contract cases without
  introducing a public `matmul`/`linalg-matmul` surface.
- Added the shim to both `project.json` direct build sources and
  `scripts/build_omni_chelpers.sh` for AOT/e2e helper-archive linkage.
- Updated Tensor plan/area notes to record that ordinary Tensor storage remains
  native/scoped and is not replaced with `ForeignHandle`.

Commands run and key results:
- `jj status`: working copy already had unrelated dirty/untracked operational
  artifacts plus Tensor docs/source changes from the current session.
- `find /usr/include /usr/local/include /usr/include/aarch64-linux-gnu ...`:
  found no local `cblas.h`, `lapacke.h`, or OpenBLAS headers.
- `ldconfig -p` / `nm -D`: found generic `/lib/aarch64-linux-gnu/libblas.so.3`
  exporting `cblas_dgemm`; no hard OpenBLAS pkg-config/header path was present.
- `c3c build --obj-out obj`: passed with the existing deprecation warnings in
  entry file/read/build helpers.
- `bash -n scripts/build_omni_chelpers.sh`: passed.
- `./scripts/build_omni_chelpers.sh`: passed.
- `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
- Direct Tensor smoke
  `(ref (realize (contract a b [1 0])) [1 1])`: returned `154.0`.
- Direct Tensor smokes for transpose-backed rank-2 contractions returned
  `84.0` for `[0 0]`, `68.0` for `[1 1]`, and `123.0` for `[0 1]`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed first at `217 passed, 0 failed`; after the transpose follow-up it
  passed at `221 passed, 0 failed`.
- Subagent check: explorer `019d8aec-bae8-7462-a5c6-4ec1fbd360af` confirmed
  the CBLAS row-major transpose mapping for all four rank-2 axis-pair cases.
- Final `jj status` and `jj diff --stat` attempts failed with a corrupt loose
  Git object at `.git/objects/da/96d1f92060d63d31f50ba48b1086b0fbc60545`;
  `git status --short` and targeted `git diff --stat` still worked.

Current best recommendation / checkpoint:
- Keep `TensorVal` as the normal scoped Tensor storage model.
- Treat direct native backend calls as implementation details behind `contract`
  / `realize`, not as public `TensorHandle` or user-level FFI handles.
- Continue `TENSOR-090` by deciding LAPACK/LAPACKE solver/decomposition
  surface names. Remaining BLAS broadening, such as rank-2/rank-1 `gemv`,
  should stay private and continue treating the pure C3 kernel as the
  validation oracle.

Unresolved issues:
- Full/heavy validation was not run in this session.
- LAPACK/LAPACKE has not been wired yet; this slice only covers the first BLAS
  `dgemm` family behind rank-2 `contract`.
- BLAS `gemv`/vector-specialized paths are still deferred; rank-2/rank-1
  contractions currently use the pure fallback.
- VCS health issue at that Tensor checkpoint: `jj` status/diff tripped over
  the corrupt loose Git object above. This was not reproduced after the later
  BigInteger checkpoint, where `jj status` and `jj diff --stat` worked.

Signature: Codex GPT-5

## 2026-04-12 18:35 CEST - Scientific Numerics and Tensor Planning Checkpoint

Objective attempted: define the forward plan for Omni scientific work, including exact integer promotion, BigFloat, Tensor constructor conversions, GSL compatibility, and adjacent scientific libraries.

Workspace: `/home/christos/Omni`.

Code/configuration changes made:
- Added `.agents/PLAN.md` with a phased scientific numerics plan.
- Recorded the durable design decision in memory under `Omni scientific numerics tensor plan 2026-04-12`.

Key results:
- Chose constructor/coercion dispatch as the public API rule for Tensor conversions, matching existing `Array` behavior such as `(Array '(1 2 3))`.
- Planned GMP-backed exact integer storage with automatic promotion on overflow while preserving inline small `Integer`.
- Planned MPFR-backed `BigFloat` as explicit high-precision float support, while keeping `Double` as the default hardware float and avoiding silent `Double` overflow promotion unless the owner later explicitly chooses it.
- Planned Tensor as the scientific data boundary: unboxed numeric storage or foreign backend handle with dtype/shape/strides/rank/device/ownership metadata, while `Array` remains heterogeneous boxed `Value*` storage.
- Planned GSL as an optional Tensor-aware package/backend first, not a mandatory core dependency, with C shim error conversion and layout/device/dtype checks.
- Recommended scientific library ordering: OpenBLAS + LAPACK/LAPACKE, HDF5, optional FFTW, SUNDIALS, NetCDF, SuiteSparse/GraphBLAS, ARPACK-ng, and libtorch as the ML/autograd/GPU Tensor backend.

Commands run:
- `sed -n '1,260p' .agents/PLAN.md`
- `git diff --check -- .agents/PLAN.md`
- `git diff --check -- .agents/PLAN.md .agents/SESSION_REPORT.md`

Validation:
- `git diff --check -- .agents/PLAN.md`: passed.
- `git diff --check -- .agents/PLAN.md .agents/SESSION_REPORT.md`: passed.
- No build/test commands were run because this was a planning-only pass with no source code changes.

Current best recommendation / checkpoint:
- Start implementation with Phase 1 in `.agents/PLAN.md`: exact integer representation, `Integer`/`BigInteger` constructor and dispatch behavior, overflow-promotion helper API, FFI range-check policy, and boundary lifetime regressions for GMP-backed values.
- Do not start by binding all of GSL; Tensor constructor conversion and exact integer promotion are better foundations.

Unresolved issues:
- Naming for public `(Float x)` remains owner-level: the plan recommends using it only if approved, otherwise keeping `Double` as the canonical hardware-float constructor.
- GSL default-core inclusion remains blocked on an explicit GPL-compatible distribution decision.

Signature: Codex GPT-5

## 2026-04-14 07:30:08 CEST - Tensor Naming Decision Integration

Objective attempted:
- Investigate prior Tensor naming decisions after the owner recalled that
  Tensor iteration involved explicit realization, Tensor arithmetic should
  use the normal tensor surface, and `matmul` had been rejected in favor of
  another name.
- Pull and integrate the owner-pushed remote bookmark
  `bookmark/omni-local-master-2026-04-14@origin`.
- Apply the follow-up owner decision to drop public Tensor `materialize` in
  favor of `realize`, and simplify common `contract` notation with paired-axis
  arrays.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Rebased local reader-tag and operational-artifact work on top of remote
  commit `87680cae` (`docs: fix tensor broadcasting status drift`).
- Resolved `.agents/PLAN.md` by replacing the stale standalone Tensor plan with
  a short operational note pointing at the integrated canonical Tensor plan.
- Resolved `memory/CHANGELOG.md` by keeping the remote changelog and reinserting
  the local 2026-04-13 reader-tag entry.
- Renamed the public Tensor boundary primitive from `materialize` to `realize`
  with no alias, updating runtime registration, AOT primitive lookup, Tensor
  error messages, docs, examples, and tests.
- Added `contract` paired-axis shorthand: `(contract a b [1 0])` for the
  common rank-2 pair and `[[1 0] [2 3]]` for multi-axis pairs. The explicit
  `(contract a b [1] [0])` form remains accepted.
- Confirmed no shipped `TensorHandle` type exists in `TensorVal`; the backend
  plan keeps ordinary Tensor storage native/scoped and reserves explicit
  ownership/finalizer policy for genuinely opaque backend resources.

Commands run and key results:
- `jj status`: showed the existing dirty working copy plus the operational
  artifacts/docs from this planning session.
- `jj git fetch --remote origin --branch bookmark/omni-local-master-2026-04-14`:
  fetched `bookmark/omni-local-master-2026-04-14@origin`.
- `jj rebase -r @ -d 'bookmark/omni-local-master-2026-04-14@origin'`: rebased
  the local working-copy commit onto the fetched bookmark and produced
  conflicts only in `.agents/PLAN.md` and `memory/CHANGELOG.md`.
- Targeted `rg` searches over `.agents`, `docs`, `memory`, `examples`, and
  `plan.jsonl` initially found no local replacement for rejected `matmul`, but
  the fetched bookmark carried the missing canonical Tensor plan.
- `docs/plans/tensor-scientific-computing-plan-2026-04-11.md` now records the
  recovered decision: `contract` is the canonical tensor contraction operation,
  rank-2 multiplication is `(contract a b [1 0])`, `realize` is the
  tensor-expression-to-storage boundary, and `matmul` is rejected as
  programming jargon/rank-2 biased.
- `c3c build --obj-out obj`: passed; existing deprecation warnings remain in
  `src/entry_file_read_reporting.c3`, `src/entry_build_aot_temp.c3`, and
  `src/entry_build_backend_compile.c3`.
- Direct Tensor smoke `(ref (realize (contract a b [1 0])) [1 1])`: returned
  `154.0`.
- Direct Tensor smoke `(ref (realize (contract a b [[1 0]])) [1 1])`:
  returned `154.0`.
- Direct Tensor smoke `(ref (realize (map * x 2.0)) [1 2])`: returned `12.0`.
- Direct old-name removal smoke `(materialize (Tensor Double [1] [1]))`:
  failed with `unbound variable 'materialize'`, as intended.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `216 passed, 0 failed`.
- `OMNI_LISP_TEST_SLICE=reader-dispatch LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `16 passed, 0 failed`.
- `git diff --check` on touched Tensor/docs/artifact files: passed.
- Conflict marker scan on touched Tensor/docs/artifact files: clean.

Current best recommendation / checkpoint:
- Treat `Tensor`, tensor-dispatched `map`, `contract`, and `realize` as the
  locked Tensor surface.
- Prefer paired-axis arrays for new contraction examples:
  `(contract a b [1 0])` for rank-2 and `[[1 0] [2 3]]` for multi-axis
  contractions.
- Elementwise multiplication is `(map * a b)`, not `(* a b)`.
- BLAS rank-2 GEMM should optimize
  `(realize (contract a b [1 0]) out)` behind the Tensor backend
  boundary, not introduce `linalg/matmul` as a public surface.
- Keep the scalar scientific plan Boost-first and GSL-avoiding.

Unresolved issues:
- Heavy/full validation was not run in this session.

Signature: Codex GPT-5

## 2026-04-14 07:06:05 CEST - Boost-First Scientific Plan Correction

Objective attempted:
- Correct the scientific numerics plan after owner clarification that the goal
  is to avoid GSL/GNU-style baseline dependencies and prefer Boost precision.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Updated `.agents/PLAN.md` so the default scientific backend direction is:
  Boost.Multiprecision for exact/high-precision scalar values, Boost.Math for
  scalar scientific functions, and OpenBLAS + LAPACK/LAPACKE for dense Tensor
  linear algebra.
- Replaced the prior GMP/MPFR-backed first-slice wording with
  Boost.Multiprecision-backed `BigInteger`, `BigFloat`, and `BigComplex`
  planning.
- Replaced the prior GSL compatibility phase with a Boost.Math scientific
  functions phase behind an owned C++ shim and C ABI boundary.
- Recorded that GSL is not a default dependency and should only be revisited as
  an explicit optional compatibility package if the owner asks for it.

Commands run and key results:
- `git diff --check -- .agents/PLAN.md`: passed.

Current best recommendation / checkpoint:
- Start with a Boost.Multiprecision C++ shim for `BigInteger` (`cpp_int`) and
  plan `BigFloat`/`BigComplex` (`cpp_complex`) behind the same backend boundary.
- Keep user-facing names backend-neutral; do not expose Boost or GSL names in
  Omni language surfaces.

Unresolved issues:
- No implementation/build validation was run; this was a plan correction only.

Signature: Codex GPT-5
