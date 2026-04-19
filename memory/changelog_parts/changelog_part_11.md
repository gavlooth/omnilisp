# Memory Changelog Index Part 11

Source: `memory/CHANGELOG.md`

## 2026-04-11

- Landed `FFI-TOML-004B` generated bind manifest:
  - `--bind` now writes `lib/ffi/<name>_manifest.toml` beside each generated
    raw binding module and facade stub.
  - The manifest records the effective shipped bindgen output config:
    dependency name, library, raw syntax, generated raw/facade paths, and
    `strip-prefixes`.
  - Manifest TOML string emission escapes quotes, backslashes, and control
    characters, and fails closed for inconsistent prefix pointer/count input.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=215 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`

- Closed `VALIDATION-ALL-SLICE-NESTED-LET-2026-04-11`:
  - Root cause: the memory-stress `nested let 10 levels => 55` fixture had a
    malformed expression with two extra closing parentheses, so both the
    interpreter and JIT harnesses stopped at parse.
  - Fixed the test fixture in `tests_core_groups.c3` and left the parser
    behavior unchanged.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Direct `--eval` of the corrected nested-let expression: `55`
    - Docker-bounded `memory-stress` slice: unified `pass=15 fail=0`
    - Docker-bounded all-slice rerun: unified `pass=2798 fail=0`, compiler
      `pass=208 fail=0`

- Landed `FFI-TOML-004A` facade prefix stripping for bindgen:
  - `TomlFfiDep` now carries `strip-prefixes`, and `toml_parser` parses
    `[dependencies.ffi.NAME] strip-prefixes = ["prefix_"]` string arrays.
  - `--bind` threads the parsed prefixes into facade generation.
  - Raw binding modules intentionally keep C-derived names so the current FFI
    parser still derives truthful `dlsym` symbol names; prefix stripping applies
    to facade/exported Omni names and `raw-*` import aliases.
  - Added compiler coverage proving grouped raw output preserves
    `omni_bindgen_*` C symbols while the facade exposes stripped names.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=214 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`

- Revalidated the `VALIDATION-ALL-SLICE-2026-04-11` Deduce residual clusters:
  - `relation-attrs`: `pass=13 fail=0`
  - `core-runtime`: `pass=6 fail=0`
  - `rule-validation`: `pass=70 fail=0`
  - Fixed the diagnostics match/union residual by making
    `Parser.alloc_ast_array_bytes(..., count=0)` return `null` without setting
    a parser error. Zero-length AST arrays are already represented as `null` at
    callsites, and this prevents constructor patterns such as `(DiagOk)` from
    being reported as OOM.
  - The bounded all-slice lane is now split to the single remaining nested-let
    residual:
    `docs/plans/validation-all-slice-nested-let-residual-2026-04-11.md`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted Deduce filters listed above
    - Host targeted `diagnostics` slice: `pass=10 fail=0`
    - Host targeted `basic` slice: `pass=142 fail=0`
    - Docker-bounded all-slice rerun: unified `pass=2797 fail=1`, compiler
      `pass=208 fail=0`

- Fixed the `VALIDATION-ALL-SLICE-2026-04-11` TOML option arity cluster:
  - Registered `toml-parse` as variadic in the primitive table so both the
    one-argument and two-argument call shapes reach `prim_toml_parse`.
  - Kept the existing primitive-level validation as the source of truth for the
    accepted 1-2 argument contract.
  - Remaining all-slice validation work is now narrowed to Deduce relation
    forms, the Deduce CRUD example setup path, and Deduce rule-validation
    failures.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Targeted `data-format` slice: `pass=64 fail=0`

- Landed `FOREIGN-CORE-002F` capability-gated foreign import/member adapter
  hooks:
  - Added `FOREIGN_CAP_IMPORT` and `FOREIGN_CAP_MEMBER` capability gates for
    the internal foreign runtime adapter boundary.
  - Added `import_module` and `resolve_member` adapter slots, plus internal
    dispatch helpers `foreign_runtime_import_module` and
    `foreign_runtime_resolve_member`.
  - Kept public user-facing behavior unchanged: no new public primitive was
    added, and Python/Julia/CUDA behavior is still not wired yet.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`
    - Docker-bounded targeted `advanced-ffi-system` group: `pass=61 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FOREIGN-CORE-002E` explicit foreign adapter load/resolve hooks:
  - Split the C ABI lazy `dlopen` and `dlsym` path into adapter-dispatched
    `load_bound` and `resolve_bound` slots.
  - Kept public FFI behavior unchanged: `call_bound` reaches libffi only after
    adapter load/resolve succeeds, and existing unavailable-library/symbol
    behavior is preserved.
  - Reserved a `tensor_buffer` adapter slot behind `FOREIGN_CAP_TENSOR_BUFFER`
    for later BLAS/cuBLAS work; C ABI does not register Tensor buffer
    marshalling yet.
  - Added a public regression for post-call `foreign-describe` metadata on a
    C ABI bound function.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`
    - Docker-bounded targeted `advanced-ffi-system` group: `pass=61 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FOREIGN-CORE-002D` internal foreign runtime adapter dispatch:
  - Added a small `ForeignRuntimeAdapter` operation boundary with runtime kind,
    adapter capability bits, handle description, callable description, and
    handle release and bound-call slots.
  - Routed `foreign-describe`, `foreign-release`, and C ABI bound-function
    calls through the adapter dispatch point while preserving the current
    user-facing behavior.
  - Registered C ABI callable reflection and bound-function calls on the C ABI
    adapter. Generic handle describe/release remains available for current
    runtime kinds until dedicated Python, Julia, CUDA/cuBLAS, optional C++
    tooling, and polyglot adapters land.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=60 fail=0`
    - Docker-bounded targeted `advanced-ffi-system` group: `pass=60 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`

- Landed `FOREIGN-CORE-002C` bindgen descriptor comment alignment:
  - Replaced the raw bindgen module's bespoke `bindgen-meta-record` comment
    vocabulary with `bindgen-descriptor` comments that use the shared
    `ForeignCallable` descriptor terms: `type=ForeignCallable`,
    `runtime=c-abi`, `kind=function`, `name`, `c-name`, `parameters`,
    `returns`, `abi-type`, and user-facing `type`.
  - Renamed raw review-policy comments from `bindgen-meta` to
    `bindgen-policy` so generated raw modules distinguish descriptor-shaped
    metadata from generator safety/review hints.
  - This remains generated-code documentation only; runtime reflection still
    comes from `(foreign-describe ...)`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `compiler` slice: `pass=207 fail=0` (container libclang
      unavailable, bindgen descriptor strings that do not require libclang
      still ran)
    - Host targeted `compiler` slice with libclang: `pass=212 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FFI-GROUP-002` grouped bindgen output option:
  - Added a per-dependency `omni.toml` opt-in,
    `[dependencies.ffi.NAME] raw-syntax = "grouped"`, for `--bind` raw modules
    to emit `(define [ffi module] ...)` grouped C ABI declarations.
  - Kept the default `raw-syntax = "legacy"` path, preserving the existing
    generated `[ffi lib]` plus `[ffi lambda]` raw module shape and facade
    behavior.
  - Unsupported `raw-syntax` values fail closed before header parsing, so the
    option does not silently fall back to a different generated surface.
  - grouped raw output now includes a minimal `;; Raw syntax: grouped` header
    note to make review diffs easier to scan without changing binding
    semantics.
  - That header note also closed the narrow `FFI-BIND-003` reviewability slice;
    broader bindgen ergonomics such as generated manifests and prefix stripping
    were follow-up work at the time and later landed as `FFI-TOML-004A/B`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `compiler` slice: `pass=207 fail=0` (container libclang
      unavailable, grouped raw-syntax option check still ran)
    - Host targeted `compiler` slice with libclang: `pass=212 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FFI-GROUP-001` grouped C ABI parser/lowering:
  - Added `(define [ffi module] lib "path" (fn (^Type arg)) ^Return ...)` as
    sugar over the existing `[ffi lib]` plus `[ffi λ]` declarative FFI forms.
  - The grouped body is a flat sequence of function signature and return
    annotation pairs; malformed pairs, including missing return annotations,
    fail closed.
  - Grouped declarations reuse the existing `ForeignHandle` metadata dictionary
    validation, callable reflection path, AOT preload scan, and FFI contract
    manifest generation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
      `pass=60 fail=0`
    - Docker-bounded `compiler` slice: `pass=206 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`

- Landed `FOREIGN-CORE-002B` C ABI callable reflection:
  - Extended `foreign-describe` to accept FFI-bound functions as well as
    `ForeignHandle` values.
  - Bound C ABI callables now return a dictionary with quoted-symbol metadata
    keys including `'type 'ForeignCallable`, `'runtime 'c-abi`, `'kind
    'function`, `'name`, `'c-name`, `'library`, `'parameters`, `'returns`,
    `'resolved`, `'available`, and `'capabilities`.
  - `'parameters` is an array of descriptor dictionaries and `'returns` is a
    descriptor dictionary. Descriptors expose user-facing `'type` symbols
    (`'String`, `'Integer`, `'ForeignHandle`, etc.) plus ABI-level `'abi-type`
    symbols (`'string`, `'int`, `'ptr`, etc.); foreign-handle descriptors also
    preserve `'name`, `'ownership`, `'nullability`, and `'finalizer` metadata
    when present.
  - Capability arrays now promote inserted values to the root boundary before
    storing them, matching the root-backed dictionary/array reflection payload.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
      `pass=55 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FOREIGN-CORE-002A` explicit foreign-handle release primitive:
  - Added `foreign-release` as the shared explicit release boundary for
    releasable `ForeignHandle` payloads. It returns `Void`, is idempotent for
    already-closed handles, and rejects ordinary non-releasable library handles.
  - Refactored `ffi_handle_release` through a shared payload-release helper so
    explicit release and scope teardown agree: the finalizer/free path runs at
    most once, then `lib_handle` is cleared and shared wrappers observe
    `'live nil` through `foreign-describe`.
  - This remains a foreign-resource rule only; it does not add refcount or GC
    ownership for Omni language values.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
      `pass=53 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`
    - `git diff --check`

- Landed `FOREIGN-CORE-001` first common foreign runtime descriptor slice:
  - `FfiHandle` now carries common runtime-kind, handle-kind, and capability
    descriptors so C ABI, Python, Julia, CUDA/cuBLAS, optional C++ tooling, and
    polyglot adapters can share one small handle contract without exposing raw
    pointers to Omni code.
  - C ABI library handles are tagged as `'c-abi` / `'library` with
    load/resolve/call/reflect capabilities, while C pointer-return handles are
    tagged as `'c-abi` / `'opaque` and report release capability only when an
    owned finalizer policy exists.
  - Added `foreign-describe`, which returns a dictionary with quoted-symbol
    keys (`'type`, `'runtime`, `'kind`, `'ownership`, `'name`, `'live`,
    `'capabilities`); capability sequences are arrays to match the metadata
    shape used by forms such as `'parameters [..]`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
      `pass=51 fail=0`
    - Docker-bounded `compiler` slice: `pass=205 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`
    - `git diff --check`

- Closed `TENSOR-060B` lazy Tensor-expression protocol and destination
  materialization:
  - Extended native `Tensor` payloads with internal concrete/map/contract
    payload kinds, without adding a new public value tag or language-facing
    `TensorExpr` type.
  - Tensor-dispatched `map` and `contract` now return lazy Tensor expression
    payloads that preserve dtype/shape/rank metadata and retain their operand
    edges for later evaluation.
  - `materialize` now forces Tensor expressions either by allocating a
    concrete result or by staging evaluation into a temporary
    exact-shape/dtype destination tensor before copying the result after
    success.
  - `ref` over a Tensor expression now forces through the same materialization
    path before indexing, preserving the existing `ref` surface.
  - Destination materialization now preserves safe elementwise map aliasing and
    rejects contracted expression destinations that alias either source tensor,
    because contraction reads each operand more than once.
  - Boundary graph audit, provenance, parent-copy, root-store promotion, escape
    promotion, and JIT scope-chain checks now treat non-concrete Tensor payloads
    as graph-carrying values so expression callback/operand edges do not retain
    stale TEMP references across return/env/promotion boundaries.
  - Moved the boundary alias-reuse graph scan buffers off the C stack and onto
    heap scratch storage so effect-handler resume paths with small continuation
    stacks can classify returned payload graphs without stack overflow.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=183 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`

- Landed `TENSOR-060B-prep` destination-ready pure Tensor kernels:
  - Split eager Tensor `map` execution into a reusable
    `tensor_map_eval_into(...)` helper that validates the destination tensor
    and stages elementwise results before committing them into caller-
    provided storage after success.
  - Split eager Tensor `contract` execution into result-shape planning and a
    reusable `tensor_contract_eval_into(...)` helper that stages the pure C3
    fallback contraction before copying it into caller-provided storage after
    success.
  - Kept public behavior eager and unchanged for now: `map` and `contract`
    still return concrete `Tensor` values, while the runtime now has the
    internal kernel boundary needed by a later lazy tensor-expression value,
    destination `materialize`, and optional BLAS/cuBLAS dispatch.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=179 fail=0`

- Closed `TENSOR-060A` destination-fusion audit for the Tensor scientific
  surface:
  - Confirmed that true `(materialize (map ...) out)` and
    `(materialize (contract ...) out)` fusion cannot be implemented honestly
    on top of the current eager concrete `Tensor` return path: by the time
    `materialize` receives its source argument, `map` or `contract` has already
    allocated the temporary source tensor.
  - Split the remaining fusion work into `TENSOR-060B` lazy tensor-expression
    protocol and destination fusion. The recommended path is a native
    tensor-expression value rather than turning `materialize` into a macro or
    exposing public `map-into` / backend-flavored escape names.
  - Hardened current `contract` coverage while keeping the eager fallback
    semantics: list axes, zero-axis outer product, right-axis bounds,
    non-tensor operands, malformed axis containers, and non-integer axes are
    now pinned in the advanced collections module tests.
  - Updated the Tensor plan and live TODO so the backlog tracks the real
    semantic boundary before backend acceleration work.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=179 fail=0`

- Closed `TENSOR-050` eager Tensor `contract` for the scientific surface:
  - Registered public `contract` as the canonical summed-axis tensor
    contraction operation over native `Tensor` operands.
  - Implemented the pure C3 `Double` fallback for
    `(contract a b left-axes right-axes)`, including rank-2 matrix-product
    behavior, rank-0 scalar dot-product results, and multi-axis contractions.
  - Added deterministic validation for non-tensor operands, malformed axis
    lists, axis-list length mismatch, out-of-range axes, duplicate contracted
    axes, dtype mismatch, and paired contracted-dimension mismatch.
  - Kept backend acceleration out of the public surface: BLAS/LAPACK/CUDA/cuBLAS
    remain optional optimizations behind `contract`/`materialize`, not new
    canonical operation names.
  - Updated the Tensor plan, language spec, type-system docs, reference docs,
    and live TODO so the next slice is expression/destination optimization
    planning behind the pure fallback.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=179 fail=0`
    - targeted `advanced-stdlib-numeric-collections` subgroup:
      `pass=62 fail=0`
    - targeted `compiler` slice: `pass=209 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`
    - host `c3c build --warn-deprecation=no` refresh after Docker validation

- Closed `TENSOR-040` eager Tensor `map` for the scientific surface:
  - Registered internal `__tensor-map` as the Tensor elementwise execution
    primitive and extended generic `map` through fixed-arity `^Tensor` methods
    for unary tensor, tensor-scalar, scalar-tensor, and exact-shape
    tensor-tensor `Double` inputs.
  - Tensor `map` now returns concrete `Tensor` values in this first slice,
    preserving the tensor-expression contract while deferring lazy expression
    nodes and singleton-axis broadcasting.
  - Added deterministic validation for noncallable callbacks,
    non-tensor/nonnumeric sources, tensor shape mismatch, tensor dtype
    mismatch, and nonnumeric callback results through `tensor/shape-mismatch`
    and `tensor/dtype-mismatch` payloads where appropriate.
  - Updated the compiler stdlib prelude so compiled programs see the same
    Tensor `map` method surface as the runtime stdlib, and adjusted typed
    define lowering assertions to count only the tested user method names now
    that the prelude contains typed `map` methods.
  - Updated the Tensor plan, language spec, type-system docs, reference docs,
    and live TODO so the next slice is `TENSOR-050` pure `contract`; backend
    acceleration remains behind the pure fallback.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=163 fail=0`
    - targeted `advanced-stdlib-numeric-collections` subgroup:
      `pass=62 fail=0`
    - targeted `compiler` slice: `pass=209 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`
    - host `c3c build --warn-deprecation=no` refresh after Docker validation

- Closed `TENSOR-030` concrete Tensor materialization for the scientific
  surface:
  - Registered `materialize` as the tensor expression-to-storage boundary for
    concrete tensor sources.
  - Implemented `(materialize tensor)` as identity for already-concrete
    tensors, `(materialize tensor out)` as an exact-shape/dtype destination
    copy that returns `out`, and `(materialize scalar out)` as scalar fill into
    the mutable destination tensor.
  - Added deterministic destination validation for missing tensor backing
    storage, immutable destinations, dtype mismatch, shape mismatch, and
    non-tensor sources/destinations. Aliasing the same tensor destination is a
    no-op in this first slice.
  - Updated the Tensor plan, language spec, type-system docs, and live TODO so
    the next slice is `TENSOR-040` elementwise `map`; `contract` and backend
    acceleration remain behind later pure-surface-compatible slices.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=154 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`

- Closed `TENSOR-020` constructor and indexing for the Tensor scientific
  surface:
  - Registered `Tensor` as a callable constructor while preserving
    `(format "%s" Tensor)` as the `#<type Tensor>` descriptor print surface.
  - Implemented `(Tensor Double shape data-or-scalar)` for native double
    tensors. Shape accepts arrays or proper lists of non-negative integers;
    data accepts a numeric scalar fill or an array/proper list with exactly
    the shape product's element count.
  - Implemented `(ref tensor index-array)` through the existing generic `ref`
    dispatcher, with array/proper-list indices, rank checking, per-axis
    negative indexing, and row-major stride offset calculation.
  - Updated the Tensor plan, language spec, type-system docs, and live TODO so
    the next slice is `TENSOR-030` materialization/expression protocol rather
    than constructor work.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-unicode-iterator` subgroup: `pass=150 fail=0`
    - targeted `advanced-collections-module` subgroup: `pass=147 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`

- Closed `TENSOR-010` runtime representation for the Tensor scientific surface:
  - Added a native `Tensor` runtime value tag and `TensorVal` payload with
    dtype, rank, shape, strides, element count, byte length, mutability, view
    flag, and contiguous data storage.
  - Implemented deterministic scope-owned tensor payload allocation and
    destruction, plus fail-closed allocation fault paths for tensor metadata
    and storage.
  - Registered `Tensor` as a builtin type descriptor for type identity and
    dispatch; constructor/indexing landed later in `TENSOR-020`.
  - Added `tensor?`, `dtype`, `shape`, `rank`, and `length` support, with
    tensor printing as `#<tensor Double shape [...]>`.
  - Integrated tensor payloads into boundary copy, escape promotion, root-store
    cloning, env-copy, provenance, JIT scope-chain checks, and graph audit
    leaf handling so tensors deep-clone shape/stride/data storage across
    ownership boundaries.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-unicode-iterator` subgroup: `pass=142 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`
    - `git diff --check`

- Closed `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105`:
  - Added AOT bridge policy descriptors for generated declarative `ffi λ`
    declarations.
  - AOT generated code now emits `AotFfiHandlePolicySpec` arrays for
    parameter handle policy and a return-policy descriptor when the source
    annotation carries non-default `ForeignHandle` metadata.
  - The AOT runtime bridge converts descriptors into `FfiHandlePolicy`,
    preserves parameter handle family/nullability, preserves return handle
    name/ownership/finalizer, resolves owned-return finalizers with `dlsym`,
    and rejects owned handle parameter policies fail-closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=209 fail=0`
    - `git diff --check`

- Closed `AUDIT-FFI-FOREIGN-HANDLE-METADATA-104`:
  - Implemented the interpreter/JIT ForeignHandle metadata dictionary policy so
    `^ForeignHandle` stays the simple default foreign-handle annotation while
    FFI-local dictionaries can refine it.
  - Accepted the FFI-local refinements:
    `^{'name File 'ownership owned 'finalizer fclose}` implies
    `ForeignHandle`, and explicit `^{'type ForeignHandle ...}` is also
    accepted.
  - Clarified that Omni dictionaries use quoted-symbol key/value pairs and do
    not use colon keywords.
  - Runtime FFI now enforces handle family and non-null policy at call packing,
    resolves owned return finalizers with `dlsym`, and wraps pointer returns in
    named `FFI_HANDLE` boxes.
  - Added FFI AST serialization for declarative FFI forms so compiler
    roundtrip coverage preserves the metadata dictionary spelling.
  - Split AOT policy propagation into
    `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105`; that follow-up is now closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=204 fail=0`

- Closed `AUDIT-FFI-FOREIGN-HANDLE-SURFACE-103`:
  - Replaced the public FFI raw-pointer annotation surface with `^ForeignHandle`
    across runtime annotation mapping, bindgen output, compiler manifest text,
    tests, current-state docs, and tracking notes.
  - Documented `^ForeignHandle` as the opaque foreign-handle annotation that accepts
    live `FFI_HANDLE` values or `nil`, not raw integer addresses.
  - Removed raw integer address coercion from foreign-handle call packing.
  - Wrapped non-null foreign-handle returns in non-owning `FFI_HANDLE` values
    instead of exposing raw integer addresses.
  - Updated bindgen-facing code/docs so opaque foreign C values now map to
    `^ForeignHandle`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=43 fail=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - `git diff --check`

- Closed `AUDIT-IMMER-FFI-COMPAT-101` by retiring the optional Immer bridge:
  - Deleted the unsupported `lib/immer.omni` wrapper and the `lib/immer/`
    C++ bridge tree, including the tracked nested `lib/immer/immer` gitlink.
  - Deleted the obsolete `docs/plans/immer-ffi-compat-plan-2026-04-11.md`
    compatibility plan.
  - Recorded the owner decision that C++/Immer support is not core language
    infrastructure, so no `^Value`, automatic value-handle, or pointer-only
    FFI rewrite was added for this legacy optional library.
  - validation:
    - active source/reference search confirms no supported surface references
      remain outside historical TODO/changelog/plans.
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- Closed `AUDIT-PROCESS-WRAPPER-PAYLOAD-FALLBACK-100`:
  - Added untyped stdlib fallbacks for `process-spawn` and `process-kill` so
    invalid argument shapes still flow through the canonical `io/process-*`
    payload error lane, matching the surrounding I/O wrapper pattern.
  - Added regression coverage for invalid process command and signal arguments
    to verify runtime payload codes instead of generic typed-dispatch failures.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=67 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Closed `AUDIT-LIST-HELPER-ALIAS-086`:
  - Kept `List` as the canonical list constructor/conversion surface.
  - Explicitly approved lowercase `list` as an idiomatic Lisp
    list-builder/conversion helper rather than treating it as an unapproved
    constructor alias.
  - Runtime primitive registration and compiler primitive hash coverage already
    route `List` and `list` through the same implementation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- Closed `AUDIT-IMMER-PERSISTENT-DISPATCH-088`:
  - Added tagged Omni wrapper dictionaries around optional Immer bridge
    handles so public persistent collection operations unwrap by expected
    family before calling the C++ bridge.
  - Added `persistent-array?`, `persistent-dictionary?`, and
    `persistent-set?` predicates while keeping generic `count`/`conj`/`into`
    dispatch frozen.
  - Made `persistent-dictionary` reject odd key/value argument lists instead of
    silently dropping the final key.
  - Split residual runtime bridge compatibility into
    `AUDIT-IMMER-FFI-COMPAT-101` because `lib/immer.omni` still uses the
    retired `ffi-declare` / `(ffi "...")` surface and current declarative FFI
    cannot truthfully pass arbitrary Omni value payloads as `void*`.
  - historical validation before the bridge was retired:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/immer.omni`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - runtime preload remains blocked by `AUDIT-IMMER-FFI-COMPAT-101`
      because `ffi-declare` is no longer bound.
    - local bridge build remains blocked on missing Immer headers:
      `make -C lib/immer test_bridge` fails on `immer/flex_vector.hpp`.

- Closed `AUDIT-LET-BRACKET-SHORTHAND-102`:
  - Removed legacy outer `let [...]` shorthand from live library code.
  - `lib/core.omni` macro expansions now emit flat-pair `let` and named `let`
    binding lists.
  - `lib/test-utils.omni` now uses flat-pair `let` binding syntax.
  - The only remaining `let [` text matches are the syntax decision note and
    the negative parser regression that verifies the shorthand is rejected.
  - validation:
    - `rg "\(let\s*\[" -n lib stdlib tests examples docs src` returns only
      the syntax decision note and the negative parser regression.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/test-utils.omni`
    - bounded `basic` slice: `pass=142 fail=0`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- Closed `AUDIT-FILESYSTEM-SURFACE-087`:
  - Selected `fs-*` as the canonical filesystem wrapper/primitive family and
    removed the remaining stdlib `filesystem-*` compatibility aliases.
  - Added regression coverage that verifies removed long-form filesystem aliases
    are no longer bound while the canonical `fs-*` roundtrip still works.
  - Updated reference docs and the audit plan so the filesystem surface is no
    longer described as pending or compatibility-backed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=65 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Closed `AUDIT-COMPILER-PRIMITIVE-HASH-COVERAGE-099`:
  - Completed public runtime primitive value-position coverage in the compiler
    primitive hash table so public primitives are lowered through
    `aot::lookup_prim(...)` instead of drifting into closure-captured C3 local
    identifiers.
  - Increased the primitive hash table size to keep the now-complete registry
    load comfortably below 50% and updated the stale sizing comment.
  - Extended the closure-capture regression to cover the broader primitive
    families that were previously underrepresented: math (`sin`), string
    utilities (`string-byte-length`), and collection helpers (`sort-by`).
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - audit script: `hash entries=195`, `focus missing public-ish hash entries=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Closed `AUDIT-COMPILER-PRIMITIVE-CLASSIFICATION-098`:
  - Replaced the duplicated compiler free-variable/delegation primitive-name
    arrays with a shared hash-backed classifier, making the compiler primitive
    hash table the single value-lowering source for primitive/literal symbols.
  - Preserved legacy non-hash classification exceptions for `ForeignHandle`
    and `__ui-ftxui-run`, which were already excluded from closure capture but
    do not have normal AOT value lowering entries.
  - Extended the closure-capture regression to cover hash-only primitives such
    as `Dict` and `json-parse`, preventing drift from reintroducing accidental
    captured C3 identifiers.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Closed `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096`:
  - Added a shared compiler symbol-value emission helper so variable lowering
    and closure capture initialization both route primitive symbols through the
    registered primitive cache name instead of emitting raw C3 identifiers.
  - Added missing compiler primitive/free-variable/hash coverage for runtime
