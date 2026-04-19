# Active TODO Index Part 04

Source: `TODO.md`

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
