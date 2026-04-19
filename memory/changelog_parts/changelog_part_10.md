# Memory Changelog Index Part 10

Source: `memory/CHANGELOG.md`

  - Owned-path release behavior is still driven by explicit finalizer
    authority; manual-return handles remain non-releasable.
  - AOT declaration policy now rejects manual ownership combined with a
    finalizer, matching the interpreter metadata parser.
  - `(foreign-describe)` now reflects manual ownership for returned handles as
    `'ownership manual` instead of implicitly downgrading to borrowed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`

- Landed `FOREIGN-CORE-002L` AOT finalizer/ownership parity:
  - `aot_ffi_policy_from_spec` now rejects return policies that carry a finalizer
    unless ownership is explicitly `owned`.
  - This closes the AOT/JIT gap where borrowed/default finalizer-bearing
    returns were accepted by AOT policy parsing but ignored by finalizer
    resolution.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=258 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - `git diff --check`

- Landed `FOREIGN-CORE-002M` finalizer-owned foreign payload release hardening:
  - Finalizer-backed handle families now free their own heap payloads after
    running resource-specific teardown because `make_ffi_box` makes native
    finalizers authoritative when a finalizer is present.
  - Covered payload wrappers include UV timer callback, process, signal,
    TCP, UDP, FS stream, TLS, deduce relation, deduce database, and deduce
    transaction handles.
  - `uv-timer-callback-unhandle` remains a non-destructive detach operation, so
    stale-handle diagnostics still work until the owning `ForeignHandle` is
    destroyed or explicitly released.
  - Deduce relation scan column-key-value forced-OOM handling now fails before
    allocating the relation cache, avoiding a sanitizer-visible test-only leak
    on the injected OOM path.
  - Validation:
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - ASAN targeted `deduce` slice: `pass=330 fail=0`
    - final `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - `scripts/check_status_consistency.sh`
    - `git diff --check`

- Landed `FOREIGN-CORE-002N` foreign handle construction-failure ownership:
  - `make_ffi_handle_ex_with_descriptor` now treats the constructor as the
    failure-path release authority for finalizer/free-backed non-library
    payloads once it accepts the raw handle.
  - Box-allocation and value-allocation failures both release owned foreign
    payloads through the same finalizer/free policy used by normal
    `ForeignHandle` teardown.
  - C ABI library handles remain non-releasable: constructor failure does not
    `dlclose` them, so the existing `dlopen` caller cleanup path stays
    authoritative.
  - Owned C FFI pointer returns no longer run a second caller-side finalizer
    after constructor failure.
  - TLS client/server handle wrapper OOM paths now use `tls_handle_finalizer`
    for connected initialized handles, preserving graceful close/session logic
    before freeing nested TLS storage.
  - Allocation-failure tests now pin exactly-once finalizer cleanup for
    finalizer-backed FFI payloads after both box-allocation and
    value-allocation failure.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - host targeted `compiler` slice: `pass=258 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - ASAN targeted `deduce` slice: `pass=330 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=204 fail=0`
    - final `c3c build --warn-deprecation=no`
    - `scripts/check_status_consistency.sh`
    - `git diff --check`

- Landed `FOREIGN-CORE-002O` constructor-failure caller cleanup alignment:
  - Removed redundant caller-side cleanup after `make_ffi_handle_ex` null
    returns now that the constructor owns finalizer/free-backed non-library
    payload release on failure.
  - Covered async TCP/UDP/process/signal/UV timer callback/FS stream handles,
    scheduler task/thread handles, atomic refs, TLS wrapper error paths, deduce
    database/transaction/relation handles, C ABI/AOT library registration
    failure paths, and boundary helper tests.
  - C ABI/AOT library registration failures now consume the constructed wrapper
    safely by clearing the root-scoped value pointer before releasing the
    wrapper box after closing the raw library handle.
  - Added advanced FFI regressions for malformed C ABI library and opaque
    descriptors with `FOREIGN_CAP_RELEASE`: `foreign-release` still rejects
    them as non-releasable, the malformed opaque payload stays live, and the
    opaque descriptor is not reflected as owned without finalizer/free release
    authority.
  - `foreign-describe` no longer reports released handles as owned after the
    payload is cleared and release capability has been removed.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=74 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=207 fail=0`

- Landed `TENSOR-060C` tensor shape/zero-contraction hardening:
  - Tensor shape parsing now checks non-negative dimensions with an unsigned
    comparison, avoiding the 64-bit `(long)usz.max` wraparound that rejected
    valid shapes such as `[2]` and `[2 3]`.
  - `contract` handles zero-size contracted axes by producing the additive
    identity instead of reaching a divide/modulo-by-zero path.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-collections-module` group: `pass=185 fail=0`
    - direct probes for `(Tensor Double [2] [1 2])`, tensor `ref`, and
      zero-size contracted scalar output.

- Landed `TENSOR-060D` materialize edge hardening:
  - Added regressions for concrete `Tensor Double []` materialization into
    rank-0 destination tensors, scalar materialization into `[0]` destination,
    lazy zero-size source materialization into zero-size destination, aliased
    elementwise `map` materialization into its destination, and duplicate-axis
    detection after negative-axis normalization.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-collections-module` group: `pass=196 fail=0`

- Landed `TENSOR-110` Tensor cleanup surface lane closure:
  - `examples/scicomp_demo.omni` now uses canonical `Tensor`, `map`,
    `contract`, and `materialize` forms, with the legacy `vec-*`, `mat-*`, and
    `mat-mul` prototypes removed from the canonical public surface.
  - `Tensor` now returns lazy expression payloads for `map`/`contract`, and
    `materialize` is the explicit expression-to-storage boundary used in this
    cleanup lane.
  - Added regression coverage for lazy expression returns and closure capture
    under map/contract execution paths.
  - Metadata-shape/index payload examples intentionally remain `Array`-based to
    preserve the existing metadata transport convention while closing this lane.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` tensor coverage: `pass=200 fail=0`
    - `git diff --check`

- Landed `FOREIGN-CORE-002I` borrowed-handle release regression alignment:
  - The borrowed-return-handle release regression now binds the real C `fopen`
    symbol with borrowed `ForeignHandle` metadata before closing it through
    `fclose` and checking that `foreign-release` rejects the non-releasable
    borrowed wrapper.
  - This keeps the advanced FFI ownership regression on the intended safety
    path instead of accidentally probing a nonexistent `fopen_borrowed` symbol.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=66 fail=0`

- Landed `FFI-BIND-005A` bindgen return metadata fail-closed:
  - `bindgen_append_function_decl` and
    `bindgen_append_grouped_function_decl` now set failure state when return
    Omni metadata is unsafe (for example, contains control characters).
  - Mixed emitted-function lists now fail closed on an unsafe return-metadata
    entry and do not write any raw/facade output.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=253 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-005B` bindgen stale-output cleanup fail-closed:
  - `bindgen_delete_output_file_if_exists` now returns deletion failure status
    and `generate_ffi_module_pair_with_options` fail-closes if it cannot remove a
    newly written raw artifact when facade generation fails.
  - Existing raw artifacts are preserved on rerun failures so bindgen does not
    delete previously generated files outside the first-write failure lane.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=256 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-005C` bindgen metadata control-character hardening:
  - `bindgen_metadata_text_is_safe` now rejects all ASCII control characters
    below `0x20` in metadata fields and generated comments, covering non-newline
    controls such as tabs.
  - Overflow-guard metadata regression now includes a tab control character case
    and validates fail-closed behavior before any generated output is written.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=257 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-004V` anonymous bindgen parameter fallback naming:
  - Anonymous C parameter fallback names now format the full numeric index
    (`arg123`, etc.) instead of only supporting one- or two-digit indexes.
  - This keeps high-arity generated bindings from producing invalid fallback
    parameter symbols once an anonymous parameter index reaches three digits.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=253 fail=0`

- Landed `FFI-BIND-004U` atomic bindgen text writes:
  - Raw, facade, and manifest writers now write to a sibling temp path and
    rename into place only after the full text write and close succeeds.
  - Failed final renames clean the temp output, preventing writer-level
    partial/truncated generated files from replacing the target.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=249 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-004T` manifest failure cleanup:
  - If raw/facade generation succeeds but the manifest write fails, first-time
    raw, facade, and manifest artifacts are cleaned before the dependency fails.
  - Existing raw, facade, or manifest artifacts are left in place on rerun
    failures.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=248 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-004S` raw/facade pair failure cleanup:
  - If a bindgen raw module is newly written but facade generation fails, the
    new raw file is removed before the pair writer returns failure.
  - Existing raw files are left in place on rerun failures, avoiding deletion
    of a previously generated or user-reviewed raw artifact.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=247 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004R` section-header comment/context hardening:
  - `[dependencies.ffi.NAME] # comment` now parses as the intended FFI
    dependency section instead of falling through as a non-section line.
  - Malformed section-header lines starting with `[` reset parser context so
    following keys cannot mutate the previously active dependency section.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=246 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004Q` partial header parse cleanup:
  - `--bind` now releases functions parsed from earlier headers when a later
    header fails to parse or trips the header-path guard.
  - Partial multi-header parse failures still fail before output generation,
    but no longer leave parsed parameter metadata behind on the error path.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=244 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004P` explicit function filter match enforcement:
  - When `[dependencies.ffi.NAME] functions = [...]` is present, every
    requested function name must be discovered in the parsed headers.
  - Missing filter entries now fail the dependency before output generation
    instead of silently producing a partial binding set or a successful no-op.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=243 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004O` unsafe bind library stem rejection:
  - `--bind` now rejects dependency `library` stems containing slash,
    backslash, quote, whitespace, or control characters before header parsing
    or generated raw/facade/manifest output.
  - The lower-level bindgen writers also reject unsafe library stems, keeping
    direct generator calls aligned with the `omni.toml` bind path.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=242 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004N` unknown FFI dependency key rejection:
  - Unknown keys in `[dependencies.ffi.NAME]` now fail the dependency before
    libclang/header parsing instead of being ignored. This keeps typos such as
    `function = [...]` from accidentally binding all exported functions.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=241 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004M` malformed adjacent quoted scalar rejection:
  - Strict FFI dependency scalar parsing now rejects values such as
    `library = "sqlite3" "m"` instead of accepting the whole malformed tail as
    one library or raw-syntax string.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=240 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004L` dependency count overflow fail-closed:
  - FFI dependency parsing now records when `[dependencies.ffi.NAME]` sections
    exceed `TOML_MAX_DEPS` and `--bind` exits before raw syntax resolution,
    header parsing, or output generation.
  - Overflow sections reset parser section state so their keys cannot mutate
    the last accepted dependency.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=239 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004K` inline comment handling for strict bind TOML values:
  - FFI dependency value parsing now strips `#` comments only when the hash is
    outside a quoted string, so documented examples such as
    `library = "m" # comment` remain valid while quoted values containing `#`
    are preserved.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=238 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FFI-TOML-004J` duplicate FFI dependency section rejection:
  - Repeated `[dependencies.ffi.NAME]` sections now mark both dependencies
    invalid instead of letting multiple entries target the same generated raw,
    facade, and manifest output stem.
  - `--bind` now rejects invalid or empty dependency names before raw syntax
    resolution and header parsing.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=237 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FFI-TOML-004I` duplicate bind dependency TOML key rejection:
  - FFI dependency `library`, `raw-syntax`, `headers`, `functions`, and
    `strip-prefixes` keys now fail closed when repeated in the same
    `[dependencies.ffi.NAME]` section instead of letting a later value silently
    overwrite the bind target, function filter, syntax mode, or generated-name
    rewrite policy.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=236 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FFI-TOML-004H` strict bind dependency core TOML fields:
  - FFI dependency `library`, `raw-syntax`, `headers`, and `functions` parsing
    now fails closed for missing required, malformed, empty, overlong, or
    incorrectly shaped values instead of silently truncating, accepting missing
    required fields as no-ops, or accepting malformed arrays before header
    parsing.
  - This keeps `--bind` from generating against the wrong shared library,
    using an unquoted raw syntax selector, parsing a truncated header path, or
    applying a truncated function filter.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=231 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FFI-TOML-004G` strict `strip-prefixes` TOML parsing:
  - `strip-prefixes` now rejects malformed, empty, or overlong entries instead of
    silently truncating prefix strings before generated-name rewriting.
  - `--bind` fails the dependency before header parsing when `strip-prefixes`
    is invalid, keeping the generated raw/facade/manifest output contract
    fail-closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=224 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004F` prefix-stripped emitted name collision preflight:
  - Bindgen now rejects duplicate generated Omni-facing function names before
    writing raw or facade files.
  - This catches cases where prefix stripping makes two C functions collide,
    such as `sqlite3_open` and `open` both emitting `open` when `sqlite3_` is
    stripped.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=221 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004E` prefix-stripped emitted name preflight:
  - Bindgen now validates generated Omni-facing function names before writing
    raw or facade files.
  - Prefix stripping now fails closed when it would emit a name that the Omni
    lexer would read as a number-leading token, such as stripping `sqlite3_`
    from `sqlite3_3d_distance`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=220 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004D` overlong bind dependency section names fail closed:
  - `toml_parser` no longer silently truncates `[dependencies.ffi.NAME]`
    section names longer than the bind output stem limit.
  - Overlong names now flow into the existing empty-name output guard and fail
    before raw, facade, or manifest files are written, avoiding surprising file
    stems or generated Omni module names.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=219 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004C` path-safe bind output dependency names:
  - `--bind` output path construction now rejects empty FFI dependency names
    and names containing anything outside ASCII letters, digits, `_`, and `-`
    before writing raw, facade, or manifest files under `lib/ffi/`. This keeps
    dependency names safe as both file stems and generated Omni module names.
  - Bind path failures now report "too long or unsafe" instead of treating all
    failures as length overflow.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=218 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BUFFER-009C` string-buffer `inout` direction precedence:
  - Bindgen string-buffer direction inference now checks the `inout` name hint
    before the broader `out` substring.
  - Parameters such as `inout_buffer` now generate `buffer-direction=inout`
    instead of being misclassified as output-only buffers.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=217 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-010C` char-pointer depth classification:
  - Bindgen now treats only single-level plain `char*` pointers as
    string-shaped.
  - `char**`, `const char**`, and similar pointer-to-pointer spellings remain
    opaque `ForeignHandle` values instead of string inputs, buffers, or returns.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=217 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-010B` const-pointee char-pointer classification:
  - Bindgen now distinguishes pointee const from top-level pointer const when
    classifying plain `char*` types.
  - `const char*` and `char const*` stay string-input shaped, while
    `char* const` remains a mutable string-buffer contract that requires the
    generated fail-closed buffer helper path.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=217 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-010A` byte-pointer bindgen classification:
  - libclang bindgen now treats only a plain `char` token as string-shaped.
  - `signed char*`, `unsigned char*`, and their `const` variants remain opaque
    `ForeignHandle` pointers instead of being classified as string
    buffers/returns.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=217 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BUFFER-009B` string-buffer none-policy guards:
  - Generated `none` teardown mutable string-buffer helpers now validate role
    and ownership before returning the caller-owned buffer.
  - This keeps the pass-through path narrow instead of relying only on the
    top-level teardown dispatch and size/direction validation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=216 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BUFFER-009A` string-buffer manual-review fail-closed cleanup:
  - Generated mutable string-buffer helpers now raise on `manual-review`
    teardown until the facade is edited with an explicit allocation and
    writeback policy.
  - `none` teardown buffer helpers remain caller-owned pass-through values
    after validation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=215 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-CALLBACK-007B` callback cleanup idempotence:
  - Generated callback unregister helpers now keep nil cleanup as a true
    idempotent no-op by nesting the metadata validation and fail-closed shim
    requirement under the non-nil branch.
  - Non-nil generic callback handles still fail closed until a concrete
    subsystem callback-handle shim is supplied.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=215 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-CALLBACK-007A` fail-closed generic callback scaffolds:
  - Bindgen-generated callback wrapper helpers now validate callback metadata
    but raise until the facade is edited for a concrete subsystem
    callback-handle shim.
  - Generic bindgen output no longer routes arbitrary C callback parameters
    through the `uv` timer callback prototype.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=215 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FOREIGN-CORE-002H` opaque handle name reflection cleanup:
  - `(foreign-describe handle)` now returns opaque foreign-resource handle
    families such as `File` as symbols under `'name`, matching
    `^{'name File ...}` metadata and callable return descriptors.
  - C ABI library handles keep their soname/path `'name` as a string because
    that field is the `dlopen` target, not a resource-family symbol.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FOREIGN-CORE-002G` C ABI string-return cleanup:
  - Fixed the interpreter/AOT shared FFI call path so non-null `^String`
    returns copy the returned C `char*` into an Omni `String` instead of
    exposing the foreign address as an `Integer`.
  - Null C string returns still map to `nil`, matching the documented
    `^String` contract.
  - Added runtime regression coverage for `strchr` string and null returns.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - final `c3c build --warn-deprecation=no`
