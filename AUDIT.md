# Omni Audit Ledger

Purpose: durable accumulator for bugs, regressions, antipatterns, and verification
risks found during codebase audits. New findings should be appended here with a
stable ID, evidence, impact, and next action. Do not remove a finding unless the
fix has landed and the validation command is recorded.

Last audit pass: 2026-04-25
Auditor: GPT-5 Codex

## Verification Snapshot

- `c3c -C build`: passed.
- `scripts/check_e2e_baseline_policy.sh`: failed.
- `scripts/check_file_size_gate.sh`: failed.
- `scripts/check_status_consistency.sh`: passed on the follow-up audit pass.
- `scripts/check_build_config_parity.sh`: passed on the follow-up audit pass.
- Follow-up source audit: C string boundary scan, macro conversion scan, TLS
  arity scan, and shell primitive inspection completed.
- Follow-up source audit: primitive arity table scan, filesystem variadic call
  scan, and system/random primitive range checks completed.
- Follow-up source audit: data-format temporal validation, JSON conversion
  error handling, and CSV quoted-field parsing completed.
- Follow-up source audit: Deduce tuple codec storage boundaries and compression
  decompression size-growth paths completed.
- Follow-up source audit: `when`/`unless` stdlib, compiler, docs, and direct
  runtime parity inspected.
- Follow-up source audit: stdlib macro parity between runtime `stdlib.lisp` and
  the AOT `STDLIB_PRELUDE` inspected.
- Follow-up source audit: documented stdlib function parity between runtime
  `stdlib.lisp` and the AOT `STDLIB_PRELUDE` inspected.
- Follow-up source audit: string delimiter contracts and symbol table growth
  bounds completed.
- Follow-up source audit: compile-output ownership and error propagation
  inspected.
- `AUDIT_2.md` candidate merge inspected; verified unique items promoted and
  stale C3 switch-fallthrough candidates deduped/invalidated.
- Fix pass closed `AUDIT-149` through `AUDIT-151`, `AUDIT-154`, `AUDIT-157`,
  and `AUDIT-158` through `AUDIT-160`; scoped module open is now visible to
  effect explanation and Deduce demand extraction, stdlib `when`/`unless`
  multi-body expansion is runtime/compiler-visible, AOT allocation and result
  ownership paths fail closed, CLI C-string boundaries reject overlong
  arguments, IO parity workflow script exists, and helper C builds honor `$CC`.
- Fix pass closed `AUDIT-202` through `AUDIT-207`; deferred Deduce commit
  validation and fact-reference scans now fail closed on storage/decode/cursor
  failures, nested AOT FFI declarations reach embedded contract JSON, stale
  compile sidecar unlink failures are reported, REPL clone-start failure rolls
  back sessions, and `main` reports scheduler shutdown failure.
- Follow-up source audit: UTF-8 index consistency and Pika regex result-builder
  error propagation completed.
- Follow-up source audit: shared-handle projection lifetimes and Pika regex
  empty-match/split edge contracts completed.
- Follow-up source audit: reader-tag hash dispatch, data reader tag primitives,
  and TimePoint UTC formatting completed.
- Follow-up source audit: AOT data-reader primitive parity and JSON Pointer
  lookup failure/side-effect paths completed.
- Follow-up source audit: JSON emit dictionary key normalization and CSV
  delimiter/quote-style edge contracts completed.
- Follow-up source audit: `sort`/`sort-by` list-size and comparator contracts
  completed.
- Follow-up source audit: dictionary/set iteration ordering, list-to-array
  conversion bounds, and manual gensym uniqueness completed.
- Follow-up source audit: async read option validation, stream read caps, and
  HTTP response shape validation completed.
- Follow-up source audit: FFI callback cons-failure propagation, shared dlopen
  capacity preflight, string primitive contract hardening, and async read option
  public-wrapper parity completed.
- Follow-up source audit: scope-region reset/shutdown hygiene, native BLAS/
  LAPACK/Vulkan loader hardening, CSV quoted-empty-field parsing, and manual
  gensym counter formatting completed.
- Follow-up source audit: iterator range overflow and iterator materialization
  caps completed.
- Follow-up source audit: FFI callback string argument and return conversion
  boundaries completed.
- Follow-up source audit: Unicode grapheme/codepoint materialization caps
  completed.
- Follow-up source audit: ML visualization multi-series disambiguation and
  terminal zoom-bound propagation completed.
- Follow-up source audit: Kernel capture validation parity and tensor graph-run
  scratch allocation paths completed.
- Follow-up source audit: scheduler task/thread batch cleanup paths completed.
- Follow-up source audit: async FFI string argument boundary ownership
  completed.
- Follow-up source audit: `random-int` sampling uniformity completed.
- Follow-up source audit: numeric conversion boundaries, exact/inexact
  comparison precision, and arbitrary-precision `Number` helper consumers
  completed.
- Follow-up source audit: NN checkpoint scalar decode and dictionary encoding
  capacity behavior completed.
- Follow-up source audit: NN checkpoint dictionary decode sizing, shared
  checkpoint load diagnostics, and JSON object parse capacity bounds completed.
- Follow-up source audit: CLI project scaffolding paths, REPL project preload
  path parity, and compile-side FFI sidecar failure cleanup completed.
- Follow-up source audit: formatter write atomicity, AOT temp-file cleanup, and
  JSON UTF-8 escaping consistency completed.
- Follow-up source audit: module import publication rollback, retry semantics,
  and nested-load module-table pointer stability completed.
- Follow-up source audit: REPL TCP tooling docs, process signal validation, and
  process wait status/error ordering completed.
- Follow-up source audit: async sleep/offload duration validation, offload job
  argument-tail validation, and scheduler task/thread join completion
  preservation completed.
- Follow-up source audit: stack-context clone relocation math and residual
  source-stack pointer detection completed.
- Follow-up source audit: AOT primitive resolver parity and Base64 decoded-size
  arithmetic completed.
- Follow-up source audit: ML image export publication and atomic-ref FFI handle
  family validation completed.
- Follow-up source audit: FTXUI renderer callback ownership and high-level
  button message lowering completed.
- Follow-up source audit: FTXUI session event buffering and event text payload
  sizing completed.
- Follow-up source audit: Pika `re-replace` contract drift and regex cache
  lock scope completed.
- Follow-up source audit: Pika parser-state and TIE user-view lifetime paths
  completed.
- Follow-up source audit: Pika named grammar replacement and finalization
  cleanup paths completed.
- Follow-up source audit: Pika grammar unresolved-reference and compile-error
  cleanup paths completed.
- Follow-up source audit: filesystem handle read caps and directory-entry
  encoding completed.
- Follow-up source audit: process/signal variadic edge contracts and
  `read-lines` empty-file line materialization completed.
- Follow-up source audit: pipe listener path mutation and TCP listener backlog
  policy completed.
- Follow-up source audit: TCP IPv6 resolver parity and HTTP request-builder
  fail-closed behavior completed.
- Follow-up source audit: TLS session-cache lifecycle and Float64 dictionary-key
  hashing completed.
- Follow-up source audit: schema effect explanation scoped-module traversal
  completed.
- Follow-up source audit: Deduce query demand extraction across scoped module
  open completed.
- Follow-up source audit: AOT `when`/`unless` synthetic block allocation
  failure paths completed.
- Follow-up source audit: stable-escape prepared graph capacity limits and
  fallback behavior completed.
- Follow-up source audit: AOT call-argument temporary allocation paths
  completed.
- Source material reviewed: existing uncommitted `AUDIT_REPORT_2026-04-24.md`,
  live source scans, and direct gate runs.

## Open Findings

### AUDIT-001: AOT runtime manifest omits live `src/lisp/*.c3` sources

- Severity: High
- Status: Closed
- Classification: static build/config, targeted manifest repair
- Evidence:
  - `scripts/check_e2e_baseline_policy.sh` fails with:
    `FAIL: AOT lisp runtime manifests do not contain every non-test src/lisp/*.c3 source`
  - The parity check is implemented in `scripts/check_e2e_baseline_policy.sh:21`.
  - Missing manifest entries verified against `src/entry_build_runtime_manifest_lisp_part0.c3`
    through `src/entry_build_runtime_manifest_lisp_part3.c3`:
    - `src/lisp/eval_boundary_planner.c3`
    - `src/lisp/ffi_bridge_boundary.c3`
    - `src/lisp/macros_builtin_str.c3`
    - `src/lisp/parser_with_module.c3`
    - `src/lisp/stable_escape_epoch.c3`
    - `src/lisp/stable_escape_store.c3`
    - `src/lisp/stable_escape_store_passport.c3`
    - `src/lisp/symbol_lambda_helpers.c3`
    - `src/lisp/value_boundary_ownership_policy.c3`
- Impact: AOT/e2e compile parity can silently omit runtime/parser/boundary code,
  including newly added scoped module open parsing.
- Next action: add the missing files to the manifest part arrays and rerun
  `scripts/check_e2e_baseline_policy.sh`.
- Negative-memory constraint: do not bypass this gate by loosening the parity
  script; the source manifests are the contract.

### AUDIT-002: Code file-size gate is failing

- Severity: High
- Status: Closed
- Classification: static maintainability, targeted split
- Evidence:
  - `scripts/check_file_size_gate.sh` fails.
  - Offending file: `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
    at 1015 LOC.
- Impact: blocks the global gate and violates the 1000 LOC code-file rule.
- Next action: split `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
  top-down into another part file, then rerun `scripts/check_file_size_gate.sh`.

### AUDIT-003: Boundary telemetry uses integer printf formats that do not match argument types

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted logging correctness
- Evidence:
  - `src/scope_region_temp_pool_stats.c3:260` through
    `src/scope_region_temp_pool_stats.c3:264` use `%d` for transfer counters
    that are not explicitly `int`.
  - `src/lisp/eval_boundary_graph_audit_logging.c3:14` through
    `src/lisp/eval_boundary_graph_audit_logging.c3:56` use `%d` while passing
    `(long)` values.
  - `src/lisp/eval_boundary_graph_audit_telemetry.c3:37`,
    `src/lisp/eval_boundary_graph_audit_telemetry.c3:45`, and
    `src/lisp/eval_boundary_graph_audit_telemetry.c3:53` use `%d` for
    `(long)` counters.
- Impact: telemetry can truncate or misread values on platforms where `int`,
  `long`, and `usz` differ. This is especially risky for memory-boundary
  diagnostics used to prove optimization safety.
- Next action: replace formats with the C3-supported equivalents for `usz` and
  `long`, then run `c3c -C build` and a boundary telemetry smoke.

### AUDIT-004: `char[]` slices are passed to `%s` logging formats

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted logging correctness
- Evidence:
  - `src/scope_region_global_guards.c3:54` passes `char[] op` to `%s`.
  - `src/lisp/eval_boundary_graph_audit_meta.c3:15`,
    `src/lisp/eval_boundary_graph_audit_meta.c3:23`, and
    `src/lisp/eval_boundary_graph_audit_meta.c3:31` return `char[]` names.
  - Those names are passed to `%s`, for example at
    `src/lisp/eval_boundary_telemetry.c3:473`.
- Impact: `char[]` is a slice, not necessarily NUL-terminated. `%s` can read
  past the slice if the backing data is not a valid `ZString`.
- Next action: either return `ZString` for static reason names or use an
  explicit length-aware format everywhere these helpers are logged.

### AUDIT-005: ScopeRegion fast-path allocators dereference `self` after nullable guard

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted contract hardening
- Evidence:
  - `src/scope_region_allocators.c3:89` calls `scope_guard_owner(self, ...)`
    and then dereferences `self.bump` at `src/scope_region_allocators.c3:94`.
  - `src/scope_region_allocators.c3:111` follows the same pattern and
    dereferences `self.escape_bump` at `src/scope_region_allocators.c3:116`.
- Impact: `scope_guard_owner` returns for null scopes, so a mistaken null caller
  would continue into a null dereference. The intended non-null precondition is
  not encoded at the API boundary.
- Next action: add `@require self != null` or an explicit null fail-closed path
  to both hot-path allocators.

### AUDIT-006: `scope_reset_temp_lane` leaves allocation counters cumulative

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted telemetry correctness
- Evidence:
  - `src/scope_region_reset_helpers.c3:74` through
    `src/scope_region_reset_helpers.c3:104` reset TEMP chunks and destructors
    but do not reset `alloc_bytes` or `alloc_count`.
- Impact: lane-only resets can keep stale allocation telemetry even though TEMP
  allocation state has been reset, which can distort memory-shape diagnostics.
- Next action: decide whether TEMP-lane reset semantics should match full
  `scope_reset`; if yes, clear `alloc_bytes` and `alloc_count` there and add a
  focused scope-region test.
- Resolution: `scope_reset_temp_lane` now clears `alloc_bytes` and
  `alloc_count`, matching the lane-reset telemetry contract. Scope-region reset
  tests assert the counters are reset.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib
  ./build/main --test-suite scope` passed with `63 passed, 0 failed`; `git diff
  --check` passed.

### AUDIT-007: Global scope mutex is never destroyed during freelist cleanup

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted shutdown hygiene
- Evidence:
  - `src/scope_region_reset_adopt.c3:112` through
    `src/scope_region_reset_adopt.c3:136` frees the global scope/fiber
    freelists but does not destroy `g_scope_global_mu`.
- Impact: likely minor on process exit, but it is a lifecycle leak and weakens
  sanitizer/no-leak shutdown checks on platforms that require mutex teardown.
- Next action: destroy the global mutex after final cleanup if the C3 mutex API
  supports repeated init/destroy safely; add a shutdown cleanup regression if
  possible.
- Resolution: final scope freelist cleanup now tears down the global scope mutex
  through a guarded shutdown helper; later use after cleanup asserts instead of
  silently touching destroyed synchronization state.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib
  ./build/main --test-suite scope` passed with `63 passed, 0 failed`; `git diff
  --check` passed.

### AUDIT-008: BLAS dynamic symbol resolver assigns unchecked null symbols

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted native-loader hardening
- Evidence:
  - `csrc/tensor_blas_helpers.c:94` gates availability only on `dgemm_symbol`.
  - `csrc/tensor_blas_helpers.c:97` through `csrc/tensor_blas_helpers.c:99`
    assign `dgemv`, `ddot`, and `dger` function pointers even when their
    `dlsym` result is null.
- Impact: current callers may check per-operation availability, but the global
  pointer state is brittle and can become a null-call regression if a future
  call site assumes resolver success means all BLAS slots are valid.
- Next action: assign each function pointer only when that symbol is present,
  or split resolver availability by operation.
- Resolution: optional BLAS function pointers are assigned only when their
  `dlsym` result is non-null, preserving per-operation availability state.
- Validation: `scripts/build_omni_chelpers.sh` passed; `LIBRARY_PATH=/home/christos/.local/lib
  c3c --threads 1 build` linked `build/main`; `cc -fsyntax-only -std=c11 -I csrc
  csrc/tensor_blas_helpers.c` passed; `git diff --check` passed.

### AUDIT-009: LAPACK dynamic symbol resolver assigns unchecked null symbols

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted native-loader hardening
- Evidence:
  - `csrc/tensor_lapack_helpers.c:309` through
    `csrc/tensor_lapack_helpers.c:320` accept the library when any supported
    symbol group is present.
  - `csrc/tensor_lapack_helpers.c:322` through
    `csrc/tensor_lapack_helpers.c:332` then assign every LAPACKE function
    pointer, including absent symbols.
- Impact: same class as AUDIT-008; global function pointer state does not encode
  which operations are actually available.
- Next action: store only present symbols or add per-operation resolver state and
  tests for partial LAPACK libraries.
- Resolution: LAPACK function pointers are now assigned only after non-null
  `dlsym` checks, so partial libraries do not leave absent operation slots
  looking initialized.
- Validation: `scripts/build_omni_chelpers.sh` passed; `LIBRARY_PATH=/home/christos/.local/lib
  c3c --threads 1 build` linked `build/main`; `cc -fsyntax-only -std=c11 -I csrc
  csrc/tensor_lapack_helpers.c` passed; `git diff --check` passed.

### AUDIT-010: Vulkan queue family property fetch ignores second-call failure

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted native-loader hardening
- Evidence:
  - `csrc/tensor_vulkan_helpers_core.c:232` probes queue count.
  - `csrc/tensor_vulkan_helpers_core.c:237` calls
    `omni_vulkan_get_queue_family_properties` again to fill the allocated
    array, but the return value is ignored.
- Impact: if the second call fails, selection proceeds over zeroed or stale
  queue metadata. It will probably fail closed, but the device selection result
  becomes dependent on calloc contents instead of the Vulkan query result.
- Next action: check the second call result, free `queues`, and skip the device
  on failure.
- Resolution: the second queue-family property fetch now checks failure and
  post-fetch count bounds before iterating the allocated queue-property buffer.
- Validation: `scripts/build_omni_chelpers.sh` passed; `LIBRARY_PATH=/home/christos/.local/lib
  c3c --threads 1 build` linked `build/main`; `cc -fsyntax-only -std=c11 -I csrc
  csrc/tensor_vulkan_helpers_core.c` passed; `git diff --check` passed.

### AUDIT-011: Scope generation comparison is 32-bit and can theoretically wrap

- Severity: Low
- Status: Closed
- Classification: runtime behavior, structural lifetime hardening
- Evidence:
  - `src/lisp/eval_boundary_provenance.c3:630` through
    `src/lisp/eval_boundary_provenance.c3:634` treat a same-address edge as
    safe when `edge.scope_gen != target_scope.generation`.
- Impact: after enough scope generations, wraparound could make a recycled
  scope generation collide with a stale value and weaken TEMP-edge proof.
  Probability is low, but the code is in memory-boundary safety logic.
- Next action: evaluate migrating scope generation fields to a wider type or
  adding an epoch component to stable proof comparisons.

### AUDIT-012: Several core scope APIs rely on comments instead of preconditions

- Severity: Low
- Status: Closed
- Classification: static/runtime contract hygiene
- Evidence:
  - `src/scope_region_reset_adopt.c3:7` accepts nullable `parent`/`child` for
    `scope_splice_escapes`.
  - `src/scope_region_chunk_helpers.c3:61` and
    `src/scope_region_chunk_helpers.c3:75` register destructors against a raw
    `ScopeRegion*` without `@require`.
  - `src/scope_region_destroy.c3:98` accepts nullable `scope` for descendant
    destruction.
- Impact: the codebase increasingly depends on explicit memory-lifetime
  contracts; public scope APIs without clear preconditions make future audits
  noisier and allow accidental null-tolerant behavior drift.
- Next action: closed; keep explicit null handling only where it is part of an
  API contract.
- Resolution: destructor registration APIs, owned-descendant destruction, and
  `scope_splice_escapes` now encode non-null preconditions. The splice path no
  longer silently treats null parent/child as a valid no-op contract.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib
  ./build/main --test-suite scope` passed with `63 passed, 0 failed`; `git diff
  --check` passed.

### AUDIT-013: Host build configuration depends on out-of-band native library paths

- Severity: Low
- Status: Closed
- Classification: static build/config, targeted documentation/config repair
- Evidence:
  - `project.json:289` links `lightning`, `replxx`, `omni_chelpers`, and
    `omni_ftxui`.
  - `project.json:290` only searches `build`, `/usr/local/lib`, and `deps/lib`.
  - Recent local `c3c build` failed at link with missing `-llightning`,
    `-lreplxx`, `-lomni_chelpers`, and `-lomni_ftxui`.
- Impact: host builds are fragile unless the developer environment happens to
  install these libraries in the configured paths.
- Next action: either document the required `LIBRARY_PATH`/toolchain location in
  the active build instructions or add a supported local toolchain path to the
  build config.
- Resolution: the primary README and developer tooling build loop now document
  `OMNI_HOST_TOOLCHAIN_LIB_PATH`, show how it is threaded into `LIBRARY_PATH`
  for linking and `LD_LIBRARY_PATH` for runtime, and name the native libraries
  covered by the configured `project.json` search paths.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  passed before this documentation update; `git diff --check` passed after the
  audit/session ledger update and will be rerun for the final wave checkpoint.

### AUDIT-014: Mutable-capture prescan does not traverse scoped module open bodies

- Severity: Medium
- Status: Closed
- Classification: runtime compiler behavior, targeted traversal repair
- Evidence:
  - `src/lisp/compiler_mutable_capture_prescan.c3:85` through
    `src/lisp/compiler_mutable_capture_prescan.c3:156` switch over expression
    tags but do not handle `E_WITH_MODULE`.
  - The dedicated capture-detection walkers do traverse scoped module open
    bodies at `src/lisp/compiler_mutable_capture_detection_walk.c3:114` and
    `src/lisp/compiler_mutable_capture_detection_walk.c3:260`.
  - Lambda scanning also handles `E_WITH_MODULE` via
    `src/lisp/compiler_lambda_scan.c3:135` and
    `src/lisp/compiler_lambda_scan.c3:176`.
- Impact: mutable captures inside `(with module ...)` bodies can be omitted from
  the prescan phase even though adjacent compiler walkers already know how to
  inspect the body. That makes scoped-open compiler support inconsistent and is
  a likely regression point for AOT/flat compilation once `with` lowering is
  expanded beyond its current unsupported path.
- Next action: add an `E_WITH_MODULE` prescan helper that walks every
  `expr.with_module.body[i]`, then add a compiler regression with a `set!` or
  nested lambda inside `(with ...)`.
- Negative-memory constraint: do not paper over this in the later compile
  failure path; the traversal matrix should stay complete for every expression
  node with child expressions.

### AUDIT-015: AOT `define_var` can assert on binding failures instead of returning an error

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted AOT error propagation
- Evidence:
  - `src/lisp/aot_runtime_bridge_helpers.c3:16` through
    `src/lisp/aot_runtime_bridge_helpers.c3:21` expose `define_var` as `void`
    and call `env_define_with_barrier`.
  - `src/lisp/value_environment_barrier.c3:95` through
    `src/lisp/value_environment_barrier.c3:105` provide
    `env_define_with_barrier_result`, which can return a `ValueTag.ERROR` for
    null env, invalid symbol, promotion failure, or env allocation failure.
  - `src/lisp/value_environment_barrier.c3:112` through
    `src/lisp/value_environment_barrier.c3:116` wraps that path in an assert.
- Impact: generated AOT code using `aot::define_var` can abort the process under
  OOM or promotion/barrier failure instead of preserving Omni's normal error
  value flow. Other AOT bridge paths already use checked binding helpers, for
  example `src/lisp/aot_runtime_bridge_ffi.c3:132`.
- Resolution: `aot::define_var` now returns a nullable `Value*` error instead
  of asserting, uses `env_define_with_barrier_result`, and generated mutable-
  capture/import call sites observe the returned error before continuing.
- Validation: C3 LSP diagnostics were clean for the touched AOT/compiler files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  passed; source-generation regressions now assert that generated mutable-
  capture and import `define_var` calls observe the returned error. The broader
  compiler slice remains blocked by the pre-existing
  `scope global mutex used after shutdown cleanup` lifecycle abort in
  `run_compiler_group_aot_runtime_parity_tests`.

### AUDIT-016: TLS trust-store cause helper passes raw Omni string storage as a C path

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted FFI/string boundary hardening
- Evidence:
  - The main TLS connect path length-copies Omni strings with
    `tls_dup_string(host_v.str_chars[:host_v.str_len], 255)` at
    `src/lisp/tls_connect_primitives.c3:112` and does the same for CA/client
    paths at `src/lisp/tls_connect_primitives.c3:118` through
    `src/lisp/tls_connect_primitives.c3:126`.
  - The trust-store regression helper bypasses that pattern and calls
    `omni_tls_trust_store_load_file((ZString)args[0].str_chars)` at
    `src/lisp/tls_connect_primitives.c3:172`.
- Impact: this relies on raw Omni string storage being a valid C path and ignores
  `str_len`. Embedded NUL bytes truncate the path, and any future string storage
  change that weakens NUL termination would become an FFI over-read risk.
- Next action: duplicate the path with `tls_dup_string(args[0].str_chars[:args[0].str_len])`,
  reject embedded NUL if paths are meant to be C strings, and add a focused
  regression for the helper.
- Resolution: the trust-store cause helper now routes the path through the
  shared C API string boundary helper, which length-copies valid path bytes and
  rejects embedded NUL before calling the native TLS helper. Regression coverage
  exercises the helper through direct C-level primitive invocation.
- Validation: C3 LSP diagnostics were clean for the touched TLS/string-boundary
  and test files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  passed; `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1`
  passed with `pass=162 fail=0`.

### AUDIT-017: Value printing ignores tracked string length

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted printer correctness
- Evidence:
  - `src/lisp/value_constructors_core.c3:88` through
    `src/lisp/value_constructors_core.c3:109` store both `str_chars` and
    `str_len`.
  - `src/lisp/value_print_buf.c3:243` through
    `src/lisp/value_print_buf.c3:246` prints strings with
    `pb.append_zstr((ZString)v.str_chars)`.
  - `src/lisp/value_print.c3:54` through `src/lisp/value_print.c3:55` prints
    strings with `io::printf("\"%s\"", (ZString)v.str_chars)`.
  - Runtime string producers can build strings from arbitrary byte slices, for
    example compression paths call `make_string(interp, out_buf[:actual])`.
- Impact: printed strings truncate at the first embedded NUL and are not
  length-faithful to the runtime `Value`. This can corrupt debugging output,
  serialized-ish display output, and tests that rely on printed string identity.
- Next action: make both printers append exactly `v.str_chars[:v.str_len]` with
  escaping for non-printable bytes, then add a regression that constructs a
  string containing an embedded NUL.
- Resolution: `print_value` and `print_value_to_buf` now print exactly the
  tracked string byte slice and share escaping for embedded NUL/control bytes.
  The basic C-level printer regression constructs `{'a', 0, 'b'}` and expects
  the length-faithful escaped form `"a\x00b"`.
- Validation: C3 LSP diagnostics were clean for the touched printer/test files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1` passed with
  `pass=173 fail=0`; `git diff --check` passed.

### AUDIT-018: Public C API boundaries pass Omni strings as raw `ZString` paths/commands

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, systemic FFI/string boundary hardening
- Evidence:
  - `src/lisp/prim_io_fs_handles.c3:52`, `src/lisp/prim_io_fs_handles.c3:89`,
    `src/lisp/prim_io_fs_handles.c3:126`, and
    `src/lisp/prim_io_fs_handles.c3:141` pass filesystem paths directly from
    `args[*].str_chars` to native filesystem helpers.
  - `src/lisp/prim_io_fs_stream.c3:102` does the same for `fs-open`.
  - `src/lisp/prim_system.c3:24` and `src/lisp/prim_system.c3:65` pass command
    and environment variable strings directly to `popen` and `getenv`.
  - `src/lisp/tls_connection_primitives.c3:65` passes TLS certificate/key paths
    directly to `omni_tls_server_creds_load`.
  - `src/lisp/eval_ffi_eval.c3:47` and `src/lisp/eval_ffi_eval.c3:54` format
    a failed FFI path with `%s` from raw `path_val.str_chars`.
- Impact: these call sites ignore `str_len` and rely on runtime string storage
  being a trusted C string. Embedded NUL bytes can truncate paths, command
  strings, FFI error messages, and environment lookups. Future string storage
  changes could also turn the same pattern into over-read risk.
- Next action: add a shared string-to-C-boundary helper that length-copies or
  validates no embedded NUL for C APIs, then migrate filesystem, system, FFI,
  and TLS server path call sites to it.
- Negative-memory constraint: do not fix only one primitive at a time; this is
  the same boundary contract as `AUDIT-016` and needs one shared policy.
- Resolution: added `c_api_string_boundary.c3` and migrated filesystem paths,
  `fs-open`, `shell`, `getenv`, TLS server/trust-store paths, TLS connect
  string validation, FFI library error formatting, and shared dlopen registry
  path copying to length-aware C string handling with embedded-NUL rejection.
  Regression coverage exercises FS, system, TLS helper, TLS connect, and dlopen
  registry embedded-NUL rejection.
- Validation: C3 LSP diagnostics were clean for the touched C API boundary,
  filesystem, system, TLS, FFI, and test files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  passed; `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1`
  passed with `pass=162 fail=0`.

### AUDIT-019: Macro value/expression conversion lags the parser special-form set

- Severity: Medium
- Status: Closed
- Classification: runtime macro behavior, structural traversal/conversion repair
- Evidence:
  - `src/lisp/parser_expr_head_symbol_forms.c3:38` through
    `src/lisp/parser_expr_head_symbol_forms.c3:57` recognize forms including
    `handle`, `resolve`, `with`, `match`, `module`, `import`, and
    `export-from`.
  - `src/lisp/macros_define_hygiene.c3:203` through
    `src/lisp/macros_define_hygiene.c3:228` treats many of those names as
    special-form symbols.
  - `src/lisp/macros_expr_conversion_special_forms.c3:5` through
    `src/lisp/macros_expr_conversion_special_forms.c3:72` converts only a
    smaller subset back into AST nodes and falls through to ordinary call
    conversion for the missing forms.
  - `src/lisp/macros_expr_conversion.c3:56` through
    `src/lisp/macros_expr_conversion.c3:116` converts unsupported AST forms
    back to `nil`, not to an error or faithful syntax representation.
  - Macro expansion depends on this conversion at
    `src/lisp/macros_expansion_helpers.c3:160` through
    `src/lisp/macros_expansion_helpers.c3:196`, and `macroexpand` does the same
    at `src/lisp/primitives_meta_predicates.c3:113` through
    `src/lisp/primitives_meta_predicates.c3:128`.
- Impact: macros cannot reliably consume or emit newer special forms. A macro
  that emits `(with ...)`, `(handle ...)`, `(match ...)`, `(module ...)`, or
  `(import ...)` can be lowered as an ordinary call or round-tripped as `nil`,
  which changes semantics silently.
- Resolution: macro value/expression conversion now covers the audited parser
  special-form set, including handle/resolve, match, module/import,
  scoped-module open, and export-from round trips.
- Validation: C3 LSP diagnostics were clean for the touched macro conversion
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  passed; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene-special-form-conversion
  OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` passed with
  `pass=9 fail=0`. The broader `advanced-macro-hygiene` filter is still blocked
  by an unrelated `advanced-macro-hygiene-string-number` non-tail recursion
  segfault (`status=139`).

### AUDIT-020: `tls-server-wrap` ignores extra direct-call arguments

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted arity validation
- Evidence:
  - `src/lisp/tls_connection_primitives.c3:7` documents
    `(tls-server-wrap tcp-handle cert-pem-path key-pem-path)`.
  - `docs/reference/11-appendix-primitives.md:663` lists
    `__raw-tls-server-wrap` arity as `3`.
  - The packed-list path rejects extra arguments at
    `src/lisp/tls_connection_primitives.c3:28`.
  - The direct-call path checks only `args.len < 3` at
    `src/lisp/tls_connection_primitives.c3:30`, then reads `args[0]` through
    `args[2]` and ignores any trailing arguments.
  - `src/lisp/eval_init_primitive_tables.c3:315` registers the primitive as
    variadic (`-1`), so direct-call arity enforcement is delegated to this
    function.
- Impact: malformed calls can be accepted with ignored user data. That weakens
  API predictability and leaves tests covering only too-few or wrong-type
  arguments, not too-many arguments.
- Next action: require exactly three arguments in the direct-call path and add a
  regression for `(__raw-tls-server-wrap nil "cert.pem" "key.pem" "extra")`.
- Resolution: `prim_tls_server_wrap` now requires exactly three raw direct
  arguments and rejects any non-empty packed tail after the third payload item.
  Regressions cover both direct and packed excess-argument calls.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=async` passed with `pass=79
  fail=0`; `git diff --check` passed.

### AUDIT-021: `shell` truncates stdout at 8192 bytes and reports raw wait status loosely

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted system primitive correctness
- Evidence:
  - `src/lisp/prim_system.c3:11` through `src/lisp/prim_system.c3:14` documents
    `(shell cmd)` as returning stdout and `(shell cmd true)` as returning
    `(stdout-string exit-code)`.
  - `src/lisp/prim_system.c3:30` uses a fixed `char[8192]` buffer.
  - `src/lisp/prim_system.c3:32` through `src/lisp/prim_system.c3:37` breaks
    once `total >= 8192` without signaling truncation.
  - `src/lisp/prim_system.c3:38` through `src/lisp/prim_system.c3:39` decodes
    the `pclose` status with `status >> 8` directly instead of checking normal
    exit versus signal/error cases.
  - The reference table lists `shell` as an OS primitive at
    `docs/reference/11-appendix-primitives.md:524`, and the current smoke test
    only covers short output at
    `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:724`.
- Impact: commands producing more than 8192 bytes return silently truncated
  output, and signaled or abnormal process termination can surface as a
  misleading exit code.
- Next action: grow the output buffer dynamically or fail closed on truncation,
  decode `pclose` status with proper wait-status checks, and add regressions for
  large stdout and signaled commands.
- Resolution: `shell` now grows its stdout buffer dynamically beyond the former
  8192-byte cap, fails closed on allocation/range failure, and decodes `pclose`
  status into normal exit codes or `128 + signal`. Regressions cover 9000-byte
  stdout and status tuple success/failure.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `pass=151
  fail=0`; `git diff --check` passed.

### AUDIT-022: System primitive arity contracts drift between docs, table, and implementation

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted primitive registration repair
- Evidence:
  - `docs/reference/11-appendix-primitives.md:524` documents `shell` as arity
    `1-2`.
  - `src/lisp/prim_system.c3:47` through `src/lisp/prim_system.c3:50` contains
    implementation support for a second truthy argument that returns
    `(stdout-string exit-code)`.
  - `src/lisp/eval_init_primitive_tables.c3:227` registers `shell` with arity
    `1`, so the normal multi-argument apply path rejects `(shell cmd true)`
    before the implementation can observe the second argument.
  - `docs/reference/11-appendix-primitives.md:529` documents `exit` as arity
    `0-1`.
  - `src/lisp/eval_init_primitive_tables.c3:234` registers `exit` as variadic
    (`-1`), and `src/lisp/prim_system.c3:109` through
    `src/lisp/prim_system.c3:117` ignores every argument after `args[0]`.
- Impact: one documented system form is unreachable through ordinary calls, and
  another accepts malformed extra arguments while performing a terminal process
  action. This weakens the primitive table as an arity source of truth.
- Next action: register `shell` as variadic with explicit `1-2` validation or
  split the exit-code form into a separate primitive; make `exit` enforce
  `0-1`; add language-level arity regressions for both.
- Resolution: `shell` is registered variadic with explicit `1-2` validation so
  the documented status tuple form is reachable. `exit` now enforces `0-1`
  arguments and requires an integer status when provided.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `pass=151
  fail=0`; `git diff --check` passed.

### AUDIT-023: `fs-open` ignores extra variadic arguments

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted arity validation
- Evidence:
  - `docs/reference/11-appendix-primitives.md:163` documents `fs-open` as
    variadic with `mode/options`.
  - The packed effect-fast-path parser in `src/lisp/prim_io_fs_stream.c3:63`
    through `src/lisp/prim_io_fs_stream.c3:74` reads path, flags, and optional
    mode but never checks whether the packed list has additional tail items
    after mode.
  - The direct-call path in `src/lisp/prim_io_fs_stream.c3:74` through
    `src/lisp/prim_io_fs_stream.c3:78` accepts any `args.len >= 2`, reads only
    `args[0]`, `args[1]`, and optionally `args[2]`, and ignores `args[3...]`.
- Impact: malformed filesystem calls can silently drop user-supplied options or
  garbage arguments. This is especially risky because `fs-open` is side-effecting
  and its docs imply option handling.
- Next action: choose the real contract: either enforce `2-3` arguments now, or
  parse a documented options value after mode; add direct-call and packed-list
  regressions for too many arguments.
- Resolution: `fs-open` now enforces the current shipped `2-3` argument
  contract in direct and packed raw paths, rejecting any non-empty packed tail
  after optional mode. Regressions cover direct and packed too-many-argument
  calls.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-effect-union-limit` passed with `pass=77
  fail=0`; `git diff --check` passed.

### AUDIT-024: `sleep` accepts negative and overflowing durations

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted range validation
- Evidence:
  - `docs/reference/11-appendix-primitives.md:528` documents `sleep` as
    sleeping for `N` seconds.
  - `src/lisp/prim_system.c3:128` through `src/lisp/prim_system.c3:134`
    accepts integer, double, and float32 inputs but does not reject negative,
    NaN, infinite, or excessively large values.
  - `src/lisp/prim_system.c3:135` casts `secs * 1000000.0` directly to `uint`
    before calling `usleep`.
- Impact: `(sleep -1)`, non-finite values, or very large values can wrap or
  truncate into an unintended microsecond duration instead of failing closed.
- Next action: require finite `secs >= 0`, cap at `uint.max / 1000000.0` or use
  a wider sleep loop, and add regressions for negative, NaN/infinite, and huge
  duration inputs.
- Resolution: `sleep` now requires exactly one numeric argument, rejects
  non-finite, negative, and `uint` microsecond overflow durations before
  calling `usleep`, and preserves zero/tiny valid sleeps.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `pass=144
  fail=0`; `git diff --check` passed.

### AUDIT-025: `random-int` returns `0` for non-positive bounds

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted range validation
- Evidence:
  - `docs/reference/11-appendix-primitives.md:531` documents `random-int` as a
    random integer in `[0, n)`.
  - `src/lisp/prim_math.c3:36` through `src/lisp/prim_math.c3:50` parses the
    bound as a number.
  - `src/lisp/prim_math.c3:51` returns `0` when `n <= 0`.
- Impact: for `n <= 0`, the documented interval is empty, but the primitive
  returns a successful value. That can hide caller bugs and produce a value that
  is not actually inside `[0, n)`.
- Next action: reject non-positive bounds with a range error and add regressions
  for `0`, negative integer, and negative float inputs.
- Resolution: `random-int` now requires exactly one argument and rejects
  non-positive bounds with `random-int: bound must be positive` instead of
  returning `0`.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `pass=151
  fail=0`; `git diff --check` passed.

### AUDIT-026: JIT effect fast path bypasses fixed primitive arity checks

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted arity validation
- Evidence:
  - Fixed-arity raw effect primitives are registered with exact arity metadata,
    for example `__raw-tcp-write` at `src/lisp/eval_init_primitive_tables.c3:279`,
    `__raw-tcp-close` at `src/lisp/eval_init_primitive_tables.c3:280`,
    `__raw-tls-write` at `src/lisp/eval_init_primitive_tables.c3:317`, and
    `__raw-tls-close` at `src/lisp/eval_init_primitive_tables.c3:318`.
  - The multi-argument normal JIT primitive path checks exact arity before
    invoking the primitive at `src/lisp/jit_apply_multi_prims.c3:128` through
    `src/lisp/jit_apply_multi_prims.c3:133`.
  - The effect fast-path list call counts a cons payload and then invokes the
    primitive directly at `src/lisp/jit_runtime_effects_signal.c3:19` through
    `src/lisp/jit_runtime_effects_signal.c3:57`, with no `fp_prim.prim_val.arity`
    equality check for fixed-arity primitives.
- Impact: direct calls and effect fast-path calls can disagree on malformed
  payloads. Side-effecting raw primitives may execute while silently ignoring
  trailing payload values if their local implementation only validates minimum
  arity.
- Resolution: `jit_signal_call_fast_path_primitive_list_preserving_error` now
  checks registered fixed arity before invoking the primitive while preserving
  variadic primitives. The regression covers a fixed-arity raw primitive
  rejecting an overlong packed payload and a variadic raw primitive still
  receiving the payload.
- Validation: `OMNI_LISP_TEST_SLICE=jit-policy
  OMNI_JIT_POLICY_FILTER=fixed-arity-raw-effect-payload-arity-gate
  OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` passed with
  `pass=1 fail=0`.

### AUDIT-027: Network raw primitives silently ignore extra arguments

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted arity validation
- Evidence:
  - `__raw-tcp-listen`, `__raw-tcp-read`, `__raw-udp-bind`,
    `__raw-udp-send`, and `__raw-udp-recv` are registered as variadic in
    `src/lisp/eval_init_primitive_tables.c3:276` through
    `src/lisp/eval_init_primitive_tables.c3:284`, even though the reference
    table documents narrow contracts at `docs/reference/11-appendix-primitives.md:633`
    through `docs/reference/11-appendix-primitives.md:640`.
  - `prim_tcp_listen` reads host, port, and optional backlog from packed or
    direct arguments at `src/lisp/async_tcp_transport_listen.c3:15` through
    `src/lisp/async_tcp_transport_listen.c3:28`, but never rejects additional
    packed or direct arguments.
  - `prim_udp_bind` and `prim_udp_send` read only their required fields at
    `src/lisp/async_udp_primitives.c3:29` through
    `src/lisp/async_udp_primitives.c3:45` and
    `src/lisp/async_udp_primitives.c3:91` through
    `src/lisp/async_udp_primitives.c3:113`; neither path rejects trailing
    payload values.
  - `prim_udp_recv` accepts any `args.len >= 1` and only interprets
    `args[0]` and optional `args[1]` at `src/lisp/async_udp_primitives.c3:156`
    through `src/lisp/async_udp_primitives.c3:177`.
- Impact: malformed network effect payloads can perform real socket operations
  while dropping caller-supplied trailing data. This is a regression risk for
  effect handlers, generated wrappers, and future option objects because invalid
  payload shape is not fail-closed.
- Resolution: raw TCP/UDP primitives now enforce their finite argument
  contracts inside the primitive implementation for both direct arguments and
  packed cons payloads. `__raw-tcp-connect` is registered as variadic so effect
  wrappers can pass packed payloads through to the primitive's own validation
  instead of failing at the generic arity gate.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build
  --obj-out obj` linked `build/main`; direct eval probe
  `(__raw-tcp-connect "127.0.0.1" 1 nil)` returned
  `io/tcp-connect-expected-host-port`; wrapper/effect probe
  `(tcp-connect "127.0.0.1" 65536)` returned
  `io/tcp-connect-invalid-port`.

### AUDIT-028: Filesystem libuv wrapper truncates buffers above `UINT_MAX`

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted native boundary validation
- Evidence:
  - `omni_uv_fs_read` and `omni_uv_fs_write` accept `size_t len` at
    `csrc/uv_helpers.c:68` and `csrc/uv_helpers.c:77`.
  - Both wrappers pass `(unsigned int)len` to `uv_buf_init` at
    `csrc/uv_helpers.c:71` and `csrc/uv_helpers.c:80`, with no guard or
    chunking before the narrowing conversion.
  - `fs-write` passes the full string length directly to `fs_uv_write` at
    `src/lisp/prim_io_fs_stream.c3:157` through
    `src/lisp/prim_io_fs_stream.c3:158`; the async `write-file` offload loops
    around `omni_uv_fs_write` at `src/lisp/scheduler_offload_ops.c3:143`
    through `src/lisp/scheduler_offload_ops.c3:152` but still does not bound
    each native call to `UINT_MAX`.
- Impact: large file writes can be truncated, fail with a zero-length native
  write when the remaining size is an exact multiple of 2^32, or report a
  byte-count that reflects libuv's narrowed buffer rather than the requested
  Omni string length. The read wrapper is partially shielded by current
  `fs-read` clamping, but the native helper itself remains unsafe.
- Resolution: `omni_uv_fs_read` and `omni_uv_fs_write` now chunk requests
  through a shared effective maximum buffer length before calling
  `uv_buf_init`, so the native helper no longer narrows arbitrary `size_t`
  lengths directly to `unsigned int`.
- Validation: native chunk regression `tests/native/uv_fs_chunk_test.c` passed
  with a debug max-buffer override; `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  linked `build/main`.

### AUDIT-029: Integer string serializers overflow on `long.min`

- Severity: Medium
- Status: Closed
- Classification: runtime/compiler behavior, targeted integer formatting
- Evidence:
  - `Compiler.serialize_int_value_to_buf` negates negative `long` values at
    `src/lisp/compiler_expr_serialize_values.c3:76` through
    `src/lisp/compiler_expr_serialize_values.c3:80` without a `long.min` guard.
  - Shared `int_to_string` repeats the same pattern at
    `src/lisp/value_predicates_accessors_core.c3:14` through
    `src/lisp/value_predicates_accessors_core.c3:20`.
  - `Compiler.emit_int` already has the correct magnitude handling for
    `long.min` at `src/lisp/compiler_output_helpers.c3:76` through
    `src/lisp/compiler_output_helpers.c3:84`, so the safer implementation
    already exists in-tree.
- Impact: serializing the minimum signed integer can overflow or produce an
  invalid representation in compiler output and shared runtime formatting paths
  such as padded integers.
- Next action: consolidate signed integer formatting behind the guarded
  `emit_int`-style magnitude path and add regressions for `long.min` in both
  expression serialization and shared string conversion.
- Resolution: shared `int_to_string` now formats signed integers through an
  unsigned magnitude path with explicit `long.min` handling, and compiler value
  serialization reuses that helper. Regressions cover compiler serialization and
  `(String (parse-number "-9223372036854775808"))`.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=compiler` passed with `pass=319
  fail=0`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math` passed with
  `pass=173 fail=0`; `git diff --check` passed.

### AUDIT-030: Vulkan shared context release leaves a dangling global pointer

- Severity: High
- Status: Closed
- Classification: runtime behavior, structural native lifetime hardening
- Evidence:
  - `omni_tensor_backend_vulkan_context_release` releases the mutex before
    destroying the device/instance and freeing the context at
    `csrc/tensor_vulkan_helpers_core_context.inc:71` through
    `csrc/tensor_vulkan_helpers_core_context.inc:82`.
  - The same release path never clears
    `omni_tensor_vulkan_shared_context`.
  - `omni_tensor_backend_vulkan_get_shared_context` later treats a non-null
    shared pointer as live, retains it, and returns it at
    `csrc/tensor_vulkan_helpers_core_context.inc:177` through
    `csrc/tensor_vulkan_helpers_core_context.inc:185`.
  - The global shared pointer is defined at
    `csrc/tensor_vulkan_helpers_core.c:53`.
- Impact: after the final release of the shared context, the next shared-context
  acquisition can retain and return freed memory. Because destruction happens
  outside the mutex, another thread can also observe or acquire the context
  while device teardown is in progress.
- Resolution: `omni_tensor_backend_vulkan_context_release` now clears the
  shared context pointer under the Vulkan context mutex before teardown, keeping
  one destruction authority and preventing later retain of freed context
  memory.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `./build/vulkan_resource_safety_test` passed; ASAN variant passed with
  `ASAN_OPTIONS=detect_leaks=0`.

### AUDIT-031: Baseline metadata review-rule check is logically always true

- Severity: Medium
- Status: Closed
- Classification: static build/config, targeted script repair
- Evidence:
  - `scripts/check_e2e_baseline_policy.sh:191` checks:
    `[[ "$review_rule" != *"memory/CHANGELOG.md"* || "$review_rule" != *"docs/areas/types-dispatch.md"* ]]`.
  - A single string cannot make both `!=` terms false unless it contains both
    substrings, so rows that contain only one approved review target are still
    reported as `bad_review_rule` at
    `scripts/check_e2e_baseline_policy.sh:192`.
  - A direct run currently exits earlier with the manifest parity failure from
    `AUDIT-001`, so this is a secondary gate failure that will surface after
    the manifest is repaired.
- Impact: valid ownership metadata can be rejected after the earlier manifest
  failure is fixed, keeping the e2e baseline policy gate red for a different
  reason.
- Next action: decide whether the rule requires either approved target or both;
  encode that with `&&` for either-target acceptance or update the message/docs
  if both are required, then add a shell regression with one-target and
  both-target metadata rows.
- Resolution: the review-rule predicate is now a named helper that accepts
  either approved review document, and `--self-test-review-rule` covers
  memory-only, types-only, both-docs, and missing-approved-doc cases.
- Validation: `bash -n scripts/check_e2e_baseline_policy.sh scripts/run_e2e.sh
  scripts/run_global_gates.sh` passed;
  `scripts/check_e2e_baseline_policy.sh --self-test-review-rule` passed;
  `git diff --check` passed.

### AUDIT-032: JIT continuation-sensitivity scans skip scoped module open bodies

- Severity: Medium
- Status: Closed
- Classification: runtime compiler behavior, targeted traversal repair
- Evidence:
  - `expr_contains_perform` handles several expression tags in
    `src/lisp/jit_apply_eval.c3:42` through `src/lisp/jit_apply_eval.c3:79`,
    but has no `E_WITH_MODULE` branch.
  - `expr_contains_shift` likewise handles several expression tags in
    `src/lisp/jit_apply_eval.c3:82` through `src/lisp/jit_apply_eval.c3:126`,
    but has no `E_WITH_MODULE` branch.
  - The same file's cache warmer does know how to traverse scoped module open
    bodies at `src/lisp/jit_apply_eval.c3:258` through
    `src/lisp/jit_apply_eval.c3:263`, proving the AST has a traversable body
    surface here.
  - `jit_compile_expr_core.c3:162` uses these scans to decide whether a call is
    continuation-sensitive.
- Impact: `shift` or `perform` inside `(with module ...)` can be misclassified
  as continuation-insensitive, causing the JIT to choose the wrong call
  lowering path for a scoped-module-open body.
- Resolution: `expr_contains_perform` and `expr_contains_shift` now traverse
  `E_WITH_MODULE` bodies, with a scoped-module-open continuation-sensitivity
  regression.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  passed; `OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` no longer reports the scoped-module-open
  continuation scan regression. The slice still has five pre-existing unrelated
  JIT failures tracked by other audit items.

### AUDIT-033: Continuation wrapper construction assumes `alloc_value` cannot fail

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted OOM hardening
- Evidence:
  - `make_continuation` allocates a `Value*` and immediately writes through it
    at `src/lisp/value_constructors_core.c3:303` through
    `src/lisp/value_constructors_core.c3:309`, with no null check.
  - `jit_shift_value` stores the returned wrapper at
    `src/lisp/jit_runtime_effects_reset_shift.c3:97` through
    `src/lisp/jit_runtime_effects_reset_shift.c3:107` without validating it.
  - `jit_shift_impl` passes the wrapper into `jit_env_extend` at
    `src/lisp/jit_reset_shift.c3:115` through `src/lisp/jit_reset_shift.c3:130`
    without validating it.
  - `jit_handle_signal.c3:107` through `src/lisp/jit_handle_signal.c3:123` and
    `src/lisp/jit_runtime_effects_handle.c3:106` through
    `src/lisp/jit_runtime_effects_handle.c3:117` follow the same continuation
    allocation pattern.
- Impact: under value-allocation failure, continuation creation can null-deref
  before returning an error. In call sites that already allocated a
  `Continuation*`, a failed wrapper allocation also needs to discard that
  continuation to avoid leaking captured stack state.
- Resolution: `make_continuation` now returns a runtime OOM error on wrapper
  allocation failure, and reset/shift plus handle/signal call sites check for
  null/error wrappers, discard the raw continuation, and propagate the failure.
- Validation: `OMNI_LISP_TEST_SLICE=jit-policy
  OMNI_JIT_POLICY_FILTER=continuation-wrapper-alloc-failure
  OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` passed with
  `pass=1 fail=0`.

### AUDIT-034: Vulkan buffer handle refcount is unsynchronized

- Severity: High
- Status: Closed
- Classification: runtime behavior, structural native lifetime hardening
- Evidence:
  - `OmniTensorVulkanBuffer.ref_count` is a plain `uint32_t` at
    `csrc/tensor_vulkan_helpers_internal.h:712` through
    `csrc/tensor_vulkan_helpers_internal.h:718`.
  - `omni_tensor_backend_vulkan_retain` increments it without a mutex or atomic
    operation at `csrc/tensor_vulkan_helpers_core_context.inc:433` through
    `csrc/tensor_vulkan_helpers_core_context.inc:437`.
  - `omni_tensor_backend_vulkan_destroy_buffer_handle` decrements and frees from
    the same field without a mutex or atomic operation at
    `csrc/tensor_vulkan_helpers_core_context.inc:85` through
    `csrc/tensor_vulkan_helpers_core_context.inc:98`.
  - The backend already has a mutex for shared context refcounts, visible in
    `csrc/tensor_vulkan_helpers_core_context.inc:66` through
    `csrc/tensor_vulkan_helpers_core_context.inc:79`, but buffer handles do not
    use an equivalent guard.
- Impact: shared tensor/device handles copied or destroyed from different
  threads can race retain/release, causing leaked buffers, double frees, or
  freeing a Vulkan buffer while another owner still holds it.
- Resolution: Vulkan buffer handles now use atomic refcounts; retain uses an
  atomic compare/exchange loop and release uses atomic decrement before the
  final free path.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `./build/vulkan_resource_safety_test` passed; ASAN variant passed with
  `ASAN_OPTIONS=detect_leaks=0`.

### AUDIT-035: Vulkan subrange host copies map the entire device buffer

- Severity: Medium
- Status: Closed
- Classification: runtime performance/robustness, targeted native copy repair
- Evidence:
  - `omni_tensor_backend_vulkan_copy_range_to_host` validates the requested
    subrange at `csrc/tensor_vulkan_helpers.c:51` through
    `csrc/tensor_vulkan_helpers.c:54`.
  - It then maps from offset `0` for `handle->byte_len` bytes at
    `csrc/tensor_vulkan_helpers.c:57` through
    `csrc/tensor_vulkan_helpers.c:59`, and only applies the caller offset in
    the subsequent `memcpy` at `csrc/tensor_vulkan_helpers.c:61`.
  - Small status/scalar reads call this helper with `sizeof(double)` or
    `sizeof(float)`, for example
    `csrc/tensor_vulkan_helpers_matrix_ops_status.c:10` through
    `csrc/tensor_vulkan_helpers_matrix_ops_status.c:15` and
    `csrc/tensor_vulkan_helpers_matrix_ops_spectral_norm.c:37` through
    `csrc/tensor_vulkan_helpers_matrix_ops_spectral_norm.c:43`.
- Impact: scalar reads from large GPU outputs pay full-buffer map cost and can
  fail if the whole allocation is expensive or not practically mappable, even
  though the requested range is tiny.
- Resolution: Vulkan host subrange copy now maps the validated `(offset,
  byte_len)` range directly and copies from the mapped range base instead of
  mapping the full device buffer.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `./build/vulkan_resource_safety_test` passed; ASAN variant passed with
  `ASAN_OPTIONS=detect_leaks=0`.

### AUDIT-036: `run_e2e.sh` exports extra Docker mounts under a variable the hard-cap runner ignores

- Severity: Medium
- Status: Closed
- Classification: static build/config, targeted script repair
- Evidence:
  - Outside a validation container, `scripts/run_e2e.sh:23` through
    `scripts/run_e2e.sh:53` builds auto-detected mount flags and exports them as
    `OMNI_VALIDATION_EXTRA_ARGS`.
  - The same script then runs build/test phases through
    `omni_run_with_hard_cap` at `scripts/run_e2e.sh:71`,
    `scripts/run_e2e.sh:75`, `scripts/run_e2e.sh:80`,
    `scripts/run_e2e.sh:93`, and `scripts/run_e2e.sh:109`.
  - The Docker hard-cap implementation appends only `OMNI_DOCKER_EXTRA_ARGS` at
    `scripts/c3c_limits.sh:362` through `scripts/c3c_limits.sh:365`.
  - `scripts/run_validation_container.sh` does consume
    `OMNI_VALIDATION_EXTRA_ARGS` directly at
    `scripts/run_validation_container.sh:119` through
    `scripts/run_validation_container.sh:149`, so the naming contract differs
    between the two Docker paths.
- Impact: host header/library mounts auto-detected by `run_e2e.sh` can be
  silently dropped for the actual Docker-capped commands, making e2e behavior
  depend on what is already baked into the validation image.
- Next action: normalize on one extra-args variable or bridge
  `OMNI_VALIDATION_EXTRA_ARGS` into `OMNI_DOCKER_EXTRA_ARGS` before calling
  `omni_run_with_hard_cap`; add a shell regression that proves an auto-detected
  mount reaches the generated `docker run` command.
- Resolution: `run_e2e.sh` now bridges auto-detected validation mounts into
  `OMNI_DOCKER_EXTRA_ARGS` for Docker hard-cap execution while keeping the
  existing validation-container variable populated. Boost is intentionally not
  duplicated into Docker extras because `c3c_limits.sh` already mounts it.
  `--self-test-validation-mount-bridge` covers normal bridge, boost skip, and
  existing Docker mount de-duplication.
- Validation: `bash -n scripts/check_e2e_baseline_policy.sh scripts/run_e2e.sh
  scripts/run_global_gates.sh` passed;
  `scripts/run_e2e.sh --self-test-validation-mount-bridge` passed;
  `git diff --check` passed.

### AUDIT-037: Global gates cannot enable `memory-lifetime-bench` independently

- Severity: Low
- Status: Closed
- Classification: static build/config, targeted script repair
- Evidence:
  - `scripts/run_global_gates.sh:14` defines
    `OMNI_GLOBAL_GATES_INCLUDE_LIFETIME_SOAK`.
  - `scripts/run_global_gates.sh:15` defines
    `OMNI_GLOBAL_GATES_INCLUDE_ALLOCATOR_BENCH`.
  - The lisp slice selection correctly gates `memory-lifetime-soak` on
    `OMNI_GLOBAL_GATES_INCLUDE_LIFETIME_SOAK` at
    `scripts/run_global_gates.sh:84` through `scripts/run_global_gates.sh:89`.
  - Both `allocator-bench` and `memory-lifetime-bench` are gated on
    `OMNI_GLOBAL_GATES_INCLUDE_ALLOCATOR_BENCH` at
    `scripts/run_global_gates.sh:90` through `scripts/run_global_gates.sh:100`,
    and no separate `OMNI_GLOBAL_GATES_INCLUDE_MEMORY_LIFETIME_BENCH` variable
    exists in the scripts/docs search.
- Impact: operators cannot request the memory-lifetime benchmark without also
  enabling the allocator benchmark, and the status echo only reports allocator
  bench inclusion. This weakens benchmark collection for memory-model work.
- Next action: introduce a dedicated
  `OMNI_GLOBAL_GATES_INCLUDE_MEMORY_LIFETIME_BENCH` gate or document that both
  benchmarks intentionally share one switch; update the status echo and tooling
  docs accordingly.
- Resolution: `scripts/run_global_gates.sh` now has a dedicated
  `OMNI_GLOBAL_GATES_INCLUDE_MEMORY_LIFETIME_BENCH` switch and status echo, so
  `allocator-bench` and `memory-lifetime-bench` can be enabled independently.
- Validation: `bash -n scripts/check_e2e_baseline_policy.sh scripts/run_e2e.sh
  scripts/run_global_gates.sh` passed; `git diff --check` passed.

### AUDIT-038: FFI callback finalization can race active foreign callbacks

- Severity: High
- Status: Closed
- Classification: runtime behavior, structural FFI lifetime hardening
- Evidence:
  - `FfiCallbackContext` stores an `open` flag at
    `src/lisp/prim_ffi_callback.c3:52` through
    `src/lisp/prim_ffi_callback.c3:62`.
  - The foreign callback entrypoint reads `ctx.open`, `ctx.callback`,
    `ctx.interp`, and `ctx.param_types` with no lock or retain guard at
    `src/lisp/prim_ffi_callback.c3:223` through
    `src/lisp/prim_ffi_callback.c3:245`.
  - The finalizer frees the libffi closure, releases the owner scope, frees
    `ctx.param_types`, and frees `ctx` at
    `src/lisp/prim_ffi_callback.c3:207` through
    `src/lisp/prim_ffi_callback.c3:220`, but never flips `ctx.open` false or
    coordinates with a currently executing callback.
  - The C closure keeps the same `user_data` pointer and forwards it directly
    into C3 at `csrc/ffi_helpers.c:140` through `csrc/ffi_helpers.c:143`.
- Impact: if a foreign thread invokes a callback while Omni releases the
  `ForeignHandle`, dispatch can read freed context memory or call into an
  interpreter/scope being torn down. This is the FFI analogue of the Vulkan
  refcount race and can become a use-after-free at language/native boundaries.
- Resolution: FFI callback contexts now enter through native helper guards,
  track atomic open/active/finalize/released state, defer resource release until
  active calls drain, and leave a tombstone so stale foreign `user_data` fails
  closed instead of reading freed context memory.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build
  --obj-out obj` linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=176 fail=0`.

### AUDIT-039: Unsigned FFI parameters and returns silently wrap signed Omni integers

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted FFI ABI validation
- Evidence:
  - Type annotations can resolve `UInt32` and `UInt64` to FFI tags at
    `src/lisp/value_predicates_accessors_basic.c3:194` through
    `src/lisp/value_predicates_accessors_basic.c3:200`.
  - `ffi_pack_call_arg` accepts any `INT` for `FFI_TYPE_UINT32` and writes
    `(uint)arg.int_val` with no negative or upper-bound check at
    `src/lisp/eval_ffi_bound_call.c3:253` through
    `src/lisp/eval_ffi_bound_call.c3:259`.
  - `ffi_pack_call_arg` accepts any `INT` for `FFI_TYPE_UINT64` and writes
    `(long)(ulong)arg.int_val` at
    `src/lisp/eval_ffi_bound_call.c3:261` through
    `src/lisp/eval_ffi_bound_call.c3:267`.
  - Return storage groups unsigned returns into a signed `long` slot at
    `src/lisp/eval_ffi_bound_call.c3:338` through
    `src/lisp/eval_ffi_bound_call.c3:346`, and converts them back with
    `make_int` at `src/lisp/eval_ffi_bound_call.c3:418` through
    `src/lisp/eval_ffi_bound_call.c3:422`.
  - The async path has the same unsigned-return storage shape at
    `src/lisp/prim_ffi_async.c3:66` through
    `src/lisp/prim_ffi_async.c3:73` and
    `src/lisp/prim_ffi_async.c3:102` through
    `src/lisp/prim_ffi_async.c3:112`.
- Impact: negative Omni integers can be passed to unsigned C parameters as huge
  values, and unsigned C return values above `long.max` can re-enter Omni as
  negative integers. That can corrupt sizes, flags, IDs, or handles for
  generated bindings that expose unsigned C APIs.
- Resolution: sync and async FFI unsigned marshalling now rejects negative and
  out-of-range unsigned arguments, stores unsigned return values separately,
  and rejects `UInt64` returns that cannot be represented as non-negative Omni
  fixed integers.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build
  --obj-out obj` linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=176 fail=0`.

### AUDIT-040: FFI callback argument-list construction can mask cons/promotion failures

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted FFI error propagation
- Evidence:
  - `ffi_callback_dispatch` builds callback arguments by repeatedly assigning
    `arg_list = make_cons(interp, arg_val, arg_list)` at
    `src/lisp/prim_ffi_callback.c3:230` through
    `src/lisp/prim_ffi_callback.c3:239`.
  - It checks `arg_val` for null/error at
    `src/lisp/prim_ffi_callback.c3:236` through
    `src/lisp/prim_ffi_callback.c3:237`, but does not check whether the
    returned `arg_list` is an error value before calling
    `jit_apply_multi_args` at `src/lisp/prim_ffi_callback.c3:242`.
  - `make_cons` can return boundary errors when escape-lane promotion or
    pair allocation fails at `src/lisp/value_constructors_core.c3:168` through
    `src/lisp/value_constructors_core.c3:230`.
  - `jit_apply_multi_args` treats a non-`CONS` argument list as "arg list too
    short" in primitive/closure/method-table paths, for example
    `src/lisp/jit_apply_multi_prims.c3:138` through
    `src/lisp/jit_apply_multi_prims.c3:147` and
    `src/lisp/jit_apply_multi_prims.c3:173` through
    `src/lisp/jit_apply_multi_prims.c3:177`.
- Impact: a real boundary promotion/OOM failure while building foreign callback
  arguments can be reported as an arity/list-shape error, losing the root cause
  and potentially continuing through callback dispatch with a malformed
  argument spine.
- Next action: after every `make_cons` in callback dispatch, return immediately
  if the new `arg_list` is null or `ERROR`; add a forced promotion/allocation
  regression for callback argument construction.
- Resolution: `ffi_callback_dispatch` now builds callback argument spines
  through a checked helper using `make_cons_or_error` and immediately returns
  null/error construction failures before invoking `jit_apply_multi_args`.
  A forced cons-allocation regression proves the root cause is preserved.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1` passed
  with `pass=152 fail=0`; `git diff --check` passed.

### AUDIT-041: Core value allocation asserts on scope OOM instead of failing closed

- Severity: High
- Status: Closed
- Classification: runtime behavior, structural allocation hardening
- Evidence:
  - `Interp.alloc_value` calls `self.current_scope.alloc(Value.sizeof)` and
    immediately asserts non-null at
    `src/lisp/value_interp_alloc_helpers.c3:5` through
    `src/lisp/value_interp_alloc_helpers.c3:8`.
  - `Interp.alloc_value_escape`, `Interp.alloc_value_root`, `Interp.alloc_env`,
    and `Interp.alloc_env_escape` follow the same assert-on-null pattern at
    `src/lisp/value_interp_alloc_helpers.c3:17` through
    `src/lisp/value_interp_alloc_helpers.c3:72`.
  - Many public constructors advertise non-null returns and write through the
    allocation immediately, for example `make_nil`, `make_int`, and
    `make_string` at `src/lisp/value_constructors_core.c3:31` through
    `src/lisp/value_constructors_core.c3:110`.
  - The string OOM path itself calls `interp.alloc_value()` and writes through
    the result at `src/lisp/value_constructors_core.c3:21` through
    `src/lisp/value_constructors_core.c3:28`.
- Impact: scope/arena exhaustion can abort the runtime through assertions
  instead of surfacing a Lisp error, and several attempted fail-closed paths
  still depend on the same infallible allocator. This weakens memory-model
  validation because allocation pressure can bypass normal error handling.
- Resolution: value and environment allocation helpers now return null/error
  results instead of asserting on scope OOM, and public constructor paths reached
  by the runtime fail closed under forced allocation exhaustion.
- Validation: bounded container run
  `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime
  ./build/main --test-suite lisp` passed with `pass=267 fail=0`.

### AUDIT-042: Scope destructor registration asserts on allocation failure

- Severity: High
- Status: Closed
- Classification: runtime behavior, structural allocation hardening
- Evidence:
  - `scope_register_dtor` allocates a `ScopeDtor` from the TEMP lane and
    immediately asserts non-null at `src/scope_region_chunk_helpers.c3:61`
    through `src/scope_region_chunk_helpers.c3:68`.
  - `scope_register_dtor_escape` does the same for ESCAPE-lane destructor
    metadata at `src/scope_region_chunk_helpers.c3:75` through
    `src/scope_region_chunk_helpers.c3:88`.
  - These functions are called by runtime constructors and promotion paths after
    heap/native payloads have already been allocated, for example tensor and
    closure/value registration at `src/lisp/value_constructors_core.c3:102`,
    `src/lisp/value_constructors_core.c3:125`,
    `src/lisp/value_constructors_core.c3:261`, and
    `src/lisp/eval_promotion_root_clone_basic.c3:25`.
- Impact: OOM while recording cleanup metadata aborts through assertions
  instead of returning a Lisp error. Because destructor registration often
  happens after payload allocation, this also creates a fail-closed gap where
  the runtime may either abort or strand native payload ownership instead of
  rolling back coherently.
- Resolution: TEMP and ESCAPE destructor registration now returns checked
  failure instead of asserting, and constructors/promotion paths that register
  destructors propagate allocation failure through fail-closed error/null paths.
- Validation: bounded container run
  `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime
  ./build/main --test-suite lisp` passed with `pass=267 fail=0`.

### AUDIT-043: Vulkan contract paths multiply element counts into byte lengths without overflow checks

- Severity: High
- Status: Closed
- Classification: runtime behavior, targeted native safety
- Evidence:
  - The Float32 contract helper computes `left_byte_len`, `right_byte_len`, and
    `out_byte_len` with unchecked `size_t` multiplication at
    `csrc/tensor_vulkan_helpers_contract_f32.c:99` through
    `csrc/tensor_vulkan_helpers_contract_f32.c:101`.
  - The Float64 and complex variants repeat the same pattern at
    `csrc/tensor_vulkan_helpers_contract_f64.c:99` through
    `csrc/tensor_vulkan_helpers_contract_f64.c:101`,
    `csrc/tensor_vulkan_helpers_contract_complex64.c:99` through
    `csrc/tensor_vulkan_helpers_contract_complex64.c:101`, and
    `csrc/tensor_vulkan_helpers_contract_complex128.c:99` through
    `csrc/tensor_vulkan_helpers_contract_complex128.c:101`.
  - The fused contract/scalar-chain path also multiplies contract counts into
    byte lengths without guarding `count > SIZE_MAX / sizeof(float)` at
    `csrc/tensor_vulkan_helpers_contract_region.c:292` through
    `csrc/tensor_vulkan_helpers_contract_region.c:294`.
  - Nearby map-chain code demonstrates the intended guard shape by checking
    `required > SIZE_MAX / sizeof(float)` before multiplying at
    `csrc/tensor_vulkan_helpers_dispatch_batch.c:172` through
    `csrc/tensor_vulkan_helpers_dispatch_batch.c:175`.
- Impact: extremely large shapes can wrap byte-length calculations before
  native buffer-size validation. A wrapped byte count can make too-small Vulkan
  buffers look valid to the host checks while shader metadata still describes
  the larger logical tensor.
- Resolution: Vulkan contract and fused-contract byte sizing now routes through
  shared checked element-byte helpers before buffer validation or creation.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `./build/vulkan_resource_safety_test` passed; ASAN variant passed with
  `ASAN_OPTIONS=detect_leaks=0`.

### AUDIT-044: Direct HTTP response reading silently truncates oversized responses

- Severity: High
- Status: Closed
- Classification: runtime behavior, targeted HTTP correctness regression
- Evidence:
  - `http-get` and `http-request` allocate a fixed
    `ASYNC_IO_HTTP_RESPONSE_BUFFER_BYTES` buffer and pass
    `ASYNC_IO_HTTP_RESPONSE_READ_MAX_BYTES` to `http_read_connection` at
    `src/lisp/http.c3:67` through `src/lisp/http.c3:75` and
    `src/lisp/http.c3:156` through `src/lisp/http.c3:164`.
  - The response read cap is fixed at 65,000 bytes in
    `src/lisp/async_io_shared.c3:9` through
    `src/lisp/async_io_shared.c3:10`.
  - `http_read_connection` loops only while `resp_len < max_len`, clamps the
    final chunk to `max_len - resp_len`, then returns success with the copied
    prefix; it never checks whether the transport still has unread bytes at
    `src/lisp/http_connection.c3:42` through
    `src/lisp/http_connection.c3:63`.
  - The offload HTTP helper has explicit `"response too large"` error paths at
    `src/lisp/scheduler_offload_http_helpers.c3:31` and
    `src/lisp/scheduler_offload_http_helpers.c3:62`, so the direct fiber path
    is inconsistent with the hardened offload behavior.
- Impact: direct `http-get` / `http-request` can return a syntactically parsed
  but incomplete response body without any error once the fixed cap is reached.
  This can corrupt API clients and makes the historical response-integrity
  remediation appear closed while the active direct path still violates it.
- Resolution: direct HTTP reads now fail closed when `Content-Length` or actual
  stream data exceeds the fixed response cap, and the offload append helper uses
  the same cap check instead of retaining an oversized partial buffer.
- Validation: `OMNI_LISP_TEST_SLICE=http OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=34 fail=0`; `git diff
  --check` passed for the touched HTTP files.

### AUDIT-045: HTTP response parser can underflow header slicing on malformed responses

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted parser robustness
- Evidence:
  - `parse_response` finds the first `\r\n\r\n` and stores `header_end = i + 4`
    without validating that a status line and header span are present at
    `src/lisp/http_url_response.c3:181` through
    `src/lisp/http_url_response.c3:188`.
  - When `header_end > 0`, it advances `hdr_start` to the first newline and
    then slices `response[hdr_start..header_end - 5]` at
    `src/lisp/http_url_response.c3:190` through
    `src/lisp/http_url_response.c3:199`.
  - For malformed but possible peer input such as a response beginning with
    `\r\n\r\n`, `header_end` is `4`, so `header_end - 5` underflows as an
    unsigned `usz` before the slice is built.
- Impact: a malformed HTTP server response can drive invalid slice bounds in
  response parsing instead of returning an `io/http-*` error. Depending on C3
  slice checks and build mode, this can become a crash/assertion path or a
  bogus oversized slice into response memory.
- Next action: validate the status line and require `header_end >= 5` plus
  `hdr_start <= header_end - 5` before slicing headers; return a payloaded
  malformed-response error and add parser-only regressions for empty,
  headerless, and short-header responses.
- Resolution: `parse_response` now validates an HTTP status line and header
  terminator before slicing, uses explicit header delimiter bounds, and treats an
  empty header block as a valid empty header string. Malformed short/headerless
  responses return `http: malformed response`.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=http` passed with `pass=31
  fail=0`; `git diff --check` passed.

### AUDIT-046: Stable escape store frees an initialized mutex without teardown

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted shutdown hygiene
- Evidence:
  - `stable_escape_store_ensure_ready` initializes the interpreter-owned stable
    escape store mutex at `src/lisp/stable_escape_store.c3:573` through
    `src/lisp/stable_escape_store.c3:580`.
  - Interpreter teardown calls `stable_escape_store_reset(self)` while the root
    scope is still alive at `src/lisp/value_interp_lifecycle.c3:421` through
    `src/lisp/value_interp_lifecycle.c3:423`.
  - The same teardown later frees `self.stable_escape_store` directly at
    `src/lisp/value_interp_lifecycle.c3:464` through
    `src/lisp/value_interp_lifecycle.c3:466` without a stable-store destroy
    helper, mutex teardown, or `initialized = false` transition.
- Impact: the new stable escape registry adds another initialized synchronization
  primitive whose lifetime is shorter than process lifetime but whose teardown is
  not represented in the owner. That weakens Valgrind/ASAN shutdown checks and
  can become a platform leak or invalid synchronization-object lifetime if the
  mutex implementation requires explicit destruction.
- Next action: add `stable_escape_store_destroy(Interp*)` that resets entries,
  tears down the mutex when supported by the C3 thread API, clears initialized
  state, and is used from `Interp.destroy`; add a create/use/destroy regression
  under Valgrind or ASAN.

### AUDIT-047: Shared dlopen registry leaks a just-opened handle on capacity overflow

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted native-resource hardening
- Evidence:
  - `ffi_dlopen_registry_acquire` opens a new library handle before growing the
    registry at `src/lisp/ffi_dlopen_registry.c3:80` through
    `src/lisp/ffi_dlopen_registry.c3:82`.
  - If `g_dlopen_registry_count >= g_dlopen_registry_capacity` and
    `g_dlopen_registry_capacity > uint.max / 2`, it returns null immediately at
    `src/lisp/ffi_dlopen_registry.c3:84` through
    `src/lisp/ffi_dlopen_registry.c3:86`.
  - The later `mem::realloc` failure branch correctly closes the just-opened
    handle at `src/lisp/ffi_dlopen_registry.c3:87` through
    `src/lisp/ffi_dlopen_registry.c3:91`, so the overflow-capacity branch is the
    inconsistent path.
  - Existing tests cover overlong path rejection at
    `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:414` through
    `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:425`, but do
    not exercise capacity-overflow cleanup.
- Impact: an extreme registry-growth failure can report library-load failure
  while leaking the native dlopen handle that succeeded moments earlier. This is
  rare in normal use, but it is exactly the kind of boundary resource leak that
  FFI hardening is meant to make fail closed.
- Next action: move the capacity overflow guard before `dlopen`, or close
  `handle` before returning null from that branch; also guard refcount increment
  at `src/lisp/ffi_dlopen_registry.c3:72` through
  `src/lisp/ffi_dlopen_registry.c3:75` against `uint.max` wrap, and add a
  fault-injection/unit regression for registry capacity exhaustion.
- Resolution: `ffi_dlopen_registry_acquire` now proves/grows registry capacity
  before calling `dlopen`, with an element-size overflow guard in the grow path.
  Existing duplicate-handle refcount checks still run before any new open.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1` passed
  with `pass=152 fail=0`; `git diff --check` passed. No focused capacity
  exhaustion test exists yet because the registry has no current fault-injection
  seam.

### AUDIT-048: Scoped module open is runtime-only and still fails AOT lowering

- Severity: High
- Status: Closed
- Classification: runtime behavior, structural compiler/runtime parity
- Evidence:
  - Runtime/JIT evaluation implements scoped module open by loading the module,
    creating a child environment, importing all exports into that child, and
    evaluating the body at `src/lisp/jit_module_import.c3:157` through
    `src/lisp/jit_module_import.c3:178`.
  - The AOT compiler path for the same `E_WITH_MODULE` node immediately records
    `compiler: scoped module open is not supported by AOT yet` at
    `src/lisp/compiler_tail_position_compilation_tco.c3:180` through
    `src/lisp/compiler_tail_position_compilation_tco.c3:183`.
  - Runtime tests cover the user-facing scoped-open behavior at
    `src/lisp/tests_advanced_stdlib_module_groups.c3:176` through
    `src/lisp/tests_advanced_stdlib_module_groups.c3:181`.
  - The open TODO entry already records the missing AOT slice at
    `docs/todo_parts/todo_part_17.md:109` through
    `docs/todo_parts/todo_part_17.md:115`, but the audit ledger did not yet
    capture it as a compiler/runtime regression.
- Impact: programs using `(with module ...)` can pass interpreter/JIT tests while
  failing AOT compilation outright. This is stronger than the manifest drift in
  `AUDIT-001`: even with manifests fixed, the semantic lowering path is still
  intentionally unsupported.
- Resolution: AOT lowering now supports scoped module open by resolving module
  exports, binding generated local aliases for the open body, preserving alias
  shadowing during symbol emission, and supporting tail-position
  `E_WITH_MODULE` lowering. Compile mode bootstraps the runtime interpreter so
  loaded-module export resolution is available.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build
  --obj-out obj` linked `build/main`; a direct `--compile` probe for
  `(with math ...)` succeeded and generated `aot::lookup_module_export("math",
  ...)` for `abs`, `sin`, and `cos`. The broader compiler slice still aborts in
  the unrelated `aot_init` global scope-mutex shutdown path.

### AUDIT-049: TimePoint constructors accept impossible calendar dates

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted data-format validation
- Evidence:
  - `#time` parsing only checks broad numeric ranges at
    `src/lisp/primitives_data_formats.c3:228` through
    `src/lisp/primitives_data_formats.c3:232`; any month `1..12` and day
    `1..31` are accepted.
  - `TimePoint` constructor validation uses the same broad date predicate at
    `src/lisp/primitives_data_formats.c3:235` through
    `src/lisp/primitives_data_formats.c3:239`, then reuses it for `date`,
    `datetime`, and `datetime-tz` at
    `src/lisp/primitives_data_formats.c3:257` through
    `src/lisp/primitives_data_formats.c3:345`.
  - `make_time_point` only stores the fields at
    `src/lisp/value_constructors_core.c3:313` through
    `src/lisp/value_constructors_core.c3:335`; it does not validate calendar
    month lengths or leap years.
  - Existing tests reject month `13` at
    `src/lisp/tests_runtime_data_unicode_groups.c3:154` through
    `src/lisp/tests_runtime_data_unicode_groups.c3:155`, but there are no
    regressions for `2023-02-29`, `2024-02-31`, or `2026-04-31`.
- Impact: `#time "2024-02-31T10:30:00Z"` and
  `(TimePoint 'date 2026 4 31)` can create impossible temporal values. Those
  values can then be serialized, compared, stored in dictionaries, or returned
  through reader tags as if they were real instants/dates.
- Next action: add a shared calendar-aware date validator, including leap-year
  handling, and use it from `#time`, `TimePoint 'date`, `TimePoint 'datetime`,
  `TimePoint 'datetime-tz`, and TOML timestamp conversion if the TOML bridge can
  surface unchecked parts. Add pass/fail tests for valid leap day and invalid
  February/April dates.
- Resolution: `#time` and `TimePoint 'date`/`'datetime`/`'datetime-tz` now share
  month-length and leap-year validation before constructing `TIME_POINT` values.
  Regressions cover valid `2024-02-29` and invalid February/April dates.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=data-format` passed with `pass=73
  fail=0`; `git diff --check` passed.

### AUDIT-050: JSON conversion returns `nil` for iterator allocation/internal failures

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted parser error handling
- Evidence:
  - JSON array conversion allocates a yyjson iterator at
    `src/lisp/json.c3:40`; if allocation or initialization fails, it returns
    `nil` at `src/lisp/json.c3:41`.
  - The same array loop returns `nil` when `omni_yyjson_arr_iter_next` yields
    null before the expected `count` elements at `src/lisp/json.c3:43` through
    `src/lisp/json.c3:45`.
  - JSON object conversion has the same pattern for iterator allocation, missing
    keys, and missing values at `src/lisp/json.c3:61` through
    `src/lisp/json.c3:68`.
  - The native wrappers allocate iterators with `malloc` and return null on
    allocation/init failure at `csrc/json_helpers.c:44` through
    `csrc/json_helpers.c:51` and `csrc/json_helpers.c:66` through
    `csrc/json_helpers.c:73`.
- Impact: `json-parse` can report a successful `nil` value for allocation
  failure or impossible internal iterator state. That masks parser/runtime
  failures as legitimate JSON `null`, losing data and making callers unable to
  distinguish malformed runtime state from valid input.
- Next action: replace these `make_nil` branches with structured
  `parser/out-of-memory` or `parser/invalid-state` errors, add yyjson iterator
  fault-injection coverage, and verify that valid JSON `null` still maps to
  `nil` only through the explicit `omni_yyjson_is_null` branch.
- Resolution: JSON conversion now raises `parser/out-of-memory` for iterator
  allocation/init failures and `parser/invalid-state` for impossible iterator
  exhaustion or missing object values. Native yyjson iterator fault-injection
  hooks cover array/object allocation, premature iteration end, and missing
  object values; valid JSON `null` remains mapped through the explicit null
  branch.
- Validation: `scripts/build_omni_chelpers.sh` rebuilt the native helper
  library; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1`
  passed with `pass=77 fail=0`; `git diff --check` passed.

### AUDIT-051: CSV parser drops quoted empty fields

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted parser state fix
- Evidence:
  - The parser tracks quoted state with `in_quotes` and `after_quote`, but it
    does not track whether the current empty field was explicitly quoted at
    `src/lisp/primitives_data_formats_csv_parse.c3:139` through
    `src/lisp/primitives_data_formats_csv_parse.c3:203`.
  - `csv_flush_field` treats an empty buffer in an empty row as absent and
    returns without appending a cell at
    `src/lisp/primitives_data_formats_csv_parse.c3:91` through
    `src/lisp/primitives_data_formats_csv_parse.c3:96`.
  - At end-of-input, `csv_flush_row` applies the same empty-buffer/empty-row
    skip at `src/lisp/primitives_data_formats_csv_parse.c3:99` through
    `src/lisp/primitives_data_formats_csv_parse.c3:104`.
  - Tests cover quoted delimiters and trailing empty unquoted fields at
    `src/lisp/tests_runtime_data_unicode_groups.c3:174` through
    `src/lisp/tests_runtime_data_unicode_groups.c3:177`, but do not cover
    `""`, `"",x`, or `x,""`.
- Impact: valid CSV with quoted empty fields is parsed with missing cells or
  missing rows. For example, `""` should parse as one row with one empty string,
  but the current state machine reaches end-of-input with an empty buffer and
  empty row, so it drops the field.
- Next action: add an explicit `field_started` or `field_was_quoted` flag that
  survives closing quotes and delimiters, use it in `csv_flush_field` and
  `csv_flush_row`, and add regressions for single quoted empty field, leading
  quoted empty field, trailing quoted empty field, and quoted empty middle cell.
- Resolution: CSV parsing now tracks `field_started`, so explicitly quoted empty
  fields are flushed as real cells instead of being treated as absent.
  Regressions cover `""`, `"",x`, `x,""`, and `x,"",y`.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1`
  passed with `pass=77 fail=0`; `git diff --check` passed.

### AUDIT-052: Deduce tuple encoding silently truncates strings above 65,535 bytes

- Severity: High
- Status: Closed
- Classification: runtime behavior, structural storage codec correctness
- Evidence:
  - The tuple codec documents strings as `2 byte len + data` at
    `src/lisp/deduce_tuple_codec.c3:6` through
    `src/lisp/deduce_tuple_codec.c3:7`.
  - `deduce_tuple_encoded_size` clamps string length to `0xFFFF` instead of
    failing at `src/lisp/deduce_tuple_codec.c3:34` through
    `src/lisp/deduce_tuple_codec.c3:37`.
  - `encode_tuple` repeats the same clamp before writing bytes at
    `src/lisp/deduce_tuple_codec.c3:77` through
    `src/lisp/deduce_tuple_codec.c3:84`.
  - The supported-value predicate accepts every `STRING` without checking
    length at `src/lisp/deduce_db_handles_storage.c3:5` through
    `src/lisp/deduce_db_handles_storage.c3:12`.
  - `deduce_encode_value_vector_to_owned_string` dynamically grows tuple-key
    buffers up to 1 MiB at `src/lisp/deduce_rule_eval_exec_helpers.c3:70`
    through `src/lisp/deduce_rule_eval_exec_helpers.c3:95`, so long string
    values can reach this codec outside the fixed 4096-byte fact buffer paths.
- Impact: two distinct Deduce values that share the same first 65,535 bytes
  encode to the same tuple bytes. That can corrupt proof/support keys, rule
  signature literal blobs, or any future relation path that moves off the
  current small stack buffer, because callers see successful encoding rather
  than a size error.
- Next action: make the codec reject strings whose byte length exceeds the
  on-disk width, or widen the string-length field and migrate the codec version.
  Add tests proving long strings fail closed or round-trip exactly, including
  two values that only differ after byte 65,535.
- Resolution: the tuple codec now has a shared
  `DEDUCE_TUPLE_MAX_STRING_BYTES` limit and rejects oversized strings during
  sizing, encoding, and supported-value validation instead of truncating them.
  Regression coverage proves max-size strings are accepted while strings above
  the two-byte width fail closed.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=deduce
  OMNI_DEDUCE_GROUP_FILTER=parallel OMNI_TEST_SUMMARY=1` passed with `pass=8
  fail=0`; `git diff --check` passed. A broader unfiltered Deduce slice still
  fails unrelated restart subtests in this host setup because spawned child
  `./build/main` processes cannot locate `liblightning.so.2`.

### AUDIT-053: Deduce tuple decoding builds an invalid empty-string slice

- Severity: High
- Status: Closed
- Classification: runtime behavior, targeted storage codec boundary fix
- Evidence:
  - String decode reads a two-byte length into `slen` at
    `src/lisp/deduce_tuple_codec.c3:137` through
    `src/lisp/deduce_tuple_codec.c3:140`.
  - It then constructs the source slice as `buf[pos..pos + slen - 1]` at
    `src/lisp/deduce_tuple_codec.c3:141`.
  - C3 slice ranges are inclusive (`arr[1..3]` contains three elements in
    `c3_cheatsheet.md:106` through `c3_cheatsheet.md:109`), so `slen == 0`
    makes the end expression `pos - 1`.
  - Deduce tests cover empty relations and clear/retract paths, but the test
    scan did not find a codec regression for an empty string fact/literal
    round-trip.
- Impact: an encoded empty string can decode from an underflowed/inverted slice
  instead of a zero-length slice. Depending on C3 runtime slice checks, this can
  become a decode failure, an oversized copy attempt, or memory read beyond the
  tuple buffer. Empty strings are legitimate data values, so this is a storage
  correctness and safety boundary.
- Next action: special-case `slen == 0` and call `make_string(interp, "")`, or
  switch to an exclusive-end slice expression that represents empty spans
  directly. Add relation fact/query and rule literal persistence tests for empty
  string values.
- Resolution: tuple decoding now passes `buf[pos:slen]` to `make_string`, so
  `slen == 0` is a valid zero-length slice instead of an underflowed inclusive
  range. Focused codec coverage verifies empty-string round-trip decoding.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=deduce
  OMNI_DEDUCE_GROUP_FILTER=parallel OMNI_TEST_SUMMARY=1` passed with `pass=8
  fail=0`; `git diff --check` passed.

### AUDIT-054: `gunzip` and raw `inflate` can overflow decompression buffer growth

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted compression bounds hardening
- Evidence:
  - `gunzip` derives its initial output buffer from `src.len * 4` without an
    overflow guard at `src/lisp/compress_gzip.c3:60` through
    `src/lisp/compress_gzip.c3:63`.
  - On insufficient space, `gunzip` multiplies `try_size *= 4` without checking
    for wraparound at `src/lisp/compress_gzip.c3:82` through
    `src/lisp/compress_gzip.c3:85`.
  - Raw `inflate` has the same unchecked initial multiply and retry multiply at
    `src/lisp/compress_codecs.c3:71` through
    `src/lisp/compress_codecs.c3:76` and
    `src/lisp/compress_codecs.c3:94` through
    `src/lisp/compress_codecs.c3:97`.
  - `zlib-decompress` already carries both the initial `src.len > usz.max / 4`
    guard and the retry growth guard at `src/lisp/compress_codecs.c3:170`
    through `src/lisp/compress_codecs.c3:200`, proving the intended fail-closed
    contract exists but is not consistently applied.
- Impact: oversized compressed inputs or repeated insufficient-space responses
  can wrap `try_size`, leading to undersized allocations and misleading
  decompression errors instead of a truthful size-limit failure. This is most
  relevant for untrusted compressed payloads and offload/task paths that expose
  gzip/deflate helpers.
- Next action: mirror the `zlib-decompress` size guards in `gunzip` and
  `inflate`, including explicit checks before each retry multiply. Add
  boundary tests for near-`usz.max / 4` source lengths or fault-injected
  insufficient-space loops.

### AUDIT-055: Deduce read paths collapse LMDB cursor failures into empty results

- Severity: High
- Status: Closed
- Classification: runtime behavior, targeted storage-error propagation
- Evidence:
  - `deduce_tuple_iterator_open` returns plain `false` for closed databases,
    tuple-value allocation failure, read transaction open failure, and cursor open
    failure at `src/lisp/deduce_relation_scan_helpers.c3:61` through
    `src/lisp/deduce_relation_scan_helpers.c3:97`.
  - The iterator also returns plain `false` from `deduce_tuple_iterator_next` for
    invalid iterator state and for every non-`MDB_SUCCESS` cursor result after the
    scan loop, without distinguishing `MDB_NOTFOUND` from storage/runtime errors
    at `src/lisp/deduce_relation_scan_helpers.c3:168` through
    `src/lisp/deduce_relation_scan_helpers.c3:206`.
  - Public scan/query/match wrappers convert that `false` into ordinary empty
    results: `relation_scan_range` returns `nil` at
    `src/lisp/deduce_relation_scan_helpers_more.c3:24` through
    `src/lisp/deduce_relation_scan_helpers_more.c3:37`, `relation_scan_all`
    returns `nil` at `src/lisp/deduce_relation_scan_helpers_more.c3:188`
    through `src/lisp/deduce_relation_scan_helpers_more.c3:191`,
    `prim_deduce_query` returns `nil` at
    `src/lisp/deduce_schema_query_execution.c3:181` through
    `src/lisp/deduce_schema_query_execution.c3:184`, and
    `deduce_match_scan_relation` returns `nil` at
    `src/lisp/unify_scan_helpers.c3:139` through
    `src/lisp/unify_scan_helpers.c3:143`.
- Impact: allocation failures, read-transaction failures, cursor failures, and
  unexpected LMDB scan errors become indistinguishable from a valid empty
  relation. Query/rule callers can silently make decisions against incomplete
  data, and the goal-directed read bookkeeping can record a no-op path for a
  read that actually failed.
- Next action: replace the bool iterator API with an error-carrying result
  (`opened`, `eof`, `error`) or add an out-error parameter. Map open/iteration
  failures to canonical `deduce/*` errors, treat only `MDB_NOTFOUND` as EOF, and
  add fault-injection tests for transaction begin, cursor open, iterator value
  allocation, and mid-scan cursor failure.
- Resolution: Deduce tuple iterators now retain structured error values and
  scan, scan-range, query, match, and in-transaction scan/match callers
  distinguish EOF from iterator open/read failures. Fault hooks and regressions
  cover transaction open, cursor open, iterator allocation, cursor read, and
  valid empty-result behavior.
- Validation: C3 LSP diagnostics were clean for the touched Deduce files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=scan,scan-range,query-filter,match,empty-relation-stats,read-failures OMNI_TEST_SUMMARY=1`
  passed with `pass=31 fail=0`.

### AUDIT-056: `deduce/count` reports zero on transaction and cursor failures

- Severity: High
- Status: Closed
- Classification: runtime behavior, targeted storage-error propagation
- Evidence:
  - After preparing a relation read, `prim_deduce_count` begins an LMDB read
    transaction at `src/lisp/deduce_relation_ops_query_count_scan.c3:93` through
    `src/lisp/deduce_relation_ops_query_count_scan.c3:95`.
  - If `mdb_txn_begin` fails, it returns `make_int(interp, 0)` instead of a
    Deduce error at `src/lisp/deduce_relation_ops_query_count_scan.c3:95`.
  - If `mdb_cursor_open` fails, it again returns `make_int(interp, 0)` at
    `src/lisp/deduce_relation_ops_query_count_scan.c3:98` through
    `src/lisp/deduce_relation_ops_query_count_scan.c3:100`.
  - The public surface and tests use zero as a legitimate empty-relation result,
    for example `src/lisp/tests_deduce_groups_integrity.c3:269` and
    `src/lisp/tests_deduce_durability_transaction_clear_drop_tests.c3:38`, so
    callers cannot distinguish failure from truth.
- Impact: count-based validation, integrity checks, and refresh decisions can
  treat storage failures as empty relations. This is especially risky because
  `count` is commonly used as a guard in tests and application logic.
- Next action: return canonical `deduce/count-transaction-failed` and
  `deduce/count-cursor-open-failed` errors, and add fault-injection coverage that
  proves empty relations still return `0` while LMDB/open failures raise.
- Resolution: `deduce/count` now reports structured transaction, cursor-open,
  and cursor-read failures instead of returning zero. Empty relations still
  return `0`, with focused regression coverage proving the distinction.
- Validation: C3 LSP diagnostics were clean for the touched Deduce files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=scan,scan-range,query-filter,match,empty-relation-stats,read-failures OMNI_TEST_SUMMARY=1`
  passed with `pass=31 fail=0`.

### AUDIT-057: String length primitives silently coerce invalid input to zero

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted primitive contract fix
- Evidence:
  - `prim_string_length` returns `make_int(interp, 0)` when called with no
    arguments or a non-string at `src/lisp/prim_string_ops.c3:130` through
    `src/lisp/prim_string_ops.c3:136`.
  - `prim_string_byte_length` has the same fallback at
    `src/lisp/prim_string_ops.c3:142` through
    `src/lisp/prim_string_ops.c3:146`.
  - Adjacent string primitives fail closed on bad type/arity, for example
    `string-contains?` at `src/lisp/prim_string_ops.c3:15` through
    `src/lisp/prim_string_ops.c3:17` and `char-at` at
    `src/lisp/prim_string_ops.c3:91` through
    `src/lisp/prim_string_ops.c3:97`.
  - The language/reference tables list both primitives as arity-1 string
    operations at `docs/LANGUAGE_SPEC.part-03.md:66` through
    `docs/LANGUAGE_SPEC.part-03.md:67` and
    `docs/reference/11-appendix-primitives.md:183` through
    `docs/reference/11-appendix-primitives.md:184`.
- Impact: type errors and missing arguments are reported as an empty string
  length. That can hide parser/runtime mistakes, make validation predicates pass
  accidentally, and diverge from the surrounding string primitive error model.
- Next action: make both primitives raise `type/arity` for missing arguments and
  `type/arg-mismatch` for non-string inputs. Add negative tests for no-arg,
  integer, nil, list, array, and dict inputs.
- Resolution: `string-length` and `string-byte-length` now reject missing and
  non-string arguments instead of returning zero. Regressions cover no-arg,
  integer, nil, list, array, and dict inputs.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=string-type
  OMNI_TEST_SUMMARY=1` passed with `pass=44 fail=0`; `OMNI_LISP_TEST_SLICE=unicode
  OMNI_TEST_SUMMARY=1` passed with `pass=31 fail=0`; `git diff --check` passed.

### AUDIT-058: `string-repeat` multiplies output size without a preflight overflow guard

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted allocation bounds hardening
- Evidence:
  - `prim_string_repeat` computes the initial builder capacity as
    `args[0].str_len * (usz)n` without checking overflow at
    `src/lisp/prim_string_ops.c3:108` through
    `src/lisp/prim_string_ops.c3:113`.
  - The builder constructor only rejects the already-wrapped `initial_cap` when
    it is `>= usz.max` at `src/lisp/prim_string_format_helpers.c3:32` through
    `src/lisp/prim_string_format_helpers.c3:37`.
  - The append path has later overflow checks in `strval_target_capacity_overflows`
    and `strval_ensure` at `src/lisp/prim_string_format_helpers.c3:24` through
    `src/lisp/prim_string_format_helpers.c3:30` and
    `src/lisp/prim_string_format_helpers.c3:67` through
    `src/lisp/prim_string_format_helpers.c3:87`, but those run only after the
    repeat loop has begun.
- Impact: impossible output sizes are not rejected before allocation/planning.
  Wrapped capacities can allocate a tiny builder for a request whose true output
  length cannot fit in `usz`, then burn CPU and memory growth attempts before a
  late generic grow failure. The primitive should fail closed immediately and
  deterministically for oversized repeat counts.
- Next action: before constructing the builder, require
  `args[0].str_len <= usz.max / (usz)n` and return a range/OOM error otherwise.
  Add a targeted overflow regression using a fault-injected or direct primitive
  call with a repeat count above the safe bound.
- Resolution: `string-repeat` now preflights `str_len * count` before builder
  construction and fails closed on impossible output sizes. A regression covers
  the overflow boundary.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-string-predicate-format
  OMNI_TEST_SUMMARY=1` passed with `pass=62 fail=0`; `OMNI_LISP_TEST_SLICE=string-type
  OMNI_TEST_SUMMARY=1` passed with `pass=44 fail=0`; `git diff --check` passed.

### AUDIT-059: `string-split` treats the separator as one byte, not a string delimiter

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted primitive contract fix
- Evidence:
  - `prim_string_split` accepts two string arguments and only rejects an empty
    separator at `src/lisp/prim_string_transform.c3:196` through
    `src/lisp/prim_string_transform.c3:203`.
  - It then stores only `args[1].str_chars[0]` in `delim` at
    `src/lisp/prim_string_transform.c3:205` through
    `src/lisp/prim_string_transform.c3:207` and splits whenever a source byte
    equals that one byte at `src/lisp/prim_string_transform.c3:212` through
    `src/lisp/prim_string_transform.c3:226`.
  - The language and primitive reference describe the argument as a delimiter,
    not a delimiter byte, at `docs/LANGUAGE_SPEC.part-03.md:65` and
    `docs/reference/11-appendix-primitives.md:182`.
  - The checked-in Finwatch request decoder already calls
    `(string-split raw "\r\n\r\n")` and `(string-split head "\r\n")` at
    `examples/finwatch/server.omni:69` through `examples/finwatch/server.omni:74`,
    so the live behavior splits on bare `\r` instead of the requested HTTP
    delimiter sequence.
- Impact: multi-character separators silently behave as their first byte, and
  multibyte UTF-8 separators can split inside a codepoint and produce invalid
  string fragments. Existing example request parsing is therefore not honoring
  the delimiter it asks for.
- Next action: implement byte-slice separator matching for the full separator
  string, preserve empty-field behavior deliberately, and add tests for
  `","`, `"::"`, `"\r\n\r\n"`, and a multibyte UTF-8 separator.
- Resolution: `string-split` now matches the complete separator string instead
  of the first byte and keeps explicit empty-field behavior. Regressions cover
  multi-character, CRLFCRLF, trailing-empty, and UTF-8 separators.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=unicode OMNI_TEST_SUMMARY=1`
  passed with `pass=31 fail=0`; `OMNI_LISP_TEST_SLICE=string-type
  OMNI_TEST_SUMMARY=1` passed with `pass=44 fail=0`; `git diff --check`
  passed. The broader `advanced-unicode-iterator` group remains red on
  unrelated iterator boundary failures.

### AUDIT-060: Symbol table growth misses element-size and `SymbolId` ceiling guards

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted allocation bounds hardening
- Evidence:
  - `SymbolId` is a `uint` and `INVALID_SYMBOL_ID` is `0xFFFFFFFF` at
    `src/lisp/value_runtime_constants.c3:83` through
    `src/lisp/value_runtime_constants.c3:97`.
  - `SymbolTable.grow` only rejects capacities above `usz.max / 2` before
    doubling at `src/lisp/value_symbol_table.c3:146` through
    `src/lisp/value_symbol_table.c3:152`.
  - The grow path allocates `SymbolEntry.sizeof * new_cap` and
    `SymbolId.sizeof * new_hash_cap` without checking the byte multiplication
    bounds at `src/lisp/value_symbol_table.c3:154` through
    `src/lisp/value_symbol_table.c3:162`.
  - `SymbolTable.intern` casts `self.count` directly to `SymbolId` at
    `src/lisp/value_symbol_table.c3:259` through
    `src/lisp/value_symbol_table.c3:260` and increments the unbounded `usz`
    count at `src/lisp/value_symbol_table.c3:270` through
    `src/lisp/value_symbol_table.c3:272`.
- Impact: very large symbol tables can cross the 32-bit ID domain or wrap byte
  allocation sizes before the current `usz.max / 2` guard fires. That risks
  sentinel aliasing, corrupt hash entries, or undersized allocations on extreme
  generated-code / fuzz inputs.
- Next action: mirror the checked growth style used by `TypeRegistry.grow`:
  reject `new_cap >= INVALID_SYMBOL_ID`, check both byte multiplications before
  allocation, and rewrite the load-factor test to avoid `self.count * 10` /
  `self.capacity * 7` overflow. Add direct SymbolTable growth/ceiling tests
  using fault injection or synthetic near-limit state.
- Resolution: symbol-table growth now computes the 70% load threshold without
  multiplying `count`, rejects capacities that would exceed the `SymbolId`
  domain or allocation byte bounds, and refuses to intern a new symbol once the
  next ID would alias `INVALID_SYMBOL_ID`. Regressions cover load-factor
  overflow, ID ceiling rejection, and grow-capacity rejection.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1` passed
  with `pass=172 fail=0`; `git diff --check` passed.

### AUDIT-061: `string-index-of` reports byte offsets under the codepoint indexing contract

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted UTF-8 contract fix
- Evidence:
  - The generic string indexer documents and implements codepoint-indexed string
    access at `src/lisp/prim_collection_generic_set.c3:94` through
    `src/lisp/prim_collection_generic_set.c3:116`.
  - `char-at` also converts a codepoint index to a byte offset before slicing at
    `src/lisp/prim_string_ops.c3:91` through `src/lisp/prim_string_ops.c3:102`.
  - `string-index-of` scans raw bytes and returns the loop variable `i` directly
    at `src/lisp/prim_string_ops.c3:34` through `src/lisp/prim_string_ops.c3:47`.
  - The language/reference docs describe `string-length` as UTF-8 codepoint
    length at `docs/LANGUAGE_SPEC.part-03.md:66`, and the string indexing
    decision note says the current UTF-8 string indexing contract is
    codepoint-based at `docs/plans/list-string-constructor-decision-2026-04-11.md:30`
    through `docs/plans/list-string-constructor-decision-2026-04-11.md:31`.
  - Existing `string-index-of` tests only cover ASCII input at
    `src/lisp/tests_advanced_stdlib_numeric_misc_groups.c3:124` through
    `src/lisp/tests_advanced_stdlib_numeric_misc_groups.c3:125`.
- Impact: `(string-index-of "éx" "x")` would report byte offset `2`, while
  `(char-at "éx" 1)` and `(ref "éx" 1)` address the same character by
  codepoint index `1`. Callers that feed `string-index-of` results into
  `substring`, `char-at`, `ref`, or postfix indexing will be wrong after any
  multibyte prefix.
- Next action: return the matched codepoint index, or explicitly rename/document
  a byte-offset primitive and add a separate codepoint-index search. Add UTF-8
  regressions for multibyte prefixes and multibyte needles.
- Resolution: `string-index-of` now advances by UTF-8 codepoint boundaries and
  returns the matched codepoint index. Regression coverage includes multibyte
  prefixes, multibyte needles, and mixed-width prefixes.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-string-predicate-format
  OMNI_TEST_SUMMARY=1` passed with `pass=65 fail=0`; `git diff --check`
  passed.

### AUDIT-062: Regex position primitives expose byte positions despite codepoint string indexing

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted UTF-8 contract fix
- Evidence:
  - `RegexMatch` stores `int start` and `int end` and `regex_mk_match` slices
    the input with those byte indexes at `src/pika/regex_types.c3:11` through
    `src/pika/regex_types.c3:38`.
  - `prim_re_match_pos` returns `m.start` and `m.end` directly at
    `src/pika/lisp_pika_regex_positions.c3:28` through
    `src/pika/lisp_pika_regex_positions.c3:33`.
  - `prim_re_find_all_pos` does the same for every match at
    `src/pika/lisp_pika_regex_positions.c3:65` through
    `src/pika/lisp_pika_regex_positions.c3:72`.
  - The public docs define these as match positions at
    `docs/LANGUAGE_SPEC.part-03.md:598` through
    `docs/LANGUAGE_SPEC.part-03.md:599`, while the broader string index contract
    is codepoint-based as recorded in
    `docs/plans/list-string-constructor-decision-2026-04-11.md:30` through
    `docs/plans/list-string-constructor-decision-2026-04-11.md:31`.
  - Existing regex position tests only assert ASCII positions at
    `src/lisp/tests_runtime_feature_pika_groups.c3:127` through
    `src/lisp/tests_runtime_feature_pika_groups.c3:133`.
- Impact: regex position results cannot be safely composed with `substring`,
  `char-at`, `ref`, or postfix string indexing for non-ASCII input. A match
  after a multibyte prefix reports byte indexes, but downstream language string
  APIs interpret indexes as codepoints.
- Next action: choose and document the public contract. If positions are meant
  to be language string indexes, convert byte starts/ends to codepoint positions
  before returning them. If byte offsets are intentional, rename/document them as
  byte positions and add separate codepoint-position APIs. Add non-ASCII regex
  position tests either way.
- Resolution: `re-match-pos` and `re-find-all-pos` now expose zero-based
  codepoint half-open `(start end)` spans at the Lisp boundary while preserving
  internal regex byte spans for matching and slicing. The language spec now
  documents the public codepoint-span contract.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed
  with `pass=94 fail=0`; `git diff --check` passed.

### AUDIT-063: Pika regex result builders do not propagate allocation errors while building lists

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted allocation/error propagation fix
- Evidence:
  - `make_string` returns an `ERROR` value when string payload allocation fails at
    `src/lisp/value_constructors_core.c3:92` through
    `src/lisp/value_constructors_core.c3:98`.
  - `re-find-all` creates each result string and immediately conses it without
    checking `s.tag == ERROR` at `src/pika/lisp_pika_regex_match_split.c3:92`
    through `src/pika/lisp_pika_regex_match_split.c3:96`.
  - `re-split` nests `make_string` directly inside `make_cons` at
    `src/pika/lisp_pika_regex_match_split.c3:126` through
    `src/pika/lisp_pika_regex_match_split.c3:139`.
  - `re-find-all-pos` builds nested lists with raw `make_cons` and no
    `make_cons_or_error` checks at `src/pika/lisp_pika_regex_positions.c3:65`
    through `src/pika/lisp_pika_regex_positions.c3:72`.
  - Similar list-producing primitives have been hardened to propagate
    allocation failures explicitly, for example `string-split` checks per-part
    string errors and uses `make_cons_or_error` at
    `src/lisp/prim_string_transform.c3:221` through
    `src/lisp/prim_string_transform.c3:233`.
- Impact: under allocation pressure, regex list primitives can return a list
  containing an `ERROR` value or a malformed cons tail instead of a top-level
  structured regex/runtime error. That makes caller error handling depend on
  inspecting every element rather than the primitive result.
- Next action: switch Pika list builders to `make_cons_or_error`, check
  `make_string` results before consing, and add fault-injection tests for
  `re-find-all`, `re-split`, `re-match-pos`, and `re-find-all-pos` result
  construction.
- Resolution: Pika regex result construction now routes string/list allocation
  through checked helpers and returns structured regex out-of-memory errors for
  result-string, flat-list, and nested-position-list failures. Fault-injection
  regressions cover `re-match`, `re-fullmatch`, `re-find-all`, `re-split`,
  `re-match-pos`, and `re-find-all-pos`.
- Validation: C3 LSP diagnostics were clean for the touched Pika/test files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=102 fail=0`; `git diff --check` passed.
- Negative-memory constraint: one-shot allocation fault hooks must not use
  dual-pass interpreter/JIT helpers unless the hook is reset per pass; the first
  interpreter pass consumes the injected failure before the JIT pass observes it.

### AUDIT-064: SharedHandle byte projection exposes unpinned payload after releasing the registry lock

- Severity: High
- Status: Closed
- Classification: runtime behavior, structural lifetime fix
- Evidence:
  - The concurrency boundary docs state that production byte-sharing transport
    uses `SharedHandle(kind=BLOB)` at
    `docs/reference/09-concurrency-ffi.md:86` through
    `docs/reference/09-concurrency-ffi.md:88`.
  - `scheduler_shared_registry_resolve_blob` validates the slot/generation
    under the registry mutex, then returns `entry.payload` after the deferred
    unlock at `src/lisp/scheduler_shared_registry.c3:143` through
    `src/lisp/scheduler_shared_registry.c3:154`.
  - `scheduler_shared_destroy` can decrement the payload refcount, retire the
    registry entry, enqueue the payload for later free, or free it immediately
    on retire-queue overflow at `src/lisp/scheduler_shared_registry.c3:157`
    through `src/lisp/scheduler_shared_registry.c3:197`.
  - `scheduler_project_shared_bytes_view` returns
    `payload.bytes[:payload.len]` without taking a payload reference or holding
    the registry lock across the caller's use at
    `src/lisp/scheduler_shared_handles_blob.c3:93` through
    `src/lisp/scheduler_shared_handles_blob.c3:99`.
  - The owned-copy helper has the same race window: it resolves a payload, then
    reads `payload.len` and copies `payload.bytes` after the resolver lock has
    been released at `src/lisp/scheduler_shared_handles_blob.c3:105` through
    `src/lisp/scheduler_shared_handles_blob.c3:119`.
  - Offload code consumes these projected views across blocking work, for
    example read/write-file at `src/lisp/scheduler_offload_ops.c3:5` through
    `src/lisp/scheduler_offload_ops.c3:24` and
    `src/lisp/scheduler_offload_ops.c3:114` through
    `src/lisp/scheduler_offload_ops.c3:150`, plus compression/network paths at
    `src/lisp/scheduler_offload_network.c3:3` through
    `src/lisp/scheduler_offload_network.c3:10` and
    `src/lisp/scheduler_offload_network.c3:71` through
    `src/lisp/scheduler_offload_network.c3:105`.
- Impact: a worker can use `payload.bytes` after another thread releases the
  last handle and retires or frees the payload. This is a concurrency-boundary
  use-after-free risk in the byte transport path that backs offload and internal
  scheduler I/O.
- Next action: replace raw projection with an acquire/release API or perform
  copy-under-lock for owned transfers. Add a stress test that races projection
  and release, including retire-queue overflow, under ASAN/Valgrind.
- Resolution: shared byte consumers now copy payload bytes while the shared
  registry mutex still protects the slot/generation/payload, and offload users
  hold owned heap copies across blocking work. A deterministic regression copies
  bytes, releases the source handle, drains retirement, and verifies the copy
  remains valid.
- Validation: C3 LSP diagnostics were clean for the touched scheduler/test
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=scheduler OMNI_TEST_SUMMARY=1` passed with
  `pass=135 fail=0`; `git diff --check` passed.
- Residual risk: broader ASAN/Valgrind concurrency stress remains useful as a
  confidence pass, but the raw unlocked projection/copy root cause is removed.

### AUDIT-065: Pika regex search and find-all suppress valid empty-input matches

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted regex contract fix
- Evidence:
  - `re-match` is specified as the first match anywhere in input, and
    `re-find-all` as the non-overlapping match list at
    `docs/LANGUAGE_SPEC.part-03.md:593` through
    `docs/LANGUAGE_SPEC.part-03.md:596`.
  - Supported regex syntax includes zero-length-capable quantifiers such as
    `*`, `?`, and bounded repetitions at `docs/LANGUAGE_SPEC.part-03.md:601`
    onward, and the parser match representation explicitly models an empty
    match as `last = first - 1` at `src/pika/structs.c3:66` through
    `src/pika/structs.c3:70`.
  - `regex_fullmatch_compiled` does not reject empty input before parsing and
    accepts an empty match when `m.last == input.len - 1` at
    `src/pika/regex_cache_compile.c3:186` through
    `src/pika/regex_cache_compile.c3:205`.
  - `regex_search_compiled` returns `no_match()` immediately for
    `input.len == 0` at `src/pika/regex_cache_compile.c3:159` through
    `src/pika/regex_cache_compile.c3:183`.
  - `regex_find_all_compiled` similarly returns an empty result immediately for
    `input.len == 0` at `src/pika/regex_cache_compile.c3:208` through
    `src/pika/regex_cache_compile.c3:254`.
- Impact: the three public regex entry points disagree on zero-length-capable
  patterns. For example, a pattern that can fullmatch the empty string cannot be
  discovered by `re-match` or `re-find-all` on the same empty input, even though
  the internal representation and fullmatch path support empty matches.
- Next action: decide the public empty-match contract, then make search and
  find-all parse empty input instead of short-circuiting when empty matches are
  valid. Add tests for `re-match`, `re-fullmatch`, `re-find-all`, and positional
  variants with `a*`, `a?`, and equivalent bounded forms on empty input.
- Resolution: Pika search/find-all now consider the empty/end boundary, so
  zero-length-capable patterns match empty input consistently with fullmatch.
  Positional APIs inherit the same behavior and return `(0 0)` half-open spans.
- Validation: C3 LSP diagnostics were clean for the touched Pika files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=116 fail=0`.

### AUDIT-066: `re-split` drops leading, trailing, and consecutive empty fields

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted regex split contract fix
- Evidence:
  - The language spec describes `re-split` as splitting input on regex matches
    at `docs/LANGUAGE_SPEC.part-03.md:593` through
    `docs/LANGUAGE_SPEC.part-03.md:596`.
  - `prim_re_split` only conses a suffix when `part_start < right`, and only
    conses the prefix when `right > 0`, at
    `src/pika/lisp_pika_regex_match_split.c3:126` through
    `src/pika/lisp_pika_regex_match_split.c3:139`.
  - The ordinary `string-split` implementation preserves empty parts by
    explicitly creating an empty string when `part_len == 0` at
    `src/lisp/prim_string_transform.c3:212` through
    `src/lisp/prim_string_transform.c3:227`.
- Impact: regex splitting has different edge behavior from string splitting
  and cannot represent structurally meaningful empty fields. Inputs with a
  leading delimiter, trailing delimiter, or adjacent delimiters silently lose
  fields, which can corrupt tokenization/parsing code that relies on field
  counts.
- Next action: either align `re-split` with `string-split` by preserving empty
  fields, or document the discard-empty contract explicitly and add tests for
  leading, trailing, and consecutive delimiters.
- Resolution: `re-split` now preserves leading, trailing, consecutive, and
  empty-input fields, aligning regex splitting with `string-split` shape
  preservation. Focused Pika regressions cover the edge cases.
- Validation: C3 LSP diagnostics were clean for the touched Pika files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=116 fail=0`.

### AUDIT-067: Reader tags starting with radix-looking prefixes can be consumed by integer dispatch

- Severity: Medium
- Status: Closed
- Classification: parser/runtime surface, targeted lexer dispatch fix
- Evidence:
  - The syntax spec describes `#tag form` generically as a reader-tag prefix
    and separately notes that `#x`, `#b`, and `#o` are radix integers at
    `docs/SYNTAX_SPEC.md:24` through `docs/SYNTAX_SPEC.md:30`.
  - The grammar still describes reader tags as `"#" symbol expr`, with no
    documented exclusion for symbols beginning with `x`, `b`, or `o`, at
    `docs/SYNTAX_SPEC.md:486` through `docs/SYNTAX_SPEC.md:490`.
  - `scan_hash_dispatch` tries radix parsing before reader-tag parsing for
    `x/X`, `b/B`, and `o/O` at
    `src/lisp/parser_lexer_string_hash_helpers.c3:174` through
    `src/lisp/parser_lexer_string_hash_helpers.c3:183`.
  - `scan_hash_radix_literal` consumes the `#` and radix marker once the next
    character is a radix digit, then errors if a later symbol character is not a
    valid radix digit at
    `src/lisp/parser_lexer_string_hash_helpers.c3:82` through
    `src/lisp/parser_lexer_string_hash_helpers.c3:120`.
- Impact: ordinary reader tag names such as `#xform`, `#xface`, `#b101tag`, or
  `#o777tag` can be rejected as malformed radix integers instead of being
  parsed as `(xform ...)`, `(xface ...)`, and so on. This narrows the documented
  reader-tag namespace in a non-obvious way and can break user-defined reader
  tag macros.
- Next action: make radix literal recognition require a delimiter after the
  digit run, or document and test the reserved reader-tag prefixes explicitly.
  Add parser tests for reader tags whose names begin with `x`, `b`, and `o`
  followed by digits/hex letters.
- Resolution: radix hash dispatch now falls through to reader-tag lexing when
  the would-be radix literal continues into a symbol-like tag name. Reader tags
  such as `#xface`, `#b101tag`, and `#o777tag` remain ordinary reader-tag
  calls.
- Validation: C3 LSP diagnostics were clean for the touched reader/data-format
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=reader-dispatch OMNI_TEST_SUMMARY=1` passed with
  `pass=19 fail=0`.

### AUDIT-068: `uuid` accepts uppercase hex but returns the non-canonical input unchanged

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted data-format normalization fix
- Evidence:
  - The language spec says `uuid` should validate and return a canonical UUID
    string at `docs/LANGUAGE_SPEC.part-03.md:448` through
    `docs/LANGUAGE_SPEC.part-03.md:452`, and the primitive appendix repeats the
    same contract at `docs/reference/11-appendix-primitives.md:604` through
    `docs/reference/11-appendix-primitives.md:607`.
  - UUID validation uses `data_hex_digit_value`, which accepts both lowercase
    and uppercase `A-F`, at `src/lisp/primitives_data_formats.c3:24` through
    `src/lisp/primitives_data_formats.c3:28` and
    `src/lisp/primitives_data_formats.c3:167` through
    `src/lisp/primitives_data_formats.c3:185`.
  - After validation, `prim_uuid` returns `make_string(interp, text)` directly
    at `src/lisp/primitives_data_formats.c3:174` through
    `src/lisp/primitives_data_formats.c3:188`.
- Impact: `#uuid "550E8400-E29B-41D4-A716-446655440000"` is accepted but
  returned with uppercase hex, even though the public contract says the result
  is canonical. Callers comparing UUID strings can observe multiple spellings
  for the same UUID.
- Next action: choose whether canonical means lowercase RFC-style output or
  strict lowercase-only input. Normalize accepted uppercase hex before returning
  or reject uppercase input, then add tests for uppercase and mixed-case UUIDs.
- Resolution: `uuid` now accepts uppercase/mixed-case UUID input but returns
  lowercase canonical UUID strings. Regression coverage exercises uppercase and
  mixed-case input.
- Validation: C3 LSP diagnostics were clean for the touched data-format files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1` passed with
  `pass=89 fail=0`.

### AUDIT-069: UTC `TimePoint` formatting drops the explicit timezone marker

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted temporal formatting fix
- Evidence:
  - `TimePoint` explicitly distinguishes `TIME_POINT_DATETIME` from
    `TIME_POINT_DATETIMETZ`, and `tz_offset` is only valid for
    `TIME_POINT_DATETIMETZ`, at `src/lisp/value_core_types.c3:80` through
    `src/lisp/value_core_types.c3:101`.
  - The `#time`/`time` ISO parser returns `TIME_POINT_DATETIMETZ` with
    `tz_offset = 0` for `YYYY-MM-DDTHH:MM:SSZ` at
    `src/lisp/primitives_data_formats.c3:207` through
    `src/lisp/primitives_data_formats.c3:232`.
  - `time_point_to_string` only emits a timezone offset when
    `tp.kind == TIME_POINT_DATETIMETZ && tp.tz_offset != 0`, then falls through
    to the local datetime formatter for zero-offset UTC values at
    `src/lisp/value_predicates_accessors_core.c3:72` through
    `src/lisp/value_predicates_accessors_core.c3:125`.
  - TOML conversion has a related zero-offset downgrade: `TOML_DATETIMETZ`
    keeps `TIME_POINT_DATETIMETZ` only when `tz != 0`, otherwise it returns a
    local datetime kind at `src/lisp/primitives_toml_bridge.c3:207` through
    `src/lisp/primitives_toml_bridge.c3:213`.
- Impact: parsing a UTC timestamp can lose the visible timezone on formatting,
  and TOML offset datetimes with `Z` or `+00:00` can lose the offset-aware kind
  entirely. That makes round-trips and equality/debug output ambiguous between
  local datetimes and UTC datetimes.
- Next action: preserve `TIME_POINT_DATETIMETZ` for zero-offset TOML datetimes
  and make `time_point_to_string` emit `Z` or `+00:00` for
  `TIME_POINT_DATETIMETZ` with `tz_offset == 0`. Add round-trip tests for
  `#time "...Z"`, TOML `Z`, and TOML `+00:00`.
- Resolution: zero-offset timezone-aware `TimePoint` values now retain
  `TIME_POINT_DATETIMETZ` and format with an explicit `Z`. TOML zero-offset
  datetime conversion preserves the offset-aware kind. Regression coverage
  exercises `#time`, TOML `Z`, and TOML `+00:00` round trips.
- Validation: C3 LSP diagnostics were clean for the touched time/TOML files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1` passed with
  `pass=89 fail=0`.

### AUDIT-070: AOT primitive resolver omits `hex` and `uuid` data-reader primitives

- Severity: Medium
- Status: Closed
- Classification: static build/config, targeted AOT primitive parity fix
- Evidence:
  - Runtime primitive registration includes `hex`, `base64`, and `uuid` at
    `src/lisp/eval_init_primitive_tables.c3:253` through
    `src/lisp/eval_init_primitive_tables.c3:265`.
  - Reader tags are parsed as ordinary one-argument calls at
    `src/lisp/parser_expr_reader_forms.c3:53` through
    `src/lisp/parser_expr_reader_forms.c3:81`.
  - The AOT primitive-variable hash includes `json`, `toml`, and `base64` at
    `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3:33`
    through `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3:44`,
    but `rg 'st\\.intern\\(\"(hex|uuid)\"\\)|lookup_prim\\(\"(hex|uuid)\"\\)' src/lisp`
    finds no AOT resolver entry for `hex` or `uuid`.
  - The public docs expose `#hex` and `#uuid` as reader-tag forms at
    `docs/LANGUAGE_SPEC.part-03.md:448` through
    `docs/LANGUAGE_SPEC.part-03.md:452` and
    `docs/reference/11-appendix-primitives.md:604` through
    `docs/reference/11-appendix-primitives.md:607`.
- Impact: interpreted `#hex` and `#uuid` forms can resolve at runtime, but AOT
  compilation lacks the same primitive lookup path. This creates compile/runtime
  parity drift for documented data reader tags.
- Next action: add `hex` and `uuid` to the AOT primitive-variable hash table and
  add compiler/codegen tests for `(hex "...")`, `(uuid "...")`, `#hex`, and
  `#uuid`.
- Resolution: the AOT primitive resolver now includes `hex` and `uuid`, keeping
  documented data-reader tags in parity with runtime primitive registration.
  Compiler/codegen coverage checks direct primitive forms and reader-tag forms.
- Validation: C3 LSP diagnostics were clean for the touched compiler/data-tag
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  direct compile probes for `#time`, `#hex`, and `#uuid` emitted
  `lookup_prim("time")`, `lookup_prim("hex")`, and `lookup_prim("uuid")`.
- Residual blocker: a broad `OMNI_LISP_TEST_SLICE=compiler` run still fails
  before the new assertions in an existing AOT runtime parity path with
  `scope global mutex used after shutdown cleanup`; focused compile probes were
  used for this closure.

### AUDIT-071: JSON Pointer escape decoding reports allocation failure as invalid syntax

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted error-path fix
- Evidence:
  - `json_pointer_decode_token` allocates a decoded token buffer when a pointer
    segment contains `~`; on `mem::malloc` failure it sets `result.ok = false`
    and returns the same state used for malformed escapes at
    `src/lisp/json_pointer_option_helpers.c3:20` through
    `src/lisp/json_pointer_option_helpers.c3:71`.
  - `prim_json_get` treats every `!token.ok` as
    `json-get: invalid pointer escape sequence` at `src/lisp/json.c3:100`
    through `src/lisp/json.c3:115`.
  - Other allocation paths in the same JSON Pointer lookup flow already raise a
    specific out-of-memory error, for example pointer-token string allocation at
    `src/lisp/json_pointer_option_helpers.c3:109` through
    `src/lisp/json_pointer_option_helpers.c3:120`.
- Impact: under allocation pressure, a valid escaped pointer such as `/a~1b`
  can surface as a user syntax error instead of an out-of-memory/runtime error.
  That misleads callers and fault-injection tests, and it weakens the
  repository's fail-closed allocation-error policy.
- Next action: make `JsonPointerToken` distinguish malformed escapes from
  allocation failure, return a canonical OOM payload for allocation failure, and
  add forced-allocation-failure coverage for escaped JSON Pointer tokens.
- Resolution: decoded JSON Pointer tokens now distinguish malformed escapes
  from allocation failure, and `json-get` reports allocation failure as a
  parser out-of-memory result instead of invalid syntax. Fault injection covers
  the escaped-token allocation path.
- Validation: C3 LSP diagnostics were clean for the touched JSON/test files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1` passed with
  `pass=77 fail=0`; `git diff --check` passed.

### AUDIT-072: `json-get` interns arbitrary missing object keys during read-only lookup

- Severity: Low
- Status: Closed
- Classification: runtime behavior/performance, targeted lookup side-effect fix
- Evidence:
  - `json-get` is documented as JSON Pointer-style lookup on Omni JSON values at
    `docs/reference/11-appendix-primitives.md:596`.
  - For every HASHMAP lookup, `json_pointer_lookup` first allocates a string key
    and probes the map, then calls `interp.symbols.intern(token)` and probes
    again with a symbol key when the string probe misses at
    `src/lisp/json_pointer_option_helpers.c3:98` through
    `src/lisp/json_pointer_option_helpers.c3:123`.
  - `SymbolTable.intern` inserts and heap-allocates a new symbol when the name
    is absent at `src/lisp/value_symbol_table.c3:207` through
    `src/lisp/value_symbol_table.c3:275`.
  - The tests cover missing JSON paths returning `nil` at
    `src/lisp/tests_runtime_data_unicode_groups.c3:57` through
    `src/lisp/tests_runtime_data_unicode_groups.c3:62`, but do not assert that
    missing-key lookup leaves the symbol table unchanged.
- Impact: read-only JSON pointer misses can permanently grow the global symbol
  table with attacker/user-controlled path segments. Repeated misses such as
  `/unknown-<id>` become a memory and symbol-table growth vector unrelated to
  the parsed JSON payload.
- Next action: use a non-interning symbol lookup for the symbol-key fallback, or
  restrict symbol fallback to explicit non-JSON maps. Add a regression test that
  repeated missing `json-get` lookups do not increase the symbol count.
- Resolution: JSON Pointer symbol-key fallback now probes existing symbol-table
  entries without interning absent names. Repeated missing `json-get` lookups no
  longer grow the symbol table.
- Validation: C3 LSP diagnostics were clean for the touched JSON/test files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1` passed with
  `pass=77 fail=0`; `git diff --check` passed.

### AUDIT-073: `json-emit` can serialize distinct dictionary keys to duplicate JSON object names

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted serialization contract fix
- Evidence:
  - The reference describes `json-emit` as emitting compact JSON at
    `docs/reference/11-appendix-primitives.md:590` through
    `docs/reference/11-appendix-primitives.md:596`, and examples show Omni
    dictionary keys becoming JSON object names at
    `docs/reference/07-io-networking.md:306` through
    `docs/reference/07-io-networking.md:312`.
  - `omni_hash_key_to_json` converts both string keys and symbol keys to JSON
    string keys using only their text at `src/lisp/json_emit.c3:109` through
    `src/lisp/json_emit.c3:128`.
  - `omni_hashmap_to_json` iterates every hashmap entry and appends each
    converted key/value pair to the yyjson object without tracking normalized
    key collisions at `src/lisp/json_emit.c3:130` through
    `src/lisp/json_emit.c3:148`.
  - Tests cover unsupported dictionary key types at
    `src/lisp/tests_runtime_data_unicode_groups.c3:96` through
    `src/lisp/tests_runtime_data_unicode_groups.c3:101`, but there is no
    coverage for a dictionary containing both `"a"` and `'a` keys.
- Impact: an Omni dictionary can contain distinct keys that both serialize as
  the same JSON object member name. The resulting JSON can contain duplicate
  object names or parse back with one value overwriting the other, so
  `json-emit` can silently lose dictionary information instead of failing
  closed.
- Next action: detect normalized key collisions during object emission and raise
  a deterministic `json-emit` error, or define a canonical collision policy and
  document it. Add a regression for `(json-emit (Dictionary "a" 1 'a 2))`.
- Resolution: JSON object emission now normalizes string and symbol key names
  before insertion and rejects duplicate emitted names with a deterministic
  `json-emit` error. Regression coverage includes a dictionary containing both
  `"a"` and `'a`.
- Validation: C3 LSP diagnostics were clean for the touched JSON/test files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1` passed with
  `pass=77 fail=0`; `git diff --check` passed.

### AUDIT-074: CSV delimiter and quote-char options validate bytes, not characters

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted option validation fix
- Evidence:
  - The CSV primitive docs describe the second argument as a delimiter string
    and list `delimiter`/`quote-char` as options at
    `docs/reference/11-appendix-primitives.md:610` through
    `docs/reference/11-appendix-primitives.md:611`.
  - The option parser says these options must be a one-character string, but it
    checks `value.str_len != 1`, which is byte length, at
    `src/lisp/primitives_data_formats_csv_options.c3:42` through
    `src/lisp/primitives_data_formats_csv_options.c3:64`.
  - `CsvParseOptions` and `CsvEmitOptions` store `delimiter` and `quote_char` as
    single `char` values at
    `src/lisp/primitives_data_formats_csv_options.c3:14` through
    `src/lisp/primitives_data_formats_csv_options.c3:29`.
  - Current tests only validate a multi-byte ASCII string rejection with `";;"`
    at `src/lisp/tests_runtime_data_unicode_groups.c3:226` through
    `src/lisp/tests_runtime_data_unicode_groups.c3:227`.
- Impact: a user-visible "one-character string" contract rejects non-ASCII
  one-codepoint delimiters such as `"§"` or `"•"` because they occupy multiple
  UTF-8 bytes. If the implementation intends byte delimiters, the docs and
  diagnostics are misleading; if it intends character delimiters, the
  representation cannot carry the contract.
- Next action: either document these options as single-byte ASCII delimiters, or
  store delimiter/quote-char as `char[]`/codepoint-aware values and add
  non-ASCII delimiter tests.
- Resolution: CSV delimiter and quote-char options now validate exactly one
  UTF-8 codepoint and store/match/write the full UTF-8 sequence. Regression
  coverage includes non-ASCII delimiter and quote-char behavior.
- Validation: C3 LSP diagnostics were clean for the touched CSV files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1` passed with
  `pass=89 fail=0`.

### AUDIT-075: `csv-emit` strict mode still allows `quote-style 'none` to emit structurally ambiguous rows

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted CSV emission validation fix
- Evidence:
  - `csv-emit` exposes `quote-style`, `strict`, `delimiter`, and line-ending
    options in the public primitive appendix at
    `docs/reference/11-appendix-primitives.md:610` through
    `docs/reference/11-appendix-primitives.md:611`.
  - `prim_csv_emit` defaults `opts.strict = true`, but its strict validation
    only checks `quote_char == delimiter` at
    `src/lisp/primitives_data_formats_csv_emit.c3:131` through
    `src/lisp/primitives_data_formats_csv_emit.c3:149`.
  - `csv_emit_escaped_cell` returns raw cell text for `CSV_QUOTE_NONE` even when
    the cell contains the delimiter, quote character, `\n`, or `\r`, at
    `src/lisp/primitives_data_formats_csv_emit.c3:17` through
    `src/lisp/primitives_data_formats_csv_emit.c3:57`.
  - The current test pins this ambiguous behavior by expecting
    `(csv-emit [["a,b" "x"]] '((quote-style 'none)))` to produce `a,b,x` at
    `src/lisp/tests_runtime_data_unicode_groups.c3:206` through
    `src/lisp/tests_runtime_data_unicode_groups.c3:209`.
- Impact: strict-mode CSV emission can output a different field shape from the
  input row. A two-cell row containing `"a,b"` becomes three comma-separated
  fields, and embedded newlines can create extra records. This is data
  corruption when strict mode is expected to preserve parseable structure.
- Next action: in strict mode, reject `quote-style 'none` when a cell contains
  delimiter, quote character, or line-ending bytes; keep permissive raw emission
  behind `strict false`. Update tests to cover both strict rejection and
  permissive output.
- Resolution: strict `csv-emit` now rejects `quote-style 'none` when raw cell
  text contains the delimiter, quote character, `\r`, or `\n`; `strict false`
  preserves the permissive raw-emission behavior. Regression coverage exercises
  both strict rejection and permissive output.
- Validation: C3 LSP diagnostics were clean for the touched CSV files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1` passed with
  `pass=89 fail=0`.

### AUDIT-076: `sort` and `sort-by` reject lists longer than 256 elements without a documented contract

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted collection implementation fix
- Evidence:
  - The public reference lists `sort` and `sort-by` simply as sorting
    primitives at `docs/reference/11-appendix-primitives.md:107` through
    `docs/reference/11-appendix-primitives.md:108`, and the language spec says
    `sort` sorts a list and `sort-by` sorts a list by comparator at
    `docs/LANGUAGE_SPEC.part-03.md:453` through
    `docs/LANGUAGE_SPEC.part-03.md:454`.
  - `prim_sort` copies the input list into a fixed `Value*[256]` stack buffer
    at `src/lisp/prim_collection_sort_array.c3:23` through
    `src/lisp/prim_collection_sort_array.c3:31`, then raises
    `sort: list input too long` when cons cells remain.
  - `prim_sort_by` repeats the same fixed-buffer conversion at
    `src/lisp/prim_collection_sort_array.c3:80` through
    `src/lisp/prim_collection_sort_array.c3:88`, then raises
    `sort-by: list input too long`.
  - Current sort tests only cover short lists at
    `src/lisp/tests_advanced_stdlib_numeric_misc_groups.c3:60` through
    `src/lisp/tests_advanced_stdlib_numeric_misc_groups.c3:71`; there is no
    >256-element regression or documented range limit.
- Impact: ordinary list sorting fails at 257 elements despite the language and
  reference docs presenting sorting as a general list operation. This creates a
  data-size cliff in user code and can break previously-valid list pipelines as
  inputs grow.
- Next action: replace the fixed stack buffer with a dynamically sized
  region-owned temporary buffer/array, or explicitly document and test a
  deliberate sorting size limit. Prefer removing the limit because other list
  combinators do not advertise a comparable cap.
- Resolution: `sort` and `sort-by` now collect finite proper lists into a
  dynamically sized heap-staged buffer with cycle, overflow, and allocation
  checks instead of a fixed 256-element stack array. Regression coverage sorts
  a list longer than 256 elements.
- Validation: C3 LSP diagnostics were clean for the touched sort/test/doc
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-string-predicate-format OMNI_TEST_SUMMARY=1`
  passed with `pass=65 fail=0`; `git diff --check` passed.

### AUDIT-077: `sort-by` treats boolean `false` comparator results as truthy

- Severity: High
- Status: Closed
- Classification: runtime behavior, targeted comparator semantics fix
- Evidence:
  - Omni truthiness is documented as only `nil` and `false` being falsy at
    `docs/reference/00-overview.md:24`,
    `docs/LANGUAGE_SPEC.part-01c.md:46` through
    `docs/LANGUAGE_SPEC.part-01c.md:51`, and
    `docs/reference/11-appendix-primitives.md:207` through
    `docs/reference/11-appendix-primitives.md:211`.
  - `prim_sort_by` documents its internal comparator handling as accepting a
    negative/zero/positive result or a truthy value for `a > b` at
    `src/lisp/prim_collection_sort_array.c3:90` through
    `src/lisp/prim_collection_sort_array.c3:103`.
  - For nonnumeric comparator results, the implementation uses `gt = !is_nil(r2)`
    at `src/lisp/prim_collection_sort_array.c3:104` through
    `src/lisp/prim_collection_sort_array.c3:109`, so the boolean `false` value
    is treated as true for swap decisions.
  - The public docs show a boolean comparator example:
    `(sort-by (λ (a b) (> a b)) '(1 3 2))` at
    `docs/reference/10-system-tooling.md:24` through
    `docs/reference/10-system-tooling.md:26`, but tests only cover a numeric
    comparator at `src/lisp/tests_advanced_stdlib_numeric_misc_groups.c3:63`.
- Impact: a comparator that correctly returns `false` can still trigger a swap,
  violating Omni's truthiness contract and making boolean-comparator sorting
  nondeterministic or reversed depending on the input sequence.
- Next action: use the shared falsiness predicate for nonnumeric comparator
  results instead of checking only `nil`, and add a regression where the
  comparator returns both `true` and `false`.
- Resolution: nonnumeric `sort-by` comparator results now use Omni's shared
  falsiness predicate, so both `nil` and boolean `false` are falsy. Regression
  coverage exercises comparators returning both `true` and `false`.
- Validation: C3 LSP diagnostics were clean for the touched sort/test/doc
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-string-predicate-format OMNI_TEST_SUMMARY=1`
  passed with `pass=65 fail=0`; `git diff --check` passed.

### AUDIT-078: `sort-by` docs show an uncurried comparator while the implementation requires a curried comparator

- Severity: Medium
- Status: Closed
- Classification: static/runtime contract mismatch, targeted docs-or-runtime fix
- Evidence:
  - The public sorting example shows `(sort-by (λ (a b) (> a b)) '(1 3 2))` at
    `docs/reference/10-system-tooling.md:24` through
    `docs/reference/10-system-tooling.md:26`, which is an uncurried two-argument
    lambda.
  - `prim_sort_by` applies the comparator in two steps, first calling it with
    the left value and then applying the returned value to the right value at
    `src/lisp/prim_collection_sort_array.c3:95` through
    `src/lisp/prim_collection_sort_array.c3:100`.
  - In-repo executable examples use the curried shape, including the sort test
    at `src/lisp/tests_advanced_stdlib_numeric_misc_groups.c3:63` and the
    Finwatch portfolio example at `examples/finwatch/portfolio.omni:63` through
    `examples/finwatch/portfolio.omni:64`.
  - `examples/finwatch/TODO.md:42` explicitly records `sort-by` as using a
    curried comparator, while `docs/LANGUAGE_SPEC.part-03.md:454` only says
    "Sort list by comparator" and does not define the comparator shape.
- Impact: users following the reference example pass a strict two-argument
  lambda into a runtime path that expects a one-argument function returning
  another function. The documented form can fail with an arity/application error
  even though the implementation and examples elsewhere require a different
  contract.
- Next action: choose one canonical comparator surface. If curried comparators
  are intentional, update the reference/spec examples and add a negative test
  for uncurried misuse; if the documented uncurried form is preferred, change
  `prim_sort_by` to apply two arguments directly and update existing curried
  examples/tests.
- Resolution: `sort-by` now accepts the documented two-argument comparator
  shape by applying comparators with two arguments while preserving the existing
  comparator result contract. Docs and tests were updated around the uncurried
  public surface.
- Validation: C3 LSP diagnostics were clean for the touched sort/test/doc
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-string-predicate-format OMNI_TEST_SUMMARY=1`
  passed with `pass=65 fail=0`; `git diff --check` passed.

### AUDIT-079: `keys`, `values`, and `List(Set)` silently abandon canonical order under allocation pressure

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted allocation-failure contract fix
- Evidence:
  - The collection spec requires `keys` and `values` to use the same canonical
    deterministic key order, and requires `values` to stay key-aligned, at
    `docs/LANGUAGE_SPEC.part-03.md:157` through
    `docs/LANGUAGE_SPEC.part-03.md:161`.
  - The reference also documents deterministic dictionary order and canonical
    ordered set conversion at `docs/reference/03-collections.part-01.md:76`
    through `docs/reference/03-collections.part-01.md:79`, and shows
    `(List s)` returning a canonical ordered list at
    `docs/reference/03-collections.part-01.md:52` through
    `docs/reference/03-collections.part-01.md:60`.
  - `hashmap_sorted_slots` allocates the sorted slot array with `mem::new_array`
    at `src/lisp/prim_collection_hashmap_key_helpers.c3:258` through
    `src/lisp/prim_collection_hashmap_key_helpers.c3:285`.
  - If that allocation fails, `hashmap_keys_list_canonical` and
    `hashmap_values_list_canonical` fall back to raw hash-table slot traversal
    instead of raising an allocation error at
    `src/lisp/prim_collection_hashmap_key_helpers.c3:288` through
    `src/lisp/prim_collection_hashmap_key_helpers.c3:329`.
  - `keys`, `values`, and `List` over sets all route through these helpers at
    `src/lisp/prim_collection_generic_set.c3:151` through
    `src/lisp/prim_collection_generic_set.c3:165` and
    `src/lisp/prim_collection_generic_set.c3:291` through
    `src/lisp/prim_collection_generic_set.c3:297`.
- Impact: under allocation pressure, the runtime suppresses an internal failure
  and returns a value that violates the documented canonical ordering contract.
  This can make dictionary/set iteration nondeterministic exactly when callers
  rely on ordered keys for stable serialization, comparison, or tests.
- Next action: fail closed when sorted-slot allocation fails, or sort by a
  bounded in-place/destination-arena path that preserves the canonical order.
  Add forced-allocation-failure tests for `keys`, `values`, and `List(Set)`.
- Resolution: canonical key/value/set materialization now fails closed when
  sorted-slot scratch allocation fails instead of falling back to hash bucket
  order. Regression coverage forces allocation pressure for `keys`, `values`,
  and `List(Set)`.
- Validation: C3 LSP diagnostics were clean for the touched collection/test
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1`
  passed with `pass=2114 fail=0`.

### AUDIT-080: `Array` and `list->array` reject lists above 65,536 elements without a documented limit

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted collection conversion fix
- Evidence:
  - The language docs present `(Array '(1 2 3))` as ordinary list-to-array
    conversion at `docs/LANGUAGE_SPEC.part-04.md:486`, and the collection
    reference shows the same constructor surface at
    `docs/reference/03-collections.part-01.md:26` through
    `docs/reference/03-collections.part-01.md:32`.
  - `prim_array` counts a single list argument only while `count < 65536`, then
    reports `Array: list input too long or cyclic` if cons cells remain at
    `src/lisp/prim_collection_sort_array.c3:144` through
    `src/lisp/prim_collection_sort_array.c3:155`.
  - `prim_list_to_array` repeats the same 65,536-element cap and reports
    `list->array: list input too long or cyclic` at
    `src/lisp/prim_collection_sort_array.c3:224` through
    `src/lisp/prim_collection_sort_array.c3:237`.
  - Current coverage exercises only small conversions, for example
    `src/lisp/tests_advanced_core_unicode_groups.c3:117` through
    `src/lisp/tests_advanced_core_unicode_groups.c3:120` and
    `src/lisp/tests_advanced_stdlib_numeric_misc_groups.c3:10` through
    `src/lisp/tests_advanced_stdlib_numeric_misc_groups.c3:20`.
- Impact: valid proper lists larger than 65,536 elements cannot be converted to
  arrays even when memory is available. The error message also conflates large
  finite lists with cyclic lists despite not performing real cycle detection.
- Next action: remove the fixed traversal cap by growing the destination array
  dynamically during traversal, or document the limit as an intentional contract
  and add boundary tests for 65,536 and 65,537 elements.
- Resolution: `Array(list)` and `list->array` now use a shared proper-list
  staging helper with dynamic growth, cycle detection, overflow checks, and OOM
  errors instead of the hidden 65,536-item cap.
- Validation: C3 LSP diagnostics were clean for the touched collection/test
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1`
  passed with `pass=2114 fail=0`.

### AUDIT-081: Manual `gensym` can stop producing unique symbol names after counter signed-cast overflow

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted hygiene counter overflow fix
- Evidence:
  - The public docs describe `gensym` as generating a unique symbol at
    `docs/reference/10-system-tooling.md:16`,
    `docs/reference/11-appendix-primitives.md:497`, and
    `docs/LANGUAGE_SPEC.part-03.md:446`.
  - The interpreter stores `gensym_counter` as `usz` at
    `src/lisp/value_interp_state.c3:217`.
  - `prim_gensym` increments that unsigned counter, casts it to `long`, and
    emits decimal digits only while the signed value is greater than zero at
    `src/lisp/primitives_core.c3:222` through
    `src/lisp/primitives_core.c3:240`.
  - The auto-gensym path formats the same counter as `usz` without the signed
    cast at `src/lisp/macros_template_expansion.c3:14` through
    `src/lisp/macros_template_expansion.c3:36`, so the manual primitive is the
    inconsistent path.
- Impact: after enough generated symbols push the `usz` counter above
  `long.max`, manual `gensym` can format the bare `g#` name repeatedly until the
  counter wraps back into a positive signed range. That violates the uniqueness
  contract for long-lived macro-heavy sessions.
- Next action: format `gensym_counter` as `usz` in `prim_gensym`, guard counter
  wraparound explicitly, and add a low-level regression that seeds the counter
  near `long.max`.
- Resolution: manual `gensym` now formats the unsigned `usz` counter directly,
  matching the auto-gensym path instead of casting through `long`. A regression
  seeds the counter at `long.max` and verifies the next symbol is
  `g#9223372036854775808`.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene-gensym OMNI_TEST_SUMMARY=1`
  passed with `pass=6 fail=0`; `git diff --check` passed.

### AUDIT-082: `tcp-read` ignores non-integer optional read arguments

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted TCP option validation fix
- Evidence:
  - `parse_tcp_read_options` initializes `max_bytes` to the default and only
    validates `args[1]` when it is already an integer at
    `src/lisp/async_tcp_transport_helpers.c3:93` through
    `src/lisp/async_tcp_transport_helpers.c3:101`. A present non-integer
    max-byte argument is ignored instead of rejected.
  - The same helper initializes `timeout_ms` to `-1` and only reads `args[2]`
    when it is an integer at `src/lisp/async_tcp_transport_helpers.c3:102`
    through `src/lisp/async_tcp_transport_helpers.c3:107`, so malformed timeout
    arguments are also silently ignored.
  - `prim_tcp_read` calls this helper before scheduling the async read at
    `src/lisp/async_tcp_transport_core.c3:65` through
    `src/lisp/async_tcp_transport_core.c3:72`.
  - The public networking reference documents `tcp-read` as a TCP operation at
    `docs/reference/07-io-networking.md:75` through
    `docs/reference/07-io-networking.md:111`, and the primitive appendix lists
    `__raw-tcp-read` as variadic at
    `docs/reference/11-appendix-primitives.md:630` through
    `docs/reference/11-appendix-primitives.md:634`, but neither documents
    accepting non-integer option values as defaults.
  - Existing async tests cover default reads and fiber-context failures in
    `src/lisp/tests_runtime_async_groups.c3:30` through
    `src/lisp/tests_runtime_async_groups.c3:31` and
    `src/lisp/tests_runtime_async_groups.c3:52` through
    `src/lisp/tests_runtime_async_groups.c3:129`; there is no regression for
    non-integer `tcp-read` max-byte or timeout arguments.
- Impact: malformed calls such as `(tcp-read handle "x")` can perform a default
  4096-byte read instead of failing closed. That hides API misuse in generated
  wrappers or effect payloads and makes TCP validation inconsistent with
  `tls-read` and `udp-recv`, which reject non-integer max-byte arguments.
- Next action: reject present non-integer `max-bytes` and `timeout-ms`
  arguments in `parse_tcp_read_options`, add payload-code regressions, and keep
  the fiber-context behavior covered separately.
- Resolution: `tcp-read` now normalizes both direct and effect-packed argument
  payloads before option validation, rejects non-integer `max-bytes` and
  `timeout-ms`, and the public stdlib wrapper now forwards optional arguments
  instead of discarding them.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1` passed
  with `pass=85 fail=0`; `git diff --check` passed.

### AUDIT-083: TCP, UDP, and TLS read sizes silently clamp above 65,536 bytes

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted read-size contract fix
- Evidence:
  - The shared TCP read cap is `ASYNC_IO_TCP_READ_HARD_MAX_BYTES = 65536` at
    `src/lisp/async_io_shared.c3:5` through `src/lisp/async_io_shared.c3:6`.
  - `parse_tcp_read_options` silently lowers larger requested TCP read sizes to
    that cap at `src/lisp/async_tcp_transport_helpers.c3:93` through
    `src/lisp/async_tcp_transport_helpers.c3:101`.
  - `tls-read` validates type and positivity but then silently clamps larger
    requests to `65536` at `src/lisp/tls_primitives.c3:34` through
    `src/lisp/tls_primitives.c3:45`.
  - `udp-recv` validates type and positivity but then silently clamps larger
    requests to `65536` at `src/lisp/async_udp_primitives.c3:166` through
    `src/lisp/async_udp_primitives.c3:177`.
  - The networking reference shows `tcp-read`, `udp-recv`, and `tls-read` at
    `docs/reference/07-io-networking.md:75` through
    `docs/reference/07-io-networking.md:146` and
    `docs/reference/07-io-networking.md:220` through
    `docs/reference/07-io-networking.md:253`, but does not state a 65,536-byte
    request limit or truncation policy. The primitive appendix also lists these
    read primitives as variadic at `docs/reference/11-appendix-primitives.md:633`,
    `docs/reference/11-appendix-primitives.md:639`, and
    `docs/reference/11-appendix-primitives.md:664` without a cap contract.
  - Tests cover non-integer and non-positive `tls-read` max bytes at
    `src/lisp/tests_runtime_async_io_tls_groups.c3:187` through
    `src/lisp/tests_runtime_async_io_tls_groups.c3:198`, and default TCP/UDP
    reads in `src/lisp/tests_runtime_async_groups.c3:52` through
    `src/lisp/tests_runtime_async_groups.c3:144`, but there is no boundary test
    for requests above `65536`.
- Impact: callers requesting a larger read receive less data than requested
  without an error or documented partial-read signal. For UDP this can truncate
  datagrams; for TCP/TLS streams it obscures the actual buffering contract and
  can force hidden application-level retry logic.
- Next action: either document the 65,536-byte maximum as an explicit API
  contract and return a range error above it, or support caller-selected sizes
  through a bounded allocation policy. Add regression tests for exactly 65,536
  and 65,537 requested bytes on each public read surface.
- Resolution: `tcp-read`, `udp-recv`, and `tls-read` now reject requests above
  65,536 bytes instead of silently clamping. Public `tcp-read` and `udp-recv`
  wrappers now preserve optional read arguments in the effect payload, matching
  the `tls-read` variadic wrapper pattern.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1` passed
  with `pass=85 fail=0`; `git diff --check` passed.

### AUDIT-084: HTTP response parser returns partial success for malformed responses

- Severity: High
- Status: Closed
- Classification: runtime behavior, targeted protocol-validation fix
- Evidence:
  - `parse_response` skips bytes until the first space and then accumulates
    digits into `status`, starting from `0`, at
    `src/lisp/http_url_response.c3:158` through
    `src/lisp/http_url_response.c3:173`. It does not verify an `HTTP/` prefix
    or that at least one status digit was parsed.
  - The parser writes the `status` key before validating the header terminator
    at `src/lisp/http_url_response.c3:175` through
    `src/lisp/http_url_response.c3:188`.
  - `headers` and `body` are conditional on `header_end > 0` at
    `src/lisp/http_url_response.c3:190` through
    `src/lisp/http_url_response.c3:214`, so responses without `\r\n\r\n` return
    a dictionary containing only `status`.
  - `http-get` and `http-request` return the parser result directly after the
    network read at `src/lisp/http.c3:70` through `src/lisp/http.c3:75` and
    `src/lisp/http.c3:158` through `src/lisp/http.c3:164`.
  - The HTTP primitive comment promises `(http-get url) -> dict with 'status
    'headers 'body` at `src/lisp/http.c3:22` through `src/lisp/http.c3:24`, and
    the reference presents `http-get` as returning a response dict at
    `docs/reference/07-io-networking.md:272` through
    `docs/reference/07-io-networking.md:279`.
  - Current HTTP parser coverage exercises a well-formed response at
    `src/lisp/tests_runtime_feature_http_groups.c3:185` through
    `src/lisp/tests_runtime_feature_http_groups.c3:214`, and allocation
    failures on well-formed responses at
    `src/lisp/tests_memory_lifetime_runtime_alloc_groups_http_response.c3:12`
    through
    `src/lisp/tests_memory_lifetime_runtime_alloc_groups_http_response.c3:22`.
    There is no malformed response regression for empty input, missing status
    digits, or missing header terminator.
- Impact: a bad or malicious server can produce an apparently valid response
  dictionary with `status` equal to `0` and no `headers` or `body` instead of a
  deterministic `io/http-malformed-response` error. Client code may treat
  transport or protocol corruption as ordinary application data.
- Next action: require a valid status line, at least one status digit, and a
  header terminator before returning success. Add parser-only regressions for
  empty input, garbage input, `HTTP/1.1 OK\r\n\r\n`, and no-terminator
  responses.
- Resolution: `parse_response` now fails closed with `http: malformed response`
  before building a partial response dictionary when input is empty, lacks an
  HTTP status line, lacks three status digits, or has no `\r\n\r\n` terminator.
  Well-formed responses still return status, headers, and body.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=http` passed with `pass=31
  fail=0`; `git diff --check` passed.

### AUDIT-085: `range-from` overflows integer state at `long.max`

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted iterator range fix
- Evidence:
  - `prim_iterator_range_from` accepts a single integer start value at
    `src/lisp/primitives_iter_sources.c3:52` through
    `src/lisp/primitives_iter_sources.c3:58`.
  - The range thunk returns the current value and constructs the next iterator
    state with `current.int_val + 1` at
    `src/lisp/primitives_iter_sources.c3:63` through
    `src/lisp/primitives_iter_sources.c3:69`. There is no boundary guard or
    promotion to `BigInteger`.
  - The public contract describes `range-from` as an infinite iterator yielding
    `n, n+1, n+2...` at `docs/reference/12-appendix-stdlib.md:57`,
    `docs/reference/06-effects.md:245`, and
    `docs/LANGUAGE_SPEC.part-04.md:20`.
  - Current iterator tests cover only small starts, for example
    `src/lisp/tests_advanced_core_unicode_groups.c3:273` through
    `src/lisp/tests_advanced_core_unicode_groups.c3:277`.
- Impact: `(take 2 (range-from 9223372036854775807))` can overflow the next
  state, wrap, or trap depending on C3 integer-overflow behavior instead of
  either producing a promoted integer sequence or raising a deterministic range
  error.
- Next action: guard `current.int_val == long.max` in the range thunk. Choose
  either BigInteger-backed continuation or a deterministic range error, then add
  a boundary regression for `long.max`.
- Resolution: `range-from` now promotes the next state past `long.max` into a
  `BigInteger`, preserving the documented infinite sequence semantics instead
  of overflowing fixed-width integer state.
- Validation: C3 LSP diagnostics were clean for the touched numeric/iterator
  files; `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1`
  passed with `pass=181 fail=0`; focused direct range overflow eval passed.

### AUDIT-086: Iterator materialization silently truncates after 4096 elements

- Severity: High
- Status: Closed
- Classification: runtime behavior, structural iterator-materialization fix
- Evidence:
  - `ITER_BUFFER_MAX` is fixed at `4096` in
    `src/lisp/primitives_iter_coroutine.c3:5`.
  - `iterator_consume_items` loops only while `count < ITER_BUFFER_MAX` at
    `src/lisp/primitives_iter_terminal.c3:82` through
    `src/lisp/primitives_iter_terminal.c3:105` and returns success without
    checking whether the iterator still has additional items.
  - `prim_collect` and `prim_to_array` build a list or array from that consumed
    prefix at `src/lisp/primitives_iter_terminal.c3:111` through
    `src/lisp/primitives_iter_terminal.c3:151`.
  - `List` delegates a single iterator argument to `prim_collect` at
    `src/lisp/primitives_core.c3:151` through `src/lisp/primitives_core.c3:154`;
    `Array` delegates a single iterator argument to `prim_to_array` at
    `src/lisp/prim_collection_sort_array.c3:132` through
    `src/lisp/prim_collection_sort_array.c3:136`.
  - Tensor constructors also materialize iterator data through `prim_to_array`
    at `src/lisp/prim_tensor_construct_aux.c3:30` through
    `src/lisp/prim_tensor_construct_aux.c3:38`,
    `src/lisp/prim_tensor_construct_aux.c3:94` through
    `src/lisp/prim_tensor_construct_aux.c3:101`, and
    `src/lisp/prim_tensor_construct_aux.c3:104` through
    `src/lisp/prim_tensor_construct_aux.c3:110`.
  - The collections reference says constructor dispatch is the canonical
    terminal consumer and that `(Array iterator)`, `(List iterator)`, and
    `(Tensor iterator)` consume iterators at
    `docs/reference/03-collections.part-01.md:334` through
    `docs/reference/03-collections.part-01.md:336`; no 4096-element cap is
    documented.
  - Existing tests exercise only tiny iterator conversions, for example
    `src/lisp/tests_advanced_core_unicode_groups.c3:115` through
    `src/lisp/tests_advanced_core_unicode_groups.c3:118` and
    `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part3.c3:401`
    through
    `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part3.c3:419`.
- Impact: finite iterators with more than 4096 items are silently truncated, and
  unbounded iterators without an explicit `take` return a 4096-item prefix as if
  materialization completed. This can silently corrupt collection and tensor
  data.
- Next action: either make iterator materialization dynamically grow destination
  storage, or raise a documented `iterator/materialization-limit` style error
  when the fixed cap is reached and another item is available. Add regressions
  for a 4097-item finite iterator and direct materialization of `range-from`.
- Resolution: iterator `collect` and `to-array` now dynamically grow
  materialization storage instead of truncating at 4096, and known unbounded
  iterators fail closed unless callers bound them first.
- Validation: C3 LSP diagnostics were clean for the touched iterator/test
  files; `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1`
  passed with `pass=2114 fail=0`; bounded container
  `memory-lifetime-smoke` passed with `260 passed, 0 failed` in worker
  validation.

### AUDIT-087: `random-int` uses modulo reduction and biases non-power-of-two bounds

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted random sampling fix
- Evidence:
  - `random-int` fills a `ulong` from `getrandom` and returns
    `(long)(buf % (ulong)n)` at `src/lisp/prim_math.c3:53` through
    `src/lisp/prim_math.c3:58`.
  - The reference documents `random-int` as producing a random integer in
    `[0, n)` at `docs/reference/11-appendix-primitives.md:531`.
  - Existing tests check only type and range at
    `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:714` through
    `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:716`.
  - Existing `AUDIT-025` covers non-positive bounds only; it does not cover
    distribution quality for valid positive bounds.
- Impact: when `n` does not divide `2^64`, lower residues occur one extra time
  in the modulo mapping. The bias is tiny for small bounds but still violates
  the expected uniform sampling contract and becomes more relevant for
  randomized algorithms, tests, and security-adjacent callers.
- Next action: replace modulo sampling with rejection sampling using the largest
  accepted multiple of `n`, retry on out-of-range draws, and add deterministic
  helper-level tests for edge bounds such as `3`, `10`, `2^32 - 1`, and
  `long.max`.
- Resolution: `random-int` now uses rejection sampling against the largest
  accepted multiple of the requested bound, avoiding modulo bias for
  non-power-of-two bounds.
- Validation: C3 LSP diagnostics were clean for the touched numeric files;
  `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1`
  passed with `pass=181 fail=0`; focused large-bound `random-int` eval passed.

### AUDIT-088: Float-to-Integer range checks accept the rounded `long.max + 1` boundary

- Severity: High
- Status: Closed
- Classification: runtime behavior, targeted numeric conversion fix
- Evidence:
  - `conversion_try_double_to_int` defines
    `CONVERSION_INT_MAX_DOUBLE = (double)long.max` and only rejects
    `v > CONVERSION_INT_MAX_DOUBLE` before casting `v` to `long` at
    `src/lisp/prim_string_convert.c3:17` through
    `src/lisp/prim_string_convert.c3:38`.
  - `math_try_double_to_int` repeats the same boundary pattern with
    `MATH_INT_MAX_DOUBLE = (double)long.max` at
    `src/lisp/prim_math_arithmetic.c3:8` through
    `src/lisp/prim_math_arithmetic.c3:16`.
  - Binary64 cannot exactly represent `long.max`; `(double)long.max` rounds to
    `9223372036854775808.0`, so the exact first out-of-range integer value is
    accepted by the `>` check and then cast to `long`.
  - `Integer`, `inexact->exact`, and string-to-Integer conversion use
    `conversion_int_from_double_or_raise` at
    `src/lisp/prim_string_convert.c3:63` through
    `src/lisp/prim_string_convert.c3:125` and
    `src/lisp/prim_string_convert.c3:540` through
    `src/lisp/prim_string_convert.c3:544`.
  - Scalar rounding primitives use `math_int_from_double_or_raise` at
    `src/lisp/prim_math_core.c3:508` through
    `src/lisp/prim_math_core.c3:558`, and tensor rounding to `BigInteger`
    storage uses `math_try_double_to_int` at
    `src/lisp/prim_tensor_rounding_math.c3:262` through
    `src/lisp/prim_tensor_rounding_math.c3:279`.
  - The type-system reference requires narrowing to be finite and within
    `Integer` range, with overflow/non-finite inputs raising
    `type/arg-mismatch`, at `docs/reference/04-type-system.md:161` through
    `docs/reference/04-type-system.md:165`.
  - Current conversion tests cover string overflow and huge non-finite-ish
    Float64 inputs, but not the exact binary64 `2^63` boundary, at
    `src/lisp/tests_advanced_stdlib_numeric_groups.c3:356` through
    `src/lisp/tests_advanced_stdlib_numeric_groups.c3:364`.
- Impact: `(Integer 9223372036854775808.0)`, `(truncate 9223372036854775808.0)`,
  and tensor rounding paths can invoke an out-of-range float-to-signed-integer
  cast instead of failing closed. Depending on C3/backend behavior this can
  wrap, trap, or produce a misleading fixed-width integer.
- Next action: compare against exact half-open bounds before casting, for
  example reject `v >= 9223372036854775808.0` and `v < -9223372036854775808.0`
  after truncation semantics are applied. Add scalar and tensor regressions for
  `2^63`, `nextafter(2^63, 0)`, `long.min`, and `-2^63 - ulp`.
- Resolution: Float64-to-Integer conversion now rejects `2^63`, accepts
  `-2^63`, and avoids the unsafe direct cast near the `long.min` boundary.
  Scalar conversion and rounding paths share the corrected bounds.
- Validation: C3 LSP diagnostics were clean for the touched conversion/math
  files; `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1`
  passed with `pass=181 fail=0`.

### AUDIT-089: BigInteger/Float64 ordered comparisons lose precision above `2^53`

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, structural numeric comparison fix
- Evidence:
  - `numeric_values_compare` converts both operands to `double` whenever either
    side is `DOUBLE` or `FLOAT32`, then compares the rounded values at
    `src/lisp/value_big_integer.c3:228` through
    `src/lisp/value_big_integer.c3:239`.
  - `try_numeric_to_double` converts `BIG_INTEGER` through
    `big_integer_try_to_double` at
    `src/lisp/value_predicates_accessors_basic.c3:128` through
    `src/lisp/value_predicates_accessors_basic.c3:145`.
  - Public `<`, `>`, `<=`, and `>=` delegate to `numeric_values_compare` through
    `numeric_compare_primitive_result` at `src/lisp/primitives_core.c3:32`
    through `src/lisp/primitives_core.c3:48`.
  - `min` and `max` explicitly support `BigInteger`, but their Float64-mixed
    branches also coerce operands to double before selecting a result at
    `src/lisp/prim_math_core.c3:598` through
    `src/lisp/prim_math_core.c3:612` and
    `src/lisp/prim_math_core.c3:634` through
    `src/lisp/prim_math_core.c3:648`.
  - The language spec states that ordering comparisons, `min`, and `max`
    support `BigInteger` values at `docs/LANGUAGE_SPEC.part-01b.md:36` through
    `docs/LANGUAGE_SPEC.part-01b.md:41`, and that `min`/`max` support exact
    `BigInteger` comparison at `docs/LANGUAGE_SPEC.part-03.md:191` through
    `docs/LANGUAGE_SPEC.part-03.md:193`.
  - Existing mixed comparison coverage only checks a small exactly representable
    value, `(BigInteger "42")` against `42.5`, at
    `src/lisp/tests_advanced_stdlib_numeric_groups.c3:351` through
    `src/lisp/tests_advanced_stdlib_numeric_groups.c3:352`.
- Impact: comparisons such as
  `(< (BigInteger "9007199254740993") 9007199254740992.0)` can be evaluated
  after both sides collapse to the same binary64 value, producing incorrect
  ordering. `min`/`max` can then select the wrong side or return a rounded
  Float64 for values whose exact order is still decidable.
- Next action: implement mixed exact/inexact comparison without lossy
  BigInteger-to-double equality collapse. Compare BigInteger against the exact
  integer interval represented by the Float64 mantissa/exponent, then add
  regressions around `2^53`, `2^63`, and adjacent values.
- Resolution: mixed BigInteger/Integer versus Float64 comparisons now use exact
  binary64 decomposition instead of lossy double conversion. `min` and `max`
  preserve exact BigInteger winners when mixed with Float64, and `sort` numeric
  ordering now delegates to `numeric_values_compare` for BigInteger/BigFloat
  ordering.
- Validation: C3 LSP diagnostics were clean for the touched numeric/sort files;
  `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1`
  passed with `pass=181 fail=0`; focused BigInteger and BigFloat sort evals
  passed.

### AUDIT-090: Fixed-width-only `is_number` consumers reject public arbitrary-precision numbers

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, structural numeric predicate cleanup
- Evidence:
  - The public `number?` contract includes `BigInteger`, `BigFloat`, and
    `BigComplex` at `docs/LANGUAGE_SPEC.part-03.md:82` through
    `docs/LANGUAGE_SPEC.part-03.md:87`, and tests assert
    `(number? (BigInteger "1"))` and `(number? (BigFloat "1.25"))` at
    `src/lisp/tests_advanced_stdlib_numeric_groups.c3:172` through
    `src/lisp/tests_advanced_stdlib_numeric_groups.c3:182`.
  - The internal helper named `is_number` only returns true for `INT`,
    `DOUBLE`, `FLOAT32`, `COMPLEX128`, and `COMPLEX64` at
    `src/lisp/value_predicates_accessors_basic.c3:106` through
    `src/lisp/value_predicates_accessors_basic.c3:114`.
  - `sort` uses `is_number` for its "numbers by value" branch, so
    `BigInteger` and `BigFloat` elements skip numeric ordering entirely at
    `src/lisp/prim_collection_sort_array.c3:37` through
    `src/lisp/prim_collection_sort_array.c3:42`.
  - Deduce aggregate `sum` uses `is_number` before accepting numeric input and
    therefore rejects `BigInteger`/`BigFloat` despite raising an error that says
    "sum aggregate expects numeric input" at
    `src/lisp/deduce_rule_eval_exec_aggregates.c3:61` through
    `src/lisp/deduce_rule_eval_exec_aggregates.c3:73`.
  - Deduce literal numeric-call extraction also uses `is_number` for
    comparison and arithmetic folding at
    `src/lisp/deduce_schema_query_literal_numeric_calls.c3:29` through
    `src/lisp/deduce_schema_query_literal_numeric_calls.c3:47` and
    `src/lisp/deduce_schema_query_literal_numeric_calls.c3:58` through
    `src/lisp/deduce_schema_query_literal_numeric_calls.c3:69`.
- Impact: arbitrary-precision values are public `Number`s in one part of the
  runtime but are silently treated as non-numeric by other consumers. This can
  leave `sort` results unsorted for exact large numbers, reject valid Deduce
  aggregate input, and disable constant-folding/planning paths for numeric
  literals that should participate.
- Resolution: public numeric consumers now use public numeric ordering/helpers
  on the covered paths: collection sorting uses `numeric_values_compare`, Deduce
  `sum` accepts BigInteger/BigFloat via `is_numeric_value` while preserving the
  accumulated `Value*`, and Deduce literal numeric extraction routes through the
  generic numeric helpers/primitives.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  focused BigInteger and BigFloat sort evals passed.
- Validation: previous numeric sorting checks passed; Deduce aggregate coverage
  passed with `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=aggregate
  OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` reporting
  `pass=226 fail=0`; focused read-failure Deduce coverage also passed with
  `pass=8 fail=0`.

### AUDIT-091: NN checkpoint Float32 decode bypasses normal finite/range validation

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted checkpoint validation fix
- Evidence:
  - Normal `Float32` construction validates that the source double is finite and
    within `CONVERSION_FLOAT32_FINITE_LIMIT` before calling `make_float32`, at
    `src/lisp/prim_string_convert.c3:27` through
    `src/lisp/prim_string_convert.c3:32` and
    `src/lisp/prim_string_convert.c3:260` through
    `src/lisp/prim_string_convert.c3:264`.
  - Tensor Float32 storage uses the same finite/range guard through
    `tensor_double_to_float32` at `src/lisp/prim_tensor_construct.c3:240`
    through `src/lisp/prim_tensor_construct.c3:245`.
  - Checkpoint decode accepts an encoded `"float32"` scalar, converts JSON
    `INT`/`DOUBLE`/`FLOAT32` values to `double`, then casts directly with
    `(float)number` at `src/lisp/prim_nn_checkpoint.c3:373` through
    `src/lisp/prim_nn_checkpoint.c3:377`.
  - `make_float32` itself is a raw value constructor and does not validate
    finite/range constraints at `src/lisp/value_constructors_core.c3:62`
    through `src/lisp/value_constructors_core.c3:66`.
  - Checkpoint JSON parsing maps JSON real numbers directly to `DOUBLE` at
    `src/lisp/json.c3:26` through `src/lisp/json.c3:28`, so a malformed
    checkpoint can carry a finite Float64 value outside Float32 range into this
    unchecked cast.
  - The checkpoint loaders are documented as validating checkpoint envelopes and
    restoring model/optimizer state at
    `docs/reference/11-appendix-primitives.md:289` and
    `docs/reference/11-appendix-primitives.md:342` through
    `docs/reference/11-appendix-primitives.md:344`.
- Impact: malformed checkpoints can introduce non-finite or implementation-
  dependent Float32 scalar values even though normal language conversion and
  tensor construction reject those inputs. This weakens checkpoint validation and
  can feed invalid optimizer/spec metadata into later NN or ML paths.
- Resolution: checkpoint Float32 decode now uses the normal finite/range
  conversion path and reports deterministic checkpoint validation errors for
  out-of-range values.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=2131 fail=0`.

### AUDIT-092: NN checkpoint dictionary encoding sizes temporary arrays by hash capacity

- Severity: Low
- Status: Closed
- Classification: performance behavior, targeted checkpoint allocation fix
- Evidence:
  - `HashMap` tracks both physical `capacity` and logical `count` at
    `src/lisp/value_runtime_types.c3:123` through
    `src/lisp/value_runtime_types.c3:127`.
  - Insertions grow capacity at a 70% load threshold, while removals decrement
    `count` but never shrink capacity at `src/lisp/prim_collection_hashmap.c3:235`
    through `src/lisp/prim_collection_hashmap.c3:239` and
    `src/lisp/prim_collection_hashmap.c3:349` through
    `src/lisp/prim_collection_hashmap.c3:377`.
  - `nn_ckpt_encode_dict` allocates the encoded `entries` array with
    `hm.capacity` rather than `hm.count`, then appends only occupied slots by
    incrementing `entries.array_val.length` at
    `src/lisp/prim_nn_checkpoint.c3:129` through
    `src/lisp/prim_nn_checkpoint.c3:143`.
  - Arrays keep separate `length` and `capacity`, and `make_array_checked`
    initializes `length = 0`, so this is not a decode-corruption bug; it is
    over-allocation of checkpoint staging storage at
    `src/lisp/value_predicates_accessors_basic.c3:347` through
    `src/lisp/value_predicates_accessors_basic.c3:355`.
  - Model and optimizer checkpoints recursively encode ordinary dictionaries
    through this path via `nn_ckpt_encode_value` at
    `src/lisp/prim_nn_checkpoint.c3:220` through
    `src/lisp/prim_nn_checkpoint.c3:224`.
- Impact: sparse or churned dictionaries can allocate and scan substantially
  more checkpoint staging memory than their logical entry count requires. This is
  especially visible for optimizer/model state dictionaries that grow during
  training setup and then remove or overwrite entries before checkpointing.
- Resolution: checkpoint dictionary encoding now sizes staging entries from
  logical count while continuing to scan physical hashmap slots.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=2131 fail=0`.

### AUDIT-093: NN checkpoint dictionary decode downcasts entry-derived capacity after unchecked sizing

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted checkpoint validation fix
- Evidence:
  - `nn_ckpt_decode_dict` validates that the encoded `entries` field is an
    array, then computes the destination dictionary hint as
    `(uint)(entries.array_val.length * 2 + 4)` at
    `src/lisp/prim_nn_checkpoint.c3:291` through
    `src/lisp/prim_nn_checkpoint.c3:296`.
  - `nn_make_dict` accepts only a `uint` capacity at
    `src/lisp/prim_nn_dataspec.c3:7` through
    `src/lisp/prim_nn_dataspec.c3:8`, so the `usz` entry length has already
    been truncated before hashmap normalization can reject an oversized request.
  - Hashmap capacity normalization only sees the post-cast `uint` at
    `src/lisp/prim_collection_hashmap.c3:14` through
    `src/lisp/prim_collection_hashmap.c3:22` and
    `src/lisp/prim_collection_hashmap.c3:399` through
    `src/lisp/prim_collection_hashmap.c3:402`.
  - The shared decoder is reachable from `nn/load-spec`, `nn/load`, and
    `ml/load-optimizer` at `src/lisp/prim_nn_checkpoint.c3:496` through
    `src/lisp/prim_nn_checkpoint.c3:504`,
    `src/lisp/prim_nn_checkpoint.c3:520` through
    `src/lisp/prim_nn_checkpoint.c3:527`, and
    `src/lisp/prim_ml_optimizer_checkpoint.c3:104` through
    `src/lisp/prim_ml_optimizer_checkpoint.c3:110`.
  - Existing checkpoint round-trip tests cover small payloads, but do not cover
    oversized encoded dictionary entry counts or capacity-boundary rejection.
- Impact: malformed or hostile checkpoint payloads can move past envelope
  validation with a wrapped or truncated destination-capacity hint, then spend
  work recursively decoding entries and repeatedly growing the dictionary instead
  of failing closed before allocation. This is a resource-exhaustion and
  validation-boundary bug, not a proven memory-corruption path.
- Resolution: checkpoint dictionary decode now preflights entry-derived
  capacity before the `uint` destination hint and fails closed with a
  deterministic checkpoint error on overflow-sized payloads.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=2131 fail=0`.

### AUDIT-094: Shared checkpoint source loader reports `nn/load` for other checkpoint primitives

- Severity: Low
- Status: Closed
- Classification: runtime diagnostics, targeted shared-helper contract fix
- Evidence:
  - `nn_ckpt_load_source` hard-codes `nn/load` in both the argument-type error
    and file-read error text at `src/lisp/prim_nn_checkpoint.c3:425` through
    `src/lisp/prim_nn_checkpoint.c3:430`.
  - `nn_ckpt_parse_root` calls that helper without a primitive-name parameter at
    `src/lisp/prim_nn_checkpoint.c3:436` through
    `src/lisp/prim_nn_checkpoint.c3:438`.
  - The same parse path is used by `nn/load-spec` and `nn/load` at
    `src/lisp/prim_nn_checkpoint.c3:496` through
    `src/lisp/prim_nn_checkpoint.c3:522`, and by `ml/load-optimizer` at
    `src/lisp/prim_ml_optimizer_checkpoint.c3:104` through
    `src/lisp/prim_ml_optimizer_checkpoint.c3:107`.
  - The reference exposes these as distinct primitives at
    `docs/reference/11-appendix-primitives.md:289` and
    `docs/reference/11-appendix-primitives.md:342` through
    `docs/reference/11-appendix-primitives.md:344`.
- Impact: users and tests invoking `nn/load-spec` or `ml/load-optimizer` can
  receive diagnostics naming `nn/load`, which makes checkpoint failures harder
  to triage and can hide primitive-specific regressions behind the wrong
  surface name.
- Resolution: checkpoint source loading now threads the calling primitive name
  through the shared parse/load helpers so `nn/load-spec`, `nn/load`, and
  `ml/load-optimizer` report their own surface names.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=2131 fail=0`.

### AUDIT-095: `json-parse` object conversion downcasts object-size capacity after unchecked multiplication

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted parser capacity-bound fix
- Evidence:
  - The yyjson FFI exposes object size as `usz` at
    `src/lisp/json_yyjson_ffi.c3:42`.
  - `json_to_value` reads that size into `count`, computes
    `(count < 8 ? 8 : count * 2)`, then casts the result to `uint` before
    `make_hashmap_checked` at `src/lisp/json.c3:56` through
    `src/lisp/json.c3:59`.
  - Hashmap normalization only validates the already-truncated `uint` capacity
    at `src/lisp/prim_collection_hashmap.c3:14` through
    `src/lisp/prim_collection_hashmap.c3:22` and
    `src/lisp/prim_collection_hashmap.c3:399` through
    `src/lisp/prim_collection_hashmap.c3:402`.
  - Allocation-failure tests cover forced constructor failure for a tiny JSON
    object at `src/lisp/tests_memory_lifetime_runtime_alloc_groups_data_parsing.c3:18`
    through `src/lisp/tests_memory_lifetime_runtime_alloc_groups_data_parsing.c3:34`,
    but do not cover oversized object-size arithmetic or capacity truncation.
- Impact: extremely large parsed JSON objects are not rejected at the size
  boundary where the parser still has the authoritative `usz` count. Depending
  on the wrapped `uint` value, the conversion may fail with a generic allocation
  error or allocate an undersized dictionary and grow repeatedly during object
  insertion.
- Next action: add a shared checked helper for `usz` element-count to hashmap
  capacity-hint conversion, use it before `json_to_value` object construction,
  and add a synthetic or fault-injected regression for counts above the maximum
  representable normalized hashmap capacity.
- Resolution: added `hashmap_capacity_hint_for_entry_count` as the shared
  checked `usz` count-to-capacity helper, used it for JSON object conversion,
  and applied the same unchecked-count fix to TOML table conversion. Capacity
  hint regressions cover normal small hints and fail-closed oversized hints.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_SUMMARY=1` passed with
  `pass=89 fail=0`; `git diff --check` passed.

### AUDIT-096: `--init` accepts traversal and absolute project names before filesystem writes

- Severity: Medium
- Status: Closed
- Classification: CLI filesystem behavior, targeted path-contract fix
- Evidence:
  - `run_init` takes `argv[init_idx + 1]` as the project `name` and only rejects
    leading option-like arguments at `src/entry_project_init_bind.c3:31`
    through `src/entry_project_init_bind.c3:40`; it does not reject `/`,
    `\`, `.`, `..`, control bytes, or absolute paths.
  - `build_project_path` copies that name verbatim into each scaffold path at
    `src/entry_project_init_files.c3:12` through
    `src/entry_project_init_files.c3:27`.
  - Directory creation, file writes, and rollback cleanup all use paths derived
    from the unvalidated name at `src/entry_project_init_files.c3:80` through
    `src/entry_project_init_files.c3:108`,
    `src/entry_project_init_writers.c3:5` through
    `src/entry_project_init_writers.c3:64`, and
    `src/entry_project_init_writer_project_json.c3:5` through
    `src/entry_project_init_writer_project_json.c3:34`.
  - The user-facing tooling docs present `--init myproject` as creating one
    project directory at `docs/PROJECT_TOOLING.part-01.md:567` through
    `docs/PROJECT_TOOLING.part-01.md:587` and
    `docs/reference/10-system-tooling.md:148` through
    `docs/reference/10-system-tooling.md:155`.
  - Nearby bindgen output paths already enforce safe generated names at
    `src/entry_bind_paths.c3:42` through `src/entry_bind_paths.c3:60`, with
    regressions for `../escape`, nested paths, backslashes, dotted names,
    spaces, parens, and empty names at
    `src/lisp/tests_compiler_codegen_groups_tail_bindgen_path_guard.c3:23`
    through `src/lisp/tests_compiler_codegen_groups_tail_bindgen_path_guard.c3:47`.
- Impact: `omni --init ../escape`, `omni --init /tmp/escape`, or slash-bearing
  names can scaffold outside the intended current-directory project root. On a
  later write failure, rollback cleanup also removes paths derived from the same
  traversal-bearing name, making failure cleanup operate outside the advertised
  scaffold boundary.
- Next action: add a shared project-name validator before any `--init`
  filesystem operation, allow only a bounded non-empty safe-name set such as
  `[A-Za-z0-9_-]+`, reject slash/backslash/dot-segment/control characters, and
  add path-guard tests mirroring the existing bindgen unsafe-name cases.
- Resolution: `--init` now rejects empty/path-like project names, `.`, `..`,
  path separators, and control bytes before any scaffold filesystem writes.
  Project scaffold file writes also route through the shared atomic text-write
  helper.
- Validation: C3 LSP diagnostics were clean for touched entry files;
  `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  direct `--init ../bad` probe failed before writes with the expected invalid
  project-name error.

### AUDIT-097: REPL server project preload has a narrower project-root path contract than CLI REPL

- Severity: Low
- Status: Closed
- Classification: runtime behavior, targeted CLI/server parity fix
- Evidence:
  - The project tooling docs say `--repl-server --project [dir]` resolves the
    project root the same way as `omni --repl --project` at
    `docs/PROJECT_TOOLING.part-01.md:416` through
    `docs/PROJECT_TOOLING.part-01.md:418`.
  - The CLI REPL project resolver uses `REPL_PROJECT_PATH_BUFFER_CAP = 4096`
    and 4096-byte root/TOML path buffers at
    `src/entry_runtime_project_paths.c3:6` through
    `src/entry_runtime_project_paths.c3:8` and
    `src/entry_runtime_project_paths.c3:68` through
    `src/entry_runtime_project_paths.c3:84`.
  - The shared REPL server/session resolver duplicates that logic with
    `char[1024] root_buf` and `char[1280] toml_path_buf` at
    `src/main_repl_shared.c3:178` through `src/main_repl_shared.c3:194`.
  - Explicit server project paths are rejected when they reach the smaller root
    buffer at `src/main_repl_shared.c3:145` through
    `src/main_repl_shared.c3:155`, while the CLI helper copies the same role
    through the larger buffer at `src/entry_runtime_project_paths.c3:21`
    through `src/entry_runtime_project_paths.c3:47`.
- Impact: a project path that is valid for `omni --repl --project` can fail to
  preload in the REPL server, despite the documented parity contract. The
  duplicated path-resolution helpers also make future root-handling fixes
  likely to drift between CLI and server sessions.
- Next action: lift one shared project-root resolver and buffer-size policy for
  both CLI REPL and REPL server preload, then add parity tests for boundary-size
  project paths and the no-project-dir current-working-directory case.
- Resolution: REPL server project preload now delegates to the CLI REPL project
  resolver via `repl_resolve_project_main_path`, removing the server-only
  1024/1280-byte duplicate buffers and restoring the shared 4096-byte project
  root/path contract.
- Validation: C3 diagnostics were clean for `src/main_repl_shared.c3` and
  `src/entry_runtime_project_paths.c3`;
  `LIBRARY_PATH=/home/christos/.local/lib c3c build` passed; long-path direct
  probes succeeded for both `--repl --project <long-project>` and
  `--repl-server --stdio --project <long-project>`.

### AUDIT-098: `--compile` leaves generated C3 output behind when FFI manifest sidecar writing fails

- Severity: Medium
- Status: Closed
- Classification: CLI build artifact behavior, targeted atomic-output fix
- Evidence:
  - `run_compile` opens, writes, and closes the generated C3 output before
    attempting the FFI manifest sidecar at `src/entry_compile_runner.c3:47`
    through `src/entry_compile_runner.c3:75`.
  - If `write_compile_ffi_manifest` then fails, `run_compile` reports the
    manifest/output write error and returns failure without removing or rolling
    back the generated C3 output at `src/entry_compile_runner.c3:76` through
    `src/entry_compile_runner.c3:87`.
  - `write_compile_ffi_manifest` only emits a sidecar when the parsed source has
    FFI declarations at `src/entry_compile_manifest.c3:259` through
    `src/entry_compile_manifest.c3:269`, then builds
    `<output>.ffi-manifest.json` at `src/entry_compile_manifest.c3:271`
    through `src/entry_compile_manifest.c3:278`.
  - Existing tests exercise the manifest writer preserving an old final
    manifest on pre-rename failure at
    `src/lisp/tests_compiler_core_groups_serializer_metadata.c3:178` through
    `src/lisp/tests_compiler_core_groups_serializer_metadata.c3:214`, but they
    do not cover the higher-level `--compile` output/sidecar pair rollback.
  - Bindgen has an explicit first-time manifest-failure cleanup contract for
    generated artifacts at `docs/todo_parts/todo_part_04.md:309` through
    `docs/todo_parts/todo_part_04.md:313`; compile-side docs currently only
    guarantee atomic sidecar replacement at `docs/areas/ffi-foreign-runtime.md:154`
    through `docs/areas/ffi-foreign-runtime.md:157`.
- Impact: a failed FFI compile can leave a fresh or overwritten generated C3
  file without its required manifest sidecar. Downstream build or deployment
  tooling can mistake that output for a complete compile artifact even though
  `--compile` returned failure.
- Next action: make compile output and manifest sidecar publication atomic as a
  pair: track whether the C3 output existed before the run and remove only
  first-time outputs on sidecar failure, or write both through temp files and
  commit them together. Add a runner-level regression with forced manifest
  failure that asserts no new generated output remains.
- Resolution: `--compile` now stages generated C3 output and commits it only
  after FFI manifest sidecar success. Manifest writes use the shared staged
  atomic helper, so generated output and sidecar publication no longer leave a
  first-time generated C3 artifact behind on manifest failure.
- Validation: C3 LSP diagnostics were clean for touched entry/compiler files;
  `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed.

### AUDIT-099: `--fmt --write` truncates the source file before the replacement write is proven durable

- Severity: Medium
- Status: Closed
- Classification: CLI filesystem behavior, targeted atomic-write fix
- Evidence:
  - `run_fmt_mode` documents the write path by opening the original input path
    directly with mode `"w"` at `src/entry_fmt_mode.c3:34` through
    `src/entry_fmt_mode.c3:36`.
  - The formatter then streams the replacement text to the already-truncated
    file and only reports failure after a short write, write error, or close
    error at `src/entry_fmt_mode.c3:37` through `src/entry_fmt_mode.c3:53`.
  - There is no sibling temp file, rename, backup, or rollback in the formatter
    write path before returning success or failure at
    `src/entry_fmt_mode.c3:54` through `src/entry_fmt_mode.c3:56`.
  - The tooling docs expose `omni --fmt --write script.omni` as the in-place
    formatter mode at `docs/PROJECT_TOOLING.part-01.md:254` through
    `docs/PROJECT_TOOLING.part-01.md:268`, but do not warn that a failed write
    can leave the source truncated or partially rewritten.
- Impact: an interrupted disk write, full filesystem, permission race, or close
  failure can destroy or partially rewrite the only source copy while `--fmt`
  returns an error. This is especially risky because formatter write failures
  tend to happen in editor/save workflows where users expect the original file
  to survive a failed formatting attempt.
- Next action: route formatter writes through the same temp-file plus rename
  pattern used by hardened generated-output paths, preserve file contents on
  forced pre-rename/write/close failures, and add a regression that verifies the
  original input remains byte-identical when the formatter write fails.
- Resolution: `--fmt --write` now writes the formatted replacement to a staged
  sibling temp file and renames it into place only after the write is proven,
  avoiding direct truncation of the source path.
- Validation: C3 LSP diagnostics were clean for touched entry files;
  `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed.

### AUDIT-100: AOT temp C3 write failures leave partial `build/_aot_temp_*.c3` files behind

- Severity: Low
- Status: Closed
- Classification: CLI build artifact behavior, targeted temp-cleanup fix
- Evidence:
  - `write_aot_temp_file` creates the staged file under
    `build/_aot_temp_<pid>_<seq>.c3` with exclusive `"wx"` mode at
    `src/entry_build_aot_temp.c3:52` through
    `src/entry_build_aot_temp.c3:57`.
  - If writing or closing that file fails, the function prints an error and
    returns `false` without deleting the just-created temp path at
    `src/entry_build_aot_temp.c3:60` through
    `src/entry_build_aot_temp.c3:79`.
  - `run_build` only installs the deferred deletion after
    `write_aot_temp_file` returns success at `src/entry_build_mode.c3:120`
    through `src/entry_build_mode.c3:124`, so failed temp writes are outside
    the caller cleanup path.
  - The tooling docs describe the generated C3 as staged under
    `build/_aot_temp_*.c3` at `docs/PROJECT_TOOLING.part-01.md:635` through
    `docs/PROJECT_TOOLING.part-01.md:640`, but there is no documented
    partial-temp retention contract.
- Impact: a build that fails while writing generated C3 can leave a truncated
  temp source in `build/`. Later diagnostics, manual inspection, or cleanup
  scripts can mistake the stale temp file for a current generated artifact, and
  repeated failures accumulate build noise.
- Next action: add failure-path unlink cleanup inside `write_aot_temp_file`
  after a file has been created but before returning `false`, and add a forced
  write/close failure regression that asserts no new `_aot_temp_*.c3` file
  remains.
- Resolution: AOT temp C3 write failure paths now unlink the partially written
  temp file before returning failure, so failed generated-source writes do not
  leave stale `_aot_temp_*.c3` artifacts behind.
- Validation: C3 LSP diagnostics were clean for touched entry files;
  `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed.

### AUDIT-101: Shared CLI JSON escaping corrupts non-ASCII UTF-8 bytes on structured surfaces

- Severity: Low
- Status: Closed
- Classification: CLI/protocol behavior, targeted JSON escaping fix
- Evidence:
  - `print_json_escaped_bytes` escapes every byte `>= 0x7f` as `\u00XX` at
    `src/entry_check_reporting.c3:58` through
    `src/entry_check_reporting.c3:80`, and `--check --json` uses that helper
    for paths and diagnostic messages at `src/entry_check_reporting.c3:117`
    through `src/entry_check_reporting.c3:160`.
  - REPL-server startup/preflight JSON uses the same byte-oriented policy in
    `repl_json_print_escaped_string`, escaping every byte `>= 0x7f` as
    `\u00XX` at `src/main_repl_shared.c3:34` through
    `src/main_repl_shared.c3:58`.
  - The structured REPL JSON evaluator does not use that policy; it only
    escapes control bytes below `0x20` and otherwise emits UTF-8 bytes directly
    at `src/lisp/eval_repl_json.c3:25` through
    `src/lisp/eval_repl_json.c3:49`.
  - The REPL server protocol plan states that protocol messages are "UTF-8
    only" at `docs/plans/repl-server-protocol-2026-03-30.md:158` through
    `docs/plans/repl-server-protocol-2026-03-30.md:166`, and the project
    tooling docs expose JSON diagnostics/events at
    `docs/PROJECT_TOOLING.part-01.md:238` through
    `docs/PROJECT_TOOLING.part-01.md:252` and
    `docs/PROJECT_TOOLING.part-01.md:408` through
    `docs/PROJECT_TOOLING.part-01.md:414`.
- Impact: a UTF-8 path, CLI error, or startup/preflight message containing a
  multibyte character is converted byte-by-byte into Latin-1 code points. For
  example, a UTF-8 lambda byte sequence is emitted as `\u00ce\u00bb`, which JSON
  clients decode as mojibake rather than the original character. Different
  structured surfaces therefore disagree on whether they preserve UTF-8.
- Next action: centralize CLI/protocol JSON string escaping around one
  UTF-8-preserving helper: escape JSON control characters and quotes/backslashes,
  emit valid UTF-8 unchanged, and only use replacement/error policy for invalid
  byte sequences. Add regressions for `--check --json`, REPL-server startup
  errors, and legacy `--repl --json` with non-ASCII text.
- Resolution: shared CLI/protocol JSON escaping now preserves non-ASCII UTF-8
  bytes and only `\u00XX`-escapes control bytes, aligning the checked CLI
  reporting path and structured REPL JSON behavior.
- Validation: C3 LSP diagnostics were clean for touched entry/REPL JSON files;
  `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed; a
  direct `--eval --json` non-ASCII probe emitted raw UTF-8 bytes rather than
  `\u00XX` byte escapes.

### AUDIT-102: Failed implicit module imports leave unloaded modules that poison retries as circular imports

- Severity: Medium
- Status: Closed
- Classification: runtime module behavior, targeted rollback fix
- Evidence:
  - `jit_eval_implicit_module_file` publishes the module through
    `jit_create_file_module` before evaluating the file body at
    `src/lisp/jit_module_import_setup.c3:34` through
    `src/lisp/jit_module_import_setup.c3:40`.
  - The body loop returns the evaluation error directly and never calls
    `rollback_module_publication` when an expression fails at
    `src/lisp/jit_module_import_setup.c3:42` through
    `src/lisp/jit_module_import_setup.c3:47`.
  - The same function also returns directly on export-growth failure at
    `src/lisp/jit_module_import_setup.c3:48` through
    `src/lisp/jit_module_import_setup.c3:50`, again leaving the published
    module with `loaded == false`.
  - A later import by path checks `find_module_by_path_any` before loading the
    file and reports any unloaded matching module as a circular import at
    `src/lisp/jit_module_import.c3:20` through
    `src/lisp/jit_module_import.c3:24`.
  - A later import by name follows the same unloaded-module circular path at
    `src/lisp/jit_module_import.c3:33` through `src/lisp/jit_module_import.c3:37`.
  - Project status docs claim module load publication rolls back failed loads
    and preserves retry semantics at
    `docs/areas/memory-runtime_parts/memory-runtime_part_02.md:393` through
    `docs/areas/memory-runtime_parts/memory-runtime_part_02.md:395`.
- Impact: a transient runtime error or allocation failure during an implicit
  file module load can make all future imports of that module fail as
  `circular import detected` even after the source file or environment is fixed.
  This corrupts the interpreter's module cache until the process is restarted.
- Resolution: implicit file-module loading now records the published module
  index, restores `interp.global_env`, rolls back publication on evaluation or
  export-growth failure before `loaded = true`, and reacquires the module after
  nested evaluation boundaries.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=2131 fail=0`, including the
  implicit rollback/retry regression.

### AUDIT-103: Declared module files stay loaded when later file top-level expressions fail

- Severity: Medium
- Status: Closed
- Classification: runtime module behavior, targeted transaction fix
- Evidence:
  - `jit_eval_declared_module_file` first evaluates the module declaration with
    `jit_eval_to_result`, which publishes and marks the module loaded through
    the normal module evaluator, at `src/lisp/jit_module_import_setup.c3:21`
    through `src/lisp/jit_module_import_setup.c3:27` and
    `src/lisp/jit_module_setup_helpers.c3:122` through
    `src/lisp/jit_module_setup_helpers.c3:137`.
  - It then evaluates the remaining top-level expressions in the file at
    `src/lisp/jit_module_import_setup.c3:29`.
  - If those later top-level expressions fail, the function returns that error
    directly at `src/lisp/jit_module_import_setup.c3:30` without unloading the
    module or rolling back its path/export/env publication.
  - Subsequent imports can find the already loaded module and return success
    immediately at `src/lisp/jit_module_import_setup.c3:16` through
    `src/lisp/jit_module_import_setup.c3:19`, bypassing the top-level failure.
  - The documented shipped behavior says failed module body/path/top-level
    publication rolls back at
    `docs/plans/defect-audit-remediation-wave-2-2026-04-09_parts/defect-audit-remediation-wave-2-2026-04-09_part_03.md:98`
    through
    `docs/plans/defect-audit-remediation-wave-2-2026-04-09_parts/defect-audit-remediation-wave-2-2026-04-09_part_03.md:104`.
- Impact: a declared module file with a valid `(module ...)` form followed by a
  failing top-level expression can leave its exports visible even though the
  import failed. Later imports silently observe a partially successful load,
  which breaks the all-or-nothing module file contract and hides initialization
  errors.
- Resolution: declared module file loading is now transactional across the
  declaration and trailing top-level expressions; a trailing failure rolls back
  the module publication/path so a later retry starts clean.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=2131 fail=0`, including the
  declared trailing-failure rollback regression.

### AUDIT-104: Implicit module evaluation keeps a stale `Module*` across nested loads that can grow the module table

- Severity: High
- Status: Closed
- Classification: runtime module behavior, structural pointer-stability fix
- Evidence:
  - `jit_eval_implicit_module_file` stores the newly created module in a local
    `Module* mod` at `src/lisp/jit_module_import_setup.c3:34` through
    `src/lisp/jit_module_import_setup.c3:40`, then uses that pointer for every
    expression evaluation, export append, and final `loaded = true` write at
    `src/lisp/jit_module_import_setup.c3:42` through
    `src/lisp/jit_module_import_setup.c3:55`.
  - Evaluating an expression can execute imports and module forms through
    `jit_eval_to_result` at `src/lisp/jit_module_import_setup.c3:46`.
  - Nested module creation can grow the module table via
    `jit_create_file_module` and `jit_eval_module_impl` at
    `src/lisp/jit_module_setup_helpers.c3:86` through
    `src/lisp/jit_module_setup_helpers.c3:90` and
    `src/lisp/jit_module_setup_helpers.c3:289` through
    `src/lisp/jit_module_setup_helpers.c3:291`.
  - `Interp.ensure_module_capacity` allocates a new module array, copies the
    old entries, then frees the old `self.modules` array at
    `src/lisp/value_interp_lifecycle.c3:268` through
    `src/lisp/value_interp_lifecycle.c3:303`.
  - The status docs explicitly call out avoiding stale module pointers when
    nested loads grow the module table at `docs/todo_parts/todo_part_09.md:70`
    through `docs/todo_parts/todo_part_09.md:76`, but this implicit-module path
    still keeps and mutates the pre-growth pointer.
- Impact: importing an implicit module whose body imports enough additional
  modules to grow the table can use a freed `Module*` for subsequent
  `module_append_export` calls or the final `mod.loaded = true`. That can
  corrupt module state, fail to mark the real module loaded, or write into freed
  memory depending on allocator reuse.
- Resolution: module import/module evaluation now retains module indexes and
  reacquires module slots by index/name/path after nested evaluation boundaries
  before appending exports or setting `loaded`, avoiding stale `Module*` use
  after module-table growth.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=2131 fail=0`, including the
  nested module-table growth regression.

### AUDIT-105: TCP REPL tooling docs still describe the pre-auth, single-client contract

- Severity: Low
- Status: Closed
- Classification: static documentation regression, targeted docs repair
- Evidence:
  - The TCP listener now rejects non-loopback binds at
    `src/lisp/eval_repl_server_listeners.c3:161` through
    `src/lisp/eval_repl_server_listeners.c3:168`.
  - The same listener requires `OMNI_REPL_TCP_AUTH_TOKEN` before startup at
    `src/lisp/eval_repl_server_listeners.c3:170` through
    `src/lisp/eval_repl_server_listeners.c3:177`, then passes
    `auth_required = true` and the token to each client thread at
    `src/lisp/eval_repl_server_listeners.c3:210` through
    `src/lisp/eval_repl_server_listeners.c3:217`.
  - The auth policy test verifies token-gated secured transports and loopback
    host validation at `src/lisp/tests_runtime_async_repl_server_groups.c3:283`
    through `src/lisp/tests_runtime_async_repl_server_groups.c3:340`.
  - `docs/PROJECT_TOOLING.part-01.md:519` through
    `docs/PROJECT_TOOLING.part-01.md:526` still says TCP mode has no
    authentication/TLS layer and services one connected client at a time.
- Impact: client/tool authors reading the canonical tooling doc will implement
  the wrong startup and attach contract: they may omit the required token,
  treat auth failures as runtime regressions, or preserve unnecessary
  single-client assumptions after the listener moved to per-client threads.
- Next action: update the TCP REPL section to name the current loopback +
  `OMNI_REPL_TCP_AUTH_TOKEN` contract, distinguish missing TLS from missing
  auth, and describe the actual per-client-thread concurrency limits. Add a
  docs consistency check or targeted test note that prevents the old
  unauthenticated wording from returning.
- Resolution: `docs/PROJECT_TOOLING.part-01.md` now shows
  `OMNI_REPL_TCP_AUTH_TOKEN` in TCP REPL startup examples, documents loopback
  bind enforcement, distinguishes token authentication from missing TLS, and
  describes per-client-thread handling with interpreter/session ownership
  constraints instead of the obsolete single-client wording.
- Validation: inspected the current listener/test contracts and verified the
  tooling doc no longer contains the stale `no authentication` or
  `one connected client` TCP limitations.

### AUDIT-106: `process-kill` maps zero and negative signals to SIGKILL

- Severity: Medium
- Status: Closed
- Classification: runtime process behavior, targeted validation fix
- Evidence:
  - `SIGKILL_VAL` is hard-coded as `9` at `src/lisp/async_runtime_base.c3:178`.
  - `prim_process_kill` initializes `sig` to `SIGKILL_VAL` at
    `src/lisp/async_process_signal_dns_process.c3:144`.
  - It only overwrites that default when `sig_v.int_val > 0` at
    `src/lisp/async_process_signal_dns_process.c3:145` through
    `src/lisp/async_process_signal_dns_process.c3:150`; zero and every
    negative integer therefore fall through to `SIGKILL`.
  - The process-kill tests cover non-integer signals and a positive `15`
    termination path at
    `src/lisp/tests_advanced_io_effect_ffi_typed_effect_groups.c3:244` through
    `src/lisp/tests_advanced_io_effect_ffi_typed_effect_groups.c3:247` and
    `src/lisp/tests_advanced_io_effect_ffi_typed_effect_groups.c3:294` through
    `src/lisp/tests_advanced_io_effect_ffi_typed_effect_groups.c3:303`, but
    they do not cover `0` or negative values.
- Impact: a caller that passes `0` expecting POSIX signal-0 existence probing,
  or passes a negative value by mistake, kills the child with `SIGKILL` instead
  of failing closed. This is a destructive surprise on a process-control API.
- Resolution: `process-kill` now validates the requested signal before touching
  the process handle, rejects `<= 0`, values above the native platform maximum,
  and values outside C `int`, and returns `io/process-kill-invalid-sig` instead
  of defaulting to `SIGKILL`. The native helper exposes the platform maximum via
  `NSIG - 1` with a conservative fallback.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-effect-union-limit` passed with
  `pass=70 fail=0`; `git diff --check` passed for the touched process, IO, and
  audit files.

### AUDIT-107: `process-wait` closes the process handle before status result allocation is proven

- Severity: Medium
- Status: Closed
- Classification: runtime process lifecycle, targeted status-preservation fix
- Evidence:
  - `prim_process_wait` runs the offloaded wait and stores `pid`,
    `exit_code`, and `term_signal` in stack state at
    `src/lisp/async_process_signal_dns_process.c3:86` through
    `src/lisp/async_process_signal_dns_process.c3:101`.
  - It closes the process handle immediately afterward at
    `src/lisp/async_process_signal_dns_process.c3:103`.
  - Only after closing does it allocate and populate the user-visible wait
    result through `make_process_wait_result` at
    `src/lisp/async_process_signal_dns_process.c3:105`.
  - `make_process_wait_result` can still fail while allocating the hashmap,
    interning keys, or inserting result fields at
    `src/lisp/async_process_signal_dns_process.c3:9` through
    `src/lisp/async_process_signal_dns_process.c3:53`.
- Impact: under allocation pressure, Omni can successfully reap the child,
  close the only process handle, then return an OOM error before exposing the
  exit status. The caller cannot retry `process-wait` on the closed handle, so
  the child status is lost even though the OS wait completed.
- Resolution: `ProcessHandle` now stores a completed wait status before closing
  the libuv process handle, and `process-wait` returns the cached status on
  retry when the first materialization failed after OS wait completion.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-effect-union-limit OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `pass=86 fail=0`, including the
  allocation-fault retry regression.

### AUDIT-108: Async sleep surfaces accept invalid durations and can wrap large millisecond values

- Severity: Medium
- Status: Closed
- Classification: runtime scheduler behavior, targeted range-validation fix
- Evidence:
  - `async-sleep` accepts any integer argument and returns `Void` immediately
    for every `ms <= 0` at `src/lisp/async.c3:29` through
    `src/lisp/async.c3:30`.
  - Positive `async-sleep` values are cast directly to libuv's unsigned timer
    timeout at `src/lisp/async.c3:68`, with no documented maximum or overflow
    policy.
  - Offload/task/thread `sleep-ms` jobs copy the first integer argument into
    `work.int_arg` at `src/lisp/scheduler_state_offload_helpers.c3:138`
    through `src/lisp/scheduler_state_offload_helpers.c3:148`.
  - The worker executes `sleep-ms` only when `work.int_arg > 0` and casts
    `work.int_arg * 1000` to `uint` before calling `usleep` at
    `src/lisp/scheduler_offload_ops.c3:212` through
    `src/lisp/scheduler_offload_ops.c3:215`.
  - Existing async tests cover positive sleeps and non-fiber context errors at
    `src/lisp/tests_runtime_async_io_tls_groups.c3:51` through
    `src/lisp/tests_runtime_async_io_tls_groups.c3:64`, but no zero, negative,
    or very large duration cases.
- Impact: negative scheduler sleeps look successful instead of failing closed,
  and large `sleep-ms` values can wrap after millisecond-to-microsecond
  multiplication/cast, producing a much shorter sleep than requested. This
  weakens timeout/cancellation tests and any user code that treats these
  operations as reliable delay primitives.
- Resolution: scheduler sleep durations now share one positive bounded
  millisecond contract. `async-sleep` and `sleep-ms` offload/task/thread paths
  reject zero, negative, and values above `uint.max / 1000`; offload execution
  converts milliseconds to microseconds through a checked helper before
  `usleep`. Existing zero-duration scheduler fixtures were updated to use a
  positive sleep value, while recycle-reset assertions still verify cleared
  work slots reset to zero.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=async` passed with `pass=76 fail=0`;
  `OMNI_LISP_TEST_SLICE=scheduler` passed with `pass=122 fail=0`; `git diff
  --check` passed for touched async/scheduler files.

### AUDIT-109: Offload job builders ignore extra arguments for fixed-shape jobs

- Severity: Low
- Status: Closed
- Classification: runtime scheduler behavior, targeted arity validation
- Evidence:
  - The public offload docs show fixed-shape examples for gzip, deflate, and
    sleep-ms at `docs/reference/09-concurrency-ffi.md:64` through
    `docs/reference/09-concurrency-ffi.md:73`.
  - `scheduler_build_offload_work` parses the operation symbol and argument
    list at `src/lisp/scheduler_state_offload_helpers.c3:116` through
    `src/lisp/scheduler_state_offload_helpers.c3:125`.
  - The `sleep-ms` branch validates only that the first argument exists and is
    an integer, then returns success without checking `arg_list.cons_val.cdr`
    at `src/lisp/scheduler_state_offload_helpers.c3:138` through
    `src/lisp/scheduler_state_offload_helpers.c3:148`.
  - The `gzip` / `deflate` branch similarly validates only the first string
    payload and returns success without checking for trailing arguments at
    `src/lisp/scheduler_state_offload_helpers.c3:151` through
    `src/lisp/scheduler_state_offload_helpers.c3:163`.
  - Internal `http-get` follows the same first-argument-only pattern at
    `src/lisp/scheduler_state_offload_helpers.c3:216` through
    `src/lisp/scheduler_state_offload_helpers.c3:232`.
- Impact: malformed jobs such as `(offload 'sleep-ms 5 'ignored)` or
  `(task-spawn 'gzip "payload" 'ignored)` silently execute as if the extra
  arguments were absent. That makes scheduler jobs inconsistent with strict
  primitive arity expectations and can hide caller bugs in background work.
- Resolution: `scheduler_build_offload_work` now enforces exact proper-list
  argument counts for fixed-shape `sleep-ms`, `gzip`/`deflate`, and internal
  fixed-shape offload jobs before constructing work payloads. Regressions cover
  trailing-argument rejection across `offload`, `task-spawn`, `thread-spawn`,
  and raw batch entry points while preserving the bounded positive `sleep-ms`
  behavior from `AUDIT-108`.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=scheduler` passed with
  `pass=134 fail=0`; `scripts/check_scheduler_state_guards.sh` passed; `git
  diff --check` passed for the touched scheduler files.

### AUDIT-110: Task and thread joins consume completions before result materialization is proven

- Severity: Medium
- Status: Closed
- Classification: runtime scheduler lifecycle, targeted completion-preservation fix
- Evidence:
  - Task joins take and remove the completion from the scheduler registry via
    `scheduler_take_thread_task_completion`, which clears the task slot at
    `src/lisp/scheduler_thread_task_registry.c3:83` through
    `src/lisp/scheduler_thread_task_registry.c3:108`.
  - Both task-join paths then call `scheduler_value_from_offload_completion`,
    immediately free the completion, clear the handle id/generation, and return
    the materialized value at `src/lisp/scheduler_primitives_task_wait_join.c3:39`
    through `src/lisp/scheduler_primitives_task_wait_join.c3:49` and
    `src/lisp/scheduler_primitives_task_wait_join.c3:165` through
    `src/lisp/scheduler_primitives_task_wait_join.c3:174`.
  - OS thread joins follow the same pattern after
    `scheduler_take_os_thread_completion`, which clears the OS-thread slot and
    joins/frees the native thread handle at `src/lisp/scheduler_thread_tasks.c3:130`
    through `src/lisp/scheduler_thread_tasks.c3:168`.
  - The thread join materialization/free/handle-clear sequence appears at
    `src/lisp/scheduler_primitives_thread_helpers.c3:15` through
    `src/lisp/scheduler_primitives_thread_helpers.c3:23`,
    `src/lisp/scheduler_primitives_thread_helpers.c3:84` through
    `src/lisp/scheduler_primitives_thread_helpers.c3:92`, and
    `src/lisp/scheduler_primitives_thread_helpers.c3:155` through
    `src/lisp/scheduler_primitives_thread_helpers.c3:164`.
  - Result materialization can allocate or raise while converting completion
    payloads through `scheduler_value_from_offload_completion` at
    `src/lisp/scheduler_wakeup_io.c3:71` through
    `src/lisp/scheduler_wakeup_io.c3:95`.
- Impact: under allocation pressure, a completed task/thread can be removed
  from the scheduler, have its completion freed and handle invalidated, then
  return an OOM/materialization error instead of the actual result. The caller
  cannot retry the join because the task/thread slot and handle generation are
  already gone.
- Next action: preserve completed scheduler results until value materialization
  succeeds, or move completion payloads into a retryable per-handle completed
  state before clearing the registry. Add forced-allocation-failure regressions
  for task and thread joins with byte and error completions.

### AUDIT-111: Stack clone relocation delta can underflow before becoming signed

- Severity: High
- Status: Closed
- Classification: runtime continuation correctness, targeted stack-clone fix
- Evidence:
  - `stack_ctx_clone` computes source and destination copy addresses as `usz`
    values at `src/stack_engine_clone.c3:131` through
    `src/stack_engine_clone.c3:140`.
  - The relocation delta is computed as `long delta = (long)(copy_dst -
    copy_src)` at `src/stack_engine_clone.c3:152`, so the subtraction happens
    in unsigned address space before the signed cast.
  - That delta is then used to relocate the saved stack pointer and register
    fields at `src/stack_engine_clone.c3:153` through
    `src/stack_engine_clone.c3:154`, the frame-pointer chain at
    `src/stack_engine_clone.c3:168` through `src/stack_engine_clone.c3:170`,
    and spilled stack words at `src/stack_engine_clone.c3:177` through
    `src/stack_engine_clone.c3:181`.
- Impact: when the cloned stack mapping is below the source mapping,
  `copy_dst - copy_src` wraps before it is cast to `long`. Multi-shot
  continuation clones can then relocate saved stack pointers to invalid
  addresses, corrupt the cloned continuation, or resume with pointers into the
  wrong stack.
- Resolution: stack clone relocation now uses a signed checked address-delta
  helper that handles both address orders without unsigned wrap and validates
  representability before relocating saved stack state.
- Validation: `OMNI_TEST_SUMMARY=1 ./build/main --test-suite stack` passed with
  `pass=25 fail=0`.

### AUDIT-112: Stack clone detects residual source-stack pointers but only exposes them as optional trace output

- Severity: High
- Status: Closed
- Classification: runtime continuation hardening, targeted clone validation
- Evidence:
  - After relocating context fields and active stack words,
    `stack_ctx_clone` counts remaining words that still look like pointers into
    the source stack at `src/stack_engine_clone.c3:181` through
    `src/stack_engine_clone.c3:182`.
  - The resulting `remaining_source_words` value is used only inside the
    `OMNI_STACK_CLONE_TRACE` diagnostic block at
    `src/stack_engine_clone.c3:184` through `src/stack_engine_clone.c3:200`;
    the clone still returns successfully at `src/stack_engine_clone.c3:217`.
  - The current multi-shot clone regression only verifies that source and clone
    both resume to completion at `src/stack_engine_tests_coroutines_edges.c3:84`
    through `src/stack_engine_tests_coroutines_edges.c3:95`; it does not assert
    that residual source-stack pointers are zero or classify tolerated false
    positives.
- Impact: a cloned continuation can carry raw pointers back into the source
  stack without failing the clone or surfacing the hazard in normal validation.
  Destroying or reusing the source stack would then make the clone depend on
  stale stack memory, violating the documented multi-shot continuation
  isolation contract.
- Resolution: residual source-stack word detection now classifies raw source
  pointers separately from low-56 false-positive candidates and fails clone
  creation closed when raw residual source-stack pointers remain after
  relocation. `OMNI_STACK_CLONE_TRACE` now reports both classified counts as
  supplemental diagnostics.
- Validation: `OMNI_TEST_SUMMARY=1 ./build/main --test-suite stack` passed with
  `suite=stack_engine pass=26 fail=0`, including the residual source-pointer
  policy regression.

### AUDIT-113: AOT primitive resolver parity misses whole runtime primitive families

- Severity: High
- Status: Closed
- Classification: static compiler/runtime parity, structural resolver generation
- Evidence:
  - Runtime primitive registration includes public and internal primitives such
    as `foreign-release`, `ffi-callback`, `runtime-memory-stats`, `deduce/query`,
    and `__raw-http-get` at `src/lisp/eval_init_primitive_tables.c3:212`
    through `src/lisp/eval_init_primitive_tables.c3:215`,
    `src/lisp/eval_init_primitive_tables.c3:231`,
    `src/lisp/eval_init_primitive_tables.c3:332`, and
    `src/lisp/eval_init_primitive_tables.c3:356`.
  - The AOT primitive resolver is a separate manual table in
    `src/lisp/compiler_primitive_variable_hash_table_domains.c3` and
    `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`;
    unresolved symbols fall back to ordinary emitted variable names at
    `src/lisp/compiler_output_symbol_helpers.c3:68` through
    `src/lisp/compiler_output_symbol_helpers.c3:89`.
  - A direct source-table comparison during this audit found 353 runtime
    primitive names but only 321 AOT resolver entries; examples missing from
    the AOT resolver include `foreign-release`, `ffi-callback`,
    `runtime-memory-stats`, the `deduce/*` namespace, many `__raw-*` async/I/O
    primitives, and iterator helpers such as `__iterator-range-from`.
  - The existing narrow data-reader finding records missing `hex` and `uuid`;
    this broader comparison shows the same manual parity gap affects complete
    primitive families rather than only those two names.
- Impact: compiled code that references one of the missing runtime primitives
  is not recognized as a builtin, so the compiler can emit a plain generated C3
  variable name instead of a cached `aot::lookup_prim(...)` value. That can
  produce compile-time undefined symbols or runtime behavior that differs from
  interpreter/JIT execution for Deduce, FFI, async/I/O, iterator, and memory
  diagnostics surfaces.
- Resolution: the AOT primitive resolver tables now cover the runtime primitive
  registration surface; symbol emission falls back to `aot::lookup_prim(...)`
  for runtime primitives instead of emitting unresolved C3 variable names.
- Validation: a source-table parity check comparing runtime primitive
  registrations against `compiler_primitive_variable_hash_table*.c3` reported
  no missing primitive names; a direct `--compile` probe for
  `(runtime-memory-stats)` succeeded and generated
  `aot::lookup_prim("runtime-memory-stats")`. The broader compiler slice still
  aborts in the unrelated `aot_init` global scope-mutex shutdown path.

### AUDIT-114: `base64` decoded output length can wrap before allocation

- Severity: Medium
- Status: Closed
- Classification: runtime data-format bounds, targeted size-arithmetic fix
- Evidence:
  - `prim_base64` counts non-whitespace input bytes into `clean_count` at
    `src/lisp/primitives_data_formats.c3:95` through
    `src/lisp/primitives_data_formats.c3:116` and only checks that the cleaned
    length is divisible by four at `src/lisp/primitives_data_formats.c3:117`
    through `src/lisp/primitives_data_formats.c3:119`.
  - It computes decoded byte length as `(clean_count / 4) * 3 - padding_count`
    at `src/lisp/primitives_data_formats.c3:121` with no `usz` overflow guard,
    then allocates that length at `src/lisp/primitives_data_formats.c3:122`.
  - The decode loop writes one to three output values per quartet via
    `arr.array_val.items[out_i++]` at `src/lisp/primitives_data_formats.c3:141`
    through `src/lisp/primitives_data_formats.c3:159`; those writes trust the
    precomputed `byte_count`.
- Impact: the original wrap claim is over-broad because a real cleaned Base64
  length is bounded by a `usz` string length, so `(clean_count / 4) * 3` cannot
  exceed `usz.max`. The unchecked expression still encoded the size invariant
  implicitly and left the subtraction dependent on earlier padding validation.
- Resolution: decoded-length calculation now goes through
  `data_base64_decoded_len_checked`, which rejects non-quartet lengths,
  impossible padding, multiplication overflow, and subtraction underflow before
  allocation. The primitive uses `make_array_checked` with a `base64:` failure
  path for impossible decoded sizes.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=data-format` passed with
  `pass=67 fail=0`; `git diff --check` passed for the touched data-format files.

### AUDIT-115: `ml/export-image` writes directly to the final image path

- Severity: Medium
- Status: Closed
- Classification: runtime filesystem behavior, targeted atomic-output fix
- Evidence:
  - `ml_image_export_ppm` opens the caller-supplied final path directly with
    mode `"w"` at `src/lisp/prim_ml_visualization.c3:708` through
    `src/lisp/prim_ml_visualization.c3:710`.
  - The function streams the PPM header and one line per pixel into that already
    opened final file at `src/lisp/prim_ml_visualization.c3:713` through
    `src/lisp/prim_ml_visualization.c3:755`, then reports write or close
    failures after partial output may already be visible at
    `src/lisp/prim_ml_visualization.c3:756` through
    `src/lisp/prim_ml_visualization.c3:763`.
  - `ml_image_prepare` only rejects an empty path and unsupported options at
    `src/lisp/prim_ml_visualization.c3:600` through
    `src/lisp/prim_ml_visualization.c3:607`; it does not route the export
    through the hardened sibling-temp plus rename writer used by other generated
    file paths.
  - Existing `ml/export-image` tests assert successful metadata and some
    fail-closed option/type cases at
    `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:120`
    through `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:125`,
    but they do not force write/close failure or verify preservation of an
    existing image.
- Impact: an interrupted write, full filesystem, close failure, or later pixel
  validation failure can leave a truncated or partially rewritten final image at
  the user-provided path while the primitive returns an error. This regresses the
  newer generated-output durability contract already used by hardened tooling
  writers.
- Resolution: PPM export now writes to a sibling `final.tmp.<pid>` path, closes
  that file, then publishes with `omni_uv_fs_rename`. The temp path is removed
  before writing and again on any write/close/rename failure, so failed
  publication no longer streams partial data directly into the caller's final
  image path. A regression forces a rename failure by exporting to a directory
  path and asserts the temp file is cleaned up.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module` passed with
  `pass=2114 fail=0`; `git diff --check` passed for the touched ML
  visualization files.

### AUDIT-116: Atomic-ref primitives accept any `FFI_HANDLE` as an `AtomicRef`

- Severity: High
- Status: Closed
- Classification: runtime type safety, targeted foreign-handle family validation
- Evidence:
  - `get_atomic_ref` only checks `v.tag == FFI_HANDLE`, `v.ffi_val != null`, and
    then casts `v.ffi_val.lib_handle` to `AtomicRef*` at
    `src/lisp/threads.c3:46` through `src/lisp/threads.c3:50`.
  - `atomic-add!`, `atomic-read`, and `atomic-cas!` all trust that cast before
    loading or mutating the pointee at `src/lisp/threads.c3:55` through
    `src/lisp/threads.c3:65`, `src/lisp/threads.c3:71` through
    `src/lisp/threads.c3:78`, and `src/lisp/threads.c3:84` through
    `src/lisp/threads.c3:100`.
  - `prim_atomic` creates handles named `"atomic-ref"` at
    `src/lisp/threads.c3:34` through `src/lisp/threads.c3:40`, and
    `make_ffi_box` stores that family name in `FfiHandle.lib_name`/`name_len` at
    `src/lisp/value_constructors.c3:139` through
    `src/lisp/value_constructors.c3:148`.
  - Other handle families already guard by name before casting, for example
    process/signal handles at `src/lisp/async_process_signal_runtime.c3:142`
    through `src/lisp/async_process_signal_runtime.c3:175`.
- Impact: passing any non-atomic foreign handle to atomic primitives can cast an
  unrelated payload as `AtomicRef*`, creating memory corruption risk instead of a
  type error.
- Next action: validate the FFI handle family name is exactly `"atomic-ref"`
  before casting and add regressions for non-atomic handles passed to
  `atomic-read`, `atomic-add!`, and `atomic-cas!`.
- Resolution: `get_atomic_ref` now validates the FFI handle family name before
  casting, and atomic primitives reject mismatched FFI handles with the existing
  atomic-ref type diagnostics. Regressions cover read/add/CAS.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=atomic` passed with `pass=10
  fail=0`; `git diff --check` passed.

### AUDIT-117: FTXUI render-wrapper callbacks leak returned element wrappers

- Severity: Medium
- Status: Closed
- Classification: native UI backend memory lifecycle, targeted callback cleanup fix
- Evidence:
  - `omni_ftxui_component_wrap_renderer` builds a renderer callback wrapper at
    `csrc/ftxui_shim_component.inc:317` through
    `csrc/ftxui_shim_component.inc:370`.
  - Each render invokes the user callback into a fresh
    `omni_ftxui_element_t* out` at `csrc/ftxui_shim_component.inc:347`
    through `csrc/ftxui_shim_component.inc:350`.
  - On success, the lambda returns `out->element` directly at
    `csrc/ftxui_shim_component.inc:361` through
    `csrc/ftxui_shim_component.inc:362`, but never calls
    `omni_ftxui_element_destroy(out)` or otherwise deletes the wrapper.
  - Other native helper paths make ownership explicit, for example
    `omni_ftxui_element_destroy` is the public destructor at
    `csrc/ftxui_shim_element.inc:223` through
    `csrc/ftxui_shim_element.inc:225`, and the C3 lowering uses `defer` to
    destroy temporary elements after wrapping at
    `src/lisp/prim_ui_ftxui_plot_lowering.c3:228` through
    `src/lisp/prim_ui_ftxui_plot_lowering.c3:235`.
- Impact: any render callback that allocates a new element wrapper leaks that
  wrapper on every render frame. Long-running sessions or animated views can
  grow native heap usage even when the underlying returned `ftxui::Element` is
  copied successfully.
- Next action: copy/move the returned element into a local `ftxui::Element`,
  destroy the `omni_ftxui_element_t` wrapper before returning, and add a direct
  shim regression with a render callback that counts create/destroy calls across
  repeated renders.
- Resolution: the renderer wrapper now takes ownership of the callback-returned
  `omni_ftxui_element_t`, copies the underlying `ftxui::Element`, and destroys
  the temporary wrapper before returning from the render callback.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system`
  passed with `pass=171 fail=0`; the focused FTXUI session smoke passed;
  `git diff --check` passed.

### AUDIT-118: High-level FTXUI buttons carry messages that are discarded during lowering

- Severity: Medium
- Status: Closed
- Classification: UI behavior regression, targeted event/action lowering fix
- Evidence:
  - The public UI reference shows `(ui.nodes.button "increment" 'increment)` in
    the counter example at `docs/UI_REFERENCE.md:82` through
    `docs/UI_REFERENCE.md:92`.
  - The shipped high-level helper stores the second argument as a `'message`
    property at `examples/libraries/ftxui/lib/ui/nodes.omni:34` through
    `examples/libraries/ftxui/lib/ui/nodes.omni:35`, and the smoke example uses
    `(ui.button "increment" 'increment)` at
    `examples/libraries/ftxui/smoke.omni:7` through
    `examples/libraries/ftxui/smoke.omni:14`.
  - The C3 lowering for kind `"button"` reads only `'label`, builds
    `OMNI_FTXUI_COMPONENT_BUTTON`, and passes no callback, message, or event
    wrapper at `src/lisp/prim_ui_ftxui_lowering.c3:45` through
    `src/lisp/prim_ui_ftxui_lowering.c3:58`.
  - The native button constructor installs a no-op action (`[] {}`) at
    `csrc/ftxui_shim_component.inc:40` through
    `csrc/ftxui_shim_component.inc:42`; the action wrapper that can post work
    is a separate API at `csrc/ftxui_shim_component.inc:433` through
    `csrc/ftxui_shim_component.inc:490` and is not used by the high-level button
    lowering.
  - Existing smoke/docs coverage proves the button node can be constructed, but
    there is no high-level test that activating the button produces the stored
    message or any session event.
- Impact: high-level UI code appears to define actionable buttons, but pressing
  the button cannot emit the documented/stored message. Counter-style examples
  and stateful sessions can render correctly while silently ignoring the intended
  action contract.
- Next action: define the button event contract (`'message` as a posted custom
  event, action callback payload, or explicit unsupported field), wire the
  lowering through `omni_ftxui_component_wrap_action` or an equivalent message
  dispatcher, and add a session test that activates a button and observes the
  expected event/message.
- Resolution: high-level button lowering now wraps buttons that carry
  `'message` with a native message action. Activating the button posts a special
  event carrying the message text, and the UI reference/session smoke were
  updated for the event contract.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system`
  passed with `pass=171 fail=0`; the focused FTXUI session smoke passed;
  `git diff --check` passed.

### AUDIT-119: FTXUI session event capture overwrites unread events

- Severity: Medium
- Status: Closed
- Classification: UI runtime event delivery, targeted queue/backpressure fix
- Evidence:
  - `ui.read_event_session` is documented as returning the captured session event
    dictionary, with `kind` and optional `text`, at `docs/UI_REFERENCE.md:247`
    through `docs/UI_REFERENCE.md:252`.
  - `omni_ftxui_session_create` wraps the root component with `CatchEvent` and
    writes every incoming event into the same `session->event` slot while setting
    `session->has_event = true` at `csrc/ftxui_shim_runtime.inc:395` through
    `csrc/ftxui_shim_runtime.inc:398`.
  - `omni_ftxui_session_take_event` copies only that one slot into the result and
    then clears it at `csrc/ftxui_shim_runtime.inc:480` through
    `csrc/ftxui_shim_runtime.inc:489`.
  - There is no queue, sequence number, overflow diagnostic, or backpressure path
    between event capture and the C3 reader.
- Impact: if multiple key, mouse, or custom events arrive before user code calls
  `ui.read_event_session`, all but the latest event are silently lost. Stateful
  UIs can miss button activations, text input, or navigation sequences while
  still reporting a valid event for the last input.
- Next action: replace the single pending event slot with a bounded queue or
  explicit coalescing policy, expose overflow/drop diagnostics, and add a
  session regression that posts two distinct events before one read and verifies
  both can be observed or that overflow is explicit.
- Resolution: session capture now stores captured events in FIFO order instead
  of overwriting one pending slot. The read-result ABI also reports dropped event
  counts so overflow/backpressure is explicit instead of silent.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system`
  passed with `pass=171 fail=0`; the focused FTXUI session smoke passed;
  `git diff --check` passed.

### AUDIT-120: FTXUI session event text is silently truncated to 255 bytes

- Severity: Medium
- Status: Closed
- Classification: UI runtime payload fidelity, targeted event-text sizing fix
- Evidence:
  - The native event-read ABI stores text in a fixed `char text[256]` buffer at
    `csrc/ftxui_shim.h:137` through `csrc/ftxui_shim.h:143`.
  - `omni_ftxui_session_take_event` copies
    `min(session->event.text.size(), sizeof(out_event->text) - 1)` bytes and
    NUL-terminates at `csrc/ftxui_shim_runtime.inc:483` through
    `csrc/ftxui_shim_runtime.inc:486`, with no truncated flag or error.
  - The C3 adapter scans until NUL and returns that prefix as the event text at
    `src/lisp/prim_ui_ftxui.c3:509` through
    `src/lisp/prim_ui_ftxui.c3:512`.
  - The UI reference documents character events as
    `{'kind 'character 'text "..."}` and special/custom text payloads without a
    255-byte cap at `docs/UI_REFERENCE.md:247` through
    `docs/UI_REFERENCE.md:252`.
- Impact: pasted text, terminal escape payloads, or future custom/button messages
  longer than 255 bytes are shortened silently, and application code cannot tell
  a complete event from a truncated prefix.
- Next action: add text length to the read-result ABI or return heap-owned text
  through an explicit destructor, fail closed or set a truncation marker when the
  fixed buffer is exceeded, and add a regression for a long posted
  character/special event.
- Resolution: the event-read ABI now returns heap-owned `text` plus `text_len`
  and an explicit clear function, replacing the fixed 255-byte buffer. C3 session
  event conversion consumes the explicit length and defers native result cleanup.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system`
  passed with `pass=171 fail=0`; the focused FTXUI session smoke passed;
  `git diff --check` passed.

### AUDIT-121: `re-replace` documentation, tests, and runtime disagree on the fourth argument contract

- Severity: Medium
- Status: Closed
- Classification: regex API contract drift, targeted primitive validation and
  documentation fix
- Evidence:
  - The language spec says `re-replace` has `3-4` arguments and replaces all
    matches with `'global` at `docs/LANGUAGE_SPEC.part-03.md:591` through
    `docs/LANGUAGE_SPEC.part-03.md:597`.
  - The library reference instead documents examples in `(pattern input
    replacement)` order and a numeric limit form at
    `docs/reference/08-libraries.part-01.md:30` through
    `docs/reference/08-libraries.part-01.md:36`.
  - The implementation comment and argument binding use `(pattern replacement
    input ['global])` at `src/pika/lisp_pika_regex_replace.c3:117` through
    `src/pika/lisp_pika_regex_replace.c3:131`.
  - `re_replace_has_global_flag` treats any fourth argument with `SYMBOL` tag as
    global mode, regardless of its symbol name, at
    `src/pika/lisp_pika_regex_replace.c3:6` through
    `src/pika/lisp_pika_regex_replace.c3:8`.
  - The primitive is registered variadically at `src/pika/lisp_pika.c3:11`, and
    the runtime only rejects `args.len < 3` at
    `src/pika/lisp_pika_regex_replace.c3:119` through
    `src/pika/lisp_pika_regex_replace.c3:120`, so calls with five or more
    arguments are accepted and trailing arguments are ignored.
- Impact: user code following the reference examples gets different behavior
  from the documented result, numeric limit calls silently run as first-replace
  calls, and arbitrary fourth symbols such as `'all` or `'typo` enable global
  replacement. Extra arguments can hide malformed calls instead of failing
  closed.
- Next action: choose one canonical surface, update the reference and spec to
  match it, make the runtime reject `args.len > 4`, validate that the optional
  fourth argument is exactly `'global` if that remains the contract, and add
  regressions for wrong argument order, numeric limit, unknown symbol, and
  extra-argument calls.
- Resolution: `re-replace` now accepts only three or four arguments, and the
  optional fourth argument must be exactly `'global`. The Pika library reference
  was updated to the canonical contract, with regressions for invalid fourth
  arguments and trailing arguments.
- Validation: C3 LSP diagnostics were clean for touched Pika files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=123 fail=0`.

### AUDIT-122: Regex cache mutex serializes complete match execution

- Severity: Low
- Status: Closed
- Classification: regex runtime performance antipattern, structural cache-lock
  scope fix
- Evidence:
  - Regex cache state is global and protected by `g_regex_cache_mu` at
    `src/pika/regex_cache_state.c3:15` through
    `src/pika/regex_cache_state.c3:28`.
  - `regex_search_compiled` acquires that mutex before cache lookup and keeps it
    held through `parse_simple`, parser-state allocation, and the full match loop
    at `src/pika/regex_cache_compile.c3:159` through
    `src/pika/regex_cache_compile.c3:183`.
  - `regex_fullmatch_compiled` has the same lock scope around full parse and
    lookup at `src/pika/regex_cache_compile.c3:186` through
    `src/pika/regex_cache_compile.c3:205`.
  - `regex_find_all_compiled` also keeps the global cache lock while parsing the
    whole input and materializing every match at
    `src/pika/regex_cache_compile.c3:208` through
    `src/pika/regex_cache_compile.c3:254`.
  - Public regex primitives are exposed as ordinary library/runtime primitives
    at `docs/reference/11-appendix-primitives.md:554` through
    `docs/reference/11-appendix-primitives.md:560`, with no documented global
    serialization behavior.
- Impact: unrelated regex calls in different tasks or OS threads cannot run in
  parallel once they touch the cache. A long `re-find-all` over a large input can
  block simple `re-match` calls for unrelated patterns, turning the cache mutex
  into a runtime-wide regex execution bottleneck.
- Next action: split cache ownership from compiled-regex execution by retaining
  or cloning compiled regex objects outside the cache lock, or use a read/write
  cache policy that only holds the mutex for lookup/insert/eviction. Add a
  concurrency/performance regression that proves two independent regex matches
  can make progress without waiting for one full parse loop.
- Deferred design note: this was not changed in the Pika grammar remediation
  wave. The cache currently returns raw `CompiledRegex*`; shrinking the lock
  safely needs a retain/clone/read-lock ownership design before execution can
  run outside the mutex. Do not treat local lock narrowing as safe without that
  ownership work.
- Resolution: compiled regex entries now have explicit active-use ownership.
  Cache lookup/insert/evict/reset still happen under the mutex, but successful
  public search/fullmatch/find-all paths retain the compiled regex under lock,
  release the cache mutex before parser-state allocation and match traversal,
  then release the retained regex afterward. Eviction/reset detach cached
  entries and defer freeing until active users release them.
- Validation: C3 LSP diagnostics were clean for `src/pika/regex_types.c3`,
  `src/pika/regex_cache_state.c3`, `src/pika/regex_cache_compile.c3`, and
  `src/lisp/tests_runtime_feature_pika_groups.c3`;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=124 fail=0`; `git diff --check` passed for the touched Pika files.

### AUDIT-123: Public Pika parse primitives leak parser-state allocations

- Severity: Medium
- Status: Closed
- Classification: parser runtime lifetime bug, targeted parser-state teardown fix
- Evidence:
  - `parse` allocates `st.q`, `st.matches`, and `st.submatches` at
    `src/pika/parse.c3:63` through `src/pika/parse.c3:73`, only frees the
    temporary `terminal_q`, and returns the owning `ParserState` at
    `src/pika/parse.c3:98` through `src/pika/parse.c3:103`.
  - The regex path has an explicit teardown helper that frees the queue, matches,
    and submatches at `src/pika/regex_cache_compile.c3:136` through
    `src/pika/regex_cache_compile.c3:141`, and the regex search/fullmatch/find-all
    paths defer that release after `parse_simple` at
    `src/pika/regex_cache_compile.c3:170` through
    `src/pika/regex_cache_compile.c3:171`,
    `src/pika/regex_cache_compile.c3:195` through
    `src/pika/regex_cache_compile.c3:196`, and
    `src/pika/regex_cache_compile.c3:224` through
    `src/pika/regex_cache_compile.c3:225`.
  - `prim_pika_parse` calls `parse_simple` and returns either `nil` or the Lisp
    parse tree without releasing `st` at `src/pika/lisp_pika_parse_fold.c3:26`
    through `src/pika/lisp_pika_parse_fold.c3:30`.
  - `prim_pika_fold` repeats the same pattern at
    `src/pika/lisp_pika_parse_fold.c3:86` through
    `src/pika/lisp_pika_parse_fold.c3:91`.
  - `pika/match-span` calls `parse_simple` at
    `src/pika/lisp_pika_reader_primitives.c3:170` through
    `src/pika/lisp_pika_reader_primitives.c3:174`, then returns `nil` or the
    span at `src/pika/lisp_pika_reader_primitives.c3:176` through
    `src/pika/lisp_pika_reader_primitives.c3:184` without releasing the parser
    state.
  - `pika/parse-lisp` and `pika/eval-lisp` also receive a `ParserState` from
    `pika_parse_lisp_source` and return at
    `src/pika/lisp_pika_reader_primitives.c3:54` through
    `src/pika/lisp_pika_reader_primitives.c3:67` and
    `src/pika/lisp_pika_reader_primitives.c3:199` through
    `src/pika/lisp_pika_reader_primitives.c3:219` without a corresponding
    teardown.
- Impact: every successful or failed public Pika parse/fold/match-span/eval
  call can retain the parser queue, match table, and submatch table until process
  exit. Long-running parser users, editor tooling, or services that repeatedly
  parse user input can grow heap usage independently of Omni region lifetimes.
- Next action: move parser-state release into a shared Pika parser-state
  destructor, `defer` it in every public parse entry point after `parse_simple`
  or `pika_parse_lisp_source`, and add ASAN/Valgrind regressions that loop
  `pika/parse`, `pika/fold`, `pika/match-span`, `pika/parse-lisp`, and
  `pika/eval-lisp`.
- Resolution: public Pika parse/fold/match-span/parse-lisp/eval-lisp paths now
  release `ParserState` allocations after consuming parse results.
- Validation: C3 LSP diagnostics were clean for touched Pika files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=123 fail=0`.

### AUDIT-124: TIE user-view flattening leaks nested submatch arrays

- Severity: Medium
- Status: Closed
- Classification: parser runtime lifetime bug, targeted user-view ownership fix
- Evidence:
  - `user_view_with_submatches` copies submatches into an owned array before
    returning a `UserMatch` at `src/pika/clauses_user_view.c3:25` through
    `src/pika/clauses_user_view.c3:35`.
  - `user_view_tie` calls `get_user_view` for the inner clause at
    `src/pika/clauses_user_view.c3:102` through
    `src/pika/clauses_user_view.c3:103`, then calls `get_user_view` for each
    child during the count pass at `src/pika/clauses_user_view.c3:108` through
    `src/pika/clauses_user_view.c3:112` and again during the copy pass at
    `src/pika/clauses_user_view.c3:117` through
    `src/pika/clauses_user_view.c3:124`.
  - Those temporary `UserMatch` values can own `submatches` arrays, but
    `user_view_tie` never frees `inner.submatches` or any child
    `child.submatches` before returning only the flattened `ccmids` array at
    `src/pika/clauses_user_view.c3:127`.
  - The public parse/fold tree builders free only the immediate `um.submatches`
    returned to them at `src/pika/lisp_pika_parse_fold.c3:50` through
    `src/pika/lisp_pika_parse_fold.c3:57` and
    `src/pika/lisp_pika_parse_fold.c3:112` through
    `src/pika/lisp_pika_parse_fold.c3:118`; they cannot free the nested arrays
    discarded inside `user_view_tie`.
- Impact: grammars that use `TIE` nodes leak owned child submatch arrays while
  constructing user-facing parse trees or fold inputs. The leak scales with parse
  tree size and can remain hidden behind otherwise correct parse results.
- Next action: define a `UserMatch.release` helper, call it for every temporary
  `UserMatch` created during TIE flattening, and add a parser lifetime regression
  over a nested TIE grammar under ASAN/Valgrind.
- Resolution: TIE user-view flattening now releases temporary nested
  `UserMatch` submatch arrays during flattening, so the returned flattened view
  no longer discards owned child arrays.
- Validation: C3 LSP diagnostics were clean for touched Pika files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=123 fail=0`.

### AUDIT-125: Re-defining a named Pika grammar is a no-op that leaks the new grammar

- Severity: Medium
- Status: Closed
- Classification: parser runtime state bug, targeted named-grammar replacement
  fix
- Evidence:
  - `lookup_named_grammar` searches the global registry with
    `find_grammar_index_locked` and returns the first valid entry whose
    `SymbolId` matches the requested name at `src/pika/lisp_pika_named_grammars.c3:58`
    through `src/pika/lisp_pika_named_grammars.c3:75`.
  - `store_named_grammar` always appends to `g_named_grammars[g_grammar_count]`
    and increments the count at `src/pika/lisp_pika_named_grammars.c3:78`
    through `src/pika/lisp_pika_named_grammars.c3:89`; it never checks for an
    existing entry with the same name.
  - Public `pika/grammar` always calls `finalize_named_grammar`, which builds a
    heap-owned `Grammar` and then stores it by name at
    `src/pika/lisp_pika_grammar_compiler_api.c3:71` through
    `src/pika/lisp_pika_grammar_compiler_api.c3:90`.
  - Grammar objects own heap allocations such as clauses, seed clauses,
    terminals, epsilon tables, and names, and `regex_release_grammar_allocations`
    shows the destructor work needed to free them at
    `src/pika/regex_cache_lifetime.c3:65` through
    `src/pika/regex_cache_lifetime.c3:104`.
  - The user-facing reference presents `pika/grammar` as defining a grammar by a
    name at `docs/reference/08-libraries.part-01.md:62` through
    `docs/reference/08-libraries.part-01.md:74`, but there is no documented
    duplicate-name policy.
- Impact: a second `(pika/grammar 'g ...)` allocates and stores another grammar
  that is never selected by later `pika/parse`, `pika/fold`, `pika/grammar-rules`,
  or `pika/match-span` calls because lookup returns the older entry first. The
  newer grammar is effectively unreachable until process exit, and repeated
  dynamic grammar generation can grow the registry while preserving stale
  behavior.
- Next action: define the duplicate-name contract explicitly: either reject
  duplicate grammar names or replace the existing entry after releasing the old
  heap-owned grammar. Add regressions that redefine a grammar name and verify
  either a clear duplicate-name error or that parsing uses the replacement.
- Resolution: named Pika grammar redefinition now fails closed with
  `parser/grammar-duplicate-name` instead of appending an unreachable duplicate
  grammar.
- Validation: C3 LSP diagnostics were clean for touched Pika files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=123 fail=0`.

### AUDIT-126: Pika grammar finalization leaks temporary and failed-store allocations

- Severity: Medium
- Status: Closed
- Classification: parser runtime lifetime bug, targeted finalization cleanup fix
- Evidence:
  - `finalize_named_grammar` allocates a one-element `starts` array at
    `src/pika/lisp_pika_grammar_compiler_api.c3:79` through
    `src/pika/lisp_pika_grammar_compiler_api.c3:80`, passes it to `make_grammar`,
    and returns success at `src/pika/lisp_pika_grammar_compiler_api.c3:87` through
    `src/pika/lisp_pika_grammar_compiler_api.c3:90` without freeing
    `starts.ptr`.
  - The regex compiler performs the same `starts` allocation for `make_grammar`
    but frees it immediately after grammar construction at
    `src/pika/regex_cache_compile.c3:58` through
    `src/pika/regex_cache_compile.c3:64`, which establishes the expected
    ownership pattern.
  - `make_grammar` allocates heap-owned grammar internals for clauses, names,
    epsilon tables, seed clauses, and terminals at
    `src/pika/grammar_construction_make.c3:24` through
    `src/pika/grammar_construction_make.c3:57`.
  - If `store_named_grammar` fails after `make_grammar` succeeds,
    `finalize_named_grammar` raises `parser/grammar-oom` at
    `src/pika/lisp_pika_grammar_compiler_api.c3:87` through
    `src/pika/lisp_pika_grammar_compiler_api.c3:89` without releasing the
    just-built grammar.
  - The destructor path for those grammar internals exists in
    `regex_release_grammar_allocations` at `src/pika/regex_cache_lifetime.c3:65`
    through `src/pika/regex_cache_lifetime.c3:104`, but named-grammar
    finalization does not use it on failure.
- Impact: every successful `pika/grammar` leaks the transient `starts` array, and
  a registry-capacity or allocation failure after grammar construction leaks the
  whole built grammar. That makes grammar creation leak memory even when the
  definition itself succeeds, and the failure path is worse under allocation
  pressure.
- Next action: free `starts.ptr` after `make_grammar`, add cleanup on every
  post-construction error path in `finalize_named_grammar`, and add a forced
  registry-growth failure test under ASAN/Valgrind to prove the built grammar is
  released.
- Resolution: grammar finalization now frees the temporary `starts` array,
  releases built grammars on failed store, and uses the existing builder-payload
  cleanup path for failure cleanup.
- Validation: C3 LSP diagnostics were clean for touched Pika files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=123 fail=0`.

### AUDIT-127: Pika grammar accepts unresolved rule references as silent failing clauses

- Severity: Medium
- Status: Closed
- Classification: parser runtime behavior bug, targeted grammar validation fix
- Evidence:
  - The Pika reference allows forward references by saying later rules may be
    referenced before declaration at `docs/reference/08-libraries.part-01.md:72`
    through `docs/reference/08-libraries.part-01.md:74`.
  - `compile_clause_symbol_ref` handles an unknown symbol by adding a placeholder
    rule with `mk_fail()` and registering that placeholder name at
    `src/pika/lisp_pika_grammar_clause_nodes.c3:48` through
    `src/pika/lisp_pika_grammar_clause_nodes.c3:59`.
  - `gc_register_rule` replaces an existing placeholder only when a later rule
    with the same symbol is declared at
    `src/pika/lisp_pika_grammar_compiler_api.c3:55` through
    `src/pika/lisp_pika_grammar_compiler_api.c3:68`.
  - `finalize_named_grammar` validates only that the rule builder and start rule
    exist, then calls `make_grammar` and stores the grammar at
    `src/pika/lisp_pika_grammar_compiler_api.c3:71` through
    `src/pika/lisp_pika_grammar_compiler_api.c3:90`; it never scans for
    placeholders that were not resolved by a real rule declaration.
  - Current Pika grammar tests cover successful forward references at
    `src/lisp/tests_runtime_feature_pika_groups.c3:212` through
    `src/lisp/tests_runtime_feature_pika_groups.c3:218`, but there is no
    regression that an undefined reference is rejected.
- Impact: a typo such as `(pika/grammar 'g '(rule start misspelled-rule))`
  successfully defines a grammar whose referenced rule is a hidden `FAIL`
  clause. Later `pika/parse` calls return `nil` instead of a grammar definition
  error, making schema mistakes look like ordinary parse misses.
- Next action: track whether each registered rule entry is a placeholder or a
  real declaration, reject unresolved placeholders in `finalize_named_grammar`
  with a named `parser/grammar-undefined-rule` error, and add regressions for
  one missing rule and one valid forward reference.
- Resolution: unresolved Pika rule references now fail grammar definition with
  `parser/grammar-undefined-rule`, while valid forward references still resolve
  before finalization. Duplicate rule definitions also fail closed with
  `parser/grammar-duplicate-rule`.
- Validation: C3 LSP diagnostics were clean for touched Pika files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=123 fail=0`.

### AUDIT-128: Pika grammar compile errors leak builder-owned clause payloads

- Severity: Medium
- Status: Closed
- Classification: parser runtime lifetime bug, targeted grammar-builder teardown
  fix
- Evidence:
  - `prim_pika_grammar` initializes a `GrammarCompiler`, defers only
    `mem::free(gc.rb.rules.ptr)`, `gc.rule_names`, and `gc.rule_indices`, then
    returns immediately on parse/compile errors at
    `src/pika/lisp_pika_grammar_compiler_api.c3:110` through
    `src/pika/lisp_pika_grammar_compiler_api.c3:134`.
  - Clause compilation can allocate owned child arrays for `seq` and `first` via
    `compile_clause_children_from_list` at
    `src/pika/lisp_pika_grammar_clause_nodes.c3:32` through
    `src/pika/lisp_pika_grammar_clause_nodes.c3:39`, then stores them in
    builder rules at `src/pika/lisp_pika_grammar_clause_nodes.c3:62` through
    `src/pika/lisp_pika_grammar_clause_nodes.c3:75`.
  - Scan compilation can allocate owned character-class contexts and store them
    in `SCAN_CTX` clauses at `src/pika/lisp_pika_grammar_clause_scan.c3:56`
    through `src/pika/lisp_pika_grammar_clause_scan.c3:68` and
    `src/pika/lisp_pika_grammar_clause_scan.c3:102` through
    `src/pika/lisp_pika_grammar_clause_scan.c3:108`.
  - A teardown helper already exists for builder-owned clause payloads:
    `regex_release_rule_builder` frees `SEQ`/`FIRST` children and `SCAN_CTX`
    contexts before freeing the rule array at `src/pika/regex_cache_lifetime.c3:33`
    through `src/pika/regex_cache_lifetime.c3:63`.
  - `prim_pika_grammar` does not call that helper on error; it only frees the
    raw builder array pointer, leaving per-clause payloads allocated.
- Impact: malformed grammars that fail after partially compiling earlier clauses
  can leak child arrays and scanner contexts. Repeated invalid grammar attempts
  in an interactive process or service can grow heap usage even though each call
  reports a parser error.
- Next action: replace the raw `gc.rb.rules.ptr` defer with a grammar-builder
  release helper that frees owned clause payloads on error and only skips
  payload release after successful transfer to a stored grammar. Add ASAN or
  Valgrind coverage for an invalid grammar after a compiled `seq` and an invalid
  `scan` after a compiled character class.
- Resolution: Pika grammar compile-error paths now use builder-payload cleanup
  for partially compiled clause payloads, and forward-reference resolution moves
  clause payloads into placeholders instead of pointer-copying them.
- Validation: C3 LSP diagnostics were clean for touched Pika files;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1` passed with
  `pass=123 fail=0`.

### AUDIT-129: `fs-read` silently clamps requested reads to 8 MiB

- Severity: Medium
- Status: Closed
- Classification: filesystem runtime contract drift, targeted read-size policy
  fix
- Evidence:
  - The public primitive table documents `fs-read` as a two-argument filesystem
    handle read at `docs/reference/11-appendix-primitives.md:159` through
    `docs/reference/11-appendix-primitives.md:165`, with no maximum read size or
    partial-read signal.
  - `prim_fs_read` accepts any non-negative integer byte count, converts it to
    `usz`, and then silently clamps values above `8 * 1024 * 1024` at
    `src/lisp/prim_io_fs_stream.c3:122` through
    `src/lisp/prim_io_fs_stream.c3:129`.
  - The primitive then allocates only the clamped size, performs one native read,
    and returns the resulting string at `src/lisp/prim_io_fs_stream.c3:130`
    through `src/lisp/prim_io_fs_stream.c3:141`.
  - The lower native helper itself accepts the requested `size_t` length and
    passes it through an `unsigned int` libuv buffer length at
    `csrc/uv_helpers.c:68` through `csrc/uv_helpers.c:74`; earlier audit coverage
    flagged that native-width issue, but the public `fs-read` silent clamp is a
    separate user-visible contract problem.
- Impact: `(fs-read h 100000000)` can return at most 8 MiB while looking like a
  successful read. Callers cannot distinguish EOF, a short read from the OS, or
  an implementation-imposed cap unless they already know to loop manually.
- Resolution: `fs-read` now treats 8 MiB as a documented single-call maximum and
  fails closed with `io/fs-read-size-too-large` when the requested byte count
  exceeds that cap instead of silently clamping the request. The primitive
  appendix now describes the bounded single-read contract.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-effect-union-limit` passed with
  `pass=70 fail=0`; `git diff --check` passed for the touched filesystem,
  docs, and audit files.

### AUDIT-130: `fs-readdir` corrupts directory entry names containing newlines

- Severity: Medium
- Status: Closed
- Classification: filesystem runtime data fidelity bug, targeted native result
  encoding fix
- Evidence:
  - The public primitive table documents `fs-readdir` as returning directory
    entries at `docs/reference/11-appendix-primitives.md:167` through
    `docs/reference/11-appendix-primitives.md:170`, with no filename character
    restrictions.
  - The native helper concatenates every `uv_dirent_t.name` into one buffer,
    appending `'\n'` after each entry at `csrc/uv_helpers.c:135` through
    `csrc/uv_helpers.c:162`, then trims the final newline at
    `csrc/uv_helpers.c:164` through `csrc/uv_helpers.c:169`.
  - The C3 adapter splits the joined buffer on newline bytes and emits one
    string per non-empty segment at `src/lisp/prim_io_fs_handles.c3:87` through
    `src/lisp/prim_io_fs_handles.c3:115`.
  - POSIX filesystems permit newline bytes inside path components, so newline is
    not a safe record separator for directory entry names.
- Impact: a single file named `a\nb` is reported as two entries (`"a"` and
  `"b"`). Directory tooling built on `fs-readdir` can display, delete, rename, or
  compare the wrong path names while all calls appear successful.
- Resolution: the native `omni_uv_fs_scandir_join` bridge now returns
  NUL-delimited records with an explicit total byte length, and `prim_fs_readdir`
  splits on those NUL delimiters instead of newline bytes. POSIX filenames
  cannot contain NUL, so newline-containing names now survive the native/C3
  boundary as one exact entry.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-effect-union-limit` passed with
  `pass=71 fail=0`; `git diff --check` passed for the touched native,
  filesystem, and test files.

### AUDIT-131: Process and signal control primitives drop trailing arguments

- Severity: Medium
- Status: Closed
- Classification: runtime behavior, targeted arity validation
- Evidence:
  - The raw primitive table registers `__raw-process-kill` and
    `__raw-signal-handle` as variadic at
    `src/lisp/eval_init_primitive_tables.c3:288` through
    `src/lisp/eval_init_primitive_tables.c3:292`.
  - The process reference documents finite process/signal forms at
    `docs/reference/07-io-networking.md:173` through
    `docs/reference/07-io-networking.md:208`; the appendix also lists
    process/signal raw primitives at `docs/reference/11-appendix-primitives.md:643`
    through `docs/reference/11-appendix-primitives.md:647`.
  - `prim_process_kill` accepts direct calls whenever `args.len >= 2`, reads only
    `args[0]` and `args[1]`, and never rejects `args[2..]` at
    `src/lisp/async_process_signal_dns_process.c3:123` through
    `src/lisp/async_process_signal_dns_process.c3:151`.
  - `prim_signal_handle` accepts direct calls whenever `args.len >= 2`, reads
    only the signal and callback, and never rejects trailing arguments at
    `src/lisp/async_process_signal_dns.c3:22` through
    `src/lisp/async_process_signal_dns.c3:55`.
  - Existing process/signal tests cover valid spawn/wait/kill and invalid signal
    values, but the searched test surface contains no regressions for trailing
    `process-kill` or `signal-handle` arguments.
- Impact: malformed calls such as `(process-kill h 15 ignored)` or
  `(signal-handle 10 cb ignored)` perform side effects while dropping caller data.
  That can hide wrapper-generation bugs, future option objects, or effect payload
  shape mismatches.
- Next action: enforce exact arity for `process-kill` and `signal-handle` in both
  direct and packed-call paths, or document a real variadic contract. Add
  regressions that assert trailing arguments fail before the side effect starts.
- Resolution: `prim_process_kill` and `prim_signal_handle` now require exactly
  two arguments in raw direct calls and reject any non-empty packed tail after
  the second payload item. Regressions cover raw direct and packed trailing
  arguments, including process-kill failing before killing the child.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-effect-union-limit` passed with `pass=77
  fail=0`; `git diff --check` passed.

### AUDIT-132: `read-lines` emits a phantom line for empty files

- Severity: Medium
- Status: Closed
- Classification: filesystem runtime data-shape bug, targeted line-splitting
  fix
- Evidence:
  - The language/reference docs describe `read-lines` as reading a file into a
    list of lines at `docs/LANGUAGE_SPEC.part-03.md:114` and
    `docs/reference/11-appendix-primitives.md:157`.
  - `prim_read_lines` iterates with `i <= content_len` and emits a line whenever
    `i == content_len` or `content[i] == '\n'` at
    `src/lisp/prim_io_file.c3:150` through `src/lisp/prim_io_file.c3:164`.
  - For an empty file, `content_len == 0`; the loop still executes once with
    `line_len == 0`, allocates `make_string(interp, content[0:0])`, and conses it
    into the result.
  - Current async file tests cover `"a\nb"` and missing-path behavior at
    `src/lisp/tests_runtime_async_io_tls_groups.c3:98` through
    `src/lisp/tests_runtime_async_io_tls_groups.c3:158`, but they do not cover an
    empty file.
- Impact: an empty file returns `("")` instead of `nil`/`()`. Consumers that treat
  `length(read-lines path)` as the number of records will see one record where no
  line exists, which can corrupt CSV/log/config import logic.
- Resolution: `read-lines` now returns `nil` immediately for empty file content
  before entering the splitter loop. Existing splitting behavior is preserved for
  unterminated final lines and trailing newline line shape.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=async` passed with `pass=76 fail=0`; `git
  diff --check` passed for touched async filesystem test files.

### AUDIT-133: `pipe-listen` unlinks the requested path before binding

- Severity: High
- Status: Closed
- Classification: filesystem/network runtime data-loss bug, targeted path
  lifecycle hardening
- Evidence:
  - The pipe docs show callers creating a Unix socket with
    `(pipe-listen path)` and explicitly cleaning it up later with
    `(fs-unlink path)` at `docs/reference/07-io-networking.md:150` through
    `docs/reference/07-io-networking.md:163`.
  - The raw primitive appendix documents `__raw-pipe-listen` as a one-argument
    Unix socket listener at `docs/reference/11-appendix-primitives.md:641`
    through `docs/reference/11-appendix-primitives.md:642`; it does not state
    that the path is destructively removed first.
  - `prim_pipe_listen` passes the caller string directly to the native helper at
    `src/lisp/async_udp_pipe.c3:138` through
    `src/lisp/async_udp_pipe.c3:147`.
  - The native helper unconditionally executes `(void)unlink(path)` before
    binding at `csrc/uv_helpers_pipe.c:110` through
    `csrc/uv_helpers_pipe.c:113`.
- Impact: `(pipe-listen "/tmp/config.json")` can delete an existing regular file
  before failing or replacing it with a socket. This is surprising for a listen
  primitive and makes stale-socket cleanup indistinguishable from data loss.
- Resolution: native pipe/socket listen helpers now fail closed when the target
  path already exists instead of unconditionally unlinking it before bind. They
  only unlink an owned socket path after a post-bind listen failure. A regression
  proves `pipe-listen` failure preserves a pre-existing regular file.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=async` passed with
  `pass=77 fail=0`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `pass=141 fail=0`;
  `git diff --check` passed for the touched pipe/runtime async files.

### AUDIT-134: `tcp-listen` silently clamps backlog values

- Severity: Medium
- Status: Closed
- Classification: network runtime contract drift, targeted option validation
- Evidence:
  - The networking reference documents `tcp-listen` with an optional backlog
    third argument at `docs/reference/07-io-networking.md:89` through
    `docs/reference/07-io-networking.md:93`, with no visible minimum, maximum,
    or coercion rule.
  - `prim_tcp_listen` accepts a backlog value, then silently rewrites values
    below `1` to `1` and values above `1024` to `1024` at
    `src/lisp/async_tcp_transport_listen.c3:33` through
    `src/lisp/async_tcp_transport_listen.c3:44`.
  - The primitive remains registered as variadic at
    `src/lisp/eval_init_primitive_tables.c3:275` through
    `src/lisp/eval_init_primitive_tables.c3:277`, so local validation is the
    only place users can learn the backlog contract.
  - The searched test surface contains no explicit `tcp-listen` backlog
    boundary regression; existing "backlog" matches are scheduler queue tests,
    not listener backlog validation.
- Impact: `(tcp-listen host port 0)` and `(tcp-listen host port 100000)` can
  succeed with different backlog values than requested. Server capacity and
  overload behavior become configuration-dependent while appearing successful.
- Next action: either document the clamp as the public policy or reject out-of-
  range backlog values with `io/tcp-listen-invalid-backlog`. Add boundary tests
  for `0`, `1`, `1024`, and `1025`.

### AUDIT-135: TCP DNS resolution can return IPv6 addresses that TCP connect/listen reject

- Severity: Medium
- Status: Closed
- Classification: runtime networking behavior, targeted IPv6 parity fix
- Evidence:
  - The networking guide documents hostname-based `tcp-connect`, including
    `(tcp-connect "example.com" 80)`, at
    `docs/reference/07-io-networking.md:76` through
    `docs/reference/07-io-networking.md:79`; the TLS example also builds on
    `(tcp-connect "example.com" 443)` at
    `docs/reference/07-io-networking.md:223`.
  - `dns-resolve` renders both `AF_INET` and `AF_INET6` addrinfo results:
    `csrc/addrinfo_helpers.c:35` through `csrc/addrinfo_helpers.c:48` selects
    either IPv4 or IPv6 storage and calls `inet_ntop` with the addrinfo family.
  - `prim_tcp_connect` resolves the host with `prim_dns_resolve`, copies the
    rendered string, and passes it to `omni_uv_tcp_connect_start` at
    `src/lisp/async_tcp_transport_connect.c3:43` through
    `src/lisp/async_tcp_transport_connect.c3:72`.
  - `omni_uv_tcp_connect_start` only calls `uv_ip4_addr` into
    `struct sockaddr_in` at `csrc/uv_helpers_tcp.c:95` through
    `csrc/uv_helpers_tcp.c:101`; there is no IPv6 branch for `uv_ip6_addr` /
    `sockaddr_in6`.
  - `prim_tcp_listen` follows the same resolve-and-copy path at
    `src/lisp/async_tcp_transport_listen.c3:59` through
    `src/lisp/async_tcp_transport_listen.c3:81`, and
    `omni_uv_tcp_listen_fd` also uses only `uv_ip4_addr` at
    `csrc/uv_helpers_tcp.c:253` through `csrc/uv_helpers_tcp.c:258`.
- Impact: IPv6-only hostnames, or hostnames whose first resolved addrinfo is
  IPv6, can be accepted by `dns-resolve` and then fail in `tcp-connect` or
  `tcp-listen` with a generic async-init/bind error. The public surface appears
  hostname-capable while internally supporting only IPv4-formatted results.
- Resolution: TCP native helpers now parse numeric IPv6 addresses through
  `uv_ip6_addr` and IPv4 addresses through `uv_ip4_addr`, using
  `sockaddr_storage` for connect/listen. Hostname resolution can therefore pass
  rendered IPv6 results into the TCP helpers.
- Validation: `cc -fsyntax-only -Ideps/src/libuv/include
  csrc/uv_helpers_tcp.c` passed; `LIBRARY_PATH=/home/christos/.local/lib c3c
  --threads 1 build --obj-out obj` linked `build/main`; eval probe
  `(tcp-listen "::1" 0 8)` inside a spawned fiber returned `true` locally.

### AUDIT-136: HTTP request builder can succeed after skipping required CRLF separators

- Severity: Medium
- Status: Closed
- Classification: runtime HTTP behavior, targeted fixed-buffer validation
- Evidence:
  - Direct `http-get` and `http-request` assemble requests with `build_request`
    at `src/lisp/http.c3:56` through `src/lisp/http.c3:60` and
    `src/lisp/http.c3:145` through `src/lisp/http.c3:150`.
  - The request buffers are fixed at 4096 bytes for `http-get` and 8192 bytes
    for `http-request` in `src/lisp/async_io_shared.c3:7` through
    `src/lisp/async_io_shared.c3:8`.
  - `build_request` copies each segment while `pos < buf_size`, but optional
    two-byte separators are appended only when `pos + 1 < buf_size` at
    `src/lisp/http_url_response.c3:96`,
    `src/lisp/http_url_response.c3:105`, and
    `src/lisp/http_url_response.c3:121`.
  - If exactly one byte remains where a `\r\n` separator is required, those
    branches skip the separator without marking failure. The final guard only
    checks `pos >= buf_size` at `src/lisp/http_url_response.c3:126` through
    `src/lisp/http_url_response.c3:127`, so `pos == buf_size - 1` still returns
    success with a malformed/truncated request.
  - The public error model says HTTP client failures use canonical payloaded
    `raise` codes at `docs/ERROR_MODEL.md:49`, and the direct callers already
    map `build_request` failure to `io/http-get-request-too-large` or
    `io/http-request-too-large`; the missing separator path bypasses that
    fail-closed behavior.
- Impact: crafted method/path/header lengths can produce a request missing a
  required CRLF while still being sent over TCP/TLS. That can corrupt request
  framing, merge user headers with generated headers, or make servers reject the
  request without Omni reporting the local request-size contract violation.
- Next action: replace all append sites in `build_request` with a checked
  `append_bytes` helper that fails before partial writes, require exact space for
  each generated separator, and add boundary tests for `buf_size - 1`,
  `buf_size`, and `buf_size + 1` request lengths.
- Resolution: `build_request` now uses checked append helpers for every request
  segment and required CRLF separator, failing before partial writes instead of
  emitting malformed/truncated requests. Regression coverage proves an exact-fit
  request succeeds while a one-byte-short buffer fails.
- Validation: C3 LSP diagnostics were clean for `src/lisp/http_url_response.c3`
  and `src/lisp/tests_runtime_feature_http_groups.c3`;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed after
  removing a corrupted generated `build/main` from overlapping link attempts;
  `OMNI_LISP_TEST_SLICE=http OMNI_TEST_SUMMARY=1` passed with
  `pass=32 fail=0`.

### AUDIT-137: TLS client session cache grows without eviction or teardown

- Severity: Medium
- Status: Closed
- Classification: runtime TLS lifecycle/performance, targeted cache policy fix
- Evidence:
  - The networking reference documents optional session resumption and says Omni
    caches BearSSL client sessions per hostname when `resume-session?` is true at
    `docs/reference/07-io-networking.md:246` through
    `docs/reference/07-io-networking.md:248`.
  - `tls-connect` copies the hostname into `th.session_host` when resumption is
    enabled at `src/lisp/tls_offload_connect.c3:80` through
    `src/lisp/tls_offload_connect.c3:90`, then loads a cache entry for that
    hostname at `src/lisp/tls_offload_connect.c3:133` through
    `src/lisp/tls_offload_connect.c3:136`.
  - Closing or finalizing a TLS handle stores the session through
    `tls_maybe_store_client_session` at `src/lisp/tls_handle_lifecycle.c3:48`
    through `src/lisp/tls_handle_lifecycle.c3:50` and
    `src/lisp/tls_offload_boundary.c3:105` through
    `src/lisp/tls_offload_boundary.c3:108`.
  - The native cache is a process-global linked list rooted at
    `g_omni_tls_session_cache_head` at
    `csrc/tls_helpers_server_creds.inc:381` through
    `csrc/tls_helpers_server_creds.inc:388`.
  - `omni_tls_client_session_cache_store` appends a new node for every previously
    unseen hostname at `csrc/tls_helpers_server_creds.inc:424` through
    `csrc/tls_helpers_server_creds.inc:452`; there is no entry count limit, LRU
    eviction, expiry, public clear API, or process-shutdown cleanup path.
- Impact: long-running clients that connect with `resume-session? true` across
  many hostnames retain every hostname string and BearSSL session parameter block
  for the life of the process. That turns an optional performance cache into
  unbounded process-global memory growth.
- Next action: add an explicit TLS session-cache policy: fixed capacity with LRU
  eviction, expiry, and a teardown/clear API. Include stress tests that connect
  to more unique hostnames than the cap and verify bounded native allocation.
- Resolution: the native TLS client session cache is now a 64-entry MRU/LRU
  cache. Loads and stores refresh recency, new overflow entries evict the least
  recently used tail entry, cache entries are cleared through an explicit
  `omni_tls_client_session_cache_clear` API, and process teardown registers a
  one-time `atexit` cleanup.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  native `tests/native/tls_session_cache_policy_test.c` compiled and passed
  against `build/libomni_chelpers.a` and `deps/lib/libbearssl.a`;
  `git diff --check` passed.

### AUDIT-138: `Float64` dictionary and set keys all hash to zero

- Severity: Medium
- Status: Closed
- Classification: runtime collection performance, targeted hash/equality fix
- Evidence:
  - The language spec says dictionary keys are value-typed and include stable
    value keys beyond symbols/strings/integers at
    `docs/LANGUAGE_SPEC.part-03.md:121` through
    `docs/LANGUAGE_SPEC.part-03.md:125`; generic `ref` is documented as
    supporting dictionary lookup by any key type at
    `docs/LANGUAGE_SPEC.part-03.md:145`.
  - Set ordering is documented as using the same comparator family as dictionary
    keys at `docs/LANGUAGE_SPEC.part-03.md:168` through
    `docs/LANGUAGE_SPEC.part-03.md:176`.
  - `hashmap_key_equals` has an explicit `DOUBLE` equality branch at
    `src/lisp/prim_collection_hashmap_key_helpers.c3:130` through
    `src/lisp/prim_collection_hashmap_key_helpers.c3:131`.
  - `hash_value` handles `FLOAT32`, fixed complex, BigInteger/BigFloat/
    BigComplex, strings, symbols, and TimePoint, but has no `DOUBLE` case;
    `DOUBLE` keys therefore fall through to `default: return 0` at
    `src/lisp/prim_collection_hashmap_key_helpers.c3:37` through
    `src/lisp/prim_collection_hashmap_key_helpers.c3:116`.
  - Hash-map lookup starts from `hash_value(key) & map.mask` and probes linearly
    at `src/lisp/prim_collection_hashmap.c3:128` through
    `src/lisp/prim_collection_hashmap.c3:136`; set insertion uses the same
    hash-map backing at `src/lisp/prim_collection_generic_set.c3:247` through
    `src/lisp/prim_collection_generic_set.c3:259`.
- Impact: every `Float64` key lands in the same probe cluster. Lookups and
  insertions with many floating-point keys degrade toward linear scans, and set
  workloads inherit the same hot bucket. This creates an avoidable performance
  cliff on a documented value-key surface.
- Resolution: `hash_value` now has a `DOUBLE` branch consistent with
  `hashmap_key_equals`: `+0.0` and `-0.0` are folded to the same hash because
  they compare equal, while NaN keys may share a stable hash and still remain
  non-equal under probing. Collection regressions cover distinct Float64 hashes,
  dictionary lookup, set membership, signed-zero equality, and NaN lookup
  semantics.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module` passed with
  `pass=2110 fail=0`; `git diff --check` passed for the touched collection and
  audit files.

### AUDIT-139: `ffi-callback` `'String` parameters are passed to Omni as foreign handles

- Severity: High
- Status: Closed
- Classification: runtime FFI callback correctness, targeted string marshalling fix
- Evidence:
  - The FFI reference documents `ffi-callback` as accepting supported type symbols
    including `'String` at `docs/reference/09-concurrency-ffi.md:258` through
    `docs/reference/09-concurrency-ffi.md:265`, and says libffi marshals callback
    arguments into Omni values at `docs/reference/09-concurrency-ffi.md:267`
    through `docs/reference/09-concurrency-ffi.md:269`.
  - The shared type parser maps `String` annotations to `FFI_TYPE_STRING` at
    `src/lisp/value_predicates_accessors_basic.c3:156` through
    `src/lisp/value_predicates_accessors_basic.c3:168`, and the FFI surface name
    for that tag is `"String"` at `src/lisp/foreign_runtime_core.c3:157` through
    `src/lisp/foreign_runtime_core.c3:160`.
  - Callback argument conversion groups `FFI_TYPE_STRING` with pointer, buffer,
    and struct tags at `src/lisp/prim_ffi_callback.c3:108` through
    `src/lisp/prim_ffi_callback.c3:119`; non-null C string arguments are wrapped
    with `make_ffi_handle_ex(..., "ffi-callback-string", ...)` instead of being
    copied into an Omni `STRING`.
  - Existing callback coverage only exercises integer callbacks at
    `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:625` through
    `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:656`; there is
    no regression proving a callback declared with `'String` receives a usable
    Omni string.
- Impact: a documented callback type silently delivers a `ForeignHandle` where
  users and dispatch signatures would reasonably expect `String`. String
  operations, typed callback lambdas, and multi-dispatch on `String` fail or take
  the wrong path when invoked from C with a `char*`.
- Next action: split callback argument conversion so `FFI_TYPE_STRING` copies
  NUL-terminated `char*` arguments into Omni strings, keeping `Buffer`, `Struct`,
  and `ForeignHandle` on the handle path. Add an end-to-end callback helper that
  invokes a `char* -> Integer/String` callback and asserts the lambda receives a
  real `String`.
- Resolution: callback argument marshalling now treats `FFI_TYPE_STRING` as a
  copied NUL-terminated C string boundary and constructs a real Omni `String`
  value for the callback body, while pointer/buffer/struct values stay on the
  foreign-handle path.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system`
  passed with `pass=167 fail=0`.

### AUDIT-140: `ffi-callback` `'String` returns expose raw Omni string storage to C

- Severity: High
- Status: Closed
- Classification: runtime FFI callback lifetime boundary, targeted string
  return marshalling fix
- Evidence:
  - `ffi-callback` documents `'String` as a supported return type symbol at
    `docs/reference/09-concurrency-ffi.md:258` through
    `docs/reference/09-concurrency-ffi.md:265`.
  - Callback dispatch evaluates the Omni callable and immediately converts its
    result back into the libffi return slot at `src/lisp/prim_ffi_callback.c3:242`
    through `src/lisp/prim_ffi_callback.c3:245`.
  - Return conversion groups `FFI_TYPE_STRING` with pointer-like returns at
    `src/lisp/prim_ffi_callback.c3:181` through
    `src/lisp/prim_ffi_callback.c3:194`; if the Omni result is `STRING`, it writes
    `value.str_chars` directly into the C return slot at
    `src/lisp/prim_ffi_callback.c3:187` through
    `src/lisp/prim_ffi_callback.c3:188`.
  - Omni string storage is region/value-owned: `Value` stores `str_chars` and
    `str_len` at `src/lisp/value_core_types.c3:209` through
    `src/lisp/value_core_types.c3:210`, and `make_string` allocates/copies the
    buffer through the current interpreter scope at
    `src/lisp/value_constructors_core.c3:88` through
    `src/lisp/value_constructors_core.c3:109`.
- Impact: a C caller can receive a pointer into Omni-managed region storage with
  no explicit ownership, pinning, or copy policy. If the callback returns a fresh
  string, the pointer can outlive the callback's temporary/scope lifetime, and C
  code has no contract for whether it may retain or free the returned pointer.
- Next action: define the callback string-return contract explicitly. Either
  reject `'String` return callbacks until an ownership policy exists, or allocate
  return C strings through a documented finalizer/release path with tests for
  immediate use, retained use after the callback returns, and callback handle
  release.
- Resolution: `ffi-callback` now fails closed when asked to construct callbacks
  with a `'String` return type. The FFI reference documents that callback string
  returns remain unsupported until Omni exposes an explicit C ownership/release
  contract, preventing raw region-owned string storage from escaping to C.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system`
  passed with `pass=167 fail=0`.

### AUDIT-141: `string-graphemes` silently truncates after 512 clusters

- Severity: Medium
- Status: Closed
- Classification: runtime Unicode collection correctness, targeted materializer
  capacity fix
- Evidence:
  - `string-graphemes` is registered as a public one-argument primitive at
    `src/lisp/eval_init_primitive_tables.c3:236` through
    `src/lisp/eval_init_primitive_tables.c3:239`.
  - The reference appendix describes `string-graphemes` as "Split into graphemes"
    at `docs/reference/11-appendix-primitives.md:577` through
    `docs/reference/11-appendix-primitives.md:583`, and the library reference
    shows it returning the complete cluster list for a string at
    `docs/reference/08-libraries.part-01.md:137` through
    `docs/reference/08-libraries.part-01.md:138`.
  - The implementation uses a fixed `Value*[512] clusters` buffer and only appends
    when `cluster_count < 512` at `src/lisp/unicode.c3:60` through
    `src/lisp/unicode.c3:81`.
  - The final cluster is also skipped when `cluster_count >= 512` at
    `src/lisp/unicode.c3:89` through `src/lisp/unicode.c3:94`, then only the
    collected prefix is returned at `src/lisp/unicode.c3:96` through
    `src/lisp/unicode.c3:101`.
  - Unicode tests cover only the ASCII `"abc"` grapheme case at
    `src/lisp/tests_runtime_data_unicode_groups.c3:401` through
    `src/lisp/tests_runtime_data_unicode_groups.c3:403`; there is no regression
    above 512 clusters.
- Impact: long strings lose all grapheme clusters after the first 512 without an
  error or documented limit. Any caller using `string-graphemes` for UI cursor
  movement, display segmentation, validation, or round-tripping receives an
  incomplete list.
- Resolution: `string-graphemes` now materializes clusters through checked
  cons-list construction instead of a fixed `Value*[512]` buffer, then reverses
  the accumulated list through the same checked allocation path to preserve
  output order. Regressions cover 513 ASCII graphemes, order preservation, and a
  combining-mark grapheme cluster.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=unicode` passed with `pass=31 fail=0`;
  `git diff --check` passed for the touched Unicode and audit files.

### AUDIT-142: `string-codepoints` silently truncates after 1024 codepoints

- Severity: Medium
- Status: Closed
- Classification: runtime Unicode collection correctness, targeted materializer
  capacity fix
- Evidence:
  - `string-codepoints` is registered as a public one-argument primitive at
    `src/lisp/eval_init_primitive_tables.c3:236` through
    `src/lisp/eval_init_primitive_tables.c3:239`.
  - The reference appendix describes `string-codepoints` as "List of codepoints"
    at `docs/reference/11-appendix-primitives.md:577` through
    `docs/reference/11-appendix-primitives.md:583`, and the library reference
    shows it returning the complete codepoint list at
    `docs/reference/08-libraries.part-01.md:140` through
    `docs/reference/08-libraries.part-01.md:141`.
  - The implementation stores codepoints in a fixed `Value*[1024] cps` buffer at
    `src/lisp/unicode.c3:117` through `src/lisp/unicode.c3:118`.
  - The scanner loop stops when `count < 1024` becomes false, even if `pos` is
    still before the end of the input, at `src/lisp/unicode.c3:121` through
    `src/lisp/unicode.c3:127`.
  - Runtime Unicode tests exercise 600-codepoint strings and short ASCII cases at
    `src/lisp/tests_runtime_data_unicode_groups.c3:358` through
    `src/lisp/tests_runtime_data_unicode_groups.c3:367`, so the hidden 1024-item
    cap is not covered.
- Impact: `(string-codepoints s)` returns a valid-looking prefix for strings
  longer than 1024 codepoints. This breaks length checks, Unicode analysis,
  serialization helpers, and any code that expects the primitive's list result to
  cover the whole input.
- Resolution: `string-codepoints` now materializes every decoded codepoint
  through checked cons-list construction instead of a fixed `Value*[1024]`
  buffer, then reverses the accumulated list with allocation-failure
  propagation. Regression coverage now includes a 1025-codepoint ASCII string.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=unicode` passed with `pass=31 fail=0`;
  `git diff --check` passed for the touched Unicode and audit files.

### AUDIT-143: `ml/plot` silently treats two-sample multi-series input as x/y pairs

- Severity: Medium
- Status: Closed
- Classification: runtime ML visualization contract drift, targeted parser
  disambiguation fix
- Evidence:
  - The primitive appendix documents `ml/plot` as accepting "Array/List
    multi-series overlays" at `docs/reference/11-appendix-primitives.md:276`.
  - The collection reference says `ml/plot` accepts Array/List multi-series
    overlays and records `props.series-list` at
    `docs/reference/03-collections.part-01.md:422`.
  - The ML visualization decision note states that Array/List inputs containing
    multiple scalar series produce `props.plot-kind 'multi-series` at
    `docs/plans/ml-visualization-surface-decision-2026-04-22.md:96` through
    `docs/plans/ml-visualization-surface-decision-2026-04-22.md:100`.
  - `ml_plot_ext_try_multi_series` explicitly declines multi-series parsing when
    the first inner series has length 2 unless options contain
    `plot-kind 'multi-series`, at `src/lisp/prim_ml_plot_extensions.c3:187`
    through `src/lisp/prim_ml_plot_extensions.c3:200`.
  - The fallback x/y pair path is tested for `[[0 1] [2 5]]` at
    `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:92`
    through `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:93`,
    while the only multi-series regression uses length-3 series at
    `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:98`
    through `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:99`.
- Impact: `[[1 2] [3 4]]`, a natural two-series/two-sample overlay, is parsed as
  two x/y points unless callers know an undocumented `plot-kind 'multi-series`
  escape hatch. This makes the documented multi-series contract data-shape
  dependent and can silently render the wrong visualization.
- Next action: make the public disambiguation explicit and tested. Prefer a
  documented option contract for ambiguous two-wide nested arrays, or choose
  multi-series by default when the outer list is multiple scalar rows and add a
  separate explicit x/y mode for point lists.
- Resolution: ambiguous two-wide nested plot data now fails closed unless the
  caller supplies an explicit `plot-kind`. Use `(Dictionary 'plot-kind 'xy)` for
  x/y point pairs or `(Dictionary 'plot-kind 'multi-series)` for two-sample
  multi-series overlays.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module`
  passed with `pass=2116 fail=0`; `git diff --check` passed.

### AUDIT-144: `ml/plot` records y zoom bounds that terminal rendering ignores

- Severity: Medium
- Status: Closed
- Classification: runtime ML visualization rendering contract drift, targeted
  lowering/ABI fix
- Evidence:
  - The collection reference says graph zoom options may include `x-min`,
    `x-max`, `y-min`, and `y-max` at
    `docs/reference/03-collections.part-01.md:422`.
  - The closed ML visualization TODO records non-interactive `zoom` data-window
    bounds as shipped behavior at `docs/todo_parts/todo_part_15.md:711` through
    `docs/todo_parts/todo_part_15.md:719`.
  - `ml_plot_ext_read_zoom` parses and validates `y-min`/`y-max` at
    `src/lisp/prim_ml_plot_extensions.c3:61` through
    `src/lisp/prim_ml_plot_extensions.c3:69`.
  - `ml_plot_ext_window_series` only filters samples by x bounds; after reading
    zoom, it returns immediately when no x bound is present at
    `src/lisp/prim_ml_plot_extensions.c3:103` through
    `src/lisp/prim_ml_plot_extensions.c3:109`, and the loop only checks
    `x-min`/`x-max` at `src/lisp/prim_ml_plot_extensions.c3:115` through
    `src/lisp/prim_ml_plot_extensions.c3:130`.
  - `ml_plot_ext_apply_zoom_props` stores `y-min`/`y-max` metadata at
    `src/lisp/prim_ml_plot_extensions.c3:82` through
    `src/lisp/prim_ml_plot_extensions.c3:100`, but terminal lowering never reads
    `props.zoom`: `ui_ftxui_lower_typed_graph` copies only series, dimensions,
    x-values, and marker into `OmniFtxuiPlotOptions` at
    `src/lisp/prim_ui_ftxui_plot_lowering.c3:243` through
    `src/lisp/prim_ui_ftxui_plot_lowering.c3:313`.
  - The FTXUI plot ABI has no axis-bound fields for y-range propagation at
    `src/lisp/ftxui_ffi_types.c3:59` through `src/lisp/ftxui_ffi_types.c3:74`.
  - The regression test asserts x-window filtering and `zoom.x-min` metadata, but
    does not assert any y-bound effect, at
    `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:100`
    through `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:101`.
- Impact: callers can pass `(Dictionary 'zoom (Dictionary 'y-min ... 'y-max ...))`
  and receive valid-looking graph metadata, but native FTXUI rendering uses the
  unbounded y-domain. This is especially misleading for loss curves and
  multi-series comparisons where y-range is the primary view control.
- Next action: define y zoom semantics and wire them through the terminal path:
  either add explicit axis-bound fields to `OmniFtxuiPlotOptions` and the native
  renderer, or fail/document y-only zoom as unsupported until rendering honors
  it. Add tests for y-only zoom and combined x/y zoom.
- Resolution: `props.zoom.y-min` and `props.zoom.y-max` now flow through C3 plot
  lowering into `OmniFtxuiPlotOptions`, and the native FTXUI plot renderer uses
  those bounds for y-range scaling. The native bridge rejects non-finite or
  inverted y bounds.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module`
  passed with `pass=2116 fail=0`; `git diff --check` passed.

### AUDIT-145: `kernel/capture` skips full `Kernel` spec revalidation

- Severity: Medium
- Status: Closed
- Classification: runtime kernel metadata validation, targeted capture-entry
  parity fix
- Evidence:
  - The primitive appendix documents `kernel/capture` as validating a checked
    Vulkan `Kernel` against runtime inputs and push data at
    `docs/reference/11-appendix-primitives.md:405` through
    `docs/reference/11-appendix-primitives.md:407`.
  - The collection reference describes `kernel/capture(kernel inputs push)` as
    validating checked Vulkan direct-helper Kernel families at
    `docs/reference/03-collections.part-01.md:421`.
  - The shared `kernel_validate_spec` path rejects unknown keys, validates
    required fields, validates IO descriptors, checks push/workgroup shape, and
    validates source dictionaries at `src/lisp/prim_kernel.c3:110` through
    `src/lisp/prim_kernel.c3:142`.
  - `Kernel(...)` calls `kernel_validate_spec` before normalization at
    `src/lisp/prim_kernel.c3:564` through `src/lisp/prim_kernel.c3:568`, and
    `kernel/run` reuses the same full validation before execution at
    `src/lisp/prim_kernel.c3:574` through `src/lisp/prim_kernel.c3:577`.
  - `kernel/capture` only checks the shallow `kernel_is_value` predicate at
    `src/lisp/prim_kernel_capture.c3:158` through
    `src/lisp/prim_kernel_capture.c3:160`, then branches directly on backend and
    operation at `src/lisp/prim_kernel_capture.c3:167` through
    `src/lisp/prim_kernel_capture.c3:178`.
  - `kernel_is_value` itself only requires `kind`, symbol-valued `backend` and
    `operation`, array-valued `inputs` and `outputs`, and a non-empty outputs
    array at `src/lisp/prim_kernel.c3:11` through `src/lisp/prim_kernel.c3:23`;
    it does not reject unknown keys, invalid `workgroup`, invalid `source`,
    invalid `entry`, or invalid `metadata`.
  - Existing tests cover `Kernel` constructor validation for unknown keys,
    output descriptors, workgroup, and source data at
    `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:156`
    through `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:163`,
    but the `kernel/capture` tests only cover placement and push data at
    `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:198`
    through `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:201`.
- Impact: a mutable or hand-forged dictionary with `kind 'kernel` can bypass
  validation that `Kernel(...)` and `kernel/run` would enforce. Capture can
  publish a `kernel-graph` plan from a Kernel value that still carries invalid
  optional fields or unrecognized metadata, making capture less fail-closed than
  execution and weakening the "checked Kernel" contract.
- Resolution: `prim_kernel_capture` now calls `kernel_validate_spec` before
  trusting the shallow Kernel shape predicate, matching the constructor and
  `kernel/run` validation contract. Regressions cover mutated `workgroup`,
  mutated `source`, and hand-forged unknown-key Kernel dictionaries.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  linked `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module` passed with
  `pass=2114 fail=0`; `git diff --check` passed for the touched kernel capture
  files.

### AUDIT-146: `tensor/run` does not check graph scratch allocations before writes

- Severity: Medium
- Status: Closed
- Classification: runtime tensor graph execution OOM safety, targeted allocation
  guard fix
- Evidence:
  - The primitive appendix describes `tensor/run` as executing captured
    `tensor-graph` dictionaries and says malformed graphs fail closed at
    `docs/reference/11-appendix-primitives.md:408` through
    `docs/reference/11-appendix-primitives.md:409`.
  - `tensor/run` allocates the per-node result table with
    `mem::new_array(Value*, nodes.array_val.length)` at
    `src/lisp/prim_tensor_graph_run.c3:528`, then immediately frees and writes
    through the returned slice at `src/lisp/prim_tensor_graph_run.c3:529`
    through `src/lisp/prim_tensor_graph_run.c3:530` without checking whether the
    allocation returned a null pointer for a non-empty graph.
  - `tensor_graph_run_usz_array` allocates axis/shape scratch storage with
    `mem::new_array(usz, value.array_val.length)` at
    `src/lisp/prim_tensor_graph_run.c3:353`, then writes `items[i]` in the loop
    at `src/lisp/prim_tensor_graph_run.c3:354` through
    `src/lisp/prim_tensor_graph_run.c3:360` without checking for allocation
    failure.
  - The direct SPIR-V kernel helper shows the expected local pattern by checking
    the `mem::new_array` result and returning `nn_oom(interp)` before copying at
    `src/lisp/prim_kernel_source.c3:177` through
    `src/lisp/prim_kernel_source.c3:189`.
  - Tensor graph tests cover malformed graph dictionaries and node metadata at
    `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:447`
    through `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3:455`,
    but the memory-lifetime allocation-fault suites have no `tensor/run`
    allocation-failure regression.
- Impact: under memory pressure or with a very large user-supplied
  `tensor-graph`, `tensor/run` can dereference null scratch buffers instead of
  raising a typed `tensor/out-of-memory` or `tensor/invalid-argument` error. This
  is reachable before backend execution and undermines the graph-run fail-closed
  contract.
- Resolution: `tensor/run` now checks the per-node result table allocation and
  contract axis/shape scratch allocations before writing through them, returning
  typed `tensor/out-of-memory` errors for oversized or failed scratch buffers.
  Deterministic oversized-metadata regressions cover the result table guard and
  the axis/shape scratch helper guard.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module` passed with
  `pass=2112 fail=0`; `git diff --check` passed for touched tensor graph files.

### AUDIT-147: task/thread batch scheduler paths leak parsed work resources on early errors

- Severity: High
- Status: Closed
- Classification: runtime scheduler resource cleanup, targeted ownership helper
  fix
- Evidence:
  - The primitive appendix exposes `__raw-task-spawn-batch` and
    `__raw-thread-spawn-batch` as query-style raw scheduler primitives at
    `docs/reference/11-appendix-primitives.md:57`, and lists their arity at
    `docs/reference/11-appendix-primitives.md:653` and
    `docs/reference/11-appendix-primitives.md:658`.
  - `OffloadWork` can own a scheduler `SharedHandle*` and a `custom_ctx` with a
    `custom_free` callback at `src/lisp/scheduler_state_support_types.c3:142`
    through `src/lisp/scheduler_state_support_types.c3:153`; the cleanup helper
    for custom payloads is `scheduler_release_custom_ctx` at
    `src/lisp/scheduler_state_offload_helpers.c3:69` through
    `src/lisp/scheduler_state_offload_helpers.c3:74`.
  - `__raw-offload-batch` handles parse failure by releasing every parsed
    `works[j].shared` and custom context before returning at
    `src/lisp/scheduler_primitives_offload_execute.c3:119` through
    `src/lisp/scheduler_primitives_offload_execute.c3:125`.
  - `__raw-task-spawn-batch` builds the same `OffloadWork[]`, but on parse
    failure returns immediately at
    `src/lisp/scheduler_primitives_task_spawn.c3:128` through
    `src/lisp/scheduler_primitives_task_spawn.c3:142` without releasing any
    already-parsed `shared` or `custom_ctx` payloads. It also returns on
    `task_ids` / `generations` allocation failure at
    `src/lisp/scheduler_primitives_task_spawn.c3:145` through
    `src/lisp/scheduler_primitives_task_spawn.c3:154` after parsing all work
    items, again without releasing parsed work resources.
  - `__raw-thread-spawn-batch` has the same immediate parse-failure return at
    `src/lisp/scheduler_primitives_threads.c3:117` through
    `src/lisp/scheduler_primitives_threads.c3:122`, and returns on handle-array
    allocation failure at `src/lisp/scheduler_primitives_threads.c3:125` through
    `src/lisp/scheduler_primitives_threads.c3:131` without releasing the parsed
    work resources.
  - The thread batch path also starts all OS-thread jobs at
    `src/lisp/scheduler_primitives_threads.c3:135` through
    `src/lisp/scheduler_primitives_threads.c3:136`, then allocates the result
    array and returns OOM immediately at
    `src/lisp/scheduler_primitives_threads.c3:138` through
    `src/lisp/scheduler_primitives_threads.c3:140`; that path does not join,
    cancel, or drop the already-started thread handles before returning.
  - Existing batch tests cover successful task/thread batch execution and
    result-list cons allocation failures at
    `src/lisp/tests_scheduler_groups_batch.c3:51` through
    `src/lisp/tests_scheduler_groups_batch.c3:114` and
    `src/lisp/tests_scheduler_groups_batch.c3:235` through
    `src/lisp/tests_scheduler_groups_batch.c3:263`, but they do not force parse
    failure after earlier owned work was parsed, parsed-work scratch OOM, or the
    post-thread-start result-array OOM path.
- Impact: malformed mixed batches or allocation failure can leak shared scheduler
  payloads/custom contexts, and a thread-batch OOM after admission can leave OS
  work running after the primitive has reported failure. This violates the
  scheduler ownership model where shared/custom offload resources have a single
  explicit release path.
- Next action: add a small helper that releases a prefix or full slice of parsed
  `OffloadWork` entries, use it in task/thread batch parse and scratch
  allocation failures, and ensure the thread-batch post-admission OOM path joins
  or rolls back started handles before returning. Add allocation-fault and mixed
  valid/invalid batch regressions that assert active task/thread counts and
  shared registry state are unchanged.
- Resolution: added shared `scheduler_release_offload_work` helpers and used
  them across task/thread batch parse failures, scratch allocation failures, and
  unadmitted tail cleanup. Thread-batch post-admission result-array OOM now
  drains admitted OS work before returning.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=scheduler OMNI_TEST_SUMMARY=1` passed with
  `pass=141 fail=0`; `git diff --check` passed.

### AUDIT-148: `ffi-async-call` sends raw Omni string storage across the worker boundary

- Severity: High
- Status: Closed
- Classification: runtime FFI scheduler boundary, structural argument ownership
  fix
- Evidence:
  - The FFI/concurrency guide says scheduler and local evaluation state are
    thread-affine, and worker-thread payloads should be "sendable copies/scalars
    or explicit foreign handles" at `docs/reference/09-concurrency-ffi.md:77`
    through `docs/reference/09-concurrency-ffi.md:83`.
  - `ffi-async-call` is registered as a public variadic primitive at
    `src/lisp/eval_init_primitive_tables.c3:212` through
    `src/lisp/eval_init_primitive_tables.c3:215`.
  - `prim_ffi_async_call` allocates only an `FfiAsyncCallCtx` and then packs
    arguments directly into that context before enqueueing custom offload work at
    `src/lisp/prim_ffi_async.c3:196` through
    `src/lisp/prim_ffi_async.c3:239`.
  - The shared synchronous FFI packer stores `FFI_TYPE_STRING` arguments as
    `(void*)arg.str_chars` at `src/lisp/eval_ffi_bound_call.c3:213` through
    `src/lisp/eval_ffi_bound_call.c3:223`; the variadic packer does the same
    for string arguments at `src/lisp/eval_ffi_bound_call.c3:315` through
    `src/lisp/eval_ffi_bound_call.c3:318`.
  - The async worker callback calls `omni_ffi_call` on the worker thread using
    `ctx.arg_values` at `src/lisp/prim_ffi_async.c3:90` through
    `src/lisp/prim_ffi_async.c3:91`, so a string parameter is read by the worker
    through a pointer into Omni-owned `Value` storage rather than a copied
    payload owned by the offload context.
  - Existing async FFI tests exercise successful string arguments and copied
    string returns at
    `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:675` through
    `src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:692`, but do
    not cover lifetime stress, mutation/scope-boundary stress, or a contract
    check proving string arguments are copied into the offload context.
- Impact: async FFI currently relies on the caller's Omni string storage staying
  valid and immutable while a worker thread is executing C code. That crosses the
  scheduler boundary with an Omni-owned pointer instead of a sendable copy, which
  can become a use-after-free or cross-thread ownership violation as soon as an
  async call is delayed, run from a yielding fiber, or given a string allocated in
  a short-lived scope.
- Next action: give `FfiAsyncCallCtx` ownership of copied C strings for every
  `FFI_TYPE_STRING` fixed or variadic argument, release those copies from
  `ffi_async_call_ctx_free`, and add regressions that call `ffi-async-call` with
  temporary strings from nested scopes/fibers while forcing delayed offload
  execution.
- Resolution: `ffi-async-call` now copies fixed and variadic `String` arguments
  into the async call context before scheduling worker execution, rejects
  embedded-NUL strings at the boundary, and releases owned string copies through
  the async-context finalizer.
- Validation: `scripts/build_omni_chelpers.sh` passed;
  `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build` passed;
  `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system`
  passed with `pass=167 fail=0`.

### AUDIT-149: Effect explanation outcome scan skips scoped module open bodies

- Severity: Medium
- Status: Closed
- Classification: runtime introspection correctness, targeted AST walker repair
- Evidence:
  - `explain_effect_handler_outcome_symbol` classifies a handler clause as
    `resolve` only when `explain_expr_contains_resolve` finds an `E_RESOLVE`
    node at `src/lisp/schema_explain_effect_helpers.c3:126` through
    `src/lisp/schema_explain_effect_helpers.c3:129`.
  - `explain_expr_contains_resolve` recursively handles `E_IF`, `E_LET`,
    `E_BLOCK`, calls, `E_MATCH`, boolean forms, `E_SET`, `E_DEFINE`,
    `E_PERFORM`, `E_RESET`, and `E_SHIFT` at
    `src/lisp/schema_explain_effect_helpers.c3:67` through
    `src/lisp/schema_explain_effect_helpers.c3:123`, but has no
    `E_WITH_MODULE` branch.
  - Scoped module open is a real expression node: parser construction records
    body expressions in `ExprWithModule.body` at
    `src/lisp/parser_with_module.c3:30` through
    `src/lisp/parser_with_module.c3:49`, and runtime/JIT evaluation executes
    those body expressions in a temporary scoped environment at
    `src/lisp/jit_module_import.c3:157` through
    `src/lisp/jit_module_import.c3:180`.
  - Existing effect-explain tests cover direct resolve and abort outcomes at
    `src/lisp/tests_runtime_feature_schema_reader_groups.c3:75` through
    `src/lisp/tests_runtime_feature_schema_reader_groups.c3:78`, but do not
    cover a handler clause whose `resolve` is nested inside `(with mod ...)`.
  - Direct reproduction on the current build:
    `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib ./build/main --eval "(handle (ref (ref (explain 'effect (signal explain-with 1)) 'decision) 'outcome) (explain-with x (resolve x)))"`
    prints `resolve`, while the same clause wrapped in scoped module open,
    `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib ./build/main --eval "(handle (ref (ref (explain 'effect (signal explain-with 1)) 'decision) 'outcome) (explain-with x (with math (resolve x))))"`,
    prints `abort`.
- Impact: `explain 'effect` can report that a matching handler aborts even when
  the handler will resolve the continuation, as long as the `resolve` is nested
  in scoped-module-open code. That makes effect diagnostics and schema-reader
  regression signals untrustworthy for code using `(with module ...)`.
- Resolution: `explain_expr_contains_resolve` now traverses every
  `E_WITH_MODULE` body expression, so effect explanation outcome detection sees
  `resolve` inside scoped module open bodies.
- Validation: added `explain effect resolve outcome inside with module` in
  `src/lisp/tests_runtime_feature_schema_reader_groups.c3`; `OMNI_LISP_TEST_SLICE=schema`
  passed with `pass=43 fail=0`.

### AUDIT-150: Scoped module open disables `deduce/query` equality-demand extraction

- Severity: Medium
- Status: Closed
- Classification: runtime optimizer/performance regression, targeted AST walker
  repair
- Evidence:
  - The Deduce reference records `ephemeral-head-demand-query` as a shipped
    goal-directed read path and documents equality-demand extraction for
    `deduce/query` at `docs/reference/08-libraries.part-03.md:91` through
    `docs/reference/08-libraries.part-03.md:129`.
  - Regression coverage expects a simple equality filter
    `(lambda (row) (= (ref row 'dst) 3))` to preserve rows and record
    `last-goal-directed-read-execution-path = 'ephemeral-head-demand-query` at
    `src/lisp/tests_deduce_query_admin_surface_demand_filter_part1.c3:23`
    through `src/lisp/tests_deduce_query_admin_surface_demand_filter_part1.c3:39`.
  - The demand extractor walks `E_BLOCK`, `E_IF`, `E_LET`, `E_OR`, `E_AND`, and
    `E_CALL`, then falls through to `false` for all other expression tags at
    `src/lisp/deduce_schema_query_filter_equalities.c3:39` through
    `src/lisp/deduce_schema_query_filter_equalities.c3:241`. There is no
    `E_WITH_MODULE` branch that could inspect the scoped-open body.
  - Its helper walkers have the same gap: row-parameter detection handles many
    expression tags but falls through on `E_WITH_MODULE` at
    `src/lisp/deduce_schema_query_input_expr_scan.c3:3` through
    `src/lisp/deduce_schema_query_input_expr_scan.c3:77`, and literal
    extraction falls through on `E_WITH_MODULE` at
    `src/lisp/deduce_schema_query_literal_analysis.c3:13` through
    `src/lisp/deduce_schema_query_literal_analysis.c3:52`.
  - Direct reproduction on the current build: the plain filter
    `(lambda (row) (= (ref row 'dst) 3))` returns `(2 3
    ephemeral-head-demand-query true)`, while wrapping the same equality in
    `(with math ...)` returns `(2 3 selected-component-closure false)` using
    the same data and recursive rules.
- Impact: adding `(with module ...)` around an otherwise demand-safe query
  predicate silently disables the narrow ephemeral demand path and forces a
  broader selected-component closure refresh. This preserves result rows in the
  reproduced case, but it changes the runtime path, dirty-state behavior, and
  performance profile for local code that uses scoped module open inside
  `deduce/query` filters.
- Resolution: Deduce query demand walkers now open `E_WITH_MODULE` through a
  scoped module environment and continue equality, literal, row-ref, and
  disjunctive head-demand extraction against the tail body expression; row-param
  presence scanning traverses all scoped-open body expressions.
- Validation: added plain and selector-scoped admin-demand regressions for
  `(with math (= (ref row 'dst) ...))`; `OMNI_LISP_TEST_SLICE=deduce
  OMNI_DEDUCE_GROUP_FILTER=query OMNI_DEDUCE_QUERY_FILTER=admin-surface-demand`
  passed with `pass=58 fail=0`.

### AUDIT-151: Runtime `when`/`unless` reject documented multi-body forms while AOT lowers them

- Severity: Medium
- Status: Closed
- Classification: runtime/compiler parity regression, targeted stdlib macro
  repair
- Evidence:
  - The language macro reference defines `when` and `unless` with variadic
    bodies using `[test .. body]` patterns and expands them to `(block (splice
    body))` at `docs/reference/05-macros-modules.md:19` through
    `docs/reference/05-macros-modules.md:29`.
  - The stdlib appendix documents the public signatures as `(when test
    body...)` and `(unless test body...)` at
    `docs/reference/12-appendix-stdlib.md:11` through
    `docs/reference/12-appendix-stdlib.md:15`.
  - The AOT compiler has a dedicated variadic lowering path: `compile_call_flat`
    and `compile_call_tail_flat` route `when`/`unless` to
    `compile_when_unless_if` at `src/lisp/compiler_call_flat.c3:55` through
    `src/lisp/compiler_call_flat.c3:56` and
    `src/lisp/compiler_call_flat.c3:98` through
    `src/lisp/compiler_call_flat.c3:99`; that helper treats every argument
    after the predicate as a branch body and wraps multiple expressions in an
    `E_BLOCK` at `src/lisp/compiler_call_flat.c3:12` through
    `src/lisp/compiler_call_flat.c3:35`.
  - The compiler/runtime stdlib prelude still defines both names as ordinary
    two-argument lambdas, `(define when (lambda (test body) ...))` and
    `(define unless (lambda (test body) ...))`, at
    `src/lisp/compiler_stdlib_prelude.c3:40` through
    `src/lisp/compiler_stdlib_prelude.c3:41`.
  - Direct reproduction on the current build: `./build/main --eval "(when true
    42)"` returns `42`, but `./build/main --eval "(when true 1 2)"` fails with
    `Error at line 1, column 2: called value is Integer, not a function`.
    `./build/main --eval "(unless false 99)"` returns `99`, but
    `./build/main --eval "(unless false 1 2)"` fails with the same error.
- Impact: the public language surface and AOT compiler accept `when`/`unless`
  as multi-body control forms, but direct runtime evaluation still exposes
  them as two-argument functions. Code can therefore pass generated/AOT
  coverage while failing under `--eval`, REPL, or interpreter-driven tests when
  the documented body form contains more than one expression.
- Resolution: the AOT prelude now defines `when` and `unless` as the documented
  `[test .. body]` macros, and nested `(splice body)` template rewrites now
  splice into the surrounding emitted list instead of producing `(block (1 2))`.
  Runtime stdlib macro regressions cover returned values, sequencing, and
  skipped side effects for multi-body `when` and `unless`; compiler coverage now
  asserts variadic-body forms compile without unresolved function-call fallback.
- Validation: direct `macroexpand` now emits `(if true (block 1 2) nil)`;
  direct `--eval` for `(when true 1 2)` and `(unless false 1 2)` both returned
  `2`; `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene-stdlib-migration` passed
  with `pass=19 fail=0`; `OMNI_LISP_TEST_SLICE=compiler` passed with
  `pass=308 fail=0`.

### AUDIT-152: AOT prelude omits public stdlib macros and emits unresolved generated identifiers

- Severity: Medium
- Status: Closed
- Classification: compiler/runtime stdlib parity regression, targeted AOT
  prelude repair
- Evidence:
  - The stdlib appendix exposes `branch`, `with-defaults`, and `stream-yield` as
    public macros at `docs/reference/12-appendix-stdlib.md:11` through
    `docs/reference/12-appendix-stdlib.md:17`, with `branch` no-match semantics
    documented at `docs/reference/12-appendix-stdlib.md:19` through
    `docs/reference/12-appendix-stdlib.md:21`.
  - Runtime stdlib definitions implement these as macros in `stdlib/stdlib.lisp`:
    `branch` at `stdlib/stdlib.lisp:75` through `stdlib/stdlib.lisp:93`,
    `with-defaults` at `stdlib/stdlib.lisp:96` through
    `stdlib/stdlib.lisp:106`, and `stream-yield` at
    `stdlib/stdlib.lisp:301` through `stdlib/stdlib.lisp:306`.
  - Runtime tests cover the public macro behavior at
    `src/lisp/tests_advanced_macro_hygiene_groups.c3:30` through
    `src/lisp/tests_advanced_macro_hygiene_groups.c3:40`, including `branch`,
    `with-defaults`, and `stream-yield`.
  - The AOT compiler prelude says it is the standard library prepended to all
    compiled programs at `src/lisp/compiler_stdlib_prelude.c3:9` through
    `src/lisp/compiler_stdlib_prelude.c3:14`, but its visible definitions end
    with only ordinary two-argument `when` and `unless` definitions at
    `src/lisp/compiler_stdlib_prelude.c3:40` through
    `src/lisp/compiler_stdlib_prelude.c3:41`; it does not define `branch`,
    `with-defaults`, or `stream-yield`.
  - Direct reproduction on the current build: `./build/main --eval "(branch
    (false 1) (true 2) (_ 3))"`, `./build/main --eval "(with-defaults 42)"`,
    and `./build/main --eval "(car (checkpoint (stream-yield 9)))"` return `2`,
    `42`, and `9` respectively.
  - The same forms compile with exit code `0`, but generated C3 references
    unresolved symbols instead of expanded code: compiling `(branch (false 1)
    (true 2) (_ 3))` produces `lisp::Value* _r806 = branch;`; compiling
    `(with-defaults 42)` produces `lisp::Value* _r795 = with_defaults;`;
    compiling `(car (checkpoint (stream-yield 9)))` captures and reads
    `stream_yield` (`_closure_data_47.captured_stream_yield = stream_yield`)
    instead of lowering to `capture`.
- Impact: interpreter-visible stdlib macros can be accepted by `--compile` with a
  success message while the emitted C3 is not self-contained and will fail once
  the existing broader manifest/build blockers are cleared. This lets AOT
  generation report success for programs that use documented stdlib macros but
  have no corresponding compiled binding or macro expansion.
- Resolution: the AOT prelude now includes compiler-visible definitions for
  `branch`, `with-defaults`, and `stream-yield`, and program analysis expands
  macros in an isolated compiler sandbox before top-level/capture/lambda scans.
  The sandbox clones existing macro definitions for analysis and restores the
  interpreter macro table afterward, so compile-only macros do not leak into
  runtime state. Compiler regressions now assert `branch`, `with-defaults`, and
  `stream-yield` lower to generated `if`/`let`/`capture` code instead of
  unresolved generated identifiers.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=compiler` passed with `pass=318 fail=0`;
  `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene-stdlib-migration` passed
  with `pass=19 fail=0`; `scripts/check_status_consistency.sh`,
  `scripts/check_build_config_parity.sh`, `scripts/check_io_parity_status_map.sh`,
  and `git diff --check` passed.

### AUDIT-153: AOT prelude omits documented stdlib functions and still reports compile success

- Severity: Medium
- Status: Closed
- Classification: compiler/runtime stdlib parity regression, structural AOT
  prelude repair
- Evidence:
  - The stdlib appendix documents public higher-order and utility functions that
    are not part of the minimal AOT prelude, including `flatten`, `find`,
    `remove`, `partition`, `partial`, `default`, `delay`, `force`, and
    `stream-take` at `docs/reference/12-appendix-stdlib.md:24` through
    `docs/reference/12-appendix-stdlib.md:95`.
  - Runtime `stdlib/stdlib.lisp` defines representative missing functions:
    `default` at `stdlib/stdlib.lisp:94`, `partial` at
    `stdlib/stdlib.lisp:198`, `flatten` at `stdlib/stdlib.lisp:286`,
    `partition` at `stdlib/stdlib.lisp:289`, `remove` at
    `stdlib/stdlib.lisp:292`, `find` at `stdlib/stdlib.lisp:295`,
    `stream-take` at `stdlib/stdlib.lisp:309`, and `delay` / `force` at
    `stdlib/stdlib.lisp:312` through `stdlib/stdlib.lisp:313`.
  - Runtime tests cover part of this surface: `default` behavior is exercised at
    `src/lisp/tests_core_destructure_groups.c3:79` through
    `src/lisp/tests_core_destructure_groups.c3:84`, and `stream-take` behavior is
    exercised at `src/lisp/tests_advanced_core_unicode_groups.c3:531` through
    `src/lisp/tests_advanced_core_unicode_groups.c3:534`.
  - The AOT prelude stops after a smaller stdlib subset: after `every?`, it only
    defines `try`, `assert!`, `when`, and `unless` at
    `src/lisp/compiler_stdlib_prelude.c3:34` through
    `src/lisp/compiler_stdlib_prelude.c3:41`; there are no definitions for
    `find`, `partition`, `delay`, `force`, or the other documented runtime
    stdlib functions above.
  - Direct runtime reproduction on the current build:
    `./build/main --eval "(find (lambda (x) (> x 2)) '(1 2 3 4))"` prints `3`,
    `./build/main --eval "(partition (lambda (x) (> x 2)) '(1 2 3 4))"` prints
    `((3 4) 1 2)`, and `./build/main --eval "(force (delay (lambda () 42)))"`
    prints `42`.
  - The same forms compile with exit code `0` and a success message while
    leaving unresolved identifiers in generated C3: compiling the `find` form
    emits `lisp::Value* _r801 = find;`, and compiling the lazy form emits
    `lisp::Value* _r796 = delay;` and `lisp::Value* _r799 = force;`.
- Impact: users can write documented stdlib code that works in `--eval`, REPL,
  and interpreter tests, then receive a successful `--compile` result whose C3
  output is not self-contained. This is a broader function-surface variant of
  the macro parity issue in `AUDIT-152`, and it weakens the compiler success
  contract for ordinary stdlib programs.
- Resolution: the AOT prelude now includes the missing documented pure-Lisp
  function subset needed by the audit reproduction: `default`, `partition`,
  `find`, `stream-take`, `delay`, and `force`. Compiler parity coverage now
  checks these names are emitted as AOT prelude globals instead of unresolved
  generated identifiers; the `default` global is correctly checked as the
  compiler-mangled `_omni_default`.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=compiler` passed with `pass=318 fail=0`;
  `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene-stdlib-migration` passed
  with `pass=19 fail=0`; `scripts/check_status_consistency.sh`,
  `scripts/check_build_config_parity.sh`, `scripts/check_io_parity_status_map.sh`,
  and `git diff --check` passed.

### AUDIT-154: AOT `when`/`unless` block lowering asserts on synthetic AST allocation failure

- Severity: Medium
- Status: Closed
- Classification: compiler runtime behavior, targeted allocation hardening
- Evidence:
  - Multi-body `when` and `unless` calls are lowered by synthesizing an `E_BLOCK`
    in `Compiler.compile_when_unless_if` at
    `src/lisp/compiler_call_flat.c3:12` through
    `src/lisp/compiler_call_flat.c3:43`.
  - The multi-body branch allocates the wrapper expression with
    `self.interp.alloc_expr()` and asserts non-null at
    `src/lisp/compiler_call_flat.c3:23` through
    `src/lisp/compiler_call_flat.c3:24`.
  - It then allocates the `ExprBlock` payload and expression-pointer array with
    `ast_arena_alloc`, asserting on failure at
    `src/lisp/compiler_call_flat.c3:26` through
    `src/lisp/compiler_call_flat.c3:30`.
  - Nearby synthetic AST builders already use structured compile-error returns
    for the same class of allocation failure: `make_synthetic_lambda` calls
    `set_compile_error` and returns null at
    `src/lisp/compiler_lambda_scan_effect_wrappers.c3:10` through
    `src/lisp/compiler_lambda_scan_effect_wrappers.c3:29`, and
    `make_pair_projection_app` does the same at
    `src/lisp/compiler_lambda_scan_effect_wrappers.c3:44` through
    `src/lisp/compiler_lambda_scan_effect_wrappers.c3:49`.
  - The same lowering path is used from both normal and tail call compilation:
    `src/lisp/compiler_call_flat.c3:55` through
    `src/lisp/compiler_call_flat.c3:56` and
    `src/lisp/compiler_call_flat.c3:98` through
    `src/lisp/compiler_call_flat.c3:99`.
- Impact: AOT compilation of documented multi-body `when`/`unless` forms can
  abort the compiler process under AST arena pressure instead of reporting a
  normal compile error. This is distinct from the core runtime allocator issue
  in `AUDIT-041` because this path already lives in compiler lowering code that
  has adjacent checked-allocation conventions.
- Resolution: `Compiler.compile_when_unless_if` now uses checked synthetic
  block expression, payload, and expression-array allocations, reports stable
  compiler diagnostics through `set_compile_error`, and returns `usz.max`
  instead of asserting. Focused compiler fault-injection flags cover the three
  allocation paths.
- Validation: added compiler fail-closed regressions for expression, payload,
  and expression-array allocation failures; `LIBRARY_PATH=/home/christos/.local/lib
  c3c build` produced `build/main`, and `OMNI_LISP_TEST_SLICE=compiler` passed
  with `pass=313 fail=0`.

### AUDIT-155: Stable prepared graphs have fixed small caps that force raw fallback on medium object graphs

- Severity: Medium
- Status: Closed
- Classification: runtime performance behavior, structural memory-boundary
  architecture debt
- Evidence:
  - The active memory architecture spec says prepared stable-escape
    materialization is the preferred boundary path for TEMP graphs that must
    survive, with source metadata preparation, destination ESCAPE shells, child
    edge wiring, validation, and atomic commit at
    `docs/plans/memory-boundary-architecture-spec-2026-04-24.md:124` through
    `docs/plans/memory-boundary-architecture-spec-2026-04-24.md:136`.
  - The implementation hard-caps each prepared graph at 64 nodes and 256 edges:
    `STABLE_ESCAPE_PREPARED_MAX_NODES` and
    `STABLE_ESCAPE_PREPARED_MAX_EDGES` are fixed constants at
    `src/lisp/stable_escape_store.c3:8` through
    `src/lisp/stable_escape_store.c3:10`.
  - Registration and traversal fail closed once those limits are reached:
    `stable_escape_prepared_register_seen` returns false at
    `src/lisp/stable_escape_store.c3:133` through
    `src/lisp/stable_escape_store.c3:143`, and
    `stable_escape_prepare_graph_node` returns false when `node_count` reaches
    the node cap at `src/lisp/stable_escape_store.c3:204` through
    `src/lisp/stable_escape_store.c3:220`.
  - Normal collection shapes hit the edge cap directly: arrays larger than 256
    children return false at `src/lisp/stable_escape_store.c3:272` through
    `src/lisp/stable_escape_store.c3:299`; dictionary preparation consumes two
    child edges per live entry and returns false near the same cap at
    `src/lisp/stable_escape_store.c3:305` through
    `src/lisp/stable_escape_store.c3:337`; set preparation consumes one edge per
    member and returns false at `src/lisp/stable_escape_store.c3:343` through
    `src/lisp/stable_escape_store.c3:371`.
  - Prepare failure is not surfaced as a distinct route decision; it increments
    `g_stable_escape_store_prepare_failure_fallback_count` and falls back through
    raw publish/resolve at `src/lisp/stable_escape_store.c3:924` through
    `src/lisp/stable_escape_store.c3:934`.
  - Existing tests only prove that unsupported prepared graphs fall back through
    the raw path and increment fallback counters at
    `src/lisp/tests_memory_lifetime_boundary_groups.c3:923` through
    `src/lisp/tests_memory_lifetime_boundary_groups.c3:955`; they do not cover
    capacity-triggered fallback for arrays, dictionaries, sets, or closure envs
    just beyond the fixed caps.
- Impact: medium-sized values can silently leave the preferred stable-index /
  prepared-materialization path and lose the intended copy-debt reduction and
  passport metadata benefits. This makes memory-boundary benchmarks sensitive to
  object shape cliffs rather than ownership semantics, and it weakens the
  current TEMP/ESCAPE optimization plan without a visible user-facing diagnostic.
- Resolution: stable prepared-graph scratch storage now uses dynamically
  growing node, edge, seen-node, and seen-index buffers instead of the fixed
  64-node/256-edge arrays. Preparation transfers owned buffers into the stable
  store on success and releases partial scratch state on failure. Boundary
  regressions now cover arrays with 257 children, dictionaries with 129 entries,
  sets with 257 members, and closure capture graphs beyond the former node cap.
- Validation: bounded container validation passed with
  `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke` at `pass=256 fail=0`;
  `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked `build/main`;
  `scripts/check_status_consistency.sh`, `scripts/check_build_config_parity.sh`,
  `scripts/check_io_parity_status_map.sh`, and `git diff --check` passed.

### AUDIT-156: AOT call lowering writes through unchecked argument-temp allocation

- Severity: Medium
- Status: Closed
- Classification: compiler runtime behavior, targeted allocation hardening
- Evidence:
  - `Compiler.compile_call_arg_temps` allocates `usz[] arg_temps` with
    `mem::new_array(usz, arg_count)` and immediately writes `arg_temps[i]` in the
    loop at `src/lisp/compiler_call_arg_list_helpers.c3:10` through
    `src/lisp/compiler_call_arg_list_helpers.c3:15`.
  - `build_arg_list_from_temps` then reads the same slice while emitting the AOT
    argument list at `src/lisp/compiler_call_arg_list_helpers.c3:18` through
    `src/lisp/compiler_call_arg_list_helpers.c3:31`; it has no way to distinguish
    a successful empty slice from allocation failure because the helper does not
    return a status.
  - Normal call lowering uses the helper at
    `src/lisp/compiler_call_flat.c3:69` through
    `src/lisp/compiler_call_flat.c3:73`; tail call lowering uses it at
    `src/lisp/compiler_call_flat.c3:103` through
    `src/lisp/compiler_call_flat.c3:107`.
  - List and dictionary constructor lowering use the same helper at
    `src/lisp/compiler_call_arg_list_helpers.c3:46` through
    `src/lisp/compiler_call_arg_list_helpers.c3:48`.
  - `explain 'dispatch` lowering also uses the same helper when the thunk target
    is a call at `src/lisp/compiler_call_explain_helpers.c3:21` through
    `src/lisp/compiler_call_explain_helpers.c3:29`.
  - Comparable code treats `mem::new_array` as nullable: `kernel_copy_spirv_words`
    checks the returned array and returns `nn_oom(interp)` before copying at
    `src/lisp/prim_kernel_source.c3:177` through
    `src/lisp/prim_kernel_source.c3:189`.
  - The compiler fail-closed test surface has forced allocation controls for
    program source buffers, module tables, lambda capture tables, and lambda
    parameter tables at `src/lisp/compiler_compiler_state.c3:98` through
    `src/lisp/compiler_compiler_state.c3:110`, but there is no corresponding
    forced failure switch or regression for call-argument temp allocation.
- Impact: compiling calls with many arguments, list/dictionary literals, or
  `explain 'dispatch` forms can dereference a failed scratch allocation instead
  of returning a structured compiler error. The affected helper sits on a common
  AOT lowering path, so a single missing guard can crash or corrupt generated
  output across ordinary calls and special call-shaped forms.
- Resolution: `Compiler.compile_call_arg_temps` now returns an explicit success
  status, reports allocation and lowering failures with `set_compile_error`,
  frees partial scratch arrays, and only publishes a populated slice on success.
  Normal calls, tail calls, list/dictionary constructor lowering, and
  `explain 'dispatch` now propagate failure through `usz.max` / empty generated
  output. Forced regressions cover all affected call-shape paths.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c build` linked
  `build/main`; `OMNI_LISP_TEST_SLICE=compiler` passed with `pass=318 fail=0`;
  `scripts/check_status_consistency.sh`, `scripts/check_build_config_parity.sh`,
  `scripts/check_io_parity_status_map.sh`, and `git diff --check` passed.

### AUDIT-157: compile_to_c3_ext collapses generated-output copy failure into an empty result

- Severity: Medium
- Status: Closed
- Classification: compiler runtime behavior, targeted allocation hardening
- Evidence:
  - `compiler_own_result_slice` returns `""` for `slice.len == usz.max` and for
    a failed `mem::malloc(slice.len + 1)` at
    `src/lisp/compiler_top_level_compile_function.c3:13` through
    `src/lisp/compiler_top_level_compile_function.c3:18`; it does not set a
    compiler error, return a status, or otherwise distinguish "no data" from
    "failed to own non-empty data."
  - `compile_to_c3_ext` only populates `error_out` when
    `compiler.compile_program(source)` already returned an empty generated slice
    with `compiler.has_error` set at
    `src/lisp/compiler_top_level_compile_function.c3:41` through
    `src/lisp/compiler_top_level_compile_function.c3:48`.
  - The successful generated-output path immediately returns
    `compiler_own_result_slice(generated, interp)` at
    `src/lisp/compiler_top_level_compile_function.c3:51`, so a post-generation
    ownership allocation failure for non-empty C3 output becomes an empty result
    with no `error_out` payload.
  - The same helper copies compile-error text at
    `src/lisp/compiler_top_level_compile_function.c3:45`; if that allocation
    fails, callers lose the original compiler error and see an empty
    `compile_error` string.
  - CLI `--compile` treats `c3_code.len == 0` with an empty `compile_error` as a
    generic failure and exits at `src/entry_compile_runner.c3:38` through
    `src/entry_compile_runner.c3:44`; `--check` similarly degrades it to
    `"compiler/lowering failed"` at `src/entry_check_mode.c3:43` through
    `src/entry_check_mode.c3:49`.
  - Comparable compiler allocation paths fail closed with explicit messages:
    program-source buffer allocation calls `set_compile_error` at
    `src/lisp/compiler_program_pipeline.c3:37` through
    `src/lisp/compiler_program_pipeline.c3:54`, and compiler initialization
    records allocation failures at
    `src/lisp/compiler_compiler_initialization.c3:34` through
    `src/lisp/compiler_compiler_initialization.c3:40`.
  - The compiler fault-injection surface lists forced failures for program
    source buffers, module registries, module exports, and lambda tables at
    `src/lisp/compiler_compiler_state.c3:97` through
    `src/lisp/compiler_compiler_state.c3:110`, but there is no forced failure
    hook or regression for final generated-output ownership or error-message
    ownership.
- Impact: under memory pressure, a successful AOT compile can lose its generated
  C3 output after code generation and surface only an ambiguous empty result.
  Real compiler errors can also lose their diagnostic text at the same boundary,
  so CLI/API callers cannot distinguish post-generation OOM from ordinary
  lowering failure.
- Resolution: result ownership now returns explicit success/failure, and
  `compile_to_c3_ext` sets stable diagnostics for generated C3 output copy
  failure and compile-error-message copy failure instead of returning an
  ambiguous empty string. A fault-injection seam covers the final copy boundary.
- Validation: added compiler fail-closed regressions for generated-output copy
  failure and compile-error-message copy failure; `LIBRARY_PATH=/home/christos/.local/lib
  c3c build` produced `build/main`, and `OMNI_LISP_TEST_SLICE=compiler` passed
  with `pass=313 fail=0`.

### AUDIT-158: Shared CLI C-string length helper silently truncates arguments at 65,536 bytes

- Severity: Medium
- Status: Closed
- Classification: CLI runtime behavior, targeted boundary hardening
- Source: merged from `AUDIT_2.md` candidate `H26`, source-verified during
  canonical ledger merge.
- Evidence:
  - The shared `cstr_len` helper stops scanning at a fixed 65,536-byte cap at
    `src/entry_cli_helpers.c3:15` through `src/entry_cli_helpers.c3:18`; if the
    incoming C string is longer and still unterminated, callers receive a
    syntactically valid shorter slice instead of a truncation error.
  - `--eval` slices the source argument with this helper at
    `src/entry_eval_mode.c3:113`, so a long expression can be evaluated only up
    to the cap with no diagnostic.
  - `--compile` slices input/output paths with the same helper at
    `src/entry_compile_runner.c3:20`, `src/entry_compile_runner.c3:24`, and
    `src/entry_compile_runner.c3:47`.
  - Build path discovery and default output derivation also use the capped
    helper for `OMNI_REPO_ROOT`, `argv0`, and input path handling at
    `src/entry_build_repo_paths.c3:84` through
    `src/entry_build_repo_paths.c3:106`, and
    `src/entry_build_repo_paths.c3:133` through
    `src/entry_build_repo_paths.c3:156`.
- Impact: oversized CLI arguments, environment-provided paths, or generated
  command strings can be truncated into a different valid source/path without an
  explicit failure. This can miscompile partial `--eval` input, read/write the
  wrong file, or derive an output path from a prefix of the requested path.
- Resolution: `src/entry_cli_helpers.c3` now exposes `cstr_len_bounded`,
  `cli_cstr_len`, and `cli_cstr_slice`; all `src/entry*.c3` callers that used
  the old capped helper now check truncation before slicing or copying CLI/env
  strings. Overlong mode inputs report explicit argument-too-long diagnostics
  instead of evaluating or opening a 65,536-byte prefix.
- Validation: `rg -n '\bcstr_len\(' src/entry*.c3` shows only the new bounded
  helper family plus the unrelated REPL-project bounded helper;
  `rg -n ':\[.*cstr_len|cstr_len\([^)]*\)\]' src/entry*.c3` found no legacy
  slice patterns; `c3c -C build` passed; `c3c build` reached link and then
  failed because local `-llightning` and `-lreplxx` linker dependencies are
  missing.

### AUDIT-159: IO parity workflow references a missing status-map script

- Severity: Medium
- Status: Closed
- Classification: CI regression, targeted build-script repair
- Source: merged from `AUDIT_2.md` candidate `M56`, source-verified during
  canonical ledger merge.
- Evidence:
  - `.github/workflows/io-parity-guard.yml` includes
    `scripts/check_io_parity_status_map.sh` in its pull-request path filters at
    `.github/workflows/io-parity-guard.yml:10` through
    `.github/workflows/io-parity-guard.yml:13`.
  - The same workflow executes `bash scripts/check_io_parity_status_map.sh` at
    `.github/workflows/io-parity-guard.yml:33` through
    `.github/workflows/io-parity-guard.yml:34`.
  - `rg --files scripts | rg 'check_io_parity_status_map\.sh|io_parity|parity_status'`
    returned no matching script path during the merge audit.
- Impact: any pull request that triggers `IO Parity Guard` will fail before
  running the intended parity check, so CI reports an infrastructure error
  instead of validating IO status-map policy.
- Resolution: added `scripts/check_io_parity_status_map.sh`, matching the
  existing workflow path. The guard checks the IO fiber-required status map
  against stdlib effect/signal wrappers, raw fast-path mappings, raw primitive
  registrations, async fallback policy coverage, and IO reference documentation.
- Validation: `bash -n scripts/check_io_parity_status_map.sh`, `bash
  scripts/check_io_parity_status_map.sh`, `bash scripts/check_async_fallback_policy.sh`,
  and `scripts/check_status_consistency.sh` passed. `bash
  scripts/check_io_boundary_facade.sh` still fails on pre-existing
  `io/offload-batch`, `io/task-spawn-batch`, and `io/thread-spawn-batch`
  stdlib-effect parity gaps outside this audit item.

### AUDIT-160: helper C build ignores `$CC` while C++ paths honor `$CXX`

- Severity: Low
- Status: Closed
- Classification: build portability, targeted script repair
- Source: merged from `AUDIT_2.md` candidate `M13`, source-verified during
  canonical ledger merge.
- Evidence:
  - `compile_c_source` hardcodes `cc -O2` when compiling C helper sources at
    `scripts/build_omni_chelpers.sh:271` through
    `scripts/build_omni_chelpers.sh:283`.
  - The adjacent C++ helper builders honor `${CXX:-c++}` at
    `scripts/build_omni_chelpers.sh:299` and
    `scripts/build_omni_chelpers.sh:315`, so only the C side ignores the
    caller-selected toolchain.
- Impact: cross-compilation, hermetic toolchain, sanitizer-wrapper, and cache
  wrapper workflows that set `CC` still compile C helper objects with the host
  default `cc`, while C++ helper objects use the requested compiler. That can
  produce mixed ABI flags or bypass build instrumentation.
- Resolution: `scripts/build_omni_chelpers.sh` now invokes `"${CC:-cc}"` for C
  helper compilation, matching the existing `"${CXX:-c++}"` C++ behavior.
  `scripts/check_build_config_parity.sh` now asserts both compiler environment
  fallbacks are present and rejects a hardcoded C `cc` command.
- Validation: `bash -n scripts/build_omni_chelpers.sh
  scripts/check_build_config_parity.sh`, `scripts/check_build_config_parity.sh`,
  and `rg -n '^[[:space:]]*cc([[:space:]]|$)' scripts/build_omni_chelpers.sh`
  passed.

### AUDIT-161: Detached REPL client threads can destroy the shared scope registry

- Severity: High
- Status: Closed
- Classification: runtime lifecycle, structural thread ownership fix
- Evidence:
  - `repl_server_start_client_thread` detaches every client thread at
    `src/lisp/eval_repl_server_listeners.c3:74` through
    `src/lisp/eval_repl_server_listeners.c3:90`.
  - The client-thread entry and worker-thread entry both call
    `main::thread_registry_shutdown()` on exit at
    `src/lisp/eval_repl_server_listeners.c3:23` through
    `src/lisp/eval_repl_server_listeners.c3:30` and
    `src/lisp/eval_repl_server_worker.c3:83` through
    `src/lisp/eval_repl_server_worker.c3:86`.
  - `thread_registry_shutdown` calls `scope_freelist_cleanup` at
    `src/main_thread_registry.c3:12` through `src/main_thread_registry.c3:15`,
    and that cleanup destroys the global scope mutex at
    `src/scope_region_reset_adopt.c3:115` through
    `src/scope_region_reset_adopt.c3:142`.
- Impact: the first detached REPL thread that exits can tear down the global
  scope registry/mutex while other threads or later test cases still need it.
  This matches the repeated `scope global mutex used after shutdown cleanup`
  abort seen in async and compiler slices.
- Next action: make registry teardown process-owned, remove per-thread global
  shutdown from detached REPL workers/clients, and track or join client threads
  before server teardown.
- Resolution: detached REPL client and worker thread exits no longer call
  `thread_registry_shutdown`; process-owned REPL entry points retain the final
  registry cleanup boundary.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build
  --obj-out obj` linked `build/main`; `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib
  OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 ./build/main --test-suite
  lisp` passed with `93 passed, 0 failed`; the broader compiler slice also no
  longer aborts from this shutdown class after AUDIT-171.

### AUDIT-162: Async REPL server test returns before detached client threads finish

- Severity: Medium
- Status: Closed
- Classification: test/runtime lifecycle, targeted synchronization fix
- Evidence:
  - `run_async_repl_server_client_threads_handle_independent_streams_test`
    starts two detached client threads, exchanges responses, closes sockets, and
    returns without a join or completion barrier at
    `src/lisp/tests_runtime_async_repl_server_groups.c3:371` through
    `src/lisp/tests_runtime_async_repl_server_groups.c3:424`.
  - Those detached client threads still run their shutdown defer chain after
    the test body has returned.
- Impact: async tests can finish while background REPL work is still running,
  making later tests observe global registry shutdown or stale thread state.
- Next action: add an explicit completion handshake, or keep client handles
  joinable and wait for both client threads before the test returns.
- Resolution: the async REPL client-thread test now starts joinable client
  threads and joins both handles before returning, so the test no longer relies
  on socket close as a background-thread completion signal.
- Validation: `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib
  OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 ./build/main --test-suite
  lisp` passed with `93 passed, 0 failed`.

### AUDIT-163: Returned libuv and process file descriptors are inheritable across `exec`

- Severity: High
- Status: Closed
- Classification: native IO/security, targeted descriptor ownership fix
- Evidence:
  - TCP helpers duplicate or detach descriptors and return them without setting
    `FD_CLOEXEC` at `csrc/uv_helpers_tcp.c:73` through
    `csrc/uv_helpers_tcp.c:81`, `csrc/uv_helpers_tcp.c:151` through
    `csrc/uv_helpers_tcp.c:165`, `csrc/uv_helpers_tcp.c:215` through
    `csrc/uv_helpers_tcp.c:223`, and `csrc/uv_helpers_tcp.c:302` through
    `csrc/uv_helpers_tcp.c:313`.
  - Pipe helpers do the same at `csrc/uv_helpers_pipe.c:155` through
    `csrc/uv_helpers_pipe.c:165`, `csrc/uv_helpers_pipe.c:251` through
    `csrc/uv_helpers_pipe.c:259`, and `csrc/uv_helpers_pipe.c:314` through
    `csrc/uv_helpers_pipe.c:324`.
  - Process helpers create or duplicate stdio descriptors without close-on-exec
    at `csrc/uv_helpers_process.c:58` through `csrc/uv_helpers_process.c:64`
    and `csrc/uv_helpers_process.c:141` through
    `csrc/uv_helpers_process.c:148`.
- Impact: later child processes can inherit hidden listener, connection, pipe,
  or stdio descriptors, keeping resources alive across `exec` and leaking access
  into child processes.
- Next action: set close-on-exec on every returned descriptor using
  `dup3(..., O_CLOEXEC)` or `fcntl(F_SETFD, FD_CLOEXEC)`, and use
  `pipe2(O_CLOEXEC)` where available.
- Resolution: TCP, pipe, Unix-socket, and process helper paths now set
  `FD_CLOEXEC` on returned or internally duplicated descriptors before exposing
  them to Omni runtime code.
- Validation: `cc -fsyntax-only` passed for `csrc/uv_helpers_tcp.c`,
  `csrc/uv_helpers_pipe.c`, `csrc/uv_helpers_process.c`, and
  `tests/native/uv_helper_cloexec_test.c`; `scripts/build_omni_chelpers.sh`
  passed; `cc -O2 ... tests/native/uv_helper_cloexec_test.c ... -o
  build/uv_helper_cloexec_test && ./build/uv_helper_cloexec_test` passed.

### AUDIT-164: libffi type tags silently default to pointer ABI

- Severity: Medium
- Status: Closed
- Classification: native FFI ABI validation, targeted fail-closed fix
- Evidence:
  - The libffi type mapper in `csrc/ffi_helpers.c:25` through
    `csrc/ffi_helpers.c:41` falls back to `ffi_type_pointer` for unsupported
    tags.
  - The mapper feeds `ffi_prep_cif` / `ffi_call` call paths at
    `csrc/ffi_helpers.c:77` through `csrc/ffi_helpers.c:79`,
    `csrc/ffi_helpers.c:109` through `csrc/ffi_helpers.c:111`, and
    `csrc/ffi_helpers.c:184` through `csrc/ffi_helpers.c:188`.
- Impact: an out-of-sync or invalid `OMNI_FFI_*` tag can be marshalled as a
  pointer ABI instead of failing, causing ABI corruption or crashes at native
  call boundaries.
- Next action: make the native type mapper return null for unsupported tags and
  reject them before `ffi_prep_cif` or `ffi_call`.
- Resolution: the native libffi type mapper now returns null for unsupported
  tags, and direct, variadic, and closure allocation paths reject unsupported
  argument or return tags before libffi prep/call entry.
- Validation: `cc -fsyntax-only -I. -Ideps/src/libuv/include
  csrc/ffi_helpers.c`, `scripts/build_omni_chelpers.sh`, and
  `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib
  OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system
  OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` passed with `177 passed,
  0 failed`.

### AUDIT-165: Rejected FFI callbacks leave the native return slot indeterminate

- Severity: Medium
- Status: Closed
- Classification: native FFI callback behavior, targeted fail-closed fix
- Evidence:
  - The callback closure handler returns immediately when
    `omni_ffi_callback_try_enter` rejects a stale/tombstoned callback at
    `csrc/ffi_helpers.c:142` through `csrc/ffi_helpers.c:146`.
  - That early return does not initialize the `ret` storage for the foreign
    caller.
- Impact: a rejected callback can return garbage native data instead of a
  deterministic zero/null/default value, making stale callback failure visible
  as undefined ABI behavior.
- Next action: initialize the native return slot on early reject, using a safe
  default for the declared callback return type.
- Resolution: rejected callback entry now zeroes the native return slot using the
  prepared CIF return type size before returning to the foreign caller.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `177 passed, 0
  failed`, including callback tombstone/return-slot regressions.

### AUDIT-166: `jit_shift_impl` does not fail closed after continuation env-bind failure

- Severity: High
- Status: Closed
- Classification: runtime continuation hardening, targeted error-path fix
- Evidence:
  - `jit_shift_impl` creates a continuation at
    `src/lisp/jit_reset_shift.c3:115` through `src/lisp/jit_reset_shift.c3:127`
    and then calls `jit_env_extend` for the continuation binding at
    `src/lisp/jit_reset_shift.c3:137` through `src/lisp/jit_reset_shift.c3:138`.
  - The underlying boundary environment extension can return null on
    allocation/barrier failure at `src/lisp/eval_boundary_scope_env.c3:175`
    through `src/lisp/eval_boundary_scope_env.c3:189`.
  - The later suspend-failure branch at `src/lisp/jit_reset_shift.c3:150`
    through `src/lisp/jit_reset_shift.c3:169` clears reset state without
    discarding the already-created continuation.
- Impact: a failed continuation env bind can flow into evaluation with a null
  environment, and a suspend failure can leak a captured continuation that
  should have been discarded.
- Next action: fail closed immediately on null `shift_env`, discard the
  continuation on every post-allocation failure branch, and add forced-failure
  regressions for env-bind and suspend failure paths.
- Resolution: `jit_shift_impl` now discards allocated continuations and returns
  a structured error on continuation wrapper/env setup or suspend failure instead
  of continuing with a null environment or leaving a captured continuation linked.
- Validation: `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib
  OMNI_LISP_TEST_SLICE=jit-policy
  OMNI_JIT_POLICY_FILTER=handle-continuation-alloc-failure,signal-suspend-failure-clears-handler-state
  OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp` passed with `2 passed, 0
  failed`. The live env-bind allocation hook remained unreliable as a regression
  trigger and is recorded in negative memory; the runtime path itself now fails
  closed.

### AUDIT-167: Signal resume failure leaves handler state half-armed

- Severity: High
- Status: Closed
- Classification: runtime effect handling, targeted state rollback fix
- Evidence:
  - Signal-resume helpers set `hstate.signaled`, `signal_tag`, and
    `signal_arg` before calling `suspend_with_scope_guard` at
    `src/lisp/jit_runtime_effects_signal.c3:118` through
    `src/lisp/jit_runtime_effects_signal.c3:135` and
    `src/lisp/jit_runtime_effects_signal.c3:162` through
    `src/lisp/jit_runtime_effects_signal.c3:179`.
  - `jit_handle_signal.c3:73` through `src/lisp/jit_handle_signal.c3:75`
    documents that callers use `hstate.signaled` to decide whether to loop or
    abort.
- Impact: if suspension fails, stale signal state can remain visible to the
  handler loop and be reprocessed after the failure path should have aborted.
- Next action: clear handler signal state or route suspend-failure exits through
  a shared cleanup helper before returning the error.
- Resolution: signal resume helpers now clear `signaled`, `signal_tag`, and
  `signal_arg` before returning suspend-failure errors, so stale handler state is
  not reprocessed after an abort path.
- Validation: the focused JIT policy filter
  `signal-suspend-failure-clears-handler-state` passed as part of the `2 passed,
  0 failed` JIT policy run.

### AUDIT-168: Pattern allocation still asserts on AST-arena exhaustion

- Severity: Medium
- Status: Closed
- Classification: runtime/compiler allocation hardening, targeted OOM fix
- Evidence:
  - `Interp.alloc_pattern` hard-asserts on AST-arena exhaustion at
    `src/lisp/value_interp_alloc_helpers.c3:81` through
    `src/lisp/value_interp_alloc_helpers.c3:84`.
  - Macro conversion form builders dereference the allocated pattern directly
    at `src/lisp/macros_expr_conversion_form_builders.c3:121` through
    `src/lisp/macros_expr_conversion_form_builders.c3:123`.
- Impact: pattern synthesis during parsing or macro expansion can abort the
  process under ordinary AST allocation pressure instead of returning a
  structured Omni error.
- Next action: make pattern allocation return null on exhaustion and thread
  checked failure paths through direct parser/macro-conversion callers.
- Resolution: `Interp.alloc_pattern` now returns null on AST arena exhaustion,
  and recursive value-to-pattern conversion propagates allocation failure through
  constructor and void-literal pattern builders instead of asserting.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build
  --obj-out obj` passed; `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1
  ./build/main --test-suite lisp` passed with `326 passed, 0 failed`.

### AUDIT-169: Lost module-publication reacquire paths do not roll back before returning

- Severity: High
- Status: Closed
- Classification: runtime module transaction, targeted rollback fix
- Evidence:
  - `jit_eval_declared_module_file` returns
    `module publication lost during evaluation` after
    `reacquire_file_module_index` fails at
    `src/lisp/jit_module_import_setup.c3:56` through
    `src/lisp/jit_module_import_setup.c3:63`, without first rolling back any
    half-published record.
  - `jit_eval_implicit_module_file` has the same lost-publication return path at
    `src/lisp/jit_module_import_setup.c3:77` through
    `src/lisp/jit_module_import_setup.c3:90`.
  - Later imports treat unloaded same-path/name records as circular imports at
    `src/lisp/jit_module_import.c3:20` through
    `src/lisp/jit_module_import.c3:24` and
    `src/lisp/jit_module_import.c3:33` through
    `src/lisp/jit_module_import.c3:37`.
- Impact: rare table-growth or nested-load failure paths can still poison the
  module cache with an unloaded record even after the broader rollback fixes.
- Next action: roll back publication before every lost-publication return path
  and add a regression that forces module-table growth or reacquire failure
  during module load.
- Resolution: declared and implicit module publication paths now roll back the
  recorded publication slot before returning lost-publication errors, preventing
  unloaded cache records from poisoning later imports.
- Validation: `OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module` passed
  with `2131 passed, 0 failed`.

### AUDIT-170: FFI manifest and preload walkers skip scoped module open bodies

- Severity: Medium
- Status: Closed
- Classification: static compiler/FFI metadata, targeted traversal fix
- Evidence:
  - FFI manifest collection does not recurse into `E_WITH_MODULE` bodies at
    `src/lisp/compiler_program_top_level_ffi_manifest.c3:151` through
    `src/lisp/compiler_program_top_level_ffi_manifest.c3:182` and
    `src/lisp/compiler_program_top_level_ffi_manifest.c3:185` through
    `src/lisp/compiler_program_top_level_ffi_manifest.c3:210`.
  - The startup preload walker has the same traversal gap at
    `src/lisp/compiler_program_top_level_ffi_preload.c3:35` through
    `src/lisp/compiler_program_top_level_ffi_preload.c3:99`.
- Impact: a program whose only FFI preload/contract use lives inside
  `(with module ...)` can compile with an incomplete or empty
  `__omni_ffi_contract_json` sidecar and missing startup preload metadata.
- Next action: add `E_WITH_MODULE` descent to the FFI manifest and preload
  walkers, then cover a with-only FFI case in compiler tests.
- Resolution: FFI manifest and startup-preload walkers now recurse into scoped
  module open bodies, and the compiler regression covers FFI declarations inside
  a valid `(module ... (with ...))` body.
- Validation: `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main
  --test-suite lisp` passed with `326 passed, 0 failed`; `OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module`
  passed with `2131 passed, 0 failed`.

### AUDIT-171: Reusable AOT/compiler test shutdown destroys the process scope registry

- Severity: High
- Status: Closed
- Classification: runtime lifecycle, structural shutdown ownership fix
- Evidence:
  - `aot::aot_shutdown` called `main::thread_registry_shutdown` at
    `src/lisp/aot.c3:35` through `src/lisp/aot.c3:42`.
  - `run_compiler_tests` also called `main::thread_registry_shutdown` before the
    outer Lisp test runner destroyed its interpreter at
    `src/lisp/tests_tests.c3:556` through `src/lisp/tests_tests.c3:559`.
- Impact: in-process AOT tests or compiler-slice teardown could destroy the
  process-wide scope freelist mutex while later AOT initialization or interpreter
  destruction still needed it, causing `scope global mutex used after shutdown
  cleanup` aborts.
- Resolution: reusable `aot_init`/`aot_shutdown` now manage only the AOT
  interpreter; generated executable code uses process-owned
  `aot_process_init`/`aot_process_shutdown` for final registry cleanup; compiler
  tests no longer perform their own process registry shutdown.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build
  --obj-out obj` passed; `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib
  OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite
  lisp` passed with `326 passed, 0 failed`.

### AUDIT-172: Live REPL servers still allowed detached clients to outlive registry cleanup

- Severity: High
- Status: Closed
- Classification: runtime lifecycle, structural thread ownership fix
- Evidence:
  - Socket/TCP REPL server loops started detached clients and kept no joinable
    ownership at `src/lisp/eval_repl_server_listeners.c3:96` through
    `src/lisp/eval_repl_server_listeners.c3:102`.
  - The live socket/TCP server exit paths still deferred
    `main::thread_registry_shutdown()` at
    `src/lisp/eval_repl_server_listeners.c3:165` and
    `src/lisp/eval_repl_server_listeners.c3:231`.
- Impact: if a live server exited after accepting a client, process-wide scope
  registry cleanup could run while a detached client thread was still using the
  runtime, reintroducing the global mutex after-shutdown abort class.
- Resolution: live socket/TCP REPL servers now retain joinable client handles in
  a server-owned list and join every client before process registry shutdown.
  The detached client-start wrapper was removed from the server path.
- Validation: `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build
  --obj-out obj` passed; `OMNI_LISP_TEST_SLICE=async` passed with `93 passed, 0
  failed`.

### AUDIT-173: Value/AOT shift suspend failure leaks a captured continuation

- Severity: High
- Status: Closed
- Classification: runtime continuation hardening, targeted rollback fix
- Evidence:
  - `jit_shift_value` created and wrapped a continuation at
    `src/lisp/jit_runtime_effects_reset_shift.c3:97` through
    `src/lisp/jit_runtime_effects_reset_shift.c3:107`, but its suspend-failure
    branch did not discard the continuation before returning.
- Impact: compiled/value-based `shift` could leave a live continuation rooted
  until interpreter teardown after a suspend-guard failure.
- Resolution: the suspend-failure branch now calls
  `interp_discard_lisp_continuation` before restoring state and returning the
  structured error.
- Validation: `OMNI_LISP_TEST_SLICE=jit-policy
  OMNI_JIT_POLICY_FILTER=handle-continuation-alloc-failure,signal-suspend-failure-clears-handler-state`
  passed with `2 passed, 0 failed`.

### AUDIT-174: OS-thread cancel-start completion allocation failure can orphan a thread handle

- Severity: High
- Status: Closed
- Classification: scheduler runtime lifecycle, targeted cleanup fix
- Evidence:
  - `scheduler_try_begin_os_thread` zeroed the `OsThreadEntry` when cancel
    completion allocation failed at
    `src/lisp/scheduler_thread_task_transitions.c3:31` through
    `src/lisp/scheduler_thread_task_transitions.c3:37`, even though
    `scheduler_start_os_thread` can store a joinable `thread_handle` in the entry
    when eager detach is unavailable.
- Impact: the cancel-start race could make an in-flight thread handle
  unreachable, leaking a native handle and losing normal drop cleanup.
- Resolution: the failure path now captures any stored joinable handle, clears
  the entry under the scheduler lock, then detaches/frees the handle before
  publishing the cancellation wakeup.
- Validation: `OMNI_LISP_TEST_SLICE=scheduler` passed with `141 passed, 0
  failed`.

### AUDIT-175: Scoped module open bodies were skipped by AOT global collection

- Severity: High
- Status: Closed
- Classification: static compiler/codegen, targeted traversal fix
- Evidence:
  - Top-level global collection recursed through modules/imports/blocks but did
    not descend into `E_WITH_MODULE` bodies in
    `src/lisp/compiler_program_top_level_globals.c3:19` through
    `src/lisp/compiler_program_top_level_globals.c3:73`.
- Impact: `(with module (define x ...))` could compile to generated C3 that
  assigns to `x` without declaring the global.
- Resolution: both top-level and nested generated-global collectors now recurse
  into scoped module open bodies, with a compiler regression for a definition
  inside `(with ...)`.
- Validation: `OMNI_LISP_TEST_SLICE=compiler` passed with `328 passed, 0
  failed`.

### AUDIT-176: CLI compile FFI sidecar manifest skipped scoped module open bodies

- Severity: Medium
- Status: Closed
- Classification: static compiler/FFI metadata, targeted traversal fix
- Evidence:
  - `write_compile_ffi_manifest` helper walkers in
    `src/entry_compile_manifest.c3:59` through
    `src/entry_compile_manifest.c3:166` descended through modules and blocks but
    not `E_WITH_MODULE`.
- Impact: a source whose only FFI declarations lived under `(with ...)` could
  compile with embedded codegen metadata but omit the CLI sidecar manifest.
- Resolution: CLI sidecar manifest library/function/has-FFI walkers now recurse
  into scoped module open bodies, with a compiler regression for with-only FFI
  sidecar generation.
- Validation: `OMNI_LISP_TEST_SLICE=compiler` passed with `328 passed, 0
  failed`.

### AUDIT-177: Generated startup FFI preload discarded declaration failures

- Severity: High
- Status: Closed
- Classification: generated AOT startup behavior, targeted fail-closed fix
- Evidence:
  - Startup preload emission called `aot::ffi_declare_lib` and
    `aot::ffi_declare_fn` without checking their returned `Value*`.
- Impact: missing libraries or invalid FFI declarations could fail during
  startup preload while the generated program continued toward later, less
  actionable failures.
- Resolution: preload calls now assign to generated temps and return `1` after
  `aot_process_shutdown()` if a declaration returns an `ERROR`.
- Validation: `OMNI_LISP_TEST_SLICE=compiler` passed with `328 passed, 0
  failed`; compiler output tests assert the generated failure guard.

### AUDIT-178: Direct FFI variadic packing mapped unsupported values to null pointers

- Severity: High
- Status: Closed
- Classification: native FFI ABI validation, targeted fail-closed fix
- Evidence:
  - `ffi_pack_variadic_call_arg` defaulted unsupported variadic arguments to
    `FFI_TYPE_PTR` with a null pointer.
- Impact: unsupported Omni values in variadic FFI calls could be silently passed
  as null native pointers instead of producing a type error.
- Resolution: unsupported variadic tags now return `ffi/invalid-argument`, and
  boolean variadic packing only accepts canonical `true`/`false` symbols.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `177 passed, 0
  failed`.

### AUDIT-179: Direct FFI string packing accepted embedded NUL bytes

- Severity: Medium
- Status: Closed
- Classification: native FFI C-string boundary, targeted validation fix
- Evidence:
  - Fixed and variadic `FFI_TYPE_STRING` packing forwarded `arg.str_chars`
    directly to native call storage without checking for embedded NUL bytes.
- Impact: Omni strings containing `\0` could be truncated at the native C string
  boundary instead of failing before the call.
- Resolution: direct fixed and variadic FFI string packing now rejects embedded
  NUL bytes before native handoff.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `177 passed, 0
  failed`.

### AUDIT-180: Process/DNS C-string copies accepted embedded NUL bytes

- Severity: Medium
- Status: Closed
- Classification: native IO C-string boundary, targeted validation fix
- Evidence:
  - `copy_to_heap_c_string` copied process/DNS strings into NUL-terminated buffers
    without rejecting embedded NUL bytes.
- Impact: `process-spawn` command/argv/env strings and `dns-resolve` hostnames
  could be silently truncated before native calls.
- Resolution: process command, process string sequences, and DNS hostnames now
  reject embedded NUL bytes before heap C-string duplication.
- Validation: `OMNI_LISP_TEST_SLICE=async` passed with `93 passed, 0 failed`.

### AUDIT-181: Deduce tuple encoding collapsed unsupported values to nil

- Severity: High
- Status: Closed
- Classification: runtime data encoding, targeted fail-closed fix
- Evidence:
  - `encode_tuple` encoded unsupported value tags as tag `0` (`nil`) at the
    default branch.
- Impact: distinct unsupported fact values could alias the same LMDB key as nil,
  causing silent fact corruption or overwrites.
- Resolution: `encode_tuple` now fails on unsupported tags, and `fact!`
  pre-validates tuple values with `deduce_value_supported_by_tuple_codec` to
  return a structured `deduce/fact-unsupported-value` error.
- Validation: `OMNI_DEDUCE_GROUP_FILTER=parallel` passed with `8 passed, 0
  failed`; `OMNI_DEDUCE_GROUP_FILTER=command-surface` passed with `102 passed, 0
  failed`.

### AUDIT-182: Deduce rule-signature literal validation accepted null arrays with nonzero counts

- Severity: Medium
- Status: Closed
- Classification: runtime persistence validation, targeted fail-closed fix
- Evidence:
  - `deduce_rule_signature_literals_supported` broke out of loops when literal
    arrays were null, then returned true even when term counts were nonzero.
- Impact: malformed rule signatures with missing literal arrays could reach
  persistence slices over null arrays.
- Resolution: rule-signature literal validation now returns false when nonzero
  head/body literal counts have null literal arrays.
- Validation: `OMNI_DEDUCE_GROUP_FILTER=rule-validation` passed with `79 passed,
  0 failed`.

### AUDIT-183: Deduce time-point tuple decoding underchecked payload size

- Severity: High
- Status: Closed
- Classification: runtime persistence decode, targeted bounds fix
- Evidence:
  - `decode_tuple` tag `5` checked for only 32 remaining bytes after the tag,
    but the payload requires one kind byte plus eight `i32` fields.
- Impact: a truncated persisted time-point tuple could read one byte beyond the
  encoded tuple buffer.
- Resolution: the decoder now requires 33 remaining bytes for time-point
  payloads and rejects truncated payloads.
- Validation: `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=parallel`
  passed with `10 passed, 0 failed`.

### AUDIT-184: FFI callback Boolean returns converted true to false

- Severity: High
- Status: Closed
- Classification: native FFI callback ABI, targeted conversion fix
- Evidence:
  - `ffi_callback_value_to_c` treated `FFI_TYPE_BOOL` like integer returns and
    did not convert Omni `true`/`false` symbols.
- Impact: callbacks declared as returning `Boolean` returned C false for normal
  Omni boolean results.
- Resolution: Boolean callback returns now convert canonical `true` to C true,
  `false`/nil/non-boolean to C false, while preserving integer fallback only as
  numeric truth conversion.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `182 passed, 0
  failed`.

### AUDIT-185: FFI callback error paths left native return slots undefined

- Severity: High
- Status: Closed
- Classification: native FFI callback ABI, fail-closed deterministic return
- Evidence:
  - `ffi_callback_dispatch` returned early on callback argument or body errors
    without initializing the libffi return storage.
- Impact: foreign callers could observe indeterminate native return values after
  an Omni callback error.
- Resolution: callback dispatch now zero-initializes the typed return slot before
  argument conversion and invocation, then overwrites it on successful return.
- Validation: `OMNI_LISP_TEST_SLICE=advanced
  OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system` passed with `182 passed, 0
  failed`.

### AUDIT-186: Compile FFI sidecar manifest missed macro-generated declarations

- Severity: Medium
- Status: Closed
- Classification: compiler metadata generation, macro expansion parity fix
- Evidence:
  - `write_compile_ffi_manifest` scanned raw parsed source, so declarations
    produced by macros were invisible to sidecar manifest generation.
- Impact: AOT/compile output could depend on FFI declarations that were absent
  from the sidecar manifest.
- Resolution: compile sidecar generation now runs the compiler macro-expansion
  analysis path before scanning, and macro value-to-AST conversion now preserves
  emitted `[ffi ...]` declarations as `E_FFI_LIB`/`E_FFI_FN` nodes.
- Validation: `OMNI_LISP_TEST_SLICE=compiler
  OMNI_COMPILER_GROUP_FILTER=existing-feature` passed with `331 passed, 0
  failed`.

### AUDIT-187: No-FFI compile left stale sidecar manifests behind

- Severity: Medium
- Status: Closed
- Classification: compiler filesystem output, stale artifact cleanup
- Evidence:
  - When a compile no longer contained FFI declarations,
    `write_compile_ffi_manifest` returned success without deleting an existing
    `<output>.ffi-manifest.json`.
- Impact: stale manifests could make downstream tooling preload libraries or
  resolve symbols no longer declared by the source.
- Resolution: no-FFI manifest generation now unlinks the expected sidecar path
  through a copied NUL-terminated path.
- Validation: `OMNI_LISP_TEST_SLICE=compiler
  OMNI_COMPILER_GROUP_FILTER=existing-feature` passed with `331 passed, 0
  failed`.

### AUDIT-188: Embedded FFI manifest mapped extended ABI tags to Void

- Severity: Medium
- Status: Closed
- Classification: compiler metadata generation, ABI tag completeness
- Evidence:
  - `ffi_manifest_type_name` handled only Void, Integer, Float64,
    ForeignHandle, Boolean, and String; supported tags such as `Float32`,
    `Int32`, `UInt64`, `Buffer`, and `Struct` fell through to `Void`.
- Impact: generated embedded FFI contract metadata could misreport supported ABI
  signatures.
- Resolution: the manifest type-name table now covers the supported extended FFI
  ABI tags.
- Validation: `OMNI_LISP_TEST_SLICE=compiler
  OMNI_COMPILER_GROUP_FILTER=existing-feature` passed with `331 passed, 0
  failed`.

### AUDIT-189: OS-thread cancel completion OOM could orphan native thread handle

- Severity: High
- Status: Closed
- Classification: scheduler native-handle lifecycle, targeted cleanup fix
- Evidence:
  - In `scheduler_complete_os_thread`, cancel-completion materialization failure
    zeroed the `OsThreadEntry` without detaching/freeing an existing joinable
    native thread handle.
- Impact: rare OOM during cancel completion could leak a native thread handle.
- Resolution: the failure path now captures, detaches, and frees the joinable
  handle before publishing cancellation.
- Validation: `OMNI_LISP_TEST_SLICE=scheduler` passed with `142 passed, 0
  failed`.

### AUDIT-190: TCP REPL accept failure left stale `.omni-repl-port`

- Severity: Medium
- Status: Closed
- Classification: REPL server lifecycle, filesystem cleanup
- Evidence:
  - `repl_server_tcp` published `.omni-repl-port` before the accept loop and
    returned on accept failure without unlinking the port file.
- Impact: clients could discover a stale port file after server failure.
- Resolution: TCP REPL server startup now registers port-file unlink cleanup
  immediately after successful publication.
- Validation: `OMNI_LISP_TEST_SLICE=async` passed with `95 passed, 0 failed`.

### AUDIT-191: REPL output mutex failure fell back to unsynchronized writes

- Severity: Medium
- Status: Closed
- Classification: REPL server concurrency lifecycle, fail-closed cleanup
- Evidence:
  - `repl_server_output_write_json_doc` skipped locking when mutex init failed,
    and `repl_server_output_destroy` zeroed the struct without destroying a
    ready mutex.
- Impact: mutex-init failure could lead to unsynchronized JSON transport writes,
  and normal teardown leaked mutex resources.
- Resolution: REPL output writes now fail closed if the mutex is not ready, and
  output destruction destroys the ready mutex before clearing the struct.
- Validation: `OMNI_LISP_TEST_SLICE=async` passed with `95 passed, 0 failed`.

### AUDIT-192: Empty compile inputs left stale FFI sidecar manifests behind

- Severity: Medium
- Status: Closed
- Classification: compiler filesystem output, targeted stale-artifact cleanup
- Evidence:
  - `write_compile_ffi_manifest` returned success immediately when parsing
    yielded zero expressions, before the stale-manifest unlink path.
- Impact: empty/comment-only compiles could preserve a stale
  `<output>.ffi-manifest.json` from an earlier FFI-bearing compile.
- Resolution: manifest generation now builds the expected sidecar path before
  parsing and removes it both for zero-expression inputs and non-empty no-FFI
  programs.
- Validation: `OMNI_LISP_TEST_SLICE=compiler` passed with `335 passed, 0
  failed`.

### AUDIT-193: Nested control-flow definitions could emit undeclared AOT globals

- Severity: High
- Status: Closed
- Classification: compiler codegen correctness, structural AST walk fix
- Evidence:
  - AOT global collection only recursed through top-level containers while
    lowering can emit assignments for nested `define` forms inside supported
    forms such as `if`, `let`, `match`, calls, and handlers.
- Impact: macro-generated or nested definitions could produce generated C3 that
  assigned a global without declaring it first.
- Resolution: AOT global collection now traverses executable AST children and
  pattern guards, while still only adding actual generated global definitions.
- Validation: `OMNI_LISP_TEST_SLICE=compiler` passed with `335 passed, 0
  failed`.

### AUDIT-194: Nested FFI declarations were invisible to preload and sidecar walkers

- Severity: High
- Status: Closed
- Classification: compiler FFI metadata/preload discovery, structural AST walk
- Evidence:
  - Startup preload and compile sidecar manifest scanners descended only through
    module/block/scoped-open containers.
- Impact: supported nested forms could hide `E_FFI_LIB` and `E_FFI_FN` nodes
  from startup preload and `.ffi-manifest.json` emission.
- Resolution: preload and sidecar manifest walkers now recurse through supported
  executable expression children, including `if`, `let`, `match`, `handle`,
  calls, application, effect forms, block, module, and scoped module open.
- Validation: `OMNI_LISP_TEST_SLICE=compiler` passed with `335 passed, 0
  failed`.

### AUDIT-195: Deduce fact assertion masked LMDB read failures as missing rows

- Severity: High
- Status: Closed
- Classification: runtime persistence correctness, fail-closed storage check
- Evidence:
  - `fact!` collapsed `mdb_get(...) == MDB_SUCCESS` into a boolean `existed`
    flag, so every non-success code was treated as "not found".
- Impact: LMDB read failures could be followed by writes and commits, masking
  storage corruption or transaction errors.
- Resolution: fact assertion now distinguishes `MDB_NOTFOUND` from all other
  `mdb_get` failures and returns `deduce/fact-read-failed`, aborting the local
  write transaction when needed.
- Validation: `OMNI_LISP_TEST_SLICE=deduce` passed with `409 passed, 0 failed`.

### AUDIT-196: Deduce retract reference validation skipped unreadable sources

- Severity: High
- Status: Closed
- Classification: runtime persistence integrity, fail-closed scan validation
- Evidence:
  - Reference validation skipped source-relation DBI open failures and malformed
    scanned tuples, and did not fail on non-EOF cursor termination.
- Impact: `retract!`, relation clearing, or dropping could miss live references
  and delete still-referenced tuples.
- Resolution: reference validation now fails closed on source open, cursor open,
  tuple decode, arity mismatch, and cursor termination errors.
- Validation: `OMNI_LISP_TEST_SLICE=deduce` passed with `409 passed, 0 failed`.

### AUDIT-197: `ffi-async-call` sent Buffer arguments across worker handoff unsafely

- Severity: High
- Status: Closed
- Classification: FFI async ownership boundary, fail-closed worker handoff
- Evidence:
  - `ffi-async-call` copied `String` arguments before offload but accepted
    `Buffer` parameters through the shared packer as raw Omni-owned pointers.
- Impact: string/array-backed buffer payloads could dangle or race after the
  calling interpreter scope moved on.
- Resolution: `ffi-async-call` now rejects fixed `Buffer` parameters before
  worker handoff with a structured unsupported error.
- Validation: focused FFI system slice passed with `184 passed, 0 failed`; a
  direct runtime probe rejects async `Buffer` arguments before dispatch.

### AUDIT-198: Variadic Float32 FFI arguments used the wrong promoted ABI type

- Severity: High
- Status: Closed
- Classification: native FFI ABI correctness, variadic promotion fix
- Evidence:
  - Variadic packing stored Omni `Float32` as `FFI_TYPE_FLOAT32`, but C variadic
    calls require float arguments to be promoted by the caller.
- Impact: direct and async variadic FFI calls could mis-marshal `Float32`
  arguments on supported libffi ABIs.
- Resolution: variadic `Float32` packing now promotes to `double` storage and
  reports `FFI_TYPE_DOUBLE` to libffi.
- Validation: focused FFI system slice passed with `184 passed, 0 failed`; a
  direct `snprintf` variadic Float32 probe returned successfully.

### AUDIT-199: REPL worker mutex and condition variable were never destroyed

- Severity: Medium
- Status: Closed
- Classification: REPL server lifecycle, resource cleanup
- Evidence:
  - `ReplServerWorker` initialized a mutex and condition variable, but normal
    connection teardown only cleared queued commands and startup-failure paths
    did not destroy ready primitives.
- Impact: repeated REPL server setup/teardown leaked synchronization resources.
- Resolution: `repl_server_worker_destroy` now shuts the worker down, clears
  commands, destroys ready CV/mutex primitives, and connection teardown uses it.
- Validation: `OMNI_LISP_TEST_SLICE=async` passed with `96 passed, 0 failed`.

### AUDIT-200: Scheduler initialization had no teardown path

- Severity: Medium
- Status: Closed
- Classification: scheduler lifecycle, process resource cleanup
- Evidence:
  - `scheduler_init` allocated mutex/CV state, a libuv loop, and an async handle
    without a matching shutdown path.
- Impact: reusable process/test paths that initialized the scheduler left
  synchronization and libuv resources unreclaimed.
- Resolution: `scheduler_shutdown` now clears queued wakeups, closes the async
  handle, drains/closes the loop, destroys ready synchronization primitives,
  resets global scheduler state, and is run from process exit via `main`.
- Validation: `OMNI_LISP_TEST_SLICE=scheduler` passed with `143 passed, 0
  failed`.

### AUDIT-201: Deduce restart tests overwrote caller library paths

- Severity: Medium
- Status: Closed
- Classification: test harness/runtime validation, environment propagation fix
- Evidence:
  - Deduce durability restart tests launched child `./build/main` processes with
    `LD_LIBRARY_PATH=/usr/local/lib`, discarding the parent validation path.
- Impact: local builds using `/home/christos/.local/lib` failed restart tests
  with missing `liblightning.so.2`, hiding the actual Deduce result.
- Resolution: restart test commands now preserve the caller's `LD_LIBRARY_PATH`
  and append `/usr/local/lib`.
- Validation: `OMNI_LISP_TEST_SLICE=deduce` passed with `409 passed, 0 failed`.

### AUDIT-202: Deduce deferred commit validation skipped storage scan failures

- Severity: High
- Status: Closed
- Classification: runtime persistence integrity, fail-closed commit validation
- Evidence:
  - Deferred commit validation ignored tuple decode failures, tuple arity
    mismatches, non-EOF cursor termination, and source relation DBI open
    failures while replaying fact/reference integrity over committed snapshots.
- Impact: `deduce-commit` could report success after storage corruption or
  inconsistent relation metadata prevented complete integrity validation.
- Resolution: deferred fact and reference validation now returns
  `deduce/deferred-integrity-scan-failed` on decode, arity, source-open, and
  cursor termination failures, while intentionally dropped schemas remain
  skipped.
- Validation: `OMNI_LISP_TEST_SLICE=deduce` passed with `414 passed, 0 failed`;
  focused integrity slice passed with `39 passed, 0 failed`.

### AUDIT-203: Deduce fact reference scans collapsed cursor failures into missing-reference errors

- Severity: Medium
- Status: Closed
- Classification: runtime persistence integrity, diagnostic correctness
- Evidence:
  - Fact-path reference lookup treated any target scan termination other than a
    found row as a semantic missing-reference failure.
- Impact: LMDB cursor, decode, or arity failures could be misreported as normal
  referential-integrity misses, hiding storage faults.
- Resolution: fact reference validation now returns
  `deduce/fact-reference-integrity-scan-failed` for scan/decode/arity failures
  before falling through to `deduce/integrity-reference-missing` only on clean
  not-found scans.
- Validation: `OMNI_LISP_TEST_SLICE=deduce` passed with `414 passed, 0 failed`;
  focused integrity slice passed with `39 passed, 0 failed`.

### AUDIT-204: Embedded AOT FFI contract manifest missed nested declarations

- Severity: High
- Status: Closed
- Classification: compiler FFI metadata/preload discovery, structural AST walk
- Evidence:
  - Startup preload detection used a recursive FFI scan, but embedded
    `__omni_ffi_contract_json` emission still traversed only shallow
    module/block/scoped-open containers.
- Impact: nested `if`, `let`, `match`, `handle`, call, and effect forms could
  preload FFI at startup while publishing an incomplete or empty embedded
  contract manifest.
- Resolution: embedded contract library/function emission now recurses through
  the same executable expression families used by startup preload and sidecar
  discovery.
- Validation: `OMNI_COMPILER_GROUP_FILTER=existing-feature` compiler slice
  passed with `336 passed, 0 failed`.

### AUDIT-205: Compile FFI sidecar stale cleanup failed open

- Severity: Medium
- Status: Closed
- Classification: CLI compile artifact cleanup, fail-closed file operation
- Evidence:
  - No-FFI and empty compile paths attempted to remove stale
    `.ffi-manifest.json` sidecars but ignored unlink failures.
- Impact: compile commands could report success while leaving stale FFI metadata
  next to a newly generated output that no longer declared FFI.
- Resolution: stale sidecar cleanup now treats not-found as success but reports
  unlink failures through `write_compile_ffi_manifest` with the failed manifest
  path and an `EntryFileWriteKind`.
- Validation: `OMNI_COMPILER_GROUP_FILTER=existing-feature` compiler slice
  passed with `336 passed, 0 failed`.

### AUDIT-206: REPL clone-start failure leaked a newly created session

- Severity: High
- Status: Closed
- Classification: REPL server lifecycle, startup rollback
- Evidence:
  - `eval_repl_server_worker` created a clone session and interpreter before
    emitting the session-start event, but returned directly when event delivery
    failed.
- Impact: a failed session-start event could leave the session/interpreter
  retained after the worker command failed.
- Resolution: clone-start event failure now closes the newly created session
  before returning.
- Validation: `OMNI_LISP_TEST_SLICE=async` passed with `96 passed, 0 failed`.

### AUDIT-207: Scheduler shutdown failure was discarded at process exit

- Severity: Medium
- Status: Closed
- Classification: scheduler lifecycle, process exit status
- Evidence:
  - `main` called `scheduler_shutdown()` from a `defer` and explicitly cast the
    result to `void`.
- Impact: scheduler teardown failures were silent on otherwise successful CLI
  exits.
- Resolution: `main` now returns through `finish_main`, preserving existing
  non-zero exits while converting a successful command into exit code `1` if
  scheduler shutdown fails.
- Validation: `OMNI_LISP_TEST_SLICE=scheduler` passed with `143 passed, 0
  failed`.

### AUDIT-208: Compiler result ownership leaks on destructor-registration failure

- Severity: Medium
- Status: Closed
- Classification: compiler output ownership, scope destructor fail-closed path
- Evidence:
  - `compiler_own_result_slice` copied generated/error output into owned memory
    but ignored `scope_register_dtor` failure.
- Impact: if root-scope destructor registration failed, the owned compiler
  output buffer could leak and the caller would receive a slice without lifetime
  tracking.
- Resolution: result ownership now frees the copied buffer and returns failure
  when destructor registration fails; a compiler-local failure seam covers this
  path without perturbing normal compilation allocations.
- Validation: `OMNI_LISP_TEST_SLICE=compiler` passed with `338 passed, 0
  failed`.

### AUDIT-209: Call/app AOT lowering emitted calls after callee lowering failure

- Severity: High
- Status: Closed
- Classification: compiler AOT lowering, fail-closed code emission
- Evidence:
  - `compile_call_flat`, tail-call lowering, `compile_app_flat`, and
    `compile_app_tail_flat` used callee temporaries after `compile_to_temp`
    could have returned `usz.max`.
- Impact: a failed callee or app operand lowering could still emit
  `apply_multi`/`invoke` code with an invalid temporary reference.
- Resolution: call and app lowering now short-circuit when callee or argument
  lowering reports `has_error` or `usz.max`.
- Validation: `c3c --threads 1 build --obj-out obj` and
  `OMNI_LISP_TEST_SLICE=compiler` passed.

### AUDIT-210: Compile output commit failure left stale FFI sidecar manifests

- Severity: Medium
- Status: Closed
- Classification: CLI compile artifact cleanup, atomic output failure
- Evidence:
  - `run_compile_mode` wrote the FFI sidecar before committing the final output
    file; if the final output commit failed, the sidecar remained beside a
    missing or stale output.
- Impact: failed compile commands could leave trusted-looking
  `.ffi-manifest.json` metadata for an output that was not published.
- Resolution: output-commit failure now removes the just-written sidecar via a
  dedicated cleanup helper; compiler tests cover the helper so the CLI lifecycle
  is not invoked inside the compiler test process.
- Validation: `OMNI_LISP_TEST_SLICE=compiler` passed with `338 passed, 0
  failed`.

### AUDIT-211: `tls-connect` silently truncated overlong hostnames

- Severity: High
- Status: Closed
- Classification: TLS C-boundary string contract, fail-closed validation
- Evidence:
  - `tls_dup_string(..., 255)` truncated host strings before offload instead of
    rejecting them.
- Impact: TLS connections could be attempted against a different hostname than
  the one supplied by the program, affecting trust/session policy.
- Resolution: `tls-connect` now rejects hostnames above 255 bytes before
  offload; `tls_dup_string` also fails instead of truncating when a maximum is
  exceeded.
- Validation: `OMNI_LISP_TEST_SLICE=async` passed with `97 passed, 0 failed`.

### AUDIT-212: FTXUI event read-result clearing could free unowned memory

- Severity: High
- Status: Closed
- Classification: native FFI ABI lifecycle, ownership guard
- Evidence:
  - `omni_ftxui_event_read_result_clear` unconditionally deleted `result->text`,
    and `session_take_event` clears caller-provided output storage.
- Impact: a caller that provided ABI-sized but uninitialized result storage
  could trigger an invalid free before reading an event.
- Resolution: event read results now carry an initialization magic; `clear`
  frees text only for initialized results and normalizes size/version/fields.
- Validation: `advanced-ffi-system` passed with `185 passed, 0 failed`.

### AUDIT-213: FTXUI session event capture was unbounded

- Severity: Medium
- Status: Closed
- Classification: native UI event queue, resource bounding
- Evidence:
  - Captured FTXUI session events were appended to a deque without a capacity
    limit while reads pop one event at a time.
- Impact: prolonged input bursts could grow native memory without bound.
- Resolution: the native session queue now caps captured events at 1024 entries,
  drops the oldest event when full, and increments existing dropped-event
  accounting.
- Validation: `advanced-ffi-system` passed with `185 passed, 0 failed`.

### AUDIT-214: Schema regex validation treated regex errors as success

- Severity: High
- Status: Closed
- Classification: schema validation, structured error handling
- Evidence:
  - `schema_check_regex_clause` accepted any non-`nil` result from
    `re-fullmatch`, including `ERROR` values produced by invalid regex patterns.
- Impact: malformed schema regex clauses could validate inputs they should
  reject.
- Resolution: regex schema clauses now reject `ERROR` results.
- Validation: `OMNI_LISP_TEST_SLICE=schema` passed with `44 passed, 0 failed`.

### AUDIT-215: Pika parse-tree conversion ignored allocation failures

- Severity: High
- Status: Closed
- Classification: parser result construction, allocation fail-closed path
- Evidence:
  - `parse_tree_to_lisp_value` and `parse_tree_to_lisp` built names, matched
    text, and cons lists without checking for `ERROR`/allocation failure.
- Impact: parse APIs could return malformed trees or mask OOM failures while
  constructing parser output.
- Resolution: parse-tree conversion now checks symbol/string/cons construction
  and raises structured parser OOM errors on failure.
- Validation: `OMNI_LISP_TEST_SLICE=pika` passed with `126 passed, 0 failed`.

### AUDIT-216: Deduce tuple decoding accepted trailing bytes

- Severity: Medium
- Status: Closed
- Classification: Deduce storage codec, malformed record rejection
- Evidence:
  - `decode_tuple` stopped when the output slot array filled, even if unread
    bytes remained in the record buffer.
- Impact: malformed or extended tuple records could be accepted as valid
  prefixes by callers that supplied the expected arity.
- Resolution: tuple decoding now requires exact byte consumption and rejects
  trailing bytes.
- Validation: focused Deduce parallel group passed with `11 passed, 0 failed`.

### AUDIT-217: `InterpInputState` destroy leaked synchronization resources

- Severity: Medium
- Status: Closed
- Classification: REPL/input lifecycle, thread primitive teardown
- Evidence:
  - `io_input_state_destroy` freed the buffer and marked the state closed but
    left initialized mutex/condition-variable objects live.
- Impact: repeated REPL/session input-state creation and teardown could leak
  synchronization resources and sanitizer noise.
- Resolution: input-state destroy now broadcasts/closes, frees the buffer, then
  destroys the condition variable and mutex and clears readiness flags.
- Validation: `OMNI_LISP_TEST_SLICE=async` passed with `97 passed, 0 failed`.

### AUDIT-218: Scheduler shutdown did not reclaim shared-registry state

- Severity: Medium
- Status: Closed
- Classification: scheduler lifecycle, shared-handle registry teardown
- Evidence:
  - `scheduler_shutdown` reset `g_scheduler` without draining retired shared
    payloads, freeing active registry payloads, or destroying the registry
    mutex.
- Impact: in-process scheduler shutdown/reinit cycles could leak shared-handle
  payloads and leave stale registry synchronization state behind.
- Resolution: shutdown now drains active and retired shared payloads, destroys
  the registry mutex, clears registry state, and preserves reinitialization.
- Validation: `OMNI_LISP_TEST_SLICE=scheduler` passed with `143 passed, 0
  failed`.

### AUDIT-219: Heap-backed numeric and tensor constructors ignored destructor-registration OOM

- Severity: High
- Status: Closed
- Classification: memory lifetime, destructor registration, allocation rollback
- Evidence:
  - `make_big_integer_from_owned_handle`, `make_big_float_from_owned_handle`,
    `make_big_complex_from_owned_handle`, tensor constructors, and their
    copy/escape promotion paths shaped heap-backed `Value` payloads and ignored
    the boolean result from `scope_register_dtor` or
    `scope_register_dtor_escape`.
- Impact: under destructor-table allocation pressure, constructors could return
  live heap-backed values that would never run their payload destructor, or
  boundary copies could leak cloned payloads.
- Resolution: heap-backed `Value` destructor registration now goes through
  shared helpers that free the shaped payload on registration failure; numeric
  and tensor constructors/copy/promotion paths return structured OOM failures.
- Validation: bounded `memory-lifetime-smoke` passed with `269 passed, 0
  failed`; `c3c --threads 1 build --obj-out obj` passed.

### AUDIT-220: Compile FFI sidecar survives manifest-write failure before publish

- Severity: High
- Status: Closed
- Classification: compile pipeline, stale artifact rollback
- Evidence:
  - `write_compile_ffi_manifest` records the final sidecar path before the JSON
    body is written, while `run_compile_mode` returns on manifest-write failure
    without deleting a pre-existing final sidecar.
- Impact: a failed compile can leave an older `.ffi-manifest.json` beside the
  failed output, making downstream tooling consume stale trusted-looking
  metadata.
- Resolution: manifest write failure now removes stale final sidecars before
  returning failure, preserving the staged output contract.
- Validation: compiler slice passed with `340 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-221: Compile sidecar rollback ignores unlink failure

- Severity: Medium
- Status: Closed
- Classification: compile pipeline, stale artifact rollback
- Evidence:
  - `cleanup_compile_sidecar_on_output_commit_failure` ignores the result of
    deleting the sidecar, and `run_compile_mode` does not surface cleanup
    failure after final output commit failure.
- Impact: if the sidecar cannot be unlinked, a compile failure can still leave
  stale metadata at the trusted final path.
- Resolution: compile sidecar cleanup now uses the libuv unlink path, returns a
  visible cleanup status, and reports rollback failures instead of silently
  ignoring them.
- Validation: compiler slice passed with `340 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-222: Build backend writes the final binary directly

- Severity: Medium
- Status: Closed
- Classification: build pipeline, stale artifact rollback
- Evidence:
  - `run_aot_backend_compile` invokes `c3c -o` with the final output path, and
    `run_build` returns on backend failure without cleaning or rolling back that
    destination.
- Impact: after a failed `--build`, an old or partial binary can remain at the
  target path and look like the current build result.
- Resolution: AOT backend build now compiles to a staged temporary output and
  publishes to the final binary path only after successful backend completion.
- Validation: compiler slice passed with `340 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-223: Scheduler offload-admission teardown leaves freelist and mutex state live

- Severity: Medium
- Status: Closed
- Classification: scheduler lifecycle, shutdown cleanup
- Evidence:
  - `scheduler_offload_recycle_queued_work` maintains
    `offload_admission.queued_free_head`, and `scheduler_init_offload_admission`
    initializes `offload_admission.mu`, but `scheduler_shutdown` does not drain
    or destroy that substate.
- Impact: repeated scheduler init/shutdown cycles can leak queued work nodes and
  leave stale synchronization state.
- Resolution: scheduler shutdown now drains the offload-admission freelist,
  clears counters, destroys the admission mutex, and resets initialization
  state.
- Validation: scheduler slice passed with `143 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-224: Scheduler shutdown skips pending-I/O handle drain

- Severity: Medium
- Status: Closed
- Classification: scheduler lifecycle, libuv handle teardown
- Evidence:
  - pending TCP read/offload/sleep/DNS/connect/accept close sweeps exist in the
    run-loop/fiber paths, but `scheduler_shutdown` tries `uv_loop_close` without
    first invoking a shared pending-handle drain.
- Impact: live libuv handles can make shutdown fail early before global
  scheduler state is fully cleared.
- Resolution: pending scheduler I/O close logic is shared and invoked during
  shutdown before `uv_loop_close`.
- Validation: scheduler slice passed with `143 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-225: Scheduler shutdown does not drain thread-task and OS-thread registries

- Severity: Medium
- Status: Closed
- Classification: scheduler lifecycle, thread registry teardown
- Evidence:
  - scheduler thread-task and OS-thread registries own completions and thread
    handles, but shutdown does not walk them before destroying coordination
    primitives and zeroing `g_scheduler`.
- Impact: abandoned or concurrently completing work can strand completion
  payloads or joinable thread handles during teardown.
- Resolution: scheduler shutdown now cancels and drains active thread-task and
  OS-thread entries before destroying coordination primitives.
- Validation: scheduler slice passed with `143 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-226: Regex cache allocation failure can publish unusable cache state

- Severity: High
- Status: Closed
- Classification: parser/cache allocation failure, fail-closed behavior
- Evidence:
  - regex cache initialization updates capacity before allocation success is
    known, and the insert path can write into the cache after a failed grow.
- Impact: memory pressure during cache init/grow can leave initialized-looking
  null cache state or crash during regex compilation.
- Resolution: regex cache init/grow now allocates into temporaries, publishes
  state only after success, and falls back to uncached compiled regexes when
  cache bookkeeping allocation fails.
- Validation: Pika slice passed with `128 passed, 0 failed`; `c3c --threads 1
  build --obj-out obj` passed.

### AUDIT-227: Deduce persisted metadata read failures are discarded

- Severity: High
- Status: Closed
- Classification: Deduce persistence, materialized metadata integrity
- Evidence:
  - relation metadata restore helpers and live relation-open paths discard the
    boolean result from `deduce_relation_meta_apply_persisted_state`.
- Impact: failed metadata reads can leave default state in place while a
  relation still opens or registers, hiding persistence corruption and stale
  materialized-view status.
- Resolution: persisted metadata restore now returns structured errors, and
  live `open-named`/`define-relation` callers fail closed and free the relation
  when metadata restore fails.
- Validation: Deduce `basics` group passed with `11 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-228: Corrupt Deduce stale-reason bytes reopen as clean

- Severity: Medium
- Status: Closed
- Classification: Deduce persistence, metadata corruption handling
- Evidence:
  - persisted materialized metadata maps unknown `stale_reason` bytes to
    `DEDUCE_MATERIALIZED_STALE_NONE`.
- Impact: corrupt metadata can reopen as clean instead of being flagged as
  suspect or rejected.
- Resolution: unknown persisted stale-reason values now produce a structured
  metadata invalid-state error instead of reopening as clean.
- Validation: Deduce `basics` group passed with `11 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-229: TOML bridge coerces invalid native metadata to empty values

- Severity: Medium
- Status: Closed
- Classification: TOML parsing, native bridge invalid-state handling
- Evidence:
  - null or negative-length TOML strings and table keys become empty strings,
    and timestamp-part extraction failure becomes `nil`.
- Impact: malformed native TOML datum state can collapse distinct keys or hide
  parser corruption as valid empty/nil values.
- Resolution: TOML native bridge string, table-key, and timestamp metadata
  failures now return `parser/invalid-state`.
- Validation: data-format slice passed with `92 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-230: JSON null string storage becomes a valid empty Omni string

- Severity: Medium
- Status: Closed
- Classification: JSON parsing, native bridge invalid-state handling
- Evidence:
  - `json_val_to_omni` converts a null JSON string pointer to `make_string("")`.
- Impact: invalid parser state becomes indistinguishable from a real empty JSON
  string.
- Resolution: JSON null string storage now returns `parser/invalid-state`
  instead of a valid empty string.
- Validation: data-format slice passed with `92 passed, 0 failed`; `c3c
  --threads 1 build --obj-out obj` passed.

### AUDIT-231: Deduce materialized restart fixtures rely on block-local `define` visibility

- Severity: Medium
- Status: Open
- Classification: Deduce validation, language/script binding contract
- Evidence:
  - The broad Deduce slice currently fails six materialized restart cases with
    reader-side errors such as `unbound variable 'db-refresh'` or
    `unbound variable 'analyze'`.
  - Direct reproduction shows `(block (define db (deduce 'open "...")) db)`
    fails with `unbound variable 'db'`, so the restart fixtures are relying on
    a block-local `define` binding contract that is not currently available in
    script execution.
- Impact: `OMNI_LISP_TEST_SLICE=deduce` is not a reliable broad validation
  gate until the block/define contract is fixed or the fixtures are rewritten to
  use the supported binding form.
- Next step: decide whether `define` should bind within `block`; either
  implement that language contract and rerun the broad Deduce slice, or rewrite
  the restart scripts to use `let`/supported local binding.
- Validation: focused Deduce `basics` group for AUDIT-227/AUDIT-228 passed with
  `11 passed, 0 failed`; full Deduce slice remains red with `411 passed, 6
  failed`.

## Closed Or Invalidated Findings

- Invalidated external candidate: `AUDIT_2.md` reports missing `break`
  statements in C3 `switch` blocks as C-style fallthrough bugs for
  `src/lisp/prim_tensor_map.c3` and
  `src/lisp/prim_tensor_vulkan_map_direct.c3`. This is not a valid finding:
  `c3_cheatsheet.md:79` records C3 switch semantics as having no implicit
  fallthrough.
- Invalidated external candidate class from `AUDIT_2.md`: all additional
  "missing break" C3 switch claims in parser/JIT/compiler/scheduler/destructor
  paths were treated as stale candidate material for the same reason. C3 cases
  do not implicitly fall through, so these claims must not be promoted unless a
  separate non-fallthrough bug is proven with source/runtime evidence.
- Deduped external candidate class from `AUDIT_2.md`: candidates already covered
  by canonical ledger entries, including Vulkan shared-context lifetime,
  baseline metadata policy, Docker validation extra-args wiring, memory-lifetime
  bench gate wiring, FFI callback lifetime races, scope allocation/destructor
  OOM contracts, HTTP header underflow, module import path handling, async read
  caps, and non-exhaustive AOT/runtime parity issues, were not re-added under
  new IDs.
- Invalidated external candidate: CUDA complex matrix diagonal kernels are not
  currently proven out-of-bounds for rectangular matrices. The Lisp wrapper
  passes rows/cols through at
  `src/lisp/prim_tensor_matrix_lu_cuda_backend.c3:81` through
  `src/lisp/prim_tensor_matrix_lu_cuda_backend.c3:84`, but the native wrappers
  compute `diagonal_count = rows < cols ? rows : cols` before launch at
  `csrc/tensor_cuda_helpers_complex_matrix.inc:254` and
  `csrc/tensor_cuda_helpers_complex_matrix.inc:289`.
- Invalidated external candidate: `compile_expr` does not silently turn
  unsupported expressions into nil without recording an error. The default path
  calls `set_unsupported_expr_error` before emitting `aot::make_nil()` at
  `src/lisp/compiler_expression_compilation.c3:36` through
  `src/lisp/compiler_expression_compilation.c3:38`.

## Audit Notes

- Existing uncommitted `AUDIT_REPORT_2026-04-24.md` contains additional prose
  and false-positive notes. This ledger keeps only findings that were verified
  against the live workspace during this pass.
- The workspace already had uncommitted changes before this ledger was created:
  `AUDIT_REPORT_2026-04-24.md` and
  `memory/changelog_parts/changelog_part_37.md`. They were not modified by this
  audit pass.
