## 2026-03-07: Session 258 - Track C3 Checksum Primitives (`adler32`, `crc32`)

### Summary
Completed the remaining checksum primitive gap in Track C by adding `adler32`
and `crc32` on top of libdeflate, with known-value regression tests and
documentation/plan parity updates.

### What changed
- libdeflate checksum bindings + primitives
  - `src/lisp/compress.c3`
    - added extern bindings:
      - `libdeflate_adler32`
      - `libdeflate_crc32`
    - added primitives:
      - `(adler32 s)` (Adler-32, seed `1`)
      - `(crc32 s)` (CRC-32, seed `0`)
- primitive registration
  - `src/lisp/eval_init_primitives.c3`
    - registered:
      - `adler32` (`1`)
      - `crc32` (`1`)
- regression coverage
  - `src/lisp/tests_tests.c3`
    - added checksum known-value tests:
      - `adler32 ""` -> `1`
      - `crc32 ""` -> `0`
      - `adler32 "hello"` -> `103547413`
      - `crc32 "hello"` -> `907060870`
    - added argument validation tests for non-string inputs.
- docs + parity
  - `docs/reference/11-appendix-primitives.md`
    - added `adler32` and `crc32`.
  - `docs/reference/08-libraries.md`
    - added checksum examples.
  - `docs/CORE_LIBS_INSPECTION.md`
    - updated libdeflate section to include checksum primitives.
  - `docs/plans/library-gaps-todo.md`
    - marked `Track C / Add checksum primitives` complete.

### Validation
- Build + full tests:
  - `c3c build`
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1391/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 257 - Track C1/C2/C4 libdeflate Expansion + A5 Closure

### Summary
Closed the remaining Track A5 migration item and implemented the next two
Track C libdeflate gaps: optional compression levels for `gzip`/`deflate` and
new zlib format primitives (`zlib-compress`, `zlib-decompress`), with
regression coverage and reference parity updates.

### What changed
- libdeflate runtime primitives
  - `src/lisp/compress.c3`
    - added shared optional-level parser (`1-2` args, level range `0..12`).
    - updated `gzip` / `deflate` to accept optional level argument.
    - added zlib extern bindings:
      - `libdeflate_zlib_compress`
      - `libdeflate_zlib_compress_bound`
      - `libdeflate_zlib_decompress`
    - added language primitives:
      - `(zlib-compress s [level])`
      - `(zlib-decompress s [original-size])`
- primitive registration and arity
  - `src/lisp/eval_init_primitives.c3`
    - changed:
      - `gzip` arity `1 -> -1`
      - `deflate` arity `1 -> -1`
    - registered:
      - `zlib-compress` (`-1`)
      - `zlib-decompress` (`-1`)
- regression tests
  - `src/lisp/tests_tests.c3`
    - added round-trip tests for:
      - `gzip` level override
      - `deflate` level override
      - `zlib-compress`/`zlib-decompress` (default + explicit level)
    - added error-path tests for:
      - non-int compression level
      - out-of-range level
      - gzip too-many-args
      - corrupt zlib payload
      - corrupt gzip payload
      - corrupt deflate payload
- docs and plan parity
  - `docs/plans/library-gaps-todo.md`
    - marked A5 migration line complete (no backend swap + existing
      migration coverage retained).
    - marked Track C items complete:
      - compression levels for `gzip` / `deflate`
      - zlib format support
      - round-trip + corrupt payload regression coverage
  - `docs/reference/11-appendix-primitives.md`
    - updated compression signatures and added zlib primitives.
  - `docs/reference/08-libraries.md`
    - added examples for optional compression levels and zlib round-trip.

### Validation
- Build:
  - `c3c build` succeeded.
- Full test run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1385/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 256 - B6 TLS Integration Matrix Stabilization

### Summary
Completed the remaining Track B TLS integration test matrix item by fixing
runtime/wrapper parity bugs and test expression parse issues that caused the new
integration tests to fail under the unified test harness.

### What changed
- Fast-path primitive arity parity
  - `src/lisp/eval_init_primitives.c3`
    - changed `__raw-tls-server-wrap` registration arity from `3` to `-1`.
    - rationale: effect fast-path dispatch for wrapper payloads passes packed
      cons args; fixed-arity registration produced partial application instead
      of invoking `prim_tls_server_wrap` with packed args.
- TLS test server script correctness
  - `tests/lib/tls/server_once.omni`
    - replaced invalid `lambda (_) ...` parameter names with `lambda (ignored) ...`.
    - avoids parser errors in spawned child Omni process.
- TLS integration test expression hardening
  - `src/lisp/tests_tests.c3`
    - replaced invalid `lambda (_) ...` with `lambda (ignored) ...` in all new
      TLS process-based tests.
    - wrapped multi-step `try-message` lambda bodies in explicit `(begin ...)`.
    - corrected process wait assertions to read process dict exit status via:
      `(ref status 'exit-code)`.
    - increased child server startup wait from `20` ms to `60` ms for stability.
    - fixed parser-incompatible boolean forms by rewriting n-ary forms into
      binary nested forms:
      - `and` -> nested binary `and`
      - `or` -> nested binary `or`

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1375/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 255 - B5 Optional TLS Session Resumption Policy

### Summary
Completed Track B item 5 by adding an optional `resume-session?` policy input
to `tls-connect` and wiring a real in-process BearSSL client session cache
keyed by hostname.

### What changed
- BearSSL client session cache helpers (C helper)
  - `csrc/tls_helpers.c`
    - added process-local hostname cache for `br_ssl_session_parameters`.
    - added exported cache APIs:
      - `omni_tls_client_session_cache_load(client_ctx, hostname)`
      - `omni_tls_client_session_cache_store(client_ctx, hostname)`
    - cache access is mutex-protected for safety across runtime threads.
- TLS runtime surface
  - `src/lisp/tls.c3`
    - `tls-connect` now accepts optional final `resume-session?` boolean:
      - `true` => attempt hostname session resumption.
      - `false` => disable resumption (default behavior).
    - expanded argument forms so `tls-connect` supports packed/direct calls:
      - `(tls-connect tcp host [ca] [client-cert client-key] [resume?])`
      - direct mTLS without CA is now accepted (`tcp host cert key`).
    - added strict policy validation:
      - `resume-session?` must be symbol `true` or `false`.
    - `TlsHandle` now stores copied session hostname + resume policy bit.
    - close/finalizer paths now store session parameters back to cache when
      resumption is enabled.
    - reset path loads cached session parameters before `br_ssl_client_reset`.
- Regression coverage
  - `src/lisp/tests_tests.c3`
    - added async-suite validation tests:
      - `tls-connect rejects non-bool resume policy`
      - `tls-connect accepts bool-only resume policy form`
      - `tls-connect accepts direct mTLS form without CA`
      - `tls-connect accepts ca+resume policy form`
- Documentation / plan parity
  - `docs/reference/07-io-networking.md`
    - documented optional `resume-session?` argument and session cache behavior.
  - `docs/CORE_LIBS_INSPECTION.md`
    - BearSSL section now notes optional session resumption policy support.
  - `docs/plans/library-gaps-todo.md`
    - marked `Track B / Add optional session resumption policy` complete.

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1365/0`
    - `compiler`: `76/0`
- ASAN build:
  - `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1370/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 254 - B4 Optional mTLS Inputs on `tls-connect`

### Summary
Completed Track B item 4 by adding optional client certificate/key inputs for
`tls-connect` so Omni can present a local client identity for mTLS endpoints.

### What changed
- TLS runtime/client auth wiring
  - `src/lisp/tls.c3`
    - `tls-connect` argument handling supports optional client credential paths.
    - loads PEM client cert chain + PEM RSA private key using existing helper.
    - configures BearSSL client auth via:
      - `omni_tls_client_set_single_rsa(...)`
  - `csrc/tls_helpers.c`
    - added helper:
      - `omni_tls_client_set_single_rsa(client_ctx, creds_handle)`
- Primitive/stdlib migration
  - existing variadic `__raw-tls-connect` path preserves old call sites while
    enabling optional mTLS arguments.
- Regression coverage
  - `src/lisp/tests_tests.c3`
    - added validation tests:
      - `tls-connect rejects non-string client cert path`
      - `tls-connect rejects non-string client key path`
- Documentation / plan parity
  - `docs/plans/library-gaps-todo.md`
    - marked `Track B / Add optional mutual TLS inputs (client cert/key)` complete.

## 2026-03-07: Session 253 - B3 Server-Side TLS API (`tls-server-wrap`)

### Summary
Completed Track B item 3 by adding a server-side TLS wrapping primitive and
stdlib surface that can wrap accepted TCP stream handles using a PEM cert chain
and PEM RSA private key.

### What changed
- Server credential loading (C helper)
  - `csrc/tls_helpers.c`
    - added server credential loader utilities:
      - `omni_tls_server_context_size()`
      - `omni_tls_server_creds_load(cert_pem_path, key_pem_path)`
      - `omni_tls_server_creds_chain(...)`
      - `omni_tls_server_creds_chain_len(...)`
      - `omni_tls_server_creds_rsa_key(...)`
      - `omni_tls_server_creds_free(...)`
    - implemented PEM parsing for certificate chain and RSA private key decode
      via BearSSL PEM/X509 key decoders.
    - added deterministic cleanup for loaded chain/key allocations.
- Runtime TLS primitive
  - `src/lisp/tls.c3`
    - added `prim_tls_server_wrap(...)`:
      - `(tls-server-wrap tcp-handle cert-pem-path key-pem-path)`
    - server path now:
      - loads server creds from helper,
      - initializes `br_ssl_server_context` with `br_ssl_server_init_full_rsa`,
      - configures BearSSL I/O buffer and resets server handshake state,
      - binds to existing `TlsHandle` lifecycle/close behavior.
    - `TlsHandle` now tracks `server_creds` and frees them in teardown.
    - `tls-connect` and `tls-server-wrap` now both accept packed or direct arg
      shape from effect dispatch.
- Primitive/effect/stdlib wiring
  - `src/lisp/eval_init_primitives.c3`
    - registered `__raw-tls-server-wrap` (arity 3).
    - added fast-path mapping:
      - `io/tls-server-wrap -> __raw-tls-server-wrap`
    - fast-path count updated: `47 -> 48`.
  - `stdlib/stdlib.lisp`
    - added effect declaration:
      - `io/tls-server-wrap`
    - added stdlib wrapper:
      - `tls-server-wrap`.
- Regression coverage
  - `src/lisp/tests_tests.c3`
    - added async-suite validation tests:
      - `tls-connect rejects non-string ca bundle path`
      - `tls-server-wrap rejects non-string cert path`
      - `tls-server-wrap rejects non-stream handle`
- Documentation / plan parity
  - `docs/reference/07-io-networking.md`
    - added `tls-server-wrap` usage snippet and current RSA-key note.
  - `docs/reference/11-appendix-primitives.md`
    - added `__raw-tls-server-wrap` row.
  - `docs/reference/12-appendix-stdlib.md`
    - added `tls-server-wrap` wrapper listing.
  - `docs/plans/library-gaps-todo.md`
    - marked `Track B / Add server-side TLS` complete.

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1364/0`
    - `compiler`: `76/0`
- ASAN build:
  - `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1364/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 252 - B2 Explicit CA Bundle Option for `tls-connect`

### Summary
Completed Track B item 2 by adding an explicit CA bundle/PEM path option on
`tls-connect` while preserving default trust-store behavior.

### What changed
- TLS primitive API extension
  - `src/lisp/tls.c3`
    - `prim_tls_connect` now accepts:
      - `(tls-connect tcp-handle hostname)`
      - `(tls-connect tcp-handle hostname ca-bundle-path)`
    - added type/arity validation for optional `ca-bundle-path` string.
    - when provided, runtime loads trust anchors explicitly from the provided
      PEM bundle via `omni_tls_trust_store_load_file(...)`.
    - default path behavior remains unchanged (`OMNI_TLS_CA_FILE`,
      `SSL_CERT_FILE`, system bundle fallback).
- Primitive registration update
  - `src/lisp/eval_init_primitives.c3`
    - `__raw-tls-connect` arity updated from fixed `2` to `variadic` (`-1`)
      for optional CA bundle argument.
- Stdlib wrapper update
  - `stdlib/stdlib.lisp`
    - `tls-connect` wrapper now forwards optional trailing arguments so callers
      can provide explicit CA bundle path from language surface.
- Documentation / plan parity
  - `docs/reference/07-io-networking.md`
    - TLS section now documents optional third argument for CA bundle path.
  - `docs/reference/11-appendix-primitives.md`
    - corrected TLS primitive arities (`__raw-tls-connect` variadic,
      `__raw-tls-read` variadic, `__raw-tls-write` arity 2).
  - `docs/plans/library-gaps-todo.md`
    - marked `Track B / Add explicit CA bundle/PEM options` complete.

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1361/0`
    - `compiler`: `76/0`
- ASAN build:
  - `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1361/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 251 - B1 TLS Trust Store + Certificate Verification (`tls-connect`)

### Summary
Completed Track B item 1 by wiring BearSSL client initialization to real CA
trust anchors and enabling certificate verification for runtime TLS client
paths.

### What changed
- Runtime trust-store loader (BearSSL trust anchors from PEM bundle)
  - `csrc/tls_helpers.c`
    - expanded TLS helper surface from socket callbacks to include:
      - `omni_tls_trust_store_load_file(path)`
      - `omni_tls_trust_store_load_default()`
      - `omni_tls_trust_store_get_anchors(store)`
      - `omni_tls_trust_store_get_anchor_count(store)`
      - `omni_tls_trust_store_free(store)`
    - implemented PEM CA bundle parsing with BearSSL decoders and conversion
      into `br_x509_trust_anchor` arrays.
    - default CA resolution order:
      - `OMNI_TLS_CA_FILE`
      - `SSL_CERT_FILE`
      - common system bundle paths (`/etc/ssl/certs/ca-certificates.crt`, etc.).
- `tls-connect` trust-anchor enforcement
  - `src/lisp/tls.c3`
    - `TlsHandle` now owns a trust-store handle for connection lifetime.
    - `prim_tls_connect` now:
      - loads default trust store,
      - requires non-empty anchor set,
      - passes anchors to `br_ssl_client_init_full(...)`,
      - validates `br_ssl_client_reset(...)` success before returning handle.
    - teardown now frees trust-store allocations alongside BearSSL buffers.
- HTTPS offload parity
  - `src/lisp/scheduler_offload_worker.c3`
    - internal HTTPS (`offload 'http-get`) now initializes BearSSL with loaded
      trust anchors (instead of `null,0`).
    - added trust-store cleanup in worker path.
- Documentation / plan parity
  - `docs/plans/library-gaps-todo.md`
    - marked `Track B / Add certificate verification and trust store support in tls-connect` complete.
  - `docs/reference/07-io-networking.md`
    - documented TLS trust-store resolution order for `tls-connect`.
  - `docs/CORE_LIBS_INSPECTION.md`
    - updated BearSSL status note from “skips cert verification” to verified
      trust-store-backed behavior.

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1361/0`
    - `compiler`: `76/0`
- ASAN build:
  - `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1361/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 250 - A6 Hardening Verification + Boundary Script Pass

### Summary
Completed A6 hardening verification by confirming existing scheduler stress
coverage, running the required build/test matrix, and passing the boundary
hardening profile.

### What changed
- Plan/status normalization
  - `docs/plans/library-gaps-todo.md`
    - marked `A6` checklist complete:
      - stress coverage,
      - repeated-run stability verification,
      - required command matrix execution.
    - marked completion gate:
      - `ASAN + full test suite + boundary hardening script pass`.
- Stress coverage parity audit (existing tests already present)
  - `src/lisp/tests_tests.c3`
    - wakeup ordering/interleave stress:
      - `run_scheduler_wakeup_offload_interleave_tests`
    - duplicate ready-event handling stress:
      - `run_scheduler_duplicate_offload_ready_boundary_tests`
    - cancellation/offload teardown stress:
      - `run_scheduler_offload_cancel_boundary_tests`
    - additional wakeup/offload error + boundary restore stress:
      - `run_scheduler_wakeup_offload_error_boundary_tests`

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1361/0`
    - `compiler`: `76/0`
- ASAN build:
  - `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1360/0`
    - `compiler`: `76/0`
- Boundary hardening profile:
  - `scripts/run_boundary_hardening.sh` succeeded (`RC=0`).
  - Summary artifacts:
    - `build/boundary_hardening_summary.json`
    - `build/boundary_hardening_normal.log`
    - `build/boundary_hardening_asan.log`
  - Additional harness summary:
    - `OMNI_TEST_SUMMARY suite=stack_affinity_harness pass=1 fail=0`

## 2026-03-07: Session 249 - A5 Backend Policy Decision (Retain Worker Queue)

### Summary
Completed A5 backend-policy decision by selecting the existing offload worker
queue as the current runtime backend for CPU workloads.

### Decision
- Keep existing worker queue backend for `offload` / `thread-spawn` in this
  cycle.
- Defer potential `uv_queue_work` migration until after A6 hardening/stress
  data confirms scheduler/wakeup stability goals.

### What changed
- Plan policy finalized
  - `docs/plans/library-gaps-todo.md`
    - marked `A5 / Decide backend policy` complete.
    - documented explicit decision + rationale in-line.
- Reference documentation updated
  - `docs/reference/09-concurrency-ffi.md`
    - added backend-policy note clarifying current worker-queue backend and
      no `uv_queue_work` switch yet.

### Validation
- Documentation-only change (no runtime/code-path modification).

## 2026-03-07: Session 248 - A5 CPU-Bound Offload Policy Enforcement (Public Surface)

### Summary
Completed the first A5 item by enforcing CPU-bound policy on public
`offload`/`thread-spawn` surfaces while preserving internal runtime I/O offload
paths needed by current fiber fallback behavior.

### What changed
- Public offload policy gate
  - `src/lisp/scheduler_state_offload.c3`
    - extended `scheduler_build_offload_work(..., allow_internal_io_ops)` with
      policy-aware parsing.
    - public policy now rejects internal runtime I/O ops:
      - `accept-fd`
      - `tcp-connect`
      - `http-get`
    - added explicit error code:
      - `scheduler/offload-op-cpu-only`
- Primitive routing split (public vs internal)
  - `src/lisp/scheduler_primitives.c3`
    - added shared execution helper:
      - `scheduler_execute_offload_job(job, allow_internal_io_ops, interp)`
    - added internal runtime entrypoint:
      - `scheduler_run_internal_io_offload_job(job, interp)`
    - `prim_offload` now enforces public CPU-bound policy (`allow_internal_io_ops=false`).
    - `prim_thread_spawn` parsing now enforces public CPU-bound policy.
- Internal runtime call-site migration
  - `src/lisp/async.c3`
    - fiber fallback in `tcp-connect` and `tcp-accept` now calls
      `scheduler_run_internal_io_offload_job(...)` instead of public `prim_offload`.
  - `src/lisp/http.c3`
    - fiber path in `http-get` now calls
      `scheduler_run_internal_io_offload_job(...)` instead of public `prim_offload`.
- Regression coverage
  - `src/lisp/tests_tests.c3`
    - added:
      - `offload rejects internal io op`
      - `thread-spawn rejects internal io op`
  - `src/lisp/tests_advanced_tests.c3`
    - added payload code assertion:
      - `scheduler offload cpu-only payload code` -> `scheduler/offload-op-cpu-only`
- Documentation and plan parity
  - `docs/reference/09-concurrency-ffi.md`
    - corrected offload/thread-spawn examples to actual CPU-bound usage (`gzip`/`deflate`/`sleep-ms`).
    - documented internal-only runtime I/O offload ops.
  - `docs/reference/07-io-networking.md`
    - clarified `tcp-accept` fiber offload uses internal runtime offload path.
  - `docs/plans/library-gaps-todo.md`
    - marked `A5 / Keep offload for CPU-bound work` complete.

### Validation
- Build:
  - `c3c build` succeeded.
- Non-ASAN full run:
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1361/0`
    - `compiler`: `76/0`
- ASAN build:
  - `c3c build --sanitize=address` succeeded.
- ASAN full run:
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `20/0`
    - `scope_region`: `51/0`
    - `unified`: `1360/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 247 - A4 Signal Primitive Surface (`signal-handle` / `signal-unhandle`)

### Summary
Completed A4 signal integration by adding libuv-backed signal watcher
primitives and callback delivery wiring in runtime evaluation/scheduler loops.

### What changed
- libuv signal wrapper substrate
  - `csrc/uv_helpers.c`
    - added signal wrapper API:
      - `omni_uv_signal_start(signum, out_watch)`
      - `omni_uv_signal_poll_pending(signal_watch)`
      - `omni_uv_signal_stop(signal_watch)`
    - each watcher owns an internal libuv loop + `uv_signal_t` and tracks
      pending deliveries.
- Runtime signal primitives + callback dispatch
  - `src/lisp/async.c3`
    - added `SignalHandle` FFI wrapper type with deterministic close/finalizer.
    - added primitives:
      - `(signal-handle sig callback)`
      - `(signal-unhandle handle)`
    - callback value is promoted to root ownership for cross-boundary safety.
    - added pending-signal dispatch path:
      - `dispatch_pending_signal_callbacks(interp)`
      - callback invocation via `jit_apply_value(callback, signum)`.
    - `signal-unhandle` drains pending deliveries before teardown.
- Runtime loop integration
  - `src/lisp/eval_run_pipeline.c3`
    - dispatches pending signal callbacks at run/program boundaries.
  - `src/lisp/scheduler_primitives.c3`
    - dispatches pending signal callbacks during scheduler rounds.
- Primitive/effect registration
  - `src/lisp/eval_init_primitives.c3`
    - registered:
      - `__raw-signal-handle`
      - `__raw-signal-unhandle`
    - added effect fast-path mappings:
      - `io/signal-handle -> __raw-signal-handle`
      - `io/signal-unhandle -> __raw-signal-unhandle`
    - fast-path count updated: `45 -> 47`.
- Stdlib wiring
  - `stdlib/stdlib.lisp`
    - added effect declarations:
      - `io/signal-handle`, `io/signal-unhandle`
    - added wrappers:
      - `signal-handle`, `signal-unhandle`
- Regression coverage
  - `src/lisp/tests_advanced_tests.c3`
    - added:
      - `signal handle/unhandle callback`
    - test sends `SIGUSR1` to parent process via shell, validates callback side
      effect, and confirms clean unhandle.
- Documentation parity
  - `docs/reference/07-io-networking.md`
