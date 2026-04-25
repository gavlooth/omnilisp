## 2026-04-25 06:35 CEST - Audit Closure Integration Wave

- Objective attempted:
  - Continue addressing open `AUDIT.md` findings with parallel workers while
    keeping one parent integration owner for verification and ledger updates.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `AUDIT.md`
  - AOT bridge/codegen, macro conversion, HTTP response caps, Deduce numeric
    consumers, JIT scoped-module-open scans, NN checkpoint validation, and stack
    clone relocation.
- Code or configuration changes made:
  - Closed verified audit entries in `AUDIT.md`: `AUDIT-015`, `AUDIT-019`,
    `AUDIT-032`, `AUDIT-044`, `AUDIT-090`, `AUDIT-091`, `AUDIT-092`,
    `AUDIT-093`, `AUDIT-094`, and `AUDIT-111`.
  - Integrated AOT `define_var` fail-closed behavior and extended generated
    mutable-capture/import lowering so returned define errors are observed.
  - Integrated macro value/expression conversion coverage for the audited
    parser special-form set and fixed the missing `std::collections::list`
    import in `macros_expr_conversion_special_forms.c3`.
  - Preserved worker changes for Deduce BigInteger/BigFloat numeric consumers,
    HTTP oversized response rejection, NN checkpoint hardening, JIT scoped open
    scans, and stack clone signed address deltas.
- Commands run:
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build`
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `OMNI_LISP_TEST_SLICE=http OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `OMNI_TEST_SUMMARY=1 ./build/main --test-suite stack`
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=read-failures OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=aggregate OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene-special-form-conversion OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check` for the touched integration files.
- Key results:
  - Fresh `--obj-out obj` build passed after cleaning generated object output
    that had been mixed by concurrent builds.
  - HTTP slice passed with `pass=34 fail=0`.
  - Stack suite passed with `pass=25 fail=0`.
  - Advanced collections/module checkpoint slice passed with `pass=2131 fail=0`.
  - Deduce read-failures filter passed with `pass=8 fail=0`; Deduce aggregate
    filter passed with `pass=226 fail=0`.
  - JIT policy slice no longer reports the scoped-module-open continuation
    scan regression; it still has five unrelated pre-existing failures.
  - Macro special-form conversion filter passed with `pass=9 fail=0`.
- Invalidated assumptions or failed approaches:
  - `[FAILED]` Treating `AUDIT-015` as closed after only changing
    `aot::define_var` was insufficient. Generated mutable-capture/import call
    sites still discarded returned errors, so the integration pass extended
    codegen before closing the audit item.
  - `[FAILED]` Broad `advanced-macro-hygiene` is not a valid closure gate for
    `AUDIT-019` right now: it aborts in the unrelated
    `advanced-macro-hygiene-string-number` non-tail recursion subgroup.
- Current best recommendation or checkpoint:
  - Use focused filters for the newly closed items until the unrelated broad
    compiler/macro/JIT lifecycle failures are handled. Do not reopen the closed
    items solely because those broader slices remain red for unrelated reasons.
- Unresolved issues:
  - Broad compiler slice aborts with `scope global mutex used after shutdown
    cleanup` in `run_compiler_group_aot_runtime_parity_tests`.
  - Broad macro hygiene filter aborts with `status=139` in
    `advanced-macro-hygiene-string-number`.
  - JIT policy slice still has five unrelated failures after `AUDIT-032`
    closure.
- Dependencies, blockers, or restart requirements:
  - Rebuild with `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build
    --obj-out obj` before runtime checks if concurrent workers have touched
    build output.
- Signature: GPT-5 Codex

## 2026-04-25 06:44 CEST - Audit Closure Integration Wave 2

- Objective attempted:
  - Continue closing verified `AUDIT.md` findings with parallel workers and one
    parent integration pass.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `AUDIT.md`
  - libuv filesystem helpers, async process wait lifecycle, and stack clone
    residual pointer hardening.
- Code or configuration changes made:
  - Closed verified audit entries in `AUDIT.md`: `AUDIT-028`, `AUDIT-107`, and
    `AUDIT-112`.
  - Integrated native fs read/write chunking before `uv_buf_init` narrowing and
    added `tests/native/uv_fs_chunk_test.c`.
  - Integrated process wait-status caching before handle close and retry
    materialization coverage.
  - Integrated stack-clone residual source-pointer classification with
    fail-closed behavior for raw source-stack pointers.
- Commands run:
  - `cc -Wall -Wextra -Ideps/src/libuv/include tests/native/uv_fs_chunk_test.c csrc/uv_helpers.c deps/lib/libuv.a -pthread -ldl -lrt -o /tmp/omni_uv_fs_chunk_test && /tmp/omni_uv_fs_chunk_test`
  - `scripts/build_omni_chelpers.sh`
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_TEST_SUMMARY=1 ./build/main --test-suite stack`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-union-limit OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check` for the touched integration files.
- Key results:
  - Native fs chunk regression passed.
  - Helper rebuild passed.
  - Single-threaded C3 build passed and linked `build/main`.
  - Stack suite passed with `suite=stack_engine pass=26 fail=0`.
  - Focused advanced process scheduler slice passed with `pass=86 fail=0`.
- Current best recommendation or checkpoint:
  - Continue through remaining open `AUDIT.md` entries in independent slices,
    preserving focused validation where broad gates are known to include
    unrelated failures.
- Unresolved issues:
  - Prior broad compiler, macro hygiene, and JIT policy failures from the
    previous report entry remain outside this slice.
- Dependencies, blockers, or restart requirements:
  - Rebuild `build/main` after further C3 or native helper changes before
    relying on runtime test results.
- Signature: GPT-5 Codex

## 2026-04-25 07:02 CEST - Audit Closure Integration Wave 3

- Objective attempted:
  - Continue closing open `AUDIT.md` findings through parallel implementation
    workers and parent-owned integration validation.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - JIT effect fast-path arity, module import transactions, and TCP/UDP raw
    networking contracts.
- Code or configuration changes made:
  - Closed verified audit entries in `AUDIT.md`: `AUDIT-026`, `AUDIT-027`,
    `AUDIT-102`, `AUDIT-103`, `AUDIT-104`, and `AUDIT-135`.
  - Added a fixed-arity gate for JIT effect fast-path packed primitive calls.
  - Made implicit and declared module file loading rollback failed
    publication and reacquire module slots after nested evaluation boundaries.
  - Completed TCP raw registration integration so packed effect payloads reach
    `prim_tcp_connect` validation, and preserved IPv6 TCP helper parsing.
- Commands run:
  - C3 LSP diagnostics on touched JIT, module, async, and primitive-table files.
  - `cc -fsyntax-only -Ideps/src/libuv/include csrc/uv_helpers_tcp.c`
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=fixed-arity-raw-effect-payload-arity-gate OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib ./build/main --eval "(handle (__raw-tcp-connect \"127.0.0.1\" 1 nil) (raise msg msg))" --json`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib ./build/main --eval "(handle (await (spawn (lambda () (tcp-connect \"127.0.0.1\" 65536)))) (raise msg msg))" --json`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib ./build/main --eval "(handle (await (spawn (lambda () (block (define s (tcp-listen \"::1\" 0 8)) (tcp-close s) true)))) (raise msg msg))" --json`
- Key results:
  - Parent build passed and linked `build/main`.
  - JIT focused regression passed with `pass=1 fail=0`.
  - Advanced module focused group passed with `pass=2131 fail=0`.
  - Raw tcp-connect extra-argument eval returned
    `io/tcp-connect-expected-host-port`.
  - Wrapper/effect tcp-connect invalid-port eval returned
    `io/tcp-connect-invalid-port`, confirming the parent table integration.
  - IPv6 loopback `tcp-listen "::1"` eval returned `true` locally.
- Unresolved issues:
  - Broad `OMNI_LISP_TEST_SLICE=async` still aborts with the pre-existing
    `scope global mutex used after shutdown cleanup` failure after entering the
    async group. This is not resolved by the networking slice and remains a
    separate runtime lifecycle issue.
- Dependencies, blockers, or restart requirements:
  - Rebuild `build/main` after any further primitive-table, C3 runtime, or
    native TCP helper changes.
- Signature: GPT-5 Codex

## 2026-04-25 07:38 CEST - Audit Closure And Reaudit Pass

- Objective attempted:
  - Finish the open `AUDIT.md` backlog using mini worker verification/remediation,
    then run a fresh reaudit before committing and pushing non-artifact work.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `AUDIT.md`, compiler/AOT lowering, FFI/Vulkan/native helper hardening,
    memory allocation/continuation tests, and session audit artifacts.
- Code or configuration changes made:
  - Closed previously open audit entries in `AUDIT.md`: `AUDIT-030`,
    `AUDIT-033`, `AUDIT-034`, `AUDIT-035`, `AUDIT-038`, `AUDIT-039`,
    `AUDIT-041`, `AUDIT-042`, `AUDIT-043`, `AUDIT-048`, and `AUDIT-113`.
  - Added focused regression coverage for continuation wrapper allocation
    failure and core allocation/destructor-registration OOM paths.
  - Integrated AOT scoped module-open lowering and runtime primitive resolver
    parity checks/probes.
  - Reaudit appended new open findings `AUDIT-161` through `AUDIT-170`.
- Commands run:
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=continuation-wrapper-alloc-failure OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh bash -lc 'c3c build && env LD_LIBRARY_PATH=build:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime ./build/main --test-suite lisp'`
  - Direct `--compile` probes for `(with math ...)` and
    `(runtime-memory-stats)`.
  - Source-table parity check comparing runtime primitive registrations against
    `compiler_primitive_variable_hash_table*.c3`.
  - Static reaudit via four read-only mini explorer agents.
- Key results:
  - Parent build passed and linked `build/main`.
  - FFI focused slice passed with `pass=176 fail=0`.
  - JIT continuation wrapper allocation focused slice passed with
    `pass=1 fail=0`.
  - Bounded memory-lifetime slice passed with `pass=267 fail=0`.
  - AOT scoped module-open compile probe succeeded and generated
    `aot::lookup_module_export("math", ...)` calls for `abs`, `sin`, and `cos`.
  - Primitive parity compile probe generated
    `aot::lookup_prim("runtime-memory-stats")`; source-table parity reported no
    missing runtime primitive names.
- Invalidated assumptions or failed approaches:
  - `[FAILED]` Running `OMNI_LISP_TEST_SLICE=memory-lifetime` directly on the
    host is invalid; the test runner correctly aborts because memory ownership
    slices are container-only.
  - `[PENDING]` Broad compiler slice still aborts in the unrelated
    `aot_init` global scope-mutex shutdown path; scoped module-open and
    primitive parity probes pass independently.
- Current best recommendation or checkpoint:
  - Start the next remediation wave with `AUDIT-161`, because it likely explains
    the repeated global scope mutex shutdown abort that still blocks broad
    async/compiler validation.
- Unresolved issues:
  - New open reaudit findings: `AUDIT-161` through `AUDIT-170`.
- Dependencies, blockers, or restart requirements:
  - Rebuild `build/main` after future runtime/compiler/native changes before
    trusting runtime probes. Memory ownership slices must stay on the bounded
    container path.
- Signature: GPT-5 Codex

## 2026-04-25 08:29 CEST - Audit 161-182 Closure And Final Reaudit

- Objective attempted:
  - Close the remaining `AUDIT.md` findings from the previous reaudit wave,
    perform a final read-only reaudit, then commit and push non-artifact work.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - REPL/thread lifecycle, AOT/global collection, compile sidecar manifests,
    generated FFI preload startup guards, direct FFI argument validation,
    process/DNS C-string boundaries, scheduler cancellation, JIT shift rollback,
    and Deduce tuple/rule-signature validation.
- Code or configuration changes made:
  - Closed `AUDIT-161` through `AUDIT-171` after integrating lifecycle, FFI,
    JIT, compiler, and module-publication fixes.
  - Final reaudit produced and closed `AUDIT-172` through `AUDIT-182`.
  - Live REPL server paths now retain joinable client handles and join them
    before process registry shutdown.
  - Value/AOT shift suspend failure discards the created continuation on
    rollback.
  - OS-thread cancel-start allocation failure preserves and detaches the native
    handle instead of zeroing it out.
  - AOT global collection, CLI FFI manifests, and generated startup preload
    guards now recurse through scoped module-open bodies.
  - Direct FFI calls reject unsupported variadic values and embedded-NUL
    strings; process-spawn and DNS host boundaries reject embedded-NUL strings.
  - Deduce tuple encoding and rule-signature persistence now fail closed for
    unsupported values and malformed literal arrays.
- Commands run:
  - `cc -fsyntax-only` on touched native helper files.
  - `scripts/build_omni_chelpers.sh`
  - Native `uv_helper_cloexec_test` build and run.
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=handle-continuation-alloc-failure,signal-suspend-failure-clears-handler-state OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=scheduler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=parallel OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=command-surface OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule-validation OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `rg -n "Status: Open" AUDIT.md AUDIT_2.md`
  - `git diff --check`
- Key results:
  - Build passed and linked `build/main`.
  - Compiler slice passed with `pass=328 fail=0`.
  - Async slice passed with `pass=93 fail=0`.
  - Focused JIT policy slice passed with `pass=2 fail=0`.
  - Scheduler slice passed with `pass=141 fail=0`.
  - FFI system slice passed with `pass=177 fail=0`.
  - Advanced collections/module slice passed with `pass=2131 fail=0`.
  - Deduce filtered gates passed: parallel `pass=8 fail=0`,
    command-surface `pass=102 fail=0`, and rule-validation `pass=79 fail=0`.
  - No `Status: Open` entries remain in `AUDIT.md` or `AUDIT_2.md`.
  - `git diff --check` passed.
- Invalidated assumptions or failed approaches:
  - `[FAILED]` The full unfiltered Deduce slice is not currently a valid local
    closure signal because child subprocess restart tests fail to load
    `liblightning.so.2` from their environment. Use the focused Deduce groups
    above until the child-process loader environment is fixed.
- Current best recommendation or checkpoint:
  - Commit the integrated non-artifact work on bookmark
    `audit-remediation-2026-04-25`; if remote push is rejected, preserve the
    local jj change/bookmark and retry with credentials that can write to the
    target remote.
- Unresolved issues:
  - No open audit entries remain after the latest status scan.
  - Full unfiltered Deduce validation remains environment-blocked as described
    above.
- Dependencies, blockers, or restart requirements:
  - Rebuild `build/main` after future C3/native changes before relying on
    runtime probes. Full/high-memory memory validation must stay on the bounded
    container path.
- Signature: GPT-5 Codex

## 2026-04-25 08:55 CEST - Audit Closure Coverage Addendum

- Objective attempted:
  - Address final mini-agent reaudit coverage gaps before committing the closed
    audit backlog.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - Direct FFI string/variadic packing, process-spawn and DNS C-string
    boundaries, Deduce negative paths, JIT shift rollback, scheduler
    cancel-start cleanup, and REPL client-start rejection.
- Code or configuration changes made:
  - Added focused negative-path regressions for AUDIT-172, AUDIT-173,
    AUDIT-174, AUDIT-178, AUDIT-179, AUDIT-180, AUDIT-181, and AUDIT-182.
  - Moved DNS embedded-NUL validation ahead of fiber/loop availability checks so
    malformed C-boundary input fails closed deterministically.
  - Replaced invalid source-literal `\0` tests with direct constructed Omni
    strings that contain real embedded NUL bytes.
- Commands run:
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-union-limit OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=continuation-wrapper-alloc-failure,signal-suspend-failure-clears-handler-state,shift-suspend-failure-discards-continuation OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=scheduler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=parallel_component_scratch_pass,rule-validation OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `rg -n "Status: Open" AUDIT.md AUDIT_2.md`
  - `git diff --check`
- Key results:
  - Build passed and linked `build/main`.
  - Advanced FFI system passed with `pass=180 fail=0`.
  - Advanced effect/typed IO group passed with `pass=86 fail=0`.
  - Async slice passed with `pass=95 fail=0`.
  - Focused JIT policy slice passed with `pass=3 fail=0`.
  - Scheduler slice passed with `pass=142 fail=0`.
  - Focused Deduce gate passed with `pass=80 fail=0`.
  - No `Status: Open` entries remain in `AUDIT.md` or `AUDIT_2.md`.
  - `git diff --check` passed.
- Invalidated assumptions or failed approaches:
  - `[INVALIDATED]` Do not use source text `"\0"` as an embedded-NUL regression
    in this test surface; it does not exercise the C-boundary NUL path. Use
    constructed Omni strings with explicit `(char)0` bytes.
- Current best recommendation or checkpoint:
  - Commit the non-artifact working copy on
    `audit-remediation-2026-04-25` and attempt the remote push.
- Unresolved issues:
  - No open audit entries remain. Full unfiltered Deduce validation remains
    environment-blocked by child subprocess loader path as previously recorded.
- Signature: GPT-5 Codex

## 2026-04-25 09:00 CEST - Commit And Push Attempt

- Objective attempted:
  - Commit the integrated non-artifact audit remediation work and push the
    bookmark requested by the owner.
- Commands run:
  - `jj describe -m "Close audit backlog and final reaudit coverage"`
  - `jj bookmark set audit-remediation-2026-04-25 -r @`
  - `jj git push --bookmark audit-remediation-2026-04-25 --allow-new`
  - `git remote -v`
  - `jj log -r @ --no-graph --color never`
- Key results:
  - Local jj change/bookmark was prepared on
    `audit-remediation-2026-04-25`.
  - Push failed because the only configured remote is
    `https://github.com/gavlooth/omnilisp.git` and GitHub returned `403`:
    `Permission to gavlooth/omnilisp.git denied to
    christos-chatzifountas-biotz-io`.
- Current best recommendation or checkpoint:
  - Retry `jj git push --bookmark audit-remediation-2026-04-25` with
    credentials that have write access to `gavlooth/omnilisp.git`, or add a
    writable remote and push the same bookmark there.
- Unresolved issues:
  - Remote publication is blocked by credentials/permissions only; local
    validation and audit status are green.
- Signature: GPT-5 Codex

## 2026-04-25 10:24 CEST - Audit 202-207 Follow-Up Remediation

- Objective attempted:
  - Continue addressing `AUDIT.md` findings through parallel mini-agent
    review, integrate the confirmed fixes, update the durable audit artifacts,
    then commit and push the non-artifact work.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `AUDIT.md`
  - Deduce commit/reference validation, compiler FFI manifest discovery,
    compile sidecar cleanup, REPL clone-session rollback, and scheduler process
    shutdown.
- Code or configuration changes made:
  - Closed `AUDIT-202` through `AUDIT-207` in `AUDIT.md`.
  - Added fail-closed Deduce scan handling for deferred commit fact/reference
    validation and fact-path reference validation.
  - Added compiler regressions for nested embedded FFI contract JSON and stale
    sidecar unlink failure; recursive contract emission now matches startup
    preload traversal.
  - Made no-FFI/empty compile sidecar cleanup report unlink failures with a
    manifest path and write-kind.
  - Closed a REPL clone-start rollback leak by closing the new session when
    session-event delivery fails.
  - Routed `main` returns through `finish_main` so scheduler shutdown failure
    can change an otherwise-successful process exit to `1`.
- Commands run:
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `OMNI_LISP_TEST_SLICE=compiler
    OMNI_COMPILER_GROUP_FILTER=existing-feature OMNI_TEST_SUMMARY=1
    ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=integrity
    OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1
    ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1
    ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=scheduler OMNI_TEST_SUMMARY=1
    ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=advanced
    OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1
    ./build/main --test-suite lisp`
  - `scripts/check_status_consistency.sh`
  - `rg -n "Status: (Open|Reopened|Pending)" AUDIT.md AUDIT_2.md`
  - `git diff --check`
- Key results:
  - Build passed.
  - Compiler existing-feature slice passed with `336 passed, 0 failed`.
  - Deduce integrity slice passed with `39 passed, 0 failed`.
  - Full Deduce slice passed with `414 passed, 0 failed`.
  - Async slice passed with `96 passed, 0 failed`.
  - Scheduler slice passed with `143 passed, 0 failed`.
  - Targeted advanced FFI slice passed with `184 passed, 0 failed`.
  - Status consistency passed with zero actionable TODO items.
  - No open/reopened/pending audit entries were found in `AUDIT.md` or
    `AUDIT_2.md`.
  - `git diff --check` passed.
- Current best recommendation or checkpoint:
  - `AUDIT.md` has no known open entries after this follow-up wave; run a fresh
    reaudit before creating more remediation IDs.
- Unresolved issues:
  - Broad unfiltered advanced validation is still known to include unrelated
    pre-existing iterator boundary failures; this wave used the targeted FFI
    group for the touched FFI surface.
- Dependencies, blockers, or restart requirements:
  - Rebuild `build/main` after future C3 changes before relying on runtime test
    results.
- Signature: GPT-5 Codex

## 2026-04-25 10:55 CEST - Audit 208-218 Follow-Up Remediation

- Objective attempted:
  - Continue the audit-remediation line with parallel mini-agent review,
    integrate confirmed correctness/lifecycle fixes, update the canonical audit
    ledger, and prepare for commit/push.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `AUDIT.md`
  - Compiler result/output cleanup, TLS/FTXUI FFI boundaries, Pika/schema/Deduce
    parser/storage validation, async input-state teardown, and scheduler shared
    registry teardown.
- Code or configuration changes made:
  - Closed `AUDIT-208` through `AUDIT-218` in `AUDIT.md`.
  - Compiler result ownership now fails closed if destructor registration fails.
  - Call/app AOT lowering now stops when callee or app operand lowering fails.
  - Compile output commit failure now removes the generated FFI sidecar via a
    dedicated cleanup helper.
  - `tls-connect` rejects hostnames longer than 255 bytes instead of truncating.
  - FTXUI event-read ABI clearing now distinguishes initialized owned text from
    caller garbage and bounds the native session event queue.
  - Schema regex clauses reject regex `ERROR` results.
  - Pika parse-tree conversion propagates symbol/string/cons allocation
    failures as structured parser OOM.
  - Deduce tuple decoding rejects trailing bytes.
  - `InterpInputState` and scheduler shared-registry shutdown now destroy their
    synchronization resources and payload state.
- Commands run:
  - `scripts/build_omni_chelpers.sh`
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=schema OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=parallel OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=scheduler OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_e2e_baseline_policy.sh`
- Key results:
  - Native helper rebuild passed.
  - Build passed and linked `build/main`.
  - Compiler slice passed with `pass=338 fail=0`.
  - Pika slice passed with `pass=126 fail=0`.
  - Schema slice passed with `pass=44 fail=0`.
  - Focused Deduce parallel group passed with `pass=11 fail=0`.
  - Async slice passed with `pass=97 fail=0`.
  - Scheduler slice passed with `pass=143 fail=0`.
  - Advanced FFI system passed with `pass=185 fail=0`.
  - File-size and e2e baseline policy gates passed.
- Invalidated assumptions or failed approaches:
  - `[FAILED]` Calling `run_compile_mode` from inside the compiler test process
    is not a valid regression seam; it performs CLI lifecycle shutdown and
    destroys the global scope mutex needed by later compiler tests. The
    regression was moved to the sidecar cleanup helper instead.
- Current best recommendation or checkpoint:
  - Commit this follow-up on `audit-remediation-2026-04-25`, then push the
    bookmark with configured GitHub credentials.
- Unresolved issues:
  - `AUDIT_2.md` remains as imported external audit material; canonical closure
    status is tracked in `AUDIT.md`.
- Dependencies, blockers, or restart requirements:
  - Rebuild native helpers after FTXUI C++ changes and rebuild `build/main`
    after C3 changes before relying on runtime tests.
- Signature: GPT-5 Codex

## 2026-04-25 11:28 CEST - Audit 219-230 Remediation And Reaudit Follow-Up

- Objective attempted:
  - Continue addressing verified `AUDIT.md` issues with parallel mini-agent
    implementation, integrate fixes, validate touched surfaces, and prepare a
    non-artifact commit/push.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `AUDIT.md`
  - Runtime heap-backed value destructor registration, compile/build artifact
    rollback, scheduler shutdown, Pika regex cache, Deduce materialized
    metadata, JSON/TOML native metadata bridges.
- Code or configuration changes made:
  - Closed `AUDIT-219` through `AUDIT-230`.
  - Added shared `Value` destructor-registration helpers that clean up shaped
    heap payloads on destructor-table OOM.
  - BigInteger/BigFloat/BigComplex/tensor constructors, copy paths, and escape
    promotion paths now fail closed on destructor-registration failure.
  - Compile sidecar writes now remove stale final manifests on write failure;
    compile sidecar rollback reports unlink failure; AOT build publishes
    backend binaries from staged temp outputs.
  - Scheduler shutdown now drains pending I/O, offload-admission freelist/mutex
    state, thread-task registry, and OS-thread registry before final reset.
  - Regex cache init/grow now publishes only successful allocations and falls
    back uncached on cache bookkeeping allocation failure.
  - Deduce materialized metadata restore now propagates read failures and
    rejects unknown persisted stale-reason bytes.
  - JSON/TOML native metadata corruption now returns structured
    `parser/invalid-state` errors instead of empty/nil values.
  - Added `AUDIT-231` for the then-red broad Deduce materialized restart
    fixtures; later closure in this report supersedes the initial diagnosis.
- Commands run:
  - `scripts/build_omni_chelpers.sh`
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - bounded container `memory-lifetime-smoke`:
    `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --obj-out obj && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=scheduler OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=pika OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=data-format OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=basics OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - direct reproduction: `(block (define db (deduce 'open "...")) db)`
  - `git diff --check`
- Key results:
  - Native helper rebuild passed.
  - C3 build passed and linked `build/main` when run with local library path.
  - Memory-lifetime smoke passed with `pass=269 fail=0`.
  - Compiler slice passed with `pass=340 fail=0`.
  - Scheduler slice passed with `pass=143 fail=0`.
  - Pika slice passed with `pass=128 fail=0`.
  - Data-format slice passed with `pass=92 fail=0`.
  - Deduce basics group passed with `pass=11 fail=0`.
  - At this checkpoint, the full Deduce slice remained red with
    `pass=411 fail=6`; those materialized restart fixtures were tracked as
    `AUDIT-231` and later closed in this report.
  - `git diff --check` passed.
- Invalidated assumptions or failed approaches:
  - `[INVALIDATED]` This checkpoint's initial framing of `AUDIT-231` as a
    block-local `define` contract issue was later corrected. `define` remains
    global; the actual fixes were JIT block error short-circuiting and LMDB
    restore transaction reuse.
- Current best recommendation or checkpoint:
  - Superseded by the later `AUDIT-231` closure entry below.
- Unresolved issues:
  - Superseded by the later `AUDIT-231` closure entry below.
- Dependencies, blockers, or restart requirements:
  - Rebuild `build/main` after C3 changes before relying on runtime tests.
  - Host full `c3c build` without `LIBRARY_PATH=/home/christos/.local/lib`
    can fail to find local `liblightning`/`libreplxx`.
- Signature: GPT-5 Codex

## 2026-04-25 12:06 CEST - AUDIT-231 Closure And Reaudit

- Objective attempted:
  - Close the remaining canonical `AUDIT.md` item, then run a focused reaudit
    before committing and pushing non-artifact work.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `AUDIT.md`
  - JIT small-block lowering and Deduce persisted materialized metadata restore.
- Code or configuration changes made:
  - Closed `AUDIT-231`.
  - Kept `define` as a global definition form; no local struct/type/relation
    registry was introduced.
  - Fixed native JIT small-block lowering so non-final `ERROR` values
    short-circuit immediately instead of allowing later expressions to mask the
    real failure with unbound-variable symptoms.
  - Added focused JIT policy regressions for masked `define` RHS errors and
    synthesized empty blocks compiled in tail position.
  - Refactored Deduce materialized metadata restore to support lookup through an
    existing LMDB transaction, and reused the persisted rule-signature restore
    read transaction instead of opening a nested read transaction while a cursor
    is live.
  - Tightened materialized metadata DBI open handling so only `MDB_NOTFOUND`
    becomes "no metadata"; other LMDB open errors fail closed.
  - Added Deduce failure-injection coverage for forced materialized metadata
    restore failure during `deduce/open`.
- Commands run:
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib ./build/main /tmp/audit231-error.omni`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=block-define-rhs-error-short-circuit OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=block-define-rhs-error-short-circuit,empty-block-tail-nil OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=basics OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=materialized OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - Focused read-only mini-agent reaudit of JIT, Deduce persistence, and audit
    consistency surfaces.
- Key results:
  - C3 build passed and linked `build/main`.
  - Direct masked-error probe now reports `unbound variable 'missing-fn'`
    instead of the later `audit231-e` binding name.
  - Focused JIT policy regression first passed with `pass=1 fail=0`; after
    reaudit follow-up, the combined JIT filter passed with `pass=2 fail=0`.
  - Focused Deduce basics group passed with `pass=12 fail=0`, including the
    new metadata-restore failure-injection case.
  - Focused Deduce materialized restart group passed with `pass=8 fail=0`.
  - Full Deduce slice first passed with `pass=417 fail=0`; after adding the
    failure-injection case, it passed with `pass=418 fail=0`.
  - At this checkpoint, `AUDIT.md` had no remaining `Status: Open`,
    `Status: In Progress`, or `Status: Partial` markers after closing
    `AUDIT-231`; this was later superseded by the `AUDIT-048` inline-module
    scoped-open reopen entry in this same report.
  - Focused reaudit findings were addressed before commit: stale report wording
    was marked superseded, empty-block tail nil handling now uses the tail-aware
    helper, and materialized metadata DBI open failures now fail closed.
- Invalidated assumptions or failed approaches:
  - `[INVALIDATED]` `AUDIT-231` was not a request to make `define`
    block-local. The owner clarified that `define` should bind globally, and the
    reproducible language issue was native block error masking.
  - `[INVALIDATED]` The materialized restart fixtures did not require a local
    registry rewrite. Once error masking was fixed, the real blocker was nested
    LMDB read transaction usage during persisted rule-signature restore.
- Current best recommendation or checkpoint:
  - Commit and push the `AUDIT-231` closure. Treat `AUDIT.md` as clear unless a
    later fresh audit appends new issues.
- Unresolved issues:
  - None known in canonical `AUDIT.md` at this checkpoint.
- Dependencies, blockers, or restart requirements:
  - Rebuild `build/main` after further C3 changes before relying on runtime
    test results.
- Signature: GPT-5 Codex

## 2026-04-25 12:42 CEST - Validation Tooling Rule And Host Install

- Objective attempted:
  - Add a durable rule/pointer so future agents know which debugging,
    profiling, and memory-validation tools to use for regression closure, and
    install available host tools.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `AGENTS.md`
  - `docs/README.md`
  - `docs/VALIDATION_TOOLS.md`
- Code or configuration changes made:
  - Added `docs/VALIDATION_TOOLS.md` as the canonical regression/debugging tool
    guide for Valgrind, ASAN, gdb, perf, hyperfine, heaptrack, rr, and strace.
  - Linked the guide from `AGENTS.md` and `docs/README.md`.
  - Installed missing Ubuntu packages: `hyperfine`, `heaptrack`, and `rr`.
- Commands run:
  - `uname -a`
  - `sed -n '1,120p' /etc/os-release`
  - `apt-cache policy hyperfine heaptrack rr linux-tools-common linux-tools-generic linux-tools-$(uname -r)`
  - `sudo apt-get install -y hyperfine heaptrack rr`
  - `valgrind --version`
  - `gdb --version | head -n 1`
  - `perf --version`
  - `hyperfine --version`
  - `heaptrack --version`
  - `rr --version`
  - `strace -V | head -n 1`
  - `rr record true`
  - `rr record -n true`
  - `perf stat true`
  - `hyperfine --warmup 1 --runs 2 true`
  - `valgrind --error-exitcode=99 true`
  - `heaptrack /usr/bin/true`
  - `strace -o /tmp/omni-strace-smoke.out true`
  - `git diff --check -- AGENTS.md docs/README.md docs/VALIDATION_TOOLS.md .agents/session_report_parts/session_report_part_43.md`
- Key results:
  - Host is Ubuntu 24.04.4 LTS on aarch64.
  - Installed/verified versions: Valgrind 3.22.0, gdb 15.0.50, perf 6.17.9,
    hyperfine 1.18.0, heaptrack 1.5.0, rr 5.7.0, strace 6.8.
  - Valgrind, hyperfine, heaptrack, and strace smoke checks ran.
  - `perf stat true` and normal `rr record true` are blocked by
    `kernel.perf_event_paranoid=4`.
  - `rr record -n true` also fails on this aarch64 host because rr does not
    recognize the CPU microarchitecture.
  - `git diff --check` passed for the new guide/rule files and this report.
- Invalidated assumptions or failed approaches:
  - `[INVALIDATED]` Do not assume installed `rr` is usable on this host. It is
    present, but both normal and `-n` recording paths are currently blocked.
  - `[INVALIDATED]` Do not assume installed `perf` is enough for profiling
    evidence. The current kernel perf security setting blocks unprivileged use.
- Current best recommendation or checkpoint:
  - Future regression work should start from `docs/VALIDATION_TOOLS.md` after
    naming the broken contract in the Regression Closure Rule.
- Unresolved issues:
  - Broad/high-memory validation remains container-bound by repo policy.
  - Usable `perf`/`rr` capture requires a capable host or explicit host
    administration changes; do not change sysctl as a routine repo step.
- Dependencies, blockers, or restart requirements:
  - New packages are installed on the host; existing long-running processes may
    need restart only if they should observe updated system libraries.
- Signature: GPT-5 Codex

## 2026-04-25 13:28 CEST - Audit Follow-Up Wave And Reopened Scoped-Open Residual

- Objective attempted:
  - Continue addressing audited issues with delegated review, update the audit
    ledger, commit non-artifact work, and push.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `AUDIT.md`, `TODO.md`, `docs/todo_parts/todo_part_17.md`
  - Deduce metadata/integrity, async/NN C string boundaries, JIT/AOT
    control-flow lowering, tensor native result cleanup, validation tooling.
- Code or configuration changes made:
  - Added clamped validation memory override handling in
    `scripts/c3c_limits.sh` and `scripts/run_validation_container.sh`.
  - Added `docs/VALIDATION_TOOLS.md` and linked it from repo guidance.
  - Hardened Deduce immediate key/unique scans to fail closed on tuple decode,
    arity, and cursor-termination failures.
  - Hardened Deduce materialized metadata refresh-policy restore, while keeping
    legacy V1 no-policy metadata compatible as manual refresh.
  - Rejected embedded NUL paths in async file primitives/helpers and NN
    checkpoint load/save paths.
  - Made the async read-file offload worker report helper read failures as
    `OFFLOAD_RES_ERROR` rather than nil, with direct worker-boundary coverage
    for the embedded-NUL path case.
  - Added JIT and AOT error-propagation guards for predicate/non-final
    `ERROR` values.
  - Replaced ignored tensor native destructor registration returns with checked
    cleanup-or-error helper calls.
  - Added tests for Deduce invalid metadata policy, V1 compatibility, immediate
    scan-end failures, async file NUL paths, NN checkpoint NUL paths, JIT
    middle-expression error propagation, and AOT generated error guards.
  - Reopened `AUDIT-048` because inline module exports still leak as AOT
    generated globals; added `SCOPED-MODULE-AOT-002` to Part 17 and the live
    queue.
  - Added closed `AUDIT-232` through `AUDIT-237` entries for this wave.
- Commands run:
  - `LIBRARY_PATH=/home/christos/.local/lib c3c --threads 1 build --obj-out obj`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=core-surface,integrity ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=block-define-rhs-error-short-circuit,empty-block-tail-nil,control-flow-predicate-error ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/home/christos/.local/lib:/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `scripts/check_status_consistency.sh`
  - `scripts/check_build_config_parity.sh`
  - `scripts/check_file_size_gate.sh`
  - `scripts/check_e2e_baseline_policy.sh`
  - `git diff --check`
- Key results:
  - C3 build passed and linked `build/main`.
  - Deduce focused group passed with `pass=41 fail=0`.
  - JIT policy focused filter passed with `pass=6 fail=0`.
  - Compiler slice passed with `pass=345 fail=0`.
  - Status, build-config parity, file-size, e2e baseline, and whitespace gates
    passed.
  - Async slice still failed on unrelated lifecycle/network cases:
    `async file read cancel stress`, `pipe connect/listen in fiber via libuv
    bridge`, and `udp-recv in fiber via async bridge`; the new embedded-NUL file
    regression did not appear in the failure list.
  - Advanced collections group still failed on unrelated
    `ml/softmax Vulkan Float32 stays on device and normalizes rows`; the new NN
    checkpoint embedded-NUL regression did not appear in the failure list.
- Invalidated assumptions or failed approaches:
  - `[INVALIDATED]` Treating `AUDIT-048` as completely closed was too broad.
    External module scoped-open AOT lowering works, but inline module body
    definitions still leak exported names as globals.
  - `[INVALIDATED]` Do not reject legacy V1 Deduce materialized metadata simply
    because no refresh-policy bit exists; V1 no-policy rows must read as manual
    refresh for compatibility.
- Current best recommendation or checkpoint:
  - Superseded by the later `AUDIT-048` inline-module leak closure: the
    module-private backing-symbol implementation shipped, `SCOPED-MODULE-AOT-002`
    is closed, and the current live queue has moved to the Part 18 memory-model
    improvement items.
- Unresolved issues:
  - The later closure entry in `.agents/SESSION_REPORT.md` and `AUDIT.md`
    closes `AUDIT-048` / `SCOPED-MODULE-AOT-002`.
  - Pre-existing async lifecycle/network failures and Vulkan softmax
    interpreter failure remained outside this wave.
- Dependencies, blockers, or restart requirements:
  - Rebuild `build/main` after further C3 changes before relying on runtime
    tests. Broad/high-memory validation remains container-bound.
- Signature: GPT-5 Codex
