# Active TODO Index Part 14

Source: `TODO.md`

- [x] `META-STATS-001` remove duplicate SCC/parallel topology planner work from `deduce/stats`
  - closure evidence:
    - removed dead duplicate topology-planning wrapper that rebuilt SCC + batch metadata independently of the stats path:
      - deleted `deduce_parallel_batch_topology_counts(...)` from `src/lisp/deduce_schema_query_metadata_parallel_topology.c3`
    - the live stats path remains on a single metadata-build lane:
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
      - `src/lisp/deduce_schema_query_metadata_parallel_topology.c3`
    - no residual callsites remain for the deleted duplicate planner wrapper:
      - `rg -n "deduce_parallel_batch_topology_counts\\(" src/lisp` -> no matches
    - integration safety:
      - `c3c build` passes and links `build/main`
  - validation note:
    - the broader `OMNI_LISP_TEST_SLICE=deduce` lane is currently green in this workspace:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=324 fail=0`

- [x] `REF-SPLIT-INFRA-REPL-001` split REPL server runtime surfaces by ownership lane
  - closure evidence:
    - split `src/lisp/eval_repl_server.c3` top-down into focused runtime surfaces without behavioral contract changes:
      - request auth/dispatch lane extracted to `src/lisp/eval_repl_server_request.c3`
      - unix/tcp listener lane extracted to `src/lisp/eval_repl_server_listeners.c3`
      - stream/session orchestration retained in `src/lisp/eval_repl_server.c3`
    - primary runtime file was reduced from `332` lines to `67` lines:
      - `wc -l src/lisp/eval_repl_server.c3 src/lisp/eval_repl_server_request.c3 src/lisp/eval_repl_server_listeners.c3`
    - integration + async REPL validation remain green after the split:
      - `c3c build`
      - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=59 fail=0`

- [x] `AUDIT-BINDGEN-DEFERRED-001` resolve generated bindgen TODO ownership/teardown policy gaps
  - closure evidence:
    - bindgen wrappers now enforce concrete ownership/role/teardown guards and fail closed where policy is unresolved:
      - `src/lisp/bindgen.c3`
      - added explicit raise paths:
        - `bindgen/opaque-arg-role-mismatch`
        - `bindgen/opaque-arg-ownership-mismatch`
        - `bindgen/opaque-arg-unsupported-teardown`
        - `bindgen/opaque-arg-nil`
        - `bindgen/string-return-unknown-ownership`
        - `bindgen/opaque-return-manual-review`
    - staged comment marker switched from `TODO(bindgen)` to `REVIEW(bindgen)`:
      - `src/lisp/bindgen.c3`
      - `src/lisp/tests_compiler_codegen_groups_tail.c3`
    - compiler bindgen coverage remains green:
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=189 fail=0`

- [x] `AUDIT-DEDUCE-WHY-DERIVED-001` implement missing derived-subject why-result support
  - closure evidence:
    - removed explicit unsupported derived-subject error path from why-result lookup:
      - `src/lisp/deduce_why_result_lookup_derived.c3`
      - `rg -n "why-result-derived-subject-not-yet-supported|not-yet-supported" src/lisp/deduce_why_result_* src/lisp/deduce_*` returns no matches
    - derived why-result now returns structured provenance payloads instead of a legacy unsupported error for supported read shapes:
      - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block ... (deduce/query reach (lambda (row) (= (ref row 'src) 8))) (deduce/why-result reach 8 10))"`
      - observed payload includes:
        - `kind why-result`
        - `path-kind derived`
        - `status partial`
        - `paths` with structured support frames

- [x] `AUDIT-BUILD-IMPORT-001` restore green build by fixing invalid allocator import paths
  - closure evidence:
    - updated allocator import/module references in:
      - `src/lisp/scheduler_thread_tasks.c3`
      - `src/lisp/eval_repl_server_worker.c3`
    - `c3c build` now completes and links `build/main`.

- [x] `AUDIT-REPL-SECURITY-001` lock down unauthenticated remote REPL execution surfaces
  - closure evidence:
    - TCP REPL now enforces loopback bind (`localhost`/`127.0.0.1`/`::1`) and requires `OMNI_REPL_TCP_AUTH_TOKEN` at startup:
      - `src/lisp/eval_repl_server.c3`
    - per-request authorization gate added for non-`describe` operations with `auth` token matching:
      - `src/lisp/eval_repl_server.c3`
      - `src/lisp/eval_repl_server_protocol.c3`
      - `src/lisp/eval_repl_server_protocol_parse.c3`
      - `src/lisp/eval_repl_server_state.c3`
      - `src/lisp/eval_repl_server_output.c3`
    - regression coverage added:
      - `src/lisp/tests_runtime_async_repl_server_groups.c3`
    - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=59 fail=0`.

- [x] `AUDIT-DEDUCE-PARALLEL-RUNTIME-001` replace metadata-only parallel mode with truthful runtime state
  - closure evidence:
    - added shared runtime-truth field helper with explicit serial runtime counters:
      - `src/lisp/deduce_parallel_runtime_truth.c3`
    - updated analyze/explain/stats payloads to emit truthful runtime mode/counters:
      - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`
      - `src/lisp/deduce_rule_ops_explain_snapshot.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
    - updated assertions:
      - `src/lisp/tests_deduce_query_admin_surface_tail.c3`
      - `src/lisp/tests_deduce_rule_groups_explain.c3`
    - targeted eval checks for analyze/stats runtime mode now return `true`.

- [x] `AUDIT-DEDUCE-NAIVE-FALLBACK-001` reduce recursive component fallback to naive execution
  - closure evidence:
    - seminaive recursive aggregate path is now selected directly when seminaive recursive mode is enabled:
      - `src/lisp/deduce_rule_eval_fixpoint_component_eval_non_txn.c3`
      - `src/lisp/deduce_rule_eval_fixpoint_component_eval.c3`
    - analyze/explain runtime-truth payloads remain aligned with serial execution counters.

- [x] `AUDIT-NET-IPv6-001` remove IPv4-only DNS resolution limitation
  - closure evidence:
    - DNS address rendering now supports both `AF_INET` and `AF_INET6` and reports unsupported families explicitly:
      - `src/lisp/async_tcp_transport_helpers.c3`
      - `src/lisp/async_runtime_base.c3`
      - `src/lisp/async_process_signal_dns.c3`
    - deterministic coverage added for IPv4 + IPv6 addrinfo rendering:
      - `src/lisp/tests_runtime_async_io_tls_groups.c3`
    - `OMNI_LISP_TEST_SLICE=async ... --test-suite lisp` remains green (`pass=59 fail=0`).

- [x] `STACK-AARCH64-CONT-001` arm64 language-level continuation multi-shot parity
  - closure evidence:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` -> `Stack engine: 23 passed, 0 failed`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (with-continuation k (k 41))))"` -> `42`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(checkpoint (+ 1 (capture k (k 10))))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block (define replay-set-counter 0) (define replay-set-r (checkpoint (+ (capture k (+ (k 1) (k 1))) (block (set! replay-set-counter (+ replay-set-counter 1)) replay-set-counter)))) (+ (* 10 replay-set-r) replay-set-counter))"` -> `52`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=56 fail=0`
    - `OMNI_JIT_POLICY_FILTER=multishot-capture-scope-guard-clone OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=1 fail=0`
  - implementation note:
    - arm64 GNU lightning register IDs were corrected in `src/lisp/jit_lightning_constants.c3`.
    - effect fast-path primitive dispatch now preserves primitive error payloads and supports dotted cons payloads for fixed-arity wrappers in:
      - `src/lisp/jit_handle_signal_helpers_runtime_effects.c3`
      - `src/lisp/jit_runtime_effects_signal.c3`
- [x] `AUDIT-AOT-PRINT-DEDUCE-SENTINELS-059` close AOT/type-spec, print, and deduce persistence soundness defects
  - closure evidence:
    - AOT generated type/type-spec surfaces now reject `INVALID_SYMBOL_ID`
      before constructing type definitions, aliases, effects, method
      signatures, match helpers, dictionary symbol lookup, and effect explain
      payloads:
      - `src/lisp/aot.c3`
      - `src/lisp/aot_runtime_bridge.c3`
      - `src/lisp/aot_runtime_bridge_helpers.c3`
      - `src/lisp/aot_runtime_match_helpers.c3`
      - `src/lisp/aot_type_definitions.c3`
      - `src/lisp/aot_type_spec_helpers.c3`
    - AOT compiled list helpers now reject negative `long` indexes before
      converting to `usz`.
    - direct and buffered value printers now tolerate nullable dictionary/set
      backing storage, and `print_value_to_buf` now rejects null/zero-capacity
      buffers before writing.
    - constructor type constraint diagnostics now use guarded type-registry
      lookups, and instance type inference rejects invalid type IDs.
    - deduce tuple persistence now stores full 32-bit `SymbolId` values and
      rejects invalid/out-of-range decoded symbols.
    - deduce materialized metadata delete now distinguishes missing metadata DBI
      from real DBI-open errors.
    - deduce DBI name/path copy helpers now use checked addition before
      allocation.
    - deduce relation/rule install failure paths now roll back newly appended
      in-memory schemas/rule signatures when later fallible persistence or
      handle-publication steps fail.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `compiler`: green (`pass=191 fail=0`)
    - bounded `deduce`: green (`pass=330 fail=0`)
    - bounded `advanced`: green (`pass=1183 fail=0`)
    - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)

- [x] `AUDIT-AOT-RUNTIME-MANIFEST-061` restore manifest-backed AOT source parity
  - closure evidence:
    - `src/entry_build_runtime_manifest_lisp_part*.c3` now covers every
      non-test `src/lisp/*.c3` runtime file, including the tensor, Vulkan, and
      ML primitive sources that had drifted out of the explicit AOT manifest.
    - The manifest split now uses four under-700 parts, and
      `src/entry_build_backend_compile.c3` appends all four parts.
    - `scripts/check_e2e_baseline_policy.sh` now validates the manifest-based
      AOT source path, checks representative root/pika anchors, and compares
      the full non-test Lisp source set against the manifest entries to catch
      omissions and duplicates.
    - `src/lisp/deduce_relation_row_materialization.c3` no longer depends on
      test-only `test_c_getenv` from production runtime code; it uses the
      production boundary getenv wrapper.
  - validation:
    - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: green
    - `scripts/check_e2e_baseline_policy.sh`: green
    - `c3c build`: green
    - AOT smoke using `ml/mean-squared-error`: compiled, linked, and ran with
      output `1.66666666666667`

- [x] `AUDIT-PARSER-JIT-ASYNC-BOUNDARY-060` close parser/compiler, JIT boundary, macro splice, and async/TLS soundness defects
  - closure evidence:
    - parser language-surface interning now fails closed through a parser-local
      checked helper before publishing symbols into import paths, path
      segments, type annotations, collection constructors, explain selectors,
      relation definition rewrites, quasiquote/template underscores, and
      special-form comparisons.
    - compiler lambda effect-wrapper scanning now uses checked AST arena
      allocation and invalid-symbol guards before publishing synthetic wrapper
      bodies or handler bodies.
    - primitive variable hash bootstrap now rejects `INVALID_SYMBOL_ID` keys
      and reports initialization failure instead of treating the sentinel as an
      empty slot.
    - compiler integer emission now avoids `long.min` negation and avoids
      `usz -> long` narrowing for unsigned decimal output.
    - macro splice append now rejects improper splice tails and recursion-limit
      exhaustion instead of silently truncating to the rest/nil.
    - runtime boundary string/error copying now guards `len + 1` allocation
      arithmetic, and boundary policy integer parsing now rejects overflow.
    - JIT resolve/continuation yield-failure paths restore saved interpreter
      state before returning; pending raise handler staging now clears
      `raise_pending` only after payload/env/list construction succeeds.
    - runtime handle entry points now reject null tags/closures arrays when
      `count > 0`.
    - TLS offload yield-error paths now close pending offload state before
      returning, TCP/UDP ports and signal numbers are range-checked before
      narrowing to `int`, and file-read close failure now fails the read.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `compiler`: green (`pass=191 fail=0`)
    - bounded `async`: green (`pass=61 fail=0`)
    - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
    - bounded `advanced` macro hygiene group: green (`pass=82 fail=0`)

- [x] `AUDIT-SCHEMA-LIFETIME-WIDTH-061` close schema/deduce payload and lifetime-width soundness defects
  - closure evidence:
    - schema explain payload setters now reject failed key/value symbol
      interning before publishing dictionary keys or symbol payload values.
    - JSON pointer symbol-key fallback now treats failed token interning as a
      miss instead of constructing an invalid symbol key.
    - filesystem/process payload helpers now check result-key interning and
      file-size/handle-key bounds before constructing keys or integer payloads.
    - user-facing `exit`, `TimePoint`, Unicode codepoint predicates, `fs-open`,
      `tcp-listen`, and zlib original-size paths now validate integer ranges
      before narrowing to C `int`/`usz`/external API widths.
    - deduce materialize now rejects failed `"manual"` policy interning before
      persisting relation/schema state.
    - deduce integrity payload construction now propagates actual allocation/
      intern/set errors instead of collapsing them to `null`, and list payload
      builders now stop on cons allocation errors.
    - materialized-stale and integrity-violation payload symbols now reject
      failed interning before `make_symbol`.
    - primitive name matching now rejects null primitive backing pointers and
      overlong expected names before reading the fixed primitive name buffer.
    - checked array construction now allocates backing payloads before
      publishing the root wrapper, mirroring the hashmap constructor pattern.
    - closure escape promotion now releases any retained/detached env scope if
      final wrapper allocation fails.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `data-format`: green (`pass=64 fail=0`)
    - bounded normal+ASAN `unicode`: green (`pass=27 fail=0`)
    - bounded normal+ASAN `compression`: green (`pass=27 fail=0`)
    - bounded normal+ASAN `async`: green (`pass=65 fail=0`)
    - bounded normal+ASAN `compiler`: green (`pass=194 fail=0`)
    - bounded normal+ASAN `memory-lifetime-smoke`: green (`pass=190 fail=0`)
    - bounded normal `advanced`: green (`pass=1185 fail=0`)
    - bounded ASAN `advanced`: green (`pass=1172 fail=0`)
    - bounded normal+ASAN `deduce`: green (`pass=330 fail=0`)

- [x] `AUDIT-FTXUI-SMOKE-SEGFAULT-062` investigate FTXUI smoke crash outside the Lisp slice set
  - observed during bounded validation after all targeted Lisp slices had
    passed:
    - `scripts/run_ftxui_smoke.sh` `smoke.omni` exited with SIGSEGV.
  - closed:
    - root cause was a runtime lifetime-boundary provenance/reuse walk over a
      nested effect payload graph, not the FTXUI lowering path.
    - `src/lisp/eval_boundary_provenance.c3` now uses a bounded iterative
      alias-safety worklist with visited tracking for nested arrays, dicts,
      sets, method tables, partials, iterators, and cons payloads, avoiding the
      recursive stack overflow that surfaced through FTXUI `ui.graph` payloads.
    - `src/lisp/eval_boundary_provenance.c3` now skips scalar leaves before
      consuming alias worklist/visited capacity, so wide scalar-only payloads do
      not fail closed as if they were unsafe graph overflows.
    - `src/lisp/prim_ui_ftxui.c3` now checks child component count arithmetic
      before allocating the FTXUI child pointer array.
    - `src/lisp/prim_ui_ftxui_helpers.c3` now guards helper-array capacity
      growth and graph-series allocation size math before `realloc`/`malloc`.
    - `src/lisp/prim_ui_ftxui_lowering.c3` now rejects menu item counts that
      cannot be represented by the FTXUI `int` selected-index state.
    - `csrc/ftxui_shim.cpp` now declares `keep_alive` before `component`, so
      component teardown runs before retained borrowed backing data is released.
    - `csrc/ftxui_shim.cpp` now rejects nonzero child counts with null child
      arrays, checks table `rows * cols` overflow before comparing against the
      child count, and rejects table row/column selectors that cannot fit in
      FTXUI `int` APIs.
    - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` adds a minimal
      nested effect-payload regression matching the failing view shape plus a
      wide scalar payload regression, and
      `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` keeps both in the
      bounded smoke lane.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      green (`pass=192 fail=0`)

- [x] `ML-VK-001` freeze the backend-neutral Vulkan ML suite contract and capability matrix
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - audit items:
    - [x] `AUDIT-ML-VK-001-CAPABILITY-KEYS` add explicit backend-neutral ML
      capability keys to `tensor-backends` for all backend entries, with
      regression coverage proving unimplemented ML families report `false`.
    - [x] `AUDIT-ML-VK-001-NAMING` freeze `ml/linear` as the first
      backend-neutral public ML operation name.
    - [x] `AUDIT-ML-VK-001-FAIL-CLOSED` add Vulkan regression tests for the
      first frozen unsupported public ML operation name, asserting the call
      fails closed with no CPU fallback and that the capability key remains
      `false`.
    - [x] `AUDIT-ML-LINEAR-NONCPU-OPERANDS` cover `ml/linear` fail-closed
      behavior for Vulkan weights and Vulkan bias operands, not only Vulkan
      input tensors.
    - [x] `AUDIT-ML-LINEAR-CPU-ORACLE-SHAPES` cover the CPU oracle for lazy CPU
      inputs, rank-3 batch projection, and dtype mismatch diagnostics.
    - [x] `AUDIT-VK-SCIENTIFIC-F64-CAPABILITY` split or document partial
      Vulkan Float64 scientific coverage so `stats/normal-*` support is not
      hidden behind the coarse `scientific-map-float64` key.
   - [x] `AUDIT-ML-LINEAR-DIRECT-FLOAT32` keep broad Vulkan `ml-linear` false
      while documenting the narrow `ml-linear-direct-float32` partial
      capability for already-materialized concrete Vulkan `Float32` `ml/linear`
      only.
   - [x] `AUDIT-ML-VK-010-004-SURFACE` split `ML-VK-010-004` into a public
      batched-reduction surface decision before reducer coverage.
  - scope:
    - define public ML operation names without backend-flavored APIs;
    - add `tensor-backends` capability keys for ML operation families;
    - document inference-only versus training-capable operations;
    - specify mixed-device, unsupported-layout, lazy Tensor, and unsupported
      dtype diagnostics;
    - add fail-closed tests proving missing Vulkan ML kernels do not copy to CPU.
  - acceptance:
    - CPU/Vulkan capability reporting is truthful on Vulkan-visible and
      Vulkan-unavailable hosts;
    - unsupported Vulkan ML operations raise Tensor backend diagnostics before
      any hidden transfer or CPU fallback.

- [x] `ML-VK-010` add Vulkan batched linear algebra foundations for ML workloads
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - audit split:
    - [x] `ML-VK-010-001` implement the first direct concrete Vulkan `Float32`
      no-bias lowering path for `ml/linear` through the existing contract
      route, preserving Tensor placement, keeping `tensor-backends` truthful,
      and failing closed on mixed-device and unsupported-layout operands.
    - [x] `ML-VK-010-002` add Vulkan `Float32` no-bias batched matmul coverage
      with CPU-oracle comparisons, dtype/device preservation checks,
      shape diagnostics, and no-hidden-CPU-fallback regressions.
    - [x] `ML-VK-010-003` add Vulkan `Float32` batched bias-add coverage with
      no-CPU-fallback regressions.
    - [x] `ML-VK-010-004` freeze `ml/linear/batched-reduce` as the public
      batched-reduction surface for Vulkan `Float32` ml/linear before reducer
      coverage lands.
    - [x] `ML-VK-010-004-001` implement Vulkan `Float32` batched-reduction
      coverage for `ml/linear/batched-reduce` with no-CPU-fallback
      regressions.
      - [x] `AUDIT-ML-VK-010-004-001-PUBLIC-DOCS` document
        `ml/linear/batched-reduce` in public language/reference docs with
        explicit contract and argument shape expectations.
      - [x] `AUDIT-ML-VK-010-004-001-CAPABILITY-TRUTH` confirm the new
        operation remains on the `ml-linear-direct-float32` narrow path and does
        not imply broad `ml-linear` Vulkan readiness.
      - [x] `AUDIT-ML-VK-010-004-001-DIAGNOSTICS` verify tensor backend
        diagnostics are emitted for unsupported layouts, mixed devices, and
        view-backed/cross-backend operands before any CPU fallback path.
    - [x] `ML-VK-010-005` decide and implement expression-backed Vulkan
      `ml/linear` lowering beyond already-materialized direct map results, or
      keep the concrete/view boundary explicit with permanent fail-closed tests
      for view-backed operands.
      - [x] `ML-VK-010-005-DECISION: IMPLEMENT-NARROW-EXPR-BACKEND` allow
        only Vulkan-only expressions that existing Tensor realization lowers
        into concrete dense Vulkan storage.
      - [x] `ML-VK-010-005-EXPR-SCOPE` cover direct concrete inputs,
        supported Vulkan map/contract materialization, and Vulkan transpose
        views; keep arbitrary views and CPU/Vulkan mixes unsupported.
      - [x] `ML-VK-010-005-DIAGNOSTICS` lock `tensor/backend-unsupported` for
        mixed-device and non-Vulkan view operands before CPU fallback.
      - [x] `ML-VK-010-005-CAPABILITY-TRUTH` keep broad Vulkan `ml-linear`
        false and keep `ml-linear-direct-float32` as the partial Float32 lane.
      - [x] `ML-VK-010-005-SPEC-LOCK` document the narrow expression/view
        boundary in the public language/reference docs.
    - [x] `ML-VK-010-006` add Vulkan `Float64` `ml/linear` and
      `ml/linear/batched-reduce` coverage through existing `contract` plus
      bias `map` paths, or record a concrete blocker with fail-closed tests.
      - [x] `ML-VK-010-006-RUNTIME` remove the stale Float32-only Vulkan
        `ml/linear` gate and route both `Float64` and `Float32` through the
        existing `contract` plus optional bias `map` path.
      - [x] `ML-VK-010-006-CAPABILITY` add explicit
        `ml-linear-direct-float64` backend capability reporting without
        widening broad Vulkan `ml-linear`.
      - [x] `ML-VK-010-006-COVERAGE` cover Vulkan `Float64` direct, rank-3,
        bias, mapped bias, mapped source, transpose-view source,
        transpose-view weights, and `ml/linear/batched-reduce` parity.
      - [x] `ML-VK-010-006-DIAGNOSTICS` keep mixed CPU/Vulkan operands and
        unsupported layouts fail-closed with Tensor backend diagnostics.
  - scope:
    - batched matmul;
    - bias add;
    - batched reductions;
    - dense layer forward primitives over explicit Tensor placement.
  - acceptance:
    - `Float32` lands first, then `Float64`;
    - results preserve dtype/device placement;
    - no-LAPACK/no-CPU-fallback probes cover Vulkan operands;
    - shape and broadcast diagnostics match the CPU oracle.

- [x] `ML-VK-020` add Vulkan neural elementwise, reduction, softmax, and loss kernels
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - audit split:
    - [x] `ML-VK-020-001` freeze `ml/relu` as the first public neural
      elementwise surface and add truthful narrow capability bits.
    - [x] `ML-VK-020-002` implement `ml/relu` for `Float64` and `Float32`
      Tensor inputs through existing Tensor `map max x 0`, preserving
      CPU/CUDA/Vulkan placement rules.
    - [x] `ML-VK-020-003` add CPU oracle, Vulkan placement/dtype, and
      unsupported-dtype regressions for `ml/relu`.
    - [x] `ML-VK-020-003-LR` add canonical `ml/leaky-relu(input [negative-slope])`.
      - [x] Support `Float64` and `Float32` tensors through composed `Tensor`
        map kernels: `max(input, 0) + slope * min(input, 0)`.
      - [x] Preserve CPU/CUDA/Vulkan placement with no hidden CPU fallback.
      - [x] Add explicit narrow capability bits
        `ml-neural-leaky-relu-float64` and `ml-neural-leaky-relu-float32`.
      - [x] Add CPU, Vulkan, invalid-slope, and unsupported-dtype regressions.
    - [x] `ML-VK-020-004` add the remaining Float32 activation surfaces:
      `ml/sigmoid`, `ml/tanh`, and tanh-approximation `ml/gelu`, with
      no-hidden-CPU-fallback Vulkan tests.
      - [x] Freeze these as canonical `ml/*` activation surfaces, not aliases
        for bare scientific math primitives.
      - [x] Preserve `Float32` dtype and CPU/CUDA/Vulkan placement through
        composed Tensor `map` kernels.
      - [x] Keep `Float64` transcendental activation policy open under
        `ML-VK-020-005` until validated or fail-closed explicitly.
    - [x] `ML-VK-020-005` define the Float64 transcendental activation path
      as explicit fail-closed behavior until validated approximations land.
      - [x] Keep Float64 `ml/sigmoid`, `ml/tanh`, and `ml/gelu` capability
        bits false for CPU/CUDA/Vulkan ML activation reporting.
      - [x] Add CPU and Vulkan Float64 fail-closed regressions so unsupported
        ML activations cannot silently run through generic math or CPU fallback.
      - [x] Preserve the negative constraint: exact GELU still needs a real
        `erf` backend route or a separate explicit fail-closed surface.
    - [x] `ML-VK-020-006` add canonical `ml/sum`, `ml/mean`, and
      population `ml/variance` as a real CPU reduction layer instead of
      misusing matrix `contract`.
      - [x] Freeze the public surface as `ml/sum`, `ml/mean`, and
        `ml/variance`; reject `tensor/reduce/*` as an unsupported namespace.
      - [x] Support integer axes and array/proper-list multi-axis reductions,
        dropping reduced axes from the result shape and preserving Float64 or
        Float32 dtype.
      - [x] Add CPU oracle coverage, duplicate/empty-axis diagnostics,
        unsupported-dtype checks, and Vulkan fail-closed regression coverage.
      - [x] Add truthful `ml-reduction-float64` and `ml-reduction-float32`
        capability bits: CPU true, CUDA/cuBLAS false, Vulkan tied to the real
        Float64/Float32 reduction kernels.
      - [x] `AUDIT-ML-REDUCTION-DIAGNOSTIC` split Float32 reduction
        overflow/representation failures into `tensor/numeric-overflow`
        instead of reusing `tensor/backend-unsupported`.
      - [x] `AUDIT-ML-REDUCTION-COVERAGE` add CPU regressions for proper-list
        axes and view-input materialization on the reduction path.
    - [x] `ML-VK-020-006-MAX` add canonical CPU `ml/max` as the axis maximum
      reduction needed by stable `logsumexp`/`softmax` implementations.
    - [x] `ML-VK-020-006-VK` add Vulkan axis-reduction kernels for
      `ml/sum`, `ml/mean`, and `ml/variance`.
      - [x] Add one-input Float64/Float32 Vulkan reduction shaders and helper
        dispatch that preserve non-reduced axes without abusing `contract`.
      - [x] add Vulkan Float64/Float32 reduction parity tests that require the
        result tensor to stay on Vulkan and match CPU after `to-device 'cpu`.
      - [x] keep unsupported dtypes fail-closed on Vulkan reductions while
        `ml/logsumexp` and `ml/softmax` remain backend-unsupported on Vulkan.
      - [x] keep `ml-reduction-float64` and `ml-reduction-float32` aligned with
        the Vulkan `float64` and `float32` capability bits.
      - [x] add a rank-3 middle-axis regression proving shared reduction
        indexing for `ml/sum`, `ml/logsumexp`, and `ml/softmax`.
    - [x] `ML-VK-020-006-VK-MAX` add Vulkan axis maximum kernels for
      `ml/max`.
      - [x] Preserve Float64/Float32 dtype and device placement on Vulkan.
      - [x] Add parity tests that stay on Vulkan and match CPU after
        `to-device 'cpu`.
      - [x] Keep unsupported dtypes and mixed-device operands fail closed while
        `ml/logsumexp` and `ml/softmax` remain backend-unsupported on Vulkan.
    - [x] `ML-VK-020-007-A` add CPU stable `ml/logsumexp(input axes)` and
      `ml/softmax(input axis)`.
      - [x] Keep `ml/logsumexp(input axes)` aligned with multi-axis reduction
        parsing and implement it with a max-shifted reduction path.
      - [x] Keep `ml/softmax(input axis)` single-axis only and reject axis
        lists/multi-axis inputs until a product contract says otherwise.
      - [x] Preserve Float64/Float32 dtype, stable large-logit behavior, and
        CUDA/Vulkan fail-closed behavior with focused regressions.
    - [x] `ML-VK-020-007-VK-LSE-F32` add Vulkan Float32
      `ml/logsumexp(input axes)`.
      - [x] Reuse the reduction helper shape contract and compute max-shifted
        `max + log(sum(exp(input - max)))` in a real Vulkan shader.
      - [x] Preserve Float32 dtype and Vulkan placement, with CPU copy-back
        parity tests for row and all-axis reductions.
      - [x] Keep Vulkan Float64 `ml/logsumexp` fail-closed until a validated
        Float64 exp/log policy lands.
      - [x] Split Vulkan `ml/softmax` into a same-shape axis-normalization
        shader item instead of overloading the rank-reducing helper.
    - [x] `ML-VK-020-007-VK-SM-F32` add Vulkan Float32
      `ml/softmax(input axis)`.
      - [x] Add a dedicated same-shape axis-normalization shader/helper that
        computes max-shifted slice denominators without hidden CPU fallback.
      - [x] Preserve Float32 dtype and Vulkan placement, with CPU copy-back
        parity tests for stable row normalization.
      - [x] Keep Vulkan Float64 `ml/softmax` fail-closed until a validated
        Float64 exp/log policy lands.
    - [x] `ML-VK-020-007-VK-MSE` add Vulkan Float64/Float32
      `ml/mean-squared-error(predictions targets)`.
      - [x] Add a dedicated two-input scalar loss shader/helper instead of
        composing public Tensor ops or falling back to CPU.
      - [x] Preserve dtype and Vulkan placement, with CPU copy-back parity
        tests for Float64 and Float32.
      - [x] Map Vulkan Float32 non-finite scalar output to
        `tensor/numeric-overflow`, matching the CPU Float32 write contract.
      - [x] Keep mixed CPU/Vulkan operands fail-closed before fallback.
    - [x] `ML-VK-020-007-B` add canonical `ml/cross-entropy(logits targets axis)`.
      - [x] Decide targets are same-shape probability/one-hot tensors, not
        class-index tensors.
      - [x] Normalize over one explicit class axis and keep target/logit shape
        diagnostics explicit.
    - [x] `ML-VK-020-007-VK-CE-F32` add Vulkan Float32
      `ml/cross-entropy(logits targets axis)`.
      - [x] Add a dedicated fused loss shader/helper instead of composing
        softmax/log/reduction public Tensor ops or falling back to CPU.
      - [x] Preserve Vulkan placement, target probability diagnostics, and
        Float32 non-finite result mapping.
      - [x] Keep Vulkan Float64 cross-entropy fail-closed until a validated
        Float64 exp/log policy lands.
    - [x] `ML-VK-020-007-VK-CE-PAR` replace the direct scalar Vulkan
      cross-entropy kernel with a staged status-preserving loss reduction.
      - [x] Preserve invalid-target diagnostics while reducing per-invocation
        work for large class or batch shapes.
      - [x] Keep the current fused Float32 semantics and no-hidden-CPU-fallback
        contract; do not regress the Vulkan invalid-target tests.
    - [x] `ML-VK-020-007-C` add canonical
      `ml/mean-squared-error(predictions targets)`.
      - [x] Return a scalar loss over all elements and reject shape/dtype
        mismatches with Tensor diagnostics.
      - [x] Keep CUDA fail-closed; Vulkan support is tracked and shipped
        separately by `ML-VK-020-007-VK-MSE`.
    - [x] `ML-VK-020-007-VK-MSE-PAR` replace the single-invocation Vulkan MSE
      reduction with a parallel staged reduction.
      - [x] Current helper no longer computes the whole scalar loss in one
        shader invocation; it emits chunked partial sums and reduces them in
        staged Vulkan passes.
      - [x] Implement a staged partial-sum kernel plus final scalar reduction,
        preserving the current no-CPU-fallback contract and Float32 overflow
        status mapping.
      - [x] Add a larger guarded Vulkan regression that exercises the staged
        path without exceeding the bounded validation policy.
  - scope:
    - activation kernels: `relu`, `leaky-relu`, `sigmoid`, `tanh`, `gelu`;
    - stable `exp`, `log`, `logsumexp`, and `softmax`;
    - axis `sum`, `mean`, `variance`, and `max`;
    - cross-entropy loss.
  - negative constraint:
    - do not reuse invalidated GLSL double-transcendental assumptions for
      Vulkan `Float64`; use validated approximations or fail closed.
    - do not implement softmax/loss by abusing `contract`; use the real
      `ml/sum`/`ml/mean`/`ml/variance` reduction layer and add Vulkan kernels
      before claiming Vulkan reduction capability.

- [x] `ML-VK-030` add Vulkan convolution, pooling, and image-tensor kernels
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - [x] `ML-VK-030-001` ship explicit dense NCW `ml/conv1d` with OCK kernels,
    CPU `Float64`/`Float32`, direct Vulkan `Float32`, grouped convolution,
    narrow `ml-conv1d-direct-*` capability reporting, and mixed-device
    fail-closed regressions. Broad `ml-convolution` remains false.
  - [x] `ML-VK-030-002` ship explicit dense NCHW `ml/conv2d` with OIHW kernels,
    CPU `Float64`/`Float32`, direct Vulkan `Float32`, grouped convolution,
    narrow `ml-conv2d-direct-*` capability reporting, and mixed-device
    fail-closed regressions. Broad `ml-convolution` remains false.
  - [x] `ML-VK-030-003` ship explicit dense NCHW `ml/max-pool2d` and
    `ml/avg-pool2d`, CPU `Float64`/`Float32`, direct Vulkan `Float32`, narrow
    `ml-pool2d-direct-*` capability reporting, and mixed-device fail-closed
    regressions. Broad `ml-convolution` is true for CPU and direct Vulkan
    `Float32` dense convolution/pooling.
  - scope:
    - 1D and 2D convolution;
    - stride, padding, dilation, groups, and explicit batch/channel layout;
    - max and average pooling;
    - backward kernels are owned by `ML-VK-050` after the autograd contract
      lands.
  - acceptance:
    - first supported layout is explicit and capability-gated;
    - arbitrary views/strides remain fail-closed until a separate ABI lands.

- [x] `ML-VK-040` add Vulkan normalization and attention primitives
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - [x] `ML-VK-040-001` add canonical `ml/layer-normalization(input axis [epsilon])`.
    - shipped CPU `Float64`/`Float32` and direct Vulkan `Float32`.
    - preserves input shape and normalizes each explicit axis slice.
    - reports `ml-layer-normalization-float64`,
      `ml-layer-normalization-float32`, and broad `ml-normalization` capability.
    - keeps unsupported CUDA/Vulkan paths fail-closed without CPU fallback.
  - [x] `ML-VK-040-002` add canonical
    `ml/batch-normalization(input scale bias mean variance channel-axis [epsilon])`.
    - shipped CPU `Float64`/`Float32` and direct Vulkan `Float32`/`Float64`.
    - uses explicit rank-1 scale, bias, mean, and variance tensors matching the
      channel axis, preserving DataSpec-style data paths without hidden state.
    - reports `ml-batch-normalization-float64`,
      `ml-batch-normalization-float32`, and broad `ml-normalization` capability.
    - keeps mixed CPU/Vulkan operands fail-closed without CPU fallback.
  - [x] `ML-VK-040-003` add canonical
    `ml/scaled-dot-product-attention(query key value [mask] [scale])`.
    - shipped CPU `Float64`/`Float32` and direct dense Vulkan `Float32`.
    - supports optional additive `[Q K]` or batched masks and positive finite scale.
    - reports `ml-scaled-dot-product-attention-float64`,
      `ml-scaled-dot-product-attention-float32`, and broad `ml-attention` capability.
    - keeps mixed CPU/Vulkan and Vulkan Float64 paths fail-closed without CPU fallback.
  - scope:
    - shipped inference/eval normalization and attention primitives are closed;
    - training-mode/current-batch-stat batch normalization is closed as
      `ML-VK-040-TRAIN-BN-001` through the explicit `nn` stateful layer
      contract;
    - optional fused attention kernels are split to
      `ML-VK-040-FUSED-ATTENTION-001`.

- [x] `ML-VK-040-TRAIN-BN-001` add training-mode/current-batch-stat batch normalization
  - shipped as `nn/batch-normalization(channels channel-axis [options])` with
    explicit `scale`/`bias` params and `running-mean`/`running-variance` state.
  - `nn/apply` uses running stats for inference/eval lowering through
    `ml/batch-normalization`.
  - train-mode `nn/forward` computes current-batch CPU dense row-major
    statistics, returns ordinary `nn-forward` data containing updated state and
    updated model data, and leaves the original model/state immutable.
  - CUDA/Vulkan/current-batch training remains fail-closed without hidden CPU
    fallback; native device backward/state kernels are tracked separately.

- [x] `ML-VK-040-FUSED-ATTENTION-001` add optional fused attention kernels
  - shipped: `ml/scaled-dot-product-attention` already uses a single direct
    dense Vulkan `Float32` attention kernel for the supported Q/K/V and
    optional additive-mask contract, preserving the existing CPU oracle and
    no-hidden-CPU-fallback diagnostics.
  - hardening: added focused Vulkan tests for additive `[Q K]` masks and
    batched rank-3 masks so the direct fused shader path covers the same mask
    families as the primitive contract.
  - guardrails: mixed CPU/Vulkan operands and Vulkan `Float64` still fail
    closed with `tensor/backend-unsupported`; arbitrary dropout/training
    attention is outside the shipped `ml/scaled-dot-product-attention`
    inference/eval contract.
  - validation: `c3c build --obj-out obj`; focused
    `advanced-collections-module` `pass=1933 fail=0`.

- [x] `ML-VK-050` implement reverse-mode Tensor autograd with explicit Vulkan semantics
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - [x] `ML-VK-050-001` add canonical `ml/grad` data gradient spec evaluator.
    - ships CPU `Float64`/`Float32` `linear-mean-squared-error` gradients.
    - returns loss, output, input gradient, and weights/bias gradients as data.
    - keeps Vulkan backward fail-closed until real Vulkan gradient kernels land.
  - [x] `ML-VK-050-002` add CPU `linear-activation-mean-squared-error` gradients.
    - ships `relu` backward for `Float64`/`Float32` and `sigmoid`/`tanh`/`gelu` backward for `Float32`.
    - keeps CUDA/Vulkan backward fail-closed until real device kernels land.
  - [x] `ML-VK-050-003` add CPU `linear-softmax-cross-entropy` gradients.
    - ships stable softmax CE logits gradients for dense linear classifiers; keeps device backward fail-closed.
  - [x] `ML-VK-050-004` add CPU `leaky-relu` backward coverage for
        `linear-activation-mean-squared-error`.
    - ships `Float64`/`Float32` backward support, default `negative-slope`
      `0.01`, custom `negative-slope` validation for direct `ml/grad` specs,
      and matching default-slope `nn/leaky-relu` DataSpec lowering.
    - keeps CUDA/Vulkan backward fail-closed until real device kernels land.
  - [x] `ML-VK-050-005` freeze the reusable gradient tape representation and
        lifetime rules.
    - shipped: `ml/grad` results now include a versioned metadata-only
      `gradient-tape` dictionary with ordinary scope-region-owned `nodes`,
      `policy 'metadata-only`, `owner 'scope-region`, `retains-handles nil`,
      dtype/source metadata, activation metadata when present, and no hidden
      native handles or executor state.
    - boundary: this freezes the reusable tape contract for shipped gradient
      specs; Vulkan backward semantics remain open under `ML-VK-050-007`.
  - [x] `ML-VK-050-006` add CPU generic Tensor gradient accumulation for map,
        contract, softmax, and loss operations.
    - shipped: `ml/grad` now accepts `tensor-mean-squared-error` and
      `tensor-softmax-cross-entropy` specs over dense row-major CPU
      `Float64`/`Float32` Tensor expressions.
    - shipped: reverse accumulation walks `map` and `contract` expression
      nodes, computes MSE and softmax-cross-entropy upstream gradients, rejects
      unsupported map backward rules, and requires a single unambiguous `wrt`
      leaf match.
  - [x] `ML-VK-050-007` add explicit Vulkan backward availability semantics
        for shipped Vulkan forward ops.
    - prerequisite: closed `ML-VK-050-006` generic CPU backward rule inventory.
    - shipped: Tensor-expression `ml/grad` now preflights non-CPU operands
      before `wrt` leaf matching, so Vulkan forward tensors without matching
      backward kernels fail closed under `tensor/backend-unsupported` instead
      of falling through to ambiguous/unreachable-CPU diagnostics.
    - shipped: focused coverage asserts tensor-expression MSE and
      softmax-cross-entropy over Vulkan `Float32` forward tensors fail closed,
      and broad `ml-autograd` stays false for CPU/CUDA/cuBLAS/Vulkan until a
      complete backend autograd family exists.
  - scope:
    - shipped metadata-only gradient tape representation and lifetime rules as
      the base contract;
    - CPU gradient accumulation for map/contract/softmax/loss without hidden
      transfers;
    - fail-closed behavior when a CUDA/Vulkan forward op has no backend
      backward kernel.
  - constraint:
    - preserve the region/scope memory model; do not add per-Tensor ownership.

- [x] `ML-VK-050-VK-BWD-001` add the first native Vulkan backward kernel
  - shipped: `ml/grad` now has a native Vulkan dense row-major `Float32`
    `tensor-mean-squared-error` backward slice when `wrt` is the concrete
    prediction tensor.
  - behavior: loss and input-gradient stay on Vulkan; the gradient computes
    `(prediction - targets) * (2 / element-count)` through existing Vulkan
    map kernels, and the returned metadata-only tape records `device 'vulkan`.
  - guardrails: unsupported Vulkan graph backward outside shipped native slices
    still fails closed with `tensor/backend-unsupported`; broad backend
    `ml-autograd` remains false.
  - validation: `c3c build --obj-out obj`, focused
    `advanced-collections-module` `pass=1924 fail=0`,
    `scripts/check_file_size_gate.sh`, and `git diff --check`.

- [x] `ML-VK-050-VK-MAP-BWD-001` add graph-preserving Vulkan map-expression backward
  - shipped: eager Vulkan `map` now records scope-safe provenance on concrete
    `map` results without making global Vulkan `map` lazy, and
    `tensor-mean-squared-error` `ml/grad` consumes that provenance for
    same-shape dense row-major `Float32` Vulkan `map +`, `map -`, and `map *`
    paths.
  - behavior: gradients stay on Vulkan for direct wrt, `map +`, `map *`
    scalar, and `map *` tensor cases; unsupported map operators still fail
    closed with `tensor/backend-unsupported`.
  - guardrails: concrete tensors carrying map provenance copy/promote/cleanup
    their child edges through the existing scope/region boundary helpers; no
    per-Tensor ownership model or broad backend `ml-autograd` bit was added.
  - validation: `c3c build --obj-out obj`, focused
    `advanced-collections-module` `pass=1927 fail=0`.

- [x] `ML-VK-050-VK-MAP-BCAST-BWD-001` add Vulkan map backward reductions for broadcasted operands
  - shipped: Vulkan map-provenance backward now reduces upstream gradients back
    to broadcasted wrt shapes through native Vulkan `ml/sum` reduction instead
    of CPU fallback.
  - behavior: dense row-major `Float32` Vulkan `tensor-mean-squared-error`
    gradients now support leading-axis, inner singleton-axis, and rank-0 wrt
    broadcasts for `map +`/`map *` provenance; the shared guard also allows
    broadcast-compatible wrt leaves for Vulkan softmax-CE map backward.
  - guardrails: unsupported dtypes, placements, operators, duplicate wrt
    leaves, and non-broadcast-compatible shapes still fail closed with
    `tensor/backend-unsupported` or `tensor/shape-mismatch`; broad backend
    `ml-autograd` remains false.
  - validation: `c3c build --obj-out obj`; focused
    `advanced-collections-module` `pass=1931 fail=0`;
    `scripts/check_file_size_gate.sh`; `git diff --check`.

- [x] `ML-VK-050-VK-SOFTMAX-CE-BWD-001` add native Vulkan tensor softmax-cross-entropy backward
  - shipped: `tensor-softmax-cross-entropy` `ml/grad` now supports dense
    row-major `Float32` Vulkan logits/targets for direct `wrt == logits` and
    same-shape map-provenance `wrt` paths.
  - behavior: loss, softmax output, and input-gradient stay on Vulkan; the
    upstream gradient is `(softmax(logits) - targets) / slice-count` through
    existing native Vulkan softmax, cross-entropy, and map kernels.
  - guardrails: broad backend `ml-autograd` remains false; unsupported dtypes,
    mixed placement, unsupported expression graphs, and broadcast map
    reductions still fail closed.
  - validation: `c3c build --obj-out obj`, focused
    `advanced-collections-module` `pass=1928 fail=0`.
- [x] `ML-VK-050-VK-BWD-DIV-001` add Vulkan `map /` backward rule for MSE
  - shipped: left-path gradient is `upstream / right_operand`; right-path
    gradient computes `-output / right_operand` then multiplies by upstream.
  - uses existing generic Vulkan map primitives without a dedicated shader.
  - supports dense row-major `Float32` and `Float64`.
- [x] `ML-VK-050-VK-BWD-F64-001` add Vulkan `Float64` backward for MSE and softmax-CE
  - shipped: scalar creation uses `make_double` for `Float64`; dtype gates
    accept `TENSOR_DTYPE_DOUBLE` in both MSE and softmax-CE Vulkan backward.
- [x] `ML-VK-050-VK-BWD-MIXED-001` add mixed-device auto-migration for Vulkan backward targets
  - shipped: `tensor-mean-squared-error` and `tensor-softmax-cross-entropy`
    `ml/grad` auto-migrate CPU targets to Vulkan via
    `tensor_expr_resolve_vulkan_or_migrate` before backward computation.
  - predictions/logits and wrt remain strict Vulkan to preserve provenance.
  - keeps gradients on-device and avoids silent CPU fallback.
- [x] `ML-VK-050-VK-BWD-MINMAX-001` add `map min` / `map max` backward rules
  for MSE
  - shipped: CPU Tensor-expression `ml/grad` now accumulates min/max map
    gradients with comparison masks.
  - shipped: Vulkan dense row-major `Float32` and `Float64` MSE map backward
    now supports `min` and `max`, using existing Vulkan binary map comparison
    mask ops and keeping gradients on-device.
  - validation: `c3c build --obj-out obj`, direct CPU/Vulkan eval probes.
    Focused `advanced-collections-module` rerun improved from `pass=1994
    fail=22` to `pass=1996 fail=20`; remaining failures are pre-existing
    Vulkan contract expectation drift outside this slice.

- [x] `ML-VK-060` add Vulkan-capable optimizer suite
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - [x] `ML-VK-060-001` add CPU `ml/sgd-step` for data-oriented parameter trees.
  - [x] `ML-VK-060-002` add CPU `ml/optimizer-step` for SGD momentum state.
  - [x] `ML-VK-060-003` add CPU `ml/optimizer-step` for Adam and AdamW specs.
  - [x] `ML-VK-060-004` add CPU `ml/optimizer-step` for RMSProp specs.
  - [x] `ML-VK-060-005` add CPU `ml/clip-gradients` and optimizer `clip-norm`.
  - [x] `ML-VK-060-006` add CPU `ml/save-optimizer` and `ml/load-optimizer`
        checkpoint helpers for explicit optimizer spec/state data.
  - [x] `ML-VK-060-007` add Vulkan dense row-major `Float32` SGD optimizer
        kernels with explicit velocity state and `ml-optimizer-sgd-float32`
        capability reporting.
  - [x] `ML-VK-060-008` add Vulkan dense row-major `Float32` Adam and AdamW
        optimizer kernels with explicit first/second moment state and narrow
        `ml-optimizer-adam-float32` / `ml-optimizer-adamw-float32` capability
        reporting.
  - [x] `ML-VK-060-009` add Vulkan dense row-major `Float32` RMSProp optimizer
        kernels with explicit square-average/velocity state and narrow
        `ml-optimizer-rmsprop-float32` capability reporting.
  - [x] `ML-VK-060-010` add Vulkan dense row-major `Float32` gradient clipping
        through `ml/clip-gradients` for all-vulkan gradient trees, with
        `ml-clip-gradients-float32` capability reporting and `clip-norm`
        semantics before Vulkan optimizer updates.
  - [x] `ML-VK-060-011` add CUDA dense row-major `Float32` `ml/optimizer-step` SGD execution
        backed by existing CUDA elementwise map kernels, with momentum/velocity
        state and mixed-device fail-closed checks.
        - shipped contract: `tensor-backends` exposes `ml-optimizer-sgd-float32` on
          CUDA when `Float32` elementwise map kernels are available; `ml-optimizer` remains false.
  - [x] `ML-VK-060-012` add CUDA dense row-major `Float32` map-backed
        `ml/optimizer-step` Adam and AdamW execution with first/second moment
        state, explicit step preservation, and mixed-device fail-closed checks.
        - shipped contract: CUDA `ml-optimizer-adam-float32` and
          `ml-optimizer-adamw-float32` follow `elementwise-map-float32`;
          broad `ml-optimizer` remains false.
  - [x] `ML-VK-060-013` add CUDA dense row-major `Float32` map-backed
        `ml/optimizer-step` RMSProp execution with square-average/velocity
        state initialization and continuation plus mixed-device fail-closed checks.
        - shipped contract: CUDA `ml-optimizer-rmsprop-float32` follows
          `elementwise-map-float32`; broad `ml-optimizer` remains false.
  - [x] `ML-VK-060-014` route `ml/sgd-step` through existing CUDA/Vulkan dense
        row-major `Float32` stateless SGD kernels for all-device parameter and
        gradient leaves.
        - shipped contract: `ml/sgd-step` preserves CUDA/Vulkan placement when
          the corresponding `ml-optimizer-sgd-float32` capability is present,
          while mixed CPU/device leaves still fail closed with
          `tensor/backend-unsupported`.
  - [x] `ML-VK-060-F64-CAP-001` fix Vulkan optimizer `Float64` capability bits
        - changed stale hardcoded `false` to `vulkan_float64_available` for
          narrow `ml-optimizer-{sgd,adam,adamw,rmsprop}-float64` bits.
  - closure note:
    - Vulkan and map-backed CUDA optimizer execution plus `nn/train-step`
      integration are shipped for the current explicit-state optimizer suite.
    - native fused CUDA optimizer kernels are split to
      `ML-VK-060-FUSED-CUDA-001` as a performance boundary.
  - acceptance:
    - optimizer state keeps dtype/device placement explicit;
    - Adam/AdamW treats explicit `nil` moment entries as absent optional state
      on CPU, CUDA, and Vulkan, while one-sided present moment state is invalid;
    - Vulkan SGD honors the public non-negative learning-rate contract,
      including `learning-rate 0.0` no-op updates;
    - mixed-device parameter groups fail closed unless transfer is explicit.

- [x] `ML-VK-060-FUSED-CUDA-001` add a native fused CUDA SGD optimizer kernel beyond map-backed execution
  - shipped: dense row-major `Float32` CUDA `ml/optimizer-step` SGD now tries a
    native fused helper before the previous map-backed route.
  - shipped contract: one CUDA kernel computes weight decay, optional momentum
    velocity initialization/continuation, updated parameters, and updated
    velocity state for all-CUDA `Float32` SGD leaves.
  - fallback contract: the map-backed CUDA route remains available only when the
    native CUDA optimizer module is unavailable; a resolved native fused SGD
    launch/allocation failure is reported as a CUDA backend error instead of
    being hidden by a map fallback.
  - validation: `scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`,
    and focused advanced collections `1933 passed, 0 failed`.

- [x] `ML-VK-060-FUSED-CUDA-STATEFUL-001` add native fused CUDA Adam/AdamW/RMSProp optimizer kernels
  - shipped: dense row-major `Float32` CUDA Adam, AdamW, and RMSProp optimizer
    leaves now route through native fused multi-output CUDA kernels when the
    optimizer PTX module resolves.
  - shipped contract: Adam/AdamW compute updated parameters plus first- and
    second-moment state in one kernel; RMSProp computes updated parameters,
    square-average state, and optional velocity state in one kernel.
  - fallback contract: map-backed CUDA optimizer chains remain available only
    when the native CUDA optimizer module is unavailable; a resolved native
    fused Adam/AdamW/RMSProp launch/allocation failure is reported as a CUDA
    backend error instead of being hidden by map fallback.
  - validation: CUDA PTX generation and `ptxas` assembly passed,
    `scripts/build_omni_chelpers.sh`, `c3c build --obj-out obj`, direct CUDA
    Adam/AdamW/RMSProp probes, and focused advanced collections
    `2000 passed, 0 failed`.

- [x] `ML-VK-070` add backend-neutral model/layer library and serialization
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - design: `docs/plans/omni-neural-dataspec-plan-2026-04-20.md`
  - official design name: Omni Neural DataSpec
  - scope:
    - linear, convolution, embedding, and normalization layers;
    - sequential/composition helpers;
    - parameter traversal;
    - checkpoint serialization and loading.
  - constraint:
    - Vulkan execution must follow Tensor placement and capability reporting;
      do not add backend-specific public layer names.
    - `nn/*` layer constructors return validated data specs, not hidden mutable
      objects or class instances.
    - user-facing ergonomics may bundle spec/params/state/options in a
      transparent model value, but advanced APIs must expose explicit
      spec/params/state execution.
  - slices:
    - [x] `ML-VK-070-001` freeze Omni Neural DataSpec schemas and add
      `nn/validate` diagnostics for sequential, dense, activation, flatten,
      convolution, pooling, model, parameter tree, and state tree data.
      - shipped: `nn/validate` returns valid specs and raises
        `nn/invalid-spec` with path/expected/actual-kind diagnostics for
        invalid specs.
      - shipped: data-only constructors for sequential, dense, conv1d,
        conv2d, max-pool2d, avg-pool2d, flatten, and activation specs.
      - not included: parameter allocation, inference lowering, checkpoint
        round trips, and training helpers; those remain in later slices.
    - [x] `ML-VK-070-002` add deterministic parameter initialization for
      CPU/CUDA/Vulkan tensors with explicit dtype/device placement.
      - shipped: `nn/init(spec [options])` builds explicit `params` and `state`
        trees, stores `mode`/`dtype`/`device`/`metadata` on the transparent
        model bundle, and uses deterministic seeded initialization and explicit
        device placement for dense, conv1d, and conv2d parameter tensors.
    - [x] `ML-VK-070-003` add `nn/apply`, `nn/predict`, and `nn/summary` for
      inference over `ml/*` primitives with no hidden Vulkan CPU fallback.
      - shipped: `nn/apply` supports model and explicit `(spec params state)` paths;
        `nn/predict` requires `'eval` mode; first-lane lowering covers dense,
        conv1d/conv2d, pooling, activation, softmax, and CPU flatten; and
        unsupported device/layout convolution-bias paths fail closed without hidden
        CPU fallback.
    - [x] `ML-VK-070-004` add spec/model checkpoint save/load round trips for
      spec, params, state, dtype, shape, paths, and placement metadata.
      - shipped: `nn/save-spec`/`nn/load-spec` round-trip non-model DataSpecs
        through checkpoint strings or paths, and `nn/save`/`nn/load` round-trip
        transparent model bundles with tensor dtype, shape, data, and recorded
        placement metadata restored through explicit device transfer.
    - [x] `ML-VK-070-005` add the training facade after `ML-VK-050` autograd
      and `ML-VK-060` optimizers: `nn/forward`, `nn/grad`, and
      `nn/train-step` returning updated data values.
      - shipped: `nn/forward` delegates through the same data execution surface
        as `nn/apply` without requiring eval mode; stateful BN now returns
        explicit updated state/model data through `ML-VK-040-TRAIN-BN-001`.
      - shipped: `nn/grad` requires train-mode dense or sequential
        dense-plus-activation model data and lowers to the existing CPU
        `ml/grad` linear MSE / softmax cross-entropy contracts.
      - shipped: `nn/train-step` composes `nn/grad` with `ml/optimizer-step`
        and returns updated model data, optimizer state, gradients, loss, and
        output without hidden mutation.
      - shipped: CUDA/Vulkan backward remains fail-closed before hidden CPU
        fallback through the underlying `ml/grad` contract.
    - [x] `ML-VK-070-006` add ergonomic optimizer spec constructors
      `nn/sgd`, `nn/adam`, `nn/adamw`, and `nn/rmsprop`.
      - shipped: constructors validate hyperparameter ranges and unknown
        option keys, return ordinary optimizer spec dictionaries, and compose
        directly with `nn/train-step` / `ml/optimizer-step`.
    - [x] `ML-VK-070-007` add default-slope `nn/leaky-relu` DataSpec support.
      - shipped: `nn/leaky-relu` constructs a normalized activation spec,
        `nn/validate` accepts `leaky-relu`, and `nn/apply`/`nn/predict` lower
        it through `ml/leaky-relu`.

- [x] `ML-VK-080` add Vulkan ML graph capture, fusion, and memory planning
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - shipped:
    - [x] `ML-VK-080-001` add the data-oriented `Kernel` value surface.
      - shipped: `Kernel(spec)` validates recognized keys, required backend,
        operation, IO descriptors, optional push constants, and optional
        workgroup; normalizes `kind` to `'kernel`; reports `type-of` / `is?`
        as `Kernel`; preserves ordinary path/index/ref data access; and adds
        explicit `kernel/run`.
    - [x] `ML-VK-080-002` add the first checked Vulkan `kernel/run` execution
      route.
      - shipped: `operation 'scale-f32` launches the existing Vulkan Float32
        scale helper for one dense row-major Vulkan tensor input, one matching
        output descriptor, and a `scale` Float32 push constant; unsupported
        custom compilation/fusion routes remain fail-closed.
    - [x] `ML-VK-080-003` add checked Vulkan `add-f32` `kernel/run` execution.
      - shipped: `operation 'add-f32` launches the existing Vulkan Float32 map
        helper for two same-shape dense row-major Vulkan tensor inputs and one
        matching output descriptor; push constants are rejected and unsupported
        custom compilation/fusion routes remain fail-closed.
    - [x] `ML-VK-080-004` expand checked Vulkan binary Float32 `kernel/run`
      execution.
      - shipped: `operation 'sub-f32`, `mul-f32`, `div-f32`, `min-f32`, and
        `max-f32` share the same two-input dense row-major Vulkan Float32
        contract as `add-f32`; all reject push constants and preserve Vulkan
        output placement.
    - [x] `ML-VK-080-005` add checked Vulkan scalar Float32 `kernel/run`
      execution.
      - shipped: tensor-scalar `add-scalar-f32`, `sub-scalar-f32`,
        `mul-scalar-f32`, `div-scalar-f32`, `min-scalar-f32`, and
        `max-scalar-f32`, plus scalar-left `scalar-sub-f32` and
        `scalar-div-f32`; all require one dense row-major Vulkan Float32 tensor
        input and one `scalar` Float32 push constant.
    - [x] `ML-VK-080-006` add checked Vulkan unary Float32 `kernel/run`
      execution.
      - shipped: unary `abs-f32`, `neg-f32`, `sqrt-f32`, `identity-f32`,
        `zero-f32`, `sin-f32`, `cos-f32`, `tan-f32`, `asin-f32`, `acos-f32`,
        `atan-f32`, `sinh-f32`, `cosh-f32`, `tanh-f32`, `exp-f32`,
        `log-f32`, `log10-f32`, and `normal-cdf-f32`; all require one dense
        row-major Vulkan Float32 tensor input, one matching output descriptor,
        and no push constants.
    - [x] `ML-VK-080-007` add checked Vulkan single-node Kernel graph capture.
      - shipped: `kernel/capture` validates the checked direct-helper Vulkan
        Float32 Kernel families against runtime inputs and push data, then
        returns a single-node `kernel-graph` with backend, operation, family,
        dtype, device, direct-helper execution, input/output names, concrete
        runtime shape, push data, node data, and invalidation key. It does not
        launch the kernel.
    - [x] `ML-VK-080-008` add Vulkan Float32 Tensor map graph capture.
      - shipped: `tensor/capture(source)` returns a non-executing
        `tensor-graph` for all-Vulkan Float32 concrete/map Tensor expressions,
        including source/map nodes, scalar operands, output id, shape, and
        invalidation metadata. Explicit `to-device 'vulkan` preserves supported
        CPU lazy Float32 map graphs as Vulkan expressions for capture.
    - [x] `ML-VK-080-009` add Vulkan Float32 Tensor contract graph capture.
      - shipped: `tensor/capture(source)` records all-Vulkan Float32
        concrete/map/contract Tensor expression graphs; contract nodes include
        input ids, left/right axes, axis count, output shape, dtype/device, and
        backend metadata. Explicit `to-device 'vulkan` preserves supported CPU
        lazy Float32 contract graphs as Vulkan expressions for capture.
    - [x] `ML-VK-080-010` add Vulkan Float32 direct transpose-view graph
      capture.
      - shipped: `tensor/capture(source)` records direct rank-2
        `matrix/transpose-view` nodes over concrete dense Vulkan Float32
        storage, including input id, output shape, strides, storage offset,
        dtype/device, and backend metadata. Arbitrary strided views remain
        fail-closed.
    - [x] `ML-VK-080-011` add non-executing Tensor graph schedule metadata.
      - shipped: captured Tensor graphs now include topological schedule
        entries, per-node execution classes, launch booleans, graph
        `launch-count`, and `fusion 'none`. Scheduling is descriptive only and
        does not launch graph nodes.
    - [x] `ML-VK-080-012` add non-executing Tensor command-batch planning
      metadata.
      - shipped: captured Tensor graphs now record non-executing command-batch
        planning metadata for launchable `direct-helper` schedule steps.
        Launchable graphs report `command-batching 'metadata` with one serial
        `command-buffer-candidate` batch descriptor; non-launching graphs report
        `command-batching 'none`. Capture does not allocate, record, submit, or
        execute Vulkan command buffers, and it does not fuse graph nodes.
    - [x] `ML-VK-080-013` add metadata-only Tensor memory-plan records.
      - shipped: captured Tensor graphs now record a nested metadata-only
        `memory-plan` dictionary with kind `tensor-memory-plan`, `version`,
        `backend`, `dtype`, `policy`, `allocates`, `retains-handles`,
        `external-bytes`, `transient-bytes`, and `node-memory`. Capture nodes
        record `element-count`, `byte-length`, `storage-offset`,
        `storage-elements`, `storage-bytes`, `allocation`, `owner`, and
        `write-policy`; the top-level capture result remains a `tensor-graph`.
      - contract: metadata only; no allocations, no handle retention, and no
        runtime buffer reuse.
    - [x] `ML-VK-080-014` add metadata-only Tensor fusion eligibility records.
      - shipped: captured Tensor graphs now record a nested metadata-only
        `fusion-plan` dictionary with kind `tensor-fusion-plan`, policy
        `eligibility-only`, map-chain candidates for direct Vulkan Float32 map
        chains, and barrier records for contract/view nodes.
      - contract: top-level `fusion` remains `none`; no fused shader
        compilation, no command-buffer recording, no fused dispatch, and no
        runtime buffer reuse.
    - [x] `ML-VK-080-015` implement executable Vulkan Float32 two-scalar-map
      command-buffer batching.
      - shipped: `realize` now executes supported Vulkan `Float32`
        two-scalar-map expression chains as one recorded command buffer with an
        intermediate shader-write/read barrier and one queue submission.
      - contract: this consumes the narrow expression-materialization path, not
        arbitrary `tensor/capture` graph dictionaries; no hidden CPU fallback
        and no `(define [kernel] ...)` sugar were added.
    - [x] `ML-VK-080-016` implement registered-source custom `Kernel`
      dispatch for Vulkan `Float32`.
      - shipped: `kernel/run` now consumes `Kernel.source`/`Kernel.entry` for
        `source-scale-f32` with registered source `ml-clip-scale-f32`, dispatching
        through the generic Vulkan shader-module helper path.
      - contract: unsupported source names, non-`main` entries, text compilation,
        and arbitrary source languages fail closed.
    - [x] `ML-VK-080-017` implement contracted metadata-only buffer reuse and
      lifetime planning.
      - shipped: nested `memory-plan` version 2 records transient reuse groups,
        reuse policy, runtime ownership, and explicit `executes-reuse false`.
      - contract: this plans reuse candidates only; it does not allocate,
        retain, alias, transfer ownership, or reuse runtime Vulkan buffers.
    - [x] `ML-VK-080-018` implement captured Tensor source/map graph execution.
      - shipped: `tensor/run(graph)` validates captured all-Vulkan `Float32`
        graph dictionaries and replays concrete source nodes plus map nodes
        through existing Vulkan helper dispatch.
      - contract: fused dispatch, command-buffer execution from capture
        metadata, and hidden CPU fallback remain closed.
    - [x] `ML-VK-080-019` implement checked direct SPIR-V source data
      validation for custom `Kernel`.
      - shipped: `Kernel.source` accepts registered-source symbols, checked
        registered-source dictionaries, and direct SPIR-V dictionaries with
        unsigned 32-bit word arrays. Direct SPIR-V word arrays validate header
        magic, version, id bound, and schema and then fail closed for runtime
        execution.
      - contract: validating SPIR-V as data is not backend compilation or
        executable arbitrary source dispatch.
    - [x] `ML-VK-080-020` implement the first runtime buffer reuse/ownership
      transfer boundary for captured Tensor graph execution.
      - shipped: `tensor/run(graph)` detects the captured all-Vulkan `Float32`
        source -> scalar map -> scalar map graph shape and routes it through
        the native Vulkan command-buffer batch helper. The helper owns its
        intermediate scratch buffer internally and transfers one final output
        handle to a fresh Tensor.
      - contract: this does not mutate or alias `TensorVal.device_handle`, does
        not execute general `memory-plan` reuse, and does not implement broad
        graph buffer pooling.
    - [x] `ML-VK-080-021` implement captured Tensor contract/direct-view graph
      replay semantics.
      - shipped: `tensor/run(graph)` now validates and executes captured
        all-Vulkan `Float32` source/map, contract, and direct
        `matrix/transpose-view` nodes through existing Vulkan helpers.
      - contract: arbitrary strided view graphs, view-consuming map/contract
        materialization, fused dispatch, command-buffer execution from capture
        metadata, and hidden CPU fallback remain closed.
    - [x] `ML-VK-080-022` implement executable direct SPIR-V or source-language
      custom `Kernel` compilation.
      - shipped: `kernel/run` now executes validated direct SPIR-V word-array
        sources for the checked Vulkan `source-scale-f32` ABI. C3 copies the
        `Kernel.source.words` array into native `uint32_t` scratch and the
        native backend builds a compute pipeline from those words for the
        two-buffer scale dispatch.
      - contract: direct SPIR-V execution is currently limited to the
        `source-scale-f32` descriptor/push ABI with entry `main`; arbitrary
        source languages, arbitrary descriptor layouts, and unvalidated shader
        interfaces remain fail closed.
    - [x] `ML-VK-080-029` implement source-language custom `Kernel`
      compilation or a broader direct-SPIR-V ABI contract.
      - shipped: direct SPIR-V source dictionaries now have an explicit ABI
        contract. The checked `source-scale-f32-v1` ABI is accepted for
        `format 'spirv` word-array sources, omitted ABI remains the compatible
        spelling for that same checked scale ABI, and any other direct-SPIR-V
        ABI fails closed during `Kernel` validation.
      - contract: no source-language compiler or arbitrary descriptor-layout
        runtime execution was introduced. Those broader semantics are split
        below instead of being hidden behind the direct-SPIR-V scale ABI.
    - [x] `ML-VK-080-030` implement a second checked direct-SPIR-V
      custom `Kernel` ABI beyond `source-scale-f32-v1`.
      - shipped: `kernel/run` now executes registered and direct word-array
        unary `Float32` source kernels through the checked
        `source-unary-f32-v1` ABI. The registered builtin source name is
        `map-unary-f32`; direct sources must declare
        `abi 'source-unary-f32-v1`.
      - contract: the unary ABI is one Vulkan `Float32` input, one output,
        entry `main`, no push dictionary entries, and native push constants
        `{count, op, padding0, padding1}` selected from the existing unary
        operation family. Unsupported source formats, unsupported direct
        ABIs, arbitrary descriptor layouts, and text source compilation still
        fail closed.
    - [x] `ML-VK-080-033` implement a checked source-binary direct-SPIR-V
      `Kernel` ABI.
      - shipped: `kernel/run` now executes direct word-array binary
        `Float32` sources that declare `abi 'source-binary-f32-v1`.
        The checked ABI is two Vulkan `Float32` storage inputs, one Vulkan
        `Float32` output, entry `main`, no push dictionary entries, and native
        push constants `{count, padding0, padding1, padding2}`. Optional
        source metadata may declare `storage2-output1-f32-v1`; other metadata
        layouts fail closed during `Kernel` validation.
      - validation note: constructor/fail-closed coverage lives in the
        focused advanced suite. Runtime direct binary SPIR-V execution is
        verified by a standalone `--eval` probe because placing a large inline
        source-binary word-array execution fixture in the long advanced
        harness corrupts the JIT/apply path after the preceding direct
        scale/unary SPIR-V fixtures.
      - contract: source-language parsing/compilation, reflection, arbitrary
        descriptor schemas, and non-checked direct-SPIR-V ABIs remain closed.
    - [x] `ML-VK-080-036` consolidate the remaining Kernel source,
      selected-region planner, and command-lowering work into one semantic
      closure boundary.
      - churn trigger: the source/custom Kernel lane, selected-region runtime
        reuse lane, and command-buffer lowering lane each accumulated repeated
        case-specific slices while preserving the same broader capability as a
        residual.
      - shipped: checked direct scale/unary/binary SPIR-V sources now share a
        `kernel-source-layout` metadata dictionary contract that validates ABI,
        descriptor layout, dtype, input/output counts, and optional push layout
        at construction and before runtime dispatch. The legacy checked binary
        `metadata 'storage2-output1-f32-v1` spelling remains accepted only for
        the binary ABI.
      - shipped: `tensor/capture(source)` now records a top-level
        `selected-region-plan` with metadata-only native lowering candidates
        for scalar-map chains, tensor-map chains, direct-view scalar regions,
        contract scalar regions, and the concrete mixed view/dense-source
        region. Each candidate names the consumed command-buffer batch,
        dispatch ids, dependencies, launch/dispatch/barrier counts, and
        `native-selected-region-executor` runtime support.
      - shipped: `tensor/run(graph)` now requires a matching
        `selected-region-plan` candidate plus validated command-batch metadata
        before entering any native selected-region executor. Missing or
        corrupted selected-region plan metadata skips the native route and
        falls back to serial Vulkan graph replay, preserving existing numeric
        behavior without inferring native lowering from node shape alone.
      - contract: source-language parsing/compilation, reflection, arbitrary
        direct-SPIR-V descriptor schemas beyond checked scale/unary/binary
        ABIs, memory-plan-backed runtime reuse, arbitrary mixed schedules, and
        fused dispatch execution remain future capability boundaries, not
        unresolved case-specific children of this item.
    - [x] `ML-VK-080-023` lower captured scalar-map command-batch metadata to
      the runtime executor.
      - shipped: `tensor/run(graph)` validates the captured
        `command-buffer-candidate` batch for linear scalar-map graphs before
        routing to the native scalar-chain executor. If the command-batch
        metadata is missing or invalid, `tensor/run` falls back to serial
        helper replay instead of silently treating node shape alone as an
        executable batch.
      - contract: this does not lower contract/view command batches,
        tensor/tensor map regions, fused dispatch, or arbitrary graph
        schedules.
    - [x] `ML-VK-080-024` generalize runtime reuse to captured linear scalar
      map chains.
      - shipped: the native Vulkan scalar-map-chain executor accepts arrays of
        scalar map operations, records the chain into one command buffer, owns
        all intermediate scratch buffers, transfers one final output handle,
        and is used by `tensor/run(graph)` for captured source -> scalar-map*
        graphs with two or more map nodes.
      - contract: this still does not execute tensor/tensor map chains,
        contract/view-consuming regions, general `memory-plan` reuse, or C3
        handle aliasing.
    - [x] `ML-VK-080-025` generalize runtime reuse beyond linear scalar map
      chains.
      - shipped: `tensor/run(graph)` validates captured all-Vulkan `Float32`
        tensor/tensor map -> scalar-map* regions with two concrete source
        tensors and routes them through a native Vulkan selected-region
        executor. The executor records the tensor/tensor map and following
        scalar maps into one command buffer, owns all intermediate scratch
        buffers, transfers one final output handle, and leaves captured source
        tensors unchanged.
      - contract: this covers dense same-shape tensor/tensor maps followed by
        scalar maps only. Contract/view-consuming regions, multi-source DAGs,
        general `memory-plan` reuse, and C3 handle aliasing remain closed.
    - [x] `ML-VK-080-027` generalize selected-region runtime reuse beyond
      tensor-map plus scalar tails.
      - shipped: `tensor/run(graph)` now recognizes captured all-Vulkan
        `Float32` contract -> scalar-map* selected regions with two concrete
        source tensors, validates the captured region metadata, and routes the
        region through a native Vulkan executor that owns the contract
        intermediate and scalar scratch buffers internally while transferring
        one final output handle.
      - contract: this covers contract regions followed by scalar maps only;
        direct-view consumers, mixed DAGs, general `memory-plan` execution,
        and C3 handle aliasing remain closed.
    - [x] `ML-VK-080-031` generalize selected-region runtime reuse to
      direct-view consumers.
      - shipped: `tensor/run(graph)` now recognizes captured all-Vulkan
        `Float32` source -> direct transpose-view -> scalar-map* regions and
        routes the validated region through a native Vulkan executor. The
        executor records the scalar-tail dispatches into one command buffer,
        uses view strides for the first tensor operand, dense output/scratch
        strides for materialized intermediates, owns C-side scratch buffers,
        and transfers one final dense Vulkan output handle.
      - contract: this covers direct transpose-view consumers followed by
        scalar-map tails only. Mixed DAGs, memory-plan-backed reuse, broader
        view schemas, and C3 handle aliasing remain closed.
    - [x] `ML-VK-080-034` generalize selected-region runtime reuse to a
      concrete mixed DAG family.
      - shipped: `tensor/run(graph)` now recognizes captured all-Vulkan
        `Float32` source -> direct transpose-view plus a second dense source
        feeding a tensor/tensor map, followed by scalar-map tails, and routes
        validated graphs through a native mixed executor. The executor uses
        view strides for the left tensor operand, dense strides for the second
        source and scalar-tail scratch, owns intermediate buffers internally,
        and transfers one final dense Vulkan output handle.
      - contract: this closes the concrete transpose-view + dense-source
        mixed DAG reuse slice only. General mixed-region partitioning,
        memory-plan-backed reuse, broader view schemas, and C3 handle aliasing
        remain closed.
      - absorbed residual: former `ML-VK-080-037` selected-region runtime reuse
        beyond the concrete transpose-view/dense-source mixed DAG family is now
        a sub-boundary of `ML-VK-080-036`, not an independent case-specific
        work item.
    - [x] `ML-VK-080-026` generalize command-buffer lowering beyond scalar-map
      command-batch metadata.
      - shipped: `tensor/run(graph)` now validates and lowers captured
        `command-buffer-candidate` metadata for dense same-shape tensor/tensor
        map -> scalar-map* regions, not just single-source scalar-map chains.
        The lowering checks source dependencies, dispatch node ids,
        dispatch operations, and batch shape before entering the native
        selected-region command-buffer executor.
      - contract: this does not lower contract/view command batches,
        arbitrary mixed DAG regions, fused dispatch plans, or hidden CPU
        fallback.
    - [x] `ML-VK-080-028` generalize command-buffer lowering beyond tensor-map
      selected regions.
      - shipped: contract -> scalar-map* selected regions now lower validated
        captured command-batch metadata into a native Vulkan executor that
        records the contract dispatch and scalar-tail dispatches into one
        command buffer with shader-write/read barriers between dependent
        dispatches.
      - contract: this does not lower direct-view consumers, arbitrary mixed
        DAG regions, fused dispatch plans, or hidden CPU fallback.
    - [x] `ML-VK-080-032` generalize command-buffer lowering to direct-view
      consumers.
      - shipped: captured direct transpose-view -> scalar-map* command-batch
        metadata is now validated before native lowering. The lowering checks
        the view dependency, batch node ids, dispatch node ids, scalar map
        operations, launch shape, and barrier shape before entering the native
        view/scalar-chain command-buffer executor. Invalid metadata skips the
        native path and falls back to serial graph replay.
      - contract: this does not lower selected mixed graph regions, fused
        dispatch plans, arbitrary view materialization policies, or hidden CPU
        fallback.
    - [x] `ML-VK-080-035` generalize command-buffer lowering to the concrete
      transpose-view/dense-source mixed graph region.
      - shipped: captured command-batch metadata for the concrete mixed
        source -> transpose-view plus dense source -> tensor-map -> scalar-map*
        family is now validated before native lowering. The lowering checks
        source/view dependencies, batch node ids, dispatch operations, launch
        shape, and barrier shape before entering the native mixed
        command-buffer executor. Invalid metadata skips the native path and
        falls back to serial graph replay.
      - contract: this does not lower arbitrary selected mixed graph regions,
        fused dispatch plans, arbitrary memory-plan reuse, or hidden CPU
        fallback.
      - absorbed residual: former `ML-VK-080-038` command-buffer lowering
        beyond the concrete transpose-view/dense-source mixed graph region is
        now a sub-boundary of `ML-VK-080-036`, not an independent case-specific
        work item.
    - [x] `ML-VK-080-HARDEN-001` fail closed for malformed custom Kernel and
      Tensor graph replay payloads found during audit.
      - shipped: `Kernel` construction rejects source-backed specs whose
        `entry` is not `main`, matching the runtime source-dispatch contract
        instead of accepting a spec that later fails at execution.
      - shipped: bare symbolic `source 'map-unary-f32` now validates and
        executes consistently with the already-supported dictionary
        `builtin-spirv` unary source form.
      - shipped: `tensor/run(graph)` rejects binary map nodes that contain
        both `left-scalar` and `right-scalar` instead of silently choosing one
        side.
      - shipped: `tensor/capture` validates map operand shape before appending
        graph nodes so malformed unary/binary TensorVal payloads fail closed
        rather than being partially captured.
      - validation: `c3c build --obj-out obj`; direct rebuilt-binary probes for
        the non-`main` Kernel rejection and both-scalar graph rejection
        returned `true`; follow-up advanced collections module validation now
        passes with `pass=1895 fail=0`.
  - scope:
    - operation DAG capture;
    - command-buffer batching;
    - safe elementwise/reduction fusion;
    - backend compilation and execution for user-defined `Kernel` values;
    - device buffer reuse and lifetime planning;
    - deterministic invalidation on shape, dtype, device, or capability change.
  - constraint:
    - performance work only; Tensor semantics must not change;
    - do not implement `(define [kernel] ...)`; use ordinary
      `(define name (Kernel spec))` bindings for named kernels;
    - do not overload ordinary function calls, path access, postfix indexing,
      or `ref` into kernel execution.

- [x] `META-PLAN-TODO-BACKFILL-001` backfill checkbox entries for all plan documents missing TODO tracking
  - directive source: `AGENTS.md` Change Discipline section (updated 2026-04-21)
  - scope:
    - audit every `.md` file under `docs/plans/` for open/in-flight work;
    - create matching `- [ ]` or `- [x]` line items in `docs/todo_parts/` with unique IDs;
    - include plan filename reference, one-sentence description, and current status;
    - skip purely archival/decision-closed plans if they have no remaining action items.
  - constraint:
    - do not create checkbox entries for plans that are fully shipped with no residual work;
    - mark archival plans with `[x]` and closure evidence if they were previously completed;
    - keep new entries grouped by topic area (memory, compiler, deduce, ffi, vulkan, etc.).
  - done 2026-04-21:
    - audited 113 `docs/plans/*.md` files with local scans and three parallel
      subagent review slices.
    - skipped closed decision-only or already TODO-backed plans, including the
      fixed-width complex tensor lanes already tracked under `TENSOR-100H-*`.
    - backfilled live residual plans below with unique TODO entries and added
      the completed stack/aarch64 plan as a closed historical entry.
    - validation: docs-only pass with `git diff --check`.

## 2026-04-21 Plan Backfill

- [x] `RUNTIME-FILE-SPLIT-QUEUE-001` execute the active largest-runtime-file
  split queue.
  - plan: `docs/plans/largest-runtime-files-pass-2026-03-19.md`.
  - current status: active plan with an untracked `Next Queue`.
  - scope:
    - split the remaining consequential runtime files in largest-first order,
      starting with `src/lisp/aot_runtime_bridge.c3`,
      `src/lisp/async_tcp_transport_core.c3`, and the
      `eval_promotion_*` files named by the plan;
    - keep landed slices recorded in `memory/CHANGELOG.md`;
    - validate with `c3c build` and the current validation status summary
      command before closing.
  - constraint:
    - file splitting is structural work only; preserve behavior and avoid
      small-file churn outside the queued ownership surfaces.
  - done 2026-04-21:
    - superseded the stale 2026-03-19 split queue under the current owner rule:
      do not split files unless they exceed 1000 LOC.
    - refreshed the plan evidence. The former queue heads are now
      `126`, `126`, `284`, `196`, `169`, and `59` LOC respectively, not the
      stale `433`, `431`, `181`, `168`, `147`, and `51` list.
    - current `src/` + `csrc/` source inventory has no code file above 1000
      LOC; the largest observed source file is `762` LOC.
    - validation: current line-count inventory plus targeted `git diff --check`.

- [x] `UI-LIB-FACADE-001` complete the high-level `ui` facade module split and
  public-surface coverage.
  - plan: `docs/plans/ui-library-facade-plan-2026-03-27.md`.
  - current status: closed for the facade/module split and shipped public
    surface coverage; runtime-to-backend lifecycle execution is split into
    `UI-LIB-RUNTIME-BACKEND-001`.
  - scope:
    - finish the submodule import/module ownership story;
    - complete layout/style helpers and backend-owned lowering;
    - make the effect-driven runtime path the normal open/render/close path;
    - add focused examples and regressions for each shipped public `ui.*`
      family.
  - constraint:
    - keep `ui` as the canonical user-facing facade and do not expose
      backend-specific FTXUI details through public names.
  - done 2026-04-21:
    - rewired the canonical `ui.omni` facade to load
      `examples/libraries/ftxui/lib/ui/{nodes,effects,layout,style,runtime,ftxui}.omni`
      as the implementation source instead of relying on flat compatibility
      modules.
    - fixed sibling imports for `lib/ui/layout.omni`, `lib/ui/style.omni`,
      `lib/ui/runtime.omni`, and `lib/ui/evaluate.omni` so default dotted module
      paths load cleanly from `examples/libraries/ftxui/`.
    - added `module_direct_smoke.omni` and wired it into
      `scripts/run_ftxui_smoke.sh` to verify direct `(import ui.nodes)` /
      `ui.layout` / `ui.style` / `ui.effects` / `ui.runtime` / `ui.ftxui`
      module paths.
    - removed the stale static evaluator copy from `ui.runtime`; static tree
      evaluation remains isolated in `lib/ui/evaluate.omni`.
    - validation: `c3c build --obj-out obj`; direct `lib/ui/runtime.omni` and
      `lib/ui/evaluate.omni` loads; `scripts/run_ftxui_smoke.sh`.

- [x] `UI-LIB-RUNTIME-BACKEND-001` make the declarative UI effect dispatcher
  drive a real FTXUI backend lifecycle path.
  - plan: `docs/plans/ui-library-facade-plan-2026-03-27.md`.
  - current status: closed for the non-interactive backend lifecycle contract;
    richer blocking/read-loop behavior is split to
    `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001`.
  - scope:
    - define the concrete `ui.runtime` -> `ui.ftxui` handoff contract for
      `open_tree`, `render_tree`, `read_event_tree`, `invalidate_tree`,
      `post_event_tree`, and `close_tree`;
    - add a focused non-interactive smoke that proves a declarative effect tree
      can reach the FTXUI backend path without bypassing the dispatcher;
    - keep `ui.run` as a convenience wrapper over the backend, not the only
      practical runtime path.
  - done 2026-04-21:
    - added hidden primitive `__ui-ftxui-dispatch` for non-interactive backend
      effect-tree execution.
    - `ui.ftxui.dispatch` now routes through `ui.runtime.dispatch_to` into that
      primitive, so the backend path is reached through the runtime handoff
      rather than a facade-only alias.
    - the backend dispatcher requires a root `open_tree` with backend `ftxui`,
      creates a real FTXUI context/screen, lowers each `render_tree` node into
      FTXUI components, maps `invalidate_tree` to animation-frame request,
      maps `post_event_tree` to C ABI event creation/posting, and maps
      `close_tree` to screen exit.
    - `read_event_tree` is explicitly fail-closed in this non-interactive path;
      interactive read-loop support is tracked separately.
    - added `module_backend_smoke.omni` with happy-path backend lifecycle
      coverage plus fail-closed checks for unsupported `read_event_tree` and
      render-less open trees.
    - validation: `c3c build --obj-out obj`; direct
      `module_backend_smoke.omni`; `scripts/run_ftxui_smoke.sh`; bounded
      `advanced-ffi-system-surface` `pass=102 fail=0`; targeted
      `git diff --check`; `scripts/check_file_size_gate.sh`.

- [x] `UI-LIB-RUNTIME-INTERACTIVE-LOOP-001` add an interactive UI runtime loop
  contract on top of the backend lifecycle dispatcher.
  - plan: `docs/plans/ui-library-facade-plan-2026-03-27.md`.
  - current status: closed for the blocking one-shot effect-tree loop contract;
    session-owned external read/update/render remains split to
    `UI-LIB-RUNTIME-SESSION-001`.
  - scope:
    - choose the public contract for interactive sessions versus one-shot
      effect trees;
    - add a session/app handle or a higher-level loop helper that can represent
      read/update/render/close without hiding blocking terminal behavior;
    - preserve the non-interactive dispatcher as the smokeable backend
      lifecycle path.
  - done 2026-04-22:
    - added hidden primitive `__ui-ftxui-loop` and public
      `ui.ftxui.loop` / `ui.loop` wrappers routed through
      `ui.runtime.loop_to`.
    - the loop path requires a root `open_tree` with backend `ftxui` and
      exactly one `render_tree`, lowers it through the existing FTXUI app
      wrapper, enters the blocking app loop, and keeps `read_event_tree`
      explicitly fail-closed.
    - `invalidate_tree`, `post_event_tree`, and explicit pre-loop
      `close_tree` are accepted; `close_tree` returns without blocking.
    - added `module_interactive_loop_smoke.omni` and wired it into
      `scripts/run_ftxui_smoke.sh`.
    - validation: `c3c build --obj-out obj`; direct piped
      `module_interactive_loop_smoke.omni`; `scripts/run_ftxui_smoke.sh`.

- [x] `UI-LIB-RUNTIME-SESSION-001` add session-owned UI read/update/render
  state after the one-shot blocking loop.
  - plan: `docs/plans/ui-library-facade-plan-2026-03-27.md`.
  - current status: closed for the session-owned lifecycle contract; raw event
    payload reads are closed separately under
    `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001`.
  - scope:
    - choose whether session state lives in `ui.runtime`, `ui.ftxui`, or a
      future `ui.app` / `ui.session` helper;
    - expose a public contract for external read/update/render without
      confusing it with the existing blocking `ui.loop` helper;
    - keep `read_event_tree` fail-closed outside the session path until a real
      event-read contract exists.
  - next step:
    - closed; use the explicit session helpers for external lifecycle control.
  - done 2026-04-22:
    - added a shim-owned `omni_ftxui_session_t` over FTXUI `Loop` with
      `run_once`, `run_once_blocking`, `has_quitted`, and balanced
      `PreMain`/`PostMain` teardown;
    - added an owned `ui-ftxui-session` `ForeignHandle` finalizer that destroys
      the session loop before components, heap state, screen, and context;
    - exposed hidden primitives and public dotted/facade helpers:
      `open_session`, `update_session`, `render_session`,
      `read_event_session`, `invalidate_session`, `post_event_session`, and
      `close_session`;
    - added `module_session_smoke.omni` and wired it into
      `scripts/run_ftxui_smoke.sh`.
  - validation:
    - `scripts/build_omni_chelpers.sh`;
    - `c3c build --obj-out obj`;
    - direct `module_session_smoke.omni`;
    - `scripts/run_ftxui_smoke.sh` with
      `OMNI_FTXUI_RUNTIME_LD_LIBRARY_PATH=build:/usr/local/lib`.

- [x] `UI-LIB-RUNTIME-SESSION-EVENT-PAYLOAD-001` decide and implement raw
  event payload reads for session-owned UI loops.
  - plan: `docs/plans/ui-library-facade-plan-2026-03-27.md`.
  - current status: closed; session-owned raw event reads now use an explicit
    ABI capture/read contract.
  - scope:
    - decide whether `ui.read_event_session` should return raw FTXUI event
      payload values, or whether Omni should keep event processing callback /
      effect driven;
    - if raw reads are required, add a backend ABI event-capture/read contract
      instead of pretending `RunOnceBlocking` returns an event value.
  - next step:
    - closed; use `ui.read_event_session` for the explicit session event-read
      contract and keep `read_event_tree` fail-closed in one-shot dispatch/loop
      helpers.
  - done 2026-04-22:
    - added `omni_ftxui_event_read_result` and
      `omni_ftxui_session_take_event` to the FTXUI shim ABI;
    - wrapped session roots with `ftxui::CatchEvent` to capture the last event
      observed during a loop step without consuming downstream component
      handling;
    - converted captured events to Omni dictionaries with `kind` and optional
      `text`, including character events, known special keys, payload-free
      custom events, and special-key fallback input text;
    - changed `module_session_smoke.omni` to post a character event and assert
      `ui.read_event_session` returns `{'kind 'character 'text "x"}`.
  - validation:
    - `scripts/build_omni_chelpers.sh`;
    - `c3c build --obj-out obj`;
    - direct `module_session_smoke.omni`;
    - `scripts/run_ftxui_smoke.sh` with
      `OMNI_FTXUI_RUNTIME_LD_LIBRARY_PATH=build:/usr/local/lib`;
    - `scripts/check_build_config_parity.sh`;
    - `scripts/check_file_size_gate.sh`;
    - `git diff --check`.

- [x] `FTXUI-C-ABI-CONTRACT-001` lock the FTXUI C ABI wrapper matrix and open
  interface decisions.
  - plan: `docs/plans/ftxui-c-abi-shim.md`.
  - current status: closed; ABI plan now records explicit decisions for string
    ownership, coverage target, and event/custom payload ownership.
  - scope:
    - decide or explicitly freeze string ownership semantics, wrapper coverage
      target, and custom-event payload ownership;
    - record the chosen ABI contract before expanding deferred wrapper
      families.
  - constraint:
    - do not let unresolved interface questions remain as implicit backlog
      behind shipped wrapper calls.
  - done 2026-04-21:
    - locked inbound string ownership as borrowed for the call and eagerly
      copied by the shim whenever retained after return;
    - locked the ABI target as generic-builder-first rather than
      one-function-per-upstream-overload parity;
    - locked custom events as payload-free in the C ABI, with richer payloads
      reserved for callback `user_data` or the Omni-facing effect/runtime layer.

- [x] `FTXUI-C-ABI-WRAPPERS-001` implement or explicitly freeze the remaining
  FTXUI deferred wrapper families.
  - plan: `docs/plans/ftxui-c-abi-shim.md`.
  - current status: closed for the chosen generic-builder-first ABI contract;
    advanced DOM, widget-option parity, runtime-helper parity, and
    fine-grained upstream overload families are explicitly frozen as non-goals
    until a concrete Omni-facing surface requires them.
  - scope:
    - land the next missing wrapper family intentionally after
      `FTXUI-C-ABI-CONTRACT-001` has fixed the ABI contract;
    - keep unsupported families documented as explicit non-goals rather than
      silent fallbacks.
  - constraint:
    - every shipped wrapper family needs focused ABI coverage and explicit
      fail-closed behavior for unsupported calls.
  - done 2026-04-21:
    - added `omni_ftxui_component_handle_event(...)` so event callbacks can be
      dispatched and tested without starting an interactive screen loop;
    - added the previously documented-but-missing
      `omni_ftxui_component_wrap_action(...)` action callback wrapper and
      preserved callback failure status through deterministic event dispatch;
    - corrected the C3 callback-field mirror to use function-pointer aliases
      directly instead of pointer-to-function-pointer fields;
    - added focused ABI regressions for context/error lifecycle, screen
      lifecycle, event creation/posting, table render conversion, canvas render
      conversion, widget event callback delivery, and action callback delivery;
    - added focused fail-closed coverage for the unsupported piped-input screen
      path (`screen_create` and `screen_set_handle_piped_input` return
      `OMNI_FTXUI_STATUS_NOT_SUPPORTED` instead of silently ignoring the
      request);
    - explicitly froze the remaining deferred wrapper families as non-goals for
      the current ABI contract: arbitrary selection pixel callbacks, gradient
      decorators, animated/richer widget option parity, active-screen access,
      captured-mouse exposure, `WithRestoredIO`, selection API callbacks, and
      one-function-per-upstream-overload parity;
    - validation: `c3c build --obj-out obj`, bounded
      `advanced-ffi-system-surface` `pass=105 fail=0`,
      `scripts/run_ftxui_smoke.sh`, targeted `git diff --check`, and
      `scripts/check_file_size_gate.sh`.

- [x] `REPL-SERVER-MULTICLIENT-001` complete concurrent REPL server client
  handling.
  - plan: `docs/plans/repl-server-protocol-2026-03-30.md`.
  - current status: concurrent accepted-client handling is shipped for Unix
    socket and TCP transports; request queueing is split into
    `REPL-SERVER-REQUEST-QUEUE-001`.
  - scope:
    - add concurrent multi-client handling;
    - preserve structured streamed protocol replies, interrupts, and busy
      semantics with protocol tests.
  - constraint:
    - do not regress the existing single-worker fail-closed behavior while
      widening concurrency.
  - done 2026-04-21:
    - Unix socket and TCP listeners now hand each accepted fd to a detached
      client-handler thread and immediately resume accepting clients;
    - handler-start failure sends a structured
      `runtime/client-handler-start-failed` protocol error and closes the fd
      instead of falling back to serial listener-thread handling;
    - added socketpair-based regression coverage for two independent handler
      streams returning `describe` replies;
    - validation: `c3c build --obj-out obj`; bounded `async` slice
      `pass=66 fail=0`; targeted `git diff --check`; file-size gate.

- [x] `REPL-SERVER-REQUEST-QUEUE-001` add explicit per-stream runtime request
  queueing beyond one active operation.
  - plan: `docs/plans/repl-server-protocol-2026-03-30.md`.
  - current status: shipped as a bounded FIFO per-connection runtime queue.
  - scope:
    - design a bounded per-stream queue for `clone`, `eval`, `load-file`,
      `complete`, and `close`;
    - define interrupt/stdin semantics for queued versus running requests;
    - add protocol tests proving queue order, busy/backpressure, interrupt
      targeting, and teardown behavior.
  - constraint:
    - do not silently change existing `protocol/server-busy` behavior without
      an explicit queue capacity/backpressure contract.
  - done 2026-04-21:
    - replaced the single `pending_command` worker slot with an 8-entry FIFO
      queue while preserving exactly one active runtime command per connection;
    - `protocol/server-busy` is now queue-full backpressure for runtime work;
    - `interrupt` remains active-request-only, while `stdin` can target active
      or queued `eval` / `load-file` requests by request id;
    - queued `clone` work defers session allocation to the worker execution
      path, avoiding request-thread session-table reallocation while the worker
      may hold active session pointers;
    - queued commands are released during connection teardown;
    - added regressions for multiple pending commands, queue order, queued
      stdin targeting, active-only interrupt behavior, queue-full backpressure,
      and deferred clone session allocation;
    - validation: `c3c build --obj-out obj`; bounded `async` slice
      `pass=69 fail=0`.

- [x] `FFI-CALLBACK-UV-001` prototype the explicit `uv` callback-handle lane.
  - plans:
    - `docs/plans/ffi-callback-shim-prototype-uv-2026-04-08.md`.
    - `docs/plans/ffi-callback-registration-model-2026-04-08.md`.
  - current status: shipped as an explicit `uv`-scoped callback-handle
    prototype; generic callback coercion remains reserved.
  - scope:
    - add `uv`-scoped callback register/invoke/unregister shim helpers;
    - retain and release callable payloads deterministically through an opaque
      handle;
    - add focused lifetime tests for register/invoke, repeated unregister,
      stale invocation, owner-scope release, and handle isolation.
  - constraint:
    - do not expose arbitrary closure-to-native-function-pointer coercion or
      claim generic callback handles until this concrete lane proves teardown
      semantics.
  - done 2026-04-21:
    - confirmed the concrete lane is implemented through
      `__raw-uv-timer-callback-handle`,
      `__raw-uv-timer-callback-invoke`,
      `__raw-uv-timer-callback-unhandle`, and
      `__raw-uv-timer-callback-dispatch-count`;
    - confirmed the C shim remains limited to `uv` timer callback handle
      registration/invocation/unregistration and does not expose arbitrary
      closure-to-native-function-pointer coercion;
    - retained callable payload ownership through a root-scope retain on
      handle creation and deterministic release on unhandle/finalizer;
    - added a regression that forces FFI wrapper box and value allocation
      failures after shim registration and verifies the retained owner scope is
      released;
    - added a finalizer-path regression that destroys a local wrapper scope
      without explicit unhandle and verifies the retained callback owner scope
      returns to baseline;
    - validation: `c3c build --obj-out obj`; focused
      `advanced-ffi-system-surface` slice `pass=72 fail=0`; targeted
      `git diff --check`.

- [x] `FALLBACK-RUNTIME-OWNERSHIP-001` finish the staged runtime ownership
  fallback cleanup.
  - plan: `docs/plans/fallback-inventory.md`.
  - current status: closed 2026-04-22. The staged runtime ownership fallback
    family is no longer an open umbrella item; global scope-chain scan deletion
    was invalidated and is explicitly not part of the shipped closure.
  - scope:
    - close the boundary-routing plan for remaining `copy_to_parent` and
      scope-scan traversal fallback surfaces;
    - delete remaining unchecked/bypass helper surfaces once explicit boundary
      routes cover the call sites;
    - validate with boundary/lifetime tests and `c3c build`.
  - constraint:
    - preserve shared promotion-context semantics and do not replace staged
      fallback deletion with root pinning or per-type reference counts.
  - progress 2026-04-21:
    - `copy_to_parent_with_fault(...)` now returns `null` with
      `BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE` when called in checked
      mode without target/releasing scope provenance, instead of synthesizing
      an `ERROR` value that could be mistaken for copied data;
    - the boundary copy fault test now covers both the public checked facade
      and the direct checked helper;
    - bounded `memory-lifetime-smoke` passes normally with `pass=234 fail=0`;
    - global `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1` is a known-bad deletion
      path for now (`pass=215 fail=9`), so remaining scope-chain scan cleanup
      must be route-specific and backed by replacement semantics.
    - split the scope-chain scan loop from its policy gate so hinted lookups
      that fall back to a raw chain scan consume the scan budget once per
      logical lookup instead of double-checking the same budget;
    - added a focused scan-budget regression; bounded
      `memory-lifetime-smoke` now passes with `pass=235 fail=0`.
    - split `boundary_can_reuse_value_with_target_chain(...)` from the generic
      reuse predicate so `boundary_classify_return_value(...)` reuses its
      already computed target-chain membership instead of performing a second
      target-chain scan in the same classification decision;
    - validation after the target-chain reuse cleanup: normal
      `c3c build --obj-out obj`; counters build
      `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`; bounded
      counters-enabled `memory-lifetime-smoke` `pass=235 fail=0`; normal
      rebuild `c3c build --obj-out obj`; `scripts/check_file_size_gate.sh`.
    - replaced destination cons, partial, and iterator child-copy routes'
      unchecked `boundary_copy_to_parent_site_ctx(...)` calls with the checked
      `boundary_copy_to_parent_site_ctx_checked(...)` facade while preserving
      destination error bubbling semantics for child faults;
    - validation after the destination builder checked-copy cleanup:
      `c3c build --obj-out obj`; bounded `memory-lifetime-smoke`
      `pass=235 fail=0`.
    - replaced the remaining root-store and root-clone
      `boundary_copy_to_parent_site_ctx(...)` call sites with
      `boundary_copy_to_parent_site_ctx_checked(...)` plus explicit typed-fault
      to error-value conversion at the existing fail-closed boundary;
    - `rg -n "boundary_copy_to_parent_site_ctx\\(" src/lisp/eval_boundary_*
      src/lisp/eval_promotion_* src/lisp/value_* src/lisp/jit_*` now reports
      only the wrapper definition in `src/lisp/eval_boundary_api.c3`;
    - validation after full unchecked-wrapper callsite cleanup:
      `c3c build --obj-out obj`; bounded `memory-lifetime-smoke`
      `pass=235 fail=0`.
    - reordered `boundary_can_reuse_value(...)` so target-chain membership is
      computed once before alias-safety traversal and then passed to the shared
      reuse predicate, avoiding alias scans for values already known to be
      outside the target chain;
    - validation after the reuse precheck cleanup: `c3c build --obj-out obj`;
      bounded `memory-lifetime-smoke` `pass=235 fail=0`.
    - zero-generation hinted target-chain lookups now participate in the
      active promotion-context scope-chain route cache instead of bypassing the
      cache and rescanning within the same boundary epoch;
    - added a focused smoke regression proving repeated zero-generation
      lookups scan once under an active promotion context;
    - validation after the zero-hint route-cache cleanup:
      `c3c build --obj-out obj`; counters build
      `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`; bounded
      counters-enabled `memory-lifetime-smoke` `pass=236 fail=0`; normal
      rebuild `c3c build --obj-out obj`.
    - removed the dead unchecked `boundary_copy_to_parent_site_ctx(...)` and
      `copy_to_parent_site(...)` wrapper surfaces after moving remaining
      test-only callers to the checked result API;
    - removed the plain unchecked `copy_to_parent(...)` convenience after its
      remaining test-only callers moved to `test_copy_to_parent(...)` or direct
      `copy_to_parent_with_fault(...)` assertions;
    - `rg -n '[^A-Za-z0-9_]copy_to_parent\\(|boundary_copy_to_parent_site_ctx\\b|copy_to_parent_site\\b'
      src/lisp -g '*.c3'` now returns no live symbols;
    - validation after unchecked copy wrapper deletion: `c3c build --obj-out obj`;
      bounded `memory-lifetime-smoke` `pass=236 fail=0`; targeted
      `git diff --check`; `scripts/check_file_size_gate.sh`.
    - removed dead non-hinted scope-chain helper surfaces
      `boundary_ptr_in_target_scope_chain(...)`,
      `boundary_ptr_in_scope_chain_scan(...)`, and
      `boundary_ptr_in_scope_chain(...)` after verifying all live callers use
      the hinted/cache-aware route;
    - validation after dead helper cleanup: `c3c build --obj-out obj`;
      bounded `memory-lifetime-smoke` `pass=236 fail=0`; counters build
      `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`;
      counters-enabled bounded `memory-lifetime-smoke` `pass=236 fail=0`;
      normal rebuild `c3c build --obj-out obj`.
    - routed detached environment write-barrier promotion through
      `copy_to_parent_with_fault(...)` instead of calling
      `copy_to_parent_by_route(...)` directly, so env writes use the same
      typed copy-fault path and promotion-context lookup behavior as the rest
      of the boundary cleanup;
    - `rg -n 'copy_to_parent_by_route\(' src/lisp -g '*.c3'` now reports only
      the implementation and its two internal calls in
      `eval_promotion_copy.c3`;
    - validation after env-barrier checked-copy cleanup:
      `c3c build --obj-out obj`; bounded `memory-lifetime-smoke`
      `pass=236 fail=0`.
    - added `boundary_ptr_in_scope_chain_with_hint_cached(...)` for explicit
      target-scope lookups so destination builders and return classification
      reuse the active promotion-context route cache even when the target
      scope is not `interp.current_scope`;
    - routed destination partial child reuse checks and
      `boundary_classify_return_value(...)` through the explicit-target cache;
    - added a bounded smoke regression proving repeated explicit-target
      zero-generation lookups scan once under an active promotion context;
    - validation after explicit-target scope-cache cleanup:
      normal `c3c build --obj-out obj`; bounded `memory-lifetime-smoke`
      `pass=237 fail=0`; counters build
      `c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS`;
      counters-enabled bounded `memory-lifetime-smoke` `pass=237 fail=0`;
      normal rebuild `c3c build --obj-out obj`; targeted `git diff --check`;
      `scripts/check_file_size_gate.sh`.
  - done 2026-04-22:
    - extended `scripts/check_boundary_facade_usage.sh` so direct
      `copy_to_parent_by_route(...)`, `boundary_copy_to_parent_site_ctx(...)`,
      and uncached `boundary_ptr_in_scope_chain_with_hint(...)` production
      call sites are blocked outside sanctioned boundary implementation files;
    - routed the tensor expression-edge escape promotion path through
      `boundary_promote_to_escape(...)`, removing the direct facade bypass that
      the strengthened guard exposed;
    - final inventory is guard-backed: `copy_to_parent_by_route(...)` is
      internal to `eval_promotion_copy.c3`, the unchecked
      `boundary_copy_to_parent_site_ctx(...)` surface has no production call
      sites, and uncached explicit scope-chain lookup is limited to the
      boundary implementation plus ignored tests;
    - validation for closure: `bash scripts/check_boundary_facade_usage.sh`;
      `c3c build --obj-out obj`; bounded `memory-lifetime-smoke`
      `pass=237 fail=0`; targeted `git diff --check`;
      `scripts/check_file_size_gate.sh`.

- [x] `ACCESS-UNIFY-INDEX-REF-002` finish index/ref error-path parity.
  - plan: `docs/plans/index-ref-unification-decision-2026-04-08.md`.
  - current status: shared runtime lookup helper has landed, but error wording
    and negative-path parity remain as an explicit follow-up.
  - scope:
    - normalize remaining `ref` versus `expr.[key]` error message text;
    - add out-of-bounds and wrong-index-type parity tests for list, array,
      dictionary, and string paths as applicable.
  - constraint:
    - keep the current shared-runtime-helper seam; do not re-open parser-level
      desugaring unless a separate parser/AST plan justifies it.
  - done 2026-04-21:
    - confirmed `(ref coll key)` and `expr.[key]` share the
      `ref_try_lookup_collection(...)` and `ref_type_error(...)` runtime
      paths for array, list, dictionary, string, tensor, and unsupported
      collection handling.
    - confirmed `jit_compile_index(...)` still spills the collection before
      compiling the index expression, then reloads before calling
      `jit_do_index(...)`; this preserves the prior JIT clobber fix.
    - added direct parity tests for array/list/string out-of-bounds and
      wrong-index-type errors, dictionary missing-key `nil` behavior, and
      unsupported-collection errors.
    - added canonical postfix error-message assertions for array/list/string
      out-of-bounds and wrong-index-type errors.
    - validation: `c3c build --obj-out obj` passed; direct index/ref
      canonical-message probes returned `true`; basic Lisp slice reported the
      new index tests passing but still failed on unrelated tail multi-arg
      cases (`pass=164 fail=2`).

- [x] `STACK-AARCH64-001` add the aarch64 stack-switch backend under the
  existing stack contract seam.
  - plans:
    - `docs/plans/stack-backend-contract-decision-2026-04-08.md`.
    - `docs/plans/stack-aarch64-implementation-plan-2026-04-08.md`.
  - closure evidence:
    - the aarch64 plan records runtime arm64 validation for the stack suite,
      coroutine smoke, effect smoke, continuation/effect parity evals,
      advanced effect-continuation slice, and focused JIT policy slice.
    - `docs/todo_parts/todo_part_14.md` already contains the closed
      `STACK-AARCH64-CONT-001` language-level continuation parity item.
  - residuals:
    - none for the plan-backed aarch64 backend support claim in this backfill
      pass.

- [x] `ML-VK-090` add Vulkan ML validation and benchmark suite
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - completed slices:
    - [x] `ML-VK-090-001` add CPU finite-difference regressions for shipped
      `ml/grad` contracts:
      - added central finite-difference checks for
        `linear-mean-squared-error` input gradients and
        `linear-softmax-cross-entropy` weight gradients;
      - compared numeric losses through the public `ml/linear`,
        `ml/mean-squared-error`, and `ml/cross-entropy` primitives against the
        analytic `ml/grad` output;
      - validated in the focused `advanced-collections-module` slice with
        `pass=1899 fail=0`.
    - [x] `ML-VK-090-002` add no-hidden-CPU-fallback diagnostic probes for
      mixed Vulkan ML trees:
      - added focused coverage that mixed Vulkan/CPU `ml/clip-gradients` and
        `ml/optimizer-step` failures expose the Vulkan placement contract in
        their error message;
      - this guards against regressions that collapse mixed-device failures
        into generic backend errors or stale "kernel unimplemented" text;
      - validated in the focused `advanced-collections-module` slice with
        `pass=1900 fail=0`.
    - [x] `ML-VK-090-003` add bounded ML validation and benchmark fixture:
      - added `OMNI_ML_BENCH=1` benchmark summaries for CPU ML inference and
        optimizer training-step loops, including runtime memory reuse counters;
      - added `scripts/run_ml_validation_slice.sh` as the Docker-bound focused
        advanced collections ML validation entrypoint;
      - validated with host build plus bounded focused slice
        `pass=1883 fail=0`, `ml_inference_oracle inference_ok=128`, and
        `ml_training_step_oracle train_ok=64`.
    - [x] `ML-VK-090-004` add explicit ML Vulkan operation-family
      availability-gate regression:
      - added a focused `tensor-backends` host-path test that verifies
        Vulkan-visible hosts keep Float32 ML operation-family capability bits
        tied to actual Float32 placement across linear, activation, reduction,
        normalization, attention, convolution/pooling, optimizer, and gradient
        clipping families;
      - the same test executes a device-resident `ml/linear` smoke on
        Vulkan-visible hosts and requires Vulkan-unavailable hosts to report
        false ML Vulkan capabilities and fail closed with
        `tensor/backend-unavailable` for `to-device 'vulkan`;
      - validated with host build, focused advanced collections
        `pass=1901 fail=0`, and bounded ML validation slice
        `pass=1884 fail=0`.
  - closure scope:
    - CPU oracle comparisons;
    - gradient finite-difference checks;
    - no-hidden-CPU-fallback probes;
    - Vulkan-visible and Vulkan-unavailable host gates;
    - bounded focused validation and benchmark fixture.
