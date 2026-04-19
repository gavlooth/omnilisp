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
      - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
      - `src/lisp/jit_jit_runtime_effects_signal.c3`
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

- [ ] `ML-VK-001` freeze the backend-neutral Vulkan ML suite contract and capability matrix
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

- [ ] `ML-VK-010` add Vulkan batched linear algebra foundations for ML workloads
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
    - [ ] `ML-VK-010-004-001` implement Vulkan `Float32` batched-reduction
      coverage for `ml/linear/batched-reduce` with no-CPU-fallback
      regressions.
    - [ ] `ML-VK-010-005` decide and implement expression-backed Vulkan
      `ml/linear` lowering beyond already-materialized direct map results, or
      keep the concrete/view boundary explicit with permanent fail-closed tests
      for view-backed operands.
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

- [ ] `ML-VK-020` add Vulkan neural elementwise, reduction, softmax, and loss kernels
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - scope:
    - activation kernels: `relu`, `leaky-relu`, `sigmoid`, `tanh`, `gelu`;
    - stable `exp`, `log`, `logsumexp`, and `softmax`;
    - axis `sum`, `mean`, and `variance`;
    - cross-entropy and mean-squared-error losses.
  - negative constraint:
    - do not reuse invalidated GLSL double-transcendental assumptions for
      Vulkan `Float64`; use validated approximations or fail closed.

- [ ] `ML-VK-030` add Vulkan convolution, pooling, and image-tensor kernels
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - scope:
    - 1D and 2D convolution;
    - stride, padding, dilation, groups, and explicit batch/channel layout;
    - max and average pooling;
    - backward kernels only after the autograd contract lands.
  - acceptance:
    - first supported layout is explicit and capability-gated;
    - arbitrary views/strides remain fail-closed until a separate ABI lands.

- [ ] `ML-VK-040` add Vulkan normalization and attention primitives
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - scope:
    - batch normalization;
    - layer normalization;
    - stable scaled dot-product attention;
    - mask diagnostics;
    - optional fused attention kernels only after unfused oracle kernels pass.

- [ ] `ML-VK-050` implement reverse-mode Tensor autograd with explicit Vulkan semantics
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - scope:
    - gradient tape representation and lifetime rules;
    - gradient accumulation on CPU/Vulkan without hidden transfers;
    - backward rules for map, contract, matrix, convolution, normalization,
      softmax, and loss operations;
    - fail-closed behavior when a Vulkan forward op has no Vulkan backward.
  - constraint:
    - preserve the region/scope memory model; do not add per-Tensor ownership.

- [ ] `ML-VK-060` add Vulkan-capable optimizer suite
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - scope:
    - SGD with momentum;
    - Adam and AdamW;
    - RMSProp;
    - gradient clipping;
    - weight decay;
    - optimizer state checkpoint and restore.
  - acceptance:
    - optimizer state keeps dtype/device placement explicit;
    - mixed-device parameter groups fail closed unless transfer is explicit.

- [ ] `ML-VK-070` add backend-neutral model/layer library and serialization
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - scope:
    - linear, convolution, embedding, and normalization layers;
    - sequential/composition helpers;
    - parameter traversal;
    - checkpoint serialization and loading.
  - constraint:
    - Vulkan execution must follow Tensor placement and capability reporting;
      do not add backend-specific public layer names.

- [ ] `ML-VK-080` add Vulkan ML graph capture, fusion, and memory planning
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - scope:
    - operation DAG capture;
    - command-buffer batching;
    - safe elementwise/reduction fusion;
    - device buffer reuse and lifetime planning;
    - deterministic invalidation on shape, dtype, device, or capability change.
  - constraint:
    - performance work only; Tensor semantics must not change.

- [ ] `ML-VK-090` add Vulkan ML validation and benchmark suite
  - plan: `docs/plans/vulkan-ml-suite-roadmap-2026-04-19.md`
  - scope:
    - CPU oracle comparisons;
    - gradient finite-difference checks;
    - no-hidden-CPU-fallback probes;
    - Vulkan-visible and Vulkan-unavailable host paths;
    - bounded-container ML test slices;
    - inference throughput, training step time, and memory reuse benchmarks.
