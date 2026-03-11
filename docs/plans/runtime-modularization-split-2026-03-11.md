# Runtime Modularization Split Pass (2026-03-11)

Purpose: close O6 in `TODO.md` by applying largest-first modularization to
oversized runtime modules while preserving behavior and validating touched
subsystems.

## Split Results

Applied in largest-first order:

1. `schema.c3`
- extracted validation + `prim_validate` into:
  - `src/lisp/schema_validation.c3`
- retained explain/diagnostic surface in:
  - `src/lisp/schema.c3`

2. `eval_dispatch_types.c3`
- extracted dispatch match/type-check hot path into:
  - `src/lisp/eval_dispatch_match.c3`
- retained type registry + formatting helpers in:
  - `src/lisp/eval_dispatch_types.c3`

3. `scheduler_offload_worker.c3`
- extracted operation execution helpers into:
  - `src/lisp/scheduler_offload_ops.c3`
- retained queue/admission + uv callback orchestration in:
  - `src/lisp/scheduler_offload_worker.c3`

4. `schema.c3` (continuation split)
- extracted effect explain path into:
  - `src/lisp/schema_explain_effect.c3`
- retained dispatch explain + shared explain helpers + `prim_explain` entrypoint in:
  - `src/lisp/schema.c3`

5. `tests_memory_lifetime_groups.c3` (top-down continuation split)
- extracted finalize/result/lane-barrier section into:
  - `src/lisp/tests_memory_lifetime_finalize_groups.c3`
- retained root-boundary/ast-arena/fallback section in:
  - `src/lisp/tests_memory_lifetime_groups.c3`

6. `eval_boundary_diagnostics.c3` (top-down continuation split)
- extracted graph-audit traversal + boundary telemetry helpers into:
  - `src/lisp/eval_boundary_graph_audit.c3`
- retained scope-transfer reject diagnostics + invariant assertions in:
  - `src/lisp/eval_boundary_diagnostics.c3`

7. `tests_advanced_type_effect_ffi_groups.c3` (top-down continuation split)
- extracted type-parametric + dispatch-constructor suites into:
  - `src/lisp/tests_advanced_type_dispatch_groups.c3`
- retained mutation/consolidation suites in:
  - `src/lisp/tests_advanced_type_effect_ffi_groups.c3`

8. `tests_scheduler_io_task_groups.c3` (top-down continuation split)
- extracted offload/thread join-timeout suite and scheduler offload hot-path benchmark into:
  - `src/lisp/tests_scheduler_offload_thread_groups.c3`
- retained fiber-temp boundary, interleave, cancel, and fairness suites in:
  - `src/lisp/tests_scheduler_io_task_groups.c3`

9. `eval_boundary_graph_audit.c3` (top-down continuation split)
- extracted graph-audit metadata/result bookkeeping helpers into:
  - `src/lisp/eval_boundary_graph_audit_meta.c3`
- retained escape-reachability traversal + boundary dump wiring in:
  - `src/lisp/eval_boundary_graph_audit.c3`

10. `eval_promotion_escape.c3` (top-down continuation split)
- extracted root-store clone/route/promote path into:
  - `src/lisp/eval_promotion_root_store.c3`
- retained ESCAPE-lane promotion and route-by-tag helpers in:
  - `src/lisp/eval_promotion_escape.c3`

11. `tests_memory_lifetime_groups.c3` (top-down continuation split)
- extracted root-boundary promotion/clone test block into:
  - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3`
- retained fallback-gate + resume provenance + AstArena validation helpers in:
  - `src/lisp/tests_memory_lifetime_groups.c3`

12. `tests_advanced_type_dispatch_groups.c3` (top-down continuation split)
- extracted type/parametric parser matrix + lambda boundary coverage into:
  - `src/lisp/tests_advanced_type_parametric_groups.c3`
- retained dispatch-constructor coverage + hot-path benchmark hooks in:
  - `src/lisp/tests_advanced_type_dispatch_groups.c3`

13. `tests_memory_lifetime_boundary_ast_bench_groups.c3` (top-down continuation split)
- extracted AstArena parser/compile/macro suite helper block into:
  - `src/lisp/tests_memory_lifetime_ast_arena_bench_suites.c3`
- retained benchmark env toggles, summary macros, and benchmark entrypoint in:
  - `src/lisp/tests_memory_lifetime_boundary_ast_bench_groups.c3`

14. `eval_boundary_api.c3` (top-down continuation split)
- extracted boundary commit/fault taxonomies, name tables, and result helpers into:
  - `src/lisp/eval_boundary_api_meta.c3`
- retained scope boundary API wrappers and env/scope copy entrypoints in:
  - `src/lisp/eval_boundary_api.c3`

15. `tests_memory_lifetime_boundary_commit_groups.c3` (top-down continuation split)
- extracted boundary commit-escape matrix and local list/cons helpers into:
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3`
- retained boundary return-classification matrix in:
  - `src/lisp/tests_memory_lifetime_boundary_commit_groups.c3`

16. `tests_memory_lifetime_env_copy_groups.c3` (top-down continuation split)
- extracted env-copy closure retain helpers/tests into:
  - `src/lisp/tests_memory_lifetime_env_copy_closure_groups.c3`
- retained env-copy memo/rewrite/stress/fault and escape-mode suite entrypoint in:
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3`

17. `eval_type_evaluators.c3` (top-down continuation split)
- extracted instance construction/copy boundary helpers into:
  - `src/lisp/eval_type_instance_builder.c3`
- retained type declaration/evaluator paths and constructor validation flow in:
  - `src/lisp/eval_type_evaluators.c3`

18. `jit_jit_closure_define_qq.c3` (top-down continuation split)
- extracted closure/type-signature/tco helper block into:
  - `src/lisp/jit_jit_closure_support.c3`
- retained let-rec/set-path/env-extend helpers in:
  - `src/lisp/jit_jit_closure_define_qq.c3`

19. `tests_deduce_query_groups.c3` (top-down continuation split)
- extracted scan/scan-range/query-filter and benchmark utility helpers into:
  - `src/lisp/tests_deduce_query_scan_groups.c3`
- retained match coverage, benchmark core runner, and suite entrypoint in:
  - `src/lisp/tests_deduce_query_groups.c3`

20. `tests_advanced_core_unicode_groups.c3` (top-down continuation split)
- extracted core semantics (match/continuation/effect/try-raise) test block into:
  - `src/lisp/tests_advanced_core_semantics_groups.c3`
- retained unicode/iterator/runtime-control/lambda-binding matrix suites in:
  - `src/lisp/tests_advanced_core_unicode_groups.c3`

21. `json.c3` (top-down continuation split)
- extracted yyjson FFI aliases/extern declarations and JSON parse/write flag constants into:
  - `src/lisp/json_yyjson_ffi.c3`
- retained JSON pointer helpers, option parsers, and runtime parse/emit primitives in:
  - `src/lisp/json.c3`

22. `jit_jit_compile_expr_core.c3` (top-down continuation split)
- extracted fallback-audit metadata and expression-family dispatch front-end into:
  - `src/lisp/jit_jit_compile_expr_dispatch.c3`
- retained concrete core call/app expression lowerings in:
  - `src/lisp/jit_jit_compile_expr_core.c3`

23. `scheduler_primitives.c3` (top-down continuation split)
- extracted offload entrypoint/job parse/queue submission helpers into:
  - `src/lisp/scheduler_primitives_offload.c3`
- retained fiber cancel/spawn/await and scheduler run-loop primitives in:
  - `src/lisp/scheduler_primitives.c3`

24. `tests_tests.c3` (top-down continuation split)
- extracted lisp slice selection/policy enforcement helpers into:
  - `src/lisp/tests_slice_policy.c3`
- retained test summary emitters, isolated group runners, and unified test orchestration in:
  - `src/lisp/tests_tests.c3`

25. `async_tcp_transport_core.c3` (top-down continuation split)
- extracted sockaddr/resolve/read-option helper layer and shared tcp raise helpers into:
  - `src/lisp/async_tcp_transport_helpers.c3`
- retained tcp connect/listen/accept/read/write/close primitives in:
  - `src/lisp/async_tcp_transport_core.c3`

26. `primitives_meta_types.c3` (top-down continuation split)
- extracted predicate/meta/eval/apply/error primitive block into:
  - `src/lisp/primitives_meta_predicates.c3`
- retained constructor-type checks, type predicates, and unsafe-free primitive in:
  - `src/lisp/primitives_meta_types.c3`

27. `tests_scheduler_offload_thread_groups.c3` (top-down continuation split)
- extracted scheduler offload hot-path benchmark helpers/runner into:
  - `src/lisp/tests_scheduler_offload_bench_groups.c3`
- retained scheduler offload/thread join-timeout-cancel test matrices in:
  - `src/lisp/tests_scheduler_offload_thread_groups.c3`

28. `deduce.c3` (top-down continuation split)
- extracted LMDB FFI aliases/externs, DB/TXN handle lifecycle helpers, and deduce handle/name utilities into:
  - `src/lisp/deduce_db_handles.c3`
- retained relation lifecycle, open/block/commit/abort primitives, and deduce namespace/dispatch wrappers in:
  - `src/lisp/deduce.c3`

29. `tests_memory_lifetime_finalize_groups.c3` (top-down continuation split)
- extracted finalize-path parity harness and scoped finalize helper block into:
  - `src/lisp/tests_memory_lifetime_finalize_parity_groups.c3`
- retained run-result/coroutine pool checks and lane-barrier promotion suites in:
  - `src/lisp/tests_memory_lifetime_finalize_groups.c3`

30. `tests_memory_lifetime_boundary_commit_escape_groups.c3` (top-down continuation split)
- extracted destination-commit primary matrix (temp cons/partial/iterator/scalar + already-safe reuse) into:
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`
- retained mixed-uncertain provenance, routed-escape reuse, and fallback-disallowed coverage in:
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3`

31. `tests_runtime_async_groups.c3` (top-down continuation split)
- extracted async DNS/file/TLS test surface (including TLS integration handshake matrix + TLS arg-validation coverage) into:
  - `src/lisp/tests_runtime_async_io_tls_groups.c3`
- retained listener lifecycle and TCP/pipe/UDP fiber bridge coverage in:
  - `src/lisp/tests_runtime_async_groups.c3`

32. `eval_boundary_commit_flow.c3` (top-down continuation split)
- extracted fallback error construction and destination ESCAPE wrapper builders into:
  - `src/lisp/eval_boundary_commit_escape_builders.c3`
- retained commit classification/ctx/splice/fallback transaction flow in:
  - `src/lisp/eval_boundary_commit_flow.c3`

33. `scheduler_wakeup_dispatch.c3` (top-down continuation split)
- extracted wakeup queue publication/signaling surface (reliable enqueue/dequeue, wakeup publish helpers, and poll coalescing signal setters) into:
  - `src/lisp/scheduler_wakeup_queue.c3`
- retained libuv callback handling, wakeup completion handlers, and wakeup-drain loops in:
  - `src/lisp/scheduler_wakeup_dispatch.c3`

34. `value_constructors.c3` (top-down continuation split)
- extracted destructor + core constructor layer (scope dtors, scalar/list/closure/continuation/time-point constructors) into:
  - `src/lisp/value_constructors_core.c3`
- retained primitive/FFI-box ownership and raise-payload/error construction paths in:
  - `src/lisp/value_constructors.c3`

35. `tests_memory_lifetime_boundary_groups.c3` (top-down continuation split)
- extracted boundary state/copy-policy/interleaving suites into:
  - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3`
- retained transition regression, run_program boundary-state, and wrapper/splice legality suites in:
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3`

36. `eval_type_evaluators.c3` (top-down continuation split)
- extracted type declaration/type-info allocator helpers and `deftype` evaluator path into:
  - `src/lisp/eval_type_declarations.c3`
- retained constructor validation, union/alias/effect evaluators in:
  - `src/lisp/eval_type_evaluators.c3`

37. `scheduler_primitives_threads.c3` (top-down continuation split)
- extracted OS-thread handle parse/finalizer, spawn preparation, join execution, and timeout-pair parse helpers into:
  - `src/lisp/scheduler_primitives_thread_helpers.c3`
- retained thread primitive entrypoints (`thread-spawn`, `thread-join`, `thread-join-timeout`, `thread-cancel`) in:
  - `src/lisp/scheduler_primitives_threads.c3`

38. `tests_memory_lifetime_root_boundary_groups.c3` (top-down continuation split)
- extracted root-boundary promotion/direct-promotion/ARRAY clone suites into:
  - `src/lisp/tests_memory_lifetime_root_boundary_promotion_groups.c3`
- retained HASHMAP/METHOD_TABLE/PRIMITIVE clone suites in:
  - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3`

39. `unicode.c3` (top-down continuation split)
- extracted utf8proc extern/flag layer plus case-mapping primitives (`string-upcase`, `string-downcase`, `string-casefold`, `string-titlecase`) into:
  - `src/lisp/unicode_case_mapping.c3`
- retained normalization/grapheme/codepoint/category/width/property primitives in:
  - `src/lisp/unicode.c3`

40. `json.c3` (top-down continuation split)
- extracted JSON pointer decode/lookup and parse/emit option helper layer into:
  - `src/lisp/json_pointer_options.c3`
- retained yyjson-to-Omni conversion and `json-get`/`json-parse` entrypoints in:
  - `src/lisp/json.c3`

41. `value_core_types.c3` (top-down continuation split)
- extracted `SECTION 1` runtime value/fault/core-container type definitions into:
  - `src/lisp/value_runtime_types.c3`
- retained `SECTION 1.5` type-system/signature/value-union definitions in:
  - `src/lisp/value_core_types.c3`

## Size Snapshot

Before:
- `src/lisp/schema.c3`: `1127` lines
- `src/lisp/eval_dispatch_types.c3`: `696` lines
- `src/lisp/scheduler_offload_worker.c3`: `564` lines
- `src/lisp/tests_memory_lifetime_groups.c3`: `1256` lines
- `src/lisp/eval_boundary_diagnostics.c3`: `998` lines
- `src/lisp/tests_advanced_type_effect_ffi_groups.c3`: `838` lines
- `src/lisp/tests_scheduler_io_task_groups.c3`: `825` lines
- `src/lisp/eval_boundary_graph_audit.c3`: `842` lines
- `src/lisp/eval_promotion_escape.c3`: `729` lines
- `src/lisp/tests_memory_lifetime_groups.c3` (post-step-5 baseline): `724` lines
- `src/lisp/tests_memory_lifetime_boundary_ast_bench_groups.c3`: `707` lines
- `src/lisp/eval_boundary_api.c3`: `628` lines
- `src/lisp/tests_memory_lifetime_boundary_commit_groups.c3`: `616` lines
- `src/lisp/tests_memory_lifetime_env_copy_groups.c3`: `597` lines
- `src/lisp/eval_type_evaluators.c3`: `593` lines
- `src/lisp/jit_jit_closure_define_qq.c3`: `586` lines
- `src/lisp/tests_deduce_query_groups.c3`: `571` lines
- `src/lisp/tests_advanced_core_unicode_groups.c3`: `570` lines
- `src/lisp/json.c3`: `558` lines
- `src/lisp/jit_jit_compile_expr_core.c3`: `556` lines
- `src/lisp/scheduler_primitives.c3`: `555` lines
- `src/lisp/tests_tests.c3`: `553` lines
- `src/lisp/async_tcp_transport_core.c3`: `546` lines
- `src/lisp/primitives_meta_types.c3`: `541` lines
- `src/lisp/tests_scheduler_offload_thread_groups.c3`: `541` lines
- `src/lisp/deduce.c3`: `537` lines
- `src/lisp/tests_memory_lifetime_finalize_groups.c3`: `537` lines
- `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3`: `534` lines
- `src/lisp/tests_runtime_async_groups.c3`: `532` lines
- `src/lisp/eval_boundary_commit_flow.c3`: `531` lines
- `src/lisp/scheduler_wakeup_dispatch.c3`: `520` lines
- `src/lisp/value_constructors.c3`: `509` lines
- `src/lisp/tests_memory_lifetime_boundary_groups.c3`: `506` lines
- `src/lisp/eval_type_evaluators.c3` (post-step-35 baseline): `498` lines
- `src/lisp/scheduler_primitives_threads.c3` (post-step-36 baseline): `492` lines
- `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` (post-step-37 baseline): `492` lines
- `src/lisp/unicode.c3` (post-step-38 baseline): `487` lines
- `src/lisp/json.c3` (post-step-39 baseline): `485` lines
- `src/lisp/value_core_types.c3` (post-step-40 baseline): `476` lines

After:
- `src/lisp/schema.c3`: `450` lines
- `src/lisp/schema_validation.c3`: `253` lines
- `src/lisp/schema_explain_effect.c3`: `428` lines
- `src/lisp/eval_dispatch_types.c3`: `243` lines
- `src/lisp/eval_dispatch_match.c3`: `456` lines
- `src/lisp/scheduler_offload_worker.c3`: `190` lines
- `src/lisp/scheduler_offload_ops.c3`: `375` lines
- `src/lisp/tests_memory_lifetime_groups.c3`: `236` lines
- `src/lisp/tests_memory_lifetime_root_boundary_groups.c3`: `302` lines
- `src/lisp/tests_memory_lifetime_root_boundary_promotion_groups.c3`: `194` lines
- `src/lisp/tests_memory_lifetime_finalize_groups.c3`: `537` lines
- `src/lisp/tests_memory_lifetime_boundary_ast_bench_groups.c3`: `357` lines
- `src/lisp/tests_memory_lifetime_ast_arena_bench_suites.c3`: `351` lines
- `src/lisp/eval_boundary_api.c3`: `300` lines
- `src/lisp/eval_boundary_api_meta.c3`: `331` lines
- `src/lisp/tests_memory_lifetime_boundary_commit_groups.c3`: `86` lines
- `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3`: `534` lines
- `src/lisp/tests_memory_lifetime_env_copy_groups.c3`: `410` lines
- `src/lisp/tests_memory_lifetime_env_copy_closure_groups.c3`: `191` lines
- `src/lisp/eval_boundary_diagnostics.c3`: `160` lines
- `src/lisp/eval_boundary_graph_audit.c3`: `473` lines
- `src/lisp/eval_boundary_graph_audit_meta.c3`: `373` lines
- `src/lisp/tests_advanced_type_effect_ffi_groups.c3`: `129` lines
- `src/lisp/tests_advanced_type_dispatch_groups.c3`: `294` lines
- `src/lisp/tests_advanced_type_parametric_groups.c3`: `420` lines
- `src/lisp/tests_scheduler_io_task_groups.c3`: `289` lines
- `src/lisp/tests_scheduler_offload_thread_groups.c3`: `541` lines
- `src/lisp/eval_promotion_escape.c3`: `374` lines
- `src/lisp/eval_promotion_root_store.c3`: `356` lines
- `src/lisp/eval_type_evaluators.c3`: `325` lines
- `src/lisp/eval_type_declarations.c3`: `181` lines
- `src/lisp/eval_type_instance_builder.c3`: `99` lines
- `src/lisp/jit_jit_closure_define_qq.c3`: `282` lines
- `src/lisp/jit_jit_closure_support.c3`: `309` lines
- `src/lisp/tests_deduce_query_groups.c3`: `409` lines
- `src/lisp/tests_deduce_query_scan_groups.c3`: `165` lines
- `src/lisp/tests_advanced_core_unicode_groups.c3`: `433` lines
- `src/lisp/tests_advanced_core_semantics_groups.c3`: `139` lines
- `src/lisp/json.c3`: `118` lines
- `src/lisp/json_pointer_options.c3`: `368` lines
- `src/lisp/value_core_types.c3`: `208` lines
- `src/lisp/value_runtime_types.c3`: `269` lines
- `src/lisp/json_yyjson_ffi.c3`: `74` lines
- `src/lisp/jit_jit_compile_expr_core.c3`: `320` lines
- `src/lisp/jit_jit_compile_expr_dispatch.c3`: `237` lines
- `src/lisp/scheduler_primitives.c3`: `405` lines
- `src/lisp/scheduler_primitives_offload.c3`: `152` lines
- `src/lisp/tests_tests.c3`: `406` lines
- `src/lisp/tests_slice_policy.c3`: `151` lines
- `src/lisp/async_tcp_transport_core.c3`: `411` lines
- `src/lisp/async_tcp_transport_helpers.c3`: `137` lines
- `src/lisp/primitives_meta_types.c3`: `429` lines
- `src/lisp/primitives_meta_predicates.c3`: `113` lines
- `src/lisp/tests_scheduler_offload_thread_groups.c3`: `383` lines
- `src/lisp/tests_scheduler_offload_bench_groups.c3`: `162` lines
- `src/lisp/deduce.c3`: `363` lines
- `src/lisp/deduce_db_handles.c3`: `175` lines
- `src/lisp/tests_memory_lifetime_finalize_groups.c3`: `209` lines
- `src/lisp/tests_memory_lifetime_finalize_parity_groups.c3`: `332` lines
- `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3`: `213` lines
- `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`: `329` lines
- `src/lisp/tests_runtime_async_groups.c3`: `156` lines
- `src/lisp/tests_runtime_async_io_tls_groups.c3`: `383` lines
- `src/lisp/eval_boundary_commit_flow.c3`: `285` lines
- `src/lisp/eval_boundary_commit_escape_builders.c3`: `250` lines
- `src/lisp/scheduler_wakeup_dispatch.c3`: `316` lines
- `src/lisp/scheduler_wakeup_queue.c3`: `207` lines
- `src/lisp/scheduler_primitives_threads.c3`: `136` lines
- `src/lisp/scheduler_primitives_thread_helpers.c3`: `357` lines
- `src/lisp/value_constructors.c3`: `249` lines
- `src/lisp/value_constructors_core.c3`: `264` lines
- `src/lisp/tests_memory_lifetime_boundary_groups.c3`: `291` lines
- `src/lisp/tests_memory_lifetime_boundary_state_groups.c3`: `219` lines
- `src/lisp/unicode.c3`: `212` lines
- `src/lisp/unicode_case_mapping.c3`: `276` lines

## Validation

- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=schema ./build/main`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler OMNI_SCHEDULER_BENCH=1 ./build/main`
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main`
- `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main`
- `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler OMNI_SCHEDULER_BENCH=1 ./build/main`
- `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main` (post-step-10 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-10 check)
- `c3c build` (post-step-11 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-11 check)
- `c3c build` (post-step-12 check)
- `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main` (post-step-12 check; still 3 known failures)
- `c3c build` (post-step-13 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-13 check)
- `c3c build` (post-step-14 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-14 check)
- `c3c build` (post-step-15 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-15 check)
- `c3c build` (post-step-16 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-16 check)
- `c3c build` (post-step-17 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-17 check)
- `c3c build` (post-step-18 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-18 check)
- `c3c build` (post-step-19 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main` (post-step-19 check)
- `c3c build` (post-step-20 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main` (post-step-20 check; 4 known failures)
- `c3c build` (post-step-21 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main` (post-step-21 check)
- `c3c build` (post-step-22 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main` (post-step-22 check)
- `c3c build` (post-step-23 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler OMNI_SCHEDULER_BENCH=1 ./build/main` (post-step-23 check)
- `c3c build` (post-step-24 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main` (post-step-24 check)
- `c3c build` (post-step-25 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=async ./build/main` (post-step-25 check; 2 dns-resolve failures)
- `c3c build` (post-step-26 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main` (post-step-26 check)
- `c3c build` (post-step-27 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler OMNI_SCHEDULER_BENCH=1 ./build/main` (post-step-27 check)
- `c3c build` (post-step-28 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main` (post-step-28 check)
- `c3c build` (post-step-29 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-29 check)
- `c3c build` (post-step-30 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-30 check)
- `c3c build` (post-step-31 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=async ./build/main` (post-step-31 check; 2 known dns-resolve failures)
- `c3c build` (post-step-32 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-32 check)
- `c3c build` (post-step-33 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler OMNI_SCHEDULER_BENCH=1 ./build/main` (post-step-33 check)
- `c3c build` (post-step-34 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main` (post-step-34 check)
- `c3c build` (post-step-35 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-35 check)
- `c3c build` (post-step-36 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main` (post-step-36 check; 4 known failures)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main` (post-step-36 check)
- `c3c build` (post-step-37 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler OMNI_SCHEDULER_BENCH=1 ./build/main` (post-step-37 check)
- `c3c build` (post-step-38 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main` (post-step-38 check)
- `c3c build` (post-step-39 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main` (post-step-39 check; 4 known failures)
- `c3c build` (post-step-40 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main` (post-step-40 check)
- `c3c build` (post-step-41 check)
- `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main` (post-step-41 check)

Outcome:
- Behavior unchanged in targeted slices.
- Hot-path code remains intact, with diagnostics/explainability and operation
  surfaces split into dedicated files for follow-up work.
- Current workspace note: `advanced` slice reports 4 existing failures
  (`non-tail recursion exceeds former 1024 eval cap`, `parser matrix accepts Value bool constructor`, `match Some`, `match nested Some`);
- Current workspace note: `async` slice reports 2 dns failures
  (`dns-resolve localhost (fiber)`, `dns-resolve returns string (fiber)`).
  `basic`, `scheduler`, `deduce`, and `memory-lifetime-smoke` validation slices pass.
