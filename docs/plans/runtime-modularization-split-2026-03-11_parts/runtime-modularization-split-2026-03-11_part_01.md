# runtime-modularization-split-2026-03-11 Part 01

Source: `docs/plans/runtime-modularization-split-2026-03-11.md`

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
- extracted dispatch/match diagnostics formatting into:
  - `src/lisp/eval_dispatch_match_errors.c3`
- retained type registry + type-query surface in:
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

18. `jit_closure_define_qq.c3` (top-down continuation split)
- extracted closure/type-signature/tco helper block into:
  - `src/lisp/jit_closure_support.c3`
- retained let-rec/set-path/env-extend helpers in:
  - `src/lisp/jit_closure_define_qq.c3`

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

22. `jit_compile_expr_core.c3` (top-down continuation split)
- extracted fallback-audit metadata and expression-family dispatch front-end into:
  - `src/lisp/jit_compile_expr_dispatch.c3`
- retained concrete core call/app expression lowerings in:
  - `src/lisp/jit_compile_expr_core.c3`

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

42. `deduce_rule_eval.c3` (top-down continuation split)
- extracted `deduce/analyze` and `deduce/rule!` entrypoints plus stratification validation into:
  - `src/lisp/deduce_rule_eval_prims.c3`
- retained SCC planning and rule-evaluation helpers in:
  - `src/lisp/deduce_rule_eval.c3`

43. `tests_deduce_rule_groups.c3` (top-down continuation split)
- extracted join/schema/analyze validation blocks into:
  - `src/lisp/tests_deduce_rule_groups_more.c3`
- retained helper utilities plus rule-definition and explain smoke coverage in:
  - `src/lisp/tests_deduce_rule_groups.c3`
- post-step-43 line counts:
  - `src/lisp/tests_deduce_rule_groups.c3`: `582` lines
  - `src/lisp/tests_deduce_rule_groups_more.c3`: `1019` lines
- next largest target after this split:
  - `src/lisp/deduce_rule_eval.c3` (`1499` lines)

44. `deduce_rule_eval.c3` (top-down continuation split)
- extracted recursive/component fixpoint evaluation helpers into:
  - `src/lisp/deduce_rule_eval_fixpoint.c3`
- retained SCC planning, delta helpers, and naive/semi-naive rule evaluation in:
  - `src/lisp/deduce_rule_eval.c3`
- post-step-44 line counts:
  - `src/lisp/deduce_rule_eval.c3`: `1242` lines
  - `src/lisp/deduce_rule_eval_fixpoint.c3`: `260` lines
- next largest target after this split:
  - `src/lisp/tests_deduce_query_bench_groups.c3` (`1266` lines)

45. `deduce_rule_eval.c3` (top-down continuation split)
- extracted execution/context helpers and rule evaluation dispatch into:
  - `src/lisp/deduce_rule_eval_exec.c3`
- retained SCC planning in:
  - `src/lisp/deduce_rule_eval.c3`
- post-step-45 line counts:
  - `src/lisp/deduce_rule_eval.c3`: `442` lines
  - `src/lisp/deduce_rule_eval_exec.c3`: `805` lines
- next largest target after this split:
  - `src/lisp/tests_deduce_query_bench_groups.c3` (`1266` lines)

46. `tests_deduce_query_bench_groups.c3` (top-down continuation split)
- retained seed/assert helpers and query benchmark support in:
  - `src/lisp/tests_deduce_query_bench_groups.c3`
- extracted benchmark runner/reporting entrypoints into:
  - `src/lisp/tests_deduce_query_bench_groups_more.c3`
- post-step-46 line counts:
  - `src/lisp/tests_deduce_query_bench_groups.c3`: `410` lines
  - `src/lisp/tests_deduce_query_bench_groups_more.c3`: `24` lines
- next largest target after this split:
  - `src/lisp/deduce_db_handles.c3` (`1142` lines)

47. `deduce_db_handles.c3` (top-down continuation split)
- extracted lower mutation/state bookkeeping and transaction helpers into:
  - `src/lisp/deduce_db_handles_mutation.c3`
- retained LMDB externs, schema registration, and relation/index helpers in:
  - `src/lisp/deduce_db_handles.c3`
- post-step-47 line counts:
  - `src/lisp/deduce_db_handles.c3`: `544` lines
  - `src/lisp/deduce_db_handles_mutation.c3`: `602` lines
- next largest target after this split:
  - `src/lisp/tests_advanced_core_unicode_groups.c3` (`700` lines)

48. `tests_advanced_core_unicode_groups.c3` (top-down continuation split)
- retained unicode, logic, effect, and runtime-control test groups in:
  - `src/lisp/tests_advanced_core_unicode_groups.c3`
- extracted block-syntax, lambda-syntax, and binding/mutation groups into:
  - `src/lisp/tests_advanced_core_unicode_groups_more.c3`
- post-step-48 line counts:
  - `src/lisp/tests_advanced_core_unicode_groups.c3`: `540` lines
  - `src/lisp/tests_advanced_core_unicode_groups_more.c3`: `101` lines
- next largest target after this split:
  - `src/lisp/tests_runtime_feature_jit_groups.c3` (`1039` lines)

49. `aot.c3` (top-down continuation split)
- retained AOT type/definition helpers in:
  - `src/lisp/aot.c3`
- extracted the closure/runtime bridge helpers into:
  - `src/lisp/aot_runtime_bridge.c3`
- post-step-49 line counts:
  - `src/lisp/aot.c3`: `677` lines
  - `src/lisp/aot_runtime_bridge.c3`: `227` lines
- next largest target after this split:
  - `src/lisp/tests_deduce_rule_groups_more.c3` (`1019` lines)

50. `tests_deduce_rule_groups_more.c3` (top-down continuation split)
- retained the join and schema validation blocks in:
  - `src/lisp/tests_deduce_rule_groups_more.c3`
- extracted the schema-estimate and recursive/analyze validation blocks into:
  - `src/lisp/tests_deduce_rule_groups_more_tail.c3`
- post-step-50 line counts:
  - `src/lisp/tests_deduce_rule_groups_more.c3`: `618` lines
  - `src/lisp/tests_deduce_rule_groups_more_tail.c3`: `408` lines
- next largest target after this split:
  - `src/lisp/tests_scheduler_boundary_worker.c3` (`836` lines)

51. `tests_scheduler_boundary_worker.c3` (top-down continuation split)
- retained the offload, wakeup, and thread-task prestart/cancel boundary tests in:
  - `src/lisp/tests_scheduler_boundary_worker.c3`
- extracted the DNS/connect, join-timeout, and shared-retire queue boundary tests into:
  - `src/lisp/tests_scheduler_boundary_worker_more.c3`
- post-step-51 line counts:
  - `src/lisp/tests_scheduler_boundary_worker.c3`: `531` lines
  - `src/lisp/tests_scheduler_boundary_worker_more.c3`: `310` lines
- next largest target after this split:
  - `src/lisp/deduce_rule_eval_exec.c3` (`805` lines)

52. `deduce_rule_eval_exec.c3` (top-down continuation split)
- retained seminaive evaluation helpers in:
  - `src/lisp/deduce_rule_eval_exec.c3`
- extracted naive evaluation helpers into:
  - `src/lisp/deduce_rule_eval_exec_naive.c3`
- post-step-52 line counts:
  - `src/lisp/deduce_rule_eval_exec.c3`: `638` lines
  - `src/lisp/deduce_rule_eval_exec_naive.c3`: `171` lines
- next largest target after this split:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3` (`793` lines)

53. `tests_scheduler_boundary_thread_task_groups.c3` (boundary continuation split)
- retained the stress/offload/wakeup/cancel boundary tests in:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`
- extracted the join-wait failure mapping and mixed state restoration tests into:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`
- post-step-53 line counts:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`: `584` lines
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`: `214` lines
- next largest target after this split:
  - `src/lisp/jit_handle_signal.c3` (`551` lines)

54. `jit_handle_signal.c3` (signal-dispatch continuation split)
- retained the lower-level effect state and signal dispatch machinery in:
  - `src/lisp/jit_handle_signal.c3`
- extracted eval-result conversion, pending-raise dispatch, warm-clauses handling, handle-state setup, body switching, no-signal finish, implementation, and continuation application into:
  - `src/lisp/jit_handle_signal_handle.c3`
- post-step-54 line counts:
  - `src/lisp/jit_handle_signal.c3`: `551` lines
  - `src/lisp/jit_handle_signal_handle.c3`: `225` lines
- next largest target after this split:
  - `src/lisp/deduce_rule_ops.c3` (`767` lines)

55. `deduce_rule_ops.c3` (explain-path continuation split)
- retained parsing, validation, planning, and predicate-index helpers in:
  - `src/lisp/deduce_rule_ops.c3`
- extracted explain-plan rendering helpers and the public `deduce/explain` primitive into:
  - `src/lisp/deduce_rule_ops_explain.c3`
- post-step-55 line counts:
  - `src/lisp/deduce_rule_ops.c3`: `548` lines
  - `src/lisp/deduce_rule_ops_explain.c3`: `219` lines
- next largest target after this split:
  - `src/lisp/jit_compiler.c3` (`753` lines)

56. `jit_compiler.c3` (compile-path continuation split)
- retained runtime cache / lifecycle helpers in:
  - `src/lisp/jit_compiler.c3`
- extracted JIT state tracking and `jit_compile(...)` into:
  - `src/lisp/jit_compiler_compile.c3`
- post-step-56 line counts:
  - `src/lisp/jit_compiler.c3`: `638` lines
  - `src/lisp/jit_compiler_compile.c3`: `121` lines
- next largest target after this split:
  - `src/lisp/tests_compiler_core_groups.c3` (`716` lines)

57. `tests_compiler_core_groups.c3` (compiler test-suite continuation split)
- retained the syntax, stdlib, feature, and serializer groups in:
  - `src/lisp/tests_compiler_core_groups.c3`
- extracted set/path/continuation compiler groups into:
  - `src/lisp/tests_compiler_core_groups_more.c3`
- post-step-57 line counts:
  - `src/lisp/tests_compiler_core_groups.c3`: `476` lines
  - `src/lisp/tests_compiler_core_groups_more.c3`: `245` lines
- next largest target after this split:
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3` (`709` lines)

58. `tests_memory_lifetime_env_copy_groups.c3` (env-copy boundary continuation split)
- retained memo, parent rewrite, mixed-chain rewrite, and closure-retain stress tests in:
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3`
- extracted escape/fault/guard/reject/rollback env-copy tests into:
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3`
- post-step-58 line counts:
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3`: `476` lines
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3`: `245` lines
- next largest target after this split:
  - `src/lisp/deduce_relation_scan_helpers.c3` (`691` lines)

59. `deduce_relation_scan_helpers.c3` (relation-scan continuation split)
- retained iterator/join/count helpers in:
  - `src/lisp/deduce_relation_scan_helpers.c3`
- extracted relation materialization, comparison, bound parsing, and scan entrypoints into:
  - `src/lisp/deduce_relation_scan_helpers_more.c3`
- post-step-59 line counts:
  - `src/lisp/deduce_relation_scan_helpers.c3`: `477` lines
  - `src/lisp/deduce_relation_scan_helpers_more.c3`: `217` lines
- next largest target after this split:
  - `src/lisp/aot.c3` (`677` lines)

60. `aot.c3` (top-down continuation split)
- retained AOT type/definition helpers and core runtime bridge entrypoints in:
  - `src/lisp/aot.c3`
- extracted the tail-call trampoline state, invoke/apply helpers, and older debug/value helpers into:
  - `src/lisp/aot_runtime_bridge.c3`
- post-step-60 line counts:
  - `src/lisp/aot.c3`: `467` lines
  - `src/lisp/aot_runtime_bridge.c3`: `433` lines
- next largest target after this split:
  - `src/lisp/tests_runtime_feature_jit_groups.c3` (`654` lines)

61. `tests_runtime_feature_jit_groups.c3` (top-down continuation split)
- retained the cache, GC, and pre-interpreter-lifetime policy tests in:
  - `src/lisp/tests_runtime_feature_jit_groups.c3`
- extracted the multi-interpreter, continuation, handle-state, and capture-boundary policy tests into:
  - `src/lisp/tests_runtime_feature_jit_groups_tail.c3`
- post-step-61 line counts:
  - `src/lisp/tests_runtime_feature_jit_groups.c3`: `345` lines
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`: `392` lines
  - `src/lisp/tests_runtime_feature_jit_groups_tail.c3`: `314` lines
- next largest target after this split:
  - `src/lisp/jit_compiler.c3` (`638` lines)

62. `jit_compiler.c3` (top-down continuation split)
- retained the JIT compiler state, cache, and attachment bookkeeping in:
  - `src/lisp/jit_compiler.c3`
- extracted the lifecycle, GC, exec-depth, and liveness helpers into:
  - `src/lisp/jit_compiler_lifecycle.c3`
- post-step-62 line counts:
  - `src/lisp/jit_compiler.c3`: `418` lines
  - `src/lisp/jit_compiler_lifecycle.c3`: `225` lines
- next largest target after this split:
  - `src/lisp/deduce_rule_eval_exec.c3` (`638` lines)

63. `deduce_rule_eval_exec.c3` (top-down continuation split)
- retained the delta-set, SCC, and naive-rule execution helpers in:
  - `src/lisp/deduce_rule_eval_exec.c3`
- extracted the seminaive execution helpers into:
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
- post-step-63 line counts:
  - `src/lisp/deduce_rule_eval_exec.c3`: `368` lines
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`: `275` lines
- next largest target after this split:
  - `src/lisp/tests_scheduler_groups.c3` (`637` lines)

64. `tests_scheduler_groups.c3` (top-down continuation split)
- retained the spawn/await and run-loop failure scheduler tests in:
  - `src/lisp/tests_scheduler_groups.c3`
- extracted the scheduler wakeup helpers/tests into:
  - `src/lisp/tests_scheduler_groups_more.c3`
- post-step-64 line counts:
  - `src/lisp/tests_scheduler_groups.c3`: `424` lines
  - `src/lisp/tests_scheduler_groups_more.c3`: `218` lines
- next largest target after this split:
  - `src/lisp/jit_runtime_effects.c3` (`625` lines)

65. `jit_runtime_effects.c3` (top-down continuation split)
- retained the resolve-side runtime effect helpers in:
  - `src/lisp/jit_runtime_effects.c3`
- extracted the handler-application helpers into:
  - `src/lisp/jit_runtime_effects_handle.c3`
- post-step-65 line counts:
  - `src/lisp/jit_runtime_effects.c3`: `422` lines
  - `src/lisp/jit_runtime_effects_handle.c3`: `206` lines
- next largest target after this split:
  - `src/lisp/tests_deduce_rule_groups_more.c3` (`618` lines)

66. `tests_deduce_rule_groups_more.c3` (top-down continuation split)
- retained the first four join validation families in:
  - `src/lisp/tests_deduce_rule_groups_more.c3`
- extracted the adaptive-join validation families into:
  - `src/lisp/tests_deduce_rule_groups_more_join.c3`
- post-step-66 line counts:
  - `src/lisp/tests_deduce_rule_groups_more.c3`: `520` lines
  - `src/lisp/tests_deduce_rule_groups_more_join.c3`: `106` lines
- next largest target after this split:
  - `src/lisp/deduce_db_handles_mutation.c3` (`602` lines)

67. `deduce_db_handles_mutation.c3` (top-down continuation split)
- retained the schema, dirty-tracking, and rule-signature helpers in:
  - `src/lisp/deduce_db_handles_mutation.c3`
- extracted the transaction mutation-log helpers into:
  - `src/lisp/deduce_db_handles_mutation_txn.c3`
- post-step-67 line counts:
  - `src/lisp/deduce_db_handles_mutation.c3`: `466` lines
  - `src/lisp/deduce_db_handles_mutation_txn.c3`: `139` lines
- next largest target after this split:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3` (`584` lines)

68. `tests_scheduler_boundary_thread_task_groups.c3` (top-down continuation split)
- retained the scheduler/thread-task core tests in:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`
- extracted the waiter and cancel-conversion boundary families into:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`
- post-step-68 line counts:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`: `142` lines
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`: `664` lines
- next largest target after this split:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3` (`664` lines)

69. `tests_scheduler_boundary_thread_task_groups_more.c3` (top-down continuation split)
- retained the waiter, cancel, and cancel-conversion boundary families in:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`
- extracted the join-wait mapping and mixed boundary-state restore families into:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more_tail.c3`
- post-step-69 line counts:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`: `450` lines
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more_tail.c3`: `217` lines
- next largest target after this split:
  - `src/lisp/tests_deduce_rule_groups.c3` (`582` lines)

70. `tests_deduce_rule_groups.c3` (top-down continuation split)
- retained the deduce rule validation helpers and planner-order checks in:
  - `src/lisp/tests_deduce_rule_groups.c3`
- extracted the explain payload validation block into:
  - `src/lisp/tests_deduce_rule_groups_explain.c3`
- post-step-70 line counts:
  - `src/lisp/tests_deduce_rule_groups.c3`: `419` lines
  - `src/lisp/tests_deduce_rule_groups_explain.c3`: `170` lines
- next largest target after this split:
  - `src/lisp/jit_eval_scopes.c3` (`557` lines)

71. `jit_eval_scopes.c3` (top-down continuation split)
- retained the JIT lookup/eval wrappers and TCO recycle entrypoints in:
  - `src/lisp/jit_eval_scopes.c3`
- extracted the scope-chain, finalize, and recycle helper layer into:
  - `src/lisp/jit_eval_scopes_helpers.c3`
- post-step-71 line counts:
  - `src/lisp/jit_eval_scopes.c3`: `368` lines
  - `src/lisp/jit_eval_scopes_helpers.c3`: `192` lines
- next largest target after this split:
  - `src/lisp/jit_handle_signal.c3` (`551` lines)

72. `jit_handle_signal.c3` (top-down continuation split)
- retained the signal/dispatch entrypoints in:
  - `src/lisp/jit_handle_signal.c3`
- extracted the shared effect-state, continuation scan, and fast-path helpers into:
  - `src/lisp/jit_handle_signal_helpers.c3`
- post-step-72 line counts:
  - `src/lisp/jit_handle_signal.c3`: `108` lines
  - `src/lisp/jit_handle_signal_helpers.c3`: `446` lines
- next largest target after this split:
  - `src/lisp/deduce_rule_ops.c3` (`548` lines)

73. `deduce_rule_ops.c3` (top-down continuation split)
- retained parsing, IR, and safety-validation helpers in:
  - `src/lisp/deduce_rule_ops.c3`
- extracted the planning/scoring and predicate-index helpers into:
  - `src/lisp/deduce_rule_ops_planning.c3`
- post-step-73 line counts:
  - `src/lisp/deduce_rule_ops.c3`: `412` lines
  - `src/lisp/deduce_rule_ops_planning.c3`: `137` lines
- next largest target after this split:
  - `src/lisp/value_print.c3` (`547` lines)

74. `value_print.c3` (top-down continuation split)
- retained the direct value-printing entrypoints in:
  - `src/lisp/value_print.c3`
- extracted the buffer-backed print helpers and `PrintBuf` into:
  - `src/lisp/value_print_buf.c3`
- post-step-74 line counts:
  - `src/lisp/value_print.c3`: `293` lines
  - `src/lisp/value_print_buf.c3`: `257` lines
- next largest target after this split:
  - `src/lisp/deduce_db_handles.c3` (`544` lines)

75. `tests_advanced_core_unicode_groups.c3` (top-down continuation split)
- retained the Unicode, constructor, iterator, logic, and effect-continuation test helpers in:
  - `src/lisp/tests_advanced_core_unicode_groups.c3`
