# Active TODO

Last condensed: 2026-04-05

This file is now the sole live backlog.
List only still-open items here.

Current actionable count: 10

Completed backlog snapshots:

- `docs/TODO-backup-2026-03-26.md`
- `docs/TODO-backup-2026-03-31.md`
- `docs/TODO-backup-2026-04-01.md`

Closed this pass:

- `REF-SPLIT-EXEC-001`
  The structural deduce/query/explain split lane no longer has oversized residuals. The largest remaining files in that lane are now `src/lisp/deduce_schema_query_input_roles.c3` (116), `src/lisp/deduce_schema_query_metadata_schema_helpers.c3` (115), `src/lisp/deduce_schema_query_refresh_incremental_apply.c3` (114), and `src/lisp/deduce_schema_query_relation_alloc.c3` (113). Remaining code-smell follow-ups from that lane stay tracked separately below under `META-STATS-001`, `QUERY-GD-TXN-001`, and `QUERY-GD-PLAIN-001`.

Use this file only for still-open work.

## Live Queue

- REF-SPLIT-INFRA-001 split oversized runtime infrastructure/bridge files top-down, largest-first
  The structural execution/query/explain split lane is below the oversized-file threshold now, so the real remaining large-file queue is the runtime infrastructure/bridge lane.
  One additional slice is now shipped:
  - binding/value-match/result-dict helpers were extracted out of `src/lisp/unify.c3`, reducing it from 805 lines to 588 lines and isolating that top helper lane in `src/lisp/unify_match_helpers.c3`
  - tail-call trampoline and AOT closure/variadic bridge helpers were extracted out of `src/lisp/aot_runtime_bridge.c3`, reducing it from 793 lines to 416 lines and isolating that top ownership lane in `src/lisp/aot_runtime_bridge_closure.c3`
  - worker queue/interrupt/stdin/command/thread helpers were extracted out of `src/lisp/eval_repl_server.c3`, reducing it from 744 lines to 486 lines and isolating that worker execution lane in `src/lisp/eval_repl_server_worker.c3`
  - literal-value extraction helpers were extracted out of `src/lisp/deduce_schema_query_analysis.c3`, reducing it from 733 lines to 296 lines and isolating that top query-literal analysis lane in `src/lisp/deduce_schema_query_literal_analysis.c3`
  - recursive aggregate capability/fallback helpers were extracted out of `src/lisp/deduce_rule_eval_fixpoint_component_support.c3`, reducing it from 700 lines to 589 lines and isolating that aggregate support lane in `src/lisp/deduce_rule_eval_fixpoint_component_aggregates.c3`
  - schema/fact write-integrity helpers were extracted out of `src/lisp/deduce_relation_ops_validation_write_checks.c3`, reducing it from 689 lines to 155 lines and isolating that write-validation lane in `src/lisp/deduce_relation_ops_validation_fact_checks.c3`
  - filter/disjunction/union head-demand construction helpers were extracted out of `src/lisp/deduce_schema_query_head_demand.c3`, reducing it from 676 lines to 299 lines and isolating that top construction lane in `src/lisp/deduce_schema_query_head_demand_build.c3`
  - seminaive tuple replay/head-emission helpers were extracted out of `src/lisp/deduce_rule_eval_exec_seminaive_execution.c3`, reducing it from 643 lines to 439 lines and isolating that replay/emission lane in `src/lisp/deduce_rule_eval_exec_seminaive_replay_emit.c3`
  - goal-directed selector surface/parse/prepare helpers were extracted out of `src/lisp/deduce_rule_eval_fixpoint_goal_directed.c3`, reducing it from 637 lines to 390 lines and isolating that selector-prepare lane in `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
  - OS-thread handle/spawn/job-execution helpers were extracted out of `src/lisp/scheduler_primitives_thread_helpers.c3`, reducing it from 625 lines to 252 lines and isolating that top spawn/job lane in `src/lisp/scheduler_primitives_thread_spawn.c3`
  - persisted-rule-signature record codec, restore, and persist helpers were extracted out of `src/lisp/deduce_db_handles_persistence.c3`, reducing it from 617 lines to 85 lines and isolating that top persistence lane in `src/lisp/deduce_db_rule_signature_persistence.c3`
  - recursive aggregate finalize/clear/pass/batch helpers were extracted out of `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates.c3`, reducing it from 611 lines to 258 lines and isolating that top helper lane in `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregate_helpers.c3`
  - SCC plan struct, dependency-graph build, Tarjan walk, and stratum ordering helpers were extracted out of `src/lisp/deduce_rule_eval_scc.c3`, reducing it from 605 lines to 140 lines and isolating that top plan-build lane in `src/lisp/deduce_rule_eval_scc_plan.c3`
  - head-demand, observed-counter, tuple-encoding, DBI-open, and tuple-match helpers were extracted out of `src/lisp/deduce_rule_eval_exec.c3`, reducing it from 605 lines to 212 lines and isolating that shared execution-helper lane in `src/lisp/deduce_rule_eval_exec_helpers.c3`
  - recursive/head-demand relation-support analysis helpers were extracted out of `src/lisp/deduce_rule_eval_fixpoint_component_support.c3`, reducing it from 589 lines to 161 lines and isolating that top support-analysis lane in `src/lisp/deduce_rule_eval_fixpoint_component_support_helpers.c3`
  - match head-demand construction and relation scan helpers were extracted out of `src/lisp/unify.c3`, reducing it from 588 lines to 426 lines and isolating that top match-read lane in `src/lisp/unify_scan_helpers.c3`
  - scalar-param and captured-scalar-call equality collectors were extracted out of `src/lisp/deduce_schema_query_filter_equalities.c3`, reducing it from 572 lines to 240 lines and isolating that top filter-analysis lane in `src/lisp/deduce_schema_query_filter_equalities_helpers.c3`
  - relation handle/runtime open-transaction primitives were extracted out of `src/lisp/deduce.c3`, reducing it from 571 lines to 187 lines and isolating that top runtime lane in `src/lisp/deduce_runtime_primitives.c3`
  - network/compression/offload execution helpers were extracted out of `src/lisp/scheduler_offload_ops.c3`, reducing it from 562 lines to 234 lines and isolating that top network lane in `src/lisp/scheduler_offload_network.c3`
  - FTXUI state/error/sequence/component helpers were extracted out of `src/lisp/prim_ui_ftxui.c3`, reducing it from 562 lines to 343 lines and isolating that top helper lane in `src/lisp/prim_ui_ftxui_helpers.c3`
  - LMDB/core schema/rule handle declarations were extracted out of `src/lisp/deduce_db_handles.c3`, reducing it from 557 lines to 317 lines and isolating that top declarations lane in `src/lisp/deduce_db_handles_core.c3`
  - component-delta payload structs, tuple-set codecs, and serialize/restore helpers were extracted out of `src/lisp/deduce_rule_eval_exec_component_delta.c3`, reducing it from 556 lines to 116 lines and isolating that top payload lane in `src/lisp/deduce_rule_eval_exec_component_delta_payload.c3`
  - tuple-support-table bookkeeping, delta-set insertion, and SCC predicate lookup helpers were extracted out of `src/lisp/deduce_rule_eval_exec_component_state.c3`, reducing it from 550 lines to 340 lines and isolating that top helper lane in `src/lisp/deduce_rule_eval_exec_component_state_helpers.c3`
  - aggregate group container/state allocation, support-count bookkeeping, and extremum recomputation helpers were extracted out of `src/lisp/deduce_rule_eval_exec_aggregates.c3`, reducing it from 545 lines to 194 lines and isolating that top state lane in `src/lisp/deduce_rule_eval_exec_aggregate_state.c3`
  - tuple-key compare, schema-view construction, schema-write wrappers, and relation-DBI-open helpers were extracted out of `src/lisp/deduce_relation_ops_validation_fact_checks.c3`, reducing it from 537 lines to 393 lines and isolating that top validation-helper lane in `src/lisp/deduce_relation_ops_validation_fact_helpers.c3`
  - queue-work, parse, batch, and custom-job orchestration helpers were extracted out of `src/lisp/scheduler_primitives_offload.c3`, reducing it from 537 lines to 72 lines and isolating that top job-execution lane in `src/lisp/scheduler_primitives_offload_jobs.c3`
  - persisted-rule-signature record layout and tuple/record codec helpers were extracted out of `src/lisp/deduce_db_rule_signature_persistence.c3`, reducing it from 533 lines to 346 lines and isolating that top codec lane in `src/lisp/deduce_db_rule_signature_record_codec.c3`
  - relation-schema storage teardown, inferred-column helpers, runtime-default application, and schema initialization were extracted out of `src/lisp/deduce_db_handles_register.c3`, reducing it from 529 lines to 212 lines and isolating that top schema-init lane in `src/lisp/deduce_db_relation_schema_init.c3`
  - REPL capture/result/preload/eval/load-file/complete session operations were extracted out of `src/lisp/eval_repl_server.c3`, reducing it from 486 lines to 251 lines and isolating that top session-ops lane in `src/lisp/eval_repl_server_session_ops.c3`
  - REPL protocol read/request structs and parse/free helpers were extracted out of `src/lisp/eval_repl_server_state.c3`, reducing it from 486 lines to 229 lines and isolating that top protocol lane in `src/lisp/eval_repl_server_protocol.c3`
  - flat native-match guard detection and guard-lowering helpers were extracted out of `src/lisp/compiler_native_match_compilation_flat_style.c3`, reducing it from 486 lines to 324 lines and isolating that guard lane in `src/lisp/compiler_native_match_compilation_guards.c3`
  - hashmap hash/equality/canonical-order helpers were extracted out of `src/lisp/prim_collection_hashmap.c3`, reducing it from 482 lines to 239 lines and isolating that key/iteration lane in `src/lisp/prim_collection_hashmap_key_helpers.c3`
  - generic offload execute/join helpers were extracted out of `src/lisp/scheduler_primitives_offload_jobs.c3`, reducing it from 467 lines to 271 lines and isolating that execute lane in `src/lisp/scheduler_primitives_offload_execute.c3`
  - SCC plan struct, dependency-graph, plan-free, predicate-index, and Tarjan helpers were extracted out of `src/lisp/deduce_rule_eval_scc_plan.c3`, reducing it from 466 lines to 299 lines and isolating that helper lane in `src/lisp/deduce_rule_eval_scc_helpers.c3`
  - pattern match result/gensym helpers were extracted out of `src/lisp/eval_pattern_support_helpers.c3`, reducing it from 454 lines to 339 lines and isolating that top match-support lane in `src/lisp/eval_pattern_match_support.c3`
  - TLS async-runtime guard, fiber-offload, and resume-policy helpers were extracted out of `src/lisp/tls_primitives.c3`, reducing it from 445 lines to 397 lines and isolating that top runtime-support lane in `src/lisp/tls_runtime_helpers.c3`
  - TLS connect, trust-store-cause, and server-wrap helpers were extracted out of `src/lisp/tls_primitives.c3`, reducing it from 397 lines to 132 lines and isolating that top connection/setup lane in `src/lisp/tls_connection_primitives.c3`
  - observed-step counter structs, note/reset helpers, and observed-counter collection were extracted out of `src/lisp/deduce_rule_eval_exec_helpers.c3`, reducing it from 395 lines to 329 lines and isolating that top observed-counter lane in `src/lisp/deduce_rule_eval_exec_observed.c3`
  - key and unique fact-integrity checks were extracted out of `src/lisp/deduce_relation_ops_validation_fact_checks.c3`, reducing it from 393 lines to 242 lines and isolating that top uniqueness-validation lane in `src/lisp/deduce_relation_ops_validation_key_unique.c3`
  - row-dict capacity/materialization, list-reverse, and column-key initialization helpers were extracted out of `src/lisp/deduce_relation_scan_helpers_more.c3`, reducing it from 392 lines to 325 lines and isolating that top row-materialization lane in `src/lisp/deduce_relation_row_materialization.c3`
  - goal-directed relation-selector execution, selected-schema estimate snapshot/restore, dirty-target plan preparation, and plain relation-read helpers were extracted out of `src/lisp/deduce_rule_eval_fixpoint_goal_directed.c3`, reducing it from 390 lines to 64 lines and isolating that top goal-directed read lane in `src/lisp/deduce_rule_eval_fixpoint_goal_directed_read.c3`
  - component-delta tuple-set release/growth and raw byte codec helpers were extracted out of `src/lisp/deduce_rule_eval_exec_component_delta_payload.c3`, reducing it from 441 lines to 260 lines and isolating that low-level codec lane in `src/lisp/deduce_rule_eval_exec_component_delta_codec.c3`
  - env-copy value/closure/iterator policy and promotion-context helpers were extracted out of `src/lisp/eval_env_copy_helpers.c3`, reducing it from 440 lines to 271 lines and isolating that top value-policy lane in `src/lisp/eval_env_copy_values.c3`
  - query-literal control-flow/symbolic/call helpers were extracted out of `src/lisp/deduce_schema_query_literal_analysis.c3`, reducing it from 440 lines to 52 lines and isolating that recursive literal-eval lane across `src/lisp/deduce_schema_query_literal_call_helpers.c3` and `src/lisp/deduce_schema_query_literal_numeric_calls.c3`
  - seminaive rule-step recursion was extracted out of `src/lisp/deduce_rule_eval_exec_seminaive_execution.c3`, reducing it from 439 lines to 214 lines and isolating that step-execution lane in `src/lisp/deduce_rule_eval_exec_seminaive_step.c3`
  - cross-scope env promotion/barrier write helpers were extracted out of `src/lisp/value_environment.c3`, reducing it from 438 lines to 310 lines and isolating that write-barrier lane in `src/lisp/value_environment_barrier.c3`
  - primitive table definitions and bulk registration helpers were extracted out of `src/lisp/eval_init_primitives.c3`, reducing it from 435 lines to 123 lines and isolating that table-driven bootstrap lane in `src/lisp/eval_init_primitive_tables.c3`
  - TCP connect/listen/accept transport-control helpers were extracted out of `src/lisp/async_tcp_transport_core.c3`, reducing it from 431 lines to 126 lines and isolating that listener/control lane in `src/lisp/async_tcp_transport_listen.c3`
  - boundary commit state-machine execution was extracted out of `src/lisp/eval_boundary_commit_flow.c3`, reducing it from 430 lines to 64 lines and isolating that escape-commit lane in `src/lisp/eval_boundary_commit_escape.c3`
  - process/signal handle structs, transport/process/signal handle wrappers, handle finalizers, and signal runtime registration helpers were extracted out of `src/lisp/async_process_signal_handles.c3`, reducing it from 429 lines to 161 lines and isolating that runtime ownership lane in `src/lisp/async_process_signal_runtime.c3`
  - selector/plain goal-directed match execution helpers were extracted out of `src/lisp/unify.c3`, reducing it from 426 lines to 161 lines and isolating that runtime execution lane in `src/lisp/unify_goal_directed.c3`
  - async request token, pending-handle lookup, pending close, and pending completion helpers were extracted out of `src/lisp/scheduler_wakeup_io.c3`, reducing it from 426 lines to 122 lines and isolating that pending-lifecycle lane in `src/lisp/scheduler_wakeup_io_lifecycle.c3`
  - low-level rule-body/head-predicate support predicates were extracted out of `src/lisp/deduce_rule_eval_fixpoint_component_support_helpers.c3`, reducing it from 426 lines to 232 lines and isolating that predicate-analysis lane in `src/lisp/deduce_rule_eval_fixpoint_component_support_predicates.c3`
  - tuple mutation helpers for `fact!`/`retract!` were extracted out of `src/lisp/deduce_relation_ops_mutations.c3`, reducing it from 426 lines to 139 lines and isolating that row-mutation lane in `src/lisp/deduce_relation_ops_fact_mutations.c3`
  - string append/upcase/downcase/trim/join/substring/split primitives were extracted out of `src/lisp/prim_string_ops.c3`, reducing it from 423 lines to 211 lines and isolating that transform/slice lane in `src/lisp/prim_string_transform.c3`
  - module declaration, source-dir, module-path, and file-module setup helpers were extracted out of `src/lisp/jit_jit_module_import.c3`, reducing it from 422 lines to 131 lines and isolating that setup lane in `src/lisp/jit_jit_module_import_setup.c3`
  - FTXUI ABI constants, aliases, and option structs were extracted out of `src/lisp/ftxui_ffi.c3`, reducing it from 419 lines to 104 lines and isolating that type-definition lane in `src/lisp/ftxui_ffi_types.c3`
  - schema explain dict/symbol/payload construction helpers were extracted out of `src/lisp/schema_explain_helpers.c3`, reducing it from 418 lines to 204 lines and isolating that payload-builder lane in `src/lisp/schema_explain_payload_helpers.c3`
  - boundary commit destination fallback/session/build-scope helpers were extracted out of `src/lisp/eval_boundary_commit_escape_builders.c3`, reducing it from 417 lines to 324 lines and isolating that shared builder lane in `src/lisp/eval_boundary_commit_escape_builder_helpers.c3`
  - AOT truth/match/sequence/import/field/explain bridge helpers were extracted out of `src/lisp/aot_runtime_bridge.c3`, reducing it from 416 lines to 116 lines and isolating that runtime-helper lane in `src/lisp/aot_runtime_bridge_helpers.c3`
  - task-handle allocation/parsing and task spawn/batch-spawn helpers were extracted out of `src/lisp/scheduler_primitives_tasks.c3`, reducing it from 412 lines to 111 lines and isolating that spawn lane in `src/lisp/scheduler_primitives_task_spawn.c3`
  - join-count stats, key extraction, index-bound-mask, and index-selection helpers were extracted out of `src/lisp/deduce_relation_scan_helpers_join.c3`, reducing it from 405 lines to 272 lines and isolating that join-helper lane in `src/lisp/deduce_relation_scan_join_helpers.c3`
  - REPL input buffering, paren-depth tracking, eval/print helpers, and replxx session configuration were extracted out of `src/lisp/eval_repl.c3`, reducing it from 404 lines to 171 lines and isolating that helper lane in `src/lisp/eval_repl_helpers.c3`
  - persisted rule-catalog record layout, entry storage, codec, restore, and persist helpers were extracted out of `src/lisp/deduce_db_handles_persistence_catalog.c3`, reducing it from 403 lines to 29 lines and isolating that persistence lane in `src/lisp/deduce_db_rule_catalog_persistence.c3`
  - seminaive worker-scratch tracing, visible/addition counters, recursive-shape checks, and addition-accumulation helpers were extracted out of `src/lisp/deduce_rule_eval_exec_seminaive_worker_scratch.c3`, reducing it from 399 lines to 301 lines and isolating that helper lane in `src/lisp/deduce_rule_eval_exec_seminaive_worker_scratch_helpers.c3`
  - shared-handle registry/blob lifecycle, publish/project, and release helpers were extracted out of `src/lisp/scheduler_state_shared_handles.c3`, reducing it from 388 lines to 53 lines and isolating that top blob-registry lane in `src/lisp/scheduler_shared_handles_blob.c3`
  - JIT closure/runtime-backend execution, env-copy tracing, and closure-env copy helpers were extracted out of `src/lisp/jit_jit_closure_support.c3`, reducing it from 387 lines to 208 lines and isolating that top closure-runtime lane in `src/lisp/jit_jit_closure_runtime.c3`
  - root-store primitive/array/hashmap/method-table clone helpers were extracted out of `src/lisp/eval_promotion_root_store.c3`, reducing it from 386 lines to 119 lines and isolating that payload-clone lane in `src/lisp/eval_promotion_root_clones.c3`
  - UV callback wakeup bridge helpers were extracted out of `src/lisp/scheduler_wakeup_dispatch.c3`, reducing it from 385 lines to 197 lines and isolating that callback lane in `src/lisp/scheduler_wakeup_callbacks.c3`
  - concrete pattern variant and guard helpers were extracted out of `src/lisp/eval_pattern_matching.c3`, reducing it from 383 lines to 183 lines and isolating that top variant lane in `src/lisp/eval_pattern_match_variants.c3`
  - boundary graph value/env walk helpers were extracted out of `src/lisp/eval_boundary_graph_audit_reachability.c3`, reducing it from 381 lines to 160 lines and isolating that traversal lane in `src/lisp/eval_boundary_graph_audit_walkers.c3`
  - rule IR types, parse/free, and variable-id helpers were extracted out of `src/lisp/deduce_rule_ops.c3`, reducing it from 381 lines to 100 lines and isolating that IR lane in `src/lisp/deduce_rule_ir.c3`
  - tail-call trampoline state and invoke/apply bridge helpers were extracted out of `src/lisp/aot_runtime_bridge_closure.c3`, reducing it from 381 lines to 197 lines and isolating that runtime-dispatch lane in `src/lisp/aot_runtime_bridge_trampoline.c3`
  - disjunctive filter-expression head-demand collectors were extracted out of `src/lisp/deduce_schema_query_head_demand_build.c3`, reducing it from 380 lines to 62 lines and isolating that top disjunction lane in `src/lisp/deduce_schema_query_head_demand_disjunction.c3`
  - why-result row/frame/path builders and shared support-context helpers were extracted out of `src/lisp/deduce_why_result_payload.c3`, reducing it from 378 lines to 170 lines and isolating that path-payload lane in `src/lisp/deduce_why_result_path_payload.c3`
  - custom OS-thread job begin/execute helpers were extracted out of `src/lisp/scheduler_primitives_thread_spawn.c3`, reducing it from 375 lines to 229 lines and isolating that custom-thread lane in `src/lisp/scheduler_primitives_thread_custom.c3`
  - boundary graph reason/edge/result/logging helpers were extracted out of `src/lisp/eval_boundary_graph_audit_meta.c3`, reducing it from 375 lines to 115 lines and isolating that result-metadata lane in `src/lisp/eval_boundary_graph_audit_result.c3`
  - persisted rule-catalog record layout, entry storage, capacity growth, and record codec helpers were extracted out of `src/lisp/deduce_db_rule_catalog_persistence.c3`, reducing it from 375 lines to 190 lines and isolating that codec lane in `src/lisp/deduce_db_rule_catalog_record_codec.c3`
  - AOT type spec structs plus annotation/signature construction helpers were extracted out of `src/lisp/aot_type_definitions.c3`, reducing it from 375 lines to 172 lines and isolating that top helper lane in `src/lisp/aot_type_spec_helpers.c3`
  - console/input-state/rendering helpers were extracted out of `src/lisp/prim_io.c3`, reducing it from 374 lines to 45 lines and isolating that top helper lane in `src/lisp/prim_io_helpers.c3`
  - JSON pointer token/lookup and option-flag/bool coercion helpers were extracted out of `src/lisp/json_pointer_options.c3`, reducing it from 373 lines to 205 lines and isolating that top helper lane in `src/lisp/json_pointer_option_helpers.c3`
  - REPL output transport/JSON-doc/event emission helpers were extracted out of `src/lisp/eval_repl_server_output.c3`, reducing it from 371 lines to 136 lines and isolating that top event-output lane in `src/lisp/eval_repl_server_event_output.c3`
  - relation ownership plus `deduce-open` runtime helpers were extracted out of `src/lisp/deduce_runtime_primitives.c3`, reducing it from 371 lines to 152 lines and isolating that top open/runtime lane in `src/lisp/deduce_runtime_open.c3`
  - goal-directed read tracking and subject-key capture helpers were extracted out of `src/lisp/deduce_db_handles_mutation_tracking.c3`, reducing it from 370 lines to 154 lines and isolating that top bookkeeping lane in `src/lisp/deduce_db_goal_directed_read_tracking.c3`
  - boundary commit destination-promotion helpers were extracted out of `src/lisp/eval_boundary_commit_escape.c3`, reducing it from 369 lines to 250 lines and isolating that duplicated destination-build lane in `src/lisp/eval_boundary_commit_destination.c3`
  - macro expression conversion constructors and special-form handlers were extracted out of `src/lisp/macros_expr_conversion_value_to_expr.c3`, reducing it from 368 lines to 41 lines and isolating that top helper lane in `src/lisp/macros_expr_conversion_special_forms.c3`
  - gzip/deflate/zlib codec primitives were extracted out of `src/lisp/compress.c3`, reducing it from 365 lines to 93 lines and isolating that codec lane in `src/lisp/compress_codecs.c3`
  - libclang bind visitor state, C-type mapping, param shaping, and string helpers were extracted out of `src/lisp/libclang_bind_parse.c3`, reducing it from 362 lines to 74 lines and isolating that top parse-helper lane in `src/lisp/libclang_bind_parse_helpers.c3`
  - thread-task slot/generation/request/completion helpers were extracted out of `src/lisp/scheduler_thread_tasks.c3`, reducing it from 361 lines to 222 lines and isolating that top task-registry lane in `src/lisp/scheduler_thread_task_registry.c3`
  - dispatch/lambda mismatch payload builders and ambiguous-dispatch payload helpers were extracted out of `src/lisp/eval_dispatch_match_errors.c3`, reducing it from 361 lines to 129 lines and isolating that top payload lane in `src/lisp/eval_dispatch_error_payloads.c3`
  - seminaive recursive aggregate finalize/reset helpers were extracted out of `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregate_helpers.c3`, reducing it from 356 lines to 180 lines and isolating that top helper lane in `src/lisp/deduce_rule_eval_exec_seminaive_recursive_finalize.c3`
  - storage path / DBI-name / DBI-open helpers were extracted out of `src/lisp/deduce_db_handles_storage.c3`, reducing it from 355 lines to 180 lines and isolating that top storage-open lane in `src/lisp/deduce_db_storage_open.c3`
  - fiber join timeout, waiter-loop, and join-wait error helpers were extracted out of `src/lisp/scheduler_primitives_task_wait_join.c3`, reducing it from 354 lines to 197 lines and isolating that shared join-helper lane in `src/lisp/scheduler_task_join_helpers.c3`
  - core arithmetic, trig, rounding, gcd/lcm, and bitwise primitives were extracted out of `src/lisp/prim_math.c3`, reducing it from 353 lines to 30 lines and isolating that top core-math lane in `src/lisp/prim_math_core.c3`
  - runtime handle/interpreter attachment, exec-depth, and backend GC lifecycle helpers were extracted out of `src/lisp/runtime_backend_hooks.c3`, reducing it from 352 lines to 191 lines and isolating that top lifecycle lane in `src/lisp/runtime_backend_lifecycle.c3`
  - aggregate group storage/capacity and find-or-add helpers were extracted out of `src/lisp/deduce_rule_eval_exec_aggregate_state.c3`, reducing it from 352 lines to 143 lines and isolating that top aggregate-group lane in `src/lisp/deduce_rule_eval_exec_aggregate_groups.c3`
  - thread-task begin/complete/cancel transition helpers were extracted out of `src/lisp/scheduler_thread_task_transitions.c3`, reducing it from 350 lines to 154 lines and isolating that top thread-task transition lane in `src/lisp/scheduler_thread_task_transition_tasks.c3`
  - JIT apply helper, closure apply, method-table apply, and primitive apply helpers were extracted out of `src/lisp/jit_jit_apply_runtime.c3`, reducing it from 349 lines to 163 lines and isolating that top helper lane in `src/lisp/jit_jit_apply_helpers.c3`
  - macro binding lookup, pattern validation, emitted-form validation, and pattern-macro expansion helpers were extracted out of `src/lisp/macros_template_expansion.c3`, reducing it from 348 lines to 137 lines and isolating that top validation lane in `src/lisp/macros_template_validation.c3`
  - persisted rule-signature restore flow was extracted out of `src/lisp/deduce_db_rule_signature_persistence.c3`, reducing it from 346 lines to 106 lines and isolating that top restore lane in `src/lisp/deduce_db_rule_signature_restore.c3`
  - value predicates, core constructors, and list accessors were extracted out of `src/lisp/value_predicates_accessors_core.c3`, reducing it from 343 lines to 130 lines and isolating that top predicate/accessor lane in `src/lisp/value_predicates_accessors_basic.c3`
  - FTXUI node lowering helpers were extracted out of `src/lisp/prim_ui_ftxui.c3`, reducing it from 343 lines to 102 lines and isolating that top UI-lowering lane in `src/lisp/prim_ui_ftxui_lowering.c3`
  - TCP async bridge probe/setup/watcher helpers were extracted out of `src/lisp/scheduler_tcp_async_bridge.c3`, reducing it from 342 lines to 162 lines and isolating that top bridge-helper lane in `src/lisp/scheduler_tcp_async_bridge_helpers.c3`
  - scheduler fiber/pending/shared/offload/task support types were extracted out of `src/lisp/scheduler_state_types.c3`, reducing it from 342 lines to 52 lines and isolating that top support-type lane in `src/lisp/scheduler_state_support_types.c3`
  - scheduler pending IO/offload/DNS/connect/accept types were extracted out of `src/lisp/scheduler_state_support_types.c3`, reducing it from 294 lines to 218 lines and isolating that top pending-type lane in `src/lisp/scheduler_state_pending_types.c3`
  - async file runtime/offload/read helpers were extracted out of `src/lisp/prim_io_file.c3`, reducing it from 342 lines to 190 lines and isolating that top file-helper lane in `src/lisp/prim_io_file_helpers.c3`
  - JIT eval scope-state, recycle, and trace helpers were extracted out of `src/lisp/jit_jit_eval_scopes.c3`, reducing it from 341 lines to 261 lines and isolating that top scope-helper lane in `src/lisp/jit_jit_eval_scope_helpers.c3`
  - type-constructor validation and construction helpers were extracted out of `src/lisp/eval_type_evaluators.c3`, reducing it from 341 lines to 219 lines and isolating that top constructor lane in `src/lisp/eval_type_constructor.c3`
  - component delta-state mutation helpers were extracted out of `src/lisp/deduce_rule_eval_exec_component_state.c3`, reducing it from 340 lines to 247 lines and isolating that top delta-state lane in `src/lisp/deduce_rule_eval_exec_component_delta_state.c3`
  - pattern equality workspace helpers were extracted out of `src/lisp/eval_pattern_support_helpers.c3`, reducing it from 339 lines to 47 lines and isolating that top equality lane in `src/lisp/eval_pattern_equality.c3`
  - boundary commit/env-copy/copy fault metadata types and name tables were extracted out of `src/lisp/eval_boundary_api_meta.c3`, reducing it from 339 lines to 86 lines and isolating that top metadata lane in `src/lisp/eval_boundary_api_types.c3`
  - thread/os-thread waiter timeout, wake, and signal helpers were extracted out of `src/lisp/scheduler_thread_task_waiters.c3`, reducing it from 338 lines to 86 lines and isolating that top wait/signal lane in `src/lisp/scheduler_task_wait_signals.c3`
  - macro lookup/error/stamp/early call-site helpers were extracted out of `src/lisp/macros_expansion.c3`, reducing it from 337 lines to 140 lines and isolating that top helper lane in `src/lisp/macros_expansion_helpers.c3`
  - UDP socket/bind/send/recv/close primitives were extracted out of `src/lisp/async_udp_pipe.c3`, reducing it from 337 lines to 143 lines and isolating that top UDP lane in `src/lisp/async_udp_primitives.c3`
  - shared-handle registry lifecycle and slot-management helpers were extracted out of `src/lisp/scheduler_shared_handles_blob.c3`, reducing it from 335 lines to 133 lines and isolating that top registry lane in `src/lisp/scheduler_shared_registry.c3`
  - async runtime declarations, socket/uv constants, TCP/UDP handle structs, and shared IO helper primitives were extracted out of `src/lisp/async.c3`, reducing it from 335 lines to 78 lines and isolating that top runtime-base lane in `src/lisp/async_runtime_base.c3`
  - recursive scalar-parameter equality collection was extracted out of `src/lisp/deduce_schema_query_filter_equalities_helpers.c3`, reducing it from 333 lines to 80 lines and isolating that top recursive collector lane in `src/lisp/deduce_schema_query_filter_scalar_equalities.c3`
  - basic literal/variable/control-flow/application JIT expression compilers were extracted out of `src/lisp/jit_jit_compile_expr_core.c3`, reducing it from 332 lines to 201 lines and isolating that top basic-expression lane in `src/lisp/jit_jit_compile_expr_basic.c3`
  - boundary scope/env facade helpers were extracted out of `src/lisp/eval_boundary_api.c3`, reducing it from 331 lines to 119 lines and isolating that top scope/env lane in `src/lisp/eval_boundary_scope_env.c3`
  - value-to-expr helper/builders were extracted out of `src/lisp/macros_expr_conversion_special_forms.c3`, reducing it from 330 lines to 67 lines and isolating that top form-builder lane in `src/lisp/macros_expr_conversion_form_builders.c3`
  - HTTP socket/send and response-buffer helpers were extracted out of `src/lisp/scheduler_offload_network.c3`, reducing it from 329 lines to 232 lines and isolating that top HTTP helper lane in `src/lisp/scheduler_offload_http_helpers.c3`
  - selected-component schema snapshot/dirty-target planning helpers were extracted out of `src/lisp/deduce_rule_eval_fixpoint_goal_directed_read.c3`, reducing it from 329 lines to 147 lines and isolating that top prepare lane in `src/lisp/deduce_rule_eval_fixpoint_goal_directed_prepare.c3`
  - execution-state structs plus head-demand/join-input helpers were extracted out of `src/lisp/deduce_rule_eval_exec_helpers.c3`, reducing it from 329 lines to 200 lines and isolating that top execution-state lane in `src/lisp/deduce_rule_eval_exec_state.c3`
  - console output/render helpers were extracted out of `src/lisp/prim_io_helpers.c3`, reducing it from 326 lines to 199 lines and isolating that top console lane in `src/lisp/prim_io_console_helpers.c3`
  - declarative FFI bound-call helpers were extracted out of `src/lisp/eval_ffi_eval.c3`, reducing it from 325 lines to 157 lines and isolating that top bound-call lane in `src/lisp/eval_ffi_bound_call.c3`
  - tuple comparison and bound-tuple parsing helpers were extracted out of `src/lisp/deduce_relation_scan_helpers_more.c3`, reducing it from 325 lines to 222 lines and isolating that top tuple/bounds lane in `src/lisp/deduce_relation_scan_tuple_helpers.c3`
  - destination cons/partial/iterator escape builders were extracted out of `src/lisp/eval_boundary_commit_escape_builders.c3`, reducing it from 324 lines to 52 lines and isolating that top wrapper-builder lane in `src/lisp/eval_boundary_commit_escape_wrappers.c3`
  - recursive pattern-check emission was extracted out of `src/lisp/compiler_native_match_compilation_flat_style.c3`, reducing it from 324 lines to 93 lines and isolating that top recursive pattern-check lane in `src/lisp/compiler_native_match_pattern_checks.c3`
  - single-expression filter head-demand construction was extracted out of `src/lisp/deduce_schema_query_head_demand_disjunction.c3`, reducing it from 321 lines to 259 lines and isolating that top filter-demand lane in `src/lisp/deduce_schema_query_head_demand_filter.c3`
  - relation schema storage/default/inferred-column helpers were extracted out of `src/lisp/deduce_db_relation_schema_init.c3`, reducing it from 320 lines to 188 lines and isolating that top schema-helper lane in `src/lisp/deduce_db_relation_schema_helpers.c3`
  - scope-chain provenance and target-scope lookup helpers were extracted out of `src/lisp/eval_boundary_provenance.c3`, reducing it from 319 lines to 201 lines and isolating that top provenance lane in `src/lisp/eval_boundary_scope_chain.c3`
  - deduce handle helpers and DB finalizer lifecycle were extracted out of `src/lisp/deduce_db_handles.c3`, reducing it from 317 lines to 83 lines and isolating that top lifecycle lane in `src/lisp/deduce_db_handles_lifecycle.c3`
  - TOML parser helpers, section/value assignment, and string-array parsing were extracted out of `src/lisp/toml.c3`, reducing it from 316 lines to 60 lines and isolating that top parser lane in `src/lisp/toml_parser.c3`
  - continuation validation, resume, fiber-yield propagation, and continuation-apply helpers were extracted out of `src/lisp/jit_jit_reset_shift.c3`, reducing it from 316 lines to 147 lines and isolating that top continuation-runtime lane in `src/lisp/jit_jit_continuation_runtime.c3`
  - FTXUI ABI aliases and enum-style constant tables were extracted out of `src/lisp/ftxui_ffi_types.c3`, reducing it from 315 lines to 131 lines and isolating that top constant surface in `src/lisp/ftxui_ffi_constants.c3`
  - primitive registration table helpers, language constants, and dispatched primitive bootstrap were extracted out of `src/lisp/eval_init_primitive_tables.c3`, reducing it from 313 lines to 241 lines and isolating that top registration lane in `src/lisp/eval_init_primitive_registration.c3`
  - boundary transaction/session state structs, swap helpers, state-name tables, and transition-validation helpers were extracted out of `src/lisp/eval_boundary_session_txn.c3`, reducing it from 311 lines to 128 lines and isolating that top state-model lane in `src/lisp/eval_boundary_session_txn_state.c3`
  - recursive naive rule-step evaluation was extracted out of `src/lisp/deduce_rule_eval_exec_naive.c3`, reducing it from 311 lines to 165 lines and isolating that top rule-step lane in `src/lisp/deduce_rule_eval_exec_naive_step.c3`
  - env storage structs, promotion-context caches, and hash-table helpers were extracted out of `src/lisp/value_environment.c3`, reducing it from 310 lines to 104 lines and isolating that top env-storage lane in `src/lisp/value_environment_storage.c3`
  - async request token, callback-context ownership, and pending-operation lookup helpers were extracted out of `src/lisp/scheduler_wakeup_io_lifecycle.c3`, reducing it from 310 lines to 232 lines and isolating that top pending-lookup lane in `src/lisp/scheduler_wakeup_io_pending.c3`
  - numeric conversion and arithmetic primitives were extracted out of `src/lisp/prim_math_core.c3`, reducing it from 309 lines to 217 lines and isolating that top arithmetic lane in `src/lisp/prim_math_arithmetic.c3`
  - TOML parse bridge, datum conversion, and timestamp formatting helpers were extracted out of `src/lisp/primitives_data_formats.c3`, reducing it from 307 lines to 98 lines and isolating that top TOML lane in `src/lisp/primitives_toml_bridge.c3`
  - async `tcp-connect` fiber/libuv connect helpers were extracted out of `src/lisp/async_tcp_transport_listen.c3`, reducing it from 307 lines to 181 lines and isolating that top connect lane in `src/lisp/async_tcp_transport_connect.c3`
  - process wait lifecycle helpers and offload callback state were extracted out of `src/lisp/async_process_signal_dns_process.c3`, reducing it from 302 lines to 262 lines and isolating that top process-lifecycle lane in `src/lisp/async_process_lifecycle.c3`
  - task-handle lifecycle and parser helpers were extracted out of `src/lisp/scheduler_primitives_task_spawn.c3`, reducing it from 301 lines to 222 lines and isolating that top task-handle lane in `src/lisp/scheduler_primitives_task_handles.c3`
  - boundary policy env parsing, validation, and load-from-env helpers were extracted out of `src/lisp/eval_boundary_policy.c3`, reducing it from 301 lines to 86 lines and isolating that top parse/load lane in `src/lisp/eval_boundary_policy_parse.c3`
  - single-pass seminaive worker-scratch payload computation was extracted out of `src/lisp/deduce_rule_eval_exec_seminaive_worker_scratch.c3`, reducing it from 301 lines to 165 lines and isolating that top pass lane in `src/lisp/deduce_rule_eval_exec_seminaive_worker_pass.c3`
  - expression-node AST structs were extracted out of `src/lisp/value_ast_effects.c3`, reducing it from 300 lines to 190 lines and isolating that top AST-node lane in `src/lisp/value_ast_expr_nodes.c3`
  - REPL syntax-highlighting constants, state, and callback helpers were extracted out of `src/lisp/eval_repl_callbacks.c3`, reducing it from 300 lines to 86 lines and isolating that top highlighter lane in `src/lisp/eval_repl_highlighter.c3`
  - boundary graph value-walk traversal was extracted out of `src/lisp/eval_boundary_graph_audit_walkers.c3`, reducing it from 300 lines to 64 lines and isolating that top value-walk lane in `src/lisp/eval_boundary_graph_audit_value_walk.c3`
  - JIT module/import setup helpers were extracted out of `src/lisp/jit_jit_module_import_setup.c3`, reducing it from 299 lines to 109 lines and isolating that top setup lane in `src/lisp/jit_jit_module_setup_helpers.c3`
  - projected head-demand single-bound and recursive fallback/relaxation helpers were extracted out of `src/lisp/deduce_schema_query_head_demand.c3`, reducing it from 299 lines to 108 lines and isolating that top projected-demand lane in `src/lisp/deduce_schema_query_head_demand_projected.c3`
  - SCC plan graph-build, Tarjan walk, recursive-component detection, and rule-component assignment helpers were extracted out of `src/lisp/deduce_rule_eval_scc_plan.c3`, reducing it from 299 lines to 197 lines and isolating that top component-build lane in `src/lisp/deduce_rule_eval_scc_plan_components.c3`
  - JSON REPL request parse structs and helpers were extracted out of `src/lisp/eval_repl_json.c3`, reducing it from 297 lines to 172 lines and isolating that top request-parse lane in `src/lisp/eval_repl_json_request.c3`
  - row-column reference extraction helpers were extracted out of `src/lisp/deduce_schema_query_analysis.c3`, reducing it from 296 lines to 120 lines and isolating that top row-ref lane in `src/lisp/deduce_schema_query_row_ref.c3`
  - AOT match/pattern/sequence bridge helpers were extracted out of `src/lisp/aot_runtime_bridge_helpers.c3`, reducing it from 296 lines to 156 lines and isolating that top runtime-match lane in `src/lisp/aot_runtime_match_helpers.c3`
  - value printing helpers and type-descriptor lookup were extracted out of `src/lisp/value_print.c3`, reducing it from 293 lines to 81 lines and isolating that top print-helper lane in `src/lisp/value_print_helpers.c3`
  - scheduler offload/domain/error/work-build helpers were extracted out of `src/lisp/scheduler_state_offload.c3`, reducing it from 291 lines to 67 lines and isolating that top offload-helper lane in `src/lisp/scheduler_state_offload_helpers.c3`
  - ctor type-form parsing, mismatch payload, and recursive match helpers were extracted out of `src/lisp/primitives_meta_types_ctor.c3`, reducing it from 291 lines to 81 lines and isolating that top ctor-helper lane in `src/lisp/primitives_meta_types_ctor_helpers.c3`
  - rule-signature capacity growth and allocation-release helpers were extracted out of `src/lisp/deduce_db_handles_mutation.c3`, reducing it from 291 lines to 198 lines and isolating that top rule-signature helper lane in `src/lisp/deduce_db_rule_signature_helpers.c3`
  - libclang C-type mapping, filter matching, parameter shaping, and parameter-capacity helpers were extracted out of `src/lisp/libclang_bind_parse_helpers.c3`, reducing it from 289 lines to 141 lines and isolating that top visitor-helper lane in `src/lisp/libclang_bind_visitor_helpers.c3`
  - pattern-equality workspace stats, pair-capacity growth, and recursive cons/array pair-push helpers were extracted out of `src/lisp/eval_pattern_equality.c3`, reducing it from 289 lines to 140 lines and isolating that top equality-helper lane in `src/lisp/eval_pattern_equality_helpers.c3`
  - value lifecycle/destructor helpers were extracted out of `src/lisp/value_constructors_core.c3`, reducing it from 288 lines to 186 lines and isolating that top lifecycle lane in `src/lisp/value_constructors_lifecycle.c3`
  - aggregate tuple-build/materialize/count helpers were extracted out of `src/lisp/deduce_rule_eval_exec_aggregate_materialization.c3`, reducing it from 288 lines to 70 lines and isolating that top aggregate-helper lane in `src/lisp/deduce_rule_eval_exec_aggregate_helpers.c3`
  - `fact!` mutation surface and post-commit cleanup helpers were extracted out of `src/lisp/deduce_relation_ops_fact_mutations.c3`, reducing it from 287 lines to 120 lines and isolating that top assert-fact lane in `src/lisp/deduce_relation_ops_assert_fact.c3`
  - let/set mutation-detection, env-load, local-slot, and set-assignment helpers were extracted out of `src/lisp/jit_jit_compile_let_set.c3`, reducing it from 284 lines to 122 lines and isolating that top let/set helper lane in `src/lisp/jit_jit_compile_let_set_helpers.c3`
  - recursive let/set runtime bridge helpers were extracted out of `src/lisp/jit_jit_closure_define_qq.c3`, reducing it from 283 lines to 27 lines and isolating that top let/set runtime lane in `src/lisp/jit_jit_closure_let_set_helpers.c3`
  - rule variable/allocation, atom lifecycle, and atom-parse helpers were extracted out of `src/lisp/deduce_rule_ir.c3`, reducing it from 282 lines to 101 lines and isolating that top IR-helper lane in `src/lisp/deduce_rule_ir_helpers.c3`
  - scope-chain and binding-copy predicate helpers were extracted out of `src/lisp/jit_jit_eval_scopes_helpers.c3`, reducing it from 280 lines to 86 lines and isolating that top scope-chain lane in `src/lisp/jit_jit_eval_scope_chain_helpers.c3`
  - TCO env-frame copy and scope-site value-copy helpers were extracted out of `src/lisp/jit_jit_eval_scopes.c3`, reducing it from 261 lines to 186 lines and isolating that top scope-copy lane in `src/lisp/jit_jit_eval_scope_copy.c3`
  - utf8proc declarations, unicode case-mapping constants, and shared case-transform helpers were extracted out of `src/lisp/unicode_case_mapping.c3`, reducing it from 276 lines to 226 lines and isolating that top utf8proc-helper lane in `src/lisp/unicode_case_utf8proc.c3`
  - gzip and gunzip codec primitives were extracted out of `src/lisp/compress_codecs.c3`, reducing it from 276 lines to 189 lines and isolating that top gzip lane in `src/lisp/compress_gzip.c3`
  - runtime fault definitions, tag/id types, and capacity constants were extracted out of `src/lisp/value_runtime_types.c3`, reducing it from 275 lines to 179 lines and isolating that top constants/type-tag lane in `src/lisp/value_runtime_constants.c3`
  - expression tag and concrete `Expr*` node structs were extracted out of `src/lisp/value_expr_ast_core.c3`, reducing it from 274 lines to 119 lines and isolating that top expression-node lane in `src/lisp/value_expr_nodes.c3`
  - schema tag-set construction, equality predicates, and the full schema-check engine were extracted out of `src/lisp/schema_validation.c3`, reducing it from 274 lines to 62 lines and isolating that top validation-helper lane in `src/lisp/schema_validation_helpers.c3`
  - destination cons escape rebuilding was extracted out of `src/lisp/eval_boundary_commit_escape_wrappers.c3`, reducing it from 274 lines to 181 lines and isolating that top cons-wrapper lane in `src/lisp/eval_boundary_commit_escape_cons.c3`
  - basic rule arity/head-safety/negation-safety validation was extracted out of `src/lisp/deduce_rule_eval_validation.c3`, reducing it from 274 lines to 179 lines and isolating that top basic-validation lane in `src/lisp/deduce_rule_eval_validation_basic.c3`
  - index nested-loop join counting was extracted out of `src/lisp/deduce_relation_scan_helpers_join.c3`, reducing it from 272 lines to 190 lines and isolating that top index-join lane in `src/lisp/deduce_relation_scan_join_index.c3`
  - custom offload task admission/join helpers were extracted out of `src/lisp/scheduler_primitives_offload_jobs.c3`, reducing it from 271 lines to 109 lines and isolating that top custom-offload lane in `src/lisp/scheduler_primitives_offload_custom.c3`
  - env frame allocation, binding copy, and rollback helpers were extracted out of `src/lisp/eval_env_copy_helpers.c3`, reducing it from 271 lines to 118 lines and isolating that top frame-helper lane in `src/lisp/eval_env_copy_frame_helpers.c3`
  - TCP/UDP handle finalizers plus handle construction/getter helpers were extracted out of `src/lisp/async_process_signal_runtime.c3`, reducing it from 271 lines to 199 lines and isolating that top socket-handle lane in `src/lisp/async_socket_handle_runtime.c3`
  - primitive/array/hashmap root-store cloning helpers were extracted out of `src/lisp/eval_promotion_root_clones.c3`, reducing it from 270 lines to 99 lines and isolating that top basic-clone lane in `src/lisp/eval_promotion_root_clone_basic.c3`
  - TLS connect request/error/connect helpers were extracted out of `src/lisp/tls_offload_boundary.c3`, reducing it from 269 lines to 114 lines and isolating that top TLS-connect lane in `src/lisp/tls_offload_connect.c3`
  - fs-handle lifecycle plus `fs-open`/`fs-read`/`fs-write`/`fs-close` primitives were extracted out of `src/lisp/prim_io_fs_handles.c3`, reducing it from 268 lines to 112 lines and isolating that top fs-stream lane in `src/lisp/prim_io_fs_stream.c3`
  - selector-specific ephemeral goal-directed match execution was extracted out of `src/lisp/unify_goal_directed.c3`, reducing it from 266 lines to 121 lines and isolating that top selector lane in `src/lisp/unify_goal_directed_selector.c3`
  - low-level value-to-expr constructors plus simple special-form builders were extracted out of `src/lisp/macros_expr_conversion_form_builders.c3`, reducing it from 266 lines to 165 lines and isolating that top basic-form lane in `src/lisp/macros_expr_conversion_basic_forms.c3`
  - goal-directed component-summary helpers were extracted out of `src/lisp/deduce_rule_ops_explain_goal_directed.c3`, reducing it from 266 lines to 175 lines and isolating that top component-summary lane in `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3`
  - client-side TLS connect plus trust-store cause helpers were extracted out of `src/lisp/tls_connection_primitives.c3`, reducing it from 263 lines to 99 lines and isolating that top TLS-connect lane in `src/lisp/tls_connect_primitives.c3`
  - worker busy/match/submit/interrupt/stdin helpers were extracted out of `src/lisp/eval_repl_server_worker.c3`, reducing it from 263 lines to 150 lines and isolating that top worker-helper lane in `src/lisp/eval_repl_server_worker_helpers.c3`
  - process spawn argument marshaling and handle/result construction were extracted out of `src/lisp/async_process_signal_dns_process.c3`, reducing it from 262 lines to 92 lines and isolating that top process-spawn lane in `src/lisp/async_process_spawn.c3`
  Current largest residuals:
  - `src/lisp/eval_repl_server_protocol.c3` (261 lines)
  - `src/lisp/eval_boundary_graph_audit_result.c3` (261 lines)
  - `src/lisp/deduce_rule_eval_exec_component_delta_payload.c3` (260 lines)
  - `src/lisp/primitives_data_formats_csv_options.c3` (259 lines)
  - `src/lisp/deduce_schema_query_head_demand_disjunction.c3` (259 lines)
  - `src/lisp/schema_explain_effect_runtime.c3` (258 lines)
  These should be split only where ownership boundaries are already real; do not introduce abstraction layers that blur runtime state or boundary semantics.
  Next step: continue with `src/lisp/eval_repl_server_protocol.c3`, then `src/lisp/eval_boundary_graph_audit_result.c3`, then `src/lisp/deduce_rule_eval_exec_component_delta_payload.c3`.
  Why deferred: this is now the largest-first physical decomposition queue across the runtime.

- META-STATS-001 remove duplicate SCC-plan and parallel-batch metadata construction from `deduce/stats`
  Audit finding: `deduce_stats_set_parallel_topology_fields(...)` builds an SCC plan and parallel batch metadata locally, then immediately calls `deduce_parallel_batch_topology_counts(...)`, which rebuilds the SCC plan and the same batch metadata a second time.
  - first build in `deduce_stats_set_parallel_topology_fields(...)` at `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
  - second build inside `deduce_parallel_batch_topology_counts(...)` at `src/lisp/deduce_schema_query_metadata_parallel_topology.c3`
  This is a code-smell/perf issue, not a contract change, but it means the `deduce/stats` metadata path still does duplicate planner work after the recent structural split.
  Why deferred: this audit pass does not apply runtime fixes.
  Next step: refactor `deduce_parallel_batch_topology_counts(...)` to accept already-built topology metadata, or fold the counting logic into the existing `deduce_stats_set_parallel_topology_fields(...)` planning scope so the SCC plan and batch metadata are built once per request.

- QUERY-GD-TXN-001 remove duplicated selector goal-directed transaction scaffolding across query and scan-range paths
  Audit finding: the selector-single transactional helper in `src/lisp/deduce_schema_query_execution_goal_directed_single_selector_txn.c3` duplicates the same selected-component snapshot, observed-rule allocation, transaction-open, fixpoint-evaluate, subject-key capture, and goal-directed read bookkeeping shape already present in `src/lisp/deduce_relation_ops_query_scan_range_goal_directed_selector_txn.c3`.
  - selector-query transaction path at `src/lisp/deduce_schema_query_execution_goal_directed_single_selector_txn.c3`
  - selector scan-range transaction path at `src/lisp/deduce_relation_ops_query_scan_range_goal_directed_selector_txn.c3`
  This is a code-smell/maintenance issue rather than a behavioral regression, but future changes to selector-based goal-directed execution now have two near-identical runtime scaffolds to keep in sync.
  Why deferred: this audit pass does not apply runtime fixes.
  Next step: extract a shared selector goal-directed transaction scaffold that owns snapshot/observed-rule/transaction/fixpoint setup while keeping query-vs-scan-range row materialization and read-note surfaces separate.

- QUERY-GD-PLAIN-001 remove duplicated plain goal-directed transaction scaffolding across query and scan-range paths
  Audit finding: the non-selector plain goal-directed helper in `src/lisp/deduce_schema_query_execution_goal_directed_single.c3` duplicates the same selected-component snapshot, transaction-open, fixpoint-execute, subject-key capture, and goal-directed read bookkeeping shape already present in `src/lisp/deduce_relation_ops_query_scan_range_goal_directed_plain_txn.c3`.
  - plain query path at `src/lisp/deduce_schema_query_execution_goal_directed_single.c3`
  - plain scan-range transaction path at `src/lisp/deduce_relation_ops_query_scan_range_goal_directed_plain_txn.c3`
  This is a code-smell/maintenance issue rather than a behavioral regression, but future plain goal-directed fixes now need to land in two near-identical runtime paths.
  Why deferred: this audit pass does not apply runtime fixes.
  Next step: extract a shared plain goal-directed transaction scaffold that owns selected-schema snapshot, transaction/fixpoint setup, and subject-key bookkeeping while keeping query-vs-scan-range plan preparation and row materialization separate.

- OFFLOAD-TASK-001 remove duplicated offload task-handle admission/join scaffolding across custom-job and work-job paths
  Audit finding: the latest offload split left two near-identical task orchestration lanes.
  - custom task admission/join flow lives in `src/lisp/scheduler_primitives_offload_jobs.c3`
  - generic work-job admission/join flow is split across `src/lisp/scheduler_primitives_offload_jobs.c3` and `src/lisp/scheduler_primitives_offload_execute.c3`
  Both paths allocate task ids, fetch generations, enqueue work, populate `SchedulerTaskHandle`, batch-begin with rollback, and join with the same trailing cleanup pattern. This is a maintainability/code-smell issue rather than a behavior regression, but future task-handle fixes now need to land in both custom and generic offload lanes.
  Why deferred: this audit pass does not apply runtime refactors.
  Next step: extract a shared offload task-handle admission/join scaffold that owns task allocation, generation capture, enqueue, batch rollback, and join cleanup while keeping custom callback setup separate from parsed `OffloadWork` setup.

- THREAD-SPAWN-001 remove duplicated OS-thread admission/join scaffolding across custom-thread and work-job paths
  Audit finding: the latest thread spawn split left two near-identical OS-thread orchestration lanes.
  - custom OS-thread admission/join flow lives in `src/lisp/scheduler_primitives_thread_custom.c3`
  - generic work-job admission/join flow lives in `src/lisp/scheduler_primitives_thread_spawn.c3`
  Both paths allocate thread ids, fetch generations, start OS threads, populate `SchedulerOsThreadHandle`, batch-begin with rollback, and join with the same trailing cleanup pattern. This is a maintainability/code-smell issue rather than a behavior regression, but future OS-thread-handle fixes now need to land in both custom and generic thread lanes.
  Why deferred: this audit pass does not apply runtime refactors.
  Next step: extract a shared OS-thread admission/join scaffold that owns thread allocation, generation capture, OS-thread start, batch rollback, and join cleanup while keeping custom callback setup separate from prepared `OffloadWork` setup.

- THREAD-TRANSITIONS-001 remove duplicated completion/cancel transition scaffolding across thread-task and OS-thread runtime lanes
  Audit finding: the recent transition split exposed two near-identical runtime transition surfaces.
  - thread-task transition flow now lives in `src/lisp/scheduler_thread_task_transition_tasks.c3`
  - OS-thread transition flow lives in `src/lisp/scheduler_thread_task_transitions.c3`
  Both paths allocate cancel/alloc-failure completions, gate begin on generation/state, convert pending/running work into cancelled state, clear waiter ids/epochs, publish completions, and signal waiter fibers with the same fail-closed shape. This is a maintainability/code-smell issue rather than a behavior regression, but future transition fixes now need to land in both task and OS-thread lanes.
  Why deferred: this audit pass does not apply runtime refactors.
  Next step: extract a shared transition scaffold that owns cancel-completion materialization, waiter-state clearing, fail-closed cancellation publication, and completion handoff while keeping task-specific request-handle cleanup separate from OS-thread-specific `thread_created` handling.

- VCS-STATE-001 repair the broken local `jj` checkout state so the repo can use its primary version-control workflow again
  Audit finding: `jj status` and `jj bookmark list` both still fail in this workspace with `Failed to read checkout state` / `No such file or directory (os error 2)`, so the requested bookmark workflow cannot run.
  This is an operational blocker rather than a runtime contract issue, but it now repeatedly prevents the repo’s required `jj`-first workflow from being used for ordinary status/bookmark operations.
  Why deferred: this audit pass does not apply repo-state repairs.
  Next step: inspect and repair the local `.jj` checkout metadata non-destructively, confirm `jj status` and `jj bookmark list` both return cleanly again, and only then resume the normal `jj` workflow.

- ERR-FMT-REPL-UI-001 remove remaining assert-style fixed-buffer formatting from current REPL/UI runtime residuals
  Audit refresh: after the recent REPL/UI splits, the live residual has narrowed to session id construction in `src/lisp/eval_repl_server_state.c3`, which does a checked `io::bprintf(...)` and then immediately repeats the same format with unconditional `io::bprintf(... )!!`.
  This is now a single-site hardening/code-smell item rather than the older broad helper-layer issue, but it still suppresses the repo’s fail-closed error-construction policy in the REPL runtime.
  Why deferred: this audit pass does not apply runtime fixes.
  Next step: remove the duplicate formatting call and carry the checked `bprintf(...)` result through session id allocation, with a stable null/failure fallback instead of the current unconditional `!!` path.

- REF-SPLIT-002 split oversized test files physically by feature/group ownership before adding more cases
  Several test files are now large enough to obscure coverage ownership and make filtered-lane maintenance harder:
  - `src/lisp/tests_deduce_query_admin_surface_tail.c3` (2598 lines)
  - `src/lisp/tests_deduce_groups.c3` (2572 lines)
  - `src/lisp/tests_deduce_query_admin_groups.c3` (2038 lines)
  - `src/lisp/tests_deduce_query_admin_surface_demand_tests.c3` (1907 lines)
  - `src/lisp/tests_deduce_rule_groups_more_tail.c3` (1787 lines)
  - `src/lisp/tests_deduce_query_bench_groups.c3` (1684 lines)
  - `src/lisp/tests_deduce_rule_groups_explain.c3` (1471 lines)
  - `src/lisp/tests_deduce_durability_groups.c3` (1403 lines)
  - `src/lisp/tests_compiler_core_groups.c3` (1266 lines)
  - `src/lisp/tests_deduce_query_admin_surface_demand_tail.c3` (1062 lines)
  - `src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3` (1059 lines)
  - `src/lisp/tests_scheduler_groups_more.c3` (958 lines)
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3` (835 lines)
  - `src/lisp/tests_advanced_io_effect_ffi_groups.c3` (748 lines)
  - `src/lisp/tests_advanced_stdlib_numeric_groups.c3` (720 lines)
  - `src/lisp/tests_memory_lifetime_finalize_groups.c3` (674 lines)
  - `src/lisp/tests_deduce_rule_groups.c3` (590 lines)
  - `src/lisp/tests_compiler_codegen_groups_tail.c3` (585 lines)
  - `src/lisp/tests_deduce_query_admin_surface_demand_wrapper_tests.c3` (566 lines)
  - `src/lisp/tests_deduce_query_recursive_aggregate_groups.c3` (554 lines)
  - `src/lisp/tests_deduce_query_groups.c3` (526 lines)
  - `src/lisp/tests_deduce_rule_groups_more.c3` (520 lines)
  - `src/lisp/tests_tests.c3` (507 lines)
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` (484 lines)
  - `src/lisp/tests_advanced_core_unicode_groups.c3` (476 lines)
  - `src/lisp/tests_advanced_type_parametric_groups.c3` (471 lines)
  - `src/lisp/tests_harness_helpers.c3` (468 lines)
  - `src/lisp/tests_compiler_codegen_groups.c3` (456 lines)
  - `src/lisp/tests_memory_lifetime_boundary_graph_txn_groups.c3` (449 lines)
  - `src/lisp/tests_core_groups.c3` (447 lines)
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3` (446 lines)
  - `src/lisp/tests_memory_lifetime_boundary_decision_bench_groups.c3` (434 lines)
  - `src/lisp/tests_scheduler_io_task_groups.c3` (428 lines)
  - `src/lisp/tests_scheduler_boundary_offload_payload_groups.c3` (424 lines)
  - `src/lisp/tests_runtime_feature_http_groups.c3` (418 lines)
  The repo already uses filtered groups; the remaining cleanup is physical decomposition, not behavioral redesign.
  Why deferred: runtime large-file and code-smell follow-ups remain higher priority than test-only file organization.
  Next step: split the largest test files first along existing filtered-group / feature-lane seams, starting with `src/lisp/tests_deduce_query_admin_surface_tail.c3`, then `src/lisp/tests_deduce_groups.c3`.
