# Re-Audit Findings - 2026-04-23

Scope: compiler/parser/macro walkers, tensor/math/matrix/reduction runtime, repo bookkeeping/validation, and the live memory-model runtime files.

## Findings

1. **Medium** `src/lisp/prim_tensor_matrix.c3:141-173` plus Vulkan backend helpers `csrc/tensor_vulkan_helpers_matrix_ops_factorization_f64_primary.c:56-124` and `csrc/tensor_vulkan_helpers_matrix_ops_factorization_f64_secondary.c:56-124,386-525`
   - CPU `matrix/solve` treats zero-size systems and zero-column RHS as a solved empty result, but the Vulkan solve backend returns `OMNI_TENSOR_VULKAN_SINGULAR` for the same inputs.
   - This is a backend correctness mismatch for an otherwise valid empty solve case.

2. **Medium** `src/lisp/compiler_mutable_capture_detection_walk.c3:73-93` and `src/lisp/compiler_free_vars_scope_forms.c3:44-57`
   - Nested-lambda capture detection only seeds `bound` with `expr.lambda.param`, but the AST and free-variable pass support multi-parameter lambdas and rest params.
   - A shadowed later parameter can therefore be misclassified as an outer mutable capture, changing boxing/codegen behavior.
   - No regression test covers the shadowed multi-arg `set!` case.

3. **Medium** `src/lisp/compiler_mutable_capture_detection_walk.c3:38-69,126-153` and `src/lisp/compiler_lambda_scan.c3:83-94,143-144`
   - The mutable-capture walk skips `E_DEFINE` entirely and only scans `E_MATCH` clause results, while the lambda/free-var passes recurse into `define` initializers and match guard predicates.
   - A `set!` or nested lambda hidden in a local `define` initializer or a guarded pattern predicate will not mark the binding mutable-captured, which can leave it on the wrong compilation path.
   - No targeted test covers either subtree.

4. **Medium** `docs/plans/README.md:27-31`, `TODO.md:25`, `.agents/SESSION_REPORT.md:50`, `scripts/check_status_consistency.sh:103-132`
   - `docs/plans/README.md` still lists `tagged-switch-exhaustiveness-remediation-plan-2026-04-23.md` as live even though the plan is completed.
   - `TODO.md` and `.agents/SESSION_REPORT.md` have stale part-file line counts.
   - The status script still reports green because it does not validate plan-index synchronization or part-file line counts.

## Memory Model

- No concrete, high-confidence correctness issue was found in the live memory-model files that were re-audited:
  - `src/scope_region.c3`
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3`
  - `src/lisp/eval_run_pipeline.c3`
  - `src/lisp/eval_boundary_api_types.c3`
  - `src/lisp/eval_boundary_provenance*.c3`
  - `src/lisp/prim_tensor_capture.c3`
  - `src/lisp/prim_runtime_memory_stats.c3`

## Next Steps

- Fix the backend mismatch in `matrix/solve` for empty systems.
- Extend mutable-capture detection to cover multi-parameter/rest-param lambdas, `define` initializers, and match guard predicates.
- Repair the stale plan/TODO/session-report indexes and teach the status checker to validate them.
