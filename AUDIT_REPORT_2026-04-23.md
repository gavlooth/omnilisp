# Audit Report - 2026-04-23

Scope: broad regression, edge-case, bug, and antipattern audit across the current Omni Lisp codebase, with emphasis on compiler pattern handling and serializer fidelity.

## Findings

1. `src/lisp/compiler_expr_serialize_patterns.c3:8-75`
   - Guarded patterns (`PAT_GUARD`) are not serialized explicitly.
   - The serializer falls back to `_`, which drops the guard predicate and any nested subpattern semantics.
   - Impact: round-trip fidelity is broken for guarded `match` clauses, and any text-based debug output for those patterns is misleading.

2. `src/lisp/tests_advanced_core_semantics_groups.c3:27-45`, `src/lisp/tests_compiler_codegen_groups.c3:103-129`, `src/lisp/tests_compiler_core_groups.c3:18-56`
   - The current regression surface covers basic guard behavior, but not the nested guarded-subpattern scope rule or guarded-pattern serialization round-trips.
   - Impact: the exact edge touched by the recent compiler pattern work is not pinned by a direct regression test, so a future cleanup can silently reintroduce the bug.

## Validation

- `c3c build --obj-out obj`
- `git diff --check`
- `scripts/check_status_consistency.sh`

## Recommendation

Implement an explicit `PAT_GUARD` branch in `serialize_pattern_to_buf`, then add a guarded-pattern round-trip test and one nested guard-sub scope regression.

Signature: GPT-5 Codex



---

## Commit 9585f1f Review Findings

• I reviewed commit 9585f1f2e439839d209846c6278dacb997ddd5ee (Fix 9 critical findings from deep reaudit). Findings:

  - Medium src/lisp/prim_tensor_matrix.c3:141-173, csrc/tensor_vulkan_helpers_matrix_ops_factorization_f64_primary.c:56-124, csrc/tensor_vulkan_helpers_matrix_ops_factorization_f64_secondary.c:56-124,386-525
    matrix/solve accepts zero-size systems and zero-column RHS as a solved empty result on CPU, but the Vulkan solve path still hard-fails them as OMNI_TENSOR_VULKAN_SINGULAR. That is a real backend behavior mismatch for a valid
    empty-solve case.
    **Status: ALREADY FIXED in commit 362eba7. The Vulkan factorization backends now return SUCCESS for n == 0 || rhs_cols == 0.**

  - Medium src/lisp/compiler_mutable_capture_detection_walk.c3:73-93 and src/lisp/compiler_free_vars_scope_forms.c3:44-57
    Mutable-capture detection only seeds bound with expr.lambda.param, but the AST/free-var path supports multi-parameter lambdas and rest params. A later shadowed parameter can be misclassified as an outer mutable capture, which
    changes boxing/codegen behavior.
    **Status: FIXED in commit 94036be. rest_param now added to bound vars. Also added parameter shadowing check in has_set_on.**

  - Medium src/lisp/compiler_mutable_capture_detection_walk.c3:38-69,126-153 and src/lisp/compiler_lambda_scan.c3:83-94,143-144
    The mutable-capture walk skips E_DEFINE and only scans E_MATCH clause results, while the lambda/free-var passes recurse into define initializers and match guards. A set! or nested lambda hidden there will not mark the binding
    mutable-captured, so the binding can take the wrong compilation path.
    **Status: PARTIALLY FIXED in commit 94036be. E_DEFINE was already handled. Match guard prescan and free-var guard scan added. Guard scope (guard_sub bindings visible in guard_pred) fixed in commit ad88640.**

  - Medium docs/plans/README.md:27-31, TODO.md:25, .agents/SESSION_REPORT.md:50, scripts/check_status_consistency.sh:103-132
    The bookkeeping is stale: the plan index still lists tagged-switch-exhaustiveness-remediation-plan-2026-04-23.md as active after the plan is marked completed, and the line-count metadata in TODO.md and .agents/
    SESSION_REPORT.md no longer matches the actual files. The status script does not check those invariants, so it still reports green.
    **Status: CLOSED. Stale bookkeeping synced in the live plan/TODO/session-report artifacts; no code impact.**

  I did not find a concrete high-confidence defect in the memory-model files I checked.

---

# Omni Lisp Deep Audit Report — 2026-04-23 (Second Pass)

Audit scope: Full codebase after commits `9585f1f`, `94036be`, `ad88640`.
Method: 4 parallel agents (Memory/Lifetime, Compiler/Parser, Runtime/Effects, Vulkan/CUDA/ML) + manual verification of all critical/high findings.

---

## Verified Findings

### Critical

| # | File:Line | Description |
|---|-----------|-------------|
| C1 | `src/lisp/jit_reset_shift.c3:74-80` | `jit_reset_impl` leaks `StackCtx` on shift path. When `state.shifted` is true, the function returns `shift_result` at line 80 without ever calling `stack_ctx_destroy(ctx)`. The context is created at line 41 but only destroyed in the non-shift paths (lines 70, 85). |

### High

| # | File:Line | Description |
|---|-----------|-------------|
| H1 | `src/lisp/prim_ui_ftxui.c3:43-61` | `ui_ftxui_collect_children` partial-failure leak. On error after successfully lowering some children, it frees the `children` array but does not free the component pointers already stored in `children[0..i-1]`. If `ui_ftxui_lower_node` did not internally track those components, they are orphaned. |
| H2 | `csrc/tensor_cuda_complex_map.cu:17` | `omni_cuda_status_set` uses `atomicCAS(status, 0u, code)` which implements first-error-wins semantics. If multiple threads encounter different errors, the first (potentially lower-severity) error wins instead of the highest severity. Other CUDA kernels correctly use `atomicMax`. |
| H3 | `src/lisp/jit_handle_signal.c3:122-145` | `jit_handle_dispatch_signal` leaks continuation on `jit_env_extend` failure paths. At lines 122-128, 130-136, and 139-145, when `jit_env_extend` returns null, the function returns an error but the `Continuation* k` allocated at line 107 is never released. It is a root-scoped allocation, so it lives forever. |
| H4 | `src/lisp/jit_runtime_effects_handle.c3:114-115` | `jit_handle_run_signals` leaks continuation on `make_list2_or_error` failure. When `pair.tag == ERROR`, the function returns `pair` but `k` (allocated at line 106) is never released. |

### Medium

| # | File:Line | Description |
|---|-----------|-------------|
| M1 | `src/lisp/eval_boundary_provenance.c3:176-189` | `boundary_graph_alias_unsafe_for_reuse_walk` uses `boundary_alias_push_visit` (silently skips leaf types) instead of `boundary_alias_push_child` (checks `boundary_value_in_releasing_scope`) for CONS cars and non-CONS cdrs. Leaf values residing in the releasing scope are not flagged as unsafe during alias walks. |
| M2 | `src/lisp/eval_boundary_provenance.c3:423-430` | `boundary_try_scope_splice_escapes_result` does not rewrite `scope_gen` stamps; stamp rewriting is delegated to `boundary_rewrite_spliced_escape_stamps`, which is called only in the root-result variant. The non-root variant could leave stale `scope_gen` stamps on spliced values if ever used for non-root splices in production. |
| M3 | `src/lisp/prim_ml_autograd.c3:24` / `src/lisp/prim_ml_autograd_tensor_expr.c3:116` | `ml_grad_resolve_cpu` unconditionally rejects Vulkan backward with "Vulkan backward is not supported", yet concrete Vulkan backward kernels exist for MSE loss, softmax cross-entropy, and broadcast reduction. This creates inconsistent backend coverage. |
| M4 | `csrc/tensor_vulkan_helpers.c:90-101` | `omni_tensor_backend_vulkan_map_unary_f64` and f32 variants miss null check for `input_device_ptr` before forwarding to `dispatch_two_buffer_f*`. For `element_count == 0` the wrapper returns success without validating the pointer. |
| M5 | `csrc/tensor_vulkan_map_unary_f32.comp:44` | `sqrt(value)` lacks domain validation for negative inputs; the host wrapper allows `op == 2` unconditionally, so negative inputs silently produce NaN on the GPU. |
| M6 | `src/lisp/eval_dispatch_match.c3:92` | `find_best_method` ignores allocation failure in `make_cons`/`make_int` when building `best_match_indices_rev`; an error Value propagates into `dispatch_ambiguous_error` and masks the real ambiguity. |
| M7 | `src/lisp/jit_runtime_effects.c3:68-76` | **Note: Not a bug.** When `target_ctx.status == CTX_SUSPENDED` and the enclosing handle frame is still active (`hstate.frame_active == true`), the code intentionally does not release `hstate` because it is still needed by the enclosing handler loop. The decrement at line 74 is conditional on `hstate != null`. This is by design. |

### Low

| # | File:Line | Description |
|---|-----------|-------------|
| L1 | `src/lisp/prim_io_fs_stream.c3:31-47` | `make_fs_handle` closes raw fd and frees `fh` on failure, but uses manual cleanup without `defer`. Future edits adding early returns could introduce leaks. |
| L2 | `csrc/tensor_vulkan_helpers_core.c:138-156` | Physical device selection picks the first device with a compute queue without checking device type, features, or memory properties. Can silently bind to integrated GPU or software implementation when discrete GPU is available. |
| L3 | `src/lisp/prim_tensor_graph_run.c3:55-64` | `tensor_graph_run_vulkan_readable_float32` returns `true` for zero-byte tensors even when `device_handle == null` because the null check is conditional on `byte_len > 0`. A malformed graph node with `byte_len == 0` but `device_handle == null` passes validation. |
| L4 | `src/lisp/value_tensor.c3:118-123,135-140` | `tensor_alloc_payload_base_checked` allocates coefficient/rhs workspace with `mem::malloc` paired with `defer mem::free`, but the pattern is repeated across ~15 matrix primitive files without a shared helper, increasing risk of future edit-induced mismatches. |

---

## False Positives (from agents, manually verified)

| Finding | Verdict | Reason |
|---------|---------|--------|
| `jit_handle_signal_helpers_continuation_scan.c3:82` — missing `break` statements | **False** | C3 has implicit `break` in switch statements. Verified with compiler tests. |
| `jit_runtime_effects.c3:68` — HandleEffectState leak on suspended path | **False** | The `hstate` is intentionally kept alive when `hstate.frame_active == true` because the enclosing handle loop still needs it. Release happens when the handle frame completes. |
| `jit_runtime_effects_reset_shift.c3:65` — dangling `k.ctx` after destroy | **False** | The `k.ctx` is nulled out at `jit_runtime_effects.c3:19-21` before resume, and the context is destroyed after the shift body returns. No dangling reference remains. |

---

## Summary

| Severity | Count | Key Areas |
|----------|-------|-----------|

## Re-Audit Closure

The audit queue from the first pass is now closed in the working tree:

- `src/lisp/compiler_expr_serialize_patterns.c3` now serializes `PAT_GUARD`
  explicitly.
- `src/lisp/tests_compiler_core_groups_serializer_metadata.c3` now covers
  guarded-pattern round-trips, including a nested guarded pattern.
- `src/lisp/compiler_mutable_capture_detection_walk.c3` had a leaf-expression
  fall-through into `E_INDEX`; that crash was fixed and the compiler slice now
  passes.
- `src/lisp/tests_advanced_core_semantics_groups.c3` no longer contains the
  incorrect guard-scope assertion.
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part5.c3` now
  checks the Vulkan empty-solve empty-result contract.
- `docs/todo_parts/todo_part_15.md` is fully closed and `scripts/check_status_consistency.sh`
  reports `TODO actionable count: 0`.

Verification after the re-audit:

- `c3c build --obj-out obj`
- `OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric ./build/main --test-suite lisp`
- `scripts/check_status_consistency.sh`
- `git diff --check`

The compiler crash was rooted in `Compiler.has_set_on`, not in the serializer.
| Critical | 1 | Effect system (reset/shift StackCtx leak) |
| High | 4 | UI components, CUDA error semantics, effect system continuation leaks |
| Medium | 6 | Boundary provenance, ML autograd Vulkan coverage, Vulkan validation |
| Low | 4 | FS stream cleanup, Vulkan device selection, tensor graph validation |
| False Positive | 3 | C3 switch semantics, effect state lifetime |

**Recommended priority order for fixes:** C1 → H3 → H4 → H1 → H2 → M1 → M3 → M4 → M5 → M6

## Implementation Owner Re-Audit Note

This pass re-checked the owned JIT/effects/runtime and UI surfaces against the
reported findings.

- `src/lisp/jit_reset_shift.c3` already contains the `stack_ctx_destroy()`
  cleanup on the shift path.
- `src/lisp/jit_handle_signal.c3` and `src/lisp/jit_runtime_effects_handle.c3`
  already discard continuations on the failure paths called out in the audit.
- `src/lisp/value_interp_init_helpers.c3` already initializes the runtime
  flags that were previously stale.
- `src/lisp/prim_io_fs_stream.c3` now uses a deferred cleanup block around the
  raw `FsHandle` ownership transfer in `make_fs_handle()`.
- `src/lisp/prim_ui_ftxui.c3` does not expose the reported partial-failure leak
  as a live bug: `ui_ftxui_lower_node()` tracks created components in
  `UiFtxuiRunState`, and `ui_ftxui_destroy_state()` reclaims them on the error
  path.

Verification for this owner pass:

- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh env OMNI_TEST_VERBOSE=0 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/run_validation_container.sh env OMNI_TEST_VERBOSE=0 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `git diff --check`
# Re-audit note

The live tree no longer matches the original JIT/UI audit surface exactly.

- The JIT/reset-shift and continuation leak claims were already fixed in the current codebase before this pass.
- The UI `ui_ftxui_collect_children` partial-failure leak claim did not reproduce as a live bug when rechecked against `UiFtxuiRunState` ownership and `ui_ftxui_destroy_state()`.
- The concrete runtime issues that were still live and were fixed in this pass are:
  - `csrc/tensor_vulkan_helpers.c`: reject non-empty null `input_device_ptr` calls consistently across the Vulkan unary host wrappers.
  - `src/lisp/prim_tensor_graph_run.c3`: require a device handle for Vulkan view tensors even when the byte length is zero.
  - `src/lisp/eval_dispatch_match.c3`: fail closed on ambiguity-list allocation failures.

Verification:
- `c3c build --obj-out obj`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain ./build/main --test-suite lisp`
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system-surface ./build/main --test-suite lisp`
- `scripts/check_status_consistency.sh`
- `git diff --check`

## 2026-04-23 audit re-check closure

I re-read the remaining entries in the audit report against the live tree after the above fixes. The following findings are already closed in code and should no longer be treated as active work:

- `H2` is closed: `csrc/tensor_cuda_complex_map.cu` now uses `atomicMax`-style priority semantics through `omni_cuda_status_set`.
- `H3` and `H4` are closed: `src/lisp/jit_handle_signal.c3` and `src/lisp/jit_runtime_effects_handle.c3` now discard the allocated continuation on every allocation-failure path.
- `M1` is closed: `src/lisp/eval_boundary_provenance.c3` now uses `boundary_alias_push_child` for the alias walk over child payloads.
- `M4` is closed: `csrc/tensor_vulkan_helpers.c` now validates non-empty Vulkan unary calls before dispatch.
- `M6` is closed: `src/lisp/eval_dispatch_match.c3` now builds the ambiguity payload with checked allocation helpers.
- `L1` is closed: `src/lisp/prim_io_fs_stream.c3` now uses deferred cleanup around `make_fs_handle()`.
- `L2` is closed: `csrc/tensor_vulkan_helpers_core.c` now probes device properties and scores discrete GPUs above integrated/software devices instead of blindly taking the first compute-capable queue family.
- `L3` is closed: `src/lisp/prim_tensor_graph_run.c3` now keeps the zero-byte Vulkan readable-tensor contract aligned with the storage helpers.

The remaining lines in the audit report are not actionable defects in the live tree:

- `M2` is a non-live theoretical splice path; the root-result path already rewrites stamps before splicing, and the non-root helper is not used as a production splice entry point.
- `M3` is an intentional contract boundary in the generic linear-gradient path; the Vulkan-specific gradient lanes already have separate code paths where applicable.
- `M5` follows the same sqrt/NaN behavior as the CPU/shared tensor unary math path, so it is not a backend divergence in the current implementation.
- `L4` is a refactor opportunity, not a correctness defect.

With those dispositions, the actionable audit queue is exhausted again.

Signature: GPT-5 Codex

## Bookkeeping Sync

The stale foreign-runtime follow-up wording that previously suggested an open
TLS lifecycle residual has now been synchronized with the live tree:

- `docs/areas/ffi-foreign-runtime.md` now marks the TLS offload/in-flight
  lifecycle follow-up as a closed validation checkpoint.
- `docs/plans/foreign-runtime-core-plan-2026-04-11.md` now states that no live
  scheduler-bound TLS lifecycle follow-up remains.

This does not add a new defect; it closes the lingering documentation drift
recorded during the earlier audit passes.
