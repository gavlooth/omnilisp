# Historical Audit Findings - Superseded

Status as of 2026-04-23 01:18 CEST: this file is retained as the original
2026-04-22 audit note, not as the current live queue. Every numbered item below
has a corresponding closed checkbox in `docs/todo_parts/todo_part_15.md`:

- `AUDIT-2026-FFI-CALLBACK-RELEASE-SEGFAULT`
- `AUDIT-2026-ERROR-MODEL-MIGRATION`
- `AUDIT-2026-VULKAN-BWD-STATUS-CONTRADICTION`
- `AUDIT-2026-ML-VISUALIZATION-GAP`
- `AUDIT-2026-VALIDATION-MASKING`
- `AUDIT-2026-CSTRING-SCANNING`
- `AUDIT-2026-TAGGED-SWITCH`

The original `foreign-release` callback repro now returns `#<void>` on the
current binary, and the regression is covered in
`src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3`. Use `TODO.md`,
`docs/todo_parts/`, `memory/changelog_parts/changelog_part_37.md`, and
`.agents/SESSION_REPORT.md` as the current status sources.

## Original Findings

  1. Critical: ffi-callback handles segfault when released through the public foreign-release path.
     src/lisp/prim_ffi_callback.c3:207 defines ffi_callback_context_finalizer(void* raw) and casts raw to FfiCallbackContext*. But the handle is created with ctx.code_ptr as the stored FFI handle pointer at src/lisp/
     prim_ffi_callback.c3:408, while the actual ctx is only passed to libffi closure prep at src/lisp/prim_ffi_callback.c3:390. The comment at src/lisp/prim_ffi_callback.c3:424 says the context pointer is stored, but the code
     stores ctx.code_ptr.
     The release path in src/lisp/value_constructors.c3:178 passes box.lib_handle into the finalizer, and src/lisp/foreign_runtime_core.c3:360 exposes that through foreign-release. That means the finalizer receives executable
     callback code memory, not the context object, then dereferences it as a struct.
     I reproduced this with:

     LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(let (cb (ffi-callback (lambda (a b) (+ a b)) ['Integer 'Integer] 'Integer)) (foreign-release cb))"
     Result: Segmentation fault, exit status 139. Existing tests at src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:590 cover callback creation/invocation, but not callback release.
     Result: Segmentation fault, exit status 139. Existing tests at src/lisp/tests_advanced_io_effect_ffi_ffi_surface_groups.c3:590 cover callback creation/invocation, but not callback release.
  2. High: the structured error model is documented as complete, but many public/runtime paths still emit raw string errors.
     docs/ERROR_MODEL.md:1 says the baseline is complete and defines canonical error payloads with code, message, domain, and data. The runtime has structured helpers at src/lisp/value_constructors.c3:515, but the raw raise_error
     path at src/lisp/value_constructors.c3:556 still produces a payload-less pending error.
     Static scan results in src/lisp show 1244 uses of raise_error_with_payload_names, but also 1013 non-test return raise_error(interp, ...) sites. Examples include FFI callbacks at src/lisp/prim_ffi_callback.c3:254, foreign
     runtime release/unwrap paths at src/lisp/foreign_runtime_core.c3:362, HTTP at src/lisp/http.c3:24, JSON at src/lisp/json.c3:116, and generic collection mutation at src/lisp/prim_collection_generic_set.c3:9.
     This is exactly the “sane error messages” gap: the system has the structured mechanism, but the migration is not complete enough for users or tooling to rely on it.
  3. High: ML autograd Vulkan status is internally contradictory between spec, TODO, and code.
     The language spec still says ml/grad keeps CUDA/Vulkan backward fail-closed at docs/LANGUAGE_SPEC.part-01b.md:394. But TODO records shipped Vulkan backward slices for MSE, map expressions, broadcast map, and softmax cross
     entropy at docs/todo_parts/todo_part_14.md:690. The code implements Vulkan MSE backward at src/lisp/prim_ml_autograd_tensor_expr.c3:300 and Vulkan softmax-CE backward at src/lisp/prim_ml_autograd_tensor_expr.c3:749.
     There is also a stale rejection message at src/lisp/prim_ml_autograd_tensor_expr.c3:116 that still says Vulkan backward kernels are not implemented. Current behavior may be partially correct, but the user-facing contract is
     not coherent.
  4. Medium: ML visualization/graphics tooling is not a first-class surface.
     I found generic terminal graph helpers in examples/libraries/ftxui/ui_nodes.omni:55 and example references to ui.graph, but no obvious ML-facing visualization primitives such as ml/plot, tensor image display, dataset preview,
     training curves, confusion matrices, feature map inspection, or graph/export helpers. Given the repo’s ML/tensor ambitions, this is a real product gap rather than just missing examples.
  5. Medium: validation status can mask memory/lifetime diagnostics.
     The current status consistency check reports validation green, but existing project memory records that memory-lifetime-smoke can emit boundary graph-audit violations while still reporting pass=237 fail=0. I did not rerun the
     heavy memory-lifetime suite because repo policy requires bounded container execution for those runs. The risk is that diagnostic output is not always promoted to a failing verification signal, so “green” can overstate memory-
     management health unless logs are audited or the harness is tightened.
  6. Low/Medium: bounded C-string scanning is inconsistent.
     There are hardened bounded helpers, for example src/entry_runtime_project_paths.c3:9, but several CLI/bindgen/runtime-adjacent paths still scan until NUL without a cap. Examples include src/entry_cli_helpers.c3:15, src/
     entry_bind_paths.c3:7, src/entry_bind_dep_generation.c3:371, and src/main_repl_shared.c3:21.
     This is not automatically exploitable for normal argv/env strings, but the hardening style is uneven around host/runtime boundaries.
  7. Low: exhaustive tagged-switch discipline is not enforced.
     The style guide prefers exhaustive switch handling for tagged unions such as ValueTag, but default: remains common in src/lisp. Some are likely legitimate, but examples like src/lisp/value_print.c3:104, src/lisp/
     eval_promotion_copy.c3:120, src/lisp/prim_tensor_storage.c3:29, and src/lisp/foreign_runtime_core.c3:92 should be audited so new value tags cannot silently fall into generic behavior.

  Important Stale Items Avoided

  There is an existing AUDIT_REPORT_2026-04-22.md in the dirty working tree, but I did not treat it as authoritative. Several serious items in that report appear already remediated on disk: the JIT allocation checks in src/lisp/
  jit_compile_expr_core.c3:94, REPL protocol send diagnostics in src/lisp/eval_repl_server_worker.c3:18, environment hash-table error logging in src/lisp/value_environment.c3:49, and the runtime manifest entries in src/
  entry_build_runtime_manifest_lisp_part3.c3:1.

  Verification Run

  Commands actually run:

  jj status
  c3c build
  scripts/check_status_consistency.sh
  scripts/check_file_size_gate.sh
  LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(let (cb (ffi-callback (lambda (a b) (+ a b)) ['Integer 'Integer] 'Integer)) (unsafe-free! cb))"
  LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(let (cb (ffi-callback (lambda (a b) (+ a b)) ['Integer 'Integer] 'Integer)) (foreign-release cb))"

  Results:

  c3c build: passed
  check_status_consistency.sh: passed
  check_file_size_gate.sh: passed
  unsafe-free! callback probe: returned #<void>
  foreign-release callback probe: failed with SIGSEGV, status 139

  Not run: full suite, all-slice validation, memory-lifetime-soak, and ASAN. The repo policy says heavy/high-memory validation should run through the bounded Docker validation path; I kept this audit to targeted static and runtime
  probes.
