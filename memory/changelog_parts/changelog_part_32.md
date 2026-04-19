# Memory Changelog Index Part 32

Source: `memory/CHANGELOG.md`

        - `tls-server-wrap` argument/type/handle/credential/reset/oom paths,
        - `tls-read`/`tls-write`/`tls-close` argument and handle validation paths.
      - preserved user-facing error messages (for migration) while canonicalizing payload codes.
    - `src/lisp/tls_handle_lifecycle.c3`:
      - normalized TLS-handle allocation failure to canonical `io/tls-connect-out-of-memory`.
  - regression updates:
    - `src/lisp/tests_runtime_async_groups.c3`:
      - added explicit payload-code assertions for TLS wrapper error surfaces:
        - `tls-connect non-string ca bundle payload code`
        - `tls-connect invalid tcp handle payload code`
        - `tls-write expected data payload code`
        - `tls-server-wrap invalid stream payload code`
  - docs and plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-05` complete.
      - narrowed `P2.7` residual note to dispatcher-internals only (`CP-06`).
    - `docs/ERROR_MODEL.md`:
      - moved `Async/network primitives` row from `partial` to `done`.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1609/0`, `compiler: 79/0`).
- 2026-03-09 (effects/typesystem CP-06 closure: effect-dispatcher internals canonical payload normalization):
  - effect dispatcher/runtime migration:
    - `src/lisp/jit_jit_handle_signal.c3`:
      - added runtime canonical helpers for fast-path/error conversion.
      - migrated remaining mixed paths to payloaded raises:
        - `perform` type mismatch now consistently emits `runtime/effect-arg-mismatch`.
        - fast-path primitive failures now emit `runtime/fast-path-primitive-failed` with propagated message text.
        - `raise` handler nil-return path now emits `runtime/raise-handler-returned-nil`.
    - `src/lisp/jit_jit_runtime_effects.c3`:
      - migrated fast-path eval-result error conversion to canonical `runtime/fast-path-primitive-failed`.
    - `src/lisp/jit_jit_reset_shift.c3`:
      - migrated `shift` outside reset and suspend-guard failures to canonical runtime payload codes:
        - `runtime/shift-outside-reset`
        - `runtime/suspend-scope-guard`
    - `src/lisp/jit_jit_compile_effects_modules.c3`:
      - migrated `jit_do_export_from` eval-error bridge to canonical `runtime/export-from-failed`.
  - audit closure:
    - no remaining direct `raise_error(...)`/`make_error(...)` usage in:
      - `jit_jit_*effects*.c3`
      - `jit_jit_handle_signal.c3`
      - `jit_jit_reset_shift.c3`
      - `jit_jit_compile_effects_modules.c3`
  - docs and plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-06` complete.
      - marked `P2.7` complete.
    - `docs/ERROR_MODEL.md`:
      - moved `Effect dispatcher internals` row from `partial` to `done`.
      - removed stale pending note that referenced `P2.7`.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1609/0`, `compiler: 79/0`).
- 2026-03-09 (effects/typesystem CP-07 closure: migrated error-model family regression coverage):
  - regression expansion:
    - `src/lisp/tests_advanced_core_unicode_groups.c3`:
      - added explicit dispatcher/runtime payload-code assertions for migrated effect internals:
        - `shift outside reset payload code` -> `runtime/shift-outside-reset`
        - `effect arg mismatch payload code` -> `runtime/effect-arg-mismatch`
      - complements existing migrated-family code assertions already present for:
        - stdlib `try/assert!` payload normalization
        - regex malformed-pattern codes
        - file-I/O wrapper codes
        - async/network TLS codes
        - scheduler payload codes
        - deduce payload codes
        - runtime unhandled/invalid-continuation payload codes
  - docs and plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-07` complete.
      - marked `P2.10` complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1611/0`, `compiler: 79/0`).
- 2026-03-09 (effects/typesystem CP-08 closure: mixed-failure-style acceptance gap enforcement):
  - policy enforcement hardening:
    - `scripts/check_effects_contract_policy.sh`:
      - added `check_migrated_surfaces_no_legacy_failure_constructors()`.
      - enforces no `raise_error(...)` or `make_error(...)` in migrated canonicalized surfaces:
        - `src/lisp/tls_primitives.c3`
        - `src/lisp/tls_handle_lifecycle.c3`
        - `src/lisp/jit_jit_handle_signal.c3`
        - `src/lisp/jit_jit_runtime_effects.c3`
        - `src/lisp/jit_jit_reset_shift.c3`
        - `src/lisp/jit_jit_compile_effects_modules.c3`
      - gate fails deterministically if constructors reappear in those files.
  - plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-08` complete.
      - marked `A2.1` complete.
  - Validation:
    - `scripts/check_effects_contract_policy.sh` passed.
    - `scripts/run_effects_contract_lint.sh` passed.
- 2026-03-09 (effects/typesystem CP-09 closure: phase execution-order drift):
  - sequencing closure:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-09` complete.
      - marked execution-order item `E2` complete, reflecting that Phase 1 (effects semantics docs) and Phase 2 (effects-first migration matrix + behavior updates) have now converged and closed together.
- 2026-03-09 (effects/typesystem CP-10..CP-13 closure: dispatch/type parity residuals):
  - runtime dispatch/type-system behavior:
    - `src/lisp/value_core_types.c3`:
      - extended `MethodSignature` with `param_type_vars` for explicit method-level type-variable binding (`^T`).
    - `src/lisp/jit_jit_closure_define_qq.c3`:
      - propagated `param_type_vars` through method-signature allocation/copy/free paths.
      - signature builder now records type-variable symbol bindings for:
        - unresolved plain type annotations (e.g., `^T`),
        - single-key dict constraints (e.g., `^{'T Number}`).
    - `src/lisp/jit_jit_define_method_table.c3`:
      - signature equality now includes `param_type_vars`, preventing accidental coalescing of semantically distinct generic signatures.
    - `src/lisp/eval_dispatch_types.c3`:
      - added runtime unification for repeated method type variables:
        - `(^T a) (^T b)` requires equal inferred runtime types.
      - constraint checks now prefer unified bindings when available, then apply existential fallback for unconstrained/unbound dict-only cases.
  - regression coverage expansion:
    - `src/lisp/tests_advanced_type_effect_ffi_groups.c3`:
      - tightened ambiguity diagnostics coverage:
        - added equal-specificity detail assertion for positional `Int` ambiguity.
      - added parametric unification regressions:
        - same-type success path (`pair-same`),
        - mixed-type fallback path,
        - constrained (`Number`) same-type success,
        - constrained mixed-type fallback.
      - added union applicability regressions:
        - union-parent method accepts variants,
        - variant-specialized methods outrank union-parent method.
    - `src/lisp/tests_runtime_feature_groups.c3`:
      - added runtime exhaustiveness-policy anchor:
        - wildcard branch intentionally handles non-exhaustive union matches.
  - docs and plan parity:
    - `docs/type-system-syntax.md`:
      - moved parity rows to done for:
        - ambiguity handling,
        - parametric substitution/unification semantics,
        - union participation in dispatch applicability.
      - defined explicit runtime-only exhaustiveness policy (compile-time checker is an explicit non-goal for now) with test anchors.
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-10`, `CP-11`, `CP-12`, `CP-13` complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1624/0`, `compiler: 79/0`).
- 2026-03-09 (L3.2 closure: constructor annotation checking path implementation):
  - parser/evaluator wiring:
    - `src/lisp/parser_expr_head_forms.c3` now recognizes list-head `^` as constructor type-application syntax.
    - `src/lisp/parser_type_literals.c3` adds `parse_constructor_type_application`, rewriting:
      - `(^(Ctor ...) value)` to a `^` primitive call with the type form captured as quoted datum (type form is not evaluated).
    - `src/lisp/eval_init_primitives.c3` registers `^` -> `prim_ctor_type_apply` (arity 2).
  - runtime checker:
    - `src/lisp/primitives_meta_types.c3` adds constructor annotation matcher (`prim_ctor_type_apply`) with:
      - constructor applicability checks against runtime instance constructor,
      - type-arg arity checks against inferred runtime `type_args`,
      - recursive nested checks for instance-backed nested constructor args,
      - runtime-inference fallback when nested runtime values do not carry parameterized instance metadata.
    - canonical payloaded diagnostics now emitted for constructor annotation failures:
      - `type/ctor-arity-mismatch`
      - `type/ctor-type-arg-mismatch`
    - payload `data` includes deterministic fields (`ctor`, `expected-args`, `actual-args`, and mismatch path/index fields for type-arg mismatches).
  - regression coverage:
    - `src/lisp/tests_advanced_type_effect_ffi_groups.c3` adds constructor annotation tests for:
      - positive constructor type-application checks,
      - mismatch and arity mismatch payload code assertions,
      - deterministic `arg-index` / `arg-path` payload fields,
      - nested constructor mismatch path depth,
      - regression that unconstrained constructor calls still work.
  - docs/status alignment:
    - `TODO.md` marks `L3.2` complete.
    - `docs/type-system-syntax.md` updates constructor type-application status to implemented (with explicit runtime-inference fallback note).
    - `docs/areas/types-dispatch.md` removes constructor type-application from known open gaps.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1669 passed, 0 failed`; `Compiler Tests: 79 passed, 0 failed`).
- 2026-03-09 (L3.3 closure: lambda call-boundary type checking implementation):
  - runtime call-boundary checker:
    - `src/lisp/eval_dispatch_types.c3` adds `check_lambda_call_boundary(...)` and payloaded lambda call mismatch diagnostics.
    - typed closure invocation now reuses canonical dispatch migration logic (literal/exact/widen/subtype/Any + constraint/unification checks).
    - deterministic mismatch payload data includes:
      - `failure`, `param-index`, `expected`, `actual`, `expected-arity`, `actual-arity`.
    - canonical diagnostic codes used:
      - `type/arg-mismatch`
      - `type/arity` (arity-form mismatches).
  - apply-path wiring:
    - `src/lisp/jit_jit_apply_runtime.c3`:
      - single-arg closure apply (`jit_apply_value_closure`) now enforces typed boundary checks before env binding/body eval.
      - TCO single-arg closure apply (`jit_apply_value_tail`) now enforces the same boundary checks.
    - `src/lisp/jit_jit_apply_multi_prims.c3`:
      - multi-arg and variadic closure apply paths enforce typed fixed-prefix checks for both normal and tail apply flows.
  - ownership/lifetime correctness hardening for closure signatures:
    - enabling runtime reads of `Closure.type_sig` exposed stale-pointer risk from shallow signature copies across boundary promotion paths.
    - `src/lisp/eval_promotion_copy.c3` now deep-copies method signatures into target scope via `method_signature_copy_to_scope(...)` instead of struct-only pointer copy.
    - `src/lisp/eval_promotion_escape.c3` now deep-copies method signatures into ESCAPE lane storage (`method_signature_copy_to_escape(...)`) instead of struct-only pointer copy.
    - this restores deterministic signature reads for promoted/copied closures and prevents false-positive call-boundary mismatches.
  - lambda annotation shape handling:
    - `src/lisp/jit_jit_closure_define_qq.c3`:
      - `^Lambda` / `^(Lambda ...)` parameter annotations now compile to closure-valued gating (`Closure`) instead of unresolved type-variable fallback.
  - regression coverage:
    - `src/lisp/tests_advanced_type_effect_ffi_groups.c3` adds lambda boundary tests for:
      - typed lambda positive/mismatch paths,
      - deterministic mismatch payload fields,
      - variadic fixed-prefix typed-arg mismatch,
      - `^(Lambda ...)` closure-value gating.
  - docs/status alignment:
    - `docs/type-system-syntax.md`:
      - adds `1.2.1 Lambda Call-Boundary Checking` (supported shapes, diagnostics, explicit non-goals).
      - marks lambda row in implementation status as implemented for call-boundary argument checking.
    - `docs/areas/types-dispatch.md` updates open-gap text from `L3.3` to remaining `L3.4/L3.5` closure slices.
    - `TODO.md` marks `L3.3` checklist complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1676/0`, `compiler: 79/0`).
- 2026-03-09 (L3.4 closure: constructor/lambda regression suite expansion):
  - regression additions (`src/lisp/tests_advanced_type_effect_ffi_groups.c3`):
    - constructor annotation mismatch payload determinism coverage expanded:
      - asserts `data.ctor`,
      - asserts `data.expected-args` length,
      - asserts `data.actual-args` length.
    - lambda call-boundary mismatch payload determinism coverage expanded:
      - asserts `data.failure`,
      - asserts `data.expected`,
      - asserts `data.actual`.
    - lambda boundary cross-tests added for:
      - dispatch integration (typed lambda in dispatch-selected method and fallback path),
      - union subtype migration at lambda boundary,
      - numeric widening (`Int` satisfying `^Double`) parity with dispatch semantics.
  - TODO closure:
    - `TODO.md`: marked `L3.4` checklist rows complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1676/0`, `compiler: 79/0`).
- 2026-03-16 (editor tooling: syntactic call hierarchy across `omni-lsp` and `omni-nvim`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - now advertises `callHierarchyProvider`,
    - implements `textDocument/prepareCallHierarchy`,
    - implements `callHierarchy/incomingCalls`,
    - implements `callHierarchy/outgoingCalls`,
    - reuses cached open/unopened workspace declaration summaries so call
      hierarchy can resolve local declarations, open workspace files, and
      unopened workspace files without semantic indexing.
  - call hierarchy preparation prefers the exact declaration range under the
    cursor when the cursor is already on a declaration site, which avoids
    returning same-name outer declarations like a module and nested function
    with identical names.
  - outgoing/incoming calls are derived from syntactic recursive body scans over
    declaration forms, with special forms excluded from call-site capture.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers:
    - advertised call hierarchy capability,
    - outgoing calls from an open declaration into both an open workspace file
      and an unopened workspace file,
    - incoming calls back into an unopened workspace declaration.
  - `tooling/omni-nvim/lua/omni/lsp.lua` now exposes quickfix-backed incoming
    and outgoing call list helpers over the new call hierarchy requests.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspIncomingCallsList`,
    - `:OmniLspOutgoingCallsList`,
    - default buffer-local mappings:
      - `<localleader>lC` for incoming calls,
      - `<localleader>lc` for outgoing calls.
  - docs updated:
    - `tooling/omni-lsp/README.md`
    - `tooling/omni-nvim/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
    - headless Neovim check for `OmniLspIncomingCallsList` /
      `OmniLspOutgoingCallsList`, quickfix population, and buffer-local mapping
      registration.
- 2026-03-16 (editor tooling: workspace fallback references in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - `textDocument/references` now stays current-buffer when the active buffer
      owns the declaration, preserving the existing overloaded/local behavior,
    - when the active buffer has no local declaration for the symbol, the
      server now falls back to exact-name references across:
      - the current buffer,
      - other open Omni documents,
      - unopened workspace-root Omni files.
  - the fallback reuses the existing workspace declaration cache to decide when
    a workspace reference search is valid, while still returning declaration
    sites according to `includeDeclaration`.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers a workspace fallback
    reference query from `caller.omni`, proving results from:
    - the caller buffer,
    - another open workspace file,
    - an unopened workspace declaration file.
  - docs updated:
    - `tooling/omni-lsp/README.md`
    - `tooling/omni-nvim/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: workspace fallback rename in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - `textDocument/prepareRename` now succeeds on caller-side symbols when the
      active buffer has no local declaration but an exact-name workspace
      declaration exists,
    - `textDocument/rename` now follows the same local-first boundary as
      references:
      - local declarations still rename only within the active buffer,
      - caller-side symbols without a local declaration now produce exact-name
        workspace edits across open and unopened workspace Omni files.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers workspace fallback rename
    from `caller.omni`, proving edits for:
    - the caller buffer,
    - another open workspace file,
    - an unopened declaration file.
  - docs updated:
    - `tooling/omni-lsp/README.md`
    - `tooling/omni-nvim/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: source-backed completion resolve in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - `completionProvider` now advertises `resolveProvider`,
    - `completionItem/resolve` now enriches local and workspace declaration
      items with markdown docs backed by declaration snippets,
    - completion ordering and ranking are unchanged; the new path only adds
      source-backed docs/details for declaration items.
  - local completion items now carry declaration snippet metadata from the
    current buffer, and workspace completion items now carry snippet metadata
    from cached open/unopened workspace declarations.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers:
    - advertised completion resolve capability,
    - local completion resolve for `ping`,
    - workspace completion resolve for `describe-updated`.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: declaration codelens across `omni-lsp` and `omni-nvim`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - now advertises `codeLensProvider`,
    - serves `textDocument/codeLens` for non-module declarations in the current
      document,
    - now resolves codelenses lazily:
      - `textDocument/codeLens` returns lightweight unresolved declaration
        lenses,
      - `codeLens/resolve` fills in the reference count and first-party
        `omni.showReferences` payload.
  - codelens counting is workspace-aware for unique declaration names through
    the existing exact-name reference path, while overloaded local names stay
    local-only so the server does not guess which overload a workspace call
    belongs to.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers:
    - advertised codelens resolve capability,
    - unresolved codelens payload shape,
    - resolved workspace-aware codelens counts for `answer`, `Point`, and
      `helper`,
    - emitted `omni.showReferences` command payload shape after resolve.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspCodeLensRefresh`,
    - `:OmniLspCodeLensRun`,
    - default buffer-local mappings:
      - `<localleader>ll` refresh codelens,
      - `<localleader>lL` run codelens.
  - `tooling/omni-nvim/lua/omni/lsp.lua` now registers the `omni.showReferences`
    LSP command and opens the supplied locations in quickfix, so running Omni
    codelenses in Neovim does something useful instead of depending on a
    VSCode-style command id.
  - `tooling/omni-nvim/lua/omni/init.lua` now supports opt-in
    `lsp.codelens.auto_refresh` with configurable buffer-local refresh events
    (default example: `BufEnter`, `InsertLeave`) for Omni buffers.
  - docs updated:
    - `tooling/omni-lsp/README.md`
    - `tooling/omni-nvim/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
    - headless Neovim check for codelens refresh/run commands, buffer-local
      mapping registration, `omni.showReferences` command handling, and opt-in
      codelens auto-refresh autocmd wiring.
- 2026-03-16 (editor tooling: document links for Omni module/import forms):
  - `tooling/omni-lsp/omni_lsp.py`:
    - now advertises `documentLinkProvider`,
    - serves `textDocument/documentLink` for top-level `import` and
      `export-from` forms,
    - resolves string imports relative to the current file path,
    - resolves symbol targets through exact-name workspace module declarations.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers:
    - advertised document-link capability,
    - a relative string import link,
    - an `export-from` module-name link into an unopened workspace file.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: document-link opening in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now adds a synchronous
    `textDocument/documentLink` consumer that:
    - finds the link under the cursor,
    - opens file URI targets directly in Neovim,
    - reports when no link exists at the cursor.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspOpenLink`,
    - default buffer-local mapping `<localleader>lo`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniLspOpenLink`, file-target opening, and
      buffer-local mapping registration.
- 2026-03-16 (editor tooling: document-link quickfix list in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now adds a synchronous
    `textDocument/documentLink` quickfix helper for the current buffer.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspDocumentLinks`,
    - default buffer-local mapping `<localleader>lO`.
  - the quickfix list points at source link locations in the current Omni
    buffer and labels each entry with the linked target file name.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniLspDocumentLinks`, quickfix population, and
      buffer-local mapping registration.
- 2026-03-16 (editor tooling: document-link range navigation in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now adds next/previous navigation over
    document-link ranges in the current Omni buffer.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspNextLink`,
    - `:OmniLspPrevLink`,
    - default buffer-local mappings:
      - `]o` next link,
      - `[o` previous link.
  - navigation wraps across the current buffer’s document-link ranges so the
    motion remains useful even in small import/export lists.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for link-range navigation and buffer-local mapping
      registration.
- 2026-03-16 (editor tooling: document-highlight controls in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspDocumentHighlight`,
    - `:OmniLspClearReferences`,
    - default buffer-local mappings:
      - `<localleader>lh` document highlight,
      - `<localleader>lH` clear references.
  - `omni-nvim` also now supports optional buffer-local document-highlight
    autocmds under `lsp.highlights`, with separate refresh and clear event lists
    so Omni symbol highlights can follow cursor movement without depending on
    external editor glue.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for explicit highlight/clear commands, buffer-local
      mapping registration, and auto-refresh autocmd behavior.
- 2026-03-16 (editor tooling: semantic tokens in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises
    `textDocument/semanticTokens/full` with a first-party legend and serves a
    syntactic token stream for current-buffer declaration sites, parameter
    bindings, special-form heads, builtin type/value names, numbers, strings,
    and comments.
  - the implementation reuses the existing declaration/form scans plus a small
    lexical pass, so semantic highlighting stays cheap and aligned with the
    rest of the server’s local-first editor behavior.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: linked editing ranges in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises
    `textDocument/linkedEditingRange` and serves a local syntactic range set
    for parameter declarations plus same-name uses inside the owning
    declaration body.
  - the first pass is intentionally conservative: it skips obvious nested
    `define`, `module`, and `quote` forms so linked edits do not cross simple
    shadowing boundaries.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: on-type formatting in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises
    `textDocument/onTypeFormatting` for newline and `)` triggers.
  - the implementation reuses the existing conservative formatter and returns a
    line-scoped indentation edit for the touched line instead of attempting a
    broader pretty-print rewrite on each keystroke.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: monikers in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises `textDocument/moniker` and
    serves first-party Omni monikers for local declaration sites plus exact-name
    workspace fallback when the active buffer only has a caller-side symbol.
  - the initial identifier shape is container/detail/name based, with `export`
    used for declaration-site monikers, `local` for same-document references,
    and `import` for workspace fallback references.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: pull diagnostics in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises `diagnosticProvider` and
    serves `textDocument/diagnostic` with full reports backed by the same
    `omni --check --json` path already used for publish diagnostics.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: workspace pull diagnostics in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now serves `workspace/diagnostic` with full
    and unchanged per-document reports across open and unopened workspace
    `.omni` files, keyed by stable result ids derived from current document
    text.
  - the implementation still uses the same `omni --check --json` path as push
    diagnostics and text-document pull diagnostics; this is a transport surface
    expansion, not a second diagnostics engine.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: import-string navigation in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now lets definition/declaration treat
    string import targets like `"extra.omni"` as navigable locations by
    reusing the existing document-link resolution path.
  - import-string navigation lands on the imported file’s entry declaration when
    the target file is available, instead of forcing editors to treat the
    string as inert text.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: pull diagnostics commands in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now adds quickfix-backed helpers for:
    - `textDocument/diagnostic`,
    - `workspace/diagnostic`.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspDocumentDiagnostics`,
    - `:OmniLspWorkspaceDiagnostics`,
    - default buffer-local mappings:
      - `<localleader>lp` document diagnostics,
      - `<localleader>lP` workspace diagnostics.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for pull-diagnostics requests, quickfix population,
      command registration, and buffer-local mapping registration.
- 2026-03-16 (editor tooling: cached pull diagnostics in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now caches pull diagnostic
    `resultId` values and prior diagnostic items for both:
    - `textDocument/diagnostic`,
    - `workspace/diagnostic`.
  - repeated pull-diagnostics commands now send prior result ids and still
    rebuild quickfix from cached items when the Omni LSP server replies
    `unchanged`, instead of clearing the list on stable results.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for cached pull-diagnostics request params and
      unchanged-result quickfix reuse.
- 2026-03-16 (editor tooling: pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now adds opt-in buffer-local autocmd
    support under `lsp.pull_diagnostics` for current-document pull diagnostics.
  - when enabled, `omni-nvim` refreshes `textDocument/diagnostic` on the
    configured events and keeps the quickfix snapshot in sync without requiring
    manual `:OmniLspDocumentDiagnostics` calls.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniNvimPullDiagnostics` autocmd
      installation and event-driven document-diagnostics requests.
- 2026-03-16 (editor tooling: workspace pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now adds a separate opt-in
    `lsp.pull_diagnostics.workspace_auto_refresh` path with independently
    configurable events for workspace pull diagnostics.
  - when enabled, `omni-nvim` refreshes `workspace/diagnostic` on the
    configured events and keeps the workspace quickfix snapshot current without
    requiring manual `:OmniLspWorkspaceDiagnostics` calls.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniNvimWorkspacePullDiagnostics` autocmd
      installation and event-driven workspace-diagnostics requests.
- 2026-03-16 (editor tooling: pull diagnostics cache reset commands in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now exposes reset helpers for cached:
    - current-buffer `textDocument/diagnostic` result ids,
    - workspace `workspace/diagnostic` result ids.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspDocumentDiagnosticsReset`,
    - `:OmniLspWorkspaceDiagnosticsReset`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for cache-reset command registration and cleared
      prior-result-id request params.
- 2026-03-16 (editor tooling: automatic pull diagnostics cache cleanup in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now installs buffer-local cleanup
    autocmds for cached pull diagnostics state:
    - current-buffer `textDocument/diagnostic` cache entries clear on
      `BufDelete` and `BufWipeout`,
    - current-buffer and workspace pull-diagnostics caches clear when the
      `omni_lsp` client detaches from that buffer.
  - this keeps stale `previousResultId` values from surviving wiped buffers or
    old Omni LSP sessions, reducing the need for manual reset commands after
    editor-side lifecycle changes.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for buffer-wipe and `LspDetach` cache cleanup.
- 2026-03-16 (editor tooling: quiet pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` pull-diagnostics helpers now accept a
    quickfix-open option instead of always forcing `copen`.
  - `tooling/omni-nvim/lua/omni/init.lua` now runs automatic current-buffer and
    workspace pull-diagnostics refreshes with `open = false`, so configured
    refresh events keep the quickfix snapshot current without stealing the
    current window on each event.
  - manual `:OmniLspDocumentDiagnostics` and `:OmniLspWorkspaceDiagnostics`
    behavior is unchanged: explicit commands still open quickfix.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check confirming explicit commands still open quickfix,
      while event-driven auto-refresh updates quickfix items without reopening
      the quickfix window.
