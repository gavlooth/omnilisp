# Fallback Inventory

Date: 2026-03-08
Owner scope: fallback cleanup tracks (TODO Track A-H/J)

## Classification legend
- `delete_now`: remove directly, then fix callers/tests.
- `replace_then_delete`: implement explicit replacement path first, then delete.
- `stage_bound`: keep temporarily only while a named TODO stage is in progress.

## Seeded fallback surfaces

| Surface | File / Evidence | Class | Status | Removal Gate |
|---|---|---|---|---|
| Removed deduce primitive aliases (`deduce-open`, `fact!`, `retract!`, `deduce-scan`, `deduce-query`, `deduce-count`, `deduce-match`) | removed from `src/lisp/eval_init_primitives.c3`; rejection coverage in `src/lisp/tests_deduce_groups.c3` | `replace_then_delete` | Done | Completed: alias registrations deleted and old names now error |
| Error-model wrappers (`raise->message`, `try-message`) | retired from `src/lisp/compiler_stdlib_prelude.c3` and `stdlib/stdlib.lisp`; migrated callsites in test/runtime fixtures | `replace_then_delete` | Done | Completed: wrappers removed and callsites migrated to explicit payload-aware handlers |
| Parser permissive unknown `#` dispatch fallback | `src/lisp/parser_lexer_string_hash.c3:155-163` | `delete_now` | Done | Completed: unknown `#` dispatch now hard lexer error with deterministic message |
| Compiler signature residue (`emit_lambda_return_with_frame(..., has_frame)`) | `src/lisp/compiler_code_emission.c3:101,222` | `delete_now` | Done | Completed: ignored arg deleted and callsites updated |
| JIT interpreter fallback | `src/lisp/jit_compile_effects_modules.c3:252-257`, `src/lisp/jit_compile_expr_core.c3:116-126`, `src/lisp/tests_tests.c3` fallback summary | `replace_then_delete` | Done | Completed: fallback no longer dispatches to interpreter; unhandled routes now hard `JIT_COMPILE_FAILED`; audit summary currently reports `suite=jit_fallback total=0` on full normal suite |
| Regex compiled->simple fallback path | `src/pika/regex_cache_api.c3` | `replace_then_delete` | Done | Completed: default regex API no longer silently falls back to simple engine; simple fallback helper APIs removed and tests migrated to compiled/public contract checks |
| Runtime ownership fallback family (`copy_to_parent` / scope scans) | `src/lisp/eval_promotion_copy.c3`, `src/lisp/eval_boundary_scope_chain.c3`, `scripts/check_boundary_facade_usage.sh` | `stage_bound` | Done | Completed: checked copy paths return typed faults; dead non-hinted scope-chain helpers, unchecked site-copy wrappers, and the unchecked plain `copy_to_parent(...)` convenience were removed; `copy_to_parent_by_route(...)` is internal-only; explicit target-scope lookup routes share the promotion-context cache; `scripts/check_boundary_facade_usage.sh` now blocks direct low-level copy/scope-chain bypasses outside sanctioned boundary implementation files. Global scope-chain scan deletion was invalidated by bounded smoke failures and is not part of the shipped closure. |
| Scope reset defensive TEMP baseline recreation | `src/scope_region_reset_helpers.c3:7-10,95-104` | `replace_then_delete` | Done | Completed: centralized baseline invariant helper + assertions; silent recreate branch deleted |
| Undeclared-effect behavior | `src/lisp/tests_advanced_io_effect_ffi_groups.c3:43`, `docs/EFFECTS_SEMANTICS.md` | `replace_then_delete` | Done | Completed: promoted to explicit canonical language rule (undeclared effects are valid and skip declaration-based type checks) |

## Immediate execution order
1. No seeded fallback surface in this inventory is currently open.
   Runtime ownership fallback closure is guard-backed by
   `scripts/check_boundary_facade_usage.sh`. Do not reopen the invalidated
   global scope-chain scan deletion path: the 2026-04-21 bounded smoke run with
   `OMNI_BOUNDARY_SCOPE_CHAIN_SCAN_BYPASS=1` failed reuse and classification
   invariants. Future runtime ownership work should be filed as a new concrete
   boundary item with a failing signal, not by reopening this umbrella.
