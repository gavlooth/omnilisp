# Deep Audit Findings - 2026-04-23

Date: 2026-04-23 17:09:06 CEST
Scope: broad codebase audit with emphasis on compiler, pattern-matching, serialization, and runtime edge cases.

## Findings

1. `src/lisp/compiler_expr_serialize_patterns.c3:8-75` does not handle `PAT_GUARD` at all.
   Guarded patterns are serialized through the catch-all `_` path, which loses the guard predicate and any nested subpattern semantics.
   That makes the serializer lie about the source form for any guarded `match` clause and breaks round-trip fidelity for that surface.

2. `src/lisp/tests_advanced_core_semantics_groups.c3:27-45`, `src/lisp/tests_compiler_codegen_groups.c3:103-129`, and `src/lisp/tests_compiler_core_groups.c3:18-56` do not cover the nested `PAT_GUARD` and guarded-pattern serialization edges that the recent compiler changes depend on.
   The current regression set proves basic guarded matching and single-level codegen, but it does not pin the nested guard-sub scope rule or the serializer round-trip for guarded clauses.
   That leaves the exact edge touched by the audit vulnerable to silent regression.

## Validation

- `c3c build --obj-out obj`

## Recommendation

Add an explicit guarded-pattern branch to `serialize_pattern_to_buf`, then add a round-trip test that serializes and reparses a guarded `match` clause, plus one nested-guard regression for the scope rule.

Signature: GPT-5 Codex
