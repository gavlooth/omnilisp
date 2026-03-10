# Effects-First Error Model + Effects Docs + Julia-Parity Typesystem Plan

Status date: 2026-03-09
Owner: Codex session workflow
Mode: Plan only (no implementation in this document)

Legend:
- `[x]` done
- `[~]` partial / follow-up required
- `[ ]` pending

Canonical tracking rule:
- Use **`Canonical Partial Backlog`** below as the only checklist for partial items.
- Existing `[~]` markers in phase sections remain as status hints only.

## Scope

- Standardize runtime/API failure semantics around effects (`raise`) as primary path.
- Fully document effect semantics and runtime boundaries.
- Reach explicit parity target with Julia-inspired type/method-dispatch behavior and document it.
- Add lint and tests so behavior does not drift.
- Top-priority syntax drift fix: module markers use quoted symbols (`'as`, `'all`), not colon-prefixed symbols.

---

## Canonical Partial Backlog (Single Source Of Truth)

- [x] `CP-01` Sweep remaining historical docs/plans for legacy module markers (colon-prefixed alias/export markers), keeping only quoted markers (`'as`, `'all`).
  - Covers: `H0.5`
- [x] `CP-02` Finish stdlib error-surface normalization for `try`/`assert!` payload consistency.
  - Covers: `P2.5` (compatibility/user-visible migration closure), `docs/ERROR_MODEL.md` row `Stdlib try/assert! base`
- [x] `CP-03` Finalize regex malformed-pattern signaling and complete no-match-vs-error contract.
  - Covers: `N2.6`, `docs/ERROR_MODEL.md` row `Regex match/search primitives`
- [x] `CP-04` Normalize canonical error payload shape for file-I/O effect wrappers.
  - Covers: `docs/ERROR_MODEL.md` row `File I/O effect wrappers`
- [x] `CP-05` Normalize residual async/network wrapper paths (including TLS/network wrappers) to canonical payload policy.
  - Covers: `P2.7`, `docs/ERROR_MODEL.md` row `Async/network primitives`
- [x] `CP-06` Remove remaining mixed message-string error paths in effect dispatcher internals.
  - Covers: `P2.7`, `docs/ERROR_MODEL.md` row `Effect dispatcher internals`
- [x] `CP-07` Add regression coverage for every migrated error-model API family.
  - Covers: `P2.10`
- [x] `CP-08` Close acceptance gap: no mixed failure style in touched APIs.
  - Covers: `A2.1`
- [x] `CP-09` Close execution-order drift item for combined Phase 1/2 migration sequencing.
  - Covers: `E2`
- [x] `CP-10` Add deterministic ambiguity regression coverage for equal-specificity method matches.
  - Covers: `docs/type-system-syntax.md` partial row `Ambiguity handling`
- [x] `CP-11` Specify and test parametric substitution/unification semantics (beyond constructor inference).
  - Covers: `docs/type-system-syntax.md` partial row `Parametric type behavior`
- [x] `CP-12` Expand and test union participation in dispatch applicability edge cases.
  - Covers: `docs/type-system-syntax.md` partial row `Union participation in dispatch applicability`
- [x] `CP-13` Define compile-time exhaustiveness policy (or explicit non-goal) for match/union and add test anchors.
  - Covers: `docs/type-system-syntax.md` partial row `Match/union exhaustiveness`

---

## Priority Hotfix: Module Marker Syntax Drift

### Goals
- Canonicalize module marker syntax to quoted symbols (`'as`, `'all`) across parser, serializer, tests, and docs.
- Eliminate colon-marker drift (colon-prefixed alias/export markers) from normative examples and parser diagnostics.

### Checklist
- [x] H0.1 Parser accepts `'as`/`'all` module markers.
- [x] H0.2 Parser diagnostics reference `'as`/`'all`.
- [x] H0.3 Serializer emits `'as`/`'all`.
- [x] H0.4 Module-system tests updated to quoted markers.
- [x] H0.5 Remaining historical docs/plans swept for quoted-marker-only references. (`CP-01`)

---

## Phase 0: Freeze Contracts Before Coding

### Goals
- Lock semantics contracts before behavioral edits.
- Avoid coding drift across agents.

### Checklist
- [x] P0.1 Write ADR: `Effects-First Failure Contract` in `docs/ARCHITECTURE.md`.
- [x] P0.2 Write ADR: ``define` unification is canonical syntax` in `docs/LANGUAGE_SPEC.md`.
- [x] P0.3 Define failure classes: `absence`, `recoverable-op-failure`, `programmer-error`, `internal-runtime-error`.
- [x] P0.4 Define class mapping to `nil` vs `signal raise` vs hard runtime error.
- [x] P0.5 Define canonical error payload schema: `{ 'code 'message 'domain 'data }`.
- [x] P0.6 Add glossary entries for: `signal`, `raise`, `resolve`, `abort`, `resumable`, `effect boundary`.

### Acceptance
- [x] A0.1 Contracts merged in docs with examples and counterexamples.
- [x] A0.2 At least 5 existing APIs classified against contract in docs.

---

## Phase 1: Effects Semantics Documentation (Normative)

### Goals
- Make effect semantics explicit and test-linked.
- Document runtime boundaries (scheduler/thread/callback constraints).

### Checklist
- [x] P1.1 Create `docs/EFFECTS_SEMANTICS.md` as normative reference.
- [x] P1.2 Document handler lookup rules (nearest enclosing handler, tag matching).
- [x] P1.3 Document `resolve` semantics (single/multi-resume policy).
- [x] P1.4 Document abort semantics when `resolve` is not called.
- [x] P1.5 Document unhandled effect semantics and error shape.
- [x] P1.6 Document `reset/shift` interaction and ordering guarantees.
- [x] P1.7 Document async/runtime boundary rules (enqueue-only callbacks, main-thread resume).
- [x] P1.8 Add “Do/Don’t” section for effect handlers in I/O and scheduler paths.
- [x] P1.9 Cross-link from `docs/EFFECTS_GUIDE.md` and `docs/LANGUAGE_SPEC.md`.
- [x] P1.10 Add 10 executable examples mirrored by tests.

### Acceptance
- [x] A1.1 Doc uses explicit MUST/SHOULD semantics.
- [x] A1.2 Each normative rule references at least one regression test.

---

## Phase 2: Effects-First Error Model Migration

### Goals
- Converge public API failures toward effects-first.
- Keep `nil` only for intentional absence semantics.

### Checklist
- [x] P2.1 Inventory stdlib/runtime APIs by current failure style (`nil`, `ERROR`, `raise`, mixed).
- [x] P2.2 Create migration matrix in `docs/ERROR_MODEL.md`.
- [x] P2.3 Mark APIs that intentionally remain `nil`-returning (absence only).
- [x] P2.4 Mark APIs that must migrate to `signal raise`.
- [x] P2.5 Add compatibility wrappers where migration is user-visible. (`CP-02`)
- [x] P2.6 Add canonical error payload constructors/helpers in runtime.
- [x] P2.7 Normalize error domains/codes for: I/O, parser, regex, scheduler, deduce.
- [x] P2.8 Update docs/examples to contract.
- [x] P2.9 Add migration notes in `memory/CHANGELOG.md`.
- [x] P2.10 Add regression tests for each migrated API family.

### Progress Notes
- [x] N2.1 Runtime effect dispatcher/handler paths emit canonical `runtime/*` payload codes.
- [x] N2.2 Scheduler runtime paths (`offload`, `thread-*`, `spawn`/`await`, scheduler `tcp-read`) emit canonical `scheduler/*` payload codes.
- [x] N2.3 Core non-scheduler async I/O primitives (`tcp-*`, `dns-resolve`, `async-sleep`) emit canonical `io/*` payload codes.
- [x] N2.4 Deduce-family primitives (`open`/dispatch/relation/query/fact/retract/count/scan/match) emit canonical `deduce/*` payload codes.
- [x] N2.5 Parser-family primitives (`pika/grammar`, `pika/parse`, `pika/fold`, `pika/parse-lisp`, `pika/grammar-rules`, `pika/match-span`) emit canonical `parser/*` payload codes for invalid args/grammar failures.
- [x] N2.6 Regex-family primitives emit canonical `regex/*` payload codes for invalid args and malformed patterns, while preserving `nil` for valid no-match paths. (`CP-03`)

### Acceptance
- [x] A2.1 No mixed failure style in touched APIs.
- [x] A2.2 Migration matrix has owner + status per API family.
- [x] A2.3 Full suite green in normal and ASAN.

---

## Phase 3: Drift Prevention via Lint + CI Gates

### Goals
- Prevent future semantic drift automatically.

### Checklist
- [x] P3.1 Define lint rules in `docs/PROJECT_TOOLING.md` for error-style consistency.
- [x] P3.2 Add static checks for forbidden failure patterns in new stdlib primitives.
- [x] P3.3 Add checks for required error payload fields.
- [x] P3.4 Add checks for undocumented public primitives.
- [x] P3.5 Add CI gate updates in `.github/workflows/boundary-hardening.yml`.
- [x] P3.6 Emit machine-readable lint/test summaries.

### Acceptance
- [x] A3.1 CI fails on contract drift.
- [x] A3.2 Local command path matches CI behavior.

---

## Phase 4: Julia-Parity Typesystem/Dispatch Program

### Goals
- Turn “parity with Julia” into a concrete, testable matrix.

### Checklist
- [x] P4.1 Add parity target matrix to `docs/type-system-syntax.md`.
- [x] P4.2 Formalize method specificity ordering.
- [x] P4.3 Formalize ambiguity detection and deterministic tie-break policy.
- [x] P4.4 Specify parametric type behavior and variance policy.
- [x] P4.5 Specify union participation in dispatch applicability.
- [x] P4.6 Specify numeric promotion/conversion semantics for dispatch.
- [x] P4.7 Specify `where`-style constraints or mark as explicit non-goal.
- [x] P4.8 Specify match/union exhaustiveness expectations.
- [x] P4.9 Add “Par with Julia / intentionally different” table.
- [x] P4.10 Add per-row status tags: done/partial/missing.
- [x] P4.11 Add regression tests for each done row.
- [x] P4.12 Add expected-fail placeholders (TODO-ID linked) for missing rows. (No remaining `missing` rows in current matrix.)

### Acceptance
- [x] A4.1 Parity matrix complete and linked to tests.
- [x] A4.2 Ambiguous dispatch behavior deterministic and tested.
- [x] A4.3 No undocumented dispatch edge-case in release-critical paths.

---

## Phase 5: Explainability Tooling for Advanced Semantics

### Goals
- Make effect and dispatch decisions inspectable.

### Checklist
- [x] P5.0 Lock selector syntax: canonical forms are `(explain 'effect <form>)` and `(explain 'dispatch <form>)`.
- [x] P5.1 Design `explain-dispatch` in `docs/LANGUAGE_SPEC.md`.
- [x] P5.2 Design `explain-effect` in `docs/EFFECTS_SEMANTICS.md`.
- [x] P5.3 Define structured output schema for both explainers.
- [x] P5.4 Add stable-field tests (avoid brittle string dumps).
- [x] P5.5 Add docs examples for “why this method/effect path was chosen.”

### Acceptance
- [x] A5.1 Structured output deterministic.
- [x] A5.2 At least 6 explainability regressions green.
- [x] A5.3 Docs explicitly use symbol-selector syntax (`'effect`, `'dispatch`).

---

## Phase 6: Spec Cleanup + Onboarding Clarity

### Goals
- Keep unified syntax while reducing onboarding friction.

### Checklist
- [x] P6.1 Add “Core Omni” profile in `docs/LANGUAGE_SPEC.md`.
- [x] P6.2 Add “Advanced Omni” profile (effects/continuations/dispatch interplay).
- [x] P6.3 Add “Error model quick reference” section.
- [x] P6.4 Add ``define` forms catalog` (one-line intent + examples).
- [x] P6.5 Add pitfalls page (`nil` vs raise, truthiness, resolve vs abort).
- [x] P6.6 Update changelog with completion status for contract migration.

### Acceptance
- [x] A6.1 New contributor can navigate core model without reading source first.
- [x] A6.2 All advanced public primitives have behavior contracts documented.

---

## Execution Order (for implementing agent)

- [x] E0 Execute Priority Hotfix before all other phases.
- [x] E1 Complete Phase 0 before behavior edits.
- [x] E2 Execute Phases 1 and 2 together (spec + migration matrix + behavior changes).
- [x] E3 Execute Phase 3 immediately after first migration batch.
- [x] E4 Execute Phase 4 by matrix rows (small slices).
- [x] E5 Execute Phase 5 once semantics and parity matrix are stable.
- [x] E6 Execute Phase 6 as release-facing consolidation.

---

## Validation Checklist (run after each phase)

- [x] V1 `c3c build`
- [x] V2 `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- [x] V3 `c3c build --sanitize=address`
- [x] V4 `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- [x] V5 Ensure docs are updated for any behavior changes before phase close.

---

## Done Definition

- [x] D1 Effects-first error contract is normative, documented, and lint-enforced.
  - Evidence: `docs/ARCHITECTURE.md` (ADR-2026-03-06-A), `docs/ERROR_MODEL.md`, `scripts/run_effects_contract_lint.sh`, `.github/workflows/boundary-hardening.yml`.
- [x] D2 Effect semantics are fully specified and test-anchored.
  - Evidence: `docs/EFFECTS_SEMANTICS.md`, `docs/EFFECTS_GUIDE.md`, `src/lisp/tests_runtime_feature_schema_reader_groups.c3`.
- [x] D3 Julia-parity matrix is explicit, statused, and mostly green with tracked gaps.
  - Evidence: `docs/type-system-syntax.md` parity matrix rows + status tags, `src/lisp/tests_compiler_core_groups.c3`, `src/lisp/tests_e2e_generation_cases_extended.c3`.
- [x] D4 CI prevents drift in error semantics and missing docs.
  - Evidence: `.github/workflows/boundary-hardening.yml`, `scripts/check_effects_contract_policy.sh`, `scripts/check_primitive_docs_parity.sh`.
- [x] D5 Onboarding docs clearly separate core vs advanced language model.
  - Evidence: `docs/LANGUAGE_SPEC.md` sections `0. Core Omni Profile` and `0.4 Advanced Omni Profile`.
