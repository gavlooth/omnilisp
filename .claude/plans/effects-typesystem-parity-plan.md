# Effects-First Error Model + Effects Docs + Julia-Parity Typesystem Plan

Status date: 2026-03-06
Owner: Codex session workflow
Mode: Plan only (no implementation in this document)

Legend:
- `[x]` done
- `[~]` partial / follow-up required
- `[ ]` pending

## Scope

- Standardize runtime/API failure semantics around effects (`raise`) as primary path.
- Fully document effect semantics and runtime boundaries.
- Reach explicit parity target with Julia-inspired type/method-dispatch behavior and document it.
- Add lint and tests so behavior does not drift.
- Top-priority syntax drift fix: module markers use quoted symbols (`'as`, `'all`), not colon-prefixed symbols.

---

## Priority Hotfix: Module Marker Syntax Drift

### Goals
- Canonicalize module marker syntax to quoted symbols (`'as`, `'all`) across parser, serializer, tests, and docs.
- Eliminate colon-marker drift (`:as`, `:all`) from normative examples and parser diagnostics.

### Checklist
- [x] H0.1 Parser accepts `'as`/`'all` module markers.
- [x] H0.2 Parser diagnostics reference `'as`/`'all`.
- [x] H0.3 Serializer emits `'as`/`'all`.
- [x] H0.4 Module-system tests updated to quoted markers.
- [~] H0.5 Remaining historical docs/plans swept for `:as`/`:all` references.

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
- [~] P2.5 Add compatibility wrappers where migration is user-visible.
- [x] P2.6 Add canonical error payload constructors/helpers in runtime.
- [~] P2.7 Normalize error domains/codes for: I/O, parser, regex, scheduler, deduce (regex malformed-pattern signaling + residual wrappers pending).
- [x] P2.8 Update docs/examples to contract.
- [x] P2.9 Add migration notes in `memory/CHANGELOG.md`.
- [~] P2.10 Add regression tests for each migrated API family.

### Progress Notes
- [x] N2.1 Runtime effect dispatcher/handler paths emit canonical `runtime/*` payload codes.
- [x] N2.2 Scheduler runtime paths (`offload`, `thread-*`, `spawn`/`await`, scheduler `tcp-read`) emit canonical `scheduler/*` payload codes.
- [x] N2.3 Core non-scheduler async I/O primitives (`tcp-*`, `dns-resolve`, `async-sleep`) emit canonical `io/*` payload codes.
- [x] N2.4 Deduce-family primitives (`open`/dispatch/relation/query/fact/retract/count/scan/match) emit canonical `deduce/*` payload codes.
- [x] N2.5 Parser-family primitives (`pika/grammar`, `pika/parse`, `pika/fold`, `pika/parse-lisp`, `pika/grammar-rules`, `pika/match-span`) emit canonical `parser/*` payload codes for invalid args/grammar failures.
- [~] N2.6 Regex-family primitives now emit canonical `regex/*` payload codes for invalid args; malformed-pattern signaling and residual network wrappers (for example TLS path) remain.

### Acceptance
- [~] A2.1 No mixed failure style in touched APIs.
- [x] A2.2 Migration matrix has owner + status per API family.
- [x] A2.3 Full suite green in normal and ASAN.

---

## Phase 3: Drift Prevention via Lint + CI Gates

### Goals
- Prevent future semantic drift automatically.

### Checklist
- [ ] P3.1 Define lint rules in `docs/PROJECT_TOOLING.md` for error-style consistency.
- [ ] P3.2 Add static checks for forbidden failure patterns in new stdlib primitives.
- [ ] P3.3 Add checks for required error payload fields.
- [ ] P3.4 Add checks for undocumented public primitives.
- [ ] P3.5 Add CI gate updates in `.github/workflows/boundary-hardening.yml`.
- [ ] P3.6 Emit machine-readable lint/test summaries.

### Acceptance
- [ ] A3.1 CI fails on contract drift.
- [ ] A3.2 Local command path matches CI behavior.

---

## Phase 4: Julia-Parity Typesystem/Dispatch Program

### Goals
- Turn “parity with Julia” into a concrete, testable matrix.

### Checklist
- [ ] P4.1 Add parity target matrix to `docs/type-system-syntax.md`.
- [ ] P4.2 Formalize method specificity ordering.
- [ ] P4.3 Formalize ambiguity detection and deterministic tie-break policy.
- [ ] P4.4 Specify parametric type behavior and variance policy.
- [ ] P4.5 Specify union participation in dispatch applicability.
- [ ] P4.6 Specify numeric promotion/conversion semantics for dispatch.
- [ ] P4.7 Specify `where`-style constraints or mark as explicit non-goal.
- [ ] P4.8 Specify match/union exhaustiveness expectations.
- [ ] P4.9 Add “Par with Julia / intentionally different” table.
- [ ] P4.10 Add per-row status tags: done/partial/missing.
- [ ] P4.11 Add regression tests for each done row.
- [ ] P4.12 Add expected-fail placeholders (TODO-ID linked) for missing rows.

### Acceptance
- [ ] A4.1 Parity matrix complete and linked to tests.
- [ ] A4.2 Ambiguous dispatch behavior deterministic and tested.
- [ ] A4.3 No undocumented dispatch edge-case in release-critical paths.

---

## Phase 5: Explainability Tooling for Advanced Semantics

### Goals
- Make effect and dispatch decisions inspectable.

### Checklist
- [ ] P5.0 Lock selector syntax: canonical forms are `(explain 'effect <form>)` and `(explain 'dispatch <form>)`.
- [ ] P5.1 Design `explain-dispatch` in `docs/LANGUAGE_SPEC.md`.
- [ ] P5.2 Design `explain-effect` in `docs/EFFECTS_SEMANTICS.md`.
- [ ] P5.3 Define structured output schema for both explainers.
- [ ] P5.4 Add stable-field tests (avoid brittle string dumps).
- [ ] P5.5 Add docs examples for “why this method/effect path was chosen.”

### Acceptance
- [ ] A5.1 Structured output deterministic.
- [ ] A5.2 At least 6 explainability regressions green.
- [ ] A5.3 Docs explicitly use symbol-selector syntax (`'effect`, `'dispatch`).

---

## Phase 6: Spec Cleanup + Onboarding Clarity

### Goals
- Keep unified syntax while reducing onboarding friction.

### Checklist
- [ ] P6.1 Add “Core Omni” profile in `docs/LANGUAGE_SPEC.md`.
- [ ] P6.2 Add “Advanced Omni” profile (effects/continuations/dispatch interplay).
- [ ] P6.3 Add “Error model quick reference” section.
- [ ] P6.4 Add ``define` forms catalog` (one-line intent + examples).
- [ ] P6.5 Add pitfalls page (`nil` vs raise, truthiness, resolve vs abort).
- [ ] P6.6 Update changelog with completion status for contract migration.

### Acceptance
- [ ] A6.1 New contributor can navigate core model without reading source first.
- [ ] A6.2 All advanced public primitives have behavior contracts documented.

---

## Execution Order (for implementing agent)

- [x] E0 Execute Priority Hotfix before all other phases.
- [x] E1 Complete Phase 0 before behavior edits.
- [~] E2 Execute Phases 1 and 2 together (spec + migration matrix + behavior changes).
- [ ] E3 Execute Phase 3 immediately after first migration batch.
- [ ] E4 Execute Phase 4 by matrix rows (small slices).
- [ ] E5 Execute Phase 5 once semantics and parity matrix are stable.
- [ ] E6 Execute Phase 6 as release-facing consolidation.

---

## Validation Checklist (run after each phase)

- [x] V1 `c3c build`
- [x] V2 `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- [x] V3 `c3c build --sanitize=address`
- [x] V4 `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- [x] V5 Ensure docs are updated for any behavior changes before phase close.

---

## Done Definition

- [ ] D1 Effects-first error contract is normative, documented, and lint-enforced.
- [ ] D2 Effect semantics are fully specified and test-anchored.
- [ ] D3 Julia-parity matrix is explicit, statused, and mostly green with tracked gaps.
- [ ] D4 CI prevents drift in error semantics and missing docs.
- [ ] D5 Onboarding docs clearly separate core vs advanced language model.
