# Omni Effects Semantics (Normative)

Status: Normative
Last updated: 2026-03-09

This document is the normative reference for effect behavior in Omni.
`docs/EFFECTS_GUIDE.md` is tutorial-oriented. If there is a conflict, this
document wins.

For failure-class contracts and error payload policy, see
`docs/ARCHITECTURE.md` (ADR-2026-03-06-A).

## 1. Scope and Terms

Terms used here follow `docs/LANGUAGE_SPEC.md` Section 10.0 glossary:
`signal`, `raise`, `resolve`, `abort`, `resumable`, `effect boundary`.

## 2. Normative Rules

Each rule includes test anchors in Section 6.

### EFX-1: Handler Lookup and Matching

- The runtime MUST search handlers from nearest enclosing to outermost.
- A handler clause MUST match by exact effect tag symbol.
- If no clause matches in the nearest handler, search MUST continue outward
  unless blocked by `^strict`.
- `handle ^strict` MUST convert an otherwise-unhandled effect in its body into
  an immediate error at that boundary.

### EFX-2: `signal` Evaluation Order

- `signal` MUST evaluate its argument expression before handler dispatch.
- Type checking for declared effects MUST occur before dispatch.
- If an effect tag has no declaration, `signal` MUST remain valid and MUST skip
  declaration-based type checking.
- If no matching handler exists, runtime MAY use a registered fast-path
  primitive for known I/O-style effects.

### EFX-3: `resolve` Semantics

- `resolve` is valid only during active handler-clause execution.
- `resolve` MUST resume the suspended continuation at the signal site.
- A continuation resumed by `resolve` MUST be one-shot in normal handler flow:
  re-resuming the same continuation is an error.
- Multi-shot behavior MUST be explicit via `with-continuation`, not implicit
  repeated `resolve`.
- `with-continuation` is valid only inside active handler-clause execution.

### EFX-4: Abort Semantics

- If a handler clause returns without `resolve`, the enclosing `handle`
  expression MUST return that clause result.
- Remaining body work after the signal site MUST NOT execute on abort.

### EFX-5: Unhandled Effect Semantics

- Unhandled effects MUST fail deterministically.
- Diagnostic text MUST include the effect tag and argument type in the current
  runtime implementation path.
- Under `^strict`, error MUST be raised at strict boundary instead of falling
  through to fast path or outer unhandled diagnostics.

Note: error payload normalization to canonical
`{code ... message ... domain ... data ...}`
is tracked in `docs/ERROR_MODEL.md` and is not fully complete yet.

### EFX-6: `checkpoint` / `capture` Interaction

- `capture` outside `checkpoint` MUST error.
- `capture` continuations are a language-level multi-shot contract.
- Each invocation of a captured continuation MUST replay the resumed
  continuation segment from the capture site, including side effects in that
  segment, in source order.
- Each invocation MUST begin from the captured stack snapshot for that
  continuation segment. Mutations to lexical locals in the resumed segment do
  not implicitly persist across later invocations unless they target shared
  state outside the captured snapshot.
- Continuation capture/resume ordering MUST remain deterministic.
- Effects emitted inside `checkpoint`/`capture` flows MUST obey the same handler lookup
  and resolve/abort rules as non-continuation contexts.

### EFX-7: Async and Runtime Boundary Rules

- Worker/callback paths MUST enqueue wakeup/offload events; they MUST NOT run
  evaluator-level effect handlers directly from callback threads.
- Wakeup-drain and offload-completion paths MUST preserve interpreter boundary
  fields and deterministic ownership handoff invariants.
- Effect resumption MUST occur on the evaluator runtime path after queue drain,
  not inline in callback enqueue path.

### EFX-8: I/O and Scheduler Do/Don't

Do:
- Use effect tags (`io/*`) for I/O surface APIs.
- Keep callback-thread work enqueue-only and side-effect minimal.
- Keep ownership cleanup explicit on wakeup enqueue failure paths.

Don't:
- Don’t bypass handler lookup in user-visible I/O APIs.
- Don’t call handler code directly from worker/callback threads.
- Don’t mutate runtime boundary fields in wakeup/offload bookkeeping paths.

### EFX-9: Effect Explainability Surface

- Effect-path introspection MUST use the canonical selector syntax:
  - `(explain 'effect <form>)`
- `<form>` MUST be analyzed through the explain thunk boundary (no eager
  execution of the input form by explain itself).
- `explain 'effect` output MUST keep a deterministic top-level shape:
  - `kind`, `status`, `input`, `decision`, `candidates`, `trace`,
    `debug_message`
- `kind` MUST be `'effect`.
- For signal-path explain, `decision.reason` MUST classify the selected path:
  - `handler-match`
  - `strict-unhandled-effect`
  - `fast-path`
  - `unhandled-effect`
- For resolve-path explain, `decision.reason` MUST classify continuation state:
  - `resolve-effect-continuation`
  - `resolve-generic-continuation`
  - `resolve-invalid-continuation`
  - `resolve-dead-continuation`
  - `resolve-continuation-not-suspended`

## 3. Executable Examples (Mirrored by Tests)

These examples are intended to remain executable and linked to regression tests.

1. Basic resume:

```lisp
(handle (+ 1 (signal read nil))
  (read x (resolve 41)))
; => 42
```

2. Abort without resolve:

```lisp
(handle (+ 1 (signal bail 5))
  (bail x x))
; => 5
```

3. Multiple signals in one body:

```lisp
(handle (+ (signal bounce 10) (signal bounce 20))
  (bounce x (resolve x)))
; => 30
```

4. Resolve outside handler is invalid:

```lisp
(resolve 42)
; => error
```

5. `^strict` catches unhandled effects:

```lisp
(handle ^strict (signal my-eff 42)
  (other-eff v v))
; => error
```

6. Unhandled diagnostics include tag:

```lisp
(signal unknown-eff 42)
; => error contains "unknown-eff"
```

7. Unhandled diagnostics include arg type:

```lisp
(signal unknown-eff "hello")
; => error contains "String"
```

8. I/O handler suppression:

```lisp
(handle (block (println "suppressed") 42)
  (io/println x (resolve nil)))
; => 42
```

9. I/O handler capture:

```lisp
(handle (block (println "captured") nil)
  (io/println x x))
; => "captured"
```

10. Explicit multi-shot with continuation:

```lisp
(handle (+ 1 (signal choose 0))
  (choose x
    (with-continuation k (+ (k 10) (k 20)))))
; => 32
```

11. Explain effect dispatch path:

```lisp
(ref (ref (explain 'effect (signal io/print "x")) 'decision) 'reason)
; => 'fast-path
```

12. Explain resolve validity path:

```lisp
(ref (ref (explain 'effect (resolve 42)) 'decision) 'reason)
; => 'resolve-invalid-continuation
```

13. Multi-shot continuation replay:

```lisp
(checkpoint (+ 1 (capture k (+ (k 10) (k 20)))))
; => 32
```

## 4. Runtime Notes

- `with-continuation` desugars to binding the hidden continuation (`__k`) in
  handler scope and is the explicit multi-shot escape hatch.
- Parser surface rule: `with-continuation` is rejected outside handler-clause
  context.
- Fast-path dispatch is an optimization for selected effect tags and does not
  change observable handler precedence semantics.

## 5. Relationship to Error Model

- `raise` is the conventional effect for recoverable/programmer-facing failures.
- `nil` remains reserved for intentional absence semantics.
- Internal runtime invariant failures remain hard runtime errors.

See `docs/ARCHITECTURE.md` and `docs/ERROR_MODEL.md` for migration status.

## 6. Rule-to-Test Anchors

| Rule | Regression anchors |
|------|--------------------|
| EFX-1 | `src/lisp/tests_advanced_core_unicode_groups.c3` `signal/resolve basic` (`run_advanced_effect_continuation_tests`), `src/lisp/tests_tests.c3` `handle ^strict: catches unhandled` |
| EFX-2 | `src/lisp/tests_advanced_io_effect_ffi_groups.c3` `effect wrong type int`, `effect wrong type str`, `effect undeclared canonical`, `io/read-file wrong type` (`run_advanced_io_typed_effect_tests`) |
| EFX-3 | `src/lisp/tests_advanced_core_unicode_groups.c3` `resolve outside handler`; `with-continuation basic`; `with-continuation single` |
| EFX-4 | `src/lisp/tests_advanced_core_unicode_groups.c3` `signal abort`; `multi-perform abort` (legacy test name covering multi-signal abort semantics) |
| EFX-5 | `src/lisp/tests_tests.c3` `unhandled effect: shows tag name`; `unhandled effect: shows arg type`; `handle ^strict: catches unhandled` |
| EFX-6 | `src/lisp/tests_tests.c3` `capture aborts`; `capture k resumes`; `checkpoint passthrough`; `src/lisp/tests_advanced_core_unicode_groups.c3` `multi-capture sum`, `multi-shot k twice`, `multi-shot replay set! in resumed segment`, `multi-shot replay deduce fact writes`, `multi-shot replay handled io effect` |
| EFX-7 | `src/lisp/tests_tests.c3` `run_scheduler_wakeup_wraparound_boundary_tests`, `run_scheduler_wakeup_mixed_event_boundary_tests`, `run_scheduler_invalid_offload_wakeup_boundary_tests`, `run_scheduler_wakeup_full_payload_ownership_boundary_tests` |
| EFX-8 | `src/lisp/tests_advanced_io_effect_ffi_groups.c3` `io handle suppress`, `io handle capture`; `src/lisp/tests_tests.c3` scheduler boundary test group (`run_scheduler_tests`) |
| EFX-9 | `src/lisp/tests_runtime_feature_schema_reader_groups.c3` explain-effect regressions: `explain effect handler match reason`, `explain effect strict boundary reason`, `explain effect fast-path reason`, `explain effect unhandled reason`, `explain effect resolve invalid continuation reason`, `explain effect resolve continuation reason`, `explain effect top-level schema keys` |
