# Omni Architecture Decisions

This document records architecture decisions that are normative for runtime and
language-surface behavior.

## ADR-2026-03-06-A: Effects-First Failure Contract

Status: Accepted (2026-03-06)

### Context

Omni currently exposes mixed failure styles across primitives and stdlib
wrappers (`nil`, `ERROR` values, and `signal raise`). This creates ambiguity
for users and drift risk for implementers.

### Decision

Public APIs MUST classify failures into exactly one of these classes:

1. `absence`
2. `recoverable-op-failure`
3. `programmer-error`
4. `internal-runtime-error`

### Failure Classes

| Class | Meaning | Example |
|------|---------|---------|
| `absence` | Data/query is valid, but no result exists | no regex match, key not present |
| `recoverable-op-failure` | Operation failed due to environment/input state and caller can choose fallback/retry | file not found, parse failure, network timeout |
| `programmer-error` | Call contract violated by caller or invalid user program shape | wrong argument type/shape, invalid API mode |
| `internal-runtime-error` | Runtime invariant violation or internal corruption risk | impossible tag state, invalid ownership boundary state |

### Mapping to Surface Behavior

| Class | Surface behavior | Notes |
|------|------------------|-------|
| `absence` | return `nil` (or `false` for predicate APIs) | MUST NOT use `raise` |
| `recoverable-op-failure` | `signal raise` with canonical payload | resumable/catchable at effect boundary |
| `programmer-error` | `signal raise` with canonical payload | deterministic error shape, catchable for tooling and REPL flows |
| `internal-runtime-error` | hard runtime error | non-resumable; do not downcast to `nil` or ordinary `raise` |

### Canonical Error Payload Schema

All `signal raise` payloads for runtime/stdlib APIs MUST use:

```lisp
{ 'code ... 'message ... 'domain ... 'data ... }
```

Omni has no dedicated keyword type. Use quoted symbols as dict keys.

Field requirements:

| Field | Type | Requirement |
|------|------|-------------|
| `'code` | symbol | stable machine code (for tests/routing) |
| `'message` | string | human-readable explanation |
| `'domain` | symbol | subsystem source (`io`, `parser`, `regex`, `scheduler`, `deduce`, `type`, `runtime`) |
| `'data` | dict or `nil` | structured context; no required keys globally |

### Examples

Correct (`recoverable-op-failure`):

```lisp
(signal raise
  {'code 'io/not-found
   'message "read-file: path not found"
   'domain 'io
   'data {'path "missing.txt"}})
```

Correct (`absence`):

```lisp
(find (lambda (x) (= x 999)) '(1 2 3))
; => nil
```

Correct (`programmer-error`):

```lisp
(signal raise
  {'code 'scheduler/invalid-handle
   'message "tcp-accept: expected listener handle"
   'domain 'scheduler
   'data {'api 'tcp-accept}})
```

### Counterexamples

Not allowed for new or migrated APIs:

```lisp
; Wrong: returns nil for operational failure (not absence)
(define (read-file* path)
  (if (file-exists? path) (__raw-read-file path) nil))

; Wrong: unstructured raise payload
(signal raise "oops")

; Wrong: wraps internal invariant break as recoverable effect
(signal raise {'code 'runtime/impossible-state 'message "..." 'domain 'runtime 'data nil})
```

### API Classification Baseline (2026-03-06)

This table classifies existing public APIs against this contract.

| API | Primary class | Contracted surface | Current style (2026-03-06) | Target |
|-----|---------------|--------------------|------------------------------|--------|
| `find` | `absence` | `nil` when not found | `nil` | keep |
| `assoc-ref` | `absence` | `nil` when key missing | `nil` | keep |
| `re-match` | `absence` + `recoverable-op-failure` | `nil` for no match; `raise` for invalid pattern | mixed (`nil` + implementation-specific failure) | normalize to contract |
| `read-file` | `recoverable-op-failure` | `raise` on I/O failure | mixed across call paths | migrate to canonical `raise` payload |
| `write-file` | `recoverable-op-failure` | `raise` on I/O failure | mixed across call paths | migrate to canonical `raise` payload |
| `pika/parse` | `recoverable-op-failure` | `raise` on malformed grammar/input | mixed (`ERROR`/raise patterns) | migrate to canonical `raise` payload |
| `deduce` actions (`'open`, `'query`, `'fact!`, ...) | `recoverable-op-failure` or `programmer-error` | `raise` for operation/config misuse | mixed (`ERROR` + raise) | migrate to canonical `raise` payload |
| `unsafe-free!` follow-on access | `programmer-error` | `raise` with deterministic payload | message-based error path | migrate to canonical `raise` payload |
