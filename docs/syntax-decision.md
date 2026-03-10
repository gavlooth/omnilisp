# Syntax Decision Notes

## Scope

This document records canonical syntax naming decisions that were finalized during the 2026 syntax surface cleanup.
It is a short reference for contributors to avoid reintroducing removed aliases or ambiguous forms.

## Canonical Syntax Choices

### 1. Functions

- Canonical function-expression form is `lambda`.
- Shorthand function declarations use `define` with direct function syntax:

```lisp
(define (udp-open host port) ...)
```

- `fn` is deprecated and removed; canonical errors instruct migration to `lambda`.

### 2. Sequencing

- Canonical sequencing form is `block`.
- `begin` remains parser-reserved only for compatibility checks and is not a first-class public sequencing form.
- `do` is not part of the canonical surface.

### 3. Effect Handlers

- Canonical handler form is `handle`, with explicit clause shape `(tag arg body)`.
- Multi-shot handler continuations are explicit with `with-continuation`.
- `with-handlers` helper indirection in stdlib was removed in favor of explicit composition utilities.
- Public examples and docs should use canonical `handle` semantics and `resolve`/`perform` primitives as the control surface.

### 4. Transaction Commands (`deduce`)

- Canonical transaction command is:

```lisp
(deduce 'block db ['read|'write])
```

- Legacy transaction helper names were removed from parser and runtime surfaces.

### 5. Value Constructors for Dispatch

- Canonical constructor for value dispatch literals is `Value`.
- `^(Value ...)` is used for value-level dispatch in method signatures.
- Legacy `Val` spelling is rejected with explicit migration diagnostics.

### 6. Names Used in Helper Composition

- Canonical handler-composition helper naming should prefer readable terms and avoid abbreviations:
  - `handle-chain` for chain composition order,
  - explicit `with-continuation` for multi-shot control.

## Migration Principle

Do not reintroduce removed aliases in docs, examples, or stdlib surfaces.
If a source of truth entry is updated to a non-canonical spelling, update this page at the same time.
