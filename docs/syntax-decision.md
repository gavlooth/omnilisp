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

- `fn` is removed; canonical errors point to `lambda`.

### 2. Sequencing

- Canonical sequencing form is `block`.
- `begin` remains parser-reserved only for migration checks and is not a first-class public sequencing form.
- `do` is not part of the canonical surface.

### 3. Local Binding

- Canonical `let` bindings use flat pairs:

```lisp
(let (x 1 y 2) ...)
(let ^rec (fact (lambda (n) ...)) ...)
(let loop (i 0 acc nil) ...)
```

- `let` bindings are sequential left-to-right; later initializers may reference earlier bindings.
- Named `let` uses the same sequential initializer rule before entering the recursive loop body.

- Scheme-style grouped binding forms are removed:
  - `(let ((x 1) (y 2)) ...)`
  - `(let loop ((i 0) (acc nil)) ...)`
  - `(letrec ...)`
- Bracket single-binding shorthand such as `(let [x 10] ...)` is not part of the canonical surface.

### 4. Effect Handlers

- Canonical handler form is `handle`, with explicit clause shape `(tag arg body)`.
- Multi-shot handler continuations are explicit with `with-continuation`.
- Implicit continuation capture in handler clauses is removed; nested clause forms such as `((tag k arg) body)` must hard-fail.
- `with-handlers` helper indirection in stdlib was removed in favor of explicit composition utilities.
- Public examples and docs should use canonical `handle` semantics and `resolve`/`signal` primitives as the control surface.

### 4.1 Module-Qualified Effect Names

- When an effect belongs to a real public module, use dot-qualified module
  access consistently for the effect symbol as well.
- Example future UI surface:
  - `ui.text`, `ui.window` for constructors/helpers
  - `ui.open`, `ui.render`, `ui.close` for effect tags exported by the `ui`
    module
- Established slash-form families like `io/print` remain canonical existing
  surface, but they do not define the naming rule for new real module-owned
  APIs.

### 5. Transaction Commands (`deduce`)

- Canonical transaction command is:

```lisp
(deduce 'block db ['read|'write|'write-deferred])
```

- Legacy transaction helper names were removed from parser and runtime surfaces.

### 6. Value Constructors for Dispatch

- Canonical constructor for value dispatch literals is `Value`.
- `^(Value ...)` is used for value-level dispatch in method signatures.
- Legacy `Val` spelling is rejected with explicit migration diagnostics.

### 7. Names Used in Helper Composition

- Canonical handler-composition helper naming should prefer readable terms and avoid abbreviations:
  - `handle/chain` for chain composition order,
  - explicit `with-continuation` for multi-shot control.
- Migration note:
  - historical helper spellings `with-handlers` and `handle-chain` should be
    treated as non-canonical in public-facing examples/docs.

### 8. Macro Surface Model (Locked 2026-03-11)

Macro syntax is locked to one canonical model and should not drift toward
function-style overloading semantics.

- Macros have one definition form only: one macro name maps to one transformer.
- Macros are syntax transformers, not overloaded callable sets.
- Function overloading remains function-only.
- Legacy multi-clause macro definitions are removed; no new multi-clause macro
  surface should be added.

Canonical replacement model:

- `syntax-match` is the branching primitive inside macro transformers.
- `template` is the emitted syntax construction primitive.
- Interpolation is word-based and explicit:
  - `(insert x)` inserts one captured syntax value.
  - `(splice xs)` splices captured syntax sequences.
- Punctuation-heavy reader syntax (backtick/comma/comma-at) is not the primary
  public macro authoring model.

Explicit non-goals for this migration:

- No new macro-only clause-definition DSL.
- No attempt to make macros dispatch like runtime functions.
- No reader-level punctuation sugar as required surface syntax; optional sugar,
  if introduced later, must layer over the canonical word-based model.

Contributor rules for new code:

- Do not add new multi-clause macro definition surfaces.
- Prefer functions unless syntax control is required.
- Prefer explicit word-based macro notation over punctuation-heavy sugar.

Migration migration choice:

- Legacy multi-clause macro parse path is removed.
- Legacy clause-style forms fail fast with deterministic migration diagnostics
  that point to `(define [macro] name (syntax-match ...))`.

Migration examples:

```lisp
;; Before (removed)
(define [macro] when
  ([test .. body] (if test (block .. body) nil)))

;; After (canonical)
(define [macro] when
  (syntax-match
    ([test .. body]
      (template (if (insert test) (block (splice body)) nil)))))
```

Use a function instead when syntax control is not required:

```lisp
;; Prefer function for plain runtime delegation
(define (default v fallback)
  (if (null? v) fallback v))
```

Template readability rules:

- Keep templates shallow; split helpers into functions for deep nesting.
- Prefer explicit names in patterns (`test`, `body`, `rest`) over short symbols.
- Use `insert`/`splice` at the exact emission site; avoid punctuation-heavy sugar.

### 9. Macro Syntax Contract (Locked 2026-03-11)

This section locks the transformer input model and matching semantics for the
single-transformer macro surface.

#### 9.1 Transformer Input

- Macro transformers operate on syntax values represented in the existing
  parsed datum/value shape.
- The transformer receives:
  - `head`: macro name symbol.
  - `args`: argument syntax list (raw call arguments).
  - `form`: full call form list `(head ..args)`.
- Collection syntax representation in macro input:
  - symbols and literals remain scalar syntax values,
  - list syntax is cons/list representation,
  - array syntax is normalized as `(Array ...)`,
  - dict syntax is normalized as `(Dictionary k1 v1 ...)`.
- Input is raw parsed syntax with deterministic normalization for collection
  literals only (array/dict internal constructor forms as above).

#### 9.2 `syntax-match` Semantics

- `_` is wildcard and does not bind.
- Symbol/keyword literal match requires exact symbol equality.
- List patterns destructure by position.
- `..` captures the rest of a list segment.
- Nested list patterns are allowed and recurse structurally.
- Malformed patterns fail deterministically with diagnostics that identify the
  malformed pattern shape (for example invalid `..` position, duplicate rest,
  or illegal non-list rest target).

#### 9.3 Template and Hygiene Expectations

- `template` emits syntax values; it does not evaluate emitted forms.
- `(insert x)` inserts one captured syntax binding.
- `(splice xs)` splices a captured syntax sequence into list context only.
- Inserted captured syntax preserves source identity/locations when carried by
  current syntax values.
- Template-introduced identifiers follow definition-site hygiene behavior:
  literal template identifiers resolve against macro definition capture rather
  than expansion-site rebinding.
- Diagnostics should identify whether a problematic identifier came from:
  - user-supplied syntax (captured/inserted), or
  - macro-introduced template syntax.

## Migration Principle

Do not reintroduce removed aliases in docs, examples, or stdlib surfaces.
If a source of truth entry is updated to a non-canonical spelling, update this page at the same time.
