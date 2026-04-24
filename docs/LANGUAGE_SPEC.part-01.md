# Omni Lisp Language Specification

**Version:** 0.4.7
**Date:** 2026-04-24

Omni Lisp is a Lisp dialect with first-class delimited continuations, algebraic effects, strict-arity multi-param lambdas, multiple dispatch, and a structural type system. It runs on a deterministic scope-region memory system with dual-lane TEMP/ESCAPE ownership, implemented in C3 with a GNU Lightning JIT engine and a Lisp-to-C3 AOT transpiler.

Normative architecture contracts are recorded in `docs/ARCHITECTURE.md`.
Documentation authority and cross-doc coverage mapping are defined in
`docs/DOCS_CONTRACT.md`.
Removed/renamed surface syntax is centralized in
`docs/SURFACE_COMPATIBILITY.md`.

---

## Table of Contents

0. [Core Omni Profile](#0-core-omni-profile)
1. [Syntax Overview](#1-syntax-overview)
2. [Data Types](#2-data-types)
3. [Special Forms](#3-special-forms)
4. [Type System](#4-type-system)
5. [Multiple Dispatch](#5-multiple-dispatch)
6. [Path and Index Notation](#6-path-and-index-notation)
7. [Primitives](#7-primitives)
8. [Standard Library](#8-standard-library)
9. [Delimited Continuations](#9-delimited-continuations)
10. [Effect Handlers](#10-effect-handlers)
11. [Macros](#11-macros)
12. [Modules](#12-modules)
13. [REPL](#13-repl)
14. [Examples](#14-examples)
15. [CLI & Project Tooling](#15-cli--project-tooling)

---

## 0. Core Omni Profile

This profile is the shortest practical mental model for getting productive in
Omni without learning advanced semantics first.

### 0.1 Minimum Mental Model

- Evaluation is expression-based: forms evaluate to values, function calls are
  written as `(f arg1 arg2 ...)`.
- Function arity is strict for user lambdas and function definitions; there is
  no implicit partial application.
- Truthiness rule is minimal:
  - falsy: `nil`, `false`
  - truthy: everything else (`0`, `""`, empty collections, symbols, etc.)
- Start with these value families:
  - scalars: int, Float64, string, symbol, nil
  - functions: closures (`λ`, with `lambda` as a long alias)
  - collections: list, array, dict
- Function spelling: `λ` is canonical for input and output; `lambda` remains
  accepted as the long accessibility alias.
- Prefer generic collection operations (`length`, `ref`, `map`, `filter`,
  `foldl`) instead of type-specific naming.
- Surface naming policy for contributors:
  - prefer one canonical language-facing name per concept,
  - prefer descriptive non-abbreviated canonical names,
  - do not preserve non-canonical alternates by default during pre-alpha cleanup,
  - only keep shorthand spellings when explicitly approved.

### 0.2 First-Steps Command Set

Run these in REPL first:

```lisp
(define x 10)
(+ x 5)

(let (a 3 b 4) (+ a b))
(if (> x 5) "big" "small")

(define (inc n) (+ n 1))
(inc 41)

'(1 2 3)                   ; list
[1 2 3]                    ; array
{name "omni" year 2026}    ; dict; bare symbol keys auto-quote

(length [1 2 3])
(ref {a 10 b 20} 'b)
```

Core forms to learn first:
- binding/flow: `define`, `let`, `if`, `block`
- functions: `λ`, strict arity calls
- collections: list/array/dict literals + generic ops (`length`, `ref`, `map`)

### 0.3 What To Ignore Initially

You can safely postpone these until the core model above feels routine:

- Effect handlers (`signal`, `handle`, `resolve`) and strict boundaries.
- Delimited continuations (`checkpoint`, `capture`, `with-continuation`).
- Typed/multi-method dispatch details (`^Type`, value-level dispatch scoring).
- Macro authoring and expansion internals.
- FFI/modules/scheduler runtime details.
- Runtime ownership and boundary internals (region lanes, promotion paths).

### 0.4 Advanced Omni Profile

Use this profile after the core model is comfortable and you need predictable
behavior in larger systems.

#### 0.4.1 Effects and Continuations: Model Boundaries

- Effects (`signal`/`handle`/`resolve`) and continuations (`checkpoint`/`capture`) are
  separate control abstractions that can compose, but they should be reasoned
  about at explicit boundaries.
- Effect handlers are nearest-first by dynamic scope; `resolve` resumes a
  captured continuation at the signal site, while returning without `resolve`
  aborts the remaining body at that site.
- Continuation operators control evaluation flow; handler clauses should stay
  explicit about whether they resume (`resolve`) or terminate (`abort`) the
  local effect path.

#### 0.4.2 Multiple Dispatch and Typed Annotations

- Type annotations (`^Type`, `^(Value ...)`) define method applicability and
  specificity, not just documentation.
- Dispatch picks the highest-scoring applicable method; equal-best results are
  ambiguous and rejected.
- Value-literal dispatch (`^(Value ...)`) is the most specific tier and is
  useful for command-style APIs, while untyped parameters remain the broad
  fallback.
- Handler bodies can delegate type-specific behavior to dispatched functions
  instead of duplicating ad-hoc type branching.

#### 0.4.3 Runtime Ownership and Boundary Constraints (User-Facing)

- Omni values follow deterministic scope/region lifetime rules; values crossing
  boundaries may be copied/promoted, and this is part of correctness rather
  than an optimization detail.
- User code should treat boundary transitions (function return, handler
  resumption, callback/wakeup handoff) as ownership-sensitive points where
  invalid reuse assumptions can break behavior.
- Prefer explicit, boundary-safe composition patterns:
  - return plain values rather than retaining references to transient internals,
  - keep effect handlers/callbacks narrow in responsibility,
  - avoid designs that depend on implicit lifetime extension.

### 0.5 Error Model Quick Reference

Omni uses effects-first failure signaling for public API surfaces.

#### 0.5.1 Failure Class Mapping

| Failure class | Meaning | Surface behavior |
|---|---|---|
| `absence` | Query/data is valid but no result exists | return `nil` (or `false` for predicate APIs) |
| `recoverable-op-failure` | Operation failed due to input/environment/runtime state and caller can fallback/retry | `signal raise` with canonical payload |
| `programmer-error` | Call contract violated (shape/type/mode misuse) | `signal raise` with canonical payload |
| `internal-runtime-error` | Runtime invariant break or corruption risk | hard runtime error (non-resumable) |

Quick rule:
- use `nil` for intentional absence only
- use payloaded `raise` for recoverable and programmer-facing failures
- do not downcast internal runtime failures into ordinary `raise`/`nil`

#### 0.5.2 Canonical `raise` Payload Shape

```lisp
{ 'code ... 'message ... 'domain ... 'data ... }
```

Field contract:

| Key | Type | Purpose |
|---|---|---|
| `'code` | symbol | stable machine-readable code for tests/routing |
| `'message` | string | human-readable explanation |
| `'domain` | symbol | subsystem domain (`io`, `parser`, `regex`, `scheduler`, `deduce`, `type`, `runtime`) |
| `'data` | dict or `nil` | structured context payload |

#### 0.5.3 Common Domains and Codes

| Domain | Representative codes |
|---|---|
| `io` | `io/not-found`, `io/permission-denied`, `io/invalid-handle` |
| `parser` | `parser/invalid-grammar`, `parser/no-root-match` |
| `regex` | `regex/invalid-pattern`, `regex/invalid-arg-type` |
| `scheduler` | `scheduler/invalid-handle`, `scheduler/timeout` |
| `deduce` | `deduce/relation-not-found`, `deduce/txn-failed` |
| `type` | `type/arg-mismatch`, `type/arity` |
| `runtime` | `runtime/unhandled-effect`, `runtime/invalid-continuation` |

Reference contract sources:
- `docs/ARCHITECTURE.md` (ADR-2026-03-06-A)
- `docs/ERROR_MODEL.md`

### 0.6 Pitfalls Guide

#### 0.6.1 `nil` vs `raise`

Use `nil` for intentional absence, and `raise` for recoverable/programmer
failures.

```lisp
; absence (valid "not found")
(find (λ (x) (= x 999)) '(1 2 3))
; => nil

; recoverable/programmer failure
(signal raise
  {code 'io/not-found
   message "read-file: path not found"
   domain 'io
   data {path "missing.txt"}})
```

Pitfall:
- returning `nil` for operational failures hides actionable error context.

#### 0.6.2 Truthiness Is Narrow

Only `nil` and `false` are falsy. Everything else is truthy, including `0`,
empty strings, and empty collections.

`false` is a distinct boolean false value, while `nil` represents absence.
Both are falsy. Use quoted `'false` if you need the literal symbol name as
symbol data.

```lisp
(if 0 'yes 'no)            ; => 'yes
(if "" 'yes 'no)           ; => 'yes
(if [] 'yes 'no)           ; => 'yes
(if nil 'yes 'no)          ; => 'no
```

Pitfall:
- assuming "empty" values are falsy (as in some other languages) causes logic
  bugs.

#### 0.6.3 Effect `resolve` vs Abort

Inside `handle`, `resolve` resumes the suspended computation at the signal site.
Returning from the clause without `resolve` aborts that path and the clause
result becomes the `handle` result.

```lisp
; resolve: body continues
(handle (+ 1 (signal read nil))
  (read x (resolve 41)))
; => 42

; abort: body after signal does not continue
(handle (+ 1 (signal bail 5))
  (bail x x))
; => 5
```

Pitfall:
- expecting post-signal code to run when the handler did not call `resolve`.

## 1. Syntax Overview

### 1.1 Lexical Elements

```
; Comments start with semicolon and extend to end of line

; Integers
42
-17
0

; Floating-point numbers
3.14
-0.5
1.0

; Strings (double-quoted, with escape sequences)
"hello world"
"line1\nline2"
"tab\there"
"quote: \"nested\""

; Symbols (identifiers)
foo
my-function
string-length
null?

; Collection literals
[1 2 3]         ; array literal, equivalent to (Array 1 2 3)
{a 1 b 2}       ; dict literal, equivalent to (Dictionary 'a 1 'b 2)

; Quote shorthand
'symbol     ; equivalent to (quote symbol)
'(a b c)    ; equivalent to (quote (a b c))

; Quasiquote
`(a ,x ,@xs)   ; template with unquote and splicing
```

### 1.2 S-Expression Forms

```lisp
; Function application
(f arg1 arg2 ...)     ; multi-arg call (strict arity)

; Examples
(+ 1 2)              ; adds 1 and 2
(map inc '(1 2 3))   ; applies inc to each element

; Empty list / nil
()
```

### 1.3 Special Tokens

| Token | Description |
|-------|-------------|
| `_` | Wildcard (in patterns), NOT a symbol |
| `..` | Rest/spread in patterns and variadic params |
| `.[` | Parser token pattern used for postfix index parsing (`expr.[key]`) |
| `.` | Dot for field/path access |
| `^` | Type annotation prefix |
| `[` `]` | Array literals, bracket attributes, and patterns |
| `{` `}` | Dictionary literals |

Slash (`/`) is part of ordinary symbol syntax. Names like `io/println`,
`matrix/eigenpairs`, and `ml/plot` are single symbols, not module
dereferences. They are naming-convention prefixes; real module access uses
module values/path access (`mod.sym`) or explicit import/export forms.

Slash names are canonical for always-present core primitive families where the
prefix is part of the operation's public name rather than a separately imported
module. This includes surfaces such as `io/...`, `matrix/...`, `tensor/...`,
`ml/...`, `nn/...`, `ui/...`, and structured error codes such as
`io/not-found` or `ui/arg-mismatch`. Quoted forms preserve the same rule:
`'ml/plot` is the symbol named `ml/plot`, not a quote of `plot` inside an `ml`
module. Do not split core primitive families into modules solely because their
names contain `/`.

The scientific `math` and `stats` surfaces are core modules because they are
broad scientific APIs that will grow independently from the prelude. Their
canonical access form is dotted module access, for example `math.erf` and
`stats.normal-cdf`. Removed pre-alpha slash spellings such as `math/erf` and
`stats/normal-cdf` are still parsed as ordinary symbols, but they are not the
public scientific API and are not alternate module syntax.

Slash prefixes are a human-facing naming tool, not a blanket hierarchy rule.
Use them when a stable, always-present family would otherwise produce ambiguous
or hard-to-scan names (`ml/plot`, `matrix/eigenpairs`, `tensor/run`). Prefer
short generic names for operations whose meaning is intentionally cross-cutting
(`map`, `ref`, `length`, `sort`) and prefer a normal descriptive name when the
operation is unique enough that a prefix only adds noise. Avoid deep pseudo-paths
and mechanically long slash names; if a surface needs many independently
versioned or optional exports, make it a real module and use dotted access or
explicit imports.

### 1.4 Reader Dispatch (`#`)

The reader supports a small canonical `#` dispatch surface:

| Form | Meaning |
|------|---------|
| `#r"..."` | regex literal |
| `#xFF`, `#b1010`, `#o755` | integer literals in hexadecimal, binary, and octal radix |
| `#_ form` | skip next form |
| `#N_ form...` | skip next `N` forms (`N` in `1..9`) |
| `#| ... |#` | (nestable) block comment |
| `#tag form` | reader tag shorthand; parses as `(tag form)` |

Reader tags are constrained syntax, not Common Lisp style readtable hooks:
`#tag form` reads exactly one following expression and rewrites it to a normal
one-argument call. A tag can therefore be an ordinary function, or a macro
declared with the canonical `(define [reader tag] name (syntax-match ...))`
surface. Any other non-tag `#` sequence is rejected with a deterministic
parser/lexer error.

Built-in data tags include `#hex "ff 0a"` for byte arrays, `#time
"2024-01-15T10:30:00Z"` for UTC time points, and `#uuid
"550e8400-e29b-41d4-a716-446655440000"` for validated UUID strings.

---
