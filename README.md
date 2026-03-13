# Omni Lisp

Omni Lisp is a Lisp dialect with strict-arity lambdas, multiple dispatch,
algebraic effects, and delimited continuations. The runtime is deterministic
and scope/region-based, implemented in C3 with JIT and AOT compilation paths.

This README is aligned with the current syntax and language docs:
- [Syntax Spec](docs/SYNTAX_SPEC.md)
- [Language Spec](docs/LANGUAGE_SPEC.md)
- [Docs Map](docs/README.md)

## Status

Release status is tracked in [Release Status](docs/RELEASE_STATUS.md).

As of 2026-03-10:
- language feature work is considered complete at current baseline,
- full release validation is expected to run via Docker-bound gates.

## Syntax Quick Start

The canonical syntax details are in [docs/SYNTAX_SPEC.md](docs/SYNTAX_SPEC.md).
The examples below match that spec.

### Core Expressions

```lisp
(define x 10)
(define y 20)
(+ x y)

(if (> x 5) "big" "small")
(block (println "a") (println "b") 42)
```

### `let` Bindings and Destructuring

```lisp
;; flat pair bindings
(let (x 1 y 2) (+ x y))

;; array destructuring
(let ([a b .. rest] [10 20 30 40]) (+ a b))

;; dict destructuring
(let ({name age} {'name "Alice" 'age 30}) name)
```

### Functions and Arity

```lisp
;; strict arity
(define (add2 a b) (+ a b))

;; variadic parameter with ..
(define (collect first .. rest) rest)

;; lambda form
(lambda (x y) (+ x y))
```

### Collections and Access

```lisp
;; list
'(1 2 3)

;; array literal (desugars to (array ...))
[1 2 3]

;; dict literal (desugars to (dict ...))
{'a 1 'b 2}

;; path and index access
point.x
arr.[0]
dict.['key]
```

### Mutation

```lisp
(set! name value)
(set! pair.car value)
(set! pair.cdr value)
(set! collection key value) ; generic Array/Dictionary update
```

`set!` returns `Void` on success.

### Pattern Matching

```lisp
(match expr
  ((Some x) x)
  (None 0)
  (_ -1))
```

### Effects and Continuations

```lisp
;; delimited continuations
(reset (+ 1 (shift k (k 10))))

;; algebraic effects
(handle
  (signal 'ask "name")
  (ask arg (resolve "ok")))
```

## Language Rules That Matter

- Truthiness:
  - falsy: `nil`, `false`
  - truthy: everything else
- Collections: list, array, dict.
- Dispatch is explicit and type-driven; no implicit widening/coercion in method match.
- Constructors are explicit conversion points.

Canonical type names in docs/runtime are descriptive:
`Integer`, `Double`, `String`, `Symbol`, `List`, `Array`, `Dictionary`,
`Set`, `Iterator`, `Coroutine`, `TimePoint`, `Boolean`, `Nil`, `Void`.

Compatibility shorthands such as `Int`, `Bool`, and `Dict` are still accepted
where documented, but descriptive names are preferred in spec/docs/examples.

## Build and Run

```bash
# build
c3c build

# run main binary
LD_LIBRARY_PATH=/usr/local/lib ./build/main
```

## Validation

```bash
# generate + compile + run e2e compiler output parity
scripts/run_e2e.sh
```

For full gate policy and Docker-capped validation paths, use:
- [Project Tooling](docs/PROJECT_TOOLING.md)
- [AGENTS guidance](AGENTS.md)

## Documentation Index

- [Docs Map](docs/README.md)
- [Language Spec](docs/LANGUAGE_SPEC.md)
- [Syntax Spec](docs/SYNTAX_SPEC.md)
- [Type and Dispatch Syntax](docs/type-system-syntax.md)
- [Effects Guide](docs/EFFECTS_GUIDE.md)
- [Architecture](docs/ARCHITECTURE.md)
- [Project Tooling](docs/PROJECT_TOOLING.md)
- [Reference Manual](docs/OMNI_REFERENCE.md)
- [Memory Changelog (implementation truth)](memory/CHANGELOG.md)
