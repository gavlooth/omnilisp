# Omni Lisp

Omni Lisp is a Lisp dialect with strict-arity lambdas, multiple dispatch,
algebraic effects, and delimited continuations. The runtime is deterministic,
scope/region-based, and implemented in C3 with JIT and AOT compilation paths.

This README is the first stop for people using the language. The normative
contract still lives in:

- [docs/README.md](docs/README.md)
- [docs/LANGUAGE_SPEC.md](docs/LANGUAGE_SPEC.md)
- [docs/SYNTAX_SPEC.md](docs/SYNTAX_SPEC.md)
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md)

## Quick Start

```lisp
(define x 10)
(define y 20)
(+ x y)
```

```lisp
(if (> x 5) "big" "small")
(block
  (println "first")
  (println "second")
  42)
```

Omni is expression-oriented. Most forms return a value, and only `nil` and
`false` are falsy.

## Core Model

```lisp
(define answer 42)
(define greeting "hello")
(define flag true)
```

- `nil` and `false` are falsy.
- `Void` is truthy and represents successful no-result completion.
- Slash names such as `math/erf` or `io/read` are single symbols, not module
  dereferences.
- Dot syntax is the module/value access surface.

```lisp
(import math)
(math.pi)
```

## Functions And Arity

```lisp
(define (add2 a b) (+ a b))
(add2 3 4)
```

```lisp
(define (collect first .. rest) rest)
(collect 1 2 3 4)
```

```lisp
(λ (x y) (+ x y))
```

`λ` is the canonical lambda spelling. `lambda` remains accepted as a long
alias.

Strict arity matters:

```lisp
(define (square x) (* x x))
(square 5)
```

Partial application uses `_`, `|>` or explicit `partial` where appropriate.

## Bindings And Scope

```lisp
(let (x 1 y 2)
  (+ x y))
```

```lisp
(let ([a b] [10 20])
  (+ a b))
```

```lisp
(let ({name age} {name "Ada" age 37})
  name)
```

```lisp
(let ^rec (loop (n 5))
  (if (= n 0)
      1
      (* n (loop (- n 1)))))
```

Use `define` for top-level bindings and `let` for local scope. Named `let`
lowers through the recursive binding form.

## Collections And Access

```lisp
'(1 2 3)
[1 2 3]
{a 1 b 2}
```

```lisp
(define config {port 8080})
config.port
config.['port]
```

```lisp
(define xs [10 20 30])
(set! xs.[1] 99)
xs
```

- Lists, arrays, and dictionaries are the core collection types.
- `Dict` is the approved shorthand alias for `Dictionary`.
- Bare symbol keys in dictionary literals auto-quote.

## Quote, Templates, And Paths

```lisp
'(1 2 3)
`(a ,(+ 1 2) ,@(list 3 4))
```

```lisp
(define point {x 3 y 4})
point.x
point.y
point.['x]
```

- `quote` and `quasiquote` are the standard syntax tools for data and code
  templates.
- Dot access uses path semantics for fields, module values, and symbol-key
  lookups.

## Literals And Reader Tags

```lisp
#xFF
#b1010
#o755
```

```lisp
(str "hello {name}")
#hex "ff 0a 1b 00"
#time "2024-01-15T10:30:00Z"
#uuid "550e8400-e29b-41d4-a716-446655440000"
```

Reader tags lower as ordinary tag calls:

```lisp
#json "{\"ok\": true}"
```

Use the reader-tag surface when you want a compact construction or parsing
form, not a hidden new literal system.

## Mutation

```lisp
(set! name "Ada")
(set! pair.car 1)
(set! pair.cdr 2)
(set! collection key value)
```

`set!` returns `Void` on success.

## Control Flow

```lisp
(if (> x 5) "big" "small")
```

```lisp
(and (> x 0) (< x 10))
(or cached-value (compute))
```

```lisp
(match expr
  ((Some x) x)
  (None 0)
  (_ -1))
```

`match` works with literals, wildcards, sequence patterns, constructors, and
quoted symbols.

## Types And Dispatch

```lisp
(define [type] Point
  (^Integer x)
  (^Integer y))

(define p (Point 3 4))
```

```lisp
(define (describe (^Integer n)) "integer")
(define (describe (^String s)) "string")
(define (describe x) "other")
```

```lisp
(define [union] (Result T E)
  (Ok T)
  (Err E))
```

```lisp
(define [alias] Text String)
```

- `define [type]` and `define [struct]` declare nominal product types.
- `define [union]` declares sum types.
- Repeated `define` forms with the same name create multiple dispatch methods.
- Constructor calls are explicit conversion points.

## Modules And Macros

```lisp
(module math-utils (export add multiply)
  (define (add a b) (+ a b))
  (define (multiply a b) (* a b)))

(import math-utils)
(math-utils.add 3 4)
```

```lisp
(define [macro] when
  (syntax-match
    ([test .. body]
      (template (if (insert test) (block (splice body)) nil)))))
```

```lisp
(define [reader tag] reader-double
  (syntax-match
    ([x] (template (+ (insert x) (insert x))))))

#reader-double 21
```

Use modules for namespacing and macros for syntax extension. Legacy
clause-style macro forms are removed.

## Effects And Continuations

```lisp
(checkpoint
  (+ 1 (capture k (k 10))))
```

```lisp
(handle
  (signal 'ask "name")
  ((ask value resume)
    (resume "Ada")))
```

- `checkpoint` establishes a delimiter.
- `capture` captures a continuation.
- `signal` and `handle` implement algebraic effects.

## Primitives And Standard Library

```lisp
(+ 1 2)
(= x y)
(cons 1 '(2 3))
(str "hello" " " "world")
```

```lisp
(list? value)
(array? value)
(dict? value)
(set? value)
```

The full primitive and standard-library surface is documented in the reference
chapters, especially the primitives, macros, effects, and CLI/tooling
sections.

## Build And Run

```bash
# full integration build
OMNI_HOST_TOOLCHAIN_LIB_PATH="${OMNI_HOST_TOOLCHAIN_LIB_PATH:-/usr/local/lib}"
LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LIBRARY_PATH:+:$LIBRARY_PATH}" c3c build

# run the repo-local main binary directly
LD_LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ./build/main
```

```bash
# fast developer build
scripts/build_fast_dev.sh

# lean dev binary
LD_LIBRARY_PATH="$OMNI_HOST_TOOLCHAIN_LIB_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ./build/dev-fast/main-dev --eval '(+ 1 2)'
```

Use `c3c build` as the integration/full-runtime build. Use
`scripts/build_fast_dev.sh` for the default edit/build loop when you do not
need embedded test-suite support.

## Validation And Tooling

```bash
scripts/run_e2e.sh
```

For the full gate policy and Docker-capped validation paths, see:

- [docs/PROJECT_TOOLING.md](docs/PROJECT_TOOLING.md)
- [docs/RELEASE_STATUS.md](docs/RELEASE_STATUS.md)
- [docs/OMNI_REFERENCE.md](docs/OMNI_REFERENCE.md)

Editor and REPL workflow helpers live under `tooling/`.

## Reference Map

- [docs/README.md](docs/README.md) for the authoritative docs entrypoint
- [docs/OMNI_REFERENCE.md](docs/OMNI_REFERENCE.md) for chapter navigation
- [docs/LLM_LANGUAGE_DIGEST.md](docs/LLM_LANGUAGE_DIGEST.md) for a compact
  non-normative syntax guide

If you are reading this to learn the language, start with the quick start and
work section-by-section through the examples above.
