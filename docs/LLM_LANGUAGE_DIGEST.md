# Omni Lisp LLM Language Digest

**Updated:** 2026-04-27

This is a compact, non-normative guide for LLMs. Use it to avoid inventing
syntax while reading or generating Omni examples. If this digest disagrees with
the canonical specs, follow `docs/LANGUAGE_SPEC.md`, `docs/SYNTAX_SPEC.md`, and
`memory/CHANGELOG.md`.

## Authority

- Normative language contract: `docs/LANGUAGE_SPEC.md`
- Syntax/token grammar: `docs/SYNTAX_SPEC.md`
- Type and dispatch reference: `docs/type-system-syntax.md`
- Reader syntax reference: `docs/reference/10-system-tooling.md`
- Compatibility/removals: `docs/SURFACE_COMPATIBILITY.md`

## Core Shape

Omni is Lisp-shaped. Calls are parenthesized, evaluation is strict, lambdas have
strict arity, and the canonical lambda spelling is `λ`.

```lisp
(define x 42)
(define (add a b) (+ a b))

(λ (x) (* x 2))
(λ (x .. rest) body)

(if test then-expr else-expr)
(block expr1 expr2 expr3)
```

Only `nil` and `false` are falsy. `Void` is truthy and represents successful
no-result command completion.

## Literals And Collections

```lisp
42
3.14
"hello"
'symbol
true
false
nil

'(1 2 3)          ; List data
[1 2 3]           ; Array literal, constructor surface is Array
{name "Ada" age 37} ; Dictionary literal; bare keys auto-quote
```

Canonical collection type/constructor names are `List`, `Array`,
`Dictionary`, `Set`, and `Iterator`. `Dict` is the approved shorthand alias for
`Dictionary`.

Slash names such as `io/read` or `matrix/lu` are single symbols, not module
paths. Dot syntax is the module/value path surface:

```lisp
(import math-utils)
(math-utils.add 1 2)
```

## Type Definitions

Use `define` declaration attribute clauses. The bracketed clause immediately
after `define` is parser/declaration metadata, not an Array expression.

```lisp
(define [abstract] Shape)

(define [type] Point
  (^Integer x)
  (^Integer y))

(define [struct] Vec2 (^Integer x) (^Integer y)) ; alias of [type]

(define [type] (Circle Shape)
  (^Integer radius))

(define [union] (Result T E) (Ok T) (Err E))
(define [alias] Text String)
```

Construct user-defined nominal values by calling the type or variant symbol:

```lisp
(define p (Point 3 4))
p.x
p.[0]
(set! p.x 99)

(Ok 42)
Err
```

Do not invent `value-type`, `defvalue`, `#Type{...}`, or colon-keyword record
syntax. The current product-type surface is `(define [type] ...)` plus callable
constructors.

## Type Annotations

Type annotations use `^`.

```lisp
^Integer x
^String name
^(Dictionary String Integer) cache
^(Lambda Integer Integer) successor
```

Typed parameters use the annotation inside the parameter list:

```lisp
(define (area (^Circle c)) (* pi (* c.radius c.radius)))
(define (describe (^String s)) "string")
(define (describe x) "fallback")
```

## Multiple Dispatch

Repeated `define` forms with the same function name create typed methods.
Dispatch chooses the best match.

```lisp
(define (describe (^Integer n)) "integer")
(define (describe (^String s)) "string")
(define (describe x) "other")
```

Dispatch specificity is:

1. literal singleton, via `^#datum` or long form `^(Literal datum)`
2. exact nominal type
3. subtype
4. untyped fallback

Literal-singleton dispatch is not metadata dispatch:

```lisp
(define (fib (^#0 n)) 0)
(define (fib (^#1 n)) 1)
(define (fib (^Integer n)) (+ (fib (- n 1)) (fib (- n 2))))

(define (udp (^#'open cmd)) (io/udp-open))
```

`^(Literal datum)` is the explicit long form. `(Literal datum)` returns a
singleton literal type descriptor such as `#<type (Literal 3)>`; it does not box
or wrap the runtime value.

## Reader Tags

Reader tags have one grammar rule:

```lisp
#tag form
```

This parses as:

```lisp
(tag form)
```

Built-in reader tag forms include:

```lisp
#hex "ff0a"
#base64 "SGVsbG8="
#json "{\"ok\": true}"
#toml "port = 8080"
#time "2024-01-15T10:30:00Z"
#uuid "550e8400-e29b-41d4-a716-446655440000"
```

Radix integer literals are separate syntax:

```lisp
#xFF
#b1010
#o755
```

Reader tag macros use the declaration attribute surface:

```lisp
(define [reader tag] reader-double
  (syntax-match
    ([x] (template (+ (insert x) (insert x))))))

#reader-double 21
```

Use reader tags for compact construction/parsing when the tag can lower to an
ordinary function or macro. Do not use reader tags as an implicit record literal
syntax unless the tag function/macro is actually defined.

## Macros

Macros use `syntax-match` under `(define [macro] ...)`.

```lisp
(define [macro] when
  (syntax-match
    ([test .. body]
      (template (if (insert test) (block (splice body)) nil)))))
```

Legacy clause-style macro forms are removed.

## Templates

`#syntax` is the canonical template form. It produces the same AST as
quasiquote and reuses the same runtime paths.

```lisp
#syntax (a #{x} #{.. xs})   ; #{x} unquotes, #{.. xs} splices
```

Legacy `` ` ``, `,`, and `,@` remain fully supported.

## Destructuring And Patterns

Arrays and dictionaries destructure in binding and parameter positions.

```lisp
(let ([x y] [10 20]) (+ x y))
(let ({name age} {name "Ada" age 37}) name)

(define (connect {host port}) (tcp-connect host port))
```

Pattern matching uses `_` for wildcard and `..` for rest/spread.

```lisp
(match result
  ((Ok value) value)
  ((Err message) message)
  (_ nil))
```

## Effects And Continuations

Effects use `signal` and `handle`. Delimited continuations use `checkpoint` and
`capture`.

```lisp
(signal tag arg)

(handle body
  ((tag x resume) handler-body))

(checkpoint body)
(capture k body)
```

## Common Do Not Invent List

- Do not use `lambda` in generated canonical output; prefer `λ`.
- Do not use `def`, `defn`, `defstruct`, `defmacro`, or `value-type`.
- Do not write `#Type{field: value}`; current reader tags are `#tag form`.
- Do not use colon keywords for dictionaries or metadata.
- Do not treat `[type]` after `define` as an Array argument.
- Do not call `Any`, `Number`, or `Collection` as constructors.
- Do not use removed `^(Value datum)` / `^(Val datum)` literal dispatch syntax.
- Do not introduce `vector`; use `Array` for indexed arrays.
- Do not model type identity as ad-hoc dictionary metadata when a nominal
  `[type]`, `[union]`, or callable builtin type already exists.
