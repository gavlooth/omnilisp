## 8. Standard Library

Higher-order functions and utilities defined in Omni:

| Function | Signature | Description |
|----------|-----------|-------------|
| `map` | `(f coll)` | Apply f to each element (dispatched by collection type) |
| `filter` | `(pred coll)` | Keep elements matching predicate (dispatched; lazy for iterators) |
| `foldl` | `(f acc coll)` | Left fold (dispatched) |
| `foldr` | `(f init coll)` | Right fold (finite collections) |
| `append` | `(a b)` | Concatenate lists |
| `reverse` | `(coll)` | Reverse finite collection (list or array) |
| `compose` | `(f g)` | Function composition |
| `id` | `(x)` | Identity function |
| `nth` | `(n lst)` | Nth element |
| `take` | `(n coll)` | First N elements (dispatched; lazy for iterators) |
| `drop` | `(n coll)` | Drop first N elements (dispatched; lazy for iterators) |
| `zip` | `(a b)` | Zip two collections (dispatched; lazy for iterators) |
| `range` | `(n)` | List from 0 to n-1 |
| `range-from` | `(n)` | Infinite iterator from n |
| `repeat` | `(x)` | Infinite iterator repeating x |
| `cycle` | `(coll)` | Infinite iterator cycling coll |
| `for-each` | `(f lst)` | Apply f for side effects |
| `any?` | `(pred lst)` | Any element matches? |
| `every?` | `(pred lst)` | All elements match? |
| `flatten` | `(lst)` | Flatten nested list (1 level) |
| `partition` | `(pred lst)` | Split by predicate |
| `remove` | `(pred lst)` | Remove matching elements |
| `find` | `(pred lst)` | First matching element |

Stdlib functions take multiple parameters with strict arity. For partial application, `_` placeholders create lambdas `(map (+ 1 _) '(1 2 3))`, `_n` placeholders support reuse/reordering (`((- _2 _1) 3 10)`), `|>` rewrites pipeline steps by appending the piped value, and `partial` provides runtime partial application.

Compatibility note: `_n` placeholder desugaring is only active in call-argument position. Outside call arguments `_n` remains a normal symbol; however, existing call-argument bindings named like `_2` will now be interpreted as indexed placeholders.

### 8.1 Macros

| Macro | Description |
|-------|-------------|
| `when` | `(when test body...)` -- if test, evaluate body |
| `unless` | `(unless test body...)` -- if not test, evaluate body |
| `branch` | `(branch (c1 e1) ... (_ default))` -- condition chain with explicit default marker |

`when` and `unless` predicate checks use the same truthiness contract as `if`
([2.2](#22-truthiness)).

For multi-branch condition chains, prefer `branch`:

```lisp
(branch ((> x 0) "positive")
        ((= x 0) "zero")
        (_ "negative"))
```

`_` is the default marker and must appear only in final position. If no
condition matches and no `_` clause is provided, `branch` returns `nil`.

Equivalent low-level form using `match` + guards:

```lisp
(match Void
  ((? (> x 0)) "positive")
  ((? (= x 0)) "zero")
  (_ "negative"))
```

### 8.2 Effect Utilities

| Name | Description |
|------|-------------|
| `try` | `(try thunk handler)` -- catch `raise` effects |
| `assert!` | `(assert! condition msg)` -- raise if condition fails |
| `yield` | Macro for generator-style values |
| `stream-take` | Take N values from a generator stream |

### 8.3 Lazy Iteration And Constructor Materialization

Omni does not use a general public `delay` / `force` pair for collection or
Tensor laziness. Iterators are already suspended pull computations, and
constructors are the terminal materialization boundary: `(Array iterator)`,
`(List iterator)`, `(Dictionary iterator)` where key/value shape applies, and
`(Tensor iterator)` consume finite iterators through dispatch. Conversely,
`(Iterator tensor)` exposes Tensor values as flat row-major iterator elements,
matching `(Array tensor)` and `(List tensor)`, and requires explicit
`to-device 'cpu` before iterating non-CPU device tensors.

---

## 9. Delimited Continuations

### 9.1 `checkpoint` -- Establish Delimiter

```lisp
(checkpoint body)
```

### 9.2 `capture` -- Capture Continuation

```lisp
(capture k body)
```

Captures the continuation up to the enclosing `checkpoint` and binds it to `k`.

```lisp
(checkpoint (+ 1 (capture k (k (k 10)))))
; k = (λ (x) (+ 1 x))
; (k (k 10)) = (+ 1 (+ 1 10)) = 12
```

### 9.3 Semantics

- Continuations are **multi-shot**: each invocation of `k` clones the captured stack, so `k` can be called multiple times
- Continuation `resume` operation is function invocation: `(k value)` invokes the captured continuation with `value`
- The result of `capture`'s body becomes the result of `checkpoint`
- Multi-shot replay is explicit: each `k` invocation re-runs the resumed continuation segment, including side effects in that segment (`set!`, `signal`, I/O handlers), in source order.
- Each `k` invocation starts from the captured stack snapshot; lexical locals in that resumed segment are restored per invocation unless mutation targets shared state outside the snapshot.
- Side effects that occur in `capture` body code outside resumed continuation segments run when that code executes; they are not replayed unless that body path itself is executed again.
- Replay semantics are execution-mode invariant: interpreter, JIT, and compiled
  execution must preserve the same replay-visible side-effect outcomes.

Canonical continuation-resume example:

```lisp
(checkpoint (+ 1 (capture k (k 41))))
; => 42
```

Note: this continuation `resume` operation (`(k value)`) is distinct from the
coroutine primitive `resume`.

Replay example:

```lisp
(checkpoint (+ 1 (capture k (+ (k 10) (k 20)))))
; => 32

(block
  (define c 0)
  (checkpoint
    (+ (capture k (+ (k 1) (k 1)))
       (block
         (handle (signal io/println "x")
           (io/println msg (resolve 0)))
         (set! c (+ c 1))
         c))))
; handled I/O in resumed segment is replayed per k invocation
```

---

## 10. Effect Handlers

Normative semantics for this section are defined in `docs/EFFECTS_SEMANTICS.md`.

### 10.0 Glossary

| Term | Meaning |
|------|---------|
| `signal` | Emit an effect request with tag+payload from the current evaluation context. |
| `raise` | Conventional effect tag for failure signaling (`signal raise payload`). |
| `resolve` | Resume a captured effect continuation with a value from inside a handler clause. |
| `abort` | Handler path where `resolve` is not called; handler return becomes `handle` result. |
| `resumable` | Effect interaction where control returns to the suspended signal site after `resolve`. |
| `effect boundary` | Runtime boundary where effect semantics are interpreted (handler stack, scheduler callback handoff, or unhandled-effect escalation path). |

### 10.1 `signal` -- Signal Effect

```lisp
(signal effect-tag argument)
```

### 10.2 `handle` -- Install Handler

```lisp
(handle body
  (effect-tag arg handler-body...)
  ...)
```

When an effect is signalled:
- `arg` is bound to the effect argument
- `handler-body` can resolve with `(resolve value)` to resume, or return a value to abort

```lisp
(handle
  (+ 1 (signal read nil))
  (read x (resolve 41)))
; => 42
```

### 10.3 `resolve` -- Resume Computation

Inside a handler clause, `(resolve value)` sends `value` back to the body.
The body continues as if `signal` returned that value.

If `resolve` is not called, the handler's return value becomes the result
of the entire `handle` expression (abort).

Discipline contract:
- `resolve` is handler-bound: it must target the hidden continuation from the
  current `handle` clause (`__k`).
- `resolve` is single-shot for that continuation. A second `resolve` attempt on
  the same continuation raises deterministic recoverable code
  `runtime/continuation-resumed`.
- Multi-shot behavior is available only through explicit continuation calls
  (`with-continuation` + `(k ...)`), not through repeated `resolve`.

```lisp
; Resolve — body continues
(handle (signal Float64 5)
  (Float64 x (resolve (* x 2))))
; => 10

; Abort — body abandoned
(handle (+ 1 (signal bail 42))
  (bail x x))
; => 42
```

### 10.4 I/O Effects

I/O operations go through effects with a fast path:

```lisp
; These use io/print, io/println, etc. effect tags
(println "hello")     ; fast path when no handler
(print 42)

; Custom handler intercepts I/O
(handle (block (println "suppressed") 42)
  (io/println x (resolve nil)))
; => 42 (output suppressed)

; Capture output
(handle (block (println "captured") nil)
  (io/println x x))
; => "captured"
```

Effect tags: `io/print`, `io/println`, `io/display`, `io/newline`, `io/read-line`, `io/read-file`, `io/write-file`, `io/file-exists?`, `io/read-lines`

### 10.5 Typed Dispatch in Handlers

Effect handlers match on tag name only. For type-specific behavior, use dispatched functions inside the handler body — this reuses the existing MethodTable dispatch system rather than introducing a parallel matching mechanism:

```lisp
(define (on-show (^Integer x)) (string-append "int: " (String x)))
(define (on-show (^String s)) (string-append "str: " s))

(handle
  (block (signal show 42) (signal show "hello"))
  (show x (println (on-show x)) (resolve nil)))
```

### 10.6 Composition Helper Naming (Migration Note)

For helper-style handler composition in examples and public-facing docs:
- use `handle/chain` as the canonical helper name
- do not introduce new abbreviated aliases for this helper
- migrate historical `with-handlers` / `handle-chain` example spellings to
  `handle/chain`

---

## 11. Macros

### 11.1 Single-Transformer Macros

```lisp
(define [macro] when
  (syntax-match
    ([test .. body]
      (template (if (insert test) (block (splice body)) nil))))

(define [macro] unless
  (syntax-match
    ([test .. body]
      (template (if (insert test) nil (block (splice body))))))
```

- One macro surface only: `(define [macro] name (syntax-match ...))`
- Reader tag macros use `(define [reader tag] name (syntax-match ...))`;
  `#name form` parses as `(name form)`.
- Macros are syntax transformers, not overloaded callable sets
- Pattern-based with template substitution
- Hygienic: template literals resolve at definition time
- Auto-gensym: `name#` in templates generates unique symbols
- `gensym` function for manual hygiene
- Legacy clause-style macro definitions are rejected with deterministic diagnostics

### 11.2 Expansion

```lisp
(macroexpand '(when true 1 2 3))
; => (if true (block 1 2 3) nil)
```

---

## 12. Modules

```lisp
(module math-utils (export add multiply)
  (define (add a b) (+ a b))
  (define (multiply a b) (* a b)))

;; Qualified access (default)
(import math-utils)
(math-utils.add 3 4)  ; => 7

;; Selective import
(import math-utils (add multiply))
(add 3 4)  ; => 7

;; Rename on import
(import math-utils (add 'as plus))
(plus 3 4)  ; => 7

;; Import all exports unqualified
(import math-utils 'all)
(add 3 4)  ; => 7

;; Grouped imports use ordinary s-expressions, not dictionaries.
(import
  (math-utils (add 'as plus))
  (ui.nodes 'all)
  (json))

;; Dotted/path module target
(import ui.nodes)
(ui.nodes.text "ok")

;; Re-export
(export-from math-utils (add))
(export-from math-utils 'all)
(export-from ui.nodes (text))

;; Core scientific modules are prebound.
(math.erf 1.0)
(stats.normal-cdf 0.0)
```

- Default import is **qualified-only**: `(import mod)` binds module as value, access via `mod.sym`
- Selective import: `(import mod (sym1 sym2))` for specific symbols
- Rename: `(import mod (sym1 'as alias))` for renaming on import
- `'all` imports all exports unqualified (opt-in)
- Grouped import: `(import (mod spec) (other 'all) (qualified-only))` lowers
  to ordinary import commands in source order
- Omni has no dedicated keyword type; `'as`/`'all` are quoted symbols used as explicit module markers
- `{}` is data-only dictionary literal syntax and is not accepted for grouped
  module selection
- `export-from` re-exports symbols from another module
- Module targets for `module` / `import` / `export-from` can be:
  - symbol (`math-utils`)
  - dotted/path token (`ui.nodes`)
  - string file path (`"path/to/file.omni"`)
- Slash-qualified primitive names such as `ml/plot`, `matrix/eigenpairs`, and
  `io/println` are ordinary single symbols, not module targets. They remain
  available as canonical core-family primitive names; use dotted/path access
  only for actual module values.
- Do not use slash names as a substitute for modules when the surface is
  optional, independently versioned, or large enough that developers need a real
  import boundary.
- File-based import: `(import "path/to/file.omni")`
- Cached: modules loaded only once
- Circular import detection
- Core scientific modules are always available as module values. Prefer
  `math.*` and `stats.*` for scientific math and distribution operations. Old
  slash spellings such as `math/erf` and `stats/normal-cdf` were removed during
  pre-alpha cleanup; they are single-symbol parses, not module access.
- Method extensions are always global (dispatch is cross-cutting)
- `module` / `import` / `export-from` are command-style forms and return `Void` on successful completion
- Compiler backend (`AOT`) currently uses static module lowering: module bodies are inlined during compilation, and `import` / `export-from` lower to command-style `Void` no-ops (no runtime module loading/binding pass in generated code)

---

## 13. REPL

```bash
omni --repl
omni --repl --project
omni --repl --load demo.omni
```

```
Omni Lisp REPL (type 'quit' or 'exit' to leave)
---
> (define x 10)
10
> (+ x 5)
15
> (define (inc n) (+ n 1))
#<closure>
> (inc x)
11
> quit
Goodbye!
```

Project preload:

```bash
omni --repl --project
omni --repl --project myproject
omni --repl --load demo.omni
```

- `--project` resolves an Omni project root from the current directory or the
  optional directory argument.
- It requires `omni.toml` and preloads `src/main.omni` before the interactive
  session starts.
- `--load` preloads one Omni source file directly and works for standalone
  example/workspace trees that are not full Omni projects.
- Relative imports inside `src/main.omni` use the entry file's source
  directory, the same way script execution does.
- text REPL preload read failures preserve the concrete file-read cause
  (`file not found`, `permission denied`, `invalid path`, or generic
  `read failed`) instead of collapsing to one generic startup message.
- REPL preload (`--project` or `--load`) is text-REPL-only; it is not
  supported with `--json`.

---

## 14. Examples

### 14.1 Factorial

```lisp
(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))
(fact 10)  ; => 3628800
```

### 14.2 Fibonacci with Dispatch

```lisp
(define (fib (^(Literal 0) n)) 0)
(define (fib (^(Literal 1) n)) 1)
(define (fib (^Integer n)) (+ (fib (- n 1)) (fib (- n 2))))
(fib 10)  ; => 55
```

### 14.3 Option Type

```lisp
(define [union] (Option T) None (Some T))

(define (safe-div a b)
  (if (= b 0) None (Some (/ a b))))

(match (safe-div 10 3)
  (None "division by zero")
  ((Some x) x))
; => 3
```

### 14.4 Effect Handler for State

```lisp
(handle
  (let (x (signal get nil))
    (block
      (signal put (+ x 1))
      (signal get nil)))
  (get _ (resolve 0))
  (put v (resolve nil)))
```

### 14.5 Collection Literals and Generic Operations

```lisp
; Array literal
(define nums [1 2 3 4 5])
(ref nums 0)           ; => 1
(length nums)           ; => 5
(push! nums 6)          ; mutates, adds 6

; Dictionary literal
(define person {name "Alice" age 30})
(ref person 'name)      ; => "Alice"
(has? person 'age)      ; => true
(keys person)           ; => '(age name)

; Constructor dispatch
(Array '(1 2 3))        ; list → array conversion
(list [1 2 3])          ; array → list conversion
(Array (take 5 (range-from 0))) ; consume iterator into array
(list (take 5 (range-from 0)))  ; consume iterator into list
(Tensor (take 3 (range-from 1))) ; consume iterator into rank-1 tensor

; Cons mutation via dot-path
(define p (cons 1 2))
(set! p.car 99)
p.car                   ; => 99
```

### 14.6 Type Hierarchy

```lisp
(define [abstract] Shape)
(define [type] (Circle Shape) (^Integer radius))
(define [type] (Rect Shape) (^Integer width) (^Integer height))

(define (area (^Circle c)) (* pi (* c.radius c.radius)))
(define (area (^Rect r)) (* r.width r.height))

(area (Circle 5))      ; => ~78.5
(area (Rect 3 4))      ; => 12
```

---

## 15. CLI & Project Tooling

### 15.1 Running Programs

```bash
omni script.omni    # Run a script
omni --repl         # Start the REPL (explicit)
omni --repl --project [dir]  # Start REPL with project entry preloaded
omni --repl --load <file>    # Start REPL with one file preloaded
omni                # Start the REPL (default)
```

### 15.2 Compilation

```bash
omni --compile input.omni output.c3                         # Omni → C3 source
omni --build input.omni -o output                           # Omni → standalone binary (AOT)
```

### 15.3 Project Management

```bash
omni --init myproject                                       # Scaffold project directory
omni --bind myproject/                                      # Generate FFI bindings from omni.toml
```

- `--init` creates `omni.toml`, `src/main.omni`, `lib/ffi/`, `include/`, `build/` (with generated `project.json`) and now rolls back the fresh project root if a later scaffold write fails or a subpath collides with a non-directory
- `--bind` reads `omni.toml`, parses C headers via libclang, writes regenerated raw FFI modules plus facade stubs to `lib/ffi/`
- libclang is an optional runtime dependency (only needed for `--bind`)

See `docs/PROJECT_TOOLING.md` for the complete reference including `omni.toml` format, build configuration, type mapping, and workflow examples.

---

## Appendix A: Grammar (EBNF)

```ebnf
program     = { expr } ;
expr        = literal | symbol | path | quoted | quasiquoted | reader_tag
            | list | array_lit | dict_lit | indexed | accessor ;

literal     = integer | radix_integer | float | string ;
integer     = [ "-" ] digit { digit } ;
radix_integer = ("#x" | "#X") [ "-" ] hex_digit { hex_digit }
(* hex radix alpha digits are uppercase A through F; lowercase symbol-like
   forms such as #xface, #b101tag, and #o777tag are reader tags. *)
              | ("#b" | "#B") [ "-" ] bin_digit { bin_digit }
              | ("#o" | "#O") [ "-" ] oct_digit { oct_digit } ;
float       = [ "-" ] digit { digit } "." digit { digit } ;
string      = '"' { char | escape } '"' ;
symbol      = symbol_char { symbol_char } ;
path        = symbol "." symbol { "." symbol } ;

quoted      = "'" datum ;
quasiquoted = "`" datum ;
reader_tag  = "#" symbol expr ;           (* equivalent to one-argument call *)
list        = "(" { expr } ")" ;
array_lit   = "[" { expr } "]" ;           (* equivalent to Array constructor call *)
dict_lit    = "{" { dict_key expr } "}" ;  (* equivalent to Dictionary constructor call; must be even *)
dict_key    = symbol | expr ;              (* bare symbol keys auto-quote only in key position *)
indexed     = expr ".[" expr "]" ;
accessor    = "." expr ;

datum       = literal | symbol | "(" { datum } ")" | "'" datum ;

symbol_char = letter | digit | "_" | "-" | "+" | "*" | "/"
            | "=" | "<" | ">" | "!" | "?" | ":" | "@" | "#"
            | "$" | "%" | "&" | "|" | "^" | "~" ;
```

---

## Appendix B: Limits

| Resource | Limit |
|----------|-------|
| Symbol/string length | Dynamic (heap-allocated) |
| Total symbols | 8192 |
| Bindings per env frame | 512 |
| Match clauses | Dynamic (no fixed limit) |
| Pattern elements | Dynamic (no fixed limit) |
| Effect handler clauses | Dynamic (no fixed limit) |
| Handler stack depth | 16 |
| Call arguments | Dynamic AST (JIT compiles up to 16 natively) |
| Path segments | 8 |
| Block expressions | Dynamic (no fixed limit) |
| Lambda params | Dynamic (no fixed limit) |
| String literal (inline) | 63 bytes (lexer limit) |
| Macros | 64 |
| Macro transformer branches | Dynamic (inside `syntax-match`) |
| Modules | 32 |
| Module exports | 128 |
| Eval depth | 5000 |
| Registered types | 256 |
| Type fields | 16 |
| Method table entries | 64 |

---

## Appendix C: Backends
