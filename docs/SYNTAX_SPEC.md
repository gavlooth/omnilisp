# Omni Lisp Syntax Specification

**Updated:** 2026-04-24

---

## 1. Lexical Elements

### 1.1 Token Types

| Token | Pattern | Description |
|-------|---------|-------------|
| T_EOF | End of file | Reached end of input |
| T_LPAREN | `(` | Left parenthesis for list forms |
| T_RPAREN | `)` | Right parenthesis for list forms |
| T_LBRACKET | `[` | Left bracket for array literals, patterns, and declaration attribute clauses |
| T_RBRACKET | `]` | Right bracket for array literals, patterns, and declaration attribute clauses |
| T_LBRACE | `{` | Left brace for dict literals, metadata dictionaries |
| T_RBRACE | `}` | Right brace for dict literals, metadata dictionaries |
| T_DOT_BRACKET | `.[` | Parser token pattern used while parsing postfix index syntax `expr.[key]`. |
| T_QUOTE | `'` | Quote shorthand |
| T_BACKQUOTE | `` ` `` | Quasiquote shorthand |
| T_COMMA | `,` | Unquote shorthand |
| T_COMMA_AT | `,@` | Unquote-splicing shorthand |
| T_INT | `-?[0-9]+`, `#x-?[0-9a-fA-F]+`, `#b-?[01]+`, `#o-?[0-7]+` | Fixed-width signed integer literals in `long.min..long.max` |
| T_FLOAT | `-?[0-9]+\.[0-9]+` | Floating-point literals |
| T_STRING | `"..."` | String literals with escapes: `\n`, `\t`, `\\`, `\"` |
| T_REGEX | `#r"..."` | Regex literal payload, represented as a string value |
| T_READER_TAG | `#tag` | Reader tag prefix; `#tag form` parses as `(tag form)`. Built-ins include `#hex`, `#base64`, `#json`, `#toml`, `#time`, `#uuid`; `#x`, `#b`, and `#o` are radix integers. |
| T_SYMBOL | `[a-zA-Z0-9_\-+*/=<>!?:@#$%&\|^~]+` | Identifiers |
| T_PATH | `segment.segment[.segment]*` | Dot-separated paths |
| T_UNDERSCORE | `_` | Wildcard (not a symbol) |
| T_DOTDOT | `..` | Rest/spread in patterns and variadic params |
| T_ERROR | Any unrecognized | Invalid token |

Slash (`/`) is an ordinary symbol character. A name such as `math/lgamma` is a
single symbol, like `math-lgamma`; it is not module dereference syntax. Use
`module.value` path access or explicit `import` forms for real module access.
Slash-prefixed package names are only a naming convention unless a module exports
and binds that symbol explicitly.

### 1.2 Whitespace and Comments

- **Whitespace:** spaces, tabs, newlines skipped
- **Comments:** `;` to end of line

---

## 2. Expression Types (AST)

| Tag | Syntax | Description |
|-----|--------|-------------|
| E_LIT | `42`, `3.14`, `"str"` | Literal values |
| E_VAR | `name` | Variable lookup |
| E_LAMBDA | `(λ (p) body)` | Function definition; `lambda` remains accepted as long alias |
| E_APP | `(f arg)` | Function application |
| E_CALL | `(f a b c)` | Multi-arg call (3+ args) |
| E_IF | `(if t a b)` | Conditional |
| E_LET | `(let (n v) body)` | Local binding |
| E_DEFINE | `(define n v)` | Global definition |
| E_SET | `(set! n v)` | Variable mutation |
| E_BEGIN | `(block e1 e2 ...)` | Sequencing |
| E_AND | `(and a b)` | Short-circuit and |
| E_OR | `(or a b)` | Short-circuit or |
| E_QUOTE | `'datum` | Prevent evaluation |
| E_QUASIQUOTE | `` `expr `` | Template with unquote |
| E_UNQUOTE | `,expr` | Unquote inside quasiquote |
| E_UNQUOTE_SPLICING | `,@expr` | Splice into quasiquote |
| E_MATCH | `(match expr clauses...)` | Pattern matching |
| E_RESET | `(checkpoint body)` | Delimited continuation prompt |
| E_SHIFT | `(capture k body)` | Capture continuation |
| E_PERFORM | `(signal tag arg)` | Signal algebraic effect (internal tag is E_PERFORM, keyword is `signal`) |
| E_HANDLE | `(handle body clauses...)` | Handle algebraic effects |
| E_DEFMACRO | `(define [macro] ...)`, `(define [reader tag] ...)` | Pattern macro definition |
| E_MODULE | `(module name ...)` | Module definition |
| E_IMPORT | `(import name)` | Module import |
| E_DEFTYPE | `(define [type] ...)` / `(define [struct] ...)` | Struct type definition |
| E_DEFABSTRACT | `(define [abstract] ...)` | Abstract type definition |
| E_DEFUNION | `(define [union] ...)` | Union type definition |
| E_DEFALIAS | `(define [alias] ...)` | Type alias definition |
| E_INDEX | `arr.[i]` | Postfix indexing |
| E_PATH | `a.b.c` | Dot-path field access (distinct path-step semantics; not a full `ref` desugar) |

---

## 3. Special Forms

### 3.1 `λ` / `lambda` - Function Definition

`λ` is the canonical lambda spelling for input and printed/generated output.
`lambda` remains accepted as the long accessibility alias.

```lisp
;; Single parameter
(λ (x) body)

;; Multi-parameter (strict arity)
(λ (x y z) body)
;; requires exactly 3 arguments; use _ placeholder, |> pipe, or partial for partial application

;; Zero-argument
(λ () body)

;; Variadic
(λ (x .. rest) body)

;; Typed parameters (for dispatch)
(λ ((^Integer x) (^String y)) body)

;; Dictionary destructuring parameter
(λ ({name age}) (println name age))
;; caller passes a dict: (f {name "Alice" age 30})

;; Mixed positional + dict params
(λ ({host port} verbose) body)
```

### 3.2 `if` - Conditional

```lisp
(if test then-expr else-expr)
```

Three branches required (no two-branch form).

### 3.3 `let` - Local Binding

```lisp
;; Simple let (flat pairs)
(let (name init) body)

;; Multi-binding let (flat pairs, sequential left-to-right)
(let (x 1 y 2) (+ x y))

;; Array destructuring
(let ([x y] [10 20]) (+ x y))
(let ([head .. tail] '(1 2 3)) head)
(let ([a b ..] '(1 2 3 4 5)) (+ a b))

;; Dictionary destructuring
(let ({name age} {name "Alice" age 30}) name)
(let ({x y} {x 10 y 20}) (+ x y))

;; Mixed: plain + destructuring bindings
(let ([a b] [3 4] z 5) (+ a (+ b z)))

;; Recursive let
(let ^rec (name init) body)

;; Named let (initializer list is sequential; lowers through let + let ^rec)
(let loop (x 0 acc nil)
  (if (= x 10) acc (loop (+ x 1) (cons x acc))))
```

### 3.4 `define` - Global Definition

```lisp
;; Simple define
(define name value)

;; Shorthand function define
(define (f x y) body)
;; desugars to: (define f (λ (x y) body))

;; Dictionary destructuring parameter
(define (connect {host port}) (tcp-connect host port))
;; called as: (connect {host "localhost" port 8080})

;; Typed function define (multiple dispatch)
(define (f (^Integer x)) "integer")
(define (f (^String x)) "string")
(define (f x) "other")  ;; fallback

;; Declaration attribute clause after define.
;; This is NOT an Array expression and NOT destructuring.
(define [macro] name
  (syntax-match
    (pattern1 (template ...))
    (pattern2 (template ...))
    ...))
;; clause-style macro forms are removed and parse as hard errors
(define [reader tag] name
  (syntax-match
    ([x] (template ...))))
;; #name form parses as (name form), so reader tag macros still use the normal
;; single-transformer macro contract
(define [type] Name (^Type field1) (^Type field2))
(define [struct] Name (^Type field1) (^Type field2))  ;; alias of [type]
(define [type] (Child Parent) (^Type field))
(define [abstract] Name)
(define [abstract] (Child Parent))
(define [union] (Name T) Variant1 (Variant2 T))
(define [alias] Name TargetType)
```

`[ ... ]` after `define` is declaration metadata consumed by the parser. It is
not equivalent to passing an Array value to `define`. In expression position,
`[ ... ]` remains the Array literal syntax.

### 3.5 `set!` - Variable Mutation

```lisp
(set! name value)
(set! instance.field value)      ;; struct field mutation
(set! instance.nested.field value) ;; nested mutation
(set! pair.car value)            ;; cons cell car mutation
(set! pair.cdr value)            ;; cons cell cdr mutation
(set! collection key value)      ;; generic collection update (Array/Dictionary)
```

`set!` returns `Void` on successful mutation.
`set!` dispatch target matrix:

| Surface | Dispatch target | Success result | Invalid-target behavior |
|---|---|---|---|
| `(set! name value)` | variable binding | `Void` | `set!: unbound variable` |
| `(set! root.seg... value)` | dot-path over `Instance` fields and cons `.car`/`.cdr` | `Void` | path errors (below) |
| `(set! collection key value)` | generic update on `Array`/`Dictionary` | `Void` | `set!: generic form expects array or dict target` |

Dot-path errors:
- `set!: unbound path root`
- `set!: field not found in path`
- `set!: path segment is not an instance or cons`
- `set!: cons only supports .car and .cdr`
- `set!: field not found`
- `set!: target is not an instance or cons`

### 3.6 `block` - Sequencing

```lisp
(block expr1 expr2 ... exprN)  ;; returns last value, TCO on last
```

### 3.7 `quote` / `quasiquote`

```lisp
(quote datum)        ;; or 'datum
`(a ,(+ 1 2) ,@(list 3 4))  ;; => (a 3 3 4)
```

### 3.8 `match` - Pattern Matching

```lisp
(match expr
  (pattern1 result1)
  (pattern2 result2)
  (_ default))
```

Patterns: literals, variables, wildcards `_`, sequences (`[a b ..]`, `[head .. tail]`, `[.. prev last]`), quoted `'sym`, constructors `(Some x)`.

### 3.9 `checkpoint` / `capture` - Delimited Continuations

```lisp
(checkpoint body)
(capture k body)
(checkpoint (+ 1 (capture k (k (k 10)))))  ;; => 12
```

- `capture` continuations are multi-shot: `k` may be invoked multiple times.
- Each `k` invocation replays the resumed continuation segment, including side effects in that segment (`set!`, signaled effects, handled I/O).
- Each invocation starts from the captured stack snapshot for that continuation segment.

### 3.10 `signal` / `handle` - Algebraic Effects

```lisp
(signal effect-tag argument)
(handle body
  (tag1 arg (resolve expr))    ; resuming handler
  (tag2 arg expr))             ; aborting handler
```

- `signal` signals an effect with a tag and argument
- `(resolve expr)` resumes the continuation with the value of `expr`
- Omitting `resolve` aborts — the handler's result replaces the entire `handle` expression
- I/O operations go through effects: `print`, `println`, `display`, `read-file`, etc. use `io/print`, `io/println`, etc. effect tags. When no handler is installed, a fast path calls the raw primitives directly.

### 3.11 `module` / `import`

```lisp
(module name (export sym1 sym2 ...)
  body...)

(import name)                   ; qualified-only: access via name.sym
(import name (sym1 sym2))       ; selective import
(import name (sym1 'as alias))  ; rename on import
(import name 'all)              ; import all exports unqualified
(import
  (math-utils (add 'as plus))
  (ui.nodes 'all)
  (json))                       ; grouped imports, same spec syntax
(export-from name (sym1))       ; re-export specific symbols
(export-from name 'all)         ; re-export all
```

Default import is qualified-only. Modules are cached; circular imports are detected.
Omni has no dedicated keyword type; `'as` and `'all` are quoted symbols in module forms.
Grouped imports are s-expression lists of ordinary import specs; `{}` remains
dictionary literal syntax only and is not valid for module selection.

### 3.12 Collection Literals

```lisp
;; Array literal — equivalent to (Array ...)
[1 2 3]                ;; => mutable array with elements 1, 2, 3
[]                     ;; => empty array

;; Dictionary literal — equivalent to (Dictionary ...)
{a 1 b 2}              ;; => mutable dict with symbol keys a, b
{}                     ;; => empty dict
{'a 1 "b" 2}           ;; quoted symbol and string keys remain explicit

;; Must have even number of elements in dict literal
;; {a 1 b}             ;; ERROR: odd count
```

Bare symbols in dict key position are automatically quoted. Non-symbol keys
still work explicitly:

```lisp
{(+ 1 2) "three"}
{42 "answer"}
```

### 3.13 String Interpolation

`str` is a stdlib macro that expands string-literal holes to `string-append`
and `String` coercions:

```lisp
(str "hello {name}, you are {age}")   ;; => string-append expansion
(str "literal braces: {{ and }}")
```

Each `{name}` hole is parsed as an expression at macro expansion time. Use
`{{` and `}}` for literal braces.

---

## 4. Type System

### 4.1 Type Annotations

```lisp
^Integer          ;; simple type
^(List Integer)   ;; compound type
^(Value 42)       ;; canonical value-level type (dispatch on literal)
^(Value nil)      ;; nil literal dispatch
^{'T Number}      ;; metadata dictionary
```

### 4.2 Type Definitions

```lisp
(define [type] Point (^Integer x) (^Integer y))
(define [abstract] Shape)
(define [type] (Circle Shape) (^Integer radius))
(define [union] (Option T) None (Some T))
(define [alias] Num Integer)
```

### 4.3 Multiple Dispatch

Typed defines create method tables. Best match wins:
- Value literal match: score 1000
- Exact type match: score 100
- Subtype match: score 10
- Any type (untyped param): score 1

---

## 5. Path and Index Notation

Removed leading-dot accessor forms (hard-error):

```lisp
.name
.3
.'key
.[expr]
```

Use `expr.name`, `expr.[key]`, and `ref` instead.
Compatibility/removal details are maintained in
`docs/SURFACE_COMPATIBILITY.md`.

Postfix index syntax:

```lisp
arr.[0]           ;; list/array index
dict.['key]       ;; dict key lookup
tensor.[i].[j]    ;; chained indexing
```

Path syntax:

```lisp
point.x           ;; struct field access
config.port       ;; dictionary symbol-key access
line.start.y      ;; nested field access
pair.car          ;; cons cell car access
pair.cdr          ;; cons cell cdr access
```

---

## 6. Truthiness

- **Falsy:** `nil`, `false`
- **Truthy:** Everything else (including `0`, `""`, `'()`)

---

## 7. Partial Application

Lambdas have **strict arity** — `(λ (x y) body)` requires exactly 2 arguments.

There are two explicit partial-application mechanisms plus pipeline sugar:

### 7.1 `_` Placeholder (parser-level desugaring)

The `_` token in a call expression creates a lambda at parse time. Works with any function:

```lisp
(+ 1 _)            ;; => (λ (__p1) (+ 1 __p1))
(f _ 2 _)          ;; => (λ (__p1 __p2) (f __p1 2 __p2))
(map (+ 1 _) xs)   ;; adds 1 to each element
(filter (> _ 0) xs) ;; keep positives
```

Indexed placeholders are also supported in call-argument position:

```lisp
(- _2 _1)           ;; => (λ (__p1 __p2) (- __p2 __p1))
(+ _1 _1)           ;; argument reuse
(+ _2 10)           ;; arity is 2 (highest index wins)
```

Rules:
- `_n` requires `n >= 1` (`_1`, `_2`, ...).
- Lambda arity is the highest referenced index in the call.
- Repeated indices reuse the same generated lambda parameter.
- Mixing `_` and `_n` in the same call is rejected.
- Invalid `_n` forms in call args (`_0`, `_-1`, `_1x`) are rejected.
- `_n` is not globally reserved syntax; outside call arguments it remains a normal symbol.

### 7.2 `|>` Pipe Operator (parser-level desugaring)

Left-fold that appends the piped value as the **last** argument:

```lisp
(|> 5 (+ 1) (* 2))    ;; => (* 2 (+ 1 5)) => 12
(|> data (filter odd?) (map square))
```

### 7.3 `partial` (stdlib function)

Runtime partial application — prepends initial args to a variadic lambda:

```lisp
(partial + 1)       ;; => variadic lambda that prepends 1 to args
((partial + 1) 2)   ;; => 3
```

---

## 8. Grammar (EBNF)

```ebnf
program     = { expr } ;
expr        = literal | symbol | path | quoted | quasiquoted
            | list | array_lit | dict_lit | indexed | accessor | reader_tag ;

literal     = integer | radix_integer | float | string ;
integer     = [ "-" ] digit { digit } ;
radix_integer = ( "#x" | "#b" | "#o" ) [ "-" ] radix_digit { radix_digit } ;
float       = [ "-" ] digit { digit } "." digit { digit } ;
string      = '"' { char | escape } '"' ;
symbol      = symbol_char { symbol_char } ;
path        = symbol "." symbol { "." symbol } ;

quoted      = "'" datum ;
quasiquoted = "`" datum ;
list        = "(" { expr } ")" ;
array_lit   = "[" { expr } "]" ;           (* equivalent to Array constructor call *)
dict_lit    = "{" { dict_key expr } "}" ;  (* equivalent to Dictionary constructor call; must be even *)
dict_key    = symbol | expr ;              (* bare symbol keys auto-quote *)
indexed     = expr ".[" expr "]" ;
accessor    = "." expr ;                   (* removed leading-dot form; must hard-error *)
reader_tag  = "#" symbol expr ;            (* #tag form parses as (tag form) *)

datum       = literal | symbol | "(" { datum } ")" | "'" datum ;

symbol_char = letter | digit | "_" | "-" | "+" | "*" | "/"
            | "=" | "<" | ">" | "!" | "?" | ":" | "." | "@"
            | "#" | "$" | "%" | "&" | "|" | "^" | "~" ;
```

---

## 9. Limits

| Resource | Limit |
|----------|-------|
| Symbol/string length | Dynamic (heap-allocated) |
| Max symbols | 8192 |
| Max values per region | 4096 |
| Max bindings per env frame | 512 |
| Effect clauses per handle | 64 |
| Path segments | 8 |
| Handler stack depth | 16 |
| Match clauses | 128 |
| Pattern elements | 16 |
| Type fields | 16 |
| Method table entries | 64 |
| Registered types | 256 |

---

*Omni Lisp — A Lisp with delimited continuations, algebraic effects, strict-arity lambdas, and multiple dispatch*
