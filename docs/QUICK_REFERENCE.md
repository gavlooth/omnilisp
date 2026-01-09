# OmniLisp Quick Reference

**Implementation status:** this quick reference reflects the **current C compiler/runtime** unless a section is explicitly marked "Design Target" (see `omnilisp/SUMMARY.md` and `docs/LANGUAGE_PARITY_PLAN.md`).

## Lexical Rules

### Symbol Character Rules

Symbols can contain letters and certain operators, but have specific rules about which characters can appear where.

**Start with (first character):**
- Letters: `a-z`, `A-Z`
- Operators: `*`, `!`, `-`, `_`, `?`, `%`, `/`, `=`, `<`, `>`

**Excluded from start:**
- Digits: `0-9` (to avoid confusion with integers)
- Reserved syntax: `.`, `@`, `#`, `&`, `:`, `;`

**Middle/subsequent characters:**
- All of the above (letters + operators)
- **Plus:** Digits `0-9`

**Excluded entirely:**
- `.` - Used for module paths (`Math.sin`)
- `@` - Used for metadata (`^:where`)
- `#` - Used for reader macros (`#val`, `#\newline`)
- `&` - Excluded
- `:` - Used for type annotations (`{Type}`)
- `;` - Used for comments

**Convention (not enforced):**
`!` and `?` are typically only at the **start or end** of symbols:
- At end: `set!`, `define!`, `null?`, `empty?`
- At start: `!not`, `!null`, `?maybe`, `?value`
- Not in middle: `foo!bar`, `set!value` (conventionally weird)

**Examples:**
```scheme
; Valid symbols
foo                ; letters
foo-bar            ; - as separator
foo123             ; digits in middle
x1_y2              ; _ as separator
set!               ; ! at end
null?              ; ? at end
!not               ; ! at start
?maybe             ; ? at start
*                  ; single operator
<=                 ; comparison operators
==                 ; equality
50%off             ; % in middle
3/4                ; / in middle

; Invalid (can't start with digits)
123foo             ; integer, not symbol
3d                 ; digit first
7up                ; digit first

; Invalid (reserved for syntax)
.foo               ; . for paths
foo.bar            ; . for paths (not a single symbol)
@meta              ; @ for metadata
#reader           ; # for reader macros
&and               ; & excluded
:type              ; : for types
comment;more       ; ; for comments
```

## Data Types
```scheme
42              ; integer
3.14            ; float
'foo            ; symbol
#\a #\newline   ; character
'(1 2 3)        ; list
()              ; empty list
nothing         ; nothing (falsy)
false           ; false (falsy)
true            ; true
0               ; integer (truthy)
```

## Special Forms
**Implemented subset (current C compiler/runtime):** `define`, `lambda`/`fn`, `let`, `let*`, `if`, `do`/`begin`, and `quote`. Other forms listed below are design target.
```scheme
; Let bindings with destructuring
(let [x 10] [y 20]                    ; Simple bindings
  (+ x y))

(let [[x y z] my-vec]                 ; Array destructuring
  (+ x y z))

(let [(Point x y) my-point]            ; Constructor destructuring
  (* x y))

(let [x {Int} 10] [y {Int} 20]        ; Typed bindings
  (+ x y))

(let ^:seq [x 1] [y (+ x 1)]         ; Sequential (like let*)
  (* x y))

; Traditional list-style also supported
(let ((x 10) (y 20))
  (+ x y))

; Lambda/function
(lambda (x) body)                    ; Function
(lambda self (x) body)               ; Recursive function
(fn x y (* x y))                     ; Shorthand (without parens)

; Conditionals and control flow
(if cond then else)                  ; Conditional
(and e1 e2 ...)                      ; Short-circuit and
(or e1 e2 ...)                       ; Short-circuit or
(do e1 e2 ... en)                    ; Sequence, return last

; Quoting
(quote x) / 'x                       ; Quote
(quasiquote x) / `x                  ; Quasiquote
(unquote x) / ,x                     ; Unquote in quasiquote
(unquote-splicing x) / ,@x           ; Splice in quasiquote
```

## Pattern Matching (Design Target)

**Note:** `match` is not yet implemented in the current C compiler/runtime.
```scheme
(match val
  (_              result)      ; wildcard
  (x              result)      ; variable binding
  ((42)           result)      ; literal
  ((cons a b)     result)      ; constructor
  ((or p1 p2)     result)      ; or-pattern
  ((x @ pat)      result)      ; as-pattern
  ((n :when (> n 0)) result)   ; guard
  ((n :when (lambda (n) (> n 0))) result))  ; guard as lambda predicate
```

## Primitives
```scheme
; Arithmetic
(+ - * / %)

; Comparison
(= < > <= >= not)

; Lists
(cons car cdr null? list length append reverse)

; Higher-order
(map filter fold foldl apply compose flip)

; Introspection
(gensym sym-eq? eval trace)
```

## Staging
```scheme
(lift val)           ; value → code
(run code)           ; execute code (JIT)
(EM expr)            ; meta-level evaluation
(shift n expr)       ; go up n levels
(meta-level)         ; current tower level
(clambda (x) body)   ; compile lambda
```

## Handlers
```scheme
(get-meta 'name)                    ; get handler
(set-meta! 'name fn)                ; set handler
(with-handlers ((name fn)) body)    ; scoped handlers
(default-handler 'name arg)         ; call default

; Handler names: lit var lam app if lft run em clam
```

## Macros
```scheme
(defmacro name (params) body scope)
(mcall macro-name args...)
(macroexpand expr)
```

## FFI
```scheme
(ffi "func" args...)
(ffi-declare "ret" "name" "arg1" ...)
```

## Errors
```scheme
(error msg)
(try expr handler)
(assert cond)
(assert cond msg)
```

## CLI
```bash
omnilisp                    # REPL
omnilisp -e '(+ 1 2)'       # eval expression
omnilisp file.omni        # run file
omnilisp -c file.omni     # compile to C
omnilisp -c -o out.c file   # compile to file
omnilisp -v file            # verbose mode
```

## REPL Commands
```
help      - show help
quit      - exit
compile   - toggle compile mode
macros    - list macros
runtime   - print C runtime
```
