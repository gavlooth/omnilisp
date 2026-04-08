# System, Reader Syntax & CLI Tooling

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 32. System & Misc

```lisp
;; Evaluation
(eval '(+ 1 2))             ;; => 3
(apply + '(1 2))            ;; => 3
(bound? 'x)                 ;; => true if x is defined

;; Symbols
(gensym)                     ;; => unique symbol
(string->symbol "foo")       ;; => foo
(symbol->string 'foo)        ;; => "foo"

;; Conversions
(exact->inexact 42)          ;; => 42.0
(inexact->exact 42.5)        ;; => 42

;; Sorting
(sort '(3 1 2))              ;; => (1 2 3)
(sort-by (lambda (a b) (> a b)) '(1 3 2))   ;; => (3 2 1)

;; Read
(read-string "(+ 1 2)")     ;; => Lisp value (unevaluated)

;; Memory
(unsafe-free! obj)           ;; manually free heap backing

;; Type predicates
(string? "hi")    (int? 42)      (double? 3.14)
(number? 42)      (symbol? 'x)   (closure? (lambda (x) x))
(list? '(1 2))    (array? [1])   (dict? {'a 1})
(pair? '(1 . 2))  (null? nil)    (boolean? true)
(procedure? +)    (continuation? k)  (coroutine? c)
```

---

## 33. Reader Syntax

### Comments

```lisp
; Line comment — extends to end of line

#| Block comment
   Can span multiple lines
   #| Nests correctly |#
|#

#_ (this form is skipped)        ;; form comment
#3_ (a) (b) (c)                  ;; skip next 3 forms
```

### Literals

```lisp
42                ;; integer
3.14              ;; float
"hello\nworld"    ;; string (escapes: \n \t \\ \")
'symbol           ;; quoted symbol
true              ;; boolean true (symbol)
false             ;; boolean false
nil               ;; nil value
()                ;; empty list (same as nil)
```

### Collection Literals

```lisp
'(1 2 3)          ;; quoted list
[1 2 3]           ;; array literal    -> (Array 1 2 3) (canonical constructor surface)
{'a 1 'b 2}       ;; dict literal     -> (Dictionary 'a 1 'b 2) (canonical constructor surface)
(Set "a" "b" "c") ;; set constructor
```

### Regex Literals

```lisp
#r"[0-9]+"        ;; compiled regex (no string escaping needed)
```

### Special Tokens

| Token | Description |
|-------|-------------|
| `_` | Wildcard in patterns, placeholder in calls |
| `..` | Rest/spread in patterns and variadic params |
| `.[` | Parser token used for postfix index parsing (`expr.[key]`) and leading-dot bracketed key expressions (`.[expr]`) |
| `.` | Field/path access |
| `^` | Type annotation prefix |
| `[` `]` | Arrays, patterns, bracket attributes |
| `{` `}` | Dicts, metadata dictionaries |

---

## 34. CLI & Tooling

### Running Programs

```bash
omni script.omni                  # run a script
omni --repl                       # start the REPL (explicit)
omni                              # start the REPL (default)
```

### Compilation

```bash
omni --compile input.omni output.c3   # Omni -> C3 source
omni --build input.omni -o output     # Omni -> standalone binary
```

`--build` stages one generated temp source under `build/_aot_temp_*.c3`, then
invokes `c3c compile` with the checked-in runtime sources plus that generated
file. Production AOT builds exclude `src/lisp/tests*`, but they still link the
runtime support libraries used by the shipped backend, including
`omni_chelpers`, GNU Lightning, libffi, libuv, replxx, utf8proc, libdeflate,
yyjson, BearSSL, LMDB, `libdl`, and `libm`.

### Project Management

```bash
omni --init myproject         # scaffold project directory
omni --bind myproject/        # generate FFI bindings from omni.toml
```

`--init` creates:
```
myproject/
  omni.toml              # project config
  src/main.omni          # entry point
  lib/ffi/               # auto-generated FFI bindings
  include/               # C headers
  build/project.json     # C3 build config (generated)
```

### omni.toml

```toml
[project]
name = "myproject"
version = "0.1.0"

[build]
output-dir = "build"
safety = "safe"           # safe | fast | none
opt = "O0"                # O0 | O1 | O2 | O3 | Os | Oz

[dependencies.ffi.math]
library = "m"
headers = ["/usr/include/math.h"]
functions = ["sin", "cos", "sqrt"]
```

### `--bind` Workflow

1. Edit `omni.toml` to declare FFI dependencies
2. Run `omni --bind myproject/`
3. Generated raw modules and facade stubs appear in `lib/ffi/` when header parsing succeeds without overrunning the current fixed bind scratch limits
4. Import with `(import "lib/ffi/math.omni")`
5. Execution-mode note: generated modules are declarative FFI and therefore currently target interpreter/JIT workflows; AOT currently rejects declarative `ffi` forms

Requires libclang (only for `--bind`, not for running programs).

### Other Flags

```bash
omni --version                # print version
omni --help                   # print help
```
