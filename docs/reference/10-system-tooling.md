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
(Symbol "foo")       ;; => foo
(String 'foo)        ;; => "foo"

;; Conversions
(Float64 42)          ;; => 42.0
(Integer 42.5)        ;; => 42

;; Sorting
(sort '(3 1 2))              ;; => (1 2 3)
(sort-by (λ (a b) (> a b)) '(1 3 2))   ;; => (3 2 1)
;; `sort-by` also accepts curried comparators. Positive numeric or truthy results
;; move the left item after the right item; `nil` and `false` do not.

;; Read
(read-string "(+ 1 2)")     ;; => Lisp value (unevaluated)

;; Memory
(unsafe-free! obj)           ;; manually free heap backing

;; Type predicates
(string? "hi")    (int? 42)      (float64? 3.14)
(number? 42)      (symbol? 'x)   (closure? (λ (x) x))
(list? '(1 2))    (array? [1])   (dict? {a 1})
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
{a 1 b 2}         ;; dict literal     -> (Dictionary 'a 1 'b 2) (canonical constructor surface)
#xFF              ;; hex integer      -> 255
#b1010            ;; binary integer   -> 10
#o755             ;; octal integer    -> 493
#hex "ff0a"       ;; byte array       -> [255 10]
#base64 "SGVsbG8=" ;; byte array     -> [72 101 108 108 111]
#json "{\"ok\": true}"
#toml "port = 8080"
#time "2024-01-15T10:30:00Z"
#uuid "550e8400-e29b-41d4-a716-446655440000"
(Set "a" "b" "c") ;; set constructor
```

### Regex Literals

```lisp
#r"[0-9]+"        ;; compiled regex (no string escaping needed)
```

### Reader Tags

```lisp
(define (reader-inc x) (+ x 1))
#reader-inc 41    ;; parses as (reader-inc 41), evaluates to 42

(define [reader tag] reader-Float64
  (syntax-match
    ([x] (template (+ (insert x) (insert x))))))

#reader-Float64 21 ;; parses as (reader-Float64 21), macro-expands to 42
```

### Special Tokens

| Token | Description |
|-------|-------------|
| `_` | Wildcard in patterns, placeholder in calls |
| `..` | Rest/spread in patterns and variadic params |
| `.[` | Parser token used for postfix index parsing (`expr.[key]`) and leading-dot bracketed key expressions (`.[expr]`) |
| `.` | Field/path access |
| `^` | Type annotation prefix |
| `[` `]` | Array literals in expression position; declaration attribute clauses after `define`; patterns |
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
# raw-syntax = "grouped"  # optional: legacy (default) or grouped
# strip-prefixes = ["sqlite3_"]  # optional: facade-name prefix stripping
```

### `--bind` Workflow

1. Edit `omni.toml` to declare FFI dependencies
2. Run `omni --bind myproject/`
3. Generated raw modules and facade stubs appear in `lib/ffi/` when header parsing succeeds without overrunning the current fixed bind scratch limits
4. Import with `(import "lib/ffi/math.omni")`
5. Execution-mode note: generated modules are declarative FFI. Interpreter/JIT enforces `ForeignHandle` metadata dictionaries; AOT carries the same policy into generated FFI declarations with handle descriptors for parameters and returns.

Requires libclang (only for `--bind`, not for running programs).

By default, `--bind` output still uses `[ffi lib]` plus `[ffi λ]`. Grouped
`[ffi module]` output is available per FFI dependency with
quoted `raw-syntax = "grouped"`; unquoted or unsupported values fail closed
before header parsing.
Grouped raw output includes `;; Raw syntax: grouped` in the generated header so
review diffs expose the selected raw syntax explicitly.
`strip-prefixes = ["prefix_"]` strips matching C prefixes from generated facade
names and `raw-*` aliases while raw binding names keep the C-derived symbol
names used by `dlsym`. Malformed, empty, or overlong `strip-prefixes` entries
fail the dependency before header parsing.
The same fail-closed parsing applies to the bind-only `library`, `headers`, and
`functions` fields so missing required, malformed, empty, overlong, or
incorrectly shaped values cannot be truncated, accepted loosely, or treated as
no-op dependencies.
Adjacent quoted scalar tails, such as `library = "sqlite3" "m"`, are treated as
malformed and fail closed.
Unsupported keys in `[dependencies.ffi.NAME]` fail closed before header parsing;
future extension fields must be implemented deliberately instead of being
accepted as no-ops.
Unsafe `library` stems containing slash, backslash, quote, whitespace, or
control characters fail before generated raw/facade/manifest output.
Explicit `functions = [...]` filters require every listed C function to be
found in parsed headers; missing entries fail before generated output.
If a later header fails after earlier headers were parsed, `--bind` releases
the earlier parsed function metadata before failing and does not write partial
outputs.
Inline `#` comments after section headers are accepted. Malformed
section-header lines starting with `[` reset parser context so following keys
cannot mutate the previously active dependency section.
If bindgen writes a new raw module but facade generation then fails, the new
raw file is removed before returning failure. Existing raw files are left in
place on rerun failures.
If raw/facade generation succeeds but manifest writing then fails, first-time
raw, facade, and manifest artifacts are cleaned before dependency failure.
Existing raw, facade, or manifest artifacts are left in place on rerun failures.
Raw, facade, and manifest text writers use sibling temp paths and rename into
place only after a full write and close succeeds. Failed final renames clean
their temp output instead of replacing the target with a partial file.
Anonymous C parameter fallback names format the full numeric index (`arg123`,
etc.) so high-arity generated bindings do not emit invalid fallback symbols.
Repeated bind dependency keys fail closed instead of using last-write-wins
semantics for the library target, function filter, raw syntax mode, or
generated-name rewrite policy.
Repeated `[dependencies.ffi.NAME]` sections also fail closed so two dependencies
cannot target the same generated raw, facade, or manifest output stem.
Dependency section count overflow fails closed before header parsing or output
generation, and overflow section keys cannot mutate the last accepted
dependency.
Inline `#` comments are ignored outside quoted bind dependency values and
preserved inside quoted strings.
The same plan tracks fail-closed `omni.toml` extension fields such as
`exclude-functions`, `clang-args`, `mode`, `facade`, `name-style`,
`ownership-policy`, `output-raw`, `output-facade`, and `generator`.
The shipped output-format fields are `raw-syntax`, facade-only
`strip-prefixes`, `exclude-functions`, `mode`, and `generator`, with
`generator = "cppinterop"` constrained to `mode = "api"`.

### Other Flags

```bash
omni --version                # print version
omni --help                   # print help
```
