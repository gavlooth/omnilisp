# Macros & Modules

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 14. Macros

Macro declarations use the `define` attribute clause. The bracketed clause is
declaration metadata, not an Array literal:

```lisp
(define [macro] name ...)
```

### Pattern-Based Macros

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

### Usage

```lisp
(when (> x 0)
  (println "positive")
  x)

(branch ((< x 0) "negative")
        ((= x 0) "zero")
        (_ "positive"))
```

### Features

- **Single-surface**: define one transformer with `syntax-match` branches
- **Hygienic**: template literals resolve at definition time
- **Auto-gensym**: `name#` in templates generates unique symbols
- **`gensym`** function for manual hygiene
- Legacy clause-style macro forms are rejected with migration diagnostics

### Reader Tag Macros

Reader tags are constrained syntax: `#tag form` parses as `(tag form)`.
Use an ordinary function for evaluated tag calls, or define a tag macro with
the canonical reader-tag attribute:

```lisp
(define [reader tag] reader-double
  (syntax-match
    ([x] (template (+ (insert x) (insert x))))))

#reader-double 21 ;; => 42
```

### Expansion

```lisp
(macroexpand '(when true 1 2 3))
;; => (if true (block 1 2 3) nil)
```

---

## 15. Modules

### Definition

```lisp
(module math-utils (export add multiply)
  (define (add a b) (+ a b))
  (define (multiply a b) (* a b)))
```

### Import Styles

```lisp
;; Qualified access (default)
(import math-utils)
(math-utils.add 3 4)        ;; => 7

;; Selective import
(import math-utils (add multiply))
(add 3 4)                    ;; => 7

;; Rename on import
(import math-utils (add 'as plus))
(plus 3 4)                   ;; => 7

;; Import all exports unqualified
(import math-utils 'all)
(add 3 4)                    ;; => 7

;; Scoped open: export bindings are local to the body
(with math-utils
  (add 3 4)
  (multiply 2 5))             ;; => 10

;; Dotted/path module target
(import ui.nodes)
(ui.nodes.text "ok")

;; File-based import
(import "path/to/file.omni")

;; Re-export
(export-from math-utils 'all)
(export-from math-utils (add))
(export-from ui.nodes (text))

;; Core scientific modules are prebound.
(math.erf 1.0)
(stats.normal-cdf 0.0)
```

### Features

- Default import is **qualified-only**
- Module targets for `module` / `import` / `export-from` can be symbol, dotted/path token, or string file path
- Modules loaded only once (cached)
- Circular import detection
- Scoped module open with `(with mod body...)`; exported names are visible only
  in the child body environment
- Method extensions are always global (dispatch is cross-cutting)
- `module` / `import` / `export-from` return `Void` on success
- `math` and `stats` are always-available core scientific module values. Use
  dotted access such as `math.erf` and `stats.normal-cdf`; old slash scientific
  spellings are transitional single-symbol primitives.
- Compiler backend (`AOT`) currently treats module surfaces as static lowering: module bodies are inlined, while `import` / `export-from` compile to command-style `Void` no-ops
