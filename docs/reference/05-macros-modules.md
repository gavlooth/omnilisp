# Macros & Modules

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 14. Macros

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

;; File-based import
(import "path/to/file.omni")

;; Re-export
(export-from math-utils 'all)
(export-from math-utils (add))
```

### Features

- Default import is **qualified-only**
- Modules loaded only once (cached)
- Circular import detection
- Method extensions are always global (dispatch is cross-cutting)
- `module` / `import` / `export-from` return `Void` on success
- Compiler backend (`AOT`) currently treats module surfaces as static lowering: module bodies are inlined, while `import` / `export-from` compile to command-style `Void` no-ops
