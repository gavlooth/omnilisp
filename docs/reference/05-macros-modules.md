# Macros & Modules

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 14. Macros

### Pattern-Based Macros

```lisp
(define [macro] when
  ([test .. body] (if test (begin .. body) nil)))

(define [macro] unless
  ([test .. body] (if test nil (begin .. body))))

(define [macro] cond
  ([] nil)
  ([test body .. rest] (if test body (cond .. rest))))
```

### Usage

```lisp
(when (> x 0)
  (println "positive")
  x)

(cond
  (< x 0) "negative"
  (= x 0) "zero"
  true     "positive")
```

### Features

- **Pattern-based**: each clause matches argument structure
- **Hygienic**: template literals resolve at definition time
- **Auto-gensym**: `name#` in templates generates unique symbols
- **`gensym`** function for manual hygiene
- Up to 8 clauses per macro

### Expansion

```lisp
(macroexpand '(when true 1 2 3))
;; => (if true (begin 1 2 3) nil)
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
(import math-utils (add :as plus))
(plus 3 4)                   ;; => 7

;; Import all exports unqualified
(import math-utils :all)
(add 3 4)                    ;; => 7

;; File-based import
(import "path/to/file.omni")

;; Re-export
(export-from math-utils :all)
(export-from math-utils (add))
```

### Features

- Default import is **qualified-only**
- Modules loaded only once (cached)
- Circular import detection
- Method extensions are always global (dispatch is cross-cutting)
