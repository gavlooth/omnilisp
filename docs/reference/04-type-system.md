# Type System & Multiple Dispatch

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 12. Type System

### Struct Types

```lisp
(define [type] Point (^Int x) (^Int y))

(Point 3 4)           ;; construction
(define p (Point 3 4))
p.x                    ;; => 3 (field access)
p.[0]                  ;; => 3 (positional access)
(set! p.x 99)         ;; field mutation
```

### Type Inheritance

```lisp
(define [abstract] Shape)
(define [type] (Circle Shape) (^Int radius))
(define [type] (Rect Shape) (^Int width) (^Int height))

(is? (Circle 5) 'Shape)    ;; => true
(is? (Circle 5) 'Circle)   ;; => true
(type-of (Circle 5))        ;; => Circle
```

### Union Types (ADTs)

```lisp
(define [union] (Option T) None (Some T))
(define [union] (Result T E) (Ok T) (Err E))

;; Construction
None              ;; nullary variant
(Some 42)         ;; variant with value

;; Pattern matching
(match (Some 42)
  (None "empty")
  ((Some x) x))   ;; => 42
```

### Type Aliases

```lisp
(define [alias] Num Int)
(define [alias] Text String)
```

### Parametric Types

```lisp
(define [type] (Box T) (^T value))

(Box 42)                ;; infers Box{Int}
(type-args (Box 42))    ;; => (Int)
```

### Type Introspection

```lisp
(type-of 42)                ;; => Int
(type-of "hello")           ;; => String
(type-of (Point 1 2))       ;; => Point
(is? 42 'Int)               ;; => true
(is? (Circle 5) 'Shape)     ;; => true (walks parent chain)
(instance? (Point 1 2))     ;; => true
(instance? 42)              ;; => nil
```

---

## 13. Multiple Dispatch

Define multiple implementations with typed parameters. The best match wins:

```lisp
(define (describe (^Int n)) "integer")
(define (describe (^String s)) "string")
(define (describe x) "other")

(describe 42)       ;; => "integer"
(describe "hi")     ;; => "string"
(describe '(1 2))   ;; => "other"
```

### Multi-Argument Dispatch

```lisp
(define (add2 (^Int a) (^Int b)) (+ a b))
(define (add2 (^String a) (^String b)) (string-append a b))

(add2 3 4)                ;; => 7
(add2 "hello" " world")   ;; => "hello world"
```

### Val Dispatch (Value-Level Matching)

```lisp
(define (fib (^(Val 0) n)) 0)
(define (fib (^(Val 1) n)) 1)
(define (fib (^Int n)) (+ (fib (- n 1)) (fib (- n 2))))

(fib 10)    ;; => 55
```

### Dispatch Scoring

| Match | Score | Example |
|-------|-------|---------|
| Val literal | 1000 | `^(Val 42)` matches value 42 |
| Exact type | 100 | `^Int` matches INT value |
| Subtype | 10 | `^Shape` matches Circle |
| Any (untyped) | 1 | Untyped param matches anything |

Highest total score wins. Ties broken by first-registered.

### Type Hierarchy Example

```lisp
(define [abstract] Shape)
(define [type] (Circle Shape) (^Int radius))
(define [type] (Rect Shape) (^Int width) (^Int height))

(define (area (^Circle c)) (* pi (* c.radius c.radius)))
(define (area (^Rect r))   (* r.width r.height))

(area (Circle 5))    ;; => ~78.54
(area (Rect 3 4))    ;; => 12
```
