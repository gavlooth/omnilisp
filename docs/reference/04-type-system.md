# Type System & Multiple Dispatch

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 12. Type System

### Struct Types

```lisp
(define [type] Point (^Integer x) (^Integer y))

(Point 3 4)           ;; construction
(define p (Point 3 4))
p.x                    ;; => 3 (field access)
p.[0]                  ;; => 3 (positional access)
(set! p.x 99)         ;; field mutation
```

### Type Inheritance

```lisp
(define [abstract] Shape)
(define [type] (Circle Shape) (^Integer radius))
(define [type] (Rect Shape) (^Integer width) (^Integer height))

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
(define [alias] Num Integer)
(define [alias] Text String)
```

### Parametric Types

```lisp
(define [type] (Box T) (^T value))

(Box 42)                ;; infers Box{Int}
(type-args (Box 42))    ;; => (Int)
```

### Callable Core Type Symbols

```lisp
(Integer 3.9)        ;; => 3
(Double 3)           ;; => 3.0
(String 3)           ;; => "3"
(Symbol "name")      ;; => 'name
(Boolean 0)          ;; => true
(Boolean nil)        ;; => nil
(Nil nil)            ;; => nil
(Closure (lambda (x) x))
(Coroutine (lambda () 1))
(List [1 2 3])       ;; => '(1 2 3)
(Array '(1 2 3))     ;; => [1 2 3]
(Dictionary 'a 1 'b 2) ;; => {'a 1 'b 2}
(Set 1 2 3)          ;; => (Set 1 2 3)
(Iterator [1 2 3])   ;; lazy iterator over the array
(TimePoint 'date 2026 3 7)
```

User-defined types and selected builtin/runtime types therefore share the same
“type symbol in type position, constructor/coercion in value position” surface.
Canonical names favor descriptiveness over terseness: `Integer`, `Boolean`, and
`Dictionary` are the primary spellings, while `Int`, `Bool`, and `Dict` remain
accepted compatibility aliases.

There is no builtin `Empty` type today. Use `Nil` for the language-level empty
value, and treat `Void` as an FFI / no-result annotation rather than a normal
value type.

### Type Introspection

```lisp
(type-of 42)                ;; => Integer
(type-of "hello")           ;; => String
(type-of (Dictionary 'a 1)) ;; => Dictionary
(type-of (Set 1 2 3))       ;; => Set
(type-of (Point 1 2))       ;; => Point
(is? 42 'Integer)           ;; => true
(is? (Circle 5) 'Shape)     ;; => true (walks parent chain)
(instance? (Point 1 2))     ;; => true
(instance? 42)              ;; => nil
```

`type-of` returns a symbol such as `Dictionary` or `Set`. A first-class type
descriptor value prints as `#<type Dictionary>`, while ordinary collection values
print structurally as `{'a 1}` or `(Set 1 2 3)`.

---

## 13. Multiple Dispatch

Define multiple implementations with typed parameters. The best match wins:

```lisp
(define (describe (^Integer n)) "integer")
(define (describe (^String s)) "string")
(define (describe x) "other")

(describe 42)       ;; => "integer"
(describe "hi")     ;; => "string"
(describe '(1 2))   ;; => "other"
```

### Multi-Argument Dispatch

```lisp
(define (add2 (^Integer a) (^Integer b)) (+ a b))
(define (add2 (^String a) (^String b)) (string-append a b))

(add2 3 4)                ;; => 7
(add2 "hello" " world")   ;; => "hello world"
```

### Value Dispatch (Value-Level Matching)

```lisp
(define (fib (^(Value 0) n)) 0)
(define (fib (^(Value 1) n)) 1)
(define (fib (^Integer n)) (+ (fib (- n 1)) (fib (- n 2))))

(define (udp (^(Value open) cmd)) (io/udp-open))
(define (udp (^(Value bind) cmd) h host port) (io/udp-bind h host port))
(define (udp (^(Value send) cmd) h host port payload) (io/udp-send h host port payload))

(fib 10)    ;; => 55
```

`Value` is the only supported constructor for value-literal dispatch.
Supported literals in this position are integers, symbols, strings, and booleans (`true`/`false` symbols).
Command-style facades should delegate to canonical `io/udp-*` operations. Module packaging for façade surfaces is deferred; core surface remains canonical `io/*`.

### Dispatch Scoring

| Match | Score | Example |
|-------|-------|---------|
| Value literal | 1000 | `^(Value 42)`, `^(Value open)`, `^(Value "open")`, `^(Value true)` |
| Exact type | 100 | `^Integer` matches INT value |
| Subtype | 10 | `^Shape` matches Circle |
| Any (untyped) | 1 | Untyped param matches anything |

Highest total score wins. Ties broken by first-registered.

### Type Hierarchy Example

```lisp
(define [abstract] Shape)
(define [type] (Circle Shape) (^Integer radius))
(define [type] (Rect Shape) (^Integer width) (^Integer height))

(define (area (^Circle c)) (* pi (* c.radius c.radius)))
(define (area (^Rect r))   (* r.width r.height))

(area (Circle 5))    ;; => ~78.54
(area (Rect 3 4))    ;; => 12
```
