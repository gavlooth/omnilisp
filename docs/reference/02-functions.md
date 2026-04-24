# Functions & Partial Application

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 7. Functions

### Strict Arity

Lambdas enforce exact argument counts:

```lisp
(define (add x y) (+ x y))
(add 1 2)       ;; => 3
(add 1)         ;; ERROR: arity mismatch
(add 1 2 3)     ;; ERROR: arity mismatch
```

### Closures

Lambdas capture their lexical environment:

```lisp
(define (make-adder n)
  (λ (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)       ;; => 15
```

### Variadic Functions

```lisp
(define (log msg .. args)
  (println msg)
  (for-each println args))

(log "hello" 1 2 3)
;; prints: hello, 1, 2, 3
```

### Tail-Call Optimization

The following positions are tail positions (no stack growth):
- `if`: both branches
- `let`: body
- `block`: last expression
- `and`/`or`: second operand
- `match`: clause bodies

Named let is the idiomatic loop construct:

```lisp
;; Sum 1 to n (tail recursive)
(define (sum-to n)
  (let loop (i n acc 0)
    (if (= i 0)
      acc
      (loop (- i 1) (+ acc i)))))

(sum-to 1000000)   ;; works, no stack overflow
```

### Implicit Block

`λ`/`lambda`, shorthand function `define`, `let`, `let ^rec`, and named `let`
bodies support multiple expressions:

```lisp
(define (f x)
  (println "computing...")
  (* x x))   ;; last expression is the return value
```

---

## 8. Partial Application & Pipe

Omni lambdas have strict arity. Omni keeps two explicit partial-application
mechanisms plus pipeline sugar:

### `_` Placeholder

The `_` token in a call creates a lambda at parse time:

```lisp
(+ 1 _)                    ;; => (λ (__p1) (+ 1 __p1))
(f _ 2 _)                  ;; => (λ (__p1 __p2) (f __p1 2 __p2))
(map (+ 1 _) '(1 2 3))     ;; => (2 3 4)
(filter (> _ 0) '(-1 0 1))  ;; => (1)
```

### `|>` Pipe Operator

Left-fold that appends the piped value as the last argument:

```lisp
(|> 5 (+ 1) (* 2))         ;; => (* 2 (+ 1 5)) => 12
(|> '(1 2 3 4 5)
  (filter (> _ 2))
  (map (* 2 _)))            ;; => (6 8 10)
```

### `partial` (Stdlib)

Runtime partial application for any function:

```lisp
(define add3 (partial + 1 2))
(add3 3)   ;; => 6
```
