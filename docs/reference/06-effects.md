# Effects, Continuations, Coroutines, Iterators & Error Handling

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 16. Algebraic Effects

Effects let you intercept, redirect, or suppress side effects without changing
the code that signals them.

### Basics: `signal` and `handle`

```lisp
(handle
  (+ 1 (signal read nil))
  (read x (resolve 41)))
;; => 42
```

1. Body starts evaluating `(+ 1 (signal read nil))`
2. `signal` signals the `read` effect with argument `nil`
3. Handler clause `(read x ...)` matches
4. `x` is bound to `nil`
5. `(resolve 41)` sends `41` back to the body
6. Body finishes: `(+ 1 41)` => `42`

### Resolve (Resume) vs Abort

```lisp
;; Resolve — body continues
(handle (signal double 5)
  (double x (resolve (* x 2))))
;; => 10

;; Abort — body abandoned
(handle (+ 1 (signal bail 42))
  (bail x x))
;; => 42
```

If `resolve` is not called, the handler's return value replaces the entire
`handle` expression.

Resolve discipline:
- `resolve` targets the current handler-bound continuation (`__k`).
- That continuation is single-shot for `resolve`; repeated `resolve` on the same
  continuation raises `runtime/continuation-resumed`.
- Use `with-continuation` and explicit `(k ...)` calls for multi-shot behavior.

### Multiple Clauses

```lisp
(handle
  (block
    (signal log "starting")
    (+ 1 (signal ask nil)))
  (log msg (resolve nil))
  (ask _   (resolve 41)))
;; => 42
```

### Declaring Custom Effects

```lisp
(define [effect] (my/ask (^String prompt)))

;; Now type-checked:
(signal my/ask "name?")    ;; OK
(signal my/ask 42)         ;; ERROR: 42 is not a String
```

Effect declarations are optional by language design — undeclared effects are
canonical and skip declaration-based type checking.

### I/O Effect Naming Policy

For runtime I/O surfaces, Omni uses operation-level effect tags (`io/tcp-connect`,
`io/tcp-read`, `io/tcp-close`, etc.) as the canonical contract. This is
intentional: handlers can intercept exactly one operation, and the no-handler
fast path maps each tag directly to one raw primitive.

Higher-level helper APIs may combine operations for ergonomics, but they should
be thin wrappers that signal these canonical operation tags rather than
introducing alternate runtime effect namespaces.

### Practical Patterns

**State:**
```lisp
(define (with-state initial thunk)
  (let (state initial)
    (handle (thunk)
      (get-state _ (resolve state))
      (set-state v (set! state v) (resolve nil)))))

(with-state 0
  (λ ()
    (signal set-state 10)
    (+ (signal get-state nil) 5)))   ;; => 15
```

**Logger:**
```lisp
(define (with-logger thunk)
  (let (logs '())
    (handle (block (thunk) (reverse logs))
      (log msg (set! logs (cons msg logs)) (resolve nil)))))

(with-logger
  (λ ()
    (signal log "step 1")
    (signal log "step 2")))
;; => ("step 1" "step 2")
```

**Dependency Injection:**
```lisp
(define (with-mock-db thunk)
  (handle (thunk)
    (db/query sql (resolve '((id 1) (name "test"))))))

(define (get-user) (signal db/query "SELECT * FROM users LIMIT 1"))

(with-mock-db get-user)
;; => ((id 1) (name "test"))
```

---

## 17. Delimited Continuations

### `checkpoint` / `capture`

```lisp
(checkpoint body)
(capture k body)
```

`capture` captures the continuation up to the enclosing `checkpoint` and binds it
to `k`. `k` is a function — `(k value)` resumes with `value`.

```lisp
(checkpoint (+ 1 (capture k (k 10))))
;; k = (λ (x) (+ 1 x))
;; => 11

;; Multi-shot: k can be called multiple times
(checkpoint (+ 1 (capture k (k (k 10)))))
;; => (+ 1 (+ 1 10)) => 12

;; Handled I/O replay in resumed segment
(block
  (define c 0)
  (checkpoint
    (+ (capture k (+ (k 1) (k 1)))
       (block
         (handle (signal io/println "x")
           (io/println msg (resolve 0)))
         (set! c (+ c 1))
         c))))
;; side effects in resumed segment execute per k invocation
```

### Semantics

- Continuations are **multi-shot** — each invocation clones the stack
- Continuation `resume` operation is invocation of the captured continuation:
  `(k value)`
- The result of `capture`'s body becomes the result of `checkpoint`
- Each `k` invocation replays the resumed continuation segment, so side effects in that segment (`set!`, signaled effects, handled I/O) execute per invocation in source order
- Each invocation starts from the captured stack snapshot; lexical locals in the resumed segment are restored per call unless mutation targets shared state outside that snapshot
- Replay-visible side effects are parity-bound: interpreter, JIT, and compiled execution must produce the same replay outcomes.
- Continuations run on dedicated mmap'd stacks (64KB) with guard pages
- x86_64 assembly context switching with FPU state isolation

Canonical continuation-resume example:

```lisp
(checkpoint (+ 1 (capture k (k 41))))
;; => 42
```

This continuation resume form is separate from the coroutine primitive
`(resume coroutine)`.

---

## 18. Coroutines

```lisp
(define counter
  (Coroutine (λ ()
    (yield 1)
    (yield 2)
    (yield 3))))

(resume counter)    ;; => 1
(resume counter)    ;; => 2
(resume counter)    ;; => 3
(resume counter)    ;; => nil (completed)
```

### API

| Primitive | Arity | Description |
|-----------|-------|-------------|
| `Coroutine` | 1 | Create from zero-arg thunk |
| `resume` | 1 | Resume, returns yielded/final value |
| `yield` | 1 | Yield value, suspend execution |
| `coroutine?` | 1 | Type check |

### Generator Pattern

```lisp
(define (fibonacci)
  (Coroutine (λ ()
    (let loop (a 0 b 1)
      (yield a)
      (loop b (+ a b))))))

(define fib (fibonacci))
(resume fib)   ;; => 0
(resume fib)   ;; => 1
(resume fib)   ;; => 1
(resume fib)   ;; => 2
(resume fib)   ;; => 3
```

Coroutines run on dedicated mmap'd stacks (64KB) with guard pages. They are
**not** built on effects — they use direct StackCtx-based context switching.

---

## 19. Iterators

Lazy sequences that compute values on demand.

### Creating Iterators

```lisp
(Iterator '(1 2 3))         ;; from list
(Iterator [1 2 3])          ;; from array
(Iterator {a 1 b 2})        ;; from dict (iterates keys)
(range-from 0)                ;; infinite: 0, 1, 2, 3, ...
(repeat 42)                   ;; infinite: 42, 42, 42, ...
(cycle [1 2 3])               ;; infinite: 1,2,3,1,2,3,...
```

### Lazy Combinators

```lisp
(map (+ 1 _) (Iterator '(1 2 3)))       ;; lazy +1 on each
(filter even? (Iterator '(1 2 3 4)))    ;; lazy keep evens
(take 5 (range-from 0))                  ;; first 5 naturals
(drop 3 (Iterator '(1 2 3 4 5)))        ;; skip first 3
(zip (Iterator '(1 2)) (Iterator '(a b))) ;; zip two iterators
```

### Consuming Iterators

```lisp
(list (take 5 (range-from 0)))          ;; => (0 1 2 3 4)
(Array (take 5 (range-from 0)))         ;; => [0 1 2 3 4]
(foldl + 0 (Iterator '(1 2 3 4 5)))     ;; => 15
```

### Pipeline Example

```lisp
;; First 3 even squares starting from 1
(list
  (take 3
    (filter even?
      (map (λ (x) (* x x))
        (range-from 1)))))
;; => (4 16 36)
```

### Dispatched Collection Ops on Iterators

`map`, `filter`, `take`, `drop`, `zip`, and `foldl` are dispatched.
Passing an iterator keeps lazy semantics:

```lisp
(map (+ 1 _) (Iterator '(1 2 3)))   ;; lazy iterator, not a list
(filter even? (Iterator '(1 2 3 4))) ;; lazy iterator
(take 3 (range-from 0))              ;; lazy iterator
```

---

## 20. Error Handling

Errors in Omni flow through the `raise` effect.

### `try` / `assert!`

```lisp
(try
  (λ (_)
    (assert! (> 1 2) "math is broken")
    "ok")
  (λ (msg) (str "caught: {msg}")))
;; => "caught: math is broken"
```

### Direct Handle

```lisp
(handle
  (block
    (println "before")
    (signal raise "oops")
    (println "after"))     ;; never executes
  (raise msg (str "error: {msg}")))
;; => "error: oops"
```

### Error Values

```lisp
(error "something went wrong")   ;; create error value
(error-message err)               ;; extract message
```
