# Omni Lisp Effects Guide

Effects are Omni's way of handling side effects — I/O, errors, state — as
**values you can intercept, redirect, or suppress**. Instead of calling
`println` and having output go directly to the terminal, `println` *signals*
an effect. If nobody is listening, the output goes to the terminal as usual.
If a handler is installed, it gets to decide what happens.

This means you can test code without touching the filesystem, capture printed
output to a string, or swap a real database for an in-memory mock — all without
changing the code that signals the effects.

---

## 1. The basics: `signal` and `handle`

An effect has two sides:

- **`signal`** — asks for something to happen
- **`handle`** — listens and decides what to do

```lisp
(handle
  (+ 1 (signal read nil))
  (read x (respond 41)))
;; => 42
```

What happened:
1. The body starts evaluating `(+ 1 (signal read nil))`
2. `signal` asks for the `read` effect with argument `nil`
3. The handler clause `(read x ...)` matches — because the tag `read` matches
4. `x` is bound to the argument (`nil` in this case)
5. `(respond 41)` sends `41` back to the body, substituting it where `signal` was
6. The body finishes: `(+ 1 41)` => `42`

Each handler clause has two parts — what to match and what to do:

```
(tag  arg   body...)
 |     |      |
 |     |      └── code that runs when this effect is signalled
 |     └── bound to the value from (signal tag value)
 └── matched against the effect tag
```

---

## 2. Responding and aborting

### Respond: send a value back

`(respond value)` sends a value back to the body. The body continues
as if `signal` returned that value.

```lisp
(handle
  (* 2 (signal get-factor nil))
  (get-factor x (respond 21)))
;; => 42   — body gets 21, computes (* 2 21)
```

### Abort: don't respond

If you don't call `respond`, the handler's return value becomes the result
of the entire `handle` expression. The body is abandoned.

```lisp
(handle
  (+ 1 (+ 2 (+ 3 (signal bail 42))))
  (bail x x))
;; => 42   — the entire (+ 1 (+ 2 ...)) is abandoned
```

This is how you implement early return, exceptions, or short-circuit logic.

### Transform: respond with a modified value

```lisp
(handle
  (signal double 5)
  (double x (respond (* x 2))))
;; => 10
```

---

## 3. Multiple signals in one body

A handler can intercept many `signal` calls — each one pauses, runs the
handler, and responds (if `respond` is called).

```lisp
(handle
  (+ (signal bump 10) (signal bump 20))
  (bump x (respond (+ x 1))))
;; => 32   — first bump gives 11, second gives 21
```

### Multiple clauses

Handle different effect tags in the same block:

```lisp
(handle
  (begin
    (signal log "starting")
    (+ 1 (signal ask nil)))
  (log msg (respond nil))
  (ask _   (respond 41)))
;; returns 42
```

---

## 4. I/O effects

`print`, `println`, `display`, and `newline` are all effect-based.
Under the hood:

```lisp
(define print   (lambda (x) (signal io/print x)))
(define println (lambda (x) (signal io/println x)))
(define display (lambda (x) (signal io/display x)))
(define newline (lambda ()  (signal io/newline nil)))
```

When no handler is installed, these take a **fast path** — they call the raw
I/O primitives directly with zero overhead. You don't pay for effects you
don't use.

### Suppress output

```lisp
(handle (begin (println "you won't see this") 42)
  (io/println x (respond nil)))
;; => 42   — nothing printed
```

### Capture output to a string

```lisp
(handle (begin (println "captured") nil)
  (io/println x x))
;; => "captured"
```

This works because the handler catches the effect and returns `x` (the string)
without calling `respond` — so the body aborts and the string becomes the result.

### Capture all output (with respond)

```lisp
(define (with-output-to-string thunk)
  (let ((buf ""))
    (handle (begin (thunk) buf)
      (io/println x
        (set! buf (string-append buf (number->string x) "\n"))
        (respond nil))
      (io/print x
        (set! buf (string-append buf (number->string x)))
        (respond nil))
      (io/display x
        (set! buf (string-append buf (number->string x)))
        (respond nil)))))

(with-output-to-string
  (lambda ()
    (println 1)
    (println 2)
    (println 3)))
;; => "1\n2\n3\n"
```

The handler intercepts each print call, appends to the buffer, and responds
so the thunk keeps running. After the thunk finishes, the buffer is returned.

### File I/O effects

```lisp
(read-file "data.txt")        ;; signals io/read-file
(write-file "out.txt" "data") ;; signals io/write-file
(file-exists? "data.txt")     ;; signals io/file-exists?
(read-lines "data.txt")       ;; signals io/read-lines
```

These can all be intercepted for testing:

```lisp
;; Mock the filesystem
(handle
  (read-file "config.txt")
  (io/read-file path (respond "mocked content")))
;; => "mocked content"   — no file was read
```

---

## 5. Declaring custom effects

Use `define [effect]` to declare an effect with a type constraint:

```lisp
(define [effect] (my/ask (^String prompt)))
```

Now `(signal my/ask 42)` will raise a type error because `42` is not a
string. `(signal my/ask "name?")` works.

```lisp
;; Accept any type
(define [effect] (log (^Any msg)))

;; Accept only numbers
(define [effect] (scale (^Number factor)))
```

Effects don't *need* to be declared — undeclared effects work fine, they just
skip type checking:

```lisp
(handle (signal my-tag 42) (my-tag x (respond (+ x 1))))
;; => 43   — works without a declaration
```

---

## 6. Error handling with `raise`

The stdlib declares a `raise` effect and provides `try` and `assert!`:

```lisp
(define [effect] (raise (^Any msg)))

(define (try thunk handler)
  (handle (thunk nil)
    (raise msg (handler msg))))

(define (assert! condition msg)
  (if condition true (signal raise msg)))
```

### Using try/raise

```lisp
(try
  (lambda (_)
    (assert! (> 1 2) "math is broken")
    "ok")
  (lambda (msg) (string-append "caught: " msg)))
;; => "caught: math is broken"
```

### Direct handle

```lisp
(handle
  (begin
    (println "before")
    (signal raise "oops")
    (println "after"))
  (raise msg (string-append "error: " msg)))
;; prints "before", returns "error: oops"
;; "after" never executes (respond was not called)
```

---

## 7. Nested handlers

Handlers form a stack. The innermost handler gets first chance to match:

```lisp
(handle
  (handle
    (signal greet "world")
    (greet x (respond (string-append "hello, " x))))
  (greet x (respond (string-append "hi, " x))))
;; => "hello, world"   — inner handler matched first
```

If the inner handler doesn't match the effect tag, it propagates outward:

```lisp
(handle
  (handle
    (signal outer-tag 1)
    (inner-tag x (respond 0)))
  (outer-tag x (respond (+ x 99))))
;; => 100   — inner didn't match, outer caught it
```

---

## 8. Handlers as functions

Since handlers are just code that wraps a thunk, you can define them as
regular functions and compose them:

```lisp
;; A handler is just a function: thunk -> result
(define (silent-io thunk)
  (handle (thunk)
    (io/print x   (respond nil))
    (io/println x  (respond nil))
    (io/display x  (respond nil))))

(define (catch-errors thunk)
  (handle (thunk)
    (raise msg msg)))

;; Use by nesting
(catch-errors
  (lambda () (silent-io
    (lambda ()
      (println "suppressed")
      (signal raise "oops")))))
;; => "oops"

;; Or compose with a helper
(define (with-handlers handlers thunk)
  (foldr (lambda (h acc) (lambda () (h acc))) thunk handlers))

(with-handlers (list catch-errors silent-io)
  (lambda ()
    (println "gone")
    (signal raise "fail")))
;; => "fail"
```

---

## 9. Practical patterns

### State effect

```lisp
(define [effect] (get-state (^Any _)))
(define [effect] (set-state (^Any v)))

(define (with-state initial thunk)
  (let ((state initial))
    (handle (thunk)
      (get-state _ (respond state))
      (set-state v
        (set! state v)
        (respond nil)))))

(with-state 0
  (lambda ()
    (signal set-state 10)
    (+ (signal get-state nil) 5)))
;; => 15
```

### Logger effect

```lisp
(define [effect] (log (^Any msg)))

(define (with-logger thunk)
  (let ((logs '()))
    (handle (begin (thunk) (reverse logs))
      (log msg
        (set! logs (cons msg logs))
        (respond nil)))))

(with-logger
  (lambda ()
    (signal log "step 1")
    (signal log "step 2")
    (signal log "done")))
;; => ("step 1" "step 2" "done")
```

### Dependency injection

```lisp
(define [effect] (db/query (^String sql)))

;; Production handler
(define (with-real-db thunk)
  (handle (thunk)
    (db/query sql (respond (real-db-query sql)))))

;; Test handler
(define (with-mock-db thunk)
  (handle (thunk)
    (db/query sql (respond '((id 1) (name "test"))))))

;; Same code, different handler
(define (get-user)
  (signal db/query "SELECT * FROM users LIMIT 1"))

(with-mock-db get-user)
;; => ((id 1) (name "test"))
```

---

## 10. Advanced: multi-shot continuations

In rare cases you need to resume the same continuation multiple times — for
example, to explore multiple branches of a nondeterministic computation.

Use `with-continuation` inside a handler clause to capture the continuation
as a named value you can call directly:

```lisp
(handle (+ 1 (signal choose 0))
  (choose x
    (with-continuation k
      (+ (k 10) (k 20)))))
;; => 32   — body runs twice: (+ 1 10) = 11 and (+ 1 20) = 21, summed
```

`with-continuation` binds the hidden continuation to a name (`k` here, but
any name works). You can then call it like a function. Each call resumes the
body from where `signal` was, with the given value.

This is a power-user feature. For normal effects, `respond` is all you need.

---

## Quick reference

| Form | Syntax | Purpose |
|------|--------|---------|
| Declare effect | `(define [effect] (tag (^Type arg)))` | Type-checked effect declaration |
| Signal effect | `(signal tag value)` | Ask for something to happen |
| Handle effects | `(handle body (tag arg body...) ...)` | Install a handler |
| Respond | `(respond value)` | Send a value back, body continues |
| Abort | don't call `respond` | Short-circuit, return handler result |
| Multi-shot | `(with-continuation k ...)` | Name the continuation for direct use |

| I/O Effect | Wrapper | Fast path |
|-----------|---------|-----------|
| `io/print` | `print` | Raw terminal output |
| `io/println` | `println` | Raw terminal output + newline |
| `io/display` | `display` | Raw terminal output |
| `io/newline` | `newline` | Raw newline |
| `io/read-file` | `read-file` | Raw file read |
| `io/write-file` | `write-file` | Raw file write |
| `io/file-exists?` | `file-exists?` | Raw stat check |
| `io/read-lines` | `read-lines` | Raw line reader |
