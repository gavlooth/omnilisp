# Omni Lisp Effects Guide

Effects are Omni's way of handling side effects — I/O, errors, state — as
**values you can intercept, redirect, or suppress**. Instead of calling
`println` and having output go directly to the terminal, `println` *performs*
an effect. If nobody is listening, the output goes to the terminal as usual.
If a handler is installed, it gets to decide what happens.

This means you can test code without touching the filesystem, capture printed
output to a string, or swap a real database for an in-memory mock — all without
changing the code that performs the effects.

---

## 1. The basics: `perform` and `handle`

An effect has two sides:

- **`perform`** — signals that something should happen
- **`handle`** — intercepts that signal and decides what to do

```lisp
(handle
  (+ 1 (perform read nil))
  ((read k x) (k 41)))
;; => 42
```

What happened:
1. The body starts evaluating `(+ 1 (perform read nil))`
2. `perform` signals the `read` effect with argument `nil`
3. The handler clause `(read k x)` matches
4. `k` is the **continuation** — a function representing "the rest of the computation"
5. `x` is the argument (`nil` in this case)
6. `(k 41)` resumes the computation, substituting `41` where `perform` was
7. The body finishes: `(+ 1 41)` => `42`

---

## 2. The continuation `k`

`k` is the key concept. It's a function that means "continue where we left off,
using this value as the result of `perform`."

### Resume: call `k`

```lisp
(handle
  (* 2 (perform get-factor nil))
  ((get-factor k _) (k 21)))
;; => 42   — resumed with 21, body computes (* 2 21)
```

### Abort: don't call `k`

```lisp
(handle
  (+ 1 (+ 2 (+ 3 (perform bail 42))))
  ((bail k x) x))
;; => 42   — the entire (+ 1 (+ 2 ...)) is abandoned
```

When you don't call `k`, the handler's return value becomes the result of the
entire `handle` expression. This is how you implement early return, exceptions,
or short-circuit logic.

### Transform: call `k` with a modified value

```lisp
(handle
  (perform double 5)
  ((double k x) (k (* x 2))))
;; => 10
```

---

## 3. Multiple effects in one body

A handler can intercept many `perform` calls — each one pauses, runs the
handler, and resumes (if `k` is called).

```lisp
(handle
  (+ (perform bump 10) (perform bump 20))
  ((bump k x) (k (+ x 1))))
;; => 32   — first bump gives 11, second gives 21
```

### Multiple clauses

Handle different effect tags in the same block:

```lisp
(handle
  (begin
    (perform log "starting")
    (+ 1 (perform ask nil)))
  ((log k msg) (println msg) (k nil))
  ((ask k _)   (k 41)))
;; prints "starting", returns 42
```

---

## 4. I/O effects

`print`, `println`, `display`, and `newline` are all effect-based.
Under the hood:

```lisp
(define print   (lambda (x) (perform io/print x)))
(define println (lambda (x) (perform io/println x)))
(define display (lambda (x) (perform io/display x)))
(define newline (lambda ()  (perform io/newline nil)))
```

When no handler is installed, these take a **fast path** — they call the raw
I/O primitives directly with zero overhead. You don't pay for effects you
don't use.

### Suppress output

```lisp
(handle (begin (println "you won't see this") 42)
  ((io/println k x) (k nil)))
;; => 42   — nothing printed
```

### Capture output to a string

```lisp
(handle (begin (println "captured") nil)
  ((io/println k x) x))
;; => "captured"
```

This works because the handler catches the effect and returns `x` (the string)
without calling `k` — so the body aborts and the string becomes the result.

### Capture all output (with resume)

```lisp
(define (with-output-to-string thunk)
  (let ((buf ""))
    (handle (begin (thunk) buf)
      ((io/println k x)
        (set! buf (string-append buf (number->string x) "\n"))
        (k nil))
      ((io/print k x)
        (set! buf (string-append buf (number->string x)))
        (k nil))
      ((io/display k x)
        (set! buf (string-append buf (number->string x)))
        (k nil)))))

(with-output-to-string
  (lambda ()
    (println 1)
    (println 2)
    (println 3)))
;; => "1\n2\n3\n"
```

The handler intercepts each print call, appends to the buffer, and resumes
so the thunk keeps running. After the thunk finishes, the buffer is returned.

### File I/O effects

```lisp
(read-file "data.txt")        ;; performs io/read-file
(write-file "out.txt" "data") ;; performs io/write-file
(file-exists? "data.txt")     ;; performs io/file-exists?
(read-lines "data.txt")       ;; performs io/read-lines
```

These can all be intercepted for testing:

```lisp
;; Mock the filesystem
(handle
  (read-file "config.txt")
  ((io/read-file k path) (k "mocked content")))
;; => "mocked content"   — no file was read
```

---

## 5. Declaring custom effects

Use `define [effect]` to declare an effect with a type constraint:

```lisp
(define [effect] (my/ask (^String prompt)))
```

Now `(perform my/ask 42)` will raise a type error because `42` is not a
string. `(perform my/ask "name?")` works.

```lisp
;; Accept any type
(define [effect] (log (^Any msg)))

;; Accept only numbers
(define [effect] (scale (^Number factor)))
```

Effects don't *need* to be declared — undeclared effects work fine, they just
skip type checking:

```lisp
(handle (perform my-tag 42) ((my-tag k x) (k (+ x 1))))
;; => 43   — works without a declaration
```

---

## 6. Error handling with `raise`

The stdlib declares a `raise` effect and provides `try` and `assert!`:

```lisp
(define [effect] (raise (^Any msg)))

(define (try thunk handler)
  (handle (thunk nil)
    ((raise k msg) (handler msg))))

(define (assert! condition msg)
  (if condition true (perform raise msg)))
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
    (perform raise "oops")
    (println "after"))
  ((raise k msg) (string-append "error: " msg)))
;; prints "before", returns "error: oops"
;; "after" never executes (k was not called)
```

---

## 7. Nested handlers

Handlers form a stack. The innermost handler gets first chance to match:

```lisp
(handle
  (handle
    (perform greet "world")
    ((greet k x) (k (string-append "hello, " x))))
  ((greet k x) (k (string-append "hi, " x))))
;; => "hello, world"   — inner handler matched first
```

If the inner handler doesn't match the effect tag, it propagates outward:

```lisp
(handle
  (handle
    (perform outer-tag 1)
    ((inner-tag k x) (k 0)))
  ((outer-tag k x) (k (+ x 99))))
;; => 100   — inner didn't match, outer caught it
```

---

## 8. Practical patterns

### State effect

```lisp
(define [effect] (get-state (^Any _)))
(define [effect] (set-state (^Any v)))

(define (with-state initial thunk)
  (let ((state initial))
    (handle (thunk)
      ((get-state k _)
        (k state))
      ((set-state k v)
        (set! state v)
        (k nil)))))

(with-state 0
  (lambda ()
    (perform set-state 10)
    (+ (perform get-state nil) 5)))
;; => 15
```

### Logger effect

```lisp
(define [effect] (log (^Any msg)))

(define (with-logger thunk)
  (let ((logs '()))
    (handle (begin (thunk) (reverse logs))
      ((log k msg)
        (set! logs (cons msg logs))
        (k nil)))))

(with-logger
  (lambda ()
    (perform log "step 1")
    (perform log "step 2")
    (perform log "done")))
;; => ("step 1" "step 2" "done")
```

### Dependency injection

```lisp
(define [effect] (db/query (^String sql)))

;; Production handler
(define (with-real-db thunk)
  (handle (thunk)
    ((db/query k sql) (k (real-db-query sql)))))

;; Test handler
(define (with-mock-db thunk)
  (handle (thunk)
    ((db/query k sql) (k '((id 1) (name "test"))))))

;; Same code, different handler
(define (get-user)
  (perform db/query "SELECT * FROM users LIMIT 1"))

(with-mock-db get-user)
;; => ((id 1) (name "test"))
```

---

## Quick reference

| Form | Syntax | Purpose |
|------|--------|---------|
| Declare effect | `(define [effect] (tag (^Type arg)))` | Type-checked effect declaration |
| Perform effect | `(perform tag value)` | Signal an effect |
| Handle effects | `(handle body ((tag k arg) ...))` | Install a handler |
| Resume | `(k value)` | Continue computation with value |
| Abort | don't call `k` | Short-circuit, return handler result |

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
