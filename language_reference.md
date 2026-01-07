# OmniLisp Language Reference

This is the authoritative reference for the OmniLisp language, its "Character Calculus" semantics, and its optionally-typed execution model.

---

## 1. The Character Calculus (Delimiters)

OmniLisp uses a fixed semantic "Charge" for every delimiter to ensure absolute consistency across the value and type domains.

| Character | Domain | Purpose |
| :--- | :--- | :--- |
| **`()`** | **Flow** | Execution logic, function calls, and **Value Constructors**. |
| **`[]`** | **Slot** | Argument vectors, `let` bindings, arrays, and **Type Parameters**. |
| **`{}`** | **Kind** | **Type Annotations** and the Blueprint domain. |
| **`.`**  | **Path** | Data location and navigation. Bridges Flow and Slot. |
| **`^`**  | **Tag**  | Metadata and out-of-band instructions. |
| **`#`**  | **Sign** | Reader dispatch for literal structures (Dicts). |

---

## 2. Bindings & Functions

### 2.1 Function Definitions
Functions are defined using a **Flow Template** header that mirrors the call site.

```lisp
;; 1. Implicit Any (Julia-style flexibility)
(define (add x y) (+ x y))

;; 2. Explicit Type Hints
(define (mul x {Int} y {Int}) {Int} (* x y))

;; 3. Explicit Slot brackets (optional container)
(define (sub [x y]) (- x y))

;; 4. Combined style
(define (div [x {Int} y {Int}]) {Int} (/ x y))

;; 5. Flat Clojure-style (Backward compatibility)
(define fact [n] (if (<= n 1) 1 (* n (fact (- n 1)))))
```

### 2.2 Local Bindings (`let`)
`let` uses flat, even-numbered pairs inside a Slot `[]`.

```lisp
(let [x 10
      y {Int} 20]
  (+ x y))
```

### 2.3 Type Enforcement
Every variable has an associated Kind. If no Kind is specified, it defaults to **`Any`**.
*   **Assignments:** `define`, `let`, and `set!` validate values against the Kind blueprint.
*   **Application:** `omni_apply` validates arguments against parameter Kinds and the result against the return Kind.

### 2.4 Multiple Dispatch & Multi-arity
OmniLisp functions support **Multiple Dispatch**, allowing a single name to refer to multiple implementations (methods). Dispatch is determined by:
1.  **Arity:** The number of arguments provided.
2.  **Specificity:** The Kind of each argument (Most specific match wins).

```lisp
;; 1. Multi-arity (different argument counts)
(define (area) 0)                      ; 0 args
(define (area r) (* 3.14 (* r r)))     ; 1 arg

;; 2. Type Specialization (Multiple Dispatch)
(define (describe x {Int}) "An integer")
(define (describe x {String}) "A string")

;; 3. Combined Dispatch
(define (add x {Int} y {Int}) (+ x y))
(define (add x {String} y {String}) (string-append x y))
```

---

## 3. The Type System (Kind Domain)

### 3.1 Annotations vs. Constructors
*   **Annotation `{}`**: Used in definitions to restrict a slot.
*   **Constructor `()`**: Used at the value level to create instances.

```lisp
(define x {Int} 5)           ; {Int} is an annotation
(define p (Point 10 20))     ; (Point) is a constructor (Flow)
```

### 3.2 Parametric Types
Parameters inside an annotation use the **Slot `[]`** to signify a type variable.

```lisp
{Iter Int}                   ; Concrete: Iter applied to Int
{Iter [T]}                   ; Parametric: T is a type variable Slot
{Option {List [T]}}          ; Recursive parametric
```

### 3.3 First-Class Kinds
Base types (`Int`, `String`, `Array`, `Any`, etc.) are physical Kind objects stored in the global environment. They can be manipulated as values.

```lisp
(define my-type Int)
(type? 10 my-type)           ; => true
```

### 3.4 Subtyping
OmniLisp uses a nominative subtype hierarchy. **`Any`** is the universal supertype.
*   `Int <: Any`
*   `String <: Any`
*   `Nothing` is the universal subtype (bottom).

---

## 4. Sequences & Iterators

### 4.1 Lazy Iterators
Core sequence functions are lazy and return `T_ITER` objects.
*   `(range n)`: Returns an iterator from 0 to n-1.
*   `(map f iter)`: Returns a lazy transformed iterator.
*   `(filter pred iter)`: Returns a lazy filtered iterator.

### 4.2 Realization
To convert a lazy iterator into an eager structure:
*   `(collect-list iter)`: Produces a Persistent Linked List `()`.
*   `(collect-array iter)`: Produces a Contiguous Realized Array `[]`.

---

## 5. Access & Mutation

### 5.1 Path Navigation
The dot `.` operator is used for nested data access.
*   `person.name` -> `(get person :name)`
*   `arr.[0]` -> `(get arr 0)`

### 5.2 Mutation
*   `set!`: Rebinds a name.
*   `put!`: Mutates a path (e.g., `(put! person.age 31)`).
*   `update`: Functionally transforms a path.

---

## 6. Control Flow & Effects

### 6.1 Algebraic Effects
OmniLisp uses **Algebraic Effects** as its primary mechanism for non-local control flow and error handling. Traditional `try/catch` is replaced by `handle/perform`.

*   **`handle`**: Establishes a handler for a specific set of effects.
*   **`perform`**: Triggers an effect, transferring control to the nearest handler.
*   **`resume`**: Resumes the suspended computation from within a handler.

```lisp
(defeffect ask :one-shot)

(handle
  (+ 1 (perform ask nothing))
  (ask [payload resume] (resume 42)))
;; => 43
```

### 6.2 Fibers & Channels (Two-Tier Concurrency)
OmniLisp implements a **Two-Tier Concurrency** model that separates physical parallelism from massive logical concurrency.

*   **Tier 1 (Parallel):** OS Threads (pthreads) for multi-core utilization.
*   **Tier 2 (Concurrent):** Lightweight **Fibers** (continuations) for massive concurrency (1M+ fibers).

#### Fiber Management
*   **`fiber`**: Creates a paused fiber from a thunk.
*   **`resume`**: Manually steps into a fiber (direct control).
*   **`yield`**: Suspends the current fiber, returning control to the caller or scheduler.
*   **`with-fibers`**: Establishes a local **Fiber Scheduler** scope. The block waits until all spawned fibers complete.
*   **`spawn`**: Registers a fiber with the current scheduler.
*   **`join`**: Blocks the current fiber until the target fiber completes, returning its result.
*   **`run-fibers`**: Explicitly runs the scheduler loop until all pending fibers are done.

#### Channels (CSP)
Fibers communicate via **Channels**, enabling ownership transfer without shared-memory locks.
*   **`chan`**: Creates an unbuffered (rendezvous) channel.
*   **`(chan n)`**: Creates a buffered channel with capacity `n`.
*   **`send` / `recv`**: Synchronous or buffered communication. If a channel is full (on send) or empty (on recv), the fiber **Parks** (Tier 2 suspension) to let others run.

```lisp
(with-fibers
  (define c (chan 3))
  (spawn (fiber (lambda [] (send c "ping"))))
#### Ownership Transfer
Sending a value through a channel performs an **Ownership Transfer**. The sending fiber/thread yields control of the object's lifetime to the receiver, preventing data races by ensuring only one owner exists at any time. Immutable objects (frozen) can be shared safely across Tier 1 threads.

---

## 7. Pika Grammar DSL

Define high-performance PEG grammars directly in the Flow domain.

```lisp
(define [grammar arithmetic]
  [expr (first (seq (ref term) "+" (ref expr)) (ref term))]
  [term (ref factor)]
  ...)
```

---

## 8. Memory Management (ASAP & RC-G)

OmniLisp utilizes a unique, garbage-collection-free memory model designed for predictability and performance.

### 8.1 ASAP (As Static As Possible)
The compiler statically analyzes variable lifetimes and automatically injects deallocation calls. This minimizes runtime overhead and eliminates "stop-the-world" pauses.

### 8.2 RC-G (Region-Based Reference Counting)
For dynamic data, OmniLisp uses **Region-Based Reference Counting**.
*   **Regions**: Objects are allocated into regions (arenas) that track their own internal connectivity.
*   **Transmigration**: The system can move objects between regions to optimize locality and clear cycles.
*   **Tethers**: Thread-local references that prevent premature deallocation while a value is in active use.

---

## 9. Deprecated Namespaces

The following are legacy and should not be used:
*   `violet.*`: Use core primitives instead.
*   `scicomp.*`: Pending modern refactor.
