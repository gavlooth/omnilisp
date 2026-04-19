# CORE_LIBS_INSPECTION Part 02

Source: `docs/CORE_LIBS_INSPECTION.md`

## Three-Tier Pattern Matching (PCRE2 eliminated)

Backreferences (`\1`, `\2`) are **context-sensitive** — they make regex matching NP-complete.
Pika is a **context-free** bottom-up DP engine with linear/polynomial guarantees.
These are fundamentally incompatible. Adding backreferences would break Pika's architecture.

**Dialectic reasoning (4 rounds):**

1. "Add semantic predicates" → fails: Pika parses R→L, capture group doesn't exist yet when
   backreference is encountered (it's to the left in the string, parsed later)
2. "Parse `\1` as `.*`, filter post-hoc" → fails: combinatorial explosion of spurious parse
   trees, destroys O(n³) guarantee, exponential memory
3. "Redirect to grammar DSL" → viable but ergonomically painful for simple cases
4. "Accept the theoretical boundary" → correct: backreferences are NP-complete, don't pretend
   a polynomial engine can handle them

**Resolution: three tiers, no PCRE2.**

```
Tier 1: re-match (fast, linear)     — 95% of use cases
         [0-9]+, \w+, (foo|bar), (?=...), [a-z]{3,5}
         Zero external deps, Pika-native

Tier 2: pika/grammar (powerful)     — 4.9% of use cases
         Recursive grammars, left recursion, structural parsing
         Handles what backreferences typically solve:

         ;; Match balanced quotes (would need \1 in PCRE)
         ;; Instead: match with Omni code
         (define (match-quoted s)
           (let (q (char-at s 0))
             (and (or (= q "'") (= q "\""))
                  (= q (char-at s (- (string-length s) 1))))))

Tier 3: Omni code (unlimited)       — 0.1% of use cases
         When you need context-sensitive matching, write a function.
         Multiple dispatch + Pika regex + string ops = anything.
```

**Why this beats PCRE2:**

| | PCRE2 via FFI | Three-tier (no PCRE2) |
|---|---|---|
| Dependencies | +1 C library (~500KB) | Zero new deps |
| Two regex engines | Confusing: which to use? | One engine, clear escalation |
| Catastrophic backtracking | PCRE2 can ReDoS | Pika always terminates |
| Learning curve | Two regex flavors | One system to learn |
| Maintenance | Two engines to maintain | One engine + grammar DSL |

---

### 9. Reader Dispatch and Symbol-as-Function

**Two syntax enhancements** that unlock ergonomics across contracts, Datalog, and pipelines.

#### `'symbol` as lookup function

When a quoted symbol is applied as a function, it performs a key lookup:

```lisp
('name (Dictionary 'name "Alice" 'age 30))   ;; => "Alice"

;; Powerful with HOFs and pipelines
(map 'name people)                       ;; => ("Alice" "Bob")
(filter 'active? users)
(sort-by 'age people)

;; Works with Datalog results
(map 'age (query '((person ?name ?age _))))

;; Pipe-friendly
(|> people (filter 'active?) (sort-by 'age) (map 'name))
```

Implementation: ~10 lines in `jit_apply_value` — when applying a SYMBOL, treat as `(ref arg symbol)`.

**Why not `.name` accessor lambdas:** `.name` conflicts visually with `person.name` field access.
`person.name.city` vs `.name.city` — same characters, different meanings. Confusing.
`'name` has zero ambiguity: it's always a quoted symbol, never a field access.

#### `#` reader dispatch extensions

| Syntax | Meaning | Example |
|--------|---------|---------|
| `;` | Line comment | `; this is a comment` |
| `#\| ... \|#` | Block comment (nestable) | `#\| multi-line \|#` |
| `#_` | Skip next form | `#_ (ignored) (this runs)` |
| `#N_` | Skip next N forms | `#3_ (a) (b) (c) (this runs)` |
| `Set` | Set constructor | `(Set "red" "green" "blue")` |
| `#r"pat"` | Compiled Pika regex | `#r"[0-9]+"` |

**Set literals** complete the collection trinity:
```lisp
'(1 2 3)          ;; list
[1 2 3]           ;; array
{'a 1 'b 2}       ;; dict
(Set "a" "b" "c") ;; set constructor
```

**Regex literals** make Pika regex first-class:
```lisp
;; #r"" compiles regex at read time, no string escaping needed
(re-match #r"[0-9]+" "abc123")      ;; vs (re-match "[0-9]+" "abc123")
(re-split #r"\s+" "hello world")    ;; vs (re-split "\\s+" "hello world")

;; Particularly valuable for complex patterns
(re-match #r"(\w+)\s*=\s*(\w+)" "key = value")
```

**Multi-form comment** `#N_` — the number comes BEFORE `_` to avoid ambiguity with `#_ 3`:
```lisp
;; Disable a function and its test
#2_ (define (broken x) ...)
     (assert (= (broken 1) 2))

;; Disable one guarded branch in a match Void chain
(match Void
  ((? (< x 0)) "negative")
  #1_ ((? (= x 0)) "zero")  ;; disabled: skip 1 form (the guard branch)
  (_ "non-negative"))
```

**Block comments** nest — safe to comment out code containing block comments:
```lisp
#|
  This whole section is disabled.
  #| Even nested comments |# are fine.
  (define (unused) ...)
|#
```

**Implementation:** ~80 lines in parser (reader dispatch table for `#`).

---

## Tier 3: Nice to Have

### 10. stb_image — Image loading

Single-header image decoder. Relevant given omni-torch exists. Public domain, ~100KB.

### 11. HTTP/1.1 client

Build on libuv TCP + BearSSL TLS. Parse with Pika grammar.
Alternative: libcurl via FFI if building from scratch is too slow.

### 12. Concurrency Model — Fibers, Scheduler, Threads

Omni's concurrency builds entirely on existing primitives (coroutines + effects).
No new concepts — just composition.

**Three tiers, increasing isolation:**

```
Coroutines    — cooperative, same thread, share everything (zero copy)
Fibers        — cooperative, same thread, scheduler manages (zero copy)
Threads       — parallel, isolated Interp, share NOTHING (copy flat data only)
```

#### Tier 1: Coroutines (LANDED)

Manual cooperative multitasking. `coroutine`/`resume`/`yield`. StackCtx-based.
User explicitly controls scheduling. Good for generators, lazy sequences, streams.

#### Tier 2: Fibers = Coroutines + Scheduler Effect Handler

A fiber is a coroutine whose `resume` is called by the scheduler, not the user.
The scheduler is an effect handler wrapping user code:

```lisp
;; spawn creates a coroutine, registers it with the scheduler
(spawn (lambda () (tcp-read (tcp-connect "a.com" 80))))

;; Under the hood:
;; 1. spawn = create coroutine + add to scheduler's ready queue
;; 2. tcp-connect signals io/tcp-connect effect
;; 3. Scheduler handler catches it, starts libuv async op, suspends fiber
;; 4. libuv callback fires → scheduler marks fiber as ready
;; 5. Scheduler resumes fiber with the result via resolve
```

Scheduler loop (~150 lines C3):
```
while fibers exist:
    while ready_queue not empty:
        resume next fiber
        if it signaled I/O effect → start libuv op, keep suspended
    uv_run(loop, UV_RUN_ONCE)   ;; poll libuv for completions
    ;; callbacks push fibers back to ready_queue
```

No new types. A fiber IS a coroutine. The scheduler IS an effect handler.

#### Tier 3: Threads = Fibers on OS Threads (share-nothing)

Each thread gets its own `Interp` + scope-region (complete isolation).
Communication via effects — the handler manages a lock-free inter-thread queue.
`uv_thread_create` + `uv_async_t` (cross-thread wakeup) from libuv.

**Critical design: send DATA, not closures.**

Copying closures across threads is heavy (env chain + all captured bindings = O(n)).
Instead, send flat data (int, string, symbol, list of scalars). Each thread loads
the same source code, so all functions are already defined in every Interp:

```lisp
;; LIGHT — send a symbol + an int (O(1) copy)
(signal io/thread-send (cons target '(compute-fib 40)))
;; Receiving thread already has compute-fib, just calls it

;; NOT THIS — copying a closure means copying the entire env chain
;; (signal io/thread-send (cons target (lambda () (compute big-captured-data))))
```

**No built-in channels.** Inter-thread communication goes through effects.
The "channel" is a queue inside the effect handler — not a language primitive.
Users who want channel abstractions write a handler:

```lisp
;; Channel = effect handler managing a queue
(define (make-channel)
  (let (queue (Array))
    (Dictionary 'send (lambda (msg) (signal io/chan-send (cons queue msg)))
          'recv (lambda () (signal io/chan-recv queue)))))
```

**Why no channels as primitives:**
- Effects already provide the suspend/resume mechanism
- A channel is just a queue + two effects (send/recv)
- Baking it in adds a concept where composition suffices
- Different channel types (buffered, unbuffered, broadcast) = different handlers

**What this model provides (all via composition):**

| Pattern | How |
|---------|-----|
| Message passing | Effect handler + queue |
| Future/Promise | Spawn fiber, await via effect |
| Fork/Join | Spawn N fibers, collect via effects |
| Map-reduce | Scatter data to threads, gather via effects |
| Pipeline | Chain of effect handlers |
| Supervision | Parent handler monitors child fibers |

**What's NOT needed (share-nothing eliminates):**
- Locks/mutexes — no shared mutable state
- STM — no shared state to transact on

#### Fine-Grained Shared State: Agent Pattern + Atomic Refs

Share-nothing is correct for 95% of concurrency. For the remaining 5% (hot counters,
parallel accumulators), two escape hatches — neither requires locks.

**Tier A: Agent pattern (Omni-native, no new types)**

An agent is a fiber that owns mutable state and processes update messages serially.
Other threads send requests, the agent applies them one at a time. No races by construction.
This is how Erlang's `gen_server` works.

```lisp
;; Agent = fiber that owns state, processes messages serially
(define (make-counter initial)
  (spawn (lambda ()
    (let loop (n initial)
      (let (msg (yield n))
        (match msg
          ('inc   (loop (+ n 1)))
          ('dec   (loop (- n 1)))
          ('get   (loop n))))))))

;; Any thread sends update requests
(signal io/thread-send (cons counter-agent 'inc))
```

**Tier B: Atomic refs (hardware atomics, ~30 lines C3)**

When message roundtrip overhead matters (millions of increments per second),
atomic integers provide direct hardware CAS. Uses C3's native `std::atomic` module —
no C builtins, no FFI, pure C3:

```lisp
;; Omni API
(define counter (atomic 0))      ;; shared atomic integer
(atomic-add! counter 1)          ;; hardware fetch-and-add, returns old value
(atomic-read counter)            ;; atomic load
(atomic-cas! counter 0 1)        ;; compare-and-swap, returns true/false
```

C3 implementation (uses `std::atomic::Atomic{long}`):

```c3
import std::atomic;

// atomic ref = malloc'd Atomic{long}, shared across threads (not in any scope-region)
fn Value* prim_atomic(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_int(args[0])) return raise_error(interp, "atomic: expected integer");
    atomic::Atomic{long}* atom = (atomic::Atomic{long}*)mem::malloc(atomic::Atomic{long}.sizeof);
    atom.store(args[0].int_val);
    // Wrap as FFI_HANDLE in root_scope — lives for program lifetime
    return make_ffi_handle(interp, (void*)atom, "atomic");
}

fn Value* prim_atomic_add(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "atomic-add!: expected (atomic-add! ref n)");
    atomic::Atomic{long}* atom = (atomic::Atomic{long}*)args[0].ffi_val;
    long old = atom.add(args[1].int_val);
    return make_int(interp, old);
}

fn Value* prim_atomic_read(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "atomic-read: expected (atomic-read ref)");
    atomic::Atomic{long}* atom = (atomic::Atomic{long}*)args[0].ffi_val;
    return make_int(interp, atom.load());
}

fn Value* prim_atomic_cas(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return raise_error(interp, "atomic-cas!: expected (atomic-cas! ref old new)");
    atomic::Atomic{long}* atom = (atomic::Atomic{long}*)args[0].ffi_val;
    long expected = args[1].int_val;
    long desired = args[2].int_val;
    long* exp_ptr = &expected;
    bool ok = mem::compare_exchange((long*)atom, exp_ptr, desired);
    return ok ? interp.global_env.lookup(interp.symbols.intern("true")) : make_nil(interp);
}
```

**Only integers.** No atomic closures, no atomic dicts. Atomic refs live outside
scope-regions (plain `malloc`) — shared across threads with zero coordination.

**When to use which:**

| Need | Solution | Overhead |
|------|----------|----------|
| Shared mutable state (general) | Agent pattern | Message roundtrip (~µs) |
| Hot counter / parallel accumulator | `atomic` + `atomic-add!` | Hardware CAS (~ns) |
| Shared large data (rare) | FFI to C (e.g., omni-torch tensors) | Zero (C manages it) |

**Not a GIL.** Python's GIL = one interpreter, N threads fighting for it.
Omni = N interpreters, zero contention, true parallelism. Same model as Erlang
(powers WhatsApp 2B users, Discord, RabbitMQ).

**Implementation:**

| Component | Lines (est.) | Phase |
|-----------|-------------|-------|
| Fiber scheduler (effect handler + libuv poll loop) | ~150 C3 | E |
| `spawn` / `await` primitives | ~50 C3 | E |
| Thread spawn (`uv_thread_create` + per-thread Interp init) | ~100 C3 | G |
| Inter-thread message queue (`uv_async_t` wakeup) | ~80 C3 | G |
| Cross-thread value copy (flat data only, no closures) | ~50 C3 | G |
| Atomic refs (`atomic`, `atomic-add!`, `atomic-read`, `atomic-cas!`) | ~30 C3 | G |

---
