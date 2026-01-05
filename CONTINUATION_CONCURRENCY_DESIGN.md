# Continuation-Based Concurrency for OmniLisp

Integrating delimited continuations with Vale-style region-based memory safety.

## Table of Contents

1. [Two-Tier Threading Model](#two-tier-threading-model)
2. [Background](#background)
3. [Core Design](#core-design)
4. [Memory Safety Integration](#memory-safety-integration)
5. [Concurrency Primitives](#concurrency-primitives)
6. [Scheduler](#scheduler)
7. [Implementation](#implementation)
8. [Examples](#examples)
9. [Runtime Changes](#runtime-changes)

---

## Two-Tier Threading Model

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│  Tier 1: OS Threads (pthreads)                          │
│                                                         │
│  Purpose:                                               │
│  - True parallelism (multi-core)                        │
│  - Blocking FFI calls                                   │
│  - CPU-intensive work                                   │
│                                                         │
│  API:                                                   │
│  - (thread-spawn thunk) → thread-handle                 │
│  - (thread-join handle) → result                        │
│                                                         │
│  Cost: ~8KB stack minimum, OS scheduling                │
└─────────────────────────────────────────────────────────┘
                         │
                         │ Each OS thread runs its own scheduler
                         ▼
┌─────────────────────────────────────────────────────────┐
│  Tier 2: Green Threads (continuations)                  │
│                                                         │
│  Purpose:                                               │
│  - Lightweight concurrency                              │
│  - Cooperative scheduling                               │
│  - Channels, async I/O                                  │
│                                                         │
│  API:                                                   │
│  - (spawn thunk) → task                                 │
│  - (yield)                                              │
│  - (chan-send ch val), (chan-recv ch)                   │
│  - (atom-deref a), (atom-swap! a f)                     │
│                                                         │
│  Cost: ~100 bytes per task, user-space scheduling       │
└─────────────────────────────────────────────────────────┘
```

### What to Keep (pthreads)

```c
// OS thread creation - KEEP
pthread_t thread;
pthread_create(&thread, NULL, entry, arg);
pthread_join(thread, &result);

// Cross-thread mutex - KEEP (for shared data between OS threads)
pthread_mutex_t mutex;
pthread_mutex_lock(&mutex);
pthread_mutex_unlock(&mutex);
```

### What to Remove (reimplement with continuations)

```c
// REMOVE - Channel with pthread synchronization
typedef struct Channel {
    pthread_mutex_t lock;      // REMOVE
    pthread_cond_t not_empty;  // REMOVE
    pthread_cond_t not_full;   // REMOVE
    // ...
} Channel;

// REMOVE - Atom with pthread mutex
typedef struct Atom {
    pthread_mutex_t lock;      // REMOVE
    // ...
} Atom;

// REMOVE - spawn_goroutine using pthread_create
void spawn_goroutine(...);     // REMOVE
```

### Why Two Tiers?

| Use Case | Tier |
|----------|------|
| 1000s of concurrent tasks | Green (continuations) |
| Blocking C library call | OS thread |
| CPU-bound computation | OS thread |
| Async I/O multiplexing | Green (continuations) |
| Channel communication | Green (continuations) |
| Cross-thread shared state | OS mutex + atoms |

---

## Background

### Vale's Concurrency Model

Vale uses several key techniques we adopt:

#### 1. Virtual Stacks (Expandable Stack Chunks)

```
┌─────────┐    ┌─────────┐    ┌─────────┐
│ Chunk 1 │───▶│ Chunk 2 │───▶│ Chunk 3 │
│  8KB    │    │  8KB    │    │  8KB    │
└─────────┘    └─────────┘    └─────────┘
     │
     └── Stack grows by linking new chunks
```

- Stack is a linked list of heap-allocated chunks
- No fixed 8MB reservation
- Chunks come from free-list (no malloc in hot path)

#### 2. Context Word

```c
// Thread-local or passed through all functions
uint64_t context_word;

#define CTX_IN_CONCURRENCY  0x1  // Running in scheduler
#define CTX_IN_CONTINUATION 0x2  // Inside captured continuation
#define CTX_PROMPT_DEPTH    0xFFFF0000  // Prompt stack depth
```

#### 3. Regions and Transmigration

- Each scope/thread has its own memory region
- Objects crossing regions need **transmigration**:
  - Deep copy, OR
  - Generation increment by random X (isolates references)

#### 4. Generational References

```c
struct Value {
    uint16_t generation;  // Increments on each reuse
    // ...
};

struct BorrowedRef {
    Value* target;
    uint16_t remembered_gen;  // Must match target->generation
};
```

### Delimited Continuations

Two operators from Felleisen's control/prompt:

```lisp
;; Establish a delimiter (prompt)
(prompt tag
  body...)

;; Capture continuation up to nearest prompt with matching tag
(control tag k
  ;; k is the continuation (reified stack)
  body...)
```

Semantics:
```
(prompt 'p
  (+ 1 (control 'p k (k (k 2)))))

;; k = (lambda (v) (+ 1 v))
;; (k (k 2)) = (k 3) = 4
;; Result: 4
```

---

## Core Design

### Continuation Structure

```c
typedef struct Continuation {
    StackChunk* frames;       // Captured stack chunks
    IRegion* home_region;     // Region at capture time
    uint64_t capture_gen;     // Generation for staleness detection
    uint32_t prompt_tag;      // Which prompt delimits this
    Value* prompt_menv;       // Meta-environment at prompt
    struct Continuation* parent;  // For nested continuations
} Continuation;
```

### Stack Chunk (from Vale's virtual stacks)

```c
typedef struct StackChunk {
    void* base;               // Base address
    void* sp;                 // Current stack pointer
    void* limit;              // End of chunk
    size_t size;              // Chunk size (default 8KB)
    struct StackChunk* prev;  // Previous chunk in chain
    struct StackChunk* next;  // Next chunk in chain
    uint32_t refcount;        // For continuation sharing
    IRegion* region;          // Region this chunk belongs to
} StackChunk;
```

### Prompt Stack

```c
#define MAX_PROMPT_DEPTH 64

typedef struct PromptFrame {
    uint32_t tag;             // Prompt identifier
    StackChunk* chunk;        // Stack chunk at prompt point
    void* sp;                 // Stack pointer at prompt
    Value* menv;              // Meta-environment
    jmp_buf escape;           // For non-local return
} PromptFrame;

typedef struct PromptStack {
    PromptFrame frames[MAX_PROMPT_DEPTH];
    int depth;
} PromptStack;

// Thread-local prompt stack
_Thread_local PromptStack g_prompts;
```

### Key Operations

#### prompt - Establish Delimiter

```c
Value* op_prompt(uint32_t tag, Value* body, Value* menv) {
    // Push prompt frame
    PromptFrame* frame = &g_prompts.frames[g_prompts.depth++];
    frame->tag = tag;
    frame->chunk = current_chunk();
    frame->sp = get_sp();
    frame->menv = menv;

    // Set up escape point
    if (setjmp(frame->escape) != 0) {
        // Returned via control - result in frame
        g_prompts.depth--;
        return frame->result;
    }

    // Evaluate body
    Value* result = eval(body, menv);

    // Normal return
    g_prompts.depth--;
    return result;
}
```

#### control - Capture Continuation

```c
Value* op_control(uint32_t tag, Symbol* k_name, Value* body, Value* menv) {
    // Find matching prompt
    int prompt_idx = find_prompt(tag);
    if (prompt_idx < 0) {
        return mk_error("No matching prompt for tag");
    }

    PromptFrame* prompt = &g_prompts.frames[prompt_idx];

    // Capture continuation (stack chunks from here to prompt)
    Continuation* k = capture_continuation(prompt);

    // Bind k in environment
    Value* k_val = mk_cont(k);
    Value* new_env = env_extend(menv->env, k_name, k_val);
    Value* new_menv = clone_menv(menv, new_env);

    // Unwind stack to prompt point
    unwind_to_prompt(prompt);

    // Evaluate body with k bound
    Value* result = eval(body, new_menv);

    // Return result to prompt
    prompt->result = result;
    longjmp(prompt->escape, 1);
}
```

#### Invoke Continuation

```c
Value* invoke_continuation(Continuation* k, Value* val) {
    // Handle region crossing (transmigration)
    if (val && val->region != k->home_region) {
        val = transmigrate_value(val, k->home_region);
    }

    // Check continuation is still valid (generation check)
    if (k->capture_gen != k->frames->generation) {
        return mk_error("Stale continuation");
    }

    // Restore stack chunks
    restore_stack_chunks(k->frames);

    // Jump to saved point with value
    return switch_to_continuation(k, val);
}
```

---

## Memory Safety Integration

### Regions and Continuations

Each continuation captures its **home region**:

```
┌─────────────────────────────────────────┐
│ Region A (main)                         │
│                                         │
│  (prompt 'scheduler                     │
│    ┌─────────────────────────────────┐  │
│    │ Region B (task 1)               │  │
│    │                                 │  │
│    │  (control 'scheduler k          │  │
│    │    ;; k.home_region = B         │  │
│    │    ...)                         │  │
│    └─────────────────────────────────┘  │
│  )                                      │
└─────────────────────────────────────────┘
```

### Transmigration on Continuation Invoke

When `(k val)` is called, if `val` came from a different region:

```c
Value* transmigrate_value(Value* val, IRegion* dest) {
    TransmigrationContext* ctx = transmigration_new(dest);
    TransmigrationError err;

    Value* copy = transmigrate(ctx, val, &err);

    transmigration_free(ctx);
    return copy;
}
```

This ensures no cross-region dangling pointers.

### Generation Isolation

When a continuation is invoked, increment generations to isolate:

```c
void isolate_continuation(Continuation* k) {
    uint32_t delta = random_generation_delta();

    // Walk all objects in continuation's stack chunks
    for (StackChunk* chunk = k->frames; chunk; chunk = chunk->next) {
        walk_chunk_objects(chunk, ^(Value* v) {
            v->generation += delta;
        });
    }

    // Update continuation's capture_gen
    k->capture_gen += delta;
}
```

### Stack Chunk Reference Counting

Continuations share stack chunks:

```c
Continuation* capture_continuation(PromptFrame* prompt) {
    Continuation* k = alloc_continuation();
    k->frames = current_chunk();

    // Increment refcount on all captured chunks
    for (StackChunk* c = k->frames; c != prompt->chunk->next; c = c->prev) {
        c->refcount++;
    }

    k->home_region = current_region();
    k->capture_gen = current_generation();
    k->prompt_tag = prompt->tag;

    return k;
}

void release_continuation(Continuation* k) {
    // Decrement refcount, free chunks when zero
    for (StackChunk* c = k->frames; c; c = c->prev) {
        if (--c->refcount == 0) {
            return_chunk_to_freelist(c);
        }
    }
    free(k);
}
```

---

## Concurrency Primitives

All built on `prompt`/`control` - no pthreads!

### Scheduler

```lisp
;; Global scheduler state
(define *run-queue* '())
(define *current-task* #f)

;; Scheduler prompt tag
(define 'sched (gensym))

;; Main scheduler loop
(define (run-scheduler)
  (prompt 'sched
    (scheduler-loop)))

(define (scheduler-loop)
  (if (empty? *run-queue*)
      'done
      (let ((task (dequeue!)))
        (set! *current-task* task)
        (let ((result ((task-cont task) (task-value task))))
          (when (task-done? task)
            (deliver-result task result))
          (scheduler-loop)))))

;; Yield to scheduler
(define (yield)
  (control 'sched k
    (enqueue! (make-task k #f))
    (scheduler-loop)))

;; Spawn new task
(define (spawn thunk)
  (control 'sched k
    (enqueue! (make-task k #f))
    (enqueue! (make-task (lambda (_) (thunk)) #f))
    (scheduler-loop)))
```

### Channels

```lisp
(define (make-chan)
  (vector 'chan '() '()))  ;; (tag senders receivers)

(define (chan-senders ch) (vector-ref ch 1))
(define (chan-receivers ch) (vector-ref ch 2))
(define (set-senders! ch v) (vector-set! ch 1 v))
(define (set-receivers! ch v) (vector-set! ch 2 v))

;; Send - blocks if no receiver
(define (chan-send ch val)
  (let ((receivers (chan-receivers ch)))
    (if (empty? receivers)
        ;; No receiver waiting - park sender
        (control 'sched k
          (set-senders! ch (cons (cons k val) (chan-senders ch)))
          (scheduler-loop))
        ;; Receiver waiting - hand off directly
        (let ((receiver (car receivers)))
          (set-receivers! ch (cdr receivers))
          (enqueue! (make-task receiver val))))))

;; Receive - blocks if no sender
(define (chan-recv ch)
  (let ((senders (chan-senders ch)))
    (if (empty? senders)
        ;; No sender waiting - park receiver
        (control 'sched k
          (set-receivers! ch (cons k (chan-receivers ch)))
          (scheduler-loop))
        ;; Sender waiting - take value, resume sender
        (let ((sender (car senders)))
          (set-senders! ch (cdr senders))
          (enqueue! (make-task (car sender) 'sent))
          (cdr sender)))))  ;; Return the value
```

### Atoms (Atomic References)

```lisp
(define (make-atom initial)
  (vector 'atom initial))

(define (atom-deref atom)
  (vector-ref atom 1))

(define (atom-reset! atom new-val)
  (vector-set! atom 1 new-val)
  new-val)

;; Compare-and-swap with retry
(define (atom-cas! atom expected new-val)
  (if (eq? (atom-deref atom) expected)
      (begin (atom-reset! atom new-val) #t)
      #f))

;; Swap with function (retries on conflict)
(define (atom-swap! atom f)
  (let loop ()
    (let* ((old (atom-deref atom))
           (new (f old)))
      (if (atom-cas! atom old new)
          new
          (begin (yield) (loop))))))  ;; Yield and retry
```

### Select (Multi-Channel)

```lisp
;; Select from multiple channels
(define (select . cases)
  (let ((ready (find-ready-case cases)))
    (if ready
        (execute-case ready)
        ;; No ready case - park on all
        (control 'sched k
          (for-each (lambda (case)
            (register-waiter case k)) cases)
          (scheduler-loop)))))
```

### Timeouts

```lisp
(define (timeout-after ms thunk)
  (let ((timer-task #f))
    (prompt 'timeout
      (set! timer-task
        (spawn (lambda ()
          (sleep ms)
          (control 'timeout k
            (enqueue! (make-task k 'timeout))))))
      (let ((result (thunk)))
        (cancel-task timer-task)
        result))))
```

---

## Scheduler

### Task Structure

```c
typedef struct Task {
    Continuation* cont;       // Saved continuation
    Value* value;             // Value to resume with
    IRegion* region;          // Task's memory region
    TaskState state;          // READY, RUNNING, PARKED, DONE
    struct Task* next;        // Run queue link
    uint64_t id;              // Unique task ID
    uint64_t deadline;        // For timeouts
} Task;

typedef enum {
    TASK_READY,
    TASK_RUNNING,
    TASK_PARKED,
    TASK_DONE
} TaskState;
```

### Run Queue

```c
typedef struct Scheduler {
    Task* run_queue_head;
    Task* run_queue_tail;
    Task* current;
    StackChunk* chunk_freelist;
    size_t task_count;
    uint64_t next_task_id;
} Scheduler;

// Thread-local scheduler
_Thread_local Scheduler* g_scheduler;

void scheduler_enqueue(Task* task) {
    task->state = TASK_READY;
    task->next = NULL;

    if (g_scheduler->run_queue_tail) {
        g_scheduler->run_queue_tail->next = task;
    } else {
        g_scheduler->run_queue_head = task;
    }
    g_scheduler->run_queue_tail = task;
}

Task* scheduler_dequeue(void) {
    Task* task = g_scheduler->run_queue_head;
    if (task) {
        g_scheduler->run_queue_head = task->next;
        if (!g_scheduler->run_queue_head) {
            g_scheduler->run_queue_tail = NULL;
        }
        task->state = TASK_RUNNING;
    }
    return task;
}
```

### Main Loop

```c
void scheduler_run(void) {
    // Establish scheduler prompt
    op_prompt(TAG_SCHEDULER, NULL, NULL);

    while (g_scheduler->run_queue_head) {
        Task* task = scheduler_dequeue();
        g_scheduler->current = task;

        // Switch to task's region
        IRegion* prev_region = current_region();
        set_current_region(task->region);

        // Resume continuation
        Value* result = invoke_continuation(task->cont, task->value);

        // Restore region
        set_current_region(prev_region);

        // Handle result
        if (task->state == TASK_DONE) {
            deliver_result(task, result);
            free_task(task);
        }
    }
}
```

### Work Stealing (Multi-Core)

For M:N threading (M green threads on N OS threads):

```c
typedef struct WorkerThread {
    pthread_t thread;
    Scheduler local_scheduler;
    _Atomic(Task*) steal_queue;  // Lock-free steal queue
} WorkerThread;

WorkerThread workers[NUM_CORES];

// Try to steal work from another worker
Task* try_steal(int worker_id) {
    for (int i = 0; i < NUM_CORES; i++) {
        if (i == worker_id) continue;

        Task* stolen = atomic_exchange(&workers[i].steal_queue, NULL);
        if (stolen) return stolen;
    }
    return NULL;
}
```

---

## Implementation

### C Runtime Additions

```c
// In types.h - extend Tag enum
typedef enum {
    T_INT, T_SYM, T_CELL, T_NIL, T_PRIM, T_MENV, T_CODE, T_LAMBDA,
    T_ERROR, T_BOX, T_CONT, T_CHAN, T_PROCESS, T_TASK, T_PROMPT
} Tag;

// Continuation value
Value* mk_cont(Continuation* k);
Continuation* cont_get(Value* v);
int is_cont(Value* v);

// Prompt/control primitives
Value* prim_prompt(Value* args, Value* menv);
Value* prim_control(Value* args, Value* menv);

// Stack chunk management
StackChunk* alloc_chunk(size_t size);
void free_chunk(StackChunk* chunk);
StackChunk* current_chunk(void);

// Scheduler primitives
Value* prim_spawn(Value* args, Value* menv);
Value* prim_yield(Value* args, Value* menv);
```

### Evaluator Changes

```c
Value* eval(Value* expr, Value* menv) {
    // ... existing cases ...

    // prompt special form
    if (sym_eq_str(car(expr), "prompt")) {
        Value* tag = eval(cadr(expr), menv);
        Value* body = caddr(expr);
        return op_prompt(sym_to_tag(tag), body, menv);
    }

    // control special form
    if (sym_eq_str(car(expr), "control")) {
        Value* tag = eval(cadr(expr), menv);
        Value* k_name = caddr(expr);
        Value* body = cadddr(expr);
        return op_control(sym_to_tag(tag), k_name, body, menv);
    }

    // Continuation application
    if (is_cont(fn)) {
        Continuation* k = cont_get(fn);
        Value* val = eval(cadr(expr), menv);
        return invoke_continuation(k, val);
    }

    // ... rest of eval ...
}
```

### Memory Layout

```
Continuation (40 bytes):
┌─────────────────────────────────────────┐
│ frames: StackChunk*        (8 bytes)    │
│ home_region: IRegion*      (8 bytes)    │
│ capture_gen: uint64_t      (8 bytes)    │
│ prompt_tag: uint32_t       (4 bytes)    │
│ padding                    (4 bytes)    │
│ prompt_menv: Value*        (8 bytes)    │
└─────────────────────────────────────────┘

StackChunk (64 bytes):
┌─────────────────────────────────────────┐
│ base: void*                (8 bytes)    │
│ sp: void*                  (8 bytes)    │
│ limit: void*               (8 bytes)    │
│ size: size_t               (8 bytes)    │
│ prev: StackChunk*          (8 bytes)    │
│ next: StackChunk*          (8 bytes)    │
│ refcount: uint32_t         (4 bytes)    │
│ flags: uint32_t            (4 bytes)    │
│ region: IRegion*           (8 bytes)    │
└─────────────────────────────────────────┘

Task (80 bytes):
┌─────────────────────────────────────────┐
│ cont: Continuation*        (8 bytes)    │
│ value: Value*              (8 bytes)    │
│ region: IRegion*           (8 bytes)    │
│ state: TaskState           (4 bytes)    │
│ padding                    (4 bytes)    │
│ next: Task*                (8 bytes)    │
│ id: uint64_t               (8 bytes)    │
│ deadline: uint64_t         (8 bytes)    │
│ park_reason: void*         (8 bytes)    │
│ result: Value*             (8 bytes)    │
│ parent: Task*              (8 bytes)    │
└─────────────────────────────────────────┘
```

---

## Examples

### Producer-Consumer

```lisp
(define (producer ch n)
  (do ((i 0 (+ i 1)))
      ((>= i n))
    (chan-send ch i)
    (display "Sent: ") (display i) (newline)))

(define (consumer ch n)
  (do ((i 0 (+ i 1)))
      ((>= i n))
    (let ((v (chan-recv ch)))
      (display "Received: ") (display v) (newline))))

(define (main)
  (let ((ch (make-chan)))
    (spawn (lambda () (producer ch 10)))
    (spawn (lambda () (consumer ch 10)))
    (run-scheduler)))
```

### Async/Await Pattern

```lisp
;; async is just spawn that returns a "future"
(define (async thunk)
  (let ((result-chan (make-chan)))
    (spawn (lambda ()
      (chan-send result-chan (thunk))))
    result-chan))

;; await is just chan-recv
(define (await future)
  (chan-recv future))

;; Usage
(define (main)
  (let ((f1 (async (lambda () (compute-expensive-1))))
        (f2 (async (lambda () (compute-expensive-2)))))
    ;; Both run concurrently
    (+ (await f1) (await f2))))
```

### Generator/Iterator

```lisp
;; Generator using continuations
(define (make-generator producer)
  (let ((cont #f)
        (done #f))
    (lambda (cmd)
      (case cmd
        ((next)
         (if done
             'done
             (prompt 'gen
               (if cont
                   (cont 'resume)
                   (producer (lambda (v)
                     (control 'gen k
                       (set! cont k)
                       v)))))))
        ((close)
         (set! done #t))))))

;; Usage
(define gen
  (make-generator
    (lambda (yield)
      (yield 1)
      (yield 2)
      (yield 3))))

(gen 'next)  ;; => 1
(gen 'next)  ;; => 2
(gen 'next)  ;; => 3
(gen 'next)  ;; => 'done
```

### Error Handling with Continuations

```lisp
;; try/catch as prompts
(define (try thunk handler)
  (prompt 'error
    (thunk)))

(define (throw err)
  (control 'error k
    ;; Don't invoke k - just return error
    (cons 'error err)))

;; Usage
(try
  (lambda ()
    (if (< x 0)
        (throw "negative value")
        (sqrt x)))
  (lambda (err)
    (display "Error: ") (display err)))
```

### Structured Concurrency (Nursery)

```lisp
;; All spawned tasks must complete before nursery exits
(define (with-nursery body)
  (let ((tasks '())
        (errors '()))
    (prompt 'nursery
      (body (lambda (thunk)  ;; spawn-in-nursery
              (set! tasks (cons (spawn thunk) tasks))))
      ;; Wait for all tasks
      (for-each await-task tasks)
      (if (empty? errors)
          'ok
          (throw (cons 'nursery-errors errors))))))

;; Usage
(with-nursery (lambda (spawn)
  (spawn (lambda () (task-1)))
  (spawn (lambda () (task-2)))
  (spawn (lambda () (task-3)))))
;; All three tasks complete before this returns
```

---

## Iterators with Continuations

### Why Continuations for Iterators?

Traditional iterators require explicit state management:

```c
// Traditional iterator - manual state machine
typedef struct RangeIterator {
    int current;
    int end;
} RangeIterator;

int range_next(RangeIterator* it) {
    if (it->current >= it->end) return -1;  // Done
    return it->current++;
}
```

With continuations, the iterator is just a suspended function:

```lisp
;; Continuation-based - natural control flow
(define (range n)
  (make-generator
    (lambda (yield)
      (do ((i 0 (+ i 1)))
          ((>= i n))
        (yield i)))))
```

### Generator Protocol

```
┌─────────────────────────────────────────────────────────────┐
│  Generator Lifecycle                                        │
│                                                             │
│  CREATED ──next()──▶ RUNNING ──yield()──▶ SUSPENDED        │
│                          │                    │             │
│                          │                    │             │
│                     (return)              next()            │
│                          │                    │             │
│                          ▼                    ▼             │
│                        DONE ◀───────────── RUNNING          │
└─────────────────────────────────────────────────────────────┘
```

### Built-in Generators

```lisp
;; Range generator
(define (range start end . step)
  (make-generator
    (lambda (yield)
      (let ((s (if (empty? step) 1 (car step))))
        (do ((i start (+ i s)))
            ((>= i end))
          (yield i))))))

;; Infinite sequence
(define (iterate f init)
  (make-generator
    (lambda (yield)
      (let loop ((x init))
        (yield x)
        (loop (f x))))))

;; Filter generator
(define (gen-filter pred gen)
  (make-generator
    (lambda (yield)
      (let loop ()
        (let ((v (gen-next gen)))
          (unless (gen-done? gen)
            (when (pred v) (yield v))
            (loop)))))))

;; Map generator
(define (gen-map f gen)
  (make-generator
    (lambda (yield)
      (let loop ()
        (let ((v (gen-next gen)))
          (unless (gen-done? gen)
            (yield (f v))
            (loop)))))))

;; Take n elements
(define (gen-take n gen)
  (make-generator
    (lambda (yield)
      (do ((i 0 (+ i 1)))
          ((or (>= i n) (gen-done? gen)))
        (yield (gen-next gen))))))

;; Zip generators
(define (gen-zip . gens)
  (make-generator
    (lambda (yield)
      (let loop ()
        (let ((vals (map gen-next gens)))
          (unless (any gen-done? gens)
            (yield vals)
            (loop)))))))
```

### Iterator Protocol (for-each integration)

```lisp
;; Generic for-each that works with generators
(define (for-each-gen f gen)
  (let loop ()
    (let ((v (gen-next gen)))
      (unless (gen-done? gen)
        (f v)
        (loop)))))

;; Collect to list
(define (gen->list gen)
  (let loop ((acc '()))
    (let ((v (gen-next gen)))
      (if (gen-done? gen)
          (reverse acc)
          (loop (cons v acc))))))

;; Fold over generator
(define (gen-fold f init gen)
  (let loop ((acc init))
    (let ((v (gen-next gen)))
      (if (gen-done? gen)
          acc
          (loop (f acc v))))))
```

### Lazy Evaluation with Generators

```lisp
;; Fibonacci sequence (infinite)
(define fibs
  (make-generator
    (lambda (yield)
      (let loop ((a 0) (b 1))
        (yield a)
        (loop b (+ a b))))))

;; Usage: first 10 fibs
(gen->list (gen-take 10 fibs))
;; => (0 1 1 2 3 5 8 13 21 34)

;; Prime sieve (infinite)
(define (sieve gen)
  (make-generator
    (lambda (yield)
      (let ((p (gen-next gen)))
        (yield p)
        (for-each-gen yield
          (sieve
            (gen-filter
              (lambda (x) (not (= 0 (modulo x p))))
              gen)))))))

(define primes (sieve (iterate (lambda (x) (+ x 1)) 2)))
```

---

## Async Callbacks with Continuations

### Promise-Based Async

```
┌─────────────────────────────────────────────────────────────┐
│  Promise Lifecycle                                          │
│                                                             │
│  PENDING ───resolve(v)───▶ FULFILLED                        │
│      │                                                      │
│      └────reject(e)──────▶ REJECTED                         │
│                                                             │
│  Waiters are resumed when promise settles.                  │
└─────────────────────────────────────────────────────────────┘
```

### Async/Await Pattern

```lisp
;; async wraps a thunk, returns a promise
(define (async thunk)
  (let ((p (make-promise)))
    (spawn
      (lambda ()
        (try
          (lambda ()
            (promise-resolve! p (thunk)))
          (lambda (err)
            (promise-reject! p err)))))
    p))

;; await suspends until promise resolves
(define (await promise)
  (if (promise-settled? promise)
      (promise-value promise)
      (control 'async k
        (promise-then promise
          (lambda (v) (schedule (lambda () (k v))))
          (lambda (e) (schedule (lambda () (throw e)))))
        (scheduler-loop))))

;; Usage
(define (fetch-user-data id)
  (async
    (lambda ()
      (let* ((user (await (http-get (string-append "/users/" id))))
             (posts (await (http-get (string-append "/users/" id "/posts")))))
        (list user posts)))))
```

### Callback-to-Promise Conversion

```lisp
;; Convert callback-based API to promise
(define (promisify callback-fn)
  (lambda args
    (let ((p (make-promise)))
      (apply callback-fn
        (append args
          (list (lambda (err result)
                  (if err
                      (promise-reject! p err)
                      (promise-resolve! p result))))))
      p)))

;; Example: promisify file read
(define read-file-async (promisify read-file-callback))

;; Use with await
(define contents (await (read-file-async "data.txt")))
```

### Promise Combinators

```lisp
;; Wait for all promises
(define (promise-all promises)
  (let ((p (make-promise))
        (results (make-vector (length promises)))
        (remaining (box (length promises))))
    (for-each-indexed
      (lambda (i promise)
        (promise-then promise
          (lambda (v)
            (vector-set! results i v)
            (box-set! remaining (- (box-get remaining) 1))
            (when (= 0 (box-get remaining))
              (promise-resolve! p (vector->list results))))
          (lambda (e)
            (promise-reject! p e))))
      promises)
    p))

;; Race - first to settle wins
(define (promise-race promises)
  (let ((p (make-promise))
        (settled (box #f)))
    (for-each
      (lambda (promise)
        (promise-then promise
          (lambda (v)
            (unless (box-get settled)
              (box-set! settled #t)
              (promise-resolve! p v)))
          (lambda (e)
            (unless (box-get settled)
              (box-set! settled #t)
              (promise-reject! p e)))))
      promises)
    p))

;; Any - first success wins
(define (promise-any promises)
  (let ((p (make-promise))
        (errors (make-vector (length promises)))
        (remaining (box (length promises))))
    (for-each-indexed
      (lambda (i promise)
        (promise-then promise
          (lambda (v) (promise-resolve! p v))
          (lambda (e)
            (vector-set! errors i e)
            (box-set! remaining (- (box-get remaining) 1))
            (when (= 0 (box-get remaining))
              (promise-reject! p (vector->list errors))))))
      promises)
    p))
```

### Timeout and Cancellation

```lisp
;; Timeout wrapper
(define (with-timeout ms promise)
  (promise-race
    (list promise
          (async (lambda ()
                   (sleep ms)
                   (throw 'timeout))))))

;; Cancellable operation
(define (cancellable thunk)
  (let ((cancelled (box #f))
        (p (make-promise)))
    (spawn
      (lambda ()
        (let ((result (thunk)))
          (unless (box-get cancelled)
            (promise-resolve! p result)))))
    (values p
            (lambda ()  ;; cancel function
              (box-set! cancelled #t)
              (promise-reject! p 'cancelled)))))

;; Usage
(let-values (((promise cancel) (cancellable long-operation)))
  (with-timeout 5000 promise))
```

### Event Streams (AsyncIterator)

```lisp
;; Async generator for event streams
(define (async-generator producer)
  (let ((queue (make-chan 16))
        (done (box #f)))
    ;; Producer runs in background
    (spawn
      (lambda ()
        (producer
          (lambda (v) (chan-send queue v))   ;; yield
          (lambda () (box-set! done #t)))))  ;; done
    ;; Return async iterator
    (lambda ()
      (if (box-get done)
          'done
          (await (async (lambda () (chan-recv queue))))))))

;; Usage: WebSocket messages
(define messages
  (async-generator
    (lambda (yield done)
      (websocket-on-message ws yield)
      (websocket-on-close ws done))))

;; Async for-each
(define (async-for-each f async-gen)
  (async
    (lambda ()
      (let loop ()
        (let ((v (async-gen)))
          (unless (eq? v 'done)
            (await (async (lambda () (f v))))
            (loop)))))))
```

---

## Comparison

### Lines of Code

| Feature | Pthread Version | Continuation Version |
|---------|-----------------|---------------------|
| Channels | ~150 LOC | ~30 LOC |
| Scheduler | ~200 LOC | ~50 LOC |
| Atoms | ~80 LOC | ~15 LOC |
| Spawn/Join | ~100 LOC | ~20 LOC |
| **Total** | **~530 LOC** | **~115 LOC** |

### Performance Characteristics

| Metric | Pthread | Continuation |
|--------|---------|--------------|
| Task creation | ~10μs | ~100ns |
| Context switch | ~1μs | ~50ns |
| Memory per task | ~8KB min | ~100 bytes |
| Max tasks | ~10K | ~1M+ |
| Blocking FFI | Native | Needs wrapper |

### Composability

```lisp
;; Continuations compose naturally

;; Generators + Channels
(define (chan-from-generator gen)
  (let ((ch (make-chan)))
    (spawn (lambda ()
      (let loop ((v (gen 'next)))
        (unless (eq? v 'done)
          (chan-send ch v)
          (loop (gen 'next))))
      (chan-close ch)))
    ch))

;; Error handling + Concurrency
(define (supervised-task thunk)
  (try
    (lambda () (spawn thunk))
    (lambda (err)
      (log-error err)
      (supervised-task thunk))))  ;; Restart on failure
```

---

## Migration Path

### Phase 1: Core Continuation Support
1. Add `T_CONT` type and `Continuation` struct
2. Implement `prompt`/`control` special forms
3. Add stack chunk management
4. Basic tests

### Phase 2: Scheduler
1. Implement run queue
2. Add `spawn`, `yield` primitives
3. Reimplement channels using continuations
4. Remove pthread channel implementation

### Phase 3: Full Concurrency
1. Add atoms, select, timeouts
2. Structured concurrency (nurseries)
3. Work stealing for multi-core

### Phase 4: Optimization
1. One-shot continuation optimization
2. Inline small continuations
3. Stack chunk pooling
4. Profile and tune

---

## References

- [Representing Control: A Study of the CPS Transformation](https://www.cs.indiana.edu/~dyb/pubs/representingcontrol.pdf) - Danvy & Filinski
- [Abstracting Control](https://www.cs.indiana.edu/~dyb/pubs/LaSC-1-1-pp79-104.pdf) - Danvy & Filinski
- [A Monadic Framework for Delimited Continuations](https://www.cs.indiana.edu/~dyb/pubs/monadicDC.pdf) - Dybvig et al.
- [Virtual Threads in Vale](VirtualThreads.md) - Vale docs
- [Seamless, Fearless, Structured Concurrency](https://verdagon.dev/blog/seamless-fearless-structured-concurrency) - Vale blog
- [How Stacks Are Handled in Go](https://blog.cloudflare.com/how-stacks-are-handled-in-go/) - Cloudflare
