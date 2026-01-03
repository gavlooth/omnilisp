/*
 * Continuation-Based Concurrency for Purple Runtime
 *
 * This module provides:
 * - Delimited continuations (prompt/control)
 * - Green thread scheduler
 * - Continuation-based channels
 * - Generators/iterators
 * - Async/await (promises)
 *
 * Design: CEK machine with explicit continuation frames.
 * All "stack" is heap-allocated, enabling capture without copying.
 */

#ifndef PURPLE_CONTINUATION_H
#define PURPLE_CONTINUATION_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <setjmp.h>

/* Forward declarations */
typedef struct Obj Obj;
typedef struct Frame Frame;
typedef struct Continuation Continuation;
typedef struct Task Task;
typedef struct Scheduler Scheduler;
typedef struct GreenChannel GreenChannel;
typedef struct Generator Generator;
typedef struct Promise Promise;

/* ========== Continuation Frame Types ========== */

typedef enum {
    FRAME_APP_FN,       /* Evaluating function position */
    FRAME_APP_ARGS,     /* Evaluating arguments */
    FRAME_APP_DONE,     /* Ready to apply */
    FRAME_IF_TEST,      /* Evaluated test, pick branch */
    FRAME_LET_BIND,     /* Evaluated binding value */
    FRAME_LET_BODY,     /* Evaluating let body */
    FRAME_SEQ,          /* Sequence, more exprs to eval */
    FRAME_PROMPT,       /* Delimiter frame */
    FRAME_HANDLER,      /* Effect handler */
    FRAME_YIELD,        /* Generator yield point */
    FRAME_AWAIT,        /* Promise await point */
} FrameTag;

/* Continuation frame - one "stack frame" of pending work */
struct Frame {
    FrameTag tag;
    Frame* prev;            /* Previous frame (toward base) */
    Obj* env;               /* Environment at this point */
    uint32_t refcount;      /* For sharing frames */

    union {
        /* FRAME_APP_* */
        struct {
            Obj* fn;            /* Function (when evaluated) */
            Obj* args_done;     /* Already evaluated args (reversed) */
            Obj* args_todo;     /* Args still to evaluate */
        } app;

        /* FRAME_IF_TEST */
        struct {
            Obj* then_branch;
            Obj* else_branch;
        } if_test;

        /* FRAME_LET_* */
        struct {
            Obj* var;           /* Variable to bind */
            Obj* body;          /* Body to evaluate */
            Obj* bindings_todo; /* Remaining bindings (let*) */
        } let;

        /* FRAME_SEQ */
        struct {
            Obj* remaining;     /* Remaining expressions */
        } seq;

        /* FRAME_PROMPT */
        struct {
            uint32_t tag;       /* Prompt identifier */
            jmp_buf escape;     /* For non-local return */
            Obj* result;        /* Result passed back */
            Obj* handler;       /* Optional effect handler */
        } prompt;

        /* FRAME_YIELD */
        struct {
            Generator* gen;     /* Owning generator */
        } yield;

        /* FRAME_AWAIT */
        struct {
            Promise* promise;   /* Promise being awaited */
        } await;
    };
};

/* ========== Continuation ========== */

/*
 * A captured continuation is a chain of frames from the capture
 * point back to a delimiter (prompt). When invoked, these frames
 * are "replayed" to continue the computation.
 */
struct Continuation {
    Frame* frames;          /* Captured frame chain */
    uint32_t prompt_tag;    /* Which prompt this belongs to */
    Obj* prompt_env;        /* Environment at prompt point */
    uint32_t refcount;      /* Reference count */
    bool one_shot;          /* Can only be invoked once? */
    bool invoked;           /* Has been invoked? (for one-shot) */
};

/* ========== Task (Green Thread) ========== */

typedef enum {
    TASK_READY,             /* In run queue, ready to execute */
    TASK_RUNNING,           /* Currently executing */
    TASK_PARKED,            /* Waiting on channel/promise */
    TASK_DONE,              /* Completed */
} TaskState;

struct Task {
    uint64_t id;            /* Unique task ID */
    TaskState state;

    /* Saved execution state */
    Obj* expr;              /* Current expression (C in CEK) */
    Obj* env;               /* Current environment (E in CEK) */
    Frame* cont;            /* Current continuation (K in CEK) */
    Obj* value;             /* Value to resume with */

    /* Scheduling */
    Task* next;             /* Next in queue */
    Task* prev;             /* Previous in queue (for removal) */

    /* Parking info */
    void* park_reason;      /* Channel/promise we're waiting on */
    Obj* park_value;        /* Value to send (for channel send) */

    /* Result */
    Obj* result;            /* Final result when TASK_DONE */
    Promise* completion;    /* Promise fulfilled when done */
};

/* ========== Scheduler ========== */

struct Scheduler {
    /* Run queue (doubly-linked for O(1) removal) */
    Task* ready_head;
    Task* ready_tail;

    /* Current task */
    Task* current;

    /* Task management */
    uint64_t next_task_id;
    uint64_t task_count;

    /* Frame pool (avoid malloc in hot path) */
    Frame* frame_freelist;
    size_t frame_pool_size;

    /* Prompt stack for current task */
    Frame* prompt_stack;

    /* Statistics */
    uint64_t tasks_created;
    uint64_t tasks_completed;
    uint64_t context_switches;
};

/* ========== Green Channel ========== */

/*
 * Continuation-based channel. No pthread primitives.
 * Blocking operations park the task and yield to scheduler.
 */
struct GreenChannel {
    /* Buffer (for buffered channels) */
    Obj** buffer;
    int capacity;           /* 0 = unbuffered (rendezvous) */
    int count;
    int head;
    int tail;

    /* Wait queues (parked tasks) */
    Task* send_waiters;     /* Tasks waiting to send */
    Task* recv_waiters;     /* Tasks waiting to receive */

    /* State */
    bool closed;
    uint32_t refcount;
};

/* ========== Generator (Iterator) ========== */

typedef enum {
    GEN_CREATED,            /* Not yet started */
    GEN_SUSPENDED,          /* Yielded, waiting for next() */
    GEN_RUNNING,            /* Currently executing */
    GEN_DONE,               /* Exhausted */
} GeneratorState;

struct Generator {
    GeneratorState state;

    /* Saved continuation when suspended */
    Continuation* cont;

    /* Current/last yielded value */
    Obj* current;

    /* The producer function */
    Obj* producer;

    /* Reference count */
    uint32_t refcount;
};

/* ========== Promise (Async) ========== */

typedef enum {
    PROMISE_PENDING,
    PROMISE_FULFILLED,
    PROMISE_REJECTED,
} PromiseState;

struct Promise {
    PromiseState state;

    /* Result (when fulfilled/rejected) */
    Obj* value;
    Obj* error;

    /* Waiters (tasks awaiting this promise) */
    Task* waiters;

    /* Callbacks (for non-task contexts) */
    Obj* on_fulfill;        /* List of (callback . env) */
    Obj* on_reject;         /* List of (callback . env) */

    /* Reference count */
    uint32_t refcount;
};

/* ========== API: Frame Management ========== */

Frame* frame_alloc(FrameTag tag);
void frame_free(Frame* f);
Frame* frame_clone(Frame* f);
void frame_inc_ref(Frame* f);
void frame_dec_ref(Frame* f);

/* ========== API: Continuation Operations ========== */

/* Create a new prompt (delimiter) */
Obj* cont_prompt(uint32_t tag, Obj* body, Obj* env, Obj* handler);

/* Capture continuation up to matching prompt */
Continuation* cont_capture(uint32_t tag);

/* Invoke a continuation with a value */
Obj* cont_invoke(Continuation* k, Obj* value);

/* Create continuation value */
Obj* mk_continuation_obj(Continuation* k);

/* Check if continuation is still valid */
bool cont_is_valid(Continuation* k);

/* Release continuation */
void cont_release(Continuation* k);

/* ========== API: Scheduler ========== */

/* Initialize the global scheduler */
void scheduler_init(void);

/* Shutdown scheduler */
void scheduler_shutdown(void);

/* Get current scheduler (thread-local) */
Scheduler* scheduler_current(void);

/* Run scheduler until all tasks complete */
void scheduler_run(void);

/* Step one task (for integration with event loops) */
bool scheduler_step(void);

/* Is scheduler idle? */
bool scheduler_is_idle(void);

/* ========== API: Green Threads ========== */

/* Spawn a new green thread */
Task* spawn_green(Obj* thunk);

/* Spawn and get completion promise */
Promise* spawn_async(Obj* thunk);

/* Yield to scheduler (cooperative) */
void yield_green(void);

/* Get current task */
Task* task_current(void);

/* Park current task (internal) */
void task_park(void* reason, Obj* value);

/* Unpark a task (internal) */
void task_unpark(Task* t, Obj* value);

/* ========== API: Green Channels ========== */

/* Create channel */
GreenChannel* green_channel_create(int capacity);

/* Send value (may park) */
void green_channel_send(GreenChannel* ch, Obj* value);

/* Receive value (may park) */
Obj* green_channel_recv(GreenChannel* ch);

/* Try send (non-blocking) */
bool green_channel_try_send(GreenChannel* ch, Obj* value);

/* Try receive (non-blocking) */
Obj* green_channel_try_recv(GreenChannel* ch, bool* ok);

/* Close channel */
void green_channel_close(GreenChannel* ch);

/* Is channel closed? */
bool green_channel_is_closed(GreenChannel* ch);

/* Release channel */
void green_channel_release(GreenChannel* ch);

/* ========== API: Generators (Iterators) ========== */

/* Create a generator from a producer function */
Generator* generator_create(Obj* producer);

/* Get next value (resumes generator) */
Obj* generator_next(Generator* g);

/* Is generator exhausted? */
bool generator_is_done(Generator* g);

/* Yield a value from within generator (parks generator) */
void generator_yield(Obj* value);

/* Close generator early */
void generator_close(Generator* g);

/* Release generator */
void generator_release(Generator* g);

/* Create generator Obj wrapper */
Obj* mk_generator_obj(Generator* g);

/* ========== API: Promises (Async/Await) ========== */

/* Create a pending promise */
Promise* promise_create(void);

/* Resolve promise with value */
void promise_resolve(Promise* p, Obj* value);

/* Reject promise with error */
void promise_reject(Promise* p, Obj* error);

/* Await promise (parks current task) */
Obj* promise_await(Promise* p);

/* Add fulfillment callback */
void promise_then(Promise* p, Obj* on_fulfill, Obj* on_reject);

/* Is promise settled? */
bool promise_is_settled(Promise* p);

/* Get promise state */
PromiseState promise_state(Promise* p);

/* Release promise */
void promise_release(Promise* p);

/* Create promise Obj wrapper */
Obj* mk_promise_obj(Promise* p);

/* ========== API: Select (Multi-Wait) ========== */

typedef struct SelectCase {
    enum { SELECT_RECV, SELECT_SEND, SELECT_DEFAULT } op;
    GreenChannel* channel;
    Obj* value;             /* For send */
    Obj** result;           /* For recv */
} SelectCase;

/* Select from multiple channel operations */
int green_select(SelectCase* cases, int count);

/* ========== API: Timeout ========== */

/* Create a timer that resolves after ms milliseconds */
Promise* timer_after(uint64_t ms);

/* Await with timeout (returns NULL on timeout) */
Obj* await_timeout(Promise* p, uint64_t ms);

/* ========== Prompt Tags ========== */

/* Reserved prompt tags */
#define PROMPT_TAG_SCHEDULER  0x00000001
#define PROMPT_TAG_GENERATOR  0x00000002
#define PROMPT_TAG_ASYNC      0x00000003
#define PROMPT_TAG_EXCEPTION  0x00000004
#define PROMPT_TAG_USER_BASE  0x00001000

/* Generate unique prompt tag */
uint32_t prompt_tag_generate(void);

/* ========== Thread-Local State ========== */

/* Get/set current environment */
Obj* cont_get_env(void);
void cont_set_env(Obj* env);

/* Get/set current continuation */
Frame* cont_get_current(void);
void cont_set_current(Frame* f);

#endif /* PURPLE_CONTINUATION_H */
