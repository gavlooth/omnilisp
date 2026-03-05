module lisp;

import std::core::mem;
import std::atomic::types;
import std::io;
import std::thread;
import main;

// Scheduler core: state, offload queue, initialization.
// ============================================================
// Scheduler core (state, wakeups, async I/O, fiber internals)
// ============================================================
// ============================================================
// Fiber Scheduler — coroutines + effect handler
//
// A fiber is a coroutine managed by the scheduler.
// spawn creates a fiber. The scheduler resumes fibers round-robin.
// Fibers run until they yield, complete, or signal an I/O effect.
// ============================================================

const usz MAX_FIBERS = 256;
const usz NO_FIBER = MAX_FIBERS;
const usz MAX_THREAD_TASKS = 256;
const usz NO_TASK = MAX_THREAD_TASKS;

enum FiberState : char {
    FIBER_READY,
    FIBER_RUNNING,
    FIBER_BLOCKED,
    FIBER_DONE
}

struct FiberEntry {
    Value* coroutine;    // COROUTINE value
    Value* result;       // Final result (set on completion)
    Value* pending_result;   // Completion result while waiting on children
    usz parent_id;
    usz live_children;
    usz await_target;
    bool awaiting_fiber;
    bool waiting_children;
    FiberState state;
}

struct PendingTcpRead {
    bool active;
    bool completed;
    bool consumed;
    bool timed_out;
    int fd;
    usz max_bytes;
    char* buffer;
    long nread;
    int error_code;
    void* poll_handle;
    void* timer_handle;
}

struct PendingOffload {
    bool active;
    bool completed;
    void* completion; // OffloadCompletion*
}

struct SharedBlob {
    types::Atomic{long} refcount;
    char* bytes;
    usz len;
}

enum WakeupEventType : char {
    WAKEUP_POLL_READABLE,
    WAKEUP_POLL_ERROR,
    WAKEUP_TIMER_EXPIRED,
    WAKEUP_OFFLOAD_READY
}

struct WakeupEvent {
    usz fiber_id;
    WakeupEventType type;
    int status;
    void* payload;
}

const usz WAKEUP_RING_SIZE = 256;
const usz WAKEUP_RING_MASK = WAKEUP_RING_SIZE - 1;

enum OffloadOp : char {
    OFFLOAD_SLEEP_MS,
    OFFLOAD_GZIP,
    OFFLOAD_DEFLATE
}

enum OffloadResultKind : char {
    OFFLOAD_RES_NIL,
    OFFLOAD_RES_INT,
    OFFLOAD_RES_BYTES,
    OFFLOAD_RES_ERROR
}

struct OffloadWork {
    usz fiber_id;
    usz task_id;
    OffloadOp op;
    long int_arg;
    SharedBlob* blob;
}

struct OffloadCompletion {
    OffloadResultKind kind;
    long int_result;
    SharedBlob* blob;
    char* error_msg;
    usz error_len;
}

const usz OFFLOAD_QUEUE_SIZE = 256;
const usz OFFLOAD_QUEUE_MASK = OFFLOAD_QUEUE_SIZE - 1;

struct OffloadQueue {
    OffloadWork[OFFLOAD_QUEUE_SIZE] ring;
    usz head;
    usz tail;
    usz count;
    thread::Mutex mu;
    thread::ConditionVariable cv;
    bool running;
    bool initialized;
    thread::Thread worker;
}

enum ThreadTaskState : char {
    THREAD_TASK_PENDING,
    THREAD_TASK_RUNNING,
    THREAD_TASK_DONE,
    THREAD_TASK_CANCELLED
}

struct ThreadTaskEntry {
    bool active;
    bool cancel_requested;
    ThreadTaskState state;
    OffloadCompletion* completion;
}

struct Scheduler {
    FiberEntry[MAX_FIBERS] fibers;
    PendingTcpRead[MAX_FIBERS] pending_reads;
    PendingOffload[MAX_FIBERS] pending_offloads;
    WakeupEvent[WAKEUP_RING_SIZE] wakeup_ring;
    types::Atomic{bool}[WAKEUP_RING_SIZE] wakeup_ready;
    types::Atomic{usz} wakeup_head;    // producers reserve slots
    types::Atomic{usz} wakeup_tail;    // consumer drains slots
    usz fiber_count;
    usz current_fiber_id;
    bool running;
    bool uv_loop_ready;
    void* uv_loop;
    void* async_handle;                // uv_async_t*
    OffloadQueue offload_queue;
    ThreadTaskEntry[MAX_THREAD_TASKS] thread_tasks;
    thread::Mutex thread_task_mu;
    bool thread_task_mu_ready;
    // Fairness: rotate resume start index so no fiber is permanently first
    usz resume_offset;
    // Observability: wakeup drops (ring full), updated by producer contexts.
    types::Atomic{usz} wakeup_drops;
}

// Global scheduler (single-threaded)
Scheduler g_scheduler;
bool g_scheduler_initialized = false;

fn void scheduler_async_noop_cb(void* handle) {
    // Intentionally empty; uv_async_send only wakes the loop.
    (void)handle;
}

fn void scheduler_reset_wakeup_queue() {
    g_scheduler.wakeup_head.store(0);
    g_scheduler.wakeup_tail.store(0);
    for (usz i = 0; i < WAKEUP_RING_SIZE; i++) {
        g_scheduler.wakeup_ready[i].store(false);
    }
}

fn bool scheduler_str_eq(char[] a, char[] b) @inline {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) if (a[i] != b[i]) return false;
    return true;
}

fn SharedBlob* shared_blob_new_owned(char* owned_bytes, usz len) {
    if (owned_bytes == null) return null;
    SharedBlob* blob = (SharedBlob*)mem::malloc(SharedBlob.sizeof);
    if (blob == null) {
        mem::free(owned_bytes);
        return null;
    }
    blob.refcount.store(1);
    blob.bytes = owned_bytes;
    blob.len = len;
    return blob;
}

fn SharedBlob* shared_blob_new_copy(char[] bytes) {
    usz cap = bytes.len + 1;
    if (cap < 32) cap = 32;
    char* copy = (char*)mem::malloc(cap);
    if (copy == null) return null;
    for (usz i = 0; i < bytes.len; i++) copy[i] = bytes[i];
    copy[bytes.len] = 0;
    return shared_blob_new_owned(copy, bytes.len);
}

fn void shared_blob_release(SharedBlob* blob) @inline {
    if (blob == null) return;
    long old = blob.refcount.add(-1);
    if (old <= 1) {
        if (blob.bytes != null) {
            mem::free(blob.bytes);
            blob.bytes = null;
        }
        mem::free(blob);
    }
}

fn void scheduler_free_offload_completion(void* raw_completion) {
    if (raw_completion == null) return;
    OffloadCompletion* completion = (OffloadCompletion*)raw_completion;
    if (completion.blob != null) {
        shared_blob_release(completion.blob);
        completion.blob = null;
    }
    if (completion.error_msg != null) {
        mem::free(completion.error_msg);
        completion.error_msg = null;
    }
    mem::free(completion);
}

fn void scheduler_set_offload_error(OffloadCompletion* completion, char[] message) {
    completion.kind = OFFLOAD_RES_ERROR;
    completion.error_len = message.len;
    completion.error_msg = (char*)mem::malloc(message.len);
    if (completion.error_msg == null) {
        completion.error_len = 0;
        return;
    }
    for (usz i = 0; i < message.len; i++) completion.error_msg[i] = message[i];
}

fn Value* scheduler_build_offload_work(Value* job, OffloadWork* work, Interp* interp) {
    if (!is_cons(job)) return raise_error(interp, "offload: expected (op args...)");
    Value* op_val = job.cons_val.car;
    if (!is_symbol(op_val)) return raise_error(interp, "offload: op must be a symbol");
    char[] op_name = interp.symbols.get_name(op_val.sym_val);
    Value* arg_list = job.cons_val.cdr;

    work.fiber_id = NO_FIBER;
    work.task_id = NO_TASK;
    work.op = OFFLOAD_SLEEP_MS;
    work.int_arg = 0;
    work.blob = null;

    if (scheduler_str_eq(op_name, "sleep-ms")) {
        if (!is_cons(arg_list) || !is_int(arg_list.cons_val.car)) {
            return raise_error(interp, "offload sleep-ms: expected integer milliseconds");
        }
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = arg_list.cons_val.car.int_val;
        return null;
    }

    if (scheduler_str_eq(op_name, "gzip") || scheduler_str_eq(op_name, "deflate")) {
        if (!is_cons(arg_list) || !is_string(arg_list.cons_val.car)) {
            return raise_error(interp, "offload compress: expected string payload");
        }
        Value* src = arg_list.cons_val.car;
        work.op = scheduler_str_eq(op_name, "gzip") ? OFFLOAD_GZIP : OFFLOAD_DEFLATE;
        work.blob = shared_blob_new_copy(src.str_chars[:src.str_len]);
        if (work.blob == null) return raise_error(interp, "offload: out of memory");
        return null;
    }

    return raise_error(interp, "offload: unsupported op");
}

fn void scheduler_init() {
    if (g_scheduler_initialized) return;

    g_scheduler = {};
    g_scheduler.fiber_count = 0;
    g_scheduler.current_fiber_id = NO_FIBER;
    g_scheduler.running = false;
    if (catch err = g_scheduler.thread_task_mu.init()) {
        g_scheduler.thread_task_mu_ready = false;
    } else {
        g_scheduler.thread_task_mu_ready = true;
    }
    scheduler_reset_wakeup_queue();
    scheduler_init_offload_queue();

    usz loop_size = uv_loop_size();
    if (loop_size > 0) {
        void* loop_mem = mem::malloc(loop_size);
        if (loop_mem != null && uv_loop_init(loop_mem) == 0) {
            g_scheduler.uv_loop_ready = true;
            g_scheduler.uv_loop = loop_mem;

            usz async_size = uv_handle_size(UV_HANDLE_ASYNC);
            if (async_size > 0) {
                void* async_mem = mem::malloc(async_size);
                if (async_mem != null) {
                    if (uv_async_init(g_scheduler.uv_loop, async_mem, &scheduler_async_noop_cb) == 0) {
                        g_scheduler.async_handle = async_mem;
                    } else {
                        mem::free(async_mem);
                    }
                }
            }
        } else if (loop_mem != null) {
            mem::free(loop_mem);
        }
    }

    g_scheduler_initialized = true;
}
