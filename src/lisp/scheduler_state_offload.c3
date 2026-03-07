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
const usz SCHEDULER_PRIMARY_DOMAIN_ID = 0;

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
    usz domain_id;
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
    bool readable_pending;
    bool writable_pending;
    bool write_mode;
    int fd;
    usz max_bytes;
    char* buffer;
    char* write_buffer;
    usz write_len;
    usz write_offset;
    long nread;
    long nwritten;
    int error_code;
    void* poll_handle;
    void* timer_handle;
}

struct PendingOffload {
    bool active;
    bool completed;
    void* completion; // OffloadCompletion*
}

struct PendingSleep {
    bool active;
    bool completed;
    void* timer_handle;
}

struct PendingDnsResolve {
    bool active;
    bool completed;
    int status;
    void* req_handle;
    void* addrinfo_result;
    char* host_copy;
}

enum SharedHandleKind : char {
    SHARED_HANDLE_BLOB
}

const usz SHARED_REGISTRY_CAPACITY = 1024;
const usz SHARED_RETIRED_QUEUE_CAPACITY = SHARED_REGISTRY_CAPACITY;

struct SharedHandleBlob {
    types::Atomic{long} refcount;
    char* bytes;
    usz len;
}

struct SharedHandle {
    SharedHandleKind kind;
    usz slot;
    uint generation;
    SharedHandleBlob* payload;
}

struct SharedRetiredPayload {
    usz slot;
    uint generation;
    SharedHandleBlob* payload;
}

struct SharedRegistryEntry {
    bool active;
    SharedHandleKind kind;
    uint generation;
    SharedHandleBlob* payload;
}

struct SharedRegistry {
    SharedRegistryEntry[SHARED_REGISTRY_CAPACITY] entries;
    SharedRetiredPayload[SHARED_RETIRED_QUEUE_CAPACITY] retired;
    usz next_slot;
    usz retire_head;
    usz retire_tail;
    usz retire_count;
    usz retire_overflow_count;
    thread::Mutex mu;
    bool initialized;
}

enum WakeupEventType : char {
    WAKEUP_POLL_READABLE,
    WAKEUP_POLL_ERROR,
    WAKEUP_TIMER_EXPIRED,
    WAKEUP_OFFLOAD_READY,
    WAKEUP_SLEEP_DONE,
    WAKEUP_DNS_DONE
}

struct WakeupEvent {
    usz fiber_id;
    WakeupEventType type;
    int status;
    void* payload;
}

struct ReliableWakeupNode {
    WakeupEvent event;
    ReliableWakeupNode* next;
}

struct ReliableWakeupQueue {
    ReliableWakeupNode* head;
    ReliableWakeupNode* tail;
    usz count;
    thread::Mutex mu;
    bool initialized;
}

enum OffloadOp : char {
    OFFLOAD_SLEEP_MS,
    OFFLOAD_GZIP,
    OFFLOAD_DEFLATE,
    OFFLOAD_ACCEPT_FD,
    OFFLOAD_TCP_CONNECT,
    OFFLOAD_HTTP_GET
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
    SharedHandle* shared;
}

struct OffloadCompletion {
    OffloadResultKind kind;
    long int_result;
    SharedHandle* shared;
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
    thread::Thread worker2;  // second thread so http-get and accept-fd can run concurrently
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
    PendingSleep[MAX_FIBERS] pending_sleeps;
    PendingDnsResolve[MAX_FIBERS] pending_dns;
    ReliableWakeupQueue reliable_wakeups;
    usz fiber_count;
    usz current_fiber_id;
    bool running;
    bool uv_loop_ready;
    void* uv_loop;
    void* async_handle;                // uv_async_t*
    OffloadQueue offload_queue;
    SharedRegistry shared_registry;
    ThreadTaskEntry[MAX_THREAD_TASKS] thread_tasks;
    thread::Mutex thread_task_mu;
    bool thread_task_mu_ready;
    // Fairness: rotate resume start index so no fiber is permanently first
    usz resume_offset;
    // Observability: reliable wakeup enqueue failures should be exceptional
    // (for example allocation failure).
    types::Atomic{usz} wakeup_enqueue_failures;
    types::Atomic{usz} reliable_wakeup_enqueues;
    types::Atomic{usz} readable_signal_coalesces;
}

struct SchedulerDomain {
    usz domain_id;
    Scheduler* scheduler;
    bool active;
}

// Global scheduler (single-threaded)
Scheduler g_scheduler;
SchedulerDomain g_scheduler_primary_domain;
bool g_scheduler_initialized = false;

fn void scheduler_async_noop_cb(void* handle) {
    // Intentionally empty; uv_async_send only wakes the loop.
    (void)handle;
}

fn void scheduler_reset_wakeup_queue() {
    scheduler_clear_reliable_wakeup_queue();
    for (usz i = 0; i < MAX_FIBERS; i++) {
        g_scheduler.pending_reads[i].readable_pending = false;
        g_scheduler.pending_reads[i].writable_pending = false;
    }
}

fn bool scheduler_str_eq(char[] a, char[] b) @inline {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) if (a[i] != b[i]) return false;
    return true;
}

fn Value* scheduler_raise(Interp* interp, char[] code, char[] message, Value* data = null) @inline {
    return raise_error_with_payload_names(interp, "scheduler", code, message, data);
}

fn SchedulerDomain* scheduler_primary_domain() @inline {
    return &g_scheduler_primary_domain;
}

fn SchedulerDomain* scheduler_current_domain() @inline {
    return &g_scheduler_primary_domain;
}

fn usz scheduler_domain_id(SchedulerDomain* domain) @inline {
    if (domain == null) return SCHEDULER_PRIMARY_DOMAIN_ID;
    return domain.domain_id;
}

fn bool scheduler_domain_is_local(SchedulerDomain* domain) @inline {
    if (domain == null) return false;
    return domain.active && domain.scheduler == &g_scheduler;
}

fn bool scheduler_fiber_belongs_to_domain(usz fiber_id, SchedulerDomain* domain) @inline {
    if (fiber_id >= g_scheduler.fiber_count) return false;
    return g_scheduler.fibers[fiber_id].domain_id == scheduler_domain_id(domain);
}

fn void scheduler_shared_registry_free_payload(SharedHandleBlob* payload) @inline {
    if (payload == null) return;
    if (payload.bytes != null) mem::free(payload.bytes);
    mem::free(payload);
}

fn bool scheduler_shared_registry_enqueue_retired_payload_locked(
    SharedRegistry* registry,
    usz slot,
    uint generation,
    SharedHandleBlob* payload
) @inline {
    if (registry == null || payload == null) return false;
    if (registry.retire_count >= SHARED_RETIRED_QUEUE_CAPACITY) {
        registry.retire_overflow_count++;
        return false;
    }
    SharedRetiredPayload* retired = &registry.retired[registry.retire_tail];
    retired.slot = slot;
    retired.generation = generation;
    retired.payload = payload;
    registry.retire_tail = (registry.retire_tail + 1) % SHARED_RETIRED_QUEUE_CAPACITY;
    registry.retire_count++;
    return true;
}

fn void scheduler_shared_registry_drain_retired(usz budget = 64) {
    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (!registry.initialized) return;

    SharedHandleBlob*[SHARED_RETIRED_QUEUE_CAPACITY] to_free;
    usz to_free_count = 0;
    usz drain_limit = budget;
    if (drain_limit == 0) return;
    if (drain_limit > SHARED_RETIRED_QUEUE_CAPACITY) {
        drain_limit = SHARED_RETIRED_QUEUE_CAPACITY;
    }

    registry.mu.lock();
    while (registry.retire_count > 0 && to_free_count < drain_limit) {
        SharedRetiredPayload* retired = &registry.retired[registry.retire_head];
        if (retired.payload != null) {
            to_free[to_free_count++] = retired.payload;
        }
        retired.slot = SHARED_REGISTRY_CAPACITY;
        retired.generation = 0;
        retired.payload = null;
        registry.retire_head = (registry.retire_head + 1) % SHARED_RETIRED_QUEUE_CAPACITY;
        registry.retire_count--;
    }
    registry.mu.unlock();

    for (usz i = 0; i < to_free_count; i++) {
        scheduler_shared_registry_free_payload(to_free[i]);
    }
}

fn void scheduler_domain_safe_point(SchedulerDomain* domain) @inline {
    if (!scheduler_domain_is_local(domain)) return;
    scheduler_shared_registry_drain_retired(64);
}

fn uint scheduler_shared_registry_next_generation(uint current) @inline {
    if (current == uint.max) return 1;
    return current + 1;
}

fn bool scheduler_shared_registry_ensure_ready() {
    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (registry.initialized) return true;
    if (catch err = registry.mu.init()) return false;
    registry.initialized = true;
    registry.next_slot = 0;
    registry.retire_head = 0;
    registry.retire_tail = 0;
    registry.retire_count = 0;
    registry.retire_overflow_count = 0;
    return true;
}

fn bool scheduler_shared_registry_alloc(
    SharedHandleKind kind,
    SharedHandleBlob* payload,
    usz* out_slot,
    uint* out_generation
) {
    if (!scheduler_shared_registry_ensure_ready()) return false;
    SharedRegistry* registry = &g_scheduler.shared_registry;
    registry.mu.lock();
    defer registry.mu.unlock();

    for (usz i = 0; i < SHARED_REGISTRY_CAPACITY; i++) {
        usz slot = (registry.next_slot + i) % SHARED_REGISTRY_CAPACITY;
        SharedRegistryEntry* entry = &registry.entries[slot];
        if (entry.active) continue;

        uint generation = entry.generation;
        if (generation == 0) generation = 1;

        entry.active = true;
        entry.kind = kind;
        entry.generation = generation;
        entry.payload = payload;
        registry.next_slot = (slot + 1) % SHARED_REGISTRY_CAPACITY;

        if (out_slot != null) *out_slot = slot;
        if (out_generation != null) *out_generation = generation;
        return true;
    }
    return false;
}

fn void scheduler_shared_registry_retire_entry(SharedRegistryEntry* entry) @inline {
    entry.active = false;
    entry.payload = null;
    entry.generation = scheduler_shared_registry_next_generation(entry.generation);
}

fn bool scheduler_shared_registry_destroy_slot(usz slot, uint generation, SharedHandleBlob* expected_payload = null) {
    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (!registry.initialized || slot >= SHARED_REGISTRY_CAPACITY) return false;

    registry.mu.lock();
    defer registry.mu.unlock();

    SharedRegistryEntry* entry = &registry.entries[slot];
    if (!entry.active || entry.generation != generation) return false;
    if (expected_payload != null && entry.payload != expected_payload) return false;

    SharedHandleBlob* payload = entry.payload;
    scheduler_shared_registry_retire_entry(entry);
    if (payload != null &&
        !scheduler_shared_registry_enqueue_retired_payload_locked(registry, slot, generation, payload)) {
        scheduler_shared_registry_free_payload(payload);
    }
    return true;
}

fn SharedHandleBlob* scheduler_shared_registry_resolve_blob(SharedHandle* handle) {
    if (handle == null || handle.kind != SHARED_HANDLE_BLOB) return null;
    SharedRegistry* registry = &g_scheduler.shared_registry;
    if (!registry.initialized || handle.slot >= SHARED_REGISTRY_CAPACITY) return null;

    registry.mu.lock();
    defer registry.mu.unlock();

    SharedRegistryEntry* entry = &registry.entries[handle.slot];
    if (!entry.active || entry.generation != handle.generation || entry.kind != handle.kind) return null;
    if (handle.payload != null && entry.payload != handle.payload) return null;
    return entry.payload;
}

fn bool scheduler_shared_destroy(SharedHandle* handle) {
    if (handle == null) return false;

    SharedRegistry* registry = &g_scheduler.shared_registry;
    bool released = false;
    if (registry.initialized && handle.slot < SHARED_REGISTRY_CAPACITY) {
        registry.mu.lock();
        SharedRegistryEntry* entry = &registry.entries[handle.slot];
        if (entry.active &&
            entry.generation == handle.generation &&
            entry.kind == handle.kind &&
            entry.payload != null &&
            (handle.payload == null || entry.payload == handle.payload)) {
            SharedHandleBlob* payload = entry.payload;
            long old = payload.refcount.add(-1);
            if (old <= 1) {
                scheduler_shared_registry_retire_entry(entry);
                if (!scheduler_shared_registry_enqueue_retired_payload_locked(
                    registry,
                    handle.slot,
                    handle.generation,
                    payload
                )) {
                    scheduler_shared_registry_free_payload(payload);
                }
            }
            released = true;
        }
        registry.mu.unlock();
    }

    // Fallback safety in case a handle exists before registry availability.
    if (!released && handle.payload != null) {
        long old = handle.payload.refcount.add(-1);
        if (old <= 1) {
            if (handle.payload.bytes != null) mem::free(handle.payload.bytes);
            mem::free(handle.payload);
        }
    }

    mem::free(handle);
    return released;
}

fn bool scheduler_shared_retire(SharedHandle* handle) {
    if (handle == null) return false;
    bool retired = scheduler_shared_registry_destroy_slot(handle.slot, handle.generation, handle.payload);
    mem::free(handle);
    return retired;
}

fn SharedHandleBlob* shared_handle_as_blob(SharedHandle* handle) @inline {
    return scheduler_shared_registry_resolve_blob(handle);
}

fn SharedHandle* shared_handle_blob_new_owned(char* owned_bytes, usz len) {
    if (owned_bytes == null) return null;
    if (!scheduler_shared_registry_ensure_ready()) {
        mem::free(owned_bytes);
        return null;
    }

    SharedHandleBlob* payload = (SharedHandleBlob*)mem::malloc(SharedHandleBlob.sizeof);
    if (payload == null) {
        mem::free(owned_bytes);
        return null;
    }
    payload.refcount.store(1);
    payload.bytes = owned_bytes;
    payload.len = len;

    usz slot = SHARED_REGISTRY_CAPACITY;
    uint generation = 0;
    if (!scheduler_shared_registry_alloc(SHARED_HANDLE_BLOB, payload, &slot, &generation)) {
        mem::free(payload.bytes);
        mem::free(payload);
        return null;
    }

    SharedHandle* handle = (SharedHandle*)mem::malloc(SharedHandle.sizeof);
    if (handle == null) {
        (void)scheduler_shared_registry_destroy_slot(slot, generation, payload);
        return null;
    }
    handle.kind = SHARED_HANDLE_BLOB;
    handle.slot = slot;
    handle.generation = generation;
    handle.payload = payload;
    return handle;
}

fn SharedHandle* shared_handle_blob_new_copy(char[] bytes) {
    usz cap = bytes.len + 1;
    if (cap < 32) cap = 32;
    char* copy = (char*)mem::malloc(cap);
    if (copy == null) return null;
    for (usz i = 0; i < bytes.len; i++) copy[i] = bytes[i];
    copy[bytes.len] = 0;
    return shared_handle_blob_new_owned(copy, bytes.len);
}

fn char[] shared_handle_blob_bytes(SharedHandle* handle, usz* out_len) @inline {
    SharedHandleBlob* payload = shared_handle_as_blob(handle);
    if (payload == null || payload.bytes == null) {
        if (out_len != null) *out_len = 0;
        return "";
    }
    if (out_len != null) *out_len = payload.len;
    return payload.bytes[:payload.len];
}

fn SharedHandle* scheduler_publish_shared_bytes_owned(char* owned_bytes, usz len) @inline {
    return shared_handle_blob_new_owned(owned_bytes, len);
}

fn SharedHandle* scheduler_publish_shared_bytes_copy(char[] bytes) @inline {
    return shared_handle_blob_new_copy(bytes);
}

fn SharedHandle* scheduler_publish_shared(char[] bytes) @inline {
    return scheduler_publish_shared_bytes_copy(bytes);
}

fn SharedHandle* scheduler_domain_publish_shared(SchedulerDomain* domain, char[] bytes) @inline {
    if (!scheduler_domain_is_local(domain)) return null;
    scheduler_domain_safe_point(domain);
    return scheduler_publish_shared(bytes);
}

fn SharedHandle* scheduler_domain_publish_shared_owned(SchedulerDomain* domain, char* owned_bytes, usz len) @inline {
    if (!scheduler_domain_is_local(domain)) {
        if (owned_bytes != null) mem::free(owned_bytes);
        return null;
    }
    scheduler_domain_safe_point(domain);
    return scheduler_publish_shared_bytes_owned(owned_bytes, len);
}

fn bool scheduler_project_shared_bytes_view(SharedHandle* handle, char[]* out_bytes) @inline {
    if (out_bytes != null) *out_bytes = "";
    SharedHandleBlob* payload = shared_handle_as_blob(handle);
    if (payload == null || payload.bytes == null) return false;
    if (out_bytes != null) *out_bytes = payload.bytes[:payload.len];
    return true;
}

fn bool scheduler_project_shared(SharedHandle* handle, char[]* out_bytes) @inline {
    return scheduler_project_shared_bytes_view(handle, out_bytes);
}

fn bool scheduler_take_shared_bytes_owned(SharedHandle* handle, char** out_bytes, usz* out_len) @inline {
    if (out_bytes != null) *out_bytes = null;
    if (out_len != null) *out_len = 0;
    SharedHandleBlob* payload = shared_handle_as_blob(handle);
    if (payload == null || payload.bytes == null) return false;
    if (out_bytes != null) *out_bytes = payload.bytes;
    if (out_len != null) *out_len = payload.len;
    shared_handle_clear_blob(handle);
    return true;
}

fn void shared_handle_clear_blob(SharedHandle* handle) @inline {
    SharedHandleBlob* payload = shared_handle_as_blob(handle);
    if (payload == null) return;
    payload.bytes = null;
    payload.len = 0;
}

fn void shared_handle_release(SharedHandle* handle) @inline {
    (void)scheduler_shared_destroy(handle);
}

fn void scheduler_release_shared(SharedHandle* handle) @inline {
    (void)scheduler_shared_destroy(handle);
}

fn long scheduler_publish_sendable_int(long value) @inline {
    return value;
}

fn long scheduler_domain_publish_sendable_int(SchedulerDomain* domain, long value) @inline {
    if (!scheduler_domain_is_local(domain)) return value;
    scheduler_domain_safe_point(domain);
    return scheduler_publish_sendable_int(value);
}

fn Value* scheduler_consume_sendable_int(long value, Interp* interp) @inline {
    return make_int(interp, value);
}

fn Value* scheduler_domain_consume_sendable_int(SchedulerDomain* domain, long value, Interp* interp) @inline {
    if (!scheduler_domain_is_local(domain)) {
        return scheduler_raise(interp, "scheduler/domain-invalid-sendable-consume", "scheduler: invalid domain sendable consume");
    }
    scheduler_domain_safe_point(domain);
    return scheduler_consume_sendable_int(value, interp);
}

fn Value* scheduler_project_shared_to_local_value(SharedHandle** handle_ref, Interp* interp) {
    if (handle_ref == null || *handle_ref == null) {
        return make_string(interp, "");
    }

    SharedHandle* handle = *handle_ref;
    switch (handle.kind) {
        case SHARED_HANDLE_BLOB:
            char* owned = null;
            usz len = 0;
            if (!scheduler_take_shared_bytes_owned(handle, &owned, &len)) {
                scheduler_release_shared(handle);
                *handle_ref = null;
                return make_string(interp, "");
            }

            scheduler_release_shared(handle);
            *handle_ref = null;
            return make_string_owned(interp, owned, len);
    }
}

fn Value* scheduler_domain_project_shared_to_local_value(SchedulerDomain* domain, SharedHandle** handle_ref, Interp* interp) {
    if (!scheduler_domain_is_local(domain)) {
        return scheduler_raise(interp, "scheduler/domain-invalid-shared-project", "scheduler: invalid domain shared projection");
    }
    scheduler_domain_safe_point(domain);
    return scheduler_project_shared_to_local_value(handle_ref, interp);
}

fn void scheduler_free_offload_completion(void* raw_completion) {
    if (raw_completion == null) return;
    OffloadCompletion* completion = (OffloadCompletion*)raw_completion;
    if (completion.shared != null) {
        scheduler_release_shared(completion.shared);
        completion.shared = null;
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

fn bool scheduler_set_offload_bytes_owned(
    OffloadCompletion* completion,
    char* owned_bytes,
    usz len,
    char[] alloc_error_message
) {
    SharedHandle* result = scheduler_domain_publish_shared_owned(scheduler_primary_domain(), owned_bytes, len);
    if (result == null) {
        scheduler_set_offload_error(completion, alloc_error_message);
        return false;
    }
    completion.kind = OFFLOAD_RES_BYTES;
    completion.shared = result;
    return true;
}

fn Value* scheduler_reject_non_cpu_offload_op(char[] op_name, Interp* interp) {
    char[160] ebuf;
    char[] msg = io::bprintf(
        &ebuf,
        "offload: op '%s' is internal-only; public offload/thread-spawn are CPU-bound",
        (ZString)op_name
    )!!;
    return scheduler_raise(interp, "scheduler/offload-op-cpu-only", msg);
}

fn Value* scheduler_build_offload_work(Value* job, OffloadWork* work, Interp* interp, bool allow_internal_io_ops) {
    if (!is_cons(job)) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: expected (op args...)");
    }
    Value* op_val = job.cons_val.car;
    if (!is_symbol(op_val)) {
        return scheduler_raise(interp, "scheduler/offload-op-not-symbol", "offload: op must be a symbol");
    }
    char[] op_name = interp.symbols.get_name(op_val.sym_val);
    Value* arg_list = job.cons_val.cdr;

    work.fiber_id = NO_FIBER;
    work.task_id = NO_TASK;
    work.op = OFFLOAD_SLEEP_MS;
    work.int_arg = 0;
    work.shared = null;

    if (scheduler_str_eq(op_name, "sleep-ms")) {
        if (!is_cons(arg_list) || !is_int(arg_list.cons_val.car)) {
            return scheduler_raise(
                interp,
                "scheduler/offload-invalid-sleep-ms",
                "offload sleep-ms: expected integer milliseconds"
            );
        }
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = arg_list.cons_val.car.int_val;
        return null;
    }

    if (scheduler_str_eq(op_name, "gzip") || scheduler_str_eq(op_name, "deflate")) {
        if (!is_cons(arg_list) || !is_string(arg_list.cons_val.car)) {
            return scheduler_raise(
                interp,
                "scheduler/offload-invalid-compress-payload",
                "offload compress: expected string payload"
            );
        }
        Value* src = arg_list.cons_val.car;
        work.op = scheduler_str_eq(op_name, "gzip") ? OFFLOAD_GZIP : OFFLOAD_DEFLATE;
        work.shared = scheduler_domain_publish_shared(scheduler_current_domain(), src.str_chars[:src.str_len]);
        if (work.shared == null) return scheduler_raise(interp, "scheduler/offload-out-of-memory", "offload: out of memory");
        return null;
    }

    if (scheduler_str_eq(op_name, "accept-fd")) {
        if (!allow_internal_io_ops) {
            return scheduler_reject_non_cpu_offload_op(op_name, interp);
        }
        if (!is_cons(arg_list) || !is_int(arg_list.cons_val.car)) {
            return scheduler_raise(
                interp,
                "scheduler/offload-invalid-accept-fd",
                "offload accept-fd: expected integer fd"
            );
        }
        work.op = OFFLOAD_ACCEPT_FD;
        work.int_arg = arg_list.cons_val.car.int_val;
        return null;
    }

    if (scheduler_str_eq(op_name, "tcp-connect")) {
        if (!allow_internal_io_ops) {
            return scheduler_reject_non_cpu_offload_op(op_name, interp);
        }
        // (tcp-connect host port) — host is string, port is int
        if (!is_cons(arg_list) || !is_string(arg_list.cons_val.car)) {
            return scheduler_raise(
                interp,
                "scheduler/offload-invalid-tcp-connect-host",
                "offload tcp-connect: expected string host"
            );
        }
        Value* host_val = arg_list.cons_val.car;
        Value* port_list = arg_list.cons_val.cdr;
        if (!is_cons(port_list) || !is_int(port_list.cons_val.car)) {
            return scheduler_raise(
                interp,
                "scheduler/offload-invalid-tcp-connect-port",
                "offload tcp-connect: expected integer port"
            );
        }
        // Null-terminate the host string for getaddrinfo
        usz host_len = host_val.str_len;
        char* host_copy = (char*)mem::malloc(host_len + 1);
        if (host_copy == null) return scheduler_raise(interp, "scheduler/offload-out-of-memory", "offload: out of memory");
        for (usz i = 0; i < host_len; i++) host_copy[i] = host_val.str_chars[i];
        host_copy[host_len] = 0;
        work.op = OFFLOAD_TCP_CONNECT;
        work.int_arg = port_list.cons_val.car.int_val;
        work.shared = scheduler_domain_publish_shared_owned(scheduler_current_domain(), host_copy, host_len);
        if (work.shared == null) return scheduler_raise(interp, "scheduler/offload-out-of-memory", "offload: out of memory");
        return null;
    }

    if (scheduler_str_eq(op_name, "http-get")) {
        if (!allow_internal_io_ops) {
            return scheduler_reject_non_cpu_offload_op(op_name, interp);
        }
        // (http-get url) — url is a string like "https://host/path"
        if (!is_cons(arg_list) || !is_string(arg_list.cons_val.car)) {
            return scheduler_raise(
                interp,
                "scheduler/offload-invalid-http-get",
                "offload http-get: expected string url"
            );
        }
        Value* url_val = arg_list.cons_val.car;
        work.op = OFFLOAD_HTTP_GET;
        work.shared = scheduler_domain_publish_shared(scheduler_current_domain(), url_val.str_chars[:url_val.str_len]);
        if (work.shared == null) return scheduler_raise(interp, "scheduler/offload-out-of-memory", "offload: out of memory");
        return null;
    }

    return scheduler_raise(interp, "scheduler/offload-unsupported-op", "offload: unsupported op");
}

fn void scheduler_init() {
    if (g_scheduler_initialized) return;

    g_scheduler = {};
    g_scheduler_primary_domain = {};
    g_scheduler_primary_domain.domain_id = SCHEDULER_PRIMARY_DOMAIN_ID;
    g_scheduler_primary_domain.scheduler = &g_scheduler;
    g_scheduler_primary_domain.active = true;
    g_scheduler.fiber_count = 0;
    g_scheduler.current_fiber_id = NO_FIBER;
    g_scheduler.running = false;
    (void)scheduler_shared_registry_ensure_ready();
    if (catch err = g_scheduler.thread_task_mu.init()) {
        g_scheduler.thread_task_mu_ready = false;
    } else {
        g_scheduler.thread_task_mu_ready = true;
    }
    if (catch err = g_scheduler.reliable_wakeups.mu.init()) {
        g_scheduler.reliable_wakeups.initialized = false;
    } else {
        g_scheduler.reliable_wakeups.initialized = true;
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
