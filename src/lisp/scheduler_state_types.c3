module lisp;

import std::atomic::types;
import std::thread;

const usz MAX_FIBERS = 256;
const usz NO_FIBER = MAX_FIBERS;
const usz MAX_THREAD_TASKS = 256;
const usz NO_TASK = MAX_THREAD_TASKS;
const usz MAX_OS_THREADS = 128;
const usz NO_OS_THREAD = MAX_OS_THREADS;
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

struct PendingTcpConnect {
    bool active;
    bool completed;
    int status;
    void* req_handle; // uv_connect_t*
    void* tcp_handle; // uv_tcp_t*
}

struct PendingTcpAccept {
    bool active;
    bool completed;
    int status;
    bool borrowed_server_handle;
    bool has_accepted_fd;
    int accepted_fd;
    void* server_handle; // uv_stream_t* (opened on dup(listener_fd))
    void* client_handle; // uv_stream_t*
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
    WAKEUP_DNS_DONE,
    WAKEUP_TCP_CONNECT_DONE,
    WAKEUP_TCP_ACCEPT_DONE,
    WAKEUP_TASK_DONE,
    WAKEUP_OS_THREAD_DONE
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
    OFFLOAD_HTTP_GET,
    OFFLOAD_READ_FILE,
    OFFLOAD_WRITE_FILE,
    OFFLOAD_FILE_EXISTS,
    OFFLOAD_TLS_CONNECT,
    OFFLOAD_TLS_READ,
    OFFLOAD_TLS_WRITE,
    OFFLOAD_TLS_CLOSE
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
    long int_arg2;
    SharedHandle* shared;
}

struct OffloadCompletion {
    OffloadResultKind kind;
    long int_result;
    SharedHandle* shared;
    char* error_msg;
    usz error_len;
}

const usz OFFLOAD_ADMISSION_MAX_IN_FLIGHT = 256;

struct OffloadAdmission {
    thread::Mutex mu;
    usz in_flight;
    usz max_in_flight;
    void* queued_free_head;
    usz queued_free_count;
    bool initialized;
}

enum ThreadTaskState : char {
    THREAD_TASK_PENDING,
    THREAD_TASK_RUNNING,
    THREAD_TASK_DONE,
    THREAD_TASK_CANCELLED
}

struct TaskEntry {
    bool active;
    bool cancel_requested;
    uint generation;
    ThreadTaskState state;
    OffloadCompletion* completion;
    void* request_handle; // uv_work_t* while task is pending in libuv queue
    usz waiter_fiber_id;
}

struct SchedulerTaskHandle {
    usz task_id;
    uint generation;
}

enum OsThreadState : char {
    OS_THREAD_PENDING,
    OS_THREAD_RUNNING,
    OS_THREAD_DONE,
    OS_THREAD_CANCELLED
}

struct OsThreadEntry {
    bool active;
    bool thread_created;
    bool cancel_requested;
    uint generation;
    OsThreadState state;
    void* thread_handle; // uv_thread_t* allocated via omni_uv_thread_new
    OffloadCompletion* completion;
    usz waiter_fiber_id;
}

struct SchedulerOsThreadHandle {
    usz thread_id;
    uint generation;
}

struct Scheduler {
    FiberEntry[MAX_FIBERS] fibers;
    PendingTcpRead[MAX_FIBERS] pending_reads;
    PendingOffload[MAX_FIBERS] pending_offloads;
    PendingSleep[MAX_FIBERS] pending_sleeps;
    PendingDnsResolve[MAX_FIBERS] pending_dns;
    PendingTcpConnect[MAX_FIBERS] pending_connects;
    PendingTcpAccept[MAX_FIBERS] pending_accepts;
    ReliableWakeupQueue reliable_wakeups;
    usz fiber_count;
    usz current_fiber_id;
    bool running;
    bool uv_loop_ready;
    void* uv_loop;
    void* async_handle;                // uv_async_t*
    OffloadAdmission offload_admission;
    SharedRegistry shared_registry;
    TaskEntry[MAX_THREAD_TASKS] thread_tasks;
    uint task_generation_seed;
    OsThreadEntry[MAX_OS_THREADS] os_threads;
    uint os_thread_generation_seed;
    thread::Mutex thread_task_mu;
    thread::ConditionVariable thread_task_cv;
    bool thread_task_mu_ready;
    bool thread_task_cv_ready;
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
