module lisp;

import std::atomic::types;
import std::thread;

struct Scheduler {
    FiberEntry[MAX_FIBERS] fibers;
    PendingTcpRead[MAX_FIBERS] pending_reads;
    PendingOffload[MAX_FIBERS] pending_offloads;
    PendingSleep[MAX_FIBERS] pending_sleeps;
    PendingDnsResolve[MAX_FIBERS] pending_dns;
    PendingTcpConnect[MAX_FIBERS] pending_connects;
    PendingTcpAccept[MAX_FIBERS] pending_accepts;
    // Per-fiber token for currently armed task/thread join wait registration.
    uint[MAX_FIBERS] fiber_join_wait_token;
    // Allocation-free fallback wake latches used when reliable wakeup enqueue fails.
    uint[MAX_FIBERS] latched_task_done_wake_epoch;
    uint[MAX_FIBERS] latched_os_thread_done_wake_epoch;
    ReliableWakeupQueue reliable_wakeups;
    usz fiber_count;
    uint fiber_wake_epoch_seed;
    uint async_request_token_seed;
    uint join_wait_token_seed;
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
