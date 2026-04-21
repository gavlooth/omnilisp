module lisp;

import std::core::mem;
import std::io;

// Scheduler core: state, pooled-work admission, initialization.
// ============================================================
// Global scheduler: fiber and uv-loop state is single-runtime-thread state.
// Worker/offload completion crosses threads only through the scheduler's
// mutex/condition-variable and wakeup queues; direct fiber-table mutation is
// confined to the scheduler owner thread.
Scheduler g_scheduler;
SchedulerDomain g_scheduler_primary_domain;
bool g_scheduler_initialized = false;

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
    if (catch err = g_scheduler.thread_task_cv.init()) {
        g_scheduler.thread_task_cv_ready = false;
    } else {
        g_scheduler.thread_task_cv_ready = true;
    }
    if (catch err = g_scheduler.reliable_wakeups.mu.init()) {
        g_scheduler.reliable_wakeups.initialized = false;
    } else {
        g_scheduler.reliable_wakeups.initialized = true;
    }
    scheduler_reset_wakeup_queue();
    scheduler_init_offload_admission();

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
