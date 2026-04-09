module lisp;

import std::core::mem;

struct OsThreadStartCtx {
    usz thread_id;
    uint generation;
    OffloadWork work;
}

fn uint scheduler_next_os_thread_generation_locked() @inline {
    g_scheduler.os_thread_generation_seed++;
    if (g_scheduler.os_thread_generation_seed == 0) g_scheduler.os_thread_generation_seed = 1;
    return g_scheduler.os_thread_generation_seed;
}

fn bool scheduler_os_thread_generation_matches(OsThreadEntry* entry, uint generation) @inline {
    if (generation == 0) return true;
    return entry.generation == generation;
}


fn usz scheduler_alloc_os_thread() {
    if (!g_scheduler.thread_task_mu_ready) return NO_OS_THREAD;
    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();

    for (usz i = 0; i < MAX_OS_THREADS; i++) {
        if (g_scheduler.os_threads[i].active) continue;
        g_scheduler.os_threads[i] = {};
        g_scheduler.os_threads[i].active = true;
        g_scheduler.os_threads[i].state = OS_THREAD_PENDING;
        g_scheduler.os_threads[i].cancel_requested = false;
        g_scheduler.os_threads[i].thread_created = false;
        g_scheduler.os_threads[i].thread_handle = null;
        g_scheduler.os_threads[i].generation = scheduler_next_os_thread_generation_locked();
        g_scheduler.os_threads[i].completion = null;
        g_scheduler.os_threads[i].waiter_fiber_id = NO_FIBER;
        g_scheduler.os_threads[i].waiter_wake_epoch = 0;
        return i;
    }
    return NO_OS_THREAD;
}

fn bool scheduler_get_os_thread_generation(usz thread_id, uint* out_generation) {
    if (out_generation != null) *out_generation = 0;
    if (thread_id >= MAX_OS_THREADS || !g_scheduler.thread_task_mu_ready) return false;

    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();
    OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
    if (!entry.active) return false;
    if (out_generation != null) *out_generation = entry.generation;
    return true;
}

fn void scheduler_os_thread_entry(void* ctx_ptr) {
    @pool_init(mem, 65536)
    {
        OsThreadStartCtx* ctx = (OsThreadStartCtx*)ctx_ptr;
        if (ctx == null) return;

        OffloadWork work = ctx.work;
        usz thread_id = ctx.thread_id;
        uint generation = ctx.generation;
        mem::free(ctx);

        if (!scheduler_try_begin_os_thread(thread_id, generation)) {
            scheduler_release_shared(work.shared);
            return;
        }

        OffloadCompletion* completion = scheduler_execute_offload(work);
        scheduler_complete_os_thread(thread_id, generation, completion);
        return;
    };
}

fn bool scheduler_start_os_thread(usz thread_id, uint generation, OffloadWork work) {
    if (thread_id >= MAX_OS_THREADS || generation == 0) return false;

    OsThreadStartCtx* ctx = (OsThreadStartCtx*)mem::malloc(OsThreadStartCtx.sizeof);
    if (ctx == null) return false;
    *ctx = {};
    ctx.thread_id = thread_id;
    ctx.generation = generation;
    ctx.work = work;

    void* thread_handle = omni_uv_thread_new();
    if (thread_handle == null) {
        mem::free(ctx);
        return false;
    }

    g_scheduler.thread_task_mu.lock();
    OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
    if (!entry.active || !scheduler_os_thread_generation_matches(entry, generation)) {
        g_scheduler.thread_task_mu.unlock();
        mem::free(ctx);
        omni_uv_thread_free(thread_handle);
        return false;
    }
    if (omni_uv_thread_start(thread_handle, &scheduler_os_thread_entry, ctx) != 0) {
        g_scheduler.thread_task_mu.unlock();
        mem::free(ctx);
        omni_uv_thread_free(thread_handle);
        return false;
    }

    int detach_status = omni_uv_thread_detach_handle(thread_handle);
    entry.thread_created = true;
    if (detach_status == 0) {
        // Detach eagerly so completed OS threads never accumulate joinable
        // kernel-thread resources when callers timeout and skip explicit join.
        entry.thread_handle = null;
        g_scheduler.thread_task_mu.unlock();
        omni_uv_thread_free(thread_handle);
        return true;
    }

    // If detach is unavailable, keep the joinable handle and clean it up via
    // join/detach paths at completion/drop.
    entry.thread_handle = thread_handle;
    g_scheduler.thread_task_mu.unlock();
    return true;
}

fn OffloadCompletion* scheduler_take_os_thread_completion(usz thread_id, uint generation = 0) {
    if (thread_id >= MAX_OS_THREADS || !g_scheduler.thread_task_mu_ready) return null;

    OffloadCompletion* completion = null;
    void* thread_handle = null;
    bool should_join = false;

    g_scheduler.thread_task_mu.lock();
    OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
    if (!entry.active || !scheduler_os_thread_generation_matches(entry, generation)) {
        g_scheduler.thread_task_mu.unlock();
        return null;
    }
    if (entry.state != OS_THREAD_DONE && entry.state != OS_THREAD_CANCELLED) {
        g_scheduler.thread_task_mu.unlock();
        return null;
    }

    completion = entry.completion;
    usz waiter_fiber_id = entry.waiter_fiber_id;
    uint waiter_wake_epoch = entry.waiter_wake_epoch;
    entry.waiter_fiber_id = NO_FIBER;
    entry.waiter_wake_epoch = 0;
    (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
    if (entry.thread_created && entry.thread_handle != null) {
        thread_handle = entry.thread_handle;
        should_join = true;
    }
    g_scheduler.os_threads[thread_id] = {};
    scheduler_signal_thread_task_waiters_locked();
    g_scheduler.thread_task_mu.unlock();

    if (should_join) {
        (void)omni_uv_thread_join_handle(thread_handle);
    }
    if (thread_handle != null) {
        omni_uv_thread_free(thread_handle);
    }
    return completion;
}

fn void scheduler_drop_os_thread(usz thread_id, uint generation = 0) {
    if (thread_id >= MAX_OS_THREADS || !g_scheduler.thread_task_mu_ready) return;

    OffloadCompletion* completion = null;
    void* thread_handle = null;
    bool should_join = false;
    bool should_detach = false;
    usz waiter_fiber_id = NO_FIBER;
    uint waiter_wake_epoch = 0;

    g_scheduler.thread_task_mu.lock();
    if (g_scheduler.os_threads[thread_id].active) {
        OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
        if (scheduler_os_thread_generation_matches(entry, generation)) {
            completion = entry.completion;
            waiter_fiber_id = entry.waiter_fiber_id;
            waiter_wake_epoch = entry.waiter_wake_epoch;
            if (entry.thread_created &&
                entry.state != OS_THREAD_DONE &&
                entry.state != OS_THREAD_CANCELLED) {
                entry.cancel_requested = true;
            }
            if (entry.thread_created && entry.thread_handle != null) {
                thread_handle = entry.thread_handle;
                if (entry.state == OS_THREAD_DONE || entry.state == OS_THREAD_CANCELLED) {
                    should_join = true;
                } else {
                    should_detach = true;
                }
            }
            g_scheduler.os_threads[thread_id] = {};
        }
    }
    g_scheduler.thread_task_mu.unlock();

    if (completion != null) scheduler_free_offload_completion(completion);
    if (should_join) {
        (void)omni_uv_thread_join_handle(thread_handle);
    } else if (should_detach) {
        (void)omni_uv_thread_detach_handle(thread_handle);
    }
    if (thread_handle != null) {
        omni_uv_thread_free(thread_handle);
    }
    scheduler_signal_thread_task_waiters();
    (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
}

fn bool scheduler_os_thread_exists(usz thread_id, uint generation = 0) {
    if (thread_id >= MAX_OS_THREADS || !g_scheduler.thread_task_mu_ready) return false;
    g_scheduler.thread_task_mu.lock();
    bool exists = g_scheduler.os_threads[thread_id].active &&
                  scheduler_os_thread_generation_matches(&g_scheduler.os_threads[thread_id], generation);
    g_scheduler.thread_task_mu.unlock();
    return exists;
}
