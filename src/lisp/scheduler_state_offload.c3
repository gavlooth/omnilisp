module lisp;

import std::core::mem;
import std::io;

// Scheduler core: state, pooled-work admission, initialization.
// ============================================================
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
        g_scheduler.fiber_join_wait_token[i] = 0;
        g_scheduler.latched_task_done_wake_epoch[i] = 0;
        g_scheduler.latched_os_thread_done_wake_epoch[i] = 0;
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
        "offload: op '%s' is internal-only; public offload/task-spawn are CPU-bound",
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
    work.wake_epoch = 0;
    work.op = OFFLOAD_SLEEP_MS;
    work.int_arg = 0;
    work.int_arg2 = 0;
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
