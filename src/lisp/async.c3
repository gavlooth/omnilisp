module lisp;

import std::core::mem;

<*
@require interp != null
*>
fn Value* prim_async_sleep(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return io_raise(interp, "io/async-sleep-expected-ms", "async-sleep: expected (async-sleep ms)");
    // fault: lisp::EXPECTED_INT
    if (!is_int(args[0])) return io_raise(interp, "io/async-sleep-ms-not-integer", "async-sleep: ms must be an integer");

    if (!scheduler_is_running_fiber_context()) {
        return io_raise(
            interp,
            "io/async-sleep-fiber-required",
            "async-sleep: requires running fiber context"
        );
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return io_raise(
            interp,
            "io/async-sleep-loop-unavailable",
            "async-sleep: libuv loop unavailable"
        );
    }

    long ms = args[0].int_val;
    if (ms <= 0) return make_void(interp);

    usz fiber_id = g_scheduler.current_fiber_id;
    PendingSleep* sleep = &g_scheduler.pending_sleeps[fiber_id];
    if (sleep.active) {
        return io_raise(
            interp,
            "io/async-sleep-pending-timer-exists",
            "async-sleep: pending timer already exists for this fiber"
        );
    }

    usz timer_size = uv_handle_size(UV_HANDLE_TIMER);
    if (timer_size == 0) {
        return io_raise(
            interp,
            "io/async-sleep-loop-unavailable",
            "async-sleep: libuv loop unavailable"
        );
    }

    void* timer_handle = mem::malloc(timer_size);
    if (timer_handle == null) {
        return io_raise(interp, "io/async-sleep-out-of-memory", "async-sleep: out of memory");
    }
    if (uv_timer_init(g_scheduler.uv_loop, timer_handle) != 0) {
        mem::free(timer_handle);
        return io_raise(
            interp,
            "io/async-sleep-loop-unavailable",
            "async-sleep: libuv loop unavailable"
        );
    }

    sleep.active = true;
    sleep.completed = false;
    sleep.timer_handle = timer_handle;

    if (uv_timer_start(timer_handle, &scheduler_uv_sleep_timer_cb, (ulong)ms, 0) != 0) {
        uv_close(timer_handle, &scheduler_free_handle_cb);
        g_scheduler.pending_sleeps[fiber_id] = {};
        return io_raise(
            interp,
            "io/async-sleep-loop-unavailable",
            "async-sleep: libuv loop unavailable"
        );
    }

    g_scheduler.fibers[fiber_id].state = FIBER_BLOCKED;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) {
        scheduler_close_pending_sleep(fiber_id);
        return y;
    }
    return scheduler_consume_pending_sleep(fiber_id, interp);
}
