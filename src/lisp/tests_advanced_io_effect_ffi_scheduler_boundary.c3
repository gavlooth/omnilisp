module lisp;

import std::io;

fn void run_advanced_process_wait_event_driven_test_impl(Interp* interp, int* pass, int* fail) {
    omni_uv_process_debug_reset_counters();

    EvalResult waited = run(
        "(block "
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 0.05\"] nil)) "
        "(define st (await (spawn (lambda () (process-wait (ref p 'handle)))))) "
        "(fs-close (ref p 'stdin)) "
        "(fs-close (ref p 'stdout)) "
        "(fs-close (ref p 'stderr)) "
        "(= (ref st 'exit-code) 0))",
        interp
    );

    int poll_calls = omni_uv_process_debug_poll_calls();
    int wait_calls = omni_uv_process_debug_wait_calls();
    bool ok = !waited.error.has_error &&
        waited.value != null &&
        waited.value.tag != ERROR &&
        waited.value.tag != NIL &&
        wait_calls >= 1 &&
        poll_calls == 0;

    if (ok) {
        emit_pass("process wait uses event-driven worker wakeup path");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] process wait uses event-driven worker wakeup path (wait_err=%s wait_calls=%d poll_calls=%d)",
            waited.error.has_error ? "yes" : "no",
            wait_calls,
            poll_calls
        );
        (*fail)++;
    }
}

fn void run_advanced_process_handle_concurrent_reuse_test_impl(Interp* interp, int* pass, int* fail) {
    EvalResult setup = run(
        "(define p (process-spawn \"/bin/sh\" [\"-c\" \"sleep 0.2\"] nil))",
        interp
    );
    bool ok = !setup.error.has_error;
    Value* proc_v = null;
    ProcessHandle* ph = null;
    if (ok) {
        proc_v = interp.global_env.lookup(interp.symbols.intern("p"));
        Value* handle_v = proc_v != null && proc_v.tag == HASHMAP && proc_v.hashmap_val != null
            ? hashmap_get(proc_v.hashmap_val, make_symbol(interp, interp.symbols.intern("handle")))
            : null;
        ph = get_process_handle(handle_v);
        ok = ph != null && ph.open && ph.uv_process != null && process_handle_try_begin_use(ph);
    }

    if (ok) {
        EvalResult wait_busy = run(
            "(await (spawn (lambda () (process-wait (ref p 'handle)))))",
            interp
        );
        EvalResult kill_busy = run("(process-kill (ref p 'handle) 15)", interp);
        ok = wait_busy.error.has_error &&
            kill_busy.error.has_error &&
            str_contains(wait_busy.error.message[:256], "process-handle already in use") &&
            str_contains(kill_busy.error.message[:256], "process-handle already in use");
        process_handle_end_use(ph);
        EvalResult cleanup_wait = run("(await (spawn (lambda () (process-wait (ref p 'handle)))))", interp);
        ok = ok &&
            !cleanup_wait.error.has_error &&
            cleanup_wait.value != null &&
            cleanup_wait.value.tag != ERROR &&
            cleanup_wait.value.tag != NIL;
    }

    if (ok) {
        emit_pass("process handle concurrent reuse fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] process handle concurrent reuse fails closed (setup_err=%s)",
            setup.error.has_error ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_advanced_signal_burst_delivery_test_impl(Interp* interp, int* pass, int* fail) {
    EvalResult setup_hits = run("(define signal-burst-hits {'n 0})", interp);
    EvalResult setup_handle = run(
        "(define signal-burst-h (signal-handle 10 (lambda (sig) nil)))",
        interp
    );

    bool ok = !setup_hits.error.has_error &&
        !setup_handle.error.has_error;
    int set_status = -1;
    int pending_before = -1;
    int pending_after = -1;
    int dispatch_count = -1;
    if (ok) {
        Value* handle_v = interp.global_env.lookup(interp.symbols.intern("signal-burst-h"));
        SignalHandle* signal_handle = get_signal_handle(handle_v);
        ok = signal_handle != null &&
            signal_handle.open &&
            signal_handle.uv_signal_watch != null;
        if (ok) {
            signal_debug_reset_dispatch_count();
            set_status = omni_uv_signal_debug_set_pending(signal_handle.uv_signal_watch, 1205);
            pending_before = omni_uv_signal_debug_pending(signal_handle.uv_signal_watch);
            dispatch_pending_signal_callbacks(interp);
            pending_after = omni_uv_signal_debug_pending(signal_handle.uv_signal_watch);
            dispatch_count = (int)signal_debug_dispatch_count();
        }

        ok = set_status == 0 &&
            pending_before >= 1205 &&
            pending_after == 0 &&
            dispatch_count >= 1205;
    }

    EvalResult cleanup = run("(signal-unhandle signal-burst-h)", interp);
    ok = ok &&
        !cleanup.error.has_error &&
        cleanup.value != null &&
        cleanup.value.tag == VOID;

    if (ok) {
        emit_pass("signal callback burst delivery drains all pending callbacks without 1024-cap loss");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] signal callback burst delivery drains all pending callbacks without 1024-cap loss (setup_hits_err=%s setup_handle_err=%s set_status=%d pending_before=%d pending_after=%d dispatch_count=%d cleanup_err=%s)",
            setup_hits.error.has_error ? "yes" : "no",
            setup_handle.error.has_error ? "yes" : "no",
            set_status,
            pending_before,
            pending_after,
            dispatch_count,
            cleanup.error.has_error ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void run_advanced_sigint_interrupt_routing_test_impl(Interp* interp, int* pass, int* fail) {
    Interp other = {};
    other.init();
    defer other.destroy();

    interp_interrupt_clear(interp);
    interp_interrupt_clear(&other);

    interp_sigint_bind(interp);
    defer interp_sigint_unbind(interp);
    interp_sigint_debug_mark_pending();

    bool active_requested = interp_interrupt_requested(interp);
    bool other_requested = interp_interrupt_requested(&other);
    interp_interrupt_clear(interp);
    interp_interrupt_clear(&other);

    if (active_requested && !other_requested) {
        emit_pass("sigint interrupt routing targets only bound interpreter");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] sigint interrupt routing targets only bound interpreter (active_requested=%s other_requested=%s)",
            active_requested ? "true" : "false",
            other_requested ? "true" : "false"
        );
        (*fail)++;
    }
}

fn Value* build_signal_noop_callback_impl(Interp* interp) {
    EvalResult callback_eval = run("(lambda (sig) nil)", interp);
    if (callback_eval.error.has_error) return null;
    if (callback_eval.value == null || callback_eval.value.tag == ERROR) return null;
    if (!signal_callback_callable(callback_eval.value)) return null;
    return callback_eval.value;
}

fn void run_advanced_signal_handle_registry_scaling_test_impl(Interp* interp, int* pass, int* fail) {
    const usz HANDLE_COUNT = 40;
    Value*[HANDLE_COUNT] handles;
    for (usz i = 0; i < HANDLE_COUNT; i++) handles[i] = null;

    bool ok = true;
    int create_fail_index = -1;
    int cleanup_fail_index = -1;
    Value* callback = build_signal_noop_callback_impl(interp);
    if (callback == null) {
        ok = false;
    }

    usz created_count = 0;
    if (ok) {
        for (usz i = 0; i < HANDLE_COUNT; i++) {
            Value*[2] create_args;
            create_args[0] = make_int(interp, 10);
            create_args[1] = callback;
            Value* created = prim_signal_handle(create_args[..], null, interp);
            if (created == null || created.tag == ERROR) {
                ok = false;
                create_fail_index = (int)i;
                break;
            }
            handles[i] = created;
            created_count++;
        }
    }

    for (usz i = 0; i < created_count; i++) {
        Value*[1] close_args;
        close_args[0] = handles[i];
        Value* closed = prim_signal_unhandle(close_args[..], null, interp);
        if (closed == null || closed.tag == ERROR || closed.tag != VOID) {
            ok = false;
            if (cleanup_fail_index < 0) cleanup_fail_index = (int)i;
        }
    }

    if (ok) {
        emit_pass("signal-handle supports >32 active handles without fixed registry cap");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] signal-handle supports >32 active handles without fixed registry cap (created_count=%d create_fail_index=%d cleanup_fail_index=%d)",
            (int)created_count,
            create_fail_index,
            cleanup_fail_index
        );
        (*fail)++;
    }
}

fn void run_advanced_signal_dispatch_owner_interp_isolation_test_impl(Interp* interp, int* pass, int* fail) {
    Value* callback = build_signal_noop_callback_impl(interp);
    bool ok = callback != null && signal_callback_callable(callback);
    int set_status = -1;
    int after_other = -1;
    int after_owner = -1;
    int cleanup_ok = -1;

    Value* handle_value = null;
    SignalHandle* sh = null;
    if (ok) {
        Value*[2] create_args;
        create_args[0] = make_int(interp, 10);
        create_args[1] = callback;
        handle_value = prim_signal_handle(create_args[..], null, interp);
        sh = get_signal_handle(handle_value);
        ok = handle_value != null &&
            handle_value.tag != ERROR &&
            sh != null &&
            sh.open &&
            sh.uv_signal_watch != null;
    }

    if (ok) {
        Interp other = {};
        other.init();
        defer other.destroy();

        signal_debug_reset_dispatch_count();
        set_status = omni_uv_signal_debug_set_pending(sh.uv_signal_watch, 3);
        dispatch_pending_signal_callbacks(&other);
        after_other = (int)signal_debug_dispatch_count();
        dispatch_pending_signal_callbacks(interp);
        after_owner = (int)signal_debug_dispatch_count();

        ok = set_status == 0 &&
            after_other == 0 &&
            after_owner >= 3;
    }

    if (handle_value != null && handle_value.tag != ERROR) {
        Value*[1] close_args;
        close_args[0] = handle_value;
        Value* closed = prim_signal_unhandle(close_args[..], null, interp);
        cleanup_ok = (closed != null && closed.tag == VOID) ? 1 : 0;
        ok = ok && cleanup_ok == 1;
    }

    if (ok) {
        emit_pass("signal callback dispatch stays isolated to owning interpreter");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] signal callback dispatch stays isolated to owning interpreter (set_status=%d after_other=%d after_owner=%d cleanup_ok=%d)",
            set_status,
            after_other,
            after_owner,
            cleanup_ok
        );
        (*fail)++;
    }
}

fn void run_advanced_signal_callback_scope_release_test_impl(Interp* interp, int* pass, int* fail) {
    bool ok = interp != null && interp.root_scope != null;
    uint root_before = ok ? interp.root_scope.refcount : 0;
    uint root_after_create = 0;
    uint root_after_unhandle = 0;
    int cleanup_ok = 0;

    Value* handle_value = null;
    if (ok) {
        Value* callback = build_signal_noop_callback_impl(interp);
        ok = callback != null && signal_callback_callable(callback);
        if (ok) {
            Value*[2] create_args;
            create_args[0] = make_int(interp, 10);
            create_args[1] = callback;
            handle_value = prim_signal_handle(create_args[..], null, interp);
            ok = handle_value != null && handle_value.tag != ERROR;
            if (ok) {
                root_after_create = interp.root_scope.refcount;
            }
        }
    }

    if (handle_value != null && handle_value.tag != ERROR) {
        Value*[1] close_args;
        close_args[0] = handle_value;
        Value* closed = prim_signal_unhandle(close_args[..], null, interp);
        cleanup_ok = (closed != null && closed.tag == VOID) ? 1 : 0;
        if (interp.root_scope != null) {
            root_after_unhandle = interp.root_scope.refcount;
        }
        ok = ok &&
            cleanup_ok == 1 &&
            root_after_create == (root_before + 1) &&
            root_after_unhandle == root_before;
    } else {
        ok = false;
    }

    if (ok) {
        emit_pass("signal-handle releases retained callback owner scope on unhandle");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] signal-handle releases retained callback owner scope on unhandle (root_before=%d root_after_create=%d root_after_unhandle=%d cleanup_ok=%d)",
            (int)root_before,
            (int)root_after_create,
            (int)root_after_unhandle,
            cleanup_ok
        );
        (*fail)++;
    }
}

fn void run_advanced_uv_timer_callback_register_invoke_test_impl(Interp* interp, int* pass, int* fail) {
    EvalResult setup_value = run("(define uv-cb-last-tick 0)", interp);
    EvalResult callback_eval = run("(lambda (tick) (set! uv-cb-last-tick tick))", interp);

    Value* handle_value = null;
    Value* invoke = null;
    Value* dispatch_count = null;
    Value* cleanup = null;
    EvalResult observed_tick = {};

    bool ok = !setup_value.error.has_error &&
        !callback_eval.error.has_error &&
        callback_eval.value != null &&
        callback_eval.value.tag != ERROR &&
        uv_timer_callback_callable(callback_eval.value);

    if (ok) {
        Value*[1] create_args;
        create_args[0] = callback_eval.value;
        handle_value = prim_uv_timer_callback_handle(create_args[..], null, interp);
        ok = handle_value != null && handle_value.tag != ERROR;
    }

    if (ok) {
        Value*[1] invoke_args;
        invoke_args[0] = handle_value;
        invoke = prim_uv_timer_callback_invoke(invoke_args[..], null, interp);
        dispatch_count = prim_uv_timer_callback_dispatch_count(invoke_args[..], null, interp);
        observed_tick = run("uv-cb-last-tick", interp);
        cleanup = prim_uv_timer_callback_unhandle(invoke_args[..], null, interp);

        ok = invoke != null && invoke.tag == VOID &&
            dispatch_count != null && dispatch_count.tag == INT && dispatch_count.int_val == 1 &&
            !observed_tick.error.has_error &&
            observed_tick.value != null && observed_tick.value.tag == INT && observed_tick.value.int_val == 1 &&
            cleanup != null && cleanup.tag == VOID;
    }

    if (ok) {
        emit_pass("uv timer callback shim register+invoke dispatches callback with tick context");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] uv timer callback shim register+invoke dispatches callback with tick context (setup_err=%s callback_err=%s handle_tag=%d invoke_tag=%d dispatch_count_tag=%d dispatch_count=%d observed_tick_tag=%d observed_tick=%d cleanup_tag=%d)",
            setup_value.error.has_error ? "yes" : "no",
            callback_eval.error.has_error ? "yes" : "no",
            handle_value != null ? (int)handle_value.tag : -1,
            invoke != null ? (int)invoke.tag : -1,
            dispatch_count != null ? (int)dispatch_count.tag : -1,
            dispatch_count != null && dispatch_count.tag == INT ? (int)dispatch_count.int_val : -1,
            observed_tick.value != null ? (int)observed_tick.value.tag : -1,
            observed_tick.value != null && observed_tick.value.tag == INT ? (int)observed_tick.value.int_val : -1,
            cleanup != null ? (int)cleanup.tag : -1
        );
        (*fail)++;
    }
}

fn void run_advanced_uv_timer_callback_unhandle_idempotence_test_impl(Interp* interp, int* pass, int* fail) {
    Value* callback = build_signal_noop_callback_impl(interp);
    Value* handle = null;
    Value* invoke_once = null;
    Value* unhandle_once = null;
    Value* unhandle_twice = null;
    Value* stale_invoke = null;

    bool ok = callback != null && uv_timer_callback_callable(callback);
    if (ok) {
        Value*[1] create_args;
        create_args[0] = callback;
        handle = prim_uv_timer_callback_handle(create_args[..], null, interp);
        ok = handle != null && handle.tag != ERROR;
    }

    if (ok) {
        Value*[1] op_args;
        op_args[0] = handle;
        invoke_once = prim_uv_timer_callback_invoke(op_args[..], null, interp);
        unhandle_once = prim_uv_timer_callback_unhandle(op_args[..], null, interp);
        unhandle_twice = prim_uv_timer_callback_unhandle(op_args[..], null, interp);
        stale_invoke = prim_uv_timer_callback_invoke(op_args[..], null, interp);

        ok = invoke_once != null && invoke_once.tag == VOID &&
            unhandle_once != null && unhandle_once.tag == VOID &&
            unhandle_twice != null && unhandle_twice.tag == VOID &&
            stale_invoke != null && stale_invoke.tag == ERROR &&
            stale_invoke.str_chars != null &&
            str_contains(stale_invoke.str_chars[:stale_invoke.str_len], "stale callback handle");
    }

    if (ok) {
        emit_pass("uv timer callback shim unhandle is idempotent and stale invoke fails closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] uv timer callback shim unhandle is idempotent and stale invoke fails closed (handle_tag=%d invoke_tag=%d unhandle_once_tag=%d unhandle_twice_tag=%d stale_tag=%d)",
            handle != null ? (int)handle.tag : -1,
            invoke_once != null ? (int)invoke_once.tag : -1,
            unhandle_once != null ? (int)unhandle_once.tag : -1,
            unhandle_twice != null ? (int)unhandle_twice.tag : -1,
            stale_invoke != null ? (int)stale_invoke.tag : -1
        );
        (*fail)++;
    }
}

fn void run_advanced_uv_timer_callback_scope_release_test_impl(Interp* interp, int* pass, int* fail) {
    bool ok = interp != null && interp.root_scope != null;
    uint root_before = ok ? interp.root_scope.refcount : 0;
    uint root_after_create = 0;
    uint root_after_unhandle = 0;
    int cleanup_ok = 0;

    Value* handle_value = null;
    if (ok) {
        Value* callback = build_signal_noop_callback_impl(interp);
        ok = callback != null && signal_callback_callable(callback);
        if (ok) {
            Value*[1] create_args;
            create_args[0] = callback;
            handle_value = prim_uv_timer_callback_handle(create_args[..], null, interp);
            ok = handle_value != null && handle_value.tag != ERROR;
            if (ok) {
                root_after_create = interp.root_scope.refcount;
            }
        }
    }

    if (handle_value != null && handle_value.tag != ERROR) {
        Value*[1] close_args;
        close_args[0] = handle_value;
        Value* closed = prim_uv_timer_callback_unhandle(close_args[..], null, interp);
        cleanup_ok = (closed != null && closed.tag == VOID) ? 1 : 0;
        if (interp.root_scope != null) {
            root_after_unhandle = interp.root_scope.refcount;
        }
        ok = ok &&
            cleanup_ok == 1 &&
            root_after_create == (root_before + 1) &&
            root_after_unhandle == root_before;
    } else {
        ok = false;
    }

    if (ok) {
        emit_pass("uv timer callback shim releases retained callback owner scope on unhandle");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] uv timer callback shim releases retained callback owner scope on unhandle (root_before=%d root_after_create=%d root_after_unhandle=%d cleanup_ok=%d)",
            (int)root_before,
            (int)root_after_create,
            (int)root_after_unhandle,
            cleanup_ok
        );
        (*fail)++;
    }
}

fn void run_advanced_uv_timer_callback_handle_isolation_test_impl(Interp* interp, int* pass, int* fail) {
    Value* callback = build_signal_noop_callback_impl(interp);
    Value* handle_a = null;
    Value* handle_b = null;
    Value* invoke_a = null;
    Value* invoke_b1 = null;
    Value* invoke_b2 = null;
    Value* count_a = null;
    Value* count_b = null;
    Value* cleanup_a = null;
    Value* cleanup_b = null;

    bool ok = callback != null && uv_timer_callback_callable(callback);
    if (ok) {
        Value*[1] create_args;
        create_args[0] = callback;
        handle_a = prim_uv_timer_callback_handle(create_args[..], null, interp);
        handle_b = prim_uv_timer_callback_handle(create_args[..], null, interp);
        ok = handle_a != null && handle_a.tag != ERROR &&
            handle_b != null && handle_b.tag != ERROR;
    }

    if (ok) {
        Value*[1] a_args;
        a_args[0] = handle_a;
        Value*[1] b_args;
        b_args[0] = handle_b;

        invoke_a = prim_uv_timer_callback_invoke(a_args[..], null, interp);
        invoke_b1 = prim_uv_timer_callback_invoke(b_args[..], null, interp);
        invoke_b2 = prim_uv_timer_callback_invoke(b_args[..], null, interp);
        count_a = prim_uv_timer_callback_dispatch_count(a_args[..], null, interp);
        count_b = prim_uv_timer_callback_dispatch_count(b_args[..], null, interp);
        cleanup_a = prim_uv_timer_callback_unhandle(a_args[..], null, interp);
        cleanup_b = prim_uv_timer_callback_unhandle(b_args[..], null, interp);

        ok = invoke_a != null && invoke_a.tag == VOID &&
            invoke_b1 != null && invoke_b1.tag == VOID &&
            invoke_b2 != null && invoke_b2.tag == VOID &&
            count_a != null && count_a.tag == INT && count_a.int_val == 1 &&
            count_b != null && count_b.tag == INT && count_b.int_val == 2 &&
            cleanup_a != null && cleanup_a.tag == VOID &&
            cleanup_b != null && cleanup_b.tag == VOID;
    }

    if (ok) {
        emit_pass("uv timer callback shim keeps handle dispatch isolated");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] uv timer callback shim keeps handle dispatch isolated (handle_a_tag=%d handle_b_tag=%d count_a_tag=%d count_a=%d count_b_tag=%d count_b=%d)",
            handle_a != null ? (int)handle_a.tag : -1,
            handle_b != null ? (int)handle_b.tag : -1,
            count_a != null ? (int)count_a.tag : -1,
            count_a != null && count_a.tag == INT ? (int)count_a.int_val : -1,
            count_b != null ? (int)count_b.tag : -1,
            count_b != null && count_b.tag == INT ? (int)count_b.int_val : -1
        );
        (*fail)++;
    }
}
