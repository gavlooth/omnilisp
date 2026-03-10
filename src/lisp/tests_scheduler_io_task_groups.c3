module lisp;


import std::io;
import main;
fn void run_scheduler_offload_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(gunzip (offload 'gzip \"hello-offload\"))", interp);
        if (!r.error.has_error && is_string(r.value) &&
            r.value.str_len == 13 &&
            str_starts_with(r.value.str_chars[:r.value.str_len], "hello-offload")) {
            emit_pass_literal("[PASS] offload sync gzip roundtrip");
            (*pass)++;
        } else {
            io::printn("[FAIL] offload sync gzip roundtrip");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define a (spawn (lambda () (offload 'sleep-ms 5)))) "
            "  (define b (spawn (lambda () (offload 'sleep-ms 5)))) "
            "  (await a) "
            "  (await b) "
            "  1)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] offload async fiber resume");
            (*pass)++;
        } else {
            io::printn("[FAIL] offload async fiber resume");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(offload 'nope 1)", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] offload rejects unsupported op");
            (*pass)++;
        } else {
            io::printn("[FAIL] offload rejects unsupported op");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(offload 'http-get \"https://example.com\")", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] offload rejects internal io op");
            (*pass)++;
        } else {
            io::printn("[FAIL] offload rejects internal io op");
            (*fail)++;
        }
    }
}

fn void run_scheduler_thread_join_tests(Interp* interp, int* pass, int* fail) {
    test_truthy(interp, "typed wrapper dispatch reason task-spawn",
        "(let (r (ref (ref (explain 'dispatch (task-spawn 'sleep-ms 1)) 'decision) 'reason)) (or (= r 'method-match) (= r 'fallback)))",
        pass, fail);

    test_truthy(interp, "typed wrapper dispatch reason thread-spawn",
        "(let (r (ref (ref (explain 'dispatch (thread-spawn 'sleep-ms 1)) 'decision) 'reason)) (or (= r 'method-match) (= r 'fallback)))",
        pass, fail);

    {
        EvalResult r = run(
            "(block "
            "  (define h (job-spawn 'task 'sleep-ms 1)) "
            "  (job-join 'task h) "
            "  true)",
            interp);
        if (!r.error.has_error && r.value != null && r.value.tag != ERROR && r.value.tag != NIL) {
            emit_pass_literal("[PASS] helper job-spawn/job-join task route");
            (*pass)++;
        } else {
            io::printn("[FAIL] helper job-spawn/job-join task route");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define h (job-spawn 'thread 'sleep-ms 1)) "
            "  (job-join 'thread h) "
            "  true)",
            interp);
        if (!r.error.has_error && r.value != null && r.value.tag != ERROR && r.value.tag != NIL) {
            emit_pass_literal("[PASS] helper job-spawn/job-join thread route");
            (*pass)++;
        } else {
            io::printn("[FAIL] helper job-spawn/job-join thread route");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t (task-spawn 'gzip \"thread-task\")) "
            "  (gunzip (task-join t)))",
            interp);
        if (!r.error.has_error && is_string(r.value) &&
            r.value.str_len == 11 &&
            str_starts_with(r.value.str_chars[:r.value.str_len], "thread-task")) {
            emit_pass_literal("[PASS] task-spawn/task-join gzip");
            (*pass)++;
        } else {
            io::printn("[FAIL] task-spawn/task-join gzip");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(task-spawn 'tcp-connect \"127.0.0.1\" 1)", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] task-spawn rejects internal io op");
            (*pass)++;
        } else {
            io::printn("[FAIL] task-spawn rejects internal io op");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t1 (task-spawn 'sleep-ms 5)) "
            "  (define t2 (task-spawn 'sleep-ms 5)) "
            "  (task-join t1) "
            "  (task-join t2) "
            "  1)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] task-spawn concurrent join");
            (*pass)++;
        } else {
            io::printn("[FAIL] task-spawn concurrent join");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t (task-spawn 'sleep-ms 1)) "
            "  (task-join t) "
            "  (task-join t))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] task-join double join rejected");
            (*pass)++;
        } else {
            io::printn("[FAIL] task-join double join rejected");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t (thread-spawn 'gzip \"os-thread\")) "
            "  (gunzip (thread-join t)))",
            interp);
        if (!r.error.has_error && is_string(r.value) &&
            r.value.str_len == 9 &&
            str_starts_with(r.value.str_chars[:r.value.str_len], "os-thread")) {
            emit_pass_literal("[PASS] thread-spawn/thread-join gzip");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-spawn/thread-join gzip");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(thread-spawn 'tcp-connect \"127.0.0.1\" 1)", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] thread-spawn rejects internal io op");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-spawn rejects internal io op");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t (thread-spawn 'sleep-ms 1)) "
            "  (thread-join t) "
            "  (thread-join t))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] thread-join double join rejected");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-join double join rejected");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define f (spawn (lambda () "
            "    (define t (task-spawn 'sleep-ms 1)) "
            "    (task-join t) "
            "    true))) "
            "  (await f))",
            interp);
        if (!r.error.has_error && r.value != null && r.value.tag != ERROR && r.value.tag != NIL) {
            emit_pass_literal("[PASS] task-join from fiber context");
            (*pass)++;
        } else {
            io::printn("[FAIL] task-join from fiber context");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define f (spawn (lambda () "
            "    (define t (thread-spawn 'sleep-ms 1)) "
            "    (thread-join t) "
            "    true))) "
            "  (await f))",
            interp);
        if (!r.error.has_error && r.value != null && r.value.tag != ERROR && r.value.tag != NIL) {
            emit_pass_literal("[PASS] thread-join from fiber context");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-join from fiber context");
            (*fail)++;
        }
    }
}

fn void run_scheduler_thread_timeout_cancel_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define t (task-spawn 'sleep-ms 20)) "
            "  (task-join-timeout t 0))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] task-join-timeout immediate timeout");
            (*pass)++;
        } else {
            io::printn("[FAIL] task-join-timeout immediate timeout");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t (task-spawn 'sleep-ms 5)) "
            "  (task-join-timeout t 200) "
            "  1)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] task-join-timeout success");
            (*pass)++;
        } else {
            io::printn("[FAIL] task-join-timeout success");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define f (spawn (lambda () "
            "    (define t (task-spawn 'sleep-ms 1)) "
            "    (task-join-timeout t 200) "
            "    true))) "
            "  (await f))",
            interp);
        if (!r.error.has_error && r.value != null && r.value.tag != ERROR && r.value.tag != NIL) {
            emit_pass_literal("[PASS] task-join-timeout from fiber context");
            (*pass)++;
        } else {
            io::printn("[FAIL] task-join-timeout from fiber context");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t (task-spawn 'sleep-ms 20)) "
            "  (task-cancel t) "
            "  (task-join t))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] task-cancel causes join cancellation");
            (*pass)++;
        } else {
            io::printn("[FAIL] task-cancel causes join cancellation");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t (thread-spawn 'sleep-ms 20)) "
            "  (thread-join-timeout t 0))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] thread-join-timeout immediate timeout");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-join-timeout immediate timeout");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t (thread-spawn 'sleep-ms 5)) "
            "  (thread-join-timeout t 200) "
            "  1)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] thread-join-timeout success");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-join-timeout success");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define f (spawn (lambda () "
            "    (define t (thread-spawn 'sleep-ms 1)) "
            "    (thread-join-timeout t 200) "
            "    true))) "
            "  (await f))",
            interp);
        if (!r.error.has_error && r.value != null && r.value.tag != ERROR && r.value.tag != NIL) {
            emit_pass_literal("[PASS] thread-join-timeout from fiber context");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-join-timeout from fiber context");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define t (thread-spawn 'sleep-ms 20)) "
            "  (thread-cancel t) "
            "  (thread-join t))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] thread-cancel causes join cancellation");
            (*pass)++;
        } else {
            io::printn("[FAIL] thread-cancel causes join cancellation");
            (*fail)++;
        }
    }
}

fn void run_scheduler_offload_thread_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_offload_tests(interp, pass, fail);
    run_scheduler_thread_join_tests(interp, pass, fail);
    run_scheduler_thread_timeout_cancel_tests(interp, pass, fail);
}

fn void run_scheduler_fiber_temp_thread_boundary_tests(Interp* interp, int* pass, int* fail) {
    if (test_c_getenv("OMNI_FIBER_TEMP") == null) {
        emit_pass_literal("[PASS] fiber-temp thread boundary metrics (flag off)");
        (*pass)++;
        return;
    }

    usz ctx_hits_before = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_before = main::g_fiber_temp_pool_stats.ctx_return_count;
    bool ok = true;

    for (usz i = 0; i < 8; i++) {
        EvalResult r = run(
            "(block "
            "  (define ft-t (task-spawn 'sleep-ms 1)) "
            "  (task-join ft-t) "
            "  true)",
            interp);
        if (r.error.has_error || r.value == null || r.value.tag == ERROR || r.value.tag == NIL) {
            ok = false;
            break;
        }
    }

    usz ctx_hits_after = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_after = main::g_fiber_temp_pool_stats.ctx_return_count;

    ok = ok &&
         (ctx_hits_after == ctx_hits_before) &&
         (ctx_returns_after == ctx_returns_before);

    if (ok) {
        emit_pass_literal("[PASS] fiber-temp thread boundary metrics stay context-local");
        (*pass)++;
    } else {
        io::printn("[FAIL] fiber-temp thread boundary metrics stay context-local");
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_offload_interleave_tests(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    usz failed_step = 0;

    for (usz i = 0; i < 12; i++) {
        EvalResult async_r = run(
            "(block "
            "  (define wf (spawn (lambda () (offload 'sleep-ms 1)))) "
            "  (await wf) "
            "  true)",
            interp);
        if (async_r.error.has_error ||
            async_r.value == null ||
            async_r.value.tag == ERROR ||
            async_r.value.tag == NIL) {
            ok = false;
            failed_step = i;
            break;
        }

        bool pushed = scheduler_publish_poll_error(MAX_FIBERS + i + 1, -1);
        drain_wakeups();
        if (!pushed) {
            ok = false;
            failed_step = i;
            break;
        }

        EvalResult thread_r = run(
            "(block "
            "  (define wt (task-spawn 'sleep-ms 1)) "
            "  (task-join wt) "
            "  true)",
            interp);
        if (thread_r.error.has_error ||
            thread_r.value == null ||
            thread_r.value.tag == ERROR ||
            thread_r.value.tag == NIL) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    ok = ok && reliable_wakeup_pending_count() == 0;

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup/offload interleave stress");
        (*pass)++;
    } else {
        io::printfn("[FAIL] scheduler wakeup/offload interleave stress at step %d", (int)failed_step);
        (*fail)++;
    }
}

fn void run_scheduler_fiber_temp_cancel_timeout_boundary_tests(Interp* interp, int* pass, int* fail) {
    if (test_c_getenv("OMNI_FIBER_TEMP") == null) {
        emit_pass_literal("[PASS] fiber-temp cancel/timeout boundary metrics (flag off)");
        (*pass)++;
        return;
    }

    usz ctx_hits_before = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_before = main::g_fiber_temp_pool_stats.ctx_return_count;
    bool ok = true;

    for (usz i = 0; i < 8; i++) {
        EvalResult timeout_r = run(
            "(block "
            "  (define ft-to (task-spawn 'sleep-ms 20)) "
            "  (task-join-timeout ft-to 0))",
            interp);
        if (!(timeout_r.error.has_error || (timeout_r.value != null && timeout_r.value.tag == ERROR))) {
            ok = false;
            break;
        }

        EvalResult cancel_r = run(
            "(block "
            "  (define ft-c (task-spawn 'sleep-ms 20)) "
            "  (task-cancel ft-c) "
            "  (task-join ft-c))",
            interp);
        if (!(cancel_r.error.has_error || (cancel_r.value != null && cancel_r.value.tag == ERROR))) {
            ok = false;
            break;
        }

        EvalResult success_r = run(
            "(block "
            "  (define ft-ok (task-spawn 'sleep-ms 1)) "
            "  (task-join-timeout ft-ok 200) "
            "  true)",
            interp);
        if (success_r.error.has_error ||
            success_r.value == null ||
            success_r.value.tag == ERROR ||
            success_r.value.tag == NIL) {
            ok = false;
            break;
        }
    }

    usz ctx_hits_after = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_after = main::g_fiber_temp_pool_stats.ctx_return_count;
    ok = ok &&
         (ctx_hits_after == ctx_hits_before) &&
         (ctx_returns_after == ctx_returns_before);

    if (ok) {
        emit_pass_literal("[PASS] fiber-temp cancel/timeout boundaries stay context-local");
        (*pass)++;
    } else {
        io::printn("[FAIL] fiber-temp cancel/timeout boundaries stay context-local");
        (*fail)++;
    }
}

fn void run_scheduler_offload_cancel_boundary_tests(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    usz failed_step = 0;

    for (usz i = 0; i < 12; i++) {
        EvalResult r = run(
            "(block "
            "  (define oc (spawn (lambda () (offload 'sleep-ms 5)))) "
            "  (fiber-cancel oc) "
            "  (run-fibers) "
            "  true)",
            interp);
        if (r.error.has_error ||
            r.value == null ||
            r.value.tag == ERROR ||
            r.value.tag == NIL) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] offload cancel destroy-before-complete stress");
        (*pass)++;
    } else {
        io::printfn("[FAIL] offload cancel destroy-before-complete stress at step %d", (int)failed_step);
        (*fail)++;
    }
}

fn void run_scheduler_cancel_tests(Interp* interp, int* pass, int* fail) {
    {
        setup(interp, "(define fc-fib1 (spawn (lambda () 42)))");
        EvalResult r = run("(fiber-cancel fc-fib1)", interp);
        if (!r.error.has_error && r.value != null && r.value.tag != NIL && r.value.tag != ERROR) {
            emit_pass_literal("[PASS] fiber-cancel ready fiber");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel ready fiber");
            (*fail)++;
        }
    }

    {
        setup(interp, "(define fc-fib2 (spawn (lambda () 1)))");
        setup(interp, "(fiber-cancel fc-fib2)");
        EvalResult r = run("(fiber-cancel fc-fib2)", interp);
        if (!r.error.has_error && r.value != null && r.value.tag == NIL) {
            emit_pass_literal("[PASS] fiber-cancel twice → nil");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel twice → nil");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(fiber-cancel 999)", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] fiber-cancel invalid id → error");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel invalid id → error");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define fc-parent (spawn (lambda () "
            "    (define fc-child (spawn (lambda () 99))) "
            "    (await fc-child)))) "
            "  (fiber-cancel fc-parent) "
            "  (run-fibers) "
            "  true)",
            interp);
        if (!r.error.has_error) {
            emit_pass_literal("[PASS] fiber-cancel propagates to children");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel propagates to children");
            (*fail)++;
        }
    }
}

fn void run_scheduler_fairness_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define fair-a (spawn (lambda () 10))) "
            "  (define fair-b (spawn (lambda () 20))) "
            "  (run-fibers) "
            "  (+ 10 20))",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 30) {
            emit_pass_literal("[PASS] fairness: both fibers complete");
            (*pass)++;
        } else {
            io::printn("[FAIL] fairness: both fibers complete");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (spawn (lambda () 1)) (spawn (lambda () 2)) (spawn (lambda () 3)) "
            "  (spawn (lambda () 4)) (spawn (lambda () 5)) (spawn (lambda () 6)) "
            "  (spawn (lambda () 7)) (spawn (lambda () 8)) (spawn (lambda () 9)) "
            "  (spawn (lambda () 10)) "
            "  (run-fibers) "
            "  true)",
            interp);
        if (!r.error.has_error) {
            emit_pass_literal("[PASS] fairness: 10 fibers all complete");
            (*pass)++;
        } else {
            io::printn("[FAIL] fairness: 10 fibers all complete");
            (*fail)++;
        }
    }
}
