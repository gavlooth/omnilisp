module lisp;


import std::core::mem;
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
        usz fiber_count_before = g_scheduler.fiber_count;
        EvalResult r = run(
            "(let (sched-offload-a (spawn (lambda () (offload 'sleep-ms 5))) "
            "      sched-offload-b (spawn (lambda () (offload 'sleep-ms 5)))) "
            "  (block "
            "    (await sched-offload-a) "
            "    (await sched-offload-b) "
            "    1))",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] offload async fiber resume");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] offload async fiber resume (fiber_count_before=%d error: %s)",
                    (long)fiber_count_before, (ZString)r.error.message[:256]);
            } else if (r.value == null) {
                io::printfn("[FAIL] offload async fiber resume (fiber_count_before=%d null result)",
                    (long)fiber_count_before);
            } else {
                io::printfn("[FAIL] offload async fiber resume (fiber_count_before=%d tag=%d)",
                    (long)fiber_count_before, (int)r.value.tag);
            }
            (*fail)++;
        }
    }

    scheduler_test_expect_error(
        interp,
        "(offload 'nope 1)",
        "[PASS] offload rejects unsupported op",
        "[FAIL] offload rejects unsupported op",
        pass,
        fail
    );

    scheduler_test_expect_error(
        interp,
        "(offload 'http-get \"https://example.com\")",
        "[PASS] offload rejects internal io op",
        "[FAIL] offload rejects internal io op",
        pass,
        fail
    );

    test_error_contains(interp, "offload sleep-ms rejects zero duration",
        "(offload 'sleep-ms 0)",
        "offload sleep-ms: milliseconds must be positive", pass, fail);
    test_error_contains(interp, "offload sleep-ms rejects negative duration",
        "(offload 'sleep-ms -1)",
        "offload sleep-ms: milliseconds must be positive", pass, fail);
    test_error_contains(interp, "offload sleep-ms rejects too-large duration",
        "(offload 'sleep-ms 4294968)",
        "offload sleep-ms: milliseconds must be positive", pass, fail);
    test_error_contains(interp, "offload sleep-ms rejects trailing arg",
        "(offload 'sleep-ms 1 'ignored)",
        "offload sleep-ms: expected exactly 1 argument", pass, fail);
    test_error_contains(interp, "offload gzip rejects trailing arg",
        "(offload 'gzip \"payload\" 'ignored)",
        "offload compress: expected exactly 1 argument", pass, fail);
}

fn void run_scheduler_thread_join_tests(Interp* interp, int* pass, int* fail) {
    test_truthy(interp, "typed wrapper dispatch reason task-spawn",
        "(let (r (ref (ref (explain 'dispatch (task-spawn 'sleep-ms 1)) 'decision) 'reason)) (or (= r 'method-match) (= r 'fallback)))",
        pass, fail);

    test_truthy(interp, "typed wrapper dispatch reason thread-spawn",
        "(let (r (ref (ref (explain 'dispatch (thread-spawn 'sleep-ms 1)) 'decision) 'reason)) (or (= r 'method-match) (= r 'fallback)))",
        pass, fail);

    scheduler_test_expect_truthy_success(
        interp,
        "(block "
        "  (define h (job-spawn 'task 'sleep-ms 1)) "
        "  (job-join 'task h) "
        "  true)",
        "[PASS] helper job-spawn/job-join task route",
        "[FAIL] helper job-spawn/job-join task route",
        pass,
        fail
    );

    scheduler_test_expect_truthy_success(
        interp,
        "(block "
        "  (define h (job-spawn 'thread 'sleep-ms 1)) "
        "  (job-join 'thread h) "
        "  true)",
        "[PASS] helper job-spawn/job-join thread route",
        "[FAIL] helper job-spawn/job-join thread route",
        pass,
        fail
    );

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

    scheduler_test_expect_error(
        interp,
        "(task-spawn 'tcp-connect \"127.0.0.1\" 1)",
        "[PASS] task-spawn rejects internal io op",
        "[FAIL] task-spawn rejects internal io op",
        pass,
        fail
    );

    test_error_contains(interp, "task-spawn sleep-ms rejects zero duration",
        "(task-spawn 'sleep-ms 0)",
        "offload sleep-ms: milliseconds must be positive", pass, fail);
    test_error_contains(interp, "task-spawn sleep-ms rejects negative duration",
        "(task-spawn 'sleep-ms -1)",
        "offload sleep-ms: milliseconds must be positive", pass, fail);
    test_error_contains(interp, "task-spawn sleep-ms rejects too-large duration",
        "(task-spawn 'sleep-ms 4294968)",
        "offload sleep-ms: milliseconds must be positive", pass, fail);
    test_error_contains(interp, "task-spawn sleep-ms rejects trailing arg",
        "(task-spawn 'sleep-ms 1 'ignored)",
        "offload sleep-ms: expected exactly 1 argument", pass, fail);
    test_error_contains(interp, "task-spawn gzip rejects trailing arg",
        "(task-spawn 'gzip \"payload\" 'ignored)",
        "offload compress: expected exactly 1 argument", pass, fail);

    scheduler_test_expect_int_exact(
        interp,
        "(block "
        "  (define t1 (task-spawn 'sleep-ms 5)) "
        "  (define t2 (task-spawn 'sleep-ms 5)) "
        "  (task-join t1) "
        "  (task-join t2) "
        "  1)",
        1,
        "[PASS] task-spawn concurrent join",
        "[FAIL] task-spawn concurrent join (got %d)",
        pass,
        fail
    );

    scheduler_test_expect_error(
        interp,
        "(block "
        "  (define t (task-spawn 'sleep-ms 1)) "
        "  (task-join t) "
        "  (task-join t))",
        "[PASS] task-join double join rejected",
        "[FAIL] task-join double join rejected",
        pass,
        fail
    );

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

    scheduler_test_expect_error(
        interp,
        "(thread-spawn 'tcp-connect \"127.0.0.1\" 1)",
        "[PASS] thread-spawn rejects internal io op",
        "[FAIL] thread-spawn rejects internal io op",
        pass,
        fail
    );

    test_error_contains(interp, "thread-spawn sleep-ms rejects zero duration",
        "(thread-spawn 'sleep-ms 0)",
        "offload sleep-ms: milliseconds must be positive", pass, fail);
    test_error_contains(interp, "thread-spawn sleep-ms rejects negative duration",
        "(thread-spawn 'sleep-ms -1)",
        "offload sleep-ms: milliseconds must be positive", pass, fail);
    test_error_contains(interp, "thread-spawn sleep-ms rejects too-large duration",
        "(thread-spawn 'sleep-ms 4294968)",
        "offload sleep-ms: milliseconds must be positive", pass, fail);
    test_error_contains(interp, "thread-spawn sleep-ms rejects trailing arg",
        "(thread-spawn 'sleep-ms 1 'ignored)",
        "offload sleep-ms: expected exactly 1 argument", pass, fail);
    test_error_contains(interp, "thread-spawn gzip rejects trailing arg",
        "(thread-spawn 'gzip \"payload\" 'ignored)",
        "offload compress: expected exactly 1 argument", pass, fail);

    scheduler_test_expect_error(
        interp,
        "(block "
        "  (define t (thread-spawn 'sleep-ms 1)) "
        "  (thread-join t) "
        "  (thread-join t))",
        "[PASS] thread-join double join rejected",
        "[FAIL] thread-join double join rejected",
        pass,
        fail
    );

    scheduler_test_expect_truthy_success(
        interp,
        "(block "
        "  (define f (spawn (lambda () "
        "    (define t (task-spawn 'sleep-ms 1)) "
        "    (task-join t) "
        "    true))) "
        "  (await f))",
        "[PASS] task-join from fiber context",
        "[FAIL] task-join from fiber context",
        pass,
        fail
    );

    scheduler_test_expect_truthy_success(
        interp,
        "(block "
        "  (define f (spawn (lambda () "
        "    (define t (thread-spawn 'sleep-ms 1)) "
        "    (thread-join t) "
        "    true))) "
        "  (await f))",
        "[PASS] thread-join from fiber context",
        "[FAIL] thread-join from fiber context",
        pass,
        fail
    );
}

fn void run_scheduler_thread_timeout_cancel_tests(Interp* interp, int* pass, int* fail) {
    scheduler_test_expect_error(
        interp,
        "(block "
        "  (define t (task-spawn 'sleep-ms 20)) "
        "  (task-join-timeout t 0))",
        "[PASS] task-join-timeout immediate timeout",
        "[FAIL] task-join-timeout immediate timeout",
        pass,
        fail
    );

    scheduler_test_expect_int_exact(
        interp,
        "(block "
        "  (define t (task-spawn 'sleep-ms 5)) "
        "  (task-join-timeout t 200) "
        "  1)",
        1,
        "[PASS] task-join-timeout success",
        "[FAIL] task-join-timeout success (got %d)",
        pass,
        fail
    );

    scheduler_test_expect_truthy_success(
        interp,
        "(block "
        "  (define f (spawn (lambda () "
        "    (define t (task-spawn 'sleep-ms 1)) "
        "    (task-join-timeout t 200) "
        "    true))) "
        "  (await f))",
        "[PASS] task-join-timeout from fiber context",
        "[FAIL] task-join-timeout from fiber context",
        pass,
        fail
    );

    scheduler_test_expect_void(
        interp,
        "(block "
        "  (define t (task-spawn 'sleep-ms 20)) "
        "  (task-cancel t))",
        "[PASS] task-cancel returns Void",
        "[FAIL] task-cancel returns Void",
        pass,
        fail
    );

    scheduler_test_expect_error(
        interp,
        "(block "
        "  (define t (task-spawn 'sleep-ms 20)) "
        "  (task-cancel t) "
        "  (task-join t))",
        "[PASS] task-cancel causes join cancellation",
        "[FAIL] task-cancel causes join cancellation",
        pass,
        fail
    );

    scheduler_test_expect_error(
        interp,
        "(block "
        "  (define t (thread-spawn 'sleep-ms 20)) "
        "  (thread-join-timeout t 0))",
        "[PASS] thread-join-timeout immediate timeout",
        "[FAIL] thread-join-timeout immediate timeout",
        pass,
        fail
    );

    scheduler_test_expect_int_exact(
        interp,
        "(block "
        "  (define t (thread-spawn 'sleep-ms 5)) "
        "  (thread-join-timeout t 200) "
        "  1)",
        1,
        "[PASS] thread-join-timeout success",
        "[FAIL] thread-join-timeout success (got %d)",
        pass,
        fail
    );

    scheduler_test_expect_truthy_success(
        interp,
        "(block "
        "  (define f (spawn (lambda () "
        "    (define t (thread-spawn 'sleep-ms 1)) "
        "    (thread-join-timeout t 200) "
        "    true))) "
        "  (await f))",
        "[PASS] thread-join-timeout from fiber context",
        "[FAIL] thread-join-timeout from fiber context",
        pass,
        fail
    );

    scheduler_test_expect_void(
        interp,
        "(block "
        "  (define t (thread-spawn 'sleep-ms 20)) "
        "  (thread-cancel t))",
        "[PASS] thread-cancel returns Void",
        "[FAIL] thread-cancel returns Void",
        pass,
        fail
    );

    scheduler_test_expect_error(
        interp,
        "(block "
        "  (define t (thread-spawn 'sleep-ms 20)) "
        "  (thread-cancel t) "
        "  (thread-join t))",
        "[PASS] thread-cancel causes join cancellation",
        "[FAIL] thread-cancel causes join cancellation",
        pass,
        fail
    );
}

fn void run_scheduler_offload_thread_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_offload_tests(interp, pass, fail);
    run_scheduler_thread_join_tests(interp, pass, fail);
    run_scheduler_thread_timeout_cancel_tests(interp, pass, fail);
    run_scheduler_offload_hot_path_benchmarks(interp);
}
