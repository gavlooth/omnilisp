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
    run_scheduler_offload_hot_path_benchmarks(interp);
}
