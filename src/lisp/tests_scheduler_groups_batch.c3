module lisp;

import std::io;
import std::core::mem;
import main;

fn usz scheduler_test_count_active_shared_handles() {
    if (!scheduler_shared_registry_ensure_ready()) return usz.max;
    SharedRegistry* registry = &g_scheduler.shared_registry;
    usz count = 0;
    registry.mu.lock();
    defer registry.mu.unlock();
    for (usz i = 0; i < SHARED_REGISTRY_CAPACITY; i++) {
        if (registry.entries[i].active) count++;
    }
    return count;
}

fn usz scheduler_test_count_active_os_threads() {
    if (!g_scheduler.thread_task_mu_ready) return usz.max;
    usz count = 0;
    g_scheduler.thread_task_mu.lock();
    defer g_scheduler.thread_task_mu.unlock();
    for (usz i = 0; i < MAX_OS_THREADS; i++) {
        if (g_scheduler.os_threads[i].active) count++;
    }
    return count;
}

fn void scheduler_expect_batch_error_releases_shared(
    Interp* interp,
    char[] label,
    char[] expr,
    char[] expected,
    int* pass,
    int* fail
) {
    scheduler_init();
    scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
    usz shared_before = scheduler_test_count_active_shared_handles();
    EvalResult r = run(expr, interp);
    scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
    usz shared_after = scheduler_test_count_active_shared_handles();
    bool ok = r.error.has_error &&
              str_starts_with(r.error.message[:256], expected) &&
              shared_before == shared_after;

    if (ok) {
        io::printfn("[PASS] %s", (ZString)label);
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] %s (error=%s shared_before=%d shared_after=%d)",
            (ZString)label,
            r.error.has_error ? (ZString)r.error.message[:256] : (ZString)"<none>",
            (int)shared_before,
            (int)shared_after
        );
        (*fail)++;
    }
}

fn void scheduler_expect_thread_batch_result_oom_drains_os_threads(
    Interp* interp,
    int* pass,
    int* fail
) {
    scheduler_init();
    usz threads_before = scheduler_test_count_active_os_threads();
    int saved_force = g_scheduler_force_thread_spawn_batch_results_alloc_fail_nth;
    g_scheduler_force_thread_spawn_batch_results_alloc_fail_nth = 1;
    EvalResult r = run("(__raw-thread-spawn-batch '((sleep-ms 1) (sleep-ms 1)))", interp);
    g_scheduler_force_thread_spawn_batch_results_alloc_fail_nth = saved_force;
    usz threads_after = scheduler_test_count_active_os_threads();
    bool ok = r.error.has_error &&
              str_starts_with(r.error.message[:256], "thread-spawn-batch: out of memory") &&
              threads_before == threads_after;

    if (ok) {
        emit_pass_literal("[PASS] thread-spawn batch result scratch OOM drains OS work");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] thread-spawn batch result scratch OOM drains OS work (error=%s threads_before=%d threads_after=%d)",
            r.error.has_error ? (ZString)r.error.message[:256] : (ZString)"<none>",
            (int)threads_before,
            (int)threads_after
        );
        (*fail)++;
    }
}

fn void run_scheduler_offload_batch_primitive_tests(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    defer scheduler_test_cleanup_no_async(saved_async);

    EvalResult r = run(
        "(block (define v (__raw-offload-batch '((sleep-ms 1) (sleep-ms 1)))) "
        "       (and (pair? v) "
        "            (pair? (cdr v)) "
        "            (null? (cdr (cdr v))) "
        "            (null? (car v)) "
        "            (null? (car (cdr v)))))",
        interp
    );
    bool ok = !r.error.has_error &&
              is_symbol(r.value) &&
              r.value.sym_val == interp.sym_true;

    if (ok) {
        emit_pass_literal("[PASS] offload batch primitive");
        (*pass)++;
    } else {
        char[256] rendered = {};
        usz len = print_value_to_buf(r.value, &interp.symbols, &rendered, rendered.len);
        if (r.error.has_error) {
            if (len > 0) {
                io::printfn("[FAIL] offload batch primitive (error=%s, value=%s)",
                    (ZString)r.error.message[:256],
                    (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] offload batch primitive (error=%s, value=<unprintable>)",
                    (ZString)r.error.message[:256]);
            }
        } else {
            if (len > 0) {
                io::printfn("[FAIL] offload batch primitive (value=%s)", (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] offload batch primitive (value=<unprintable>)");
            }
        }
        (*fail)++;
    }

    test_error_contains(interp, "offload batch sleep-ms rejects trailing arg",
        "(__raw-offload-batch '((sleep-ms 1 'ignored)))",
        "offload sleep-ms: expected exactly 1 argument", pass, fail);
    test_error_contains(interp, "offload batch gzip rejects trailing arg",
        "(__raw-offload-batch '((gzip \"payload\" 'ignored)))",
        "offload compress: expected exactly 1 argument", pass, fail);
}

fn void run_scheduler_task_spawn_batch_primitive_tests(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    defer scheduler_test_cleanup_no_async(saved_async);

    EvalResult r = run(
        "(block (define hs (__raw-task-spawn-batch '((sleep-ms 1) (sleep-ms 1)))) "
        "       (and (pair? hs) "
        "            (pair? (cdr hs)) "
        "            (null? (cdr (cdr hs))) "
        "            (null? (task-join (car hs))) "
        "            (null? (task-join (car (cdr hs))))))",
        interp
    );
    bool ok = !r.error.has_error &&
              is_symbol(r.value) &&
              r.value.sym_val == interp.sym_true;

    if (ok) {
        emit_pass_literal("[PASS] task spawn batch primitive");
        (*pass)++;
    } else {
        char[256] rendered = {};
        usz len = print_value_to_buf(r.value, &interp.symbols, &rendered, rendered.len);
        if (r.error.has_error) {
            if (len > 0) {
                io::printfn("[FAIL] task spawn batch primitive (error=%s, value=%s)",
                    (ZString)r.error.message[:256],
                    (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] task spawn batch primitive (error=%s, value=<unprintable>)",
                    (ZString)r.error.message[:256]);
            }
        } else {
            if (len > 0) {
                io::printfn("[FAIL] task spawn batch primitive (value=%s)", (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] task spawn batch primitive (value=<unprintable>)");
            }
        }
        (*fail)++;
    }

    test_error_contains(interp, "task-spawn batch sleep-ms rejects trailing arg",
        "(__raw-task-spawn-batch '((sleep-ms 1 'ignored)))",
        "offload sleep-ms: expected exactly 1 argument", pass, fail);
    test_error_contains(interp, "task-spawn batch gzip rejects trailing arg",
        "(__raw-task-spawn-batch '((gzip \"payload\" 'ignored)))",
        "offload compress: expected exactly 1 argument", pass, fail);
    scheduler_expect_batch_error_releases_shared(
        interp,
        "task-spawn batch parse failure releases parsed shared payload",
        "(__raw-task-spawn-batch '((gzip \"payload\") (sleep-ms 1 'ignored)))",
        "offload sleep-ms: expected exactly 1 argument",
        pass,
        fail
    );

    int saved_task_ids_force = g_scheduler_force_task_spawn_batch_task_ids_alloc_fail_nth;
    g_scheduler_force_task_spawn_batch_task_ids_alloc_fail_nth = 1;
    scheduler_expect_batch_error_releases_shared(
        interp,
        "task-spawn batch task-id scratch OOM releases parsed shared payload",
        "(__raw-task-spawn-batch '((gzip \"payload\")))",
        "task-spawn-batch: out of memory",
        pass,
        fail
    );
    g_scheduler_force_task_spawn_batch_task_ids_alloc_fail_nth = saved_task_ids_force;

    int saved_generations_force = g_scheduler_force_task_spawn_batch_generations_alloc_fail_nth;
    g_scheduler_force_task_spawn_batch_generations_alloc_fail_nth = 1;
    scheduler_expect_batch_error_releases_shared(
        interp,
        "task-spawn batch generation scratch OOM releases parsed shared payload",
        "(__raw-task-spawn-batch '((gzip \"payload\")))",
        "task-spawn-batch: out of memory",
        pass,
        fail
    );
    g_scheduler_force_task_spawn_batch_generations_alloc_fail_nth = saved_generations_force;
}

fn void run_scheduler_thread_spawn_batch_primitive_tests(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    defer scheduler_test_cleanup_no_async(saved_async);

    EvalResult r = run(
        "(block (define v (__raw-thread-spawn-batch '((sleep-ms 1) (sleep-ms 1)))) "
        "       (and (pair? v) "
        "            (pair? (cdr v)) "
        "            (null? (cdr (cdr v))) "
        "            (null? (car v)) "
        "            (null? (car (cdr v)))))",
        interp
    );
    bool ok = !r.error.has_error &&
              is_symbol(r.value) &&
              r.value.sym_val == interp.sym_true;

    if (ok) {
        emit_pass_literal("[PASS] thread spawn batch primitive");
        (*pass)++;
    } else {
        char[256] rendered = {};
        usz len = print_value_to_buf(r.value, &interp.symbols, &rendered, rendered.len);
        if (r.error.has_error) {
            if (len > 0) {
                io::printfn("[FAIL] thread spawn batch primitive (error=%s, value=%s)",
                    (ZString)r.error.message[:256],
                    (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] thread spawn batch primitive (error=%s, value=<unprintable>)",
                    (ZString)r.error.message[:256]);
            }
        } else {
            if (len > 0) {
                io::printfn("[FAIL] thread spawn batch primitive (value=%s)", (ZString)rendered[:len]);
            } else {
                io::printfn("[FAIL] thread spawn batch primitive (value=<unprintable>)");
            }
        }
        (*fail)++;
    }

    test_error_contains(interp, "thread-spawn batch sleep-ms rejects trailing arg",
        "(__raw-thread-spawn-batch '((sleep-ms 1 'ignored)))",
        "offload sleep-ms: expected exactly 1 argument", pass, fail);
    test_error_contains(interp, "thread-spawn batch gzip rejects trailing arg",
        "(__raw-thread-spawn-batch '((gzip \"payload\" 'ignored)))",
        "offload compress: expected exactly 1 argument", pass, fail);
    scheduler_expect_batch_error_releases_shared(
        interp,
        "thread-spawn batch parse failure releases parsed shared payload",
        "(__raw-thread-spawn-batch '((gzip \"payload\") (sleep-ms 1 'ignored)))",
        "offload sleep-ms: expected exactly 1 argument",
        pass,
        fail
    );

    int saved_handles_force = g_scheduler_force_thread_spawn_batch_handles_alloc_fail_nth;
    g_scheduler_force_thread_spawn_batch_handles_alloc_fail_nth = 1;
    scheduler_expect_batch_error_releases_shared(
        interp,
        "thread-spawn batch handle scratch OOM releases parsed shared payload",
        "(__raw-thread-spawn-batch '((gzip \"payload\")))",
        "thread-spawn-batch: out of memory",
        pass,
        fail
    );
    g_scheduler_force_thread_spawn_batch_handles_alloc_fail_nth = saved_handles_force;
    scheduler_expect_thread_batch_result_oom_drains_os_threads(interp, pass, fail);
}

fn void run_scheduler_os_thread_work_admit_failure_release_tests(Interp* interp, int* pass, int* fail) {
    scheduler_init();
    bool ok = scheduler_shared_registry_ensure_ready();
    char[] failed_phase = ok ? "" : "registry-init";
    usz shared_slot = SHARED_REGISTRY_CAPACITY;
    uint shared_generation = 0;
    Value* err = null;

    if (ok) {
        scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);

        OffloadWork work = {};
        work.shared = scheduler_publish_shared("thread-admit-fail");
        if (work.shared == null) {
            ok = false;
            failed_phase = "publish";
        } else {
            shared_slot = work.shared.slot;
            shared_generation = work.shared.generation;
            err = scheduler_admit_os_thread_work(work, null, interp);

            SharedHandle probe = {};
            probe.kind = SHARED_HANDLE_BLOB;
            probe.slot = shared_slot;
            probe.generation = shared_generation;
            probe.payload = null;
            bool released = scheduler_shared_registry_resolve_blob(&probe) == null;
            ok = err != null && err.tag == ERROR && released;
            if (!ok) failed_phase = released ? "error" : "shared-release";
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] os thread work admission failure releases shared payload");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] os thread work admission failure releases shared payload (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_task_join_timeout_timer_start_failure_tests(Interp* interp, int* pass, int* fail) {
    scheduler_init();
    bool ok = g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null;
    char[] failed_phase = ok ? "" : "uv-loop";
    usz task_id = NO_TASK;
    uint generation = 0;
    int saved_force = g_scheduler_force_task_join_timeout_timer_start_fail_nth;

    if (ok) {
        task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_phase = "alloc";
        }
    }
    if (ok && (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0)) {
        ok = false;
        failed_phase = "generation";
    }

    Value* result = null;
    if (ok) {
        g_scheduler_force_task_join_timeout_timer_start_fail_nth = 1;
        result = scheduler_task_join_impl(task_id, generation, 25, null, interp);
        g_scheduler_force_task_join_timeout_timer_start_fail_nth = saved_force;

        bool task_still_active = scheduler_thread_task_exists(task_id, generation);
        ok = result != null &&
             result.tag == ERROR &&
             scheduler_test_str_contains(result.str_chars[:result.str_len], "task-join: uv_timer_start failed") &&
             task_still_active;
        if (!ok) failed_phase = task_still_active ? "error" : "task-state";
    } else {
        g_scheduler_force_task_join_timeout_timer_start_fail_nth = saved_force;
    }

    if (task_id != NO_TASK && scheduler_thread_task_exists(task_id, generation)) {
        scheduler_drop_thread_task(task_id, generation);
    }

    if (ok) {
        emit_pass_literal("[PASS] task join timeout timer-start failure closes timer once");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] task join timeout timer-start failure closes timer once (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_batch_result_list_alloc_failure_tests(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    defer scheduler_test_cleanup_no_async(saved_async);

    usz tasks_before = scheduler_test_count_active_thread_tasks();
    int saved_force = g_scheduler_force_result_list_cons_alloc_fail_nth;
    g_scheduler_force_result_list_cons_alloc_fail_nth = 1;
    EvalResult offload_r = run("(__raw-offload-batch '((sleep-ms 1) (sleep-ms 1)))", interp);
    g_scheduler_force_result_list_cons_alloc_fail_nth = 1;
    EvalResult task_r = run("(__raw-task-spawn-batch '((sleep-ms 1) (sleep-ms 1)))", interp);
    g_scheduler_force_result_list_cons_alloc_fail_nth = 1;
    EvalResult thread_r = run("(__raw-thread-spawn-batch '((sleep-ms 1) (sleep-ms 1)))", interp);
    g_scheduler_force_result_list_cons_alloc_fail_nth = saved_force;
    usz tasks_after = scheduler_test_count_active_thread_tasks();

    bool ok =
        offload_r.error.has_error &&
        str_starts_with(offload_r.error.message[:256], "offload: out of memory") &&
        task_r.error.has_error &&
        str_starts_with(task_r.error.message[:256], "task-spawn-batch: out of memory") &&
        thread_r.error.has_error &&
        str_starts_with(thread_r.error.message[:256], "thread-spawn-batch: out of memory") &&
        tasks_before == tasks_after;

    if (ok) {
        emit_pass_literal("[PASS] scheduler batch result list alloc failures fail closed");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler batch result list alloc failures fail closed (offload=%s task=%s thread=%s tasks_before=%d tasks_after=%d)",
            offload_r.error.has_error ? (ZString)offload_r.error.message[:256] : (ZString)"<none>",
            task_r.error.has_error ? (ZString)task_r.error.message[:256] : (ZString)"<none>",
            thread_r.error.has_error ? (ZString)thread_r.error.message[:256] : (ZString)"<none>",
            (int)tasks_before,
            (int)tasks_after
        );
        (*fail)++;
    }
}
