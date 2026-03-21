module lisp;

import std::io;

fn void run_scheduler_join_wait_failure_mapping_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: join-wait state mapping for invalid and ready thread-task/
    // os-thread completions preserves runtime boundary state and returns the
    // expected scheduler join state codes.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    char[] failed_phase = "";

    for (usz i = 0; i < 12; i++) {
        int invalid_task_state = scheduler_wait_thread_task_completion_state(MAX_THREAD_TASKS + 11 + i, 0, 0);
        int invalid_os_state = scheduler_wait_os_thread_completion_state(MAX_OS_THREADS + 11 + i, 0, 0);
        if (invalid_task_state != SCHEDULER_JOIN_WAIT_INVALID ||
            invalid_os_state != SCHEDULER_JOIN_WAIT_INVALID) {
            ok = false;
            failed_phase = "invalid";
            break;
        }

        usz task_id = scheduler_alloc_thread_task();
        uint task_generation = 0;
        if (task_id == NO_TASK ||
            !scheduler_get_thread_task_generation(task_id, &task_generation) ||
            task_generation == 0 ||
            !scheduler_try_begin_thread_task(task_id, task_generation)) {
            ok = false;
            failed_phase = "task-alloc";
            if (task_id != NO_TASK) scheduler_drop_thread_task(task_id);
            break;
        }
        OffloadCompletion* task_completion = scheduler_make_task_cancel_completion();
        if (task_completion == null) {
            ok = false;
            failed_phase = "task-completion";
            scheduler_drop_thread_task(task_id, task_generation);
            break;
        }
        scheduler_complete_thread_task(task_id, task_completion, task_generation);
        int task_ready_state = scheduler_wait_thread_task_completion_state(task_id, task_generation, 0);
        OffloadCompletion* task_taken = null;
        bool task_ready_ok = task_ready_state == SCHEDULER_JOIN_WAIT_READY;
        if (task_ready_ok) {
            task_taken = scheduler_take_thread_task_completion(task_id, task_generation);
            task_ready_ok = task_taken != null &&
                            task_taken.kind == OFFLOAD_RES_ERROR &&
                            !scheduler_thread_task_exists(task_id, task_generation);
        }
        if (task_taken != null) scheduler_free_offload_completion(task_taken);
        if (!task_ready_ok) {
            ok = false;
            failed_phase = "task-ready";
            if (scheduler_thread_task_exists(task_id, task_generation)) scheduler_drop_thread_task(task_id, task_generation);
            break;
        }

        usz thread_id = scheduler_alloc_os_thread();
        uint thread_generation = 0;
        if (thread_id == NO_OS_THREAD ||
            !scheduler_get_os_thread_generation(thread_id, &thread_generation) ||
            thread_generation == 0 ||
            !scheduler_try_begin_os_thread(thread_id, thread_generation)) {
            ok = false;
            failed_phase = "os-thread-alloc";
            if (thread_id != NO_OS_THREAD) scheduler_drop_os_thread(thread_id);
            break;
        }
        OffloadCompletion* thread_completion = scheduler_make_os_thread_cancel_completion();
        if (thread_completion == null) {
            ok = false;
            failed_phase = "os-thread-completion";
            scheduler_drop_os_thread(thread_id, thread_generation);
            break;
        }
        scheduler_complete_os_thread(thread_id, thread_generation, thread_completion);
        int thread_ready_state = scheduler_wait_os_thread_completion_state(thread_id, thread_generation, 0);
        OffloadCompletion* thread_taken = null;
        bool thread_ready_ok = thread_ready_state == SCHEDULER_JOIN_WAIT_READY;
        if (thread_ready_ok) {
            thread_taken = scheduler_take_os_thread_completion(thread_id, thread_generation);
            thread_ready_ok = thread_taken != null &&
                              thread_taken.kind == OFFLOAD_RES_ERROR &&
                              !scheduler_os_thread_exists(thread_id, thread_generation);
        }
        if (thread_taken != null) scheduler_free_offload_completion(thread_taken);
        if (!thread_ready_ok) {
            ok = false;
            failed_phase = "os-thread-ready";
            if (scheduler_os_thread_exists(thread_id, thread_generation)) scheduler_drop_os_thread(thread_id, thread_generation);
            break;
        }

        if (!scheduler_runtime_boundary_matches_snapshot(interp, &saved)) {
            ok = false;
            failed_phase = "boundary";
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler join-wait failure mapping boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler join-wait failure mapping boundary restore (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_mixed_boundary_state_restore_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: mixed scheduler thread/offload paths (success + expected error)
    // preserve interpreter boundary/runtime state across iterations.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] fail_phase = "";
    bool d_current_scope = true;
    bool d_releasing_scope = true;
    bool d_jit_env = true;
    bool d_match_env = true;
    bool d_jit_tco_expr = true;
    bool d_jit_tco_env = true;
    bool d_tco_recycle = true;
    bool d_tco_slot = true;
    bool d_tco_active = true;
    bool d_escape_mode = true;
    bool d_active_ctx = true;
    for (usz i = 0; i < 12; i++) {
        EvalResult success_r = run(
            "(block "
            "  (define sb-t (task-spawn 'sleep-ms 1)) "
            "  (define sb-f (spawn (lambda () (offload 'sleep-ms 1)))) "
            "  (task-join sb-t) "
            "  (await sb-f) "
            "  true)",
            interp);
        bool success_ok = !success_r.error.has_error &&
                          success_r.value != null &&
                          success_r.value.tag != ERROR &&
                          success_r.value.tag != NIL;
        bool success_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!success_ok || !success_state_ok) {
            ok = false;
            failed_step = i * 2;
            fail_phase = "success";
            d_current_scope = interp.current_scope == saved.current_scope;
            d_releasing_scope = interp.releasing_scope == saved.releasing_scope;
            d_jit_env = interp.jit_env == saved.jit_env;
            d_match_env = interp.match_env == saved.match_env;
            d_jit_tco_expr = interp.jit_tco_expr == saved.jit_tco_expr;
            d_jit_tco_env = interp.jit_tco_env == saved.jit_tco_env;
            d_tco_recycle = interp.tco_recycle_scope == saved.tco_recycle_scope;
            d_tco_slot = interp.tco_scope_defer_slot == saved.tco_scope_defer_slot;
            d_tco_active = interp.tco_scope_defer_active == saved.tco_scope_defer_active;
            d_escape_mode = interp.escape_env_mode == saved.escape_env_mode;
            d_active_ctx = interp.active_promotion_ctx == saved.active_promotion_ctx;
            break;
        }

        // Expected-error path should also preserve boundary/runtime fields.
        EvalResult error_r = run(
            "(block "
            "  (define sb-c (task-spawn 'sleep-ms 20)) "
            "  (task-cancel sb-c) "
            "  (task-join sb-c))",
            interp);
        bool error_ok = error_r.error.has_error ||
                        (error_r.value != null && error_r.value.tag == ERROR);
        bool error_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!error_ok || !error_state_ok) {
            ok = false;
            failed_step = i * 2 + 1;
            fail_phase = "error";
            d_current_scope = interp.current_scope == saved.current_scope;
            d_releasing_scope = interp.releasing_scope == saved.releasing_scope;
            d_jit_env = interp.jit_env == saved.jit_env;
            d_match_env = interp.match_env == saved.match_env;
            d_jit_tco_expr = interp.jit_tco_expr == saved.jit_tco_expr;
            d_jit_tco_env = interp.jit_tco_env == saved.jit_tco_env;
            d_tco_recycle = interp.tco_recycle_scope == saved.tco_recycle_scope;
            d_tco_slot = interp.tco_scope_defer_slot == saved.tco_scope_defer_slot;
            d_tco_active = interp.tco_scope_defer_active == saved.tco_scope_defer_active;
            d_escape_mode = interp.escape_env_mode == saved.escape_env_mode;
            d_active_ctx = interp.active_promotion_ctx == saved.active_promotion_ctx;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler mixed boundary/runtime state restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler mixed boundary/runtime state restore at step %d (%s) [scope=%s releasing=%s jit_env=%s match_env=%s tco_expr=%s tco_env=%s tco_recycle=%s tco_slot=%s tco_active=%s escape=%s active_ctx=%s]",
            (int)failed_step,
            (ZString)fail_phase,
            d_current_scope ? "ok" : "drift",
            d_releasing_scope ? "ok" : "drift",
            d_jit_env ? "ok" : "drift",
            d_match_env ? "ok" : "drift",
            d_jit_tco_expr ? "ok" : "drift",
            d_jit_tco_env ? "ok" : "drift",
            d_tco_recycle ? "ok" : "drift",
            d_tco_slot ? "ok" : "drift",
            d_tco_active ? "ok" : "drift",
            d_escape_mode ? "ok" : "drift",
            d_active_ctx ? "ok" : "drift"
        );
        (*fail)++;
    }
}
