module lisp;

import std::io;
import main;

fn void run_scheduler_boundary_stress_tests(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    usz failed_step = 0;
    for (usz i = 0; i < 24; i++) {
        EvalResult r;
        switch (i & 3) {
            case 0:
                r = run(
                    "(begin "
                    "  (define st-t (task-spawn 'sleep-ms 1)) "
                    "  (task-join st-t) "
                    "  true)",
                    interp);
            case 1:
                r = run(
                    "(begin "
                    "  (define st-f (spawn (lambda () (offload 'sleep-ms 1)))) "
                    "  (await st-f) "
                    "  true)",
                    interp);
            case 2:
                r = run(
                    "(begin "
                    "  (spawn (lambda () 1)) "
                    "  (spawn (lambda () 2)) "
                    "  (run-fibers) "
                    "  true)",
                    interp);
            case 3:
                r = run(
                    "(begin "
                    "  (define st-t1 (task-spawn 'sleep-ms 1)) "
                    "  (define st-t2 (task-spawn 'sleep-ms 1)) "
                    "  (task-join st-t1) "
                    "  (task-join st-t2) "
                    "  true)",
                    interp);
            default:
                r = eval_ok(make_nil(interp));
                break;
        }

        if (r.error.has_error ||
            r.value == null ||
            (r.value.tag == ERROR) ||
            (r.value.tag == NIL)) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler/thread boundary stress (deterministic)");
        (*pass)++;
    } else {
        io::printfn("[FAIL] scheduler/thread boundary stress (deterministic) at step %d", (int)failed_step);
        (*fail)++;
    }
}
fn void run_scheduler_thread_task_completion_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: thread-task completion ownership paths (complete/take/drop and
    // invalid-id completion cleanup) preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        usz task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc-task";
            break;
        }

        OffloadCompletion* completion = scheduler_make_task_cancel_completion();
        if (completion == null) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "alloc-completion";
            break;
        }

        scheduler_complete_thread_task(task_id, completion);
        bool exists_before_take = scheduler_thread_task_exists(task_id);
        OffloadCompletion* taken = scheduler_take_thread_task_completion(task_id);
        bool take_ok = exists_before_take &&
                       taken != null &&
                       !scheduler_thread_task_exists(task_id);
        if (taken != null) scheduler_free_offload_completion(taken);

        OffloadCompletion* invalid_completion = scheduler_make_task_cancel_completion();
        if (invalid_completion == null) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "alloc-invalid-completion";
            break;
        }
        scheduler_complete_thread_task(MAX_THREAD_TASKS + 7 + i, invalid_completion);

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!take_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!take_ok) {
                failed_phase = "take";
            } else {
                failed_phase = "boundary";
            }
            scheduler_drop_thread_task(task_id);
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler thread-task completion boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler thread-task completion boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_thread_task_cancel_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: thread-task cancel state transitions preserve deterministic
    // completion semantics and boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        usz task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            break;
        }

        bool already_done = false;
        bool cancel_ok = scheduler_cancel_thread_task(task_id, &already_done);
        bool begin_ok = !scheduler_try_begin_thread_task(task_id);
        OffloadCompletion* cancelled = scheduler_take_thread_task_completion(task_id);
        bool take_ok = cancelled != null &&
                       cancelled.kind == OFFLOAD_RES_ERROR &&
                       !scheduler_thread_task_exists(task_id);
        if (cancelled != null) scheduler_free_offload_completion(cancelled);

        usz done_task = scheduler_alloc_thread_task();
        bool done_path_ok = false;
        if (done_task != NO_TASK) {
            OffloadCompletion* done_completion = scheduler_make_task_cancel_completion();
            if (done_completion != null) {
                scheduler_complete_thread_task(done_task, done_completion);
                bool done_already = false;
                bool cancel_done_ok = scheduler_cancel_thread_task(done_task, &done_already);
                OffloadCompletion* done_taken = scheduler_take_thread_task_completion(done_task);
                done_path_ok = cancel_done_ok &&
                               done_already &&
                               done_taken != null &&
                               done_taken.kind == OFFLOAD_RES_ERROR &&
                               !scheduler_thread_task_exists(done_task);
                if (done_taken != null) scheduler_free_offload_completion(done_taken);
            }
            if (!done_path_ok) scheduler_drop_thread_task(done_task);
        }

        bool invalid_cancel_ok = !scheduler_cancel_thread_task(MAX_THREAD_TASKS + 17 + i, null);
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool cycle_ok = cancel_ok &&
                        !already_done &&
                        begin_ok &&
                        take_ok &&
                        done_path_ok &&
                        invalid_cancel_ok &&
                        state_ok;
        if (!cycle_ok) {
            ok = false;
            failed_step = i;
            if (!cancel_ok || already_done || !begin_ok || !take_ok) {
                failed_phase = "cancel-path";
            } else if (!done_path_ok) {
                failed_phase = "done-path";
            } else if (!invalid_cancel_ok) {
                failed_phase = "invalid-cancel";
            } else {
                failed_phase = "boundary";
            }
            scheduler_drop_thread_task(task_id);
            if (done_task != NO_TASK) scheduler_drop_thread_task(done_task);
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler thread-task cancel boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler thread-task cancel boundary restore at step %d (%s)",
            (int)failed_step,
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
            "(begin "
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
            "(begin "
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
