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
                    "(block "
                    "  (define st-t (task-spawn 'sleep-ms 1)) "
                    "  (task-join st-t) "
                    "  true)",
                    interp);
            case 1:
                r = run(
                    "(block "
                    "  (define st-f (spawn (lambda () (offload 'sleep-ms 1)))) "
                    "  (await st-f) "
                    "  true)",
                    interp);
            case 2:
                r = run(
                    "(block "
                    "  (spawn (lambda () 1)) "
                    "  (spawn (lambda () 2)) "
                    "  (run-fibers) "
                    "  true)",
                    interp);
            case 3:
                r = run(
                    "(block "
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

    scheduler_test_finish_step_phase(
        ok,
        failed_step,
        failed_phase,
        "[PASS] scheduler thread-task completion boundary restore",
        "[FAIL] scheduler thread-task completion boundary restore at step %d (%s)",
        pass,
        fail
    );
}
fn void run_scheduler_boundary_thread_task_waiter_and_cancel_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_thread_task_null_completion_fail_closed_boundary_tests(interp, pass, fail);
    run_scheduler_waiter_take_does_not_strand_blocked_fiber_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_cancel_boundary_tests(interp, pass, fail);
    run_scheduler_cancel_conversion_payload_boundary_tests(interp, pass, fail);
}
