module lisp;

import std::io;

fn void run_scheduler_thread_task_null_completion_fail_closed_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: null completion delivery into thread-task completion path
    // must fail closed (error completion or drop) and restore boundary/runtime state.
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

        uint generation = 0;
        if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "generation";
            break;
        }

        bool started = scheduler_try_begin_thread_task(task_id, generation);
        scheduler_complete_thread_task(task_id, null, generation);
        int wait_state = scheduler_wait_thread_task_completion_state(task_id, generation, 0);
        bool resolved = false;
        if (wait_state == SCHEDULER_JOIN_WAIT_READY) {
            OffloadCompletion* completion = scheduler_take_thread_task_completion(task_id, generation);
            resolved = completion != null &&
                       completion.kind == OFFLOAD_RES_ERROR &&
                       !scheduler_thread_task_exists(task_id, generation);
            if (completion != null) scheduler_free_offload_completion(completion);
        } else if (wait_state == SCHEDULER_JOIN_WAIT_INVALID) {
            resolved = !scheduler_thread_task_exists(task_id, generation);
        }
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

        if (!started || !resolved || !state_ok) {
            ok = false;
            failed_step = i;
            if (!started) {
                failed_phase = "start";
            } else if (!resolved) {
                failed_phase = "resolve";
            } else {
                failed_phase = "boundary";
            }
            scheduler_drop_thread_task(task_id, generation);
            break;
        }
    }

    scheduler_test_finish_step_phase(
        ok,
        failed_step,
        failed_phase,
        "[PASS] scheduler thread-task null completion fail-closed boundary restore",
        "[FAIL] scheduler thread-task null completion fail-closed boundary restore at step %d (%s)",
        pass,
        fail
    );
}

fn void run_scheduler_os_thread_null_completion_fail_closed_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: null completion delivery into os-thread completion path must
    // fail closed (error completion or drop+wake) and restore boundary/runtime
    // state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    char[] failed_phase = "";

    scheduler_init();
    usz saved_fiber_count = g_scheduler.fiber_count;
    usz saved_current_fiber_id = g_scheduler.current_fiber_id;
    bool saved_running = g_scheduler.running;
    FiberEntry saved_fiber_1 = g_scheduler.fibers[1];
    uint saved_join_token_1 = g_scheduler.fiber_join_wait_token[1];
    uint saved_latched_os_thread_1 = g_scheduler.latched_os_thread_done_wake_epoch[1];
    int saved_force_fail = g_scheduler_force_transition_completion_alloc_fail_nth;

    if (g_scheduler.fiber_count < 2) g_scheduler.fiber_count = 2;
    g_scheduler.fibers[1] = {};
    g_scheduler.fibers[1].state = FIBER_BLOCKED;
    g_scheduler.fiber_join_wait_token[1] = 0;
    g_scheduler.latched_os_thread_done_wake_epoch[1] = 0;

    usz thread_id = scheduler_alloc_os_thread();
    uint generation = 0;
    if (thread_id == NO_OS_THREAD ||
        !scheduler_get_os_thread_generation(thread_id, &generation) ||
        generation == 0) {
        ok = false;
        failed_phase = "alloc";
    } else if (!scheduler_try_begin_os_thread(thread_id, generation)) {
        ok = false;
        failed_phase = "begin";
    } else {
        int wait_state = scheduler_register_os_thread_waiter(thread_id, generation, 1);
        if (wait_state != SCHEDULER_JOIN_WAIT_REGISTERED) {
            ok = false;
            failed_phase = "register";
        } else {
            g_scheduler_force_transition_completion_alloc_fail_nth = 1;
            scheduler_complete_os_thread(thread_id, generation, null);
            g_scheduler_force_transition_completion_alloc_fail_nth = saved_force_fail;
            drain_wakeups();

            bool waiter_woken = g_scheduler.fibers[1].state == FIBER_READY;
            bool token_cleared = g_scheduler.fiber_join_wait_token[1] == 0;
            bool dropped = !scheduler_os_thread_exists(thread_id, generation);
            int done_state = scheduler_wait_os_thread_completion_state(thread_id, generation, 0);
            bool invalid_after_drop = done_state == SCHEDULER_JOIN_WAIT_INVALID;
            bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

            ok = waiter_woken && token_cleared && dropped && invalid_after_drop && state_ok;
            if (!waiter_woken) {
                failed_phase = "waiter";
            } else if (!token_cleared) {
                failed_phase = "token";
            } else if (!dropped || !invalid_after_drop) {
                failed_phase = "drop";
            } else if (!state_ok) {
                failed_phase = "boundary";
            }
        }
    }

    g_scheduler_force_transition_completion_alloc_fail_nth = saved_force_fail;
    if (thread_id != NO_OS_THREAD && scheduler_os_thread_exists(thread_id, generation)) {
        scheduler_drop_os_thread(thread_id, generation);
    }
    g_scheduler.fiber_join_wait_token[1] = saved_join_token_1;
    g_scheduler.latched_os_thread_done_wake_epoch[1] = saved_latched_os_thread_1;
    g_scheduler.fibers[1] = saved_fiber_1;
    g_scheduler.running = saved_running;
    g_scheduler.current_fiber_id = saved_current_fiber_id;
    g_scheduler.fiber_count = saved_fiber_count;

    scheduler_test_finish_phase(
        ok,
        failed_phase,
        "[PASS] scheduler os-thread null completion fail-closed boundary restore",
        "[FAIL] scheduler os-thread null completion fail-closed boundary restore (%s)",
        pass,
        fail
    );

    run_scheduler_os_thread_cancel_start_handle_cleanup_failure_test(interp, pass, fail);
}

fn void scheduler_test_noop_thread_entry(void* ctx) {
    (void)ctx;
}

fn void run_scheduler_os_thread_cancel_start_handle_cleanup_failure_test(Interp* interp, int* pass, int* fail) {
    // Invariant: cancel-start completion allocation failure must drop any
    // joinable thread handle before the entry is cleared.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    char[] failed_phase = "";

    scheduler_init();
    usz saved_fiber_count = g_scheduler.fiber_count;
    usz saved_current_fiber_id = g_scheduler.current_fiber_id;
    bool saved_running = g_scheduler.running;
    uint saved_os_thread_generation_seed = g_scheduler.os_thread_generation_seed;
    int saved_force_fail = g_scheduler_force_transition_completion_alloc_fail_nth;

    usz thread_id = scheduler_alloc_os_thread();
    uint generation = 0;
    void* thread_handle = null;
    if (thread_id == NO_OS_THREAD ||
        !scheduler_get_os_thread_generation(thread_id, &generation) ||
        generation == 0) {
        ok = false;
        failed_phase = "alloc";
    } else {
        thread_handle = omni_uv_thread_new();
        if (thread_handle == null) {
            ok = false;
            failed_phase = "handle";
        } else if (omni_uv_thread_start(thread_handle, &scheduler_test_noop_thread_entry, null) != 0) {
            ok = false;
            failed_phase = "start";
            omni_uv_thread_free(thread_handle);
            thread_handle = null;
        } else {
            OsThreadEntry* entry = &g_scheduler.os_threads[thread_id];
            entry.cancel_requested = true;
            entry.thread_created = true;
            entry.thread_handle = thread_handle;

            g_scheduler_force_transition_completion_alloc_fail_nth = 1;
            bool began = scheduler_try_begin_os_thread(thread_id, generation);
            g_scheduler_force_transition_completion_alloc_fail_nth = saved_force_fail;

            bool dropped = !scheduler_os_thread_exists(thread_id, generation);
            bool handle_cleared = g_scheduler.os_threads[thread_id].thread_handle == null;
            bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
            ok = !began && dropped && handle_cleared && state_ok;
            if (!began) {
                failed_phase = "begin";
            } else if (!dropped || !handle_cleared) {
                failed_phase = "drop";
            } else if (!state_ok) {
                failed_phase = "boundary";
            }
        }
    }

    if (thread_id != NO_OS_THREAD && scheduler_os_thread_exists(thread_id, generation)) {
        scheduler_drop_os_thread(thread_id, generation);
    }
    g_scheduler_force_transition_completion_alloc_fail_nth = saved_force_fail;
    g_scheduler.running = saved_running;
    g_scheduler.current_fiber_id = saved_current_fiber_id;
    g_scheduler.fiber_count = saved_fiber_count;
    g_scheduler.os_thread_generation_seed = saved_os_thread_generation_seed;

    scheduler_test_finish_phase(
        ok,
        failed_phase,
        "[PASS] scheduler os-thread cancel-start handle cleanup failure",
        "[FAIL] scheduler os-thread cancel-start handle cleanup failure (%s)",
        pass,
        fail
    );
}

fn void run_scheduler_waiter_take_does_not_strand_blocked_fiber_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: completion-take paths must not strand an already-registered
    // blocked waiter fiber when another consumer takes completion first.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    char[] failed_phase = "";

    scheduler_init();
    usz saved_fiber_count = g_scheduler.fiber_count;
    usz saved_current_fiber_id = g_scheduler.current_fiber_id;
    bool saved_running = g_scheduler.running;
    FiberEntry saved_fiber_0 = g_scheduler.fibers[0];
    FiberEntry saved_fiber_1 = g_scheduler.fibers[1];
    uint saved_join_token_1 = g_scheduler.fiber_join_wait_token[1];
    uint saved_latched_task_1 = g_scheduler.latched_task_done_wake_epoch[1];
    uint saved_latched_os_thread_1 = g_scheduler.latched_os_thread_done_wake_epoch[1];

    if (g_scheduler.fiber_count < 2) g_scheduler.fiber_count = 2;
    g_scheduler.fibers[1] = {};
    g_scheduler.fibers[1].state = FIBER_BLOCKED;
    g_scheduler.fiber_join_wait_token[1] = 0;
    g_scheduler.latched_task_done_wake_epoch[1] = 0;
    g_scheduler.latched_os_thread_done_wake_epoch[1] = 0;

    {
        usz task_id = scheduler_alloc_thread_task();
        uint generation = 0;
        OffloadCompletion* taken = null;
        if (task_id == NO_TASK ||
            !scheduler_get_thread_task_generation(task_id, &generation) ||
            generation == 0) {
            ok = false;
            failed_phase = "task-alloc";
            if (task_id != NO_TASK) scheduler_drop_thread_task(task_id);
        } else if (!scheduler_try_begin_thread_task(task_id, generation)) {
            ok = false;
            failed_phase = "task-begin";
            scheduler_drop_thread_task(task_id, generation);
        } else {
            int wait_state = scheduler_register_thread_task_waiter(task_id, generation, 1);
            OffloadCompletion* completion = scheduler_make_task_cancel_completion();
            if (wait_state != SCHEDULER_JOIN_WAIT_REGISTERED || completion == null) {
                ok = false;
                failed_phase = "task-register";
                if (completion != null) scheduler_free_offload_completion(completion);
                scheduler_drop_thread_task(task_id, generation);
            } else {
                scheduler_complete_thread_task(task_id, completion, generation);
                taken = scheduler_take_thread_task_completion(task_id, generation);
                drain_wakeups();
                bool waiter_woken = g_scheduler.fibers[1].state == FIBER_READY;
                bool token_cleared = g_scheduler.fiber_join_wait_token[1] == 0;
                if (taken == null || !waiter_woken || !token_cleared) {
                    ok = false;
                    failed_phase = "task-wake";
                }
                if (taken != null) scheduler_free_offload_completion(taken);
                if (scheduler_thread_task_exists(task_id, generation)) {
                    scheduler_drop_thread_task(task_id, generation);
                }
            }
        }
    }

    if (ok) {
        g_scheduler.fibers[1].state = FIBER_BLOCKED;
        g_scheduler.fiber_join_wait_token[1] = 0;
        g_scheduler.latched_task_done_wake_epoch[1] = 0;
        g_scheduler.latched_os_thread_done_wake_epoch[1] = 0;

        usz thread_id = scheduler_alloc_os_thread();
        uint generation = 0;
        OffloadCompletion* taken = null;
        if (thread_id == NO_OS_THREAD ||
            !scheduler_get_os_thread_generation(thread_id, &generation) ||
            generation == 0) {
            ok = false;
            failed_phase = "os-thread-alloc";
            if (thread_id != NO_OS_THREAD) scheduler_drop_os_thread(thread_id);
        } else {
            int wait_state = scheduler_register_os_thread_waiter(thread_id, generation, 1);
            OffloadCompletion* completion = scheduler_make_os_thread_cancel_completion();
            if (wait_state != SCHEDULER_JOIN_WAIT_REGISTERED || completion == null) {
                ok = false;
                failed_phase = "os-thread-register";
                if (completion != null) scheduler_free_offload_completion(completion);
                scheduler_drop_os_thread(thread_id, generation);
            } else {
                scheduler_complete_os_thread(thread_id, generation, completion);
                taken = scheduler_take_os_thread_completion(thread_id, generation);
                drain_wakeups();
                bool waiter_woken = g_scheduler.fibers[1].state == FIBER_READY;
                bool token_cleared = g_scheduler.fiber_join_wait_token[1] == 0;
                if (taken == null || !waiter_woken || !token_cleared) {
                    ok = false;
                    failed_phase = "os-thread-wake";
                }
                if (taken != null) scheduler_free_offload_completion(taken);
                if (scheduler_os_thread_exists(thread_id, generation)) {
                    scheduler_drop_os_thread(thread_id, generation);
                }
            }
        }
    }

    g_scheduler.fiber_join_wait_token[1] = saved_join_token_1;
    g_scheduler.latched_task_done_wake_epoch[1] = saved_latched_task_1;
    g_scheduler.latched_os_thread_done_wake_epoch[1] = saved_latched_os_thread_1;
    g_scheduler.fibers[0] = saved_fiber_0;
    g_scheduler.fibers[1] = saved_fiber_1;
    g_scheduler.running = saved_running;
    g_scheduler.current_fiber_id = saved_current_fiber_id;
    g_scheduler.fiber_count = saved_fiber_count;

    bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
    if (ok && !state_ok) {
        ok = false;
        failed_phase = "boundary";
    }

    scheduler_test_finish_phase(
        ok,
        failed_phase,
        "[PASS] scheduler completion-take does not strand blocked waiter",
        "[FAIL] scheduler completion-take does not strand blocked waiter (%s)",
        pass,
        fail
    );
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
        bool start_ok = !scheduler_try_begin_thread_task(task_id);
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
                        start_ok &&
                        take_ok &&
                        done_path_ok &&
                        invalid_cancel_ok &&
                        state_ok;
        if (!cycle_ok) {
            ok = false;
            failed_step = i;
            if (!cancel_ok || already_done || !start_ok || !take_ok) {
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

fn void run_scheduler_cancel_conversion_payload_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: late worker completions converted through cancellation paths
    // keep deterministic cancellation payloads through join-visible conversion.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    char[] task_cancel_message = "thread task cancelled";
    char[] os_thread_cancel_message = "thread cancelled";
    char[] worker_failure_fragment = "worker failed";
    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        usz task_id = NO_TASK;
        uint task_generation = 0;
        OffloadCompletion* task_taken = null;
        OffloadCompletion* task_late_completion = null;
        bool task_cycle_ok = true;

        task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK ||
            !scheduler_get_thread_task_generation(task_id, &task_generation) ||
            task_generation == 0) {
            task_cycle_ok = false;
            failed_phase = "task-alloc";
        } else if (!scheduler_try_begin_thread_task(task_id, task_generation)) {
            task_cycle_ok = false;
            failed_phase = "task-begin";
        } else {
            bool task_already_done = false;
            bool task_cancel_ok = scheduler_cancel_thread_task(task_id, &task_already_done, task_generation);
            task_late_completion = scheduler_make_task_alloc_failure_completion();
            if (!task_cancel_ok || task_already_done || task_late_completion == null) {
                task_cycle_ok = false;
                failed_phase = "task-cancel";
            } else {
                scheduler_complete_thread_task(task_id, task_late_completion, task_generation);
                task_late_completion = null;
                task_taken = scheduler_take_thread_task_completion(task_id, task_generation);
                Value* task_join_visible = task_taken != null
                    ? scheduler_value_from_offload_completion(task_taken, interp)
                    : null;
                bool task_completion_payload_ok =
                    task_taken != null &&
                    task_taken.kind == OFFLOAD_RES_ERROR &&
                    task_taken.error_msg != null &&
                    task_taken.error_len == task_cancel_message.len &&
                    scheduler_test_str_contains(task_taken.error_msg[:task_taken.error_len], task_cancel_message);
                bool task_join_payload_ok =
                    task_join_visible != null &&
                    task_join_visible.tag == ERROR &&
                    task_join_visible.str_len == task_cancel_message.len &&
                    scheduler_test_str_contains(task_join_visible.str_chars[:task_join_visible.str_len], task_cancel_message) &&
                    !scheduler_test_str_contains(task_join_visible.str_chars[:task_join_visible.str_len], worker_failure_fragment);
                task_cycle_ok = task_completion_payload_ok &&
                                task_join_payload_ok &&
                                !scheduler_thread_task_exists(task_id, task_generation);
                if (!task_cycle_ok) failed_phase = "task-payload";
            }
        }
        if (task_late_completion != null) scheduler_free_offload_completion(task_late_completion);
        if (task_taken != null) scheduler_free_offload_completion(task_taken);
        if (task_id != NO_TASK && scheduler_thread_task_exists(task_id, task_generation)) {
            scheduler_drop_thread_task(task_id, task_generation);
        }
        if (!task_cycle_ok) {
            ok = false;
            failed_step = i;
            break;
        }

        usz thread_id = NO_OS_THREAD;
        uint thread_generation = 0;
        OffloadCompletion* os_thread_taken = null;
        OffloadCompletion* os_thread_late_completion = null;
        bool os_thread_cycle_ok = true;

        thread_id = scheduler_alloc_os_thread();
        if (thread_id == NO_OS_THREAD ||
            !scheduler_get_os_thread_generation(thread_id, &thread_generation) ||
            thread_generation == 0) {
            os_thread_cycle_ok = false;
            failed_phase = "thread-alloc";
        } else if (!scheduler_try_begin_os_thread(thread_id, thread_generation)) {
            os_thread_cycle_ok = false;
            failed_phase = "thread-begin";
        } else {
            bool thread_already_done = false;
            bool thread_cancel_ok = scheduler_cancel_os_thread(thread_id, &thread_already_done, thread_generation);
            os_thread_late_completion = scheduler_make_os_thread_alloc_failure_completion();
            if (!thread_cancel_ok || thread_already_done || os_thread_late_completion == null) {
                os_thread_cycle_ok = false;
                failed_phase = "thread-cancel";
            } else {
                scheduler_complete_os_thread(thread_id, thread_generation, os_thread_late_completion);
                os_thread_late_completion = null;
                os_thread_taken = scheduler_take_os_thread_completion(thread_id, thread_generation);
                Value* thread_join_visible = os_thread_taken != null
                    ? scheduler_value_from_offload_completion(os_thread_taken, interp)
                    : null;
                bool thread_completion_payload_ok =
                    os_thread_taken != null &&
                    os_thread_taken.kind == OFFLOAD_RES_ERROR &&
                    os_thread_taken.error_msg != null &&
                    os_thread_taken.error_len == os_thread_cancel_message.len &&
                    scheduler_test_str_contains(
                        os_thread_taken.error_msg[:os_thread_taken.error_len],
                        os_thread_cancel_message
                    );
                bool thread_join_payload_ok =
                    thread_join_visible != null &&
                    thread_join_visible.tag == ERROR &&
                    thread_join_visible.str_len == os_thread_cancel_message.len &&
                    scheduler_test_str_contains(
                        thread_join_visible.str_chars[:thread_join_visible.str_len],
                        os_thread_cancel_message
                    ) &&
                    !scheduler_test_str_contains(
                        thread_join_visible.str_chars[:thread_join_visible.str_len],
                        worker_failure_fragment
                    );
                os_thread_cycle_ok = thread_completion_payload_ok &&
                                     thread_join_payload_ok &&
                                     !scheduler_os_thread_exists(thread_id, thread_generation);
                if (!os_thread_cycle_ok) failed_phase = "thread-payload";
            }
        }
        if (os_thread_late_completion != null) scheduler_free_offload_completion(os_thread_late_completion);
        if (os_thread_taken != null) scheduler_free_offload_completion(os_thread_taken);
        if (thread_id != NO_OS_THREAD && scheduler_os_thread_exists(thread_id, thread_generation)) {
            scheduler_drop_os_thread(thread_id, thread_generation);
        }
        if (!os_thread_cycle_ok) {
            ok = false;
            failed_step = i;
            break;
        }

        if (!scheduler_runtime_boundary_matches_snapshot(interp, &saved)) {
            ok = false;
            failed_step = i;
            failed_phase = "boundary";
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler cancel conversion payload stays cancellation-specific");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler cancel conversion payload stays cancellation-specific at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}
