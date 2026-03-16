module lisp;

import std::core::mem;
import std::io;

fn void scheduler_test_pump_wakeups() @inline {
    if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
        (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
    }
    drain_wakeups();
}

fn bool scheduler_test_enqueue_offload_with_retry(OffloadWork work, usz max_spins = 512) {
    for (usz spin = 0; spin < max_spins; spin++) {
        if (scheduler_enqueue_offload(work)) return true;
        if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
            (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
        }
        (void)scheduler_wait_thread_task_waiters(1);
    }
    return false;
}

fn void run_scheduler_offload_worker_retry_full_wakeup_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: offload worker completion remains lossless while the reliable
    // wakeup queue already has heavy backlog, then succeeds after drain without
    // mutating interpreter boundary/runtime fields.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 8; i++) {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_offload(&saved_async);

        usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        usz pushed_fill = 0;
        for (usz j = 0; j < 512; j++) {
            if (scheduler_publish_poll_error(MAX_FIBERS + i * 512 + j + 1, -1)) {
                pushed_fill++;
            }
        }
        if (pushed_fill != 512) {
            ok = false;
            failed_step = i;
            failed_phase = "prefill";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        OffloadWork work = {};
        work.fiber_id = 0;
        work.task_id = NO_TASK;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 0;
        work.shared = null;
        bool queued = scheduler_enqueue_offload(work);
        if (!queued) {
            ok = false;
            failed_step = i;
            failed_phase = "enqueue-offload";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        // Release the backlog and wait until worker completion is delivered to
        // the active pending-offload slot, then consume it to avoid leaks.
        scheduler_test_pump_wakeups();
        bool completion_ready = false;
        for (usz spin = 0; spin < 160; spin++) {
            c_usleep(1000);
            scheduler_test_pump_wakeups();
            if (g_scheduler.pending_offloads[0].active &&
                g_scheduler.pending_offloads[0].completed &&
                g_scheduler.pending_offloads[0].completion != null &&
                g_scheduler.fibers[0].state == FIBER_READY) {
                completion_ready = true;
                break;
            }
        }

        bool consume_ok = false;
        if (completion_ready) {
            Value* consumed = scheduler_consume_pending_offload(0, interp);
            consume_ok = consumed != null && consumed.tag != ERROR;
        }

        // Defensive final drain in case a producer raced after consume.
        scheduler_test_pump_wakeups();
        c_usleep(1000);
        scheduler_test_pump_wakeups();

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool queue_empty = reliable_wakeup_pending_count() == 0;
        bool slot_cleared = !g_scheduler.pending_offloads[0].active &&
                            !g_scheduler.pending_offloads[0].completed &&
                            g_scheduler.pending_offloads[0].completion == null;
        usz failure_delta = g_scheduler.wakeup_enqueue_failures.load() - failures_before;
        usz enqueue_delta = g_scheduler.reliable_wakeup_enqueues.load() - enqueues_before;
        // Global scheduler state is shared across isolated test groups, so
        // unrelated in-flight async completions may legitimately add extra
        // reliable-wakeup enqueues beyond this cycle's expected +1 offload
        // completion signal.
        bool enqueue_ok = failure_delta == 0 && enqueue_delta >= pushed_fill + 1;
        bool cycle_ok = completion_ready && consume_ok && queue_empty && slot_cleared && enqueue_ok && state_ok;
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
        if (!cycle_ok) {
            ok = false;
            failed_step = i;
            if (!enqueue_ok) {
                failed_phase = "enqueue";
            } else if (!completion_ready || !consume_ok || !slot_cleared) {
                failed_phase = "completion";
            } else if (!queue_empty) {
                failed_phase = "queue";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler offload worker reliable queue backlog boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler offload worker reliable queue backlog boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_offload_ready_barrier_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: direct reliable offload delivery plus unrelated invalid events
    // still preserve boundary/runtime state and consume cleanly.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_offload(&saved_async);

    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) {
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
        io::printn("[FAIL] scheduler wakeup offload-ready barrier boundary restore (alloc)");
        (*fail)++;
        return;
    }
    *completion = {};
    completion.kind = OFFLOAD_RES_NIL;

    bool pushed_invalid = scheduler_publish_poll_error(MAX_FIBERS + 222, -13);
    bool pushed_ready = scheduler_publish_offload_ready(0, completion);
    drain_wakeups();
    bool delivered_ok = pushed_invalid &&
                        pushed_ready &&
                        reliable_wakeup_pending_count() == 0 &&
                        g_scheduler.pending_offloads[0].active &&
                        g_scheduler.pending_offloads[0].completed &&
                        g_scheduler.pending_offloads[0].completion == completion &&
                        g_scheduler.fibers[0].state == FIBER_READY;
    bool state_ok_second = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

    Value* consumed = scheduler_consume_pending_offload(0, interp);
    bool consume_ok = consumed != null &&
                      consumed.tag == NIL &&
                      !g_scheduler.pending_offloads[0].active &&
                      !g_scheduler.pending_offloads[0].completed &&
                      g_scheduler.pending_offloads[0].completion == null;
    bool state_ok_third = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

    bool ok = delivered_ok && consume_ok && state_ok_second && state_ok_third;

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] scheduler reliable offload-ready boundary restore");
        (*pass)++;
    } else {
        io::printn("[FAIL] scheduler reliable offload-ready boundary restore");
        (*fail)++;
    }
}

fn void run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: real worker enqueue + immediate task cancel converges to one
    // cancel completion and preserves interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
            (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
        }

        usz task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            break;
        }

        OffloadWork work = {};
        work.fiber_id = NO_FIBER;
        work.task_id = task_id;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 10;
        work.shared = null;
        bool enqueued = scheduler_test_enqueue_offload_with_retry(work);
        if (!enqueued) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "enqueue";
            break;
        }

        bool already_done = false;
        bool cancel_ok = scheduler_cancel_thread_task(task_id, &already_done);
        bool cancel_state_ok = cancel_ok && !already_done;
        if (!cancel_state_ok) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "cancel";
            break;
        }

        OffloadCompletion* completion = null;
        for (usz spin = 0; spin < 240; spin++) {
            if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
                (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
            }
            completion = scheduler_take_thread_task_completion(task_id);
            if (completion != null) break;
            (void)scheduler_wait_thread_task_waiters(1);
        }

        bool completion_ok = completion != null &&
                             completion.kind == OFFLOAD_RES_ERROR &&
                             !scheduler_thread_task_exists(task_id);
        if (completion != null) {
            scheduler_free_offload_completion(completion);
        } else {
            scheduler_drop_thread_task(task_id);
        }

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool cycle_ok = completion_ok && state_ok;
        if (!cycle_ok) {
            ok = false;
            failed_step = i;
            if (!completion_ok) {
                failed_phase = "completion";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler worker cancel interleave boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler worker cancel interleave boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_thread_task_nonexecuted_payload_release_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: if libuv after-work fires without executing work_cb (for
    // example cancelled while queued), queued shared payload is still released
    // and the task converges to one terminal completion.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    char[] failed_phase = "";

    if (!scheduler_shared_registry_ensure_ready()) {
        ok = false;
        failed_phase = "registry-init";
    }

    if (ok) {
        scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
    }

    usz task_id = NO_TASK;
    uint generation = 0;
    void* req = null;
    usz shared_slot = SHARED_REGISTRY_CAPACITY;
    uint shared_generation = 0;
    if (ok) {
        task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_phase = "task-alloc";
        }
    }

    if (ok) {
        req = omni_uv_work_req_new();
        if (req == null) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "req-alloc";
        }
    }

    if (ok) {
        OffloadWork work = {};
        work.fiber_id = NO_FIBER;
        work.task_id = task_id;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 5;
        work.shared = scheduler_publish_shared("cancel-before-work");
        if (work.shared == null) {
            omni_uv_work_req_free(req);
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "shared-publish";
        } else if (!scheduler_bind_thread_task_request(task_id, req, &generation) || generation == 0) {
            scheduler_release_shared(work.shared);
            omni_uv_work_req_free(req);
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "bind-request";
        } else {
            shared_slot = work.shared.slot;
            shared_generation = work.shared.generation;
            QueuedOffloadWork* queued = (QueuedOffloadWork*)mem::malloc(QueuedOffloadWork.sizeof);
            if (queued == null) {
                scheduler_release_shared(work.shared);
                scheduler_clear_thread_task_request(task_id, req, generation);
                omni_uv_work_req_free(req);
                scheduler_drop_thread_task(task_id, generation);
                ok = false;
                failed_phase = "queued-alloc";
            } else {
                *queued = {};
                queued.work = work;
                queued.task_generation = generation;
                queued.executed = false;
                queued.completion = null;
                omni_uv_req_set_data(req, queued);
                scheduler_uv_offload_after_cb(req, -1);
            }
        }
    }

    if (ok) {
        OffloadCompletion* completion = scheduler_take_thread_task_completion(task_id, generation);
        bool completion_ok = completion != null &&
                             completion.kind == OFFLOAD_RES_ERROR &&
                             !scheduler_thread_task_exists(task_id, generation);
        if (completion != null) scheduler_free_offload_completion(completion);

        SharedHandle probe = {};
        probe.kind = SHARED_HANDLE_BLOB;
        probe.slot = shared_slot;
        probe.generation = shared_generation;
        probe.payload = null;
        bool shared_released = scheduler_shared_registry_resolve_blob(&probe) == null;

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        ok = completion_ok && shared_released && state_ok;
        if (!ok) {
            if (!completion_ok) {
                failed_phase = "completion";
            } else if (!shared_released) {
                failed_phase = "shared-release";
            } else {
                failed_phase = "boundary";
            }
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler non-executed worker cancel releases queued payload");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler non-executed worker cancel releases queued payload (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_worker_prestart_cancel_single_shared_release_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: when task begin is rejected before execution, shared payload
    // ownership is released exactly once across worker + after-work callbacks.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    char[] failed_phase = "";

    if (!scheduler_shared_registry_ensure_ready()) {
        ok = false;
        failed_phase = "registry-init";
    }
    if (ok) {
        scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
    }

    usz task_id = NO_TASK;
    uint generation = 0;
    void* req = null;
    usz shared_slot = SHARED_REGISTRY_CAPACITY;
    uint shared_generation = 0;
    if (ok) {
        task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_phase = "task-alloc";
        }
    }

    if (ok) {
        req = omni_uv_work_req_new();
        if (req == null) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "req-alloc";
        }
    }

    if (ok) {
        OffloadWork work = {};
        work.fiber_id = NO_FIBER;
        work.task_id = task_id;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 5;
        work.shared = scheduler_publish_shared("prestart-cancel");
        if (work.shared == null) {
            omni_uv_work_req_free(req);
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "shared-publish";
        } else if (!scheduler_bind_thread_task_request(task_id, req, &generation) || generation == 0) {
            scheduler_release_shared(work.shared);
            omni_uv_work_req_free(req);
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "bind-request";
        } else {
            shared_slot = work.shared.slot;
            shared_generation = work.shared.generation;
            QueuedOffloadWork* queued = (QueuedOffloadWork*)mem::malloc(QueuedOffloadWork.sizeof);
            if (queued == null) {
                scheduler_release_shared(work.shared);
                scheduler_clear_thread_task_request(task_id, req, generation);
                omni_uv_work_req_free(req);
                scheduler_drop_thread_task(task_id, generation);
                ok = false;
                failed_phase = "queued-alloc";
            } else {
                *queued = {};
                queued.work = work;
                queued.task_generation = generation;
                queued.executed = false;
                queued.completion = null;
                omni_uv_req_set_data(req, queued);

                bool cancel_ok = scheduler_cancel_thread_task(task_id, null, generation);
                if (!cancel_ok) {
                    scheduler_release_shared(work.shared);
                    mem::free(queued);
                    scheduler_clear_thread_task_request(task_id, req, generation);
                    omni_uv_work_req_free(req);
                    scheduler_drop_thread_task(task_id, generation);
                    ok = false;
                    failed_phase = "cancel";
                } else {
                    scheduler_uv_offload_work_cb(req);
                    scheduler_uv_offload_after_cb(req, -1);
                }
            }
        }
    }

    if (ok) {
        OffloadCompletion* completion = scheduler_take_thread_task_completion(task_id, generation);
        bool completion_ok = completion != null &&
                             completion.kind == OFFLOAD_RES_ERROR &&
                             !scheduler_thread_task_exists(task_id, generation);
        if (completion != null) scheduler_free_offload_completion(completion);

        SharedHandle probe = {};
        probe.kind = SHARED_HANDLE_BLOB;
        probe.slot = shared_slot;
        probe.generation = shared_generation;
        probe.payload = null;
        bool shared_released = scheduler_shared_registry_resolve_blob(&probe) == null;

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        ok = completion_ok && shared_released && state_ok;
        if (!ok) {
            if (!completion_ok) {
                failed_phase = "completion";
            } else if (!shared_released) {
                failed_phase = "shared-release";
            } else {
                failed_phase = "boundary";
            }
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler prestart-cancel shared release remains single-owner");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler prestart-cancel shared release remains single-owner (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_dns_connect_request_token_guard_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: async DNS/connect callbacks must not rebind into an unrelated
    // pending slot just because req pointers alias.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    char[] failed_phase = "";

    scheduler_init();
    usz saved_fiber_count = g_scheduler.fiber_count;
    usz saved_current_fiber_id = g_scheduler.current_fiber_id;
    bool saved_running = g_scheduler.running;
    FiberEntry saved_fiber_0 = g_scheduler.fibers[0];
    FiberEntry saved_fiber_1 = g_scheduler.fibers[1];
    PendingDnsResolve saved_dns_1 = g_scheduler.pending_dns[1];
    PendingTcpConnect saved_connect_1 = g_scheduler.pending_connects[1];

    if (g_scheduler.fiber_count < 2) {
        g_scheduler.fiber_count = 2;
    }
    g_scheduler.fibers[0] = {};
    g_scheduler.fibers[1] = {};
    g_scheduler.fibers[0].state = FIBER_BLOCKED;
    g_scheduler.fibers[1].state = FIBER_BLOCKED;

    {
        void* dns_req = omni_uv_getaddrinfo_req_new();
        char* dns_host = copy_to_heap_c_string("stale");
        PendingDnsCallbackCtx* dns_ctx = null;
        if (dns_req == null || dns_host == null) {
            if (dns_req != null) omni_uv_getaddrinfo_req_free(dns_req);
            if (dns_host != null) mem::free(dns_host);
            ok = false;
            failed_phase = "dns-alloc";
        } else {
            dns_ctx = (PendingDnsCallbackCtx*)mem::malloc(PendingDnsCallbackCtx.sizeof);
            if (dns_ctx == null) {
                omni_uv_getaddrinfo_req_free(dns_req);
                mem::free(dns_host);
                ok = false;
                failed_phase = "dns-ctx";
            } else {
                *dns_ctx = {};
                dns_ctx.fiber_id = 0;
                dns_ctx.request_token = 701;
                dns_ctx.host_copy = dns_host;
                omni_uv_req_set_data(dns_req, dns_ctx);
                g_scheduler.pending_dns[1] = {};
                g_scheduler.pending_dns[1].active = true;
                g_scheduler.pending_dns[1].completed = false;
                g_scheduler.pending_dns[1].status = 0;
                g_scheduler.pending_dns[1].request_token = 702;
                g_scheduler.pending_dns[1].req_handle = dns_req;
                scheduler_uv_getaddrinfo_cb(dns_req, 0, null);
                bool dns_guard_ok = g_scheduler.pending_dns[1].active &&
                                    !g_scheduler.pending_dns[1].completed &&
                                    g_scheduler.pending_dns[1].status == 0 &&
                                    g_scheduler.pending_dns[1].request_token == 702;
                g_scheduler.pending_dns[1] = {};
                if (!dns_guard_ok) {
                    ok = false;
                    failed_phase = "dns-guard";
                }
            }
        }
    }

    if (ok) {
        void* connect_req = omni_uv_work_req_new();
        PendingConnectCallbackCtx* connect_ctx = null;
        if (connect_req == null) {
            ok = false;
            failed_phase = "connect-alloc";
        } else {
            connect_ctx = (PendingConnectCallbackCtx*)mem::malloc(PendingConnectCallbackCtx.sizeof);
            if (connect_ctx == null) {
                omni_uv_work_req_free(connect_req);
                ok = false;
                failed_phase = "connect-ctx";
            } else {
                *connect_ctx = {};
                connect_ctx.fiber_id = 0;
                connect_ctx.request_token = 901;
                connect_ctx.tcp_handle = null;
                omni_uv_req_set_data(connect_req, connect_ctx);
                g_scheduler.pending_connects[1] = {};
                g_scheduler.pending_connects[1].active = true;
                g_scheduler.pending_connects[1].completed = false;
                g_scheduler.pending_connects[1].status = 0;
                g_scheduler.pending_connects[1].request_token = 902;
                g_scheduler.pending_connects[1].req_handle = connect_req;
                scheduler_uv_tcp_connect_cb(connect_req, 0);
                bool connect_guard_ok = g_scheduler.pending_connects[1].active &&
                                        !g_scheduler.pending_connects[1].completed &&
                                        g_scheduler.pending_connects[1].status == 0 &&
                                        g_scheduler.pending_connects[1].request_token == 902;
                g_scheduler.pending_connects[1] = {};
                if (!connect_guard_ok) {
                    ok = false;
                    failed_phase = "connect-guard";
                }
            }
        }
    }

    g_scheduler.pending_dns[1] = saved_dns_1;
    g_scheduler.pending_connects[1] = saved_connect_1;
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

    if (ok) {
        emit_pass_literal("[PASS] scheduler async callback token guards reject req-pointer rebinding");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler async callback token guards reject req-pointer rebinding (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_task_join_timeout_then_join_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: timeout join does not consume task completion; a later join can
    // complete successfully and clear task entry, with boundary/runtime state preserved.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 12; i++) {
        if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
            (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
        }

        usz task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
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

        OffloadWork work = {};
        work.fiber_id = NO_FIBER;
        work.task_id = task_id;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 75;
        work.shared = null;
        bool enqueued = scheduler_test_enqueue_offload_with_retry(work);
        if (!enqueued) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "enqueue";
            break;
        }

        Value* timeout_v = scheduler_task_join_impl(task_id, generation, 0, null, interp);
        bool timeout_ok = timeout_v != null && timeout_v.tag == ERROR;
        bool exists_after_timeout = scheduler_thread_task_exists(task_id, generation);

        Value* join_v = scheduler_task_join_impl(task_id, generation, 250, null, interp);
        bool join_ok = join_v != null && join_v.tag != ERROR;
        bool gone_after_join = !scheduler_thread_task_exists(task_id, generation);

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool cycle_ok = timeout_ok &&
                        exists_after_timeout &&
                        join_ok &&
                        gone_after_join &&
                        state_ok;
        if (!cycle_ok) {
            scheduler_drop_thread_task(task_id, generation);
            ok = false;
            failed_step = i;
            if (!timeout_ok || !exists_after_timeout) {
                failed_phase = "timeout";
            } else if (!join_ok || !gone_after_join) {
                failed_phase = "join";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler timeout-then-join boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler timeout-then-join boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_shared_retire_queue_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: shared-handle retire queue enqueues/releases deterministically,
    // overflow path falls back to immediate free, and domain safe points drain retired payloads.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    char[] failed_phase = "";

    if (!scheduler_shared_registry_ensure_ready()) {
        ok = false;
        failed_phase = "registry-init";
    }

    SharedRegistry* registry = &g_scheduler.shared_registry;
    usz overflow_before = 0;
    if (ok) {
        scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
        overflow_before = registry.retire_overflow_count;
    }

    if (ok) {
        SharedHandle* first = scheduler_publish_shared("phase7");
        if (first == null) {
            ok = false;
            failed_phase = "publish-initial";
        } else {
            scheduler_release_shared(first);
            if (registry.retire_count == 0) {
                ok = false;
                failed_phase = "enqueue-initial";
            }
        }
    }

    if (ok) {
        scheduler_domain_safe_point(scheduler_primary_domain());
        if (registry.retire_count != 0) {
            ok = false;
            failed_phase = "drain-initial";
        }
    }

    if (ok) {
        usz stress_total = SHARED_RETIRED_QUEUE_CAPACITY + 8;
        for (usz i = 0; i < stress_total; i++) {
            SharedHandle* h = scheduler_publish_shared("q");
            if (h == null) {
                ok = false;
                failed_phase = "publish-stress";
                break;
            }
            scheduler_release_shared(h);
        }
    }

    if (ok) {
        bool queue_full = registry.retire_count == SHARED_RETIRED_QUEUE_CAPACITY;
        bool overflow_recorded = registry.retire_overflow_count >= overflow_before + 8;
        if (!queue_full || !overflow_recorded) {
            ok = false;
            failed_phase = "overflow-accounting";
        }
    }

    if (ok) {
        scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
        if (registry.retire_count != 0) {
            ok = false;
            failed_phase = "drain-final";
        }
    }

    bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
    if (!state_ok && ok) {
        ok = false;
        failed_phase = "boundary";
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler shared retire queue boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler shared retire queue boundary restore (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}
