module lisp;

import std::core::mem;
import std::io;

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
