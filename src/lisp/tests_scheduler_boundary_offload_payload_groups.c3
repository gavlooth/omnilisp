module lisp;

import std::core::mem;
import std::io;

fn void run_scheduler_invalid_offload_wakeup_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: invalid reliable offload completions must be drained safely (payload cleanup path)
    // and must not mutate interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 32; i++) {
        void* saved_async = null;
        scheduler_test_prepare_no_async(&saved_async);

        OffloadCompletion* completion = scheduler_make_task_cancel_completion();
        if (completion == null) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            scheduler_test_cleanup_no_async(saved_async);
            break;
        }

        bool pushed_ready = scheduler_publish_offload_ready(MAX_FIBERS + 4000 + i, completion);
        bool pushed_err = scheduler_publish_poll_error(MAX_FIBERS + 5000 + i, -1);
        if (!pushed_ready) {
            scheduler_free_offload_completion(completion);
            ok = false;
            failed_step = i;
            failed_phase = "enqueue";
            scheduler_test_cleanup_no_async(saved_async);
            break;
        }

        drain_wakeups();
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

        bool drain_ok = pushed_err && reliable_wakeup_pending_count() == 0;
        scheduler_test_cleanup_no_async(saved_async);
        if (!drain_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!drain_ok) {
                failed_phase = "drain";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler invalid offload wakeup boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler invalid offload wakeup boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_invalid_offload_completion_kind_boundary_tests(Interp* interp, int* pass, int* fail) {
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    OffloadCompletion invalid = {};
    ((char*)&invalid.kind)[0] = 99;

    Value* out = scheduler_value_from_offload_completion(&invalid, interp);
    bool ok = out != null &&
              out.tag == ERROR &&
              scheduler_test_str_contains(
                  out.str_chars[:out.str_len],
                  "invalid completion kind"
              ) &&
              scheduler_runtime_boundary_matches_snapshot(interp, &saved);

    if (ok) {
        emit_pass_literal("[PASS] scheduler invalid offload completion kind fails closed");
        (*pass)++;
    } else {
        io::printn("[FAIL] scheduler invalid offload completion kind fails closed");
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_full_payload_ownership_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: WAKEUP_OFFLOAD_READY payload enqueued through the reliable queue
    // must still deliver exactly once and preserve boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 12; i++) {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_offload(&saved_async);
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();

        OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
        if (completion == null) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }
        *completion = {};
        completion.kind = OFFLOAD_RES_NIL;

        bool pushed_ready = scheduler_publish_offload_ready(0, completion);
        drain_wakeups();
        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
        bool slot_ready = g_scheduler.pending_offloads[0].active &&
                          g_scheduler.pending_offloads[0].completed &&
                          g_scheduler.pending_offloads[0].completion == completion &&
                          g_scheduler.fibers[0].state == FIBER_READY;
        Value* consumed = slot_ready ? scheduler_consume_pending_offload(0, interp) : null;
        bool consume_ok = consumed != null && consumed.tag == NIL &&
                          !g_scheduler.pending_offloads[0].active &&
                          !g_scheduler.pending_offloads[0].completed &&
                          g_scheduler.pending_offloads[0].completion == null;
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool queue_ok = pushed_ready && reliable_wakeup_pending_count() == 0;
        bool drops_ok = (failures_after - failures_before) == 0 &&
                        (enqueues_after - enqueues_before) == 1;

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!queue_ok || !drops_ok || !slot_ready || !consume_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!queue_ok) {
                failed_phase = "queue";
            } else if (!drops_ok) {
                failed_phase = "observability";
            } else if (!slot_ready || !consume_ok) {
                failed_phase = "delivery";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup full payload ownership boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler wakeup full payload ownership boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_inactive_offload_ready_payload_cleanup_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: in-range WAKEUP_OFFLOAD_READY for an inactive pending slot
    // must clean up payload and preserve runtime boundary state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        void* saved_async = null;
        scheduler_test_prepare_single_fiber_no_async(&saved_async, FIBER_READY);
        g_scheduler.pending_offloads[0] = {};

        OffloadCompletion* completion = scheduler_make_task_cancel_completion();
        if (completion == null) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        bool pushed = scheduler_publish_offload_ready(0, completion);
        drain_wakeups();
        bool slot_ok = !g_scheduler.pending_offloads[0].active &&
                       !g_scheduler.pending_offloads[0].completed &&
                       g_scheduler.pending_offloads[0].completion == null &&
                       g_scheduler.fibers[0].state == FIBER_READY;
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool cycle_ok = pushed && reliable_wakeup_pending_count() == 0 && slot_ok && state_ok;

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!cycle_ok) {
            ok = false;
            failed_step = i;
            if (!pushed || reliable_wakeup_pending_count() != 0 || !slot_ok) {
                failed_phase = "drain";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler inactive offload-ready payload cleanup boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler inactive offload-ready payload cleanup boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_duplicate_offload_ready_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: duplicate/late WAKEUP_OFFLOAD_READY events for one active pending
    // offload retain the first completion payload, safely discard extras, and
    // preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_offload(&saved_async);

        OffloadCompletion* c1 = scheduler_make_task_cancel_completion();
        OffloadCompletion* c2 = scheduler_make_task_cancel_completion();
        if (c1 == null || c2 == null) {
            if (c1 != null) scheduler_free_offload_completion(c1);
            if (c2 != null) scheduler_free_offload_completion(c2);
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        bool pushed_first = scheduler_publish_offload_ready(0, c1);
        bool pushed_second = scheduler_publish_offload_ready(0, c2);
        if (!pushed_first || !pushed_second) {
            if (!pushed_first) scheduler_free_offload_completion(c1);
            if (!pushed_second) scheduler_free_offload_completion(c2);
            if (pushed_first) {
                drain_wakeups();
                if (g_scheduler.pending_offloads[0].completion != null) {
                    scheduler_free_offload_completion(g_scheduler.pending_offloads[0].completion);
                }
            }
            ok = false;
            failed_step = i;
            failed_phase = "enqueue";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        drain_wakeups();
        PendingOffload off_after_dup = g_scheduler.pending_offloads[0];
        bool state_ok_dup = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool dup_ok = reliable_wakeup_pending_count() == 0 &&
                      off_after_dup.active &&
                      off_after_dup.completed &&
                      off_after_dup.completion == c1 &&
                      off_after_dup.completion != c2 &&
                      g_scheduler.fibers[0].state == FIBER_READY;

        OffloadCompletion* c3 = scheduler_make_task_cancel_completion();
        if (c3 == null) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc-late";
        } else {
            bool pushed_late = scheduler_publish_offload_ready(0, c3);
            if (!pushed_late) {
                scheduler_free_offload_completion(c3);
                ok = false;
                failed_step = i;
                failed_phase = "enqueue-late";
            } else {
                drain_wakeups();
                PendingOffload off_after_late = g_scheduler.pending_offloads[0];
                bool state_ok_late = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
                bool late_ok = reliable_wakeup_pending_count() == 0 &&
                               off_after_late.active &&
                               off_after_late.completed &&
                               off_after_late.completion == c1;
                if (!dup_ok || !late_ok || !state_ok_dup || !state_ok_late) {
                    ok = false;
                    failed_step = i;
                    if (!dup_ok) {
                        failed_phase = "dup";
                    } else if (!late_ok) {
                        failed_phase = "late";
                    } else if (!state_ok_dup) {
                        failed_phase = "boundary-dup";
                    } else {
                        failed_phase = "boundary-late";
                    }
                }
            }
        }

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!ok) break;
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler duplicate offload-ready boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler duplicate offload-ready boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_consume_pending_offload_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: direct consume paths for pending offload (pre-completion error,
    // completed success/error, and post-consume repeat error) preserve boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        void* saved_async = null;
        scheduler_test_prepare_single_fiber_no_async(&saved_async, FIBER_READY);
        g_scheduler.pending_offloads[0] = {};

        Value* pre = scheduler_consume_pending_offload(0, interp);
        bool pre_ok = pre != null && pre.tag == ERROR;
        bool state_ok_pre = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!pre_ok || !state_ok_pre) {
            ok = false;
            failed_step = i * 3;
            failed_phase = "pre";
        }

        OffloadCompletion* completion = null;
        long expected_int = 0;
        char[] expected_bytes = "blob-ok";
        usz mode = i % 3;
        bool expect_error = mode == 1;
        bool expect_bytes = mode == 2;
        if (ok && expect_error) {
            completion = scheduler_make_task_cancel_completion();
        } else if (ok && expect_bytes) {
            completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
            if (completion != null) {
                *completion = {};
                completion.kind = OFFLOAD_RES_BYTES;
                completion.shared = scheduler_publish_shared(expected_bytes);
                if (completion.shared == null) {
                    mem::free(completion);
                    completion = null;
                }
            }
        } else if (ok) {
            completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
            if (completion != null) {
                *completion = {};
                completion.kind = OFFLOAD_RES_INT;
                expected_int = 1000 + (long)i;
                completion.int_result = expected_int;
            }
        }
        if (ok && completion == null) {
            ok = false;
            failed_step = i * 3 + 1;
            failed_phase = "alloc";
        }

        if (ok) {
            g_scheduler.pending_offloads[0].active = true;
            g_scheduler.pending_offloads[0].completed = true;
            g_scheduler.pending_offloads[0].completion = completion;
            Value* consumed = scheduler_consume_pending_offload(0, interp);

            bool consume_ok = expect_error
                              ? (consumed != null && consumed.tag == ERROR)
                              : (expect_bytes
                                 ? (consumed != null &&
                                    consumed.tag == STRING &&
                                    consumed.str_len == expected_bytes.len &&
                                    str_starts_with(consumed.str_chars[:consumed.str_len], expected_bytes))
                                 : (consumed != null && consumed.tag == INT && consumed.int_val == expected_int));
            bool slot_reset = !g_scheduler.pending_offloads[0].active &&
                              !g_scheduler.pending_offloads[0].completed &&
                              g_scheduler.pending_offloads[0].completion == null;
            bool state_ok_consume = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
            if (!consume_ok || !slot_reset || !state_ok_consume) {
                ok = false;
                failed_step = i * 3 + 1;
                if (!consume_ok) {
                    failed_phase = "consume-value";
                } else if (!slot_reset) {
                    failed_phase = "consume-reset";
                } else {
                    failed_phase = "consume-boundary";
                }
            }
        }

        if (ok) {
            Value* post = scheduler_consume_pending_offload(0, interp);
            bool post_ok = post != null && post.tag == ERROR;
            bool state_ok_post = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
            if (!post_ok || !state_ok_post) {
                ok = false;
                failed_step = i * 3 + 2;
                failed_phase = "post";
            }
        }

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!ok) break;
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler consume pending-offload boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler consume pending-offload boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}
