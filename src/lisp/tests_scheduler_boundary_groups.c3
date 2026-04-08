module lisp;


import std::core::mem;
import std::io;
import main;
import pika;

fn bool scheduler_runtime_boundary_fields_match(
    Interp* interp,
    main::ScopeRegion* current_scope,
    main::ScopeRegion* releasing_scope,
    Env* jit_env,
    Env* match_env,
    Expr* jit_tco_expr,
    Env* jit_tco_env,
    main::ScopeRegion* tco_recycle_scope,
    usz tco_scope_defer_slot,
    bool tco_scope_defer_active,
    bool escape_env_mode,
    PromotionContext* active_promotion_ctx
) @inline {
    return interp.current_scope == current_scope &&
           interp.releasing_scope == releasing_scope &&
           interp.jit_env == jit_env &&
           interp.match_env == match_env &&
           interp.jit_tco_expr == jit_tco_expr &&
           interp.jit_tco_env == jit_tco_env &&
           interp.tco_recycle_scope == tco_recycle_scope &&
           interp.tco_scope_defer_slot == tco_scope_defer_slot &&
           interp.tco_scope_defer_active == tco_scope_defer_active &&
           interp.escape_env_mode == escape_env_mode &&
           interp.active_promotion_ctx == active_promotion_ctx;
}

struct SchedulerBoundarySnapshot {
    main::ScopeRegion* current_scope;
    main::ScopeRegion* releasing_scope;
    Env* jit_env;
    Env* match_env;
    Expr* jit_tco_expr;
    Env* jit_tco_env;
    main::ScopeRegion* tco_recycle_scope;
    usz tco_scope_defer_slot;
    bool tco_scope_defer_active;
    bool escape_env_mode;
    PromotionContext* active_promotion_ctx;
}

fn SchedulerBoundarySnapshot scheduler_capture_runtime_boundary_snapshot(Interp* interp) @inline {
    SchedulerBoundarySnapshot snap = {};
    snap.current_scope = interp.current_scope;
    snap.releasing_scope = interp.releasing_scope;
    snap.jit_env = interp.jit_env;
    snap.match_env = interp.match_env;
    snap.jit_tco_expr = interp.jit_tco_expr;
    snap.jit_tco_env = interp.jit_tco_env;
    snap.tco_recycle_scope = interp.tco_recycle_scope;
    snap.tco_scope_defer_slot = interp.tco_scope_defer_slot;
    snap.tco_scope_defer_active = interp.tco_scope_defer_active;
    snap.escape_env_mode = interp.escape_env_mode;
    snap.active_promotion_ctx = interp.active_promotion_ctx;
    return snap;
}

fn bool scheduler_runtime_boundary_matches_snapshot(Interp* interp, SchedulerBoundarySnapshot* snap) @inline {
    return scheduler_runtime_boundary_fields_match(
        interp,
        snap.current_scope,
        snap.releasing_scope,
        snap.jit_env,
        snap.match_env,
        snap.jit_tco_expr,
        snap.jit_tco_env,
        snap.tco_recycle_scope,
        snap.tco_scope_defer_slot,
        snap.tco_scope_defer_active,
        snap.escape_env_mode,
        snap.active_promotion_ctx
    );
}

fn bool scheduler_boundary_session_probe_early_return(Interp* interp, main::ScopeRegion* temp_scope) {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = temp_scope;
    interp.releasing_scope = temp_scope;
    return true;
}

fn bool scheduler_boundary_session_probe_error_path(Interp* interp, main::ScopeRegion* temp_scope) {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = temp_scope;
    interp.releasing_scope = temp_scope;
    EvalResult r = run("(if)", interp);
    return r.error.has_error || (r.value != null && r.value.tag == ERROR);
}

fn bool scheduler_boundary_session_nested_lifo_case(Interp* interp) {
    main::ScopeRegion* root = interp.current_scope;
    main::ScopeRegion* initial_releasing = interp.releasing_scope;
    main::ScopeRegion* outer_scope = main::scope_create(root);
    main::ScopeRegion* inner_scope = main::scope_create(root);
    if (outer_scope == null || inner_scope == null) {
        if (inner_scope != null) main::scope_release(inner_scope);
        if (outer_scope != null) main::scope_release(outer_scope);
        return false;
    }

    bool lifo_ok = false;
    {
        BoundarySession outer = boundary_session_begin(interp);
        defer boundary_session_end(interp, &outer);
        interp.current_scope = outer_scope;
        interp.releasing_scope = root;
        {
            BoundarySession inner = boundary_session_begin(interp);
            defer boundary_session_end(interp, &inner);
            interp.current_scope = inner_scope;
            interp.releasing_scope = outer_scope;
        }
        lifo_ok = interp.current_scope == outer_scope &&
                  interp.releasing_scope == root;
    }

    main::scope_release(inner_scope);
    main::scope_release(outer_scope);
    return lifo_ok &&
           interp.current_scope == root &&
           interp.releasing_scope == initial_releasing;
}

fn void run_scheduler_boundary_session_guard_tests(Interp* interp, int* pass, int* fail) {
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    main::ScopeRegion* temp_scope = main::scope_create(saved.current_scope);
    if (temp_scope == null) {
        io::printn("[FAIL] scheduler boundary session guard (alloc)");
        (*fail)++;
        return;
    }

    bool early_probe_ok = scheduler_boundary_session_probe_early_return(interp, temp_scope);
    bool early_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
    bool nested_ok = scheduler_boundary_session_nested_lifo_case(interp);
    bool nested_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
    bool error_probe_ok = scheduler_boundary_session_probe_error_path(interp, temp_scope);
    bool error_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

    main::scope_release(temp_scope);

    bool ok = early_probe_ok &&
              early_state_ok &&
              nested_ok &&
              nested_state_ok &&
              error_probe_ok &&
              error_state_ok;
    if (ok) {
        emit_pass_literal("[PASS] scheduler boundary session guard restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler boundary session guard restore (early_probe=%s early_state=%s nested=%s nested_state=%s error_probe=%s error_state=%s)",
            early_probe_ok ? "yes" : "no",
            early_state_ok ? "yes" : "no",
            nested_ok ? "yes" : "no",
            nested_state_ok ? "yes" : "no",
            error_probe_ok ? "yes" : "no",
            error_state_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}

fn void scheduler_seed_completed_tcp_read_case(usz case_id) {
    PendingTcpRead* op = &g_scheduler.pending_reads[0];
    *op = {};
    op.active = true;
    op.completed = true;

    switch (case_id) {
        case 0:
            op.timed_out = true;
        case 1:
            op.error_code = -31;
        case 2:
            op.nread = 0;
        case 3:
            op.nread = 4;
            op.buffer = (char*)mem::malloc(5);
            if (op.buffer != null) {
                op.buffer[0] = 'p';
                op.buffer[1] = 'o';
                op.buffer[2] = 'n';
                op.buffer[3] = 'g';
                op.buffer[4] = 0;
            } else {
                op.nread = 0;
            }
        default:
            op.nread = 0;
    }
}

fn bool scheduler_tcp_case_value_ok(usz case_id, Value* v) {
    if (v == null) return false;
    switch (case_id) {
        case 0:
            return v.tag == ERROR;
        case 1:
            return v.tag == ERROR;
        case 2:
            return v.tag == STRING && v.str_len == 0;
        case 3:
            return v.tag == STRING &&
                   v.str_len == 4 &&
                   v.str_chars[0] == 'p' &&
                   v.str_chars[1] == 'o' &&
                   v.str_chars[2] == 'n' &&
                   v.str_chars[3] == 'g';
        default:
            return false;
    }
}

fn void run_scheduler_consume_pending_tcp_read_boundary_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_boundary_session_guard_tests(interp, pass, fail);

    // Invariant: direct consume paths for pending tcp-read preserve
    // boundary/runtime state and correctly reset slot ownership.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        void* saved_async = null;
        scheduler_test_prepare_single_fiber_no_async(&saved_async, FIBER_READY);
        g_scheduler.pending_reads[0] = {};

        // Phase A: consume before completion should fail.
        Value* pre = scheduler_consume_pending_tcp_read(0, interp);
        bool pre_ok = pre != null && pre.tag == ERROR;
        bool state_ok_pre = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!pre_ok || !state_ok_pre) {
            ok = false;
            failed_step = i * 3;
            failed_phase = "pre";
        }

        // Phase B: consume completed case.
        usz case_id = i & 3;
        if (ok) {
            scheduler_seed_completed_tcp_read_case(case_id);
            Value* consumed = scheduler_consume_pending_tcp_read(0, interp);
            bool consume_ok = scheduler_tcp_case_value_ok(case_id, consumed);
            bool slot_reset = !g_scheduler.pending_reads[0].active &&
                              !g_scheduler.pending_reads[0].completed &&
                              g_scheduler.pending_reads[0].buffer == null;
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

        // Phase C: consume after reset should fail again.
        if (ok) {
            Value* post = scheduler_consume_pending_tcp_read(0, interp);
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
        emit_pass_literal("[PASS] scheduler consume pending-tcp-read boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler consume pending-tcp-read boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_ready_barrier_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: repeated readable notifications for one blocked read coalesce
    // into one pending signal and preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_read(&saved_async);
    g_scheduler.pending_reads[0].fd = -1;
    g_scheduler.pending_reads[0].max_bytes = 8;
    g_scheduler.pending_reads[0].buffer = (char*)mem::malloc(8);
    if (g_scheduler.pending_reads[0].buffer == null) {
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
        io::printn("[FAIL] scheduler readable coalescing boundary restore (alloc)");
        (*fail)++;
        return;
    }

    usz coalesces_before = g_scheduler.readable_signal_coalesces.load();
    scheduler_signal_poll_readable(0);
    scheduler_signal_poll_readable(0);

    drain_wakeups();
    bool processed = g_scheduler.pending_reads[0].completed &&
                     !g_scheduler.pending_reads[0].timed_out &&
                     g_scheduler.pending_reads[0].error_code != 0 &&
                     !g_scheduler.pending_reads[0].readable_pending &&
                     g_scheduler.fibers[0].state == FIBER_READY &&
                     reliable_wakeup_pending_count() == 0;
    bool coalesce_ok = (g_scheduler.readable_signal_coalesces.load() - coalesces_before) == 1;
    bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

    bool ok = processed && coalesce_ok && state_ok;

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] scheduler readable coalescing boundary restore");
        (*pass)++;
    } else {
        io::printn("[FAIL] scheduler readable coalescing boundary restore");
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_offload_error_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: wakeup queue drains + async offload expected-error paths
    // preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";
    for (usz i = 0; i < 12; i++) {
        EvalResult success_r = run(
            "(block "
            "  (define ws-ok (spawn (lambda () (offload 'sleep-ms 1)))) "
            "  (await ws-ok) "
            "  true)",
            interp);
        bool success_ok = !success_r.error.has_error &&
                          success_r.value != null &&
                          success_r.value.tag != ERROR &&
                          success_r.value.tag != NIL;
        bool success_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!success_ok || !success_state_ok) {
            ok = false;
            failed_step = i * 3;
            failed_phase = "success";
            break;
        }

        bool pushed = scheduler_publish_poll_error(MAX_FIBERS + i + 101, -1);
        drain_wakeups();
        bool wakeup_ok = pushed && scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!wakeup_ok) {
            ok = false;
            failed_step = i * 3 + 1;
            failed_phase = "wakeup";
            break;
        }

        EvalResult error_r = run(
            "(block "
            "  (define ws-err (spawn (lambda () (offload 'nope 1)))) "
            "  (await ws-err))",
            interp);
        bool error_ok = error_r.error.has_error ||
                        (error_r.value != null && error_r.value.tag == ERROR);
        bool error_state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        if (!error_ok || !error_state_ok) {
            ok = false;
            failed_step = i * 3 + 2;
            failed_phase = "error";
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup/offload expected-error boundary restore");
        (*pass)++;
    } else {
        io::printfn("[FAIL] scheduler wakeup/offload expected-error boundary restore at step %d (%s)", (int)failed_step, (ZString)failed_phase);
        (*fail)++;
    }
}
