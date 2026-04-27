module lisp;

import std::core::mem;
import std::atomic::types;
import std::io;
import std::thread;
import main;

// Scheduler core: pending I/O, wakeups, fiber state transitions.
fn bool scheduler_parent_accepts_child(usz parent_id, Interp* interp, bool* out_owner_mismatch = null) @inline {
    if (out_owner_mismatch != null) *out_owner_mismatch = false;
    if (parent_id == NO_FIBER) return true;
    if (parent_id >= g_scheduler.fiber_count) return false;
    FiberEntry* parent = &g_scheduler.fibers[parent_id];
    if (parent.state == FIBER_DONE) return false;
    Interp* parent_owner = parent.owner_interp;
    if (parent_owner != null && parent_owner != interp) {
        if (out_owner_mismatch != null) *out_owner_mismatch = true;
        return false;
    }
    return true;
}

fn usz scheduler_add_fiber(
    Value* coroutine,
    usz parent_id,
    Interp* interp,
    bool* out_promotion_failed = null,
    bool* out_parent_invalid = null
) {
    scheduler_init();
    if (g_scheduler.fiber_count >= MAX_FIBERS) return MAX_FIBERS;

    if (out_promotion_failed != null) *out_promotion_failed = false;
    if (out_parent_invalid != null) *out_parent_invalid = false;

    if (coroutine == null || coroutine.tag != COROUTINE || coroutine.coroutine_val == null) {
        return MAX_FIBERS;
    }
    if (interp == null) return MAX_FIBERS;
    bool owner_mismatch = false;
    if (!scheduler_parent_accepts_child(parent_id, interp, &owner_mismatch)) {
        if (out_parent_invalid != null) *out_parent_invalid = true;
        if (owner_mismatch && out_promotion_failed != null) *out_promotion_failed = true;
        return MAX_FIBERS;
    }

    coroutine = boundary_promote_to_root(coroutine, interp);
    if (coroutine == null || coroutine.tag != COROUTINE || coroutine.coroutine_val == null) {
        if (out_promotion_failed != null) *out_promotion_failed = true;
        return MAX_FIBERS;
    }

    usz id = g_scheduler.fiber_count;
    FiberEntry* f = &g_scheduler.fibers[id];
    g_scheduler.fiber_wake_epoch_seed++;
    if (g_scheduler.fiber_wake_epoch_seed == 0) g_scheduler.fiber_wake_epoch_seed = 1;
    f.coroutine = coroutine;
    f.result = null;
    f.pending_result = null;
    f.owner_interp = interp;
    f.wake_epoch = g_scheduler.fiber_wake_epoch_seed;
    f.domain_id = scheduler_domain_id(scheduler_current_domain());
    f.parent_id = parent_id;
    f.live_children = 0;
    f.await_target = NO_FIBER;
    f.awaiting_fiber = false;
    f.waiting_children = false;
    f.cancel_requested = false;
    f.state = FIBER_READY;
    g_scheduler.pending_reads[id] = {};
    g_scheduler.pending_offloads[id] = {};
    g_scheduler.pending_sleeps[id] = {};
    g_scheduler.pending_dns[id] = {};
    g_scheduler.pending_connects[id] = {};
    g_scheduler.pending_accepts[id] = {};
    g_scheduler.fiber_join_wait_token[id] = 0;
    g_scheduler.fiber_count++;
    return id;
}

fn bool scheduler_fiber_belongs_to_interp(usz fiber_id, Interp* interp) @inline {
    if (interp == null) return false;
    if (fiber_id >= g_scheduler.fiber_count) return false;
    Interp* owner = g_scheduler.fibers[fiber_id].owner_interp;
    return owner == null || owner == interp;
}

fn bool scheduler_has_interp_owned_fibers(Interp* interp) {
    if (!g_scheduler_initialized || interp == null) return false;
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        if (g_scheduler.fibers[i].owner_interp == interp) return true;
    }
    return false;
}

fn bool scheduler_is_running_fiber_context() @inline {
    if (!g_scheduler.running) return false;
    if (g_scheduler.current_fiber_id >= g_scheduler.fiber_count) return false;
    FiberEntry* f = &g_scheduler.fibers[g_scheduler.current_fiber_id];
    if (f.state != FIBER_RUNNING) return false;
    if (f.coroutine == null || f.coroutine.tag != COROUTINE || f.coroutine.coroutine_val == null) return false;
    // Allow nested StackCtxes (handle bodies, reset blocks, etc. running inside a fiber)
    return main::g_current_stack_ctx != null;
}

fn bool scheduler_is_running_fiber_root_context() @inline {
    if (!scheduler_is_running_fiber_context()) return false;
    FiberEntry* f = &g_scheduler.fibers[g_scheduler.current_fiber_id];
    main::StackCtx* root_ctx = f.coroutine.coroutine_val;
    return root_ctx != null && main::g_current_stack_ctx == root_ctx;
}

fn Value* scheduler_require_fiber_root_context(char[] op_name, Interp* interp) {
    if (!scheduler_is_running_fiber_context()) return null;
    if (scheduler_is_running_fiber_root_context()) return null;
    char[128] ebuf;
    char[] msg;
    if (try formatted = io::bprintf(
            &ebuf,
            "%s: blocking scheduler op requires the fiber root context",
            (ZString)op_name
        )) {
        msg = formatted;
    } else {
        msg = "blocking scheduler op requires the fiber root context";
    }
    return scheduler_raise(
        interp,
        "scheduler/fiber-nested-blocking-op",
        msg
    );
}

fn bool scheduler_has_blocked_fibers() @inline {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        if (g_scheduler.fibers[i].state == FIBER_BLOCKED) return true;
    }
    return false;
}

fn bool scheduler_all_done() @inline {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        if (g_scheduler.fibers[i].state != FIBER_DONE) return false;
    }
    return true;
}

fn void scheduler_release_fiber_coroutine(usz fiber_id, Interp* interp) {
    if (fiber_id >= g_scheduler.fiber_count) return;
    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    Value* coroutine = f.coroutine;
    if (coroutine == null || coroutine.tag != COROUTINE || coroutine.coroutine_val == null) return;
    Interp* owner_interp = f.owner_interp != null ? f.owner_interp : interp;
    coroutine_ctx_cleanup(coroutine, coroutine.coroutine_val, owner_interp);
}

fn Value* scheduler_make_root_error(Interp* interp, char[] msg) {
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    defer interp.current_scope = saved_scope;
    return make_error(interp, msg);
}

fn Value* scheduler_publish_result_or_root_error(
    Value* result,
    Interp* owner_interp,
    char[] failure_msg
) {
    if (result == null) return null;
    Value* promoted = boundary_promote_to_root(result, owner_interp);
    if (promoted == null) {
        return scheduler_make_root_error(owner_interp, failure_msg);
    }
    if (result.tag != ERROR && promoted.tag == ERROR) {
        return scheduler_make_root_error(owner_interp, failure_msg);
    }
    return promoted;
}

fn void scheduler_mark_done(usz fiber_id, Value* result, Interp* interp) {
    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    Interp* owner_interp = f.owner_interp != null ? f.owner_interp : interp;
    Value* promoted = scheduler_publish_result_or_root_error(
        result,
        owner_interp,
        "scheduler: failed to promote fiber result to root"
    );
    f.state = FIBER_DONE;
    f.result = promoted;
    f.pending_result = null;
    f.awaiting_fiber = false;
    f.await_target = NO_FIBER;
    f.waiting_children = false;
    f.cancel_requested = false;
    scheduler_close_pending_tcp_read(fiber_id);
    scheduler_close_pending_offload(fiber_id);
    scheduler_close_pending_sleep(fiber_id);
    scheduler_close_pending_dns(fiber_id);
    scheduler_close_pending_tcp_connect(fiber_id);
    scheduler_close_pending_tcp_accept(fiber_id);
    // Clear any pending task/thread waiter slots that still point at this fiber id
    // so late completions cannot target a recycled fiber slot.
    scheduler_clear_thread_task_waiters_for_fiber(fiber_id);

    if (f.parent_id != NO_FIBER && scheduler_parent_accepts_child(f.parent_id, owner_interp)) {
        FiberEntry* parent = &g_scheduler.fibers[f.parent_id];
        if (parent.live_children > 0) parent.live_children--;

        if (parent.waiting_children && parent.live_children == 0) {
            Value* final_result;
            if (parent.cancel_requested) {
                final_result = scheduler_raise(owner_interp, "scheduler/fiber-cancelled", "fiber cancelled");
            } else {
                final_result = parent.pending_result != null ? parent.pending_result : make_nil(interp);
            }
            parent.pending_result = null;
            parent.waiting_children = false;
            scheduler_complete_fiber(f.parent_id, final_result, owner_interp);
            return;
        }

        if (parent.awaiting_fiber && parent.await_target == fiber_id && parent.state == FIBER_BLOCKED) {
            parent.awaiting_fiber = false;
            parent.await_target = NO_FIBER;
            parent.state = FIBER_READY;
        }
    }
}

fn void scheduler_complete_fiber(usz fiber_id, Value* result, Interp* interp) {
    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    Interp* owner_interp = f.owner_interp != null ? f.owner_interp : interp;
    if (f.live_children > 0) {
        Value* promoted = scheduler_publish_result_or_root_error(
            result,
            owner_interp,
            "scheduler: failed to promote child result to root"
        );
        f.pending_result = promoted;
        f.waiting_children = true;
        f.awaiting_fiber = false;
        f.await_target = NO_FIBER;
        f.state = FIBER_BLOCKED;
        return;
    }
    scheduler_mark_done(fiber_id, result, owner_interp);
}

fn void scheduler_resume_fiber(usz fiber_id, Interp* interp) {
    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    Interp* owner_interp = f.owner_interp != null ? f.owner_interp : interp;
    if (f.state != FIBER_READY) return;
    if (!scheduler_fiber_belongs_to_domain(fiber_id, scheduler_current_domain())) {
        scheduler_complete_fiber(
            fiber_id,
            scheduler_raise(
                owner_interp,
                "scheduler/domain-fiber-mismatch",
                "scheduler: fiber does not belong to the current domain"
            ),
            owner_interp
        );
        return;
    }

    f.state = FIBER_RUNNING;
    g_scheduler.current_fiber_id = fiber_id;

    Value*[1] resume_args;
    resume_args[0] = f.coroutine;
    Value* result = prim_resume(resume_args[..], null, owner_interp);

    g_scheduler.current_fiber_id = NO_FIBER;

    if (result != null && result.tag == ERROR) {
        scheduler_complete_fiber(fiber_id, result, owner_interp);
        return;
    }

    if (f.coroutine == null || f.coroutine.coroutine_val == null) {
        scheduler_complete_fiber(fiber_id, result, owner_interp);
        return;
    }

    StackCtx* ctx = f.coroutine.coroutine_val;
    if (ctx.status == main::StackCtxStatus.CTX_COMPLETED || ctx.status == main::StackCtxStatus.CTX_DEAD) {
        scheduler_complete_fiber(fiber_id, result, owner_interp);
        return;
    }

    // Default yield path: runnable again unless an async op marked it BLOCKED.
    if (f.state == FIBER_RUNNING) f.state = FIBER_READY;
}

fn bool scheduler_wait_for_io_once() {
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) return false;
    (void)uv_run(g_scheduler.uv_loop, UV_RUN_ONCE);
    return true;
}
