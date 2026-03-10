module lisp;

import std::core::mem;
import std::atomic::types;
import std::io;
import std::thread;
import main;

// Scheduler core: pending I/O, wakeups, fiber state transitions.
fn usz scheduler_add_fiber(Value* coroutine, usz parent_id, Interp* interp) {
    scheduler_init();
    if (g_scheduler.fiber_count >= MAX_FIBERS) return MAX_FIBERS;

    coroutine = boundary_promote_to_root(coroutine, interp);
    usz id = g_scheduler.fiber_count;
    FiberEntry* f = &g_scheduler.fibers[id];
    f.coroutine = coroutine;
    f.result = null;
    f.pending_result = null;
    f.domain_id = scheduler_domain_id(scheduler_current_domain());
    f.parent_id = parent_id;
    f.live_children = 0;
    f.await_target = NO_FIBER;
    f.awaiting_fiber = false;
    f.waiting_children = false;
    f.state = FIBER_READY;
    g_scheduler.pending_reads[id] = {};
    g_scheduler.pending_offloads[id] = {};
    g_scheduler.pending_sleeps[id] = {};
    g_scheduler.pending_dns[id] = {};
    g_scheduler.pending_connects[id] = {};
    g_scheduler.pending_accepts[id] = {};
    g_scheduler.fiber_count++;
    return id;
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

fn void scheduler_mark_done(usz fiber_id, Value* result, Interp* interp) {
    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    f.state = FIBER_DONE;
    f.result = boundary_promote_to_root(result, interp);
    f.pending_result = null;
    f.awaiting_fiber = false;
    f.await_target = NO_FIBER;
    f.waiting_children = false;
    scheduler_close_pending_tcp_read(fiber_id);
    scheduler_close_pending_offload(fiber_id);
    scheduler_close_pending_sleep(fiber_id);
    scheduler_close_pending_dns(fiber_id);
    scheduler_close_pending_tcp_connect(fiber_id);
    scheduler_close_pending_tcp_accept(fiber_id);

    if (f.parent_id < g_scheduler.fiber_count) {
        FiberEntry* parent = &g_scheduler.fibers[f.parent_id];
        if (parent.live_children > 0) parent.live_children--;

        if (parent.waiting_children && parent.live_children == 0) {
            Value* final_result = parent.pending_result != null ? parent.pending_result : make_nil(interp);
            parent.pending_result = null;
            parent.waiting_children = false;
            scheduler_complete_fiber(f.parent_id, final_result, interp);
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
    if (f.live_children > 0) {
        f.pending_result = boundary_promote_to_root(result, interp);
        f.waiting_children = true;
        f.awaiting_fiber = false;
        f.await_target = NO_FIBER;
        f.state = FIBER_BLOCKED;
        return;
    }
    scheduler_mark_done(fiber_id, result, interp);
}

fn void scheduler_resume_fiber(usz fiber_id, Interp* interp) {
    FiberEntry* f = &g_scheduler.fibers[fiber_id];
    if (f.state != FIBER_READY) return;
    if (!scheduler_fiber_belongs_to_domain(fiber_id, scheduler_current_domain())) {
        scheduler_complete_fiber(
            fiber_id,
            scheduler_raise(
                interp,
                "scheduler/domain-fiber-mismatch",
                "scheduler: fiber does not belong to the current domain"
            ),
            interp
        );
        return;
    }

    f.state = FIBER_RUNNING;
    g_scheduler.current_fiber_id = fiber_id;

    Value*[1] resume_args;
    resume_args[0] = f.coroutine;
    Value* result = prim_resume(resume_args[..], null, interp);

    g_scheduler.current_fiber_id = NO_FIBER;

    if (result != null && result.tag == ERROR) {
        scheduler_complete_fiber(fiber_id, result, interp);
        return;
    }

    if (f.coroutine == null || f.coroutine.coroutine_val == null) {
        scheduler_complete_fiber(fiber_id, result, interp);
        return;
    }

    StackCtx* ctx = f.coroutine.coroutine_val;
    if (ctx.status == main::StackCtxStatus.CTX_COMPLETED || ctx.status == main::StackCtxStatus.CTX_DEAD) {
        scheduler_complete_fiber(fiber_id, result, interp);
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
