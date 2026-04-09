module lisp;

import std::io;
import main;

fn void interp_init_runtime_state(Interp* self) {
    self.symbols.init();
    self.global_env = null;

    ast_arena_init(&self.ast_arena);
    self.root_scope = main::scope_create(null);
    self.current_scope = self.root_scope;

    self.handler_count = 0;
    self.handler_capacity = 0;
    self.handler_stack = null;
    self.ensure_handler_capacity(HANDLER_INITIAL_CAPACITY);
    self.reset_depth = 0;
    self.source_dir_count = 0;
    self.source_dir_capacity = 8;
    self.source_dirs = (char**)mem::malloc(uptr.sizeof * self.source_dir_capacity);
    if (self.source_dirs == null) self.source_dir_capacity = 0;
    self.raise_msg_len = 0;
    self.raise_msg[0] = 0;
    self.raise_payload = null;

    main::stack_pool_init(&self.stack_ctx_pool);
    self.continuation_head = null;
    self.resume_value = null;
    self.yield_value = null;
    self.yield_scope = null;
    self.current_reset_state = null;
}

fn Value* interp_apply_stack_overflow_error(Interp* interp) {
    // Stack-overflow errors must survive deep unwind without per-frame
    // boundary copy/promotion churn; allocate in root scope so callers can
    // reuse the same value through scope-finalization boundaries.
    if (interp == null || interp.root_scope == null) return null;
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    Value* err = make_error(interp, "stack overflow: insufficient stack headroom");
    interp.current_scope = saved_scope;
    return err;
}

fn Value* interp_apply_frame_enter(Interp* interp) @inline {
    if (interp_interrupt_requested(interp)) {
        interp_interrupt_clear(interp);
        return interp_interrupt_error(interp);
    }
    interp.eval_depth++;
    if (!main::stack_has_apply_headroom(main::STACK_APPLY_MIN_HEADROOM)) {
        if (boundary_c_getenv("OMNI_TRACE_APPLY_HEADROOM") != null) {
            io::printfn("OMNI_APPLY_HEADROOM_TRACE overflow eval_depth=%d", (int)interp.eval_depth);
        }
        interp.eval_depth--;
        return interp_apply_stack_overflow_error(interp);
    }
    return null;
}

fn void interp_apply_frame_leave(Interp* interp) @inline {
    assert(interp.eval_depth > 0, "interp eval depth underflow");
    interp.eval_depth--;
}
