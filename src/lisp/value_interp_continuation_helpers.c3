module lisp;

fn void interp_clear_resume_value(Interp* self) @inline {
    self.resume_value = null;
}

fn Value* interp_consume_resume_value_or_nil(Interp* self) @inline {
    Value* resume = self.resume_value;
    self.resume_value = null;
    return resume != null ? resume : make_nil(self);
}

fn Continuation* Interp.alloc_lisp_continuation(Interp* self) {
    Continuation* k = (Continuation*)self.root_scope.alloc(Continuation.sizeof);
    k.ctx = null;
    k.owner_interp = self;
    k.is_coroutine_based = false;
    k.handle_state = null;
    k.handle_retained = false;
    k.next_owned = self.continuation_head;
    self.continuation_head = k;
    return k;
}
