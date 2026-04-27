module lisp;

bool g_value_force_alloc_lisp_continuation_fail = false;
ContinuationAotSnapshotCapture g_continuation_aot_snapshot_capture = null;

fn void interp_clear_resume_value(Interp* self) @inline {
    self.resume_value = null;
}

fn Value* interp_consume_resume_value_or_nil(Interp* self) @inline {
    Value* resume = self.resume_value;
    self.resume_value = null;
    return resume != null ? resume : make_nil(self);
}

fn Continuation* Interp.alloc_lisp_continuation(Interp* self) {
    Continuation* k = g_value_force_alloc_lisp_continuation_fail
        ? null
        : (Continuation*)self.root_scope.alloc(Continuation.sizeof);
    if (k == null) return null;
    k.ctx = null;
    k.owner_interp = self;
    k.is_coroutine_based = false;
    k.handle_state = null;
    k.handle_retained = false;
    k.aot_mutable_snapshot = null;
    k.aot_mutable_snapshot_count = 0;
    k.aot_mutable_snapshot_replay = null;
    k.aot_mutable_snapshot_release = null;
    k.next_owned = self.continuation_head;
    self.continuation_head = k;
    return k;
}

fn void continuation_release_aot_snapshot(Continuation* k) {
    if (k == null) return;
    if (k.aot_mutable_snapshot_release != null && k.aot_mutable_snapshot != null) {
        k.aot_mutable_snapshot_release(k);
    }
    k.aot_mutable_snapshot = null;
    k.aot_mutable_snapshot_count = 0;
    k.aot_mutable_snapshot_replay = null;
    k.aot_mutable_snapshot_release = null;
}

fn void continuation_replay_aot_snapshot(Continuation* k) {
    if (k == null || k.aot_mutable_snapshot_replay == null || k.aot_mutable_snapshot == null) return;
    k.aot_mutable_snapshot_replay(k);
}

fn bool continuation_attach_aot_snapshot_if_active(Continuation* k, Value** error_out = null) {
    if (error_out != null) *error_out = null;
    if (k == null || g_continuation_aot_snapshot_capture == null) return true;
    return g_continuation_aot_snapshot_capture(k, error_out);
}

fn void interp_discard_lisp_continuation(Interp* self, Continuation* k) {
    if (self == null || k == null) return;
    // Keep the intrusive ownership list intact. The continuation node is still
    // root-owned; this helper only scrubs live runtime state so later teardown
    // paths see a dead continuation rather than a detached node.
    continuation_release_aot_snapshot(k);
    k.ctx = null;
    k.handle_state = null;
    k.handle_retained = false;
}
