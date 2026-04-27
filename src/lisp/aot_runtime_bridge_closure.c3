module lisp::aot;

import std::core::mem;
import std::io;
import main;
import lisp;

alias AotClosureFn = fn lisp::Value*(void* data, lisp::Value* arg);
alias AotVariadicFn = fn lisp::Value*(void* data, lisp::Value* arg_list);

struct AotCaptureRetention {
    main::ScopeRegion* retained_scope;
    bool scope_retained;
}

struct AotClosureData {
    void*         data;
    AotClosureFn  invoke;
    AotVariadicFn invoke_var;
    bool          is_variadic;
    bool          owns_data;
    AotCaptureRetention* retention;
    usz           ref_count;
}

struct AotMutableCell {
    lisp::Value* value;
    AotMutableCell* next;
}

struct AotMutableCellSnapshot {
    AotMutableCell* cell;
    lisp::Value* value;
}

struct AotMutableCellSnapshotFrame {
    AotMutableCellSnapshot* snapshot;
    usz count;
    usz capacity;
    AotMutableCellSnapshotFrame* previous;
}

// AOT closure allocation-failure seams (tests only).
bool g_aot_force_closure_data_alloc_fail = false;
bool g_aot_force_closure_primitive_alloc_fail = false;
bool g_aot_force_variadic_closure_data_alloc_fail = false;
bool g_aot_force_variadic_closure_primitive_alloc_fail = false;
bool g_aot_force_invoke_variadic_arg_list_alloc_fail = false;
bool g_aot_force_variadic_apply_arg_list_alloc_fail = false;
bool g_aot_force_dispatch_interp_unavailable = false;
bool g_aot_force_bridge_interp_unavailable = false;
bool g_aot_force_capture_reachability_audit_fail = false;
int g_aot_owned_data_free_count = 0;
uint g_aot_out_of_band_diagnostic_count = 0;
char[256] g_aot_last_out_of_band_diagnostic;
usz g_aot_last_out_of_band_diagnostic_len = 0;
tlocal AotMutableCell* g_aot_mutable_cells = null;
tlocal usz g_aot_mutable_cell_count = 0;
tlocal AotMutableCellSnapshotFrame* g_aot_mutable_cell_snapshot_frame = null;

fn void mutable_cell_registry_reset() {
    g_aot_mutable_cells = null;
    g_aot_mutable_cell_count = 0;
    g_aot_mutable_cell_snapshot_frame = null;
}

fn void mutable_cell_snapshot_release_for_continuation(lisp::Continuation* k) {
    if (k == null || k.aot_mutable_snapshot == null) return;
    mem::free(k.aot_mutable_snapshot);
    k.aot_mutable_snapshot = null;
    k.aot_mutable_snapshot_count = 0;
}

fn void mutable_cell_snapshot_replay_for_continuation(lisp::Continuation* k) {
    if (k == null || k.aot_mutable_snapshot == null) return;
    mutable_cell_snapshot_replay((AotMutableCellSnapshot*)k.aot_mutable_snapshot, k.aot_mutable_snapshot_count);
}

fn bool mutable_cell_snapshot_capture_for_continuation(lisp::Continuation* k, lisp::Value** error_out) {
    if (error_out != null) *error_out = null;
    if (k == null) return false;
    AotMutableCellSnapshotFrame* frame = g_aot_mutable_cell_snapshot_frame;
    if (frame == null || frame.snapshot == null || frame.count == 0) return true;
    usz bytes = 0;
    if (!lisp::compiler_checked_mul_usz(AotMutableCellSnapshot.sizeof, frame.count, &bytes)) {
        if (error_out != null) *error_out = aot_make_error_best_effort(null, "aot mutable-cell: continuation snapshot size overflow");
        return false;
    }
    AotMutableCellSnapshot* snapshot = (AotMutableCellSnapshot*)mem::malloc(bytes);
    if (snapshot == null) {
        if (error_out != null) *error_out = aot_make_error_best_effort(null, "aot mutable-cell: out of memory while snapshotting continuation");
        return false;
    }
    for (usz i = 0; i < frame.count; i++) {
        snapshot[i] = frame.snapshot[i];
    }
    mutable_cell_snapshot_release_for_continuation(k);
    k.aot_mutable_snapshot = (void*)snapshot;
    k.aot_mutable_snapshot_count = frame.count;
    k.aot_mutable_snapshot_replay = &mutable_cell_snapshot_replay_for_continuation;
    k.aot_mutable_snapshot_release = &mutable_cell_snapshot_release_for_continuation;
    return true;
}

fn bool mutable_cell_snapshot_frame_append_cell(AotMutableCellSnapshotFrame* frame, AotMutableCell* cell, lisp::Value* value, lisp::Value** error_out = null) {
    if (error_out != null) *error_out = null;
    if (frame == null || cell == null) return true;
    if (frame.count >= frame.capacity) {
        usz next_capacity = frame.capacity > 0 ? frame.capacity * 2 : 4;
        if (next_capacity < frame.count + 1) next_capacity = frame.count + 1;
        usz bytes = 0;
        if (!lisp::compiler_checked_mul_usz(AotMutableCellSnapshot.sizeof, next_capacity, &bytes)) {
            if (error_out != null) *error_out = aot_make_error_best_effort(null, "aot mutable-cell: active reset snapshot size overflow");
            return false;
        }
        AotMutableCellSnapshot* grown = (AotMutableCellSnapshot*)mem::realloc(frame.snapshot, bytes);
        if (grown == null) {
            if (error_out != null) *error_out = aot_make_error_best_effort(null, "aot mutable-cell: out of memory while extending reset snapshot");
            return false;
        }
        frame.snapshot = grown;
        frame.capacity = next_capacity;
    }
    frame.snapshot[frame.count].cell = cell;
    frame.snapshot[frame.count].value = value;
    frame.count++;
    return true;
}

fn bool mutable_cell_snapshot_append_new_cell(AotMutableCell* cell, lisp::Value* value, lisp::Value** error_out = null) {
    if (error_out != null) *error_out = null;
    AotMutableCellSnapshotFrame* frame = g_aot_mutable_cell_snapshot_frame;
    while (frame != null) {
        if (!mutable_cell_snapshot_frame_append_cell(frame, cell, value, error_out)) {
            AotMutableCellSnapshotFrame* rollback = g_aot_mutable_cell_snapshot_frame;
            while (rollback != null && rollback != frame) {
                if (rollback.count > 0 && rollback.snapshot != null && rollback.snapshot[rollback.count - 1].cell == cell) {
                    rollback.count--;
                    rollback.snapshot[rollback.count].cell = null;
                    rollback.snapshot[rollback.count].value = null;
                }
                rollback = rollback.previous;
            }
            return false;
        }
        frame = frame.previous;
    }
    return true;
}

fn void mutable_cell_snapshot_hooks_install() {
    lisp::g_continuation_aot_snapshot_capture = &mutable_cell_snapshot_capture_for_continuation;
}

fn void mutable_cell_snapshot_hooks_uninstall() {
    if (lisp::g_continuation_aot_snapshot_capture == &mutable_cell_snapshot_capture_for_continuation) {
        lisp::g_continuation_aot_snapshot_capture = null;
    }
}

fn void mutable_cell_snapshot_frame_push(AotMutableCellSnapshotFrame* frame) {
    if (frame == null) return;
    frame.previous = g_aot_mutable_cell_snapshot_frame;
    g_aot_mutable_cell_snapshot_frame = frame;
}

fn void mutable_cell_snapshot_frame_pop(AotMutableCellSnapshotFrame* frame) {
    if (frame == null) return;
    if (g_aot_mutable_cell_snapshot_frame == frame) {
        g_aot_mutable_cell_snapshot_frame = frame.previous;
    }
    frame.previous = null;
}

fn void mutable_cell_snapshot_restore_active() {
    AotMutableCellSnapshotFrame* frame = g_aot_mutable_cell_snapshot_frame;
    if (frame == null) return;
    mutable_cell_snapshot_replay(frame.snapshot, frame.count);
}

fn bool aot_closure_payload_valid(AotClosureData* cd) @inline {
    return cd != null && cd.invoke != null;
}

fn bool aot_variadic_payload_valid(AotClosureData* cd) @inline {
    return cd != null && cd.invoke_var != null;
}

fn lisp::Interp* aot_resolve_interp(lisp::Interp* interp) @inline {
    return interp != null ? interp : g_aot_interp;
}

fn lisp::Interp* aot_dispatch_interp() @inline {
    if (g_aot_force_dispatch_interp_unavailable) return null;
    return g_aot_interp;
}

fn lisp::Interp* aot_bridge_interp() @inline {
    if (g_aot_force_bridge_interp_unavailable) return null;
    return g_aot_interp;
}

fn lisp::Value* aot_make_error_best_effort(lisp::Interp* interp, char[] message) @inline {
    lisp::Interp* active_interp = aot_resolve_interp(interp);
    if (active_interp != null) return lisp::make_error(active_interp, message);
    aot_record_out_of_band_diagnostic(message);
    return null;
}

fn void aot_record_out_of_band_diagnostic(char[] message) {
    g_aot_out_of_band_diagnostic_count++;
    usz copy_len = message.len < g_aot_last_out_of_band_diagnostic.len - 1
        ? message.len
        : g_aot_last_out_of_band_diagnostic.len - 1;
    for (usz i = 0; i < copy_len; i++) {
        g_aot_last_out_of_band_diagnostic[i] = message[i];
    }
    g_aot_last_out_of_band_diagnostic[copy_len] = 0;
    g_aot_last_out_of_band_diagnostic_len = copy_len;
    io::eprintfn("[aot] %s", (ZString)&g_aot_last_out_of_band_diagnostic[0]);
}

fn void aot_out_of_band_diagnostic_reset_for_tests() {
    g_aot_out_of_band_diagnostic_count = 0;
    g_aot_last_out_of_band_diagnostic[0] = 0;
    g_aot_last_out_of_band_diagnostic_len = 0;
}

fn uint aot_out_of_band_diagnostic_count_for_tests() {
    return g_aot_out_of_band_diagnostic_count;
}

fn char[] aot_last_out_of_band_diagnostic_for_tests() {
    return g_aot_last_out_of_band_diagnostic[:g_aot_last_out_of_band_diagnostic_len];
}

fn bool aot_intern_checked(lisp::Interp* interp, char[] name, lisp::SymbolId* out) @inline {
    if (interp == null || out == null || name.ptr == null) return false;
    lisp::SymbolId sid = interp.symbols.intern(name);
    if (sid == lisp::INVALID_SYMBOL_ID) return false;
    *out = sid;
    return true;
}

fn lisp::Value* aot_make_single_arg_list(lisp::Interp* interp, lisp::Value* arg) @inline {
    if (g_aot_force_invoke_variadic_arg_list_alloc_fail) return null;
    if (interp == null) return null;
    return lisp::make_cons(interp, arg, lisp::make_nil(interp));
}

fn lisp::Value* aot_prepend_arg_checked(lisp::Interp* interp, lisp::Value* arg, lisp::Value* tail) @inline {
    if (g_aot_force_variadic_apply_arg_list_alloc_fail) return null;
    if (interp == null) return null;
    return lisp::make_cons(interp, arg, tail);
}

fn lisp::Value* aot_missing_closure_payload_error(lisp::Interp* interp) @inline {
    return aot_make_error_best_effort(interp, "aot closure apply: missing closure payload");
}

fn lisp::Value* aot_missing_variadic_closure_payload_error(lisp::Interp* interp) @inline {
    return aot_make_error_best_effort(interp, "aot variadic closure apply: missing closure payload");
}

fn bool retained_value_failed(lisp::Value* original, lisp::Value* stored) @inline {
    return lisp::env_barrier_promote_failed(original, stored);
}

fn AotCaptureRetention* capture_retention_begin() {
    AotCaptureRetention* retention = (AotCaptureRetention*)mem::malloc(AotCaptureRetention.sizeof);
    if (retention == null) return null;
    retention.retained_scope = null;
    retention.scope_retained = false;
    return retention;
}

fn void capture_retention_release(AotCaptureRetention* retention) {
    if (retention == null) return;
    if (retention.retained_scope != null && retention.scope_retained) {
        main::scope_release(retention.retained_scope);
    }
    retention.retained_scope = null;
    retention.scope_retained = false;
    mem::free(retention);
}

fn void aot_free_owned_data(void* data) {
    if (data == null) return;
    g_aot_owned_data_free_count++;
    mem::free(data);
}

fn bool aot_scope_chain_contains(main::ScopeRegion* scope, main::ScopeRegion* expected_ancestor) @inline {
    main::ScopeRegion* current = scope;
    while (current != null) {
        if (current == expected_ancestor) return true;
        current = current.parent;
    }
    return false;
}

fn void capture_retention_activate(AotCaptureRetention* retention) @inline {
    if (retention == null || retention.retained_scope == null || retention.scope_retained) return;
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (active_interp != null && aot_scope_chain_contains(active_interp.current_scope, retention.retained_scope)) return;
    main::scope_retain(retention.retained_scope);
    main::scope_detach_from_parent(retention.retained_scope);
    retention.scope_retained = true;
}

fn bool aot_capture_audit_reachable_temp(lisp::BoundaryGraphAuditResult audit) @inline {
    return audit.reason == lisp::BoundaryGraphAuditReason.BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_VALUE ||
        audit.reason == lisp::BoundaryGraphAuditReason.BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_ENV;
}

fn main::ScopeRegion* capture_value_retained_owner_scope(lisp::Value* value, lisp::Interp* interp, main::ScopeRegion* scope, bool* audit_failed = null) {
    if (audit_failed != null) *audit_failed = false;
    if (value == null || interp == null || interp.root_scope == null || scope == null) return null;
    if (g_aot_force_capture_reachability_audit_fail) {
        if (audit_failed != null) *audit_failed = true;
        return null;
    }
    main::ScopeRegion* owner = null;
    for (main::ScopeRegion* candidate = scope; candidate != null && candidate.parent != null; candidate = candidate.parent) {
        lisp::BoundaryGraphAuditResult audit = lisp::boundary_graph_audit_escape_reachability_result(
            value,
            candidate,
            false,
            null,
            interp.global_env);
        if (audit.ok) continue;
        if (aot_capture_audit_reachable_temp(audit)) {
            owner = candidate;
            continue;
        }
        if (audit_failed != null) *audit_failed = true;
        return null;
    }
    return owner;
}

fn lisp::Value* capture_retention_retain_value(AotCaptureRetention* retention, lisp::Value* value) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (retention == null) {
        return aot_make_error_best_effort(active_interp, "aot closure capture: missing retention owner");
    }
    if (active_interp == null || active_interp.root_scope == null || active_interp.current_scope == null) {
        return aot_make_error_best_effort(active_interp, "aot closure capture: runtime interpreter unavailable");
    }
    if (value == null || active_interp.current_scope == active_interp.root_scope) return null;
    bool audit_failed = false;
    main::ScopeRegion* retained_owner = capture_value_retained_owner_scope(value, active_interp, active_interp.current_scope, &audit_failed);
    if (retained_owner == null) {
        if (audit_failed) {
            return aot_make_error_best_effort(active_interp, "aot closure capture: failed to audit captured value reachability");
        }
        return null;
    }
    if (retention.retained_scope == retained_owner) return null;
    if (retention.retained_scope != null) {
        return aot_make_error_best_effort(active_interp, "aot closure capture: multiple retained scopes are unsupported");
    }
    retention.retained_scope = retained_owner;
    retention.scope_retained = false;
    return null;
}

fn void aot_closure_data_clear(AotClosureData* cd) @inline {
    if (cd == null) return;
    capture_retention_release(cd.retention);
    cd.retention = null;
    if (cd.owns_data && cd.data != null) {
        aot_free_owned_data(cd.data);
        cd.data = null;
    }
    cd.data = null;
    cd.invoke = null;
    cd.invoke_var = null;
    cd.is_variadic = false;
    cd.owns_data = false;
}

fn void aot_closure_data_dtor(void* ptr) {
    aot_closure_data_clear((AotClosureData*)ptr);
}

fn void* aot_closure_data_retain_user_data(void* ptr) {
    AotClosureData* cd = (AotClosureData*)ptr;
    if (cd == null) return null;
    capture_retention_activate(cd.retention);
    cd.ref_count++;
    return ptr;
}

fn void aot_closure_data_release_user_data(void* ptr) {
    AotClosureData* cd = (AotClosureData*)ptr;
    if (cd == null) return;
    if (cd.ref_count > 1) {
        cd.ref_count--;
        return;
    }
    cd.ref_count = 0;
    aot_closure_data_clear(cd);
    mem::free(cd);
}

fn lisp::Value* retain_root_value(lisp::Value* value, char[] failure_msg) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (active_interp == null || active_interp.root_scope == null || active_interp.current_scope == null) {
        return aot_make_error_best_effort(active_interp, failure_msg);
    }
    lisp::Value* stored = lisp::boundary_promote_to_root_site(value, active_interp, lisp::COPY_SITE_PROMOTE_TO_ROOT);
    if (retained_value_failed(value, stored)) {
        return stored != null ? stored : aot_make_error_best_effort(active_interp, failure_msg);
    }
    return stored;
}

fn AotMutableCell* make_mutable_cell_checked(lisp::Value* initial, lisp::Value** error_out = null) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (error_out != null) *error_out = null;
    if (active_interp == null || active_interp.root_scope == null || active_interp.current_scope == null) {
        if (error_out != null) *error_out = aot_make_error_best_effort(active_interp, "aot mutable-cell: runtime interpreter unavailable");
        return null;
    }
    AotMutableCell* cell = (AotMutableCell*)active_interp.root_scope.alloc(AotMutableCell.sizeof);
    if (cell == null) return null;
    lisp::Value* stored = retain_root_value(initial, "aot mutable-cell: failed to promote initial value");
    if (retained_value_failed(initial, stored)) {
        if (error_out != null) *error_out = stored;
        return null;
    }
    cell.value = stored;
    if (!mutable_cell_snapshot_append_new_cell(cell, stored, error_out)) {
        cell.value = null;
        return null;
    }
    cell.next = g_aot_mutable_cells;
    g_aot_mutable_cells = cell;
    g_aot_mutable_cell_count++;
    return cell;
}

fn AotMutableCell* make_mutable_cell(lisp::Value* initial) {
    return make_mutable_cell_checked(initial, null);
}

fn lisp::Value* mutable_cell_get(AotMutableCell* cell) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (cell == null) return aot_make_error_best_effort(active_interp, "aot mutable-cell: missing cell");
    return cell.value != null ? cell.value : lisp::make_nil(active_interp);
}

fn bool mutable_cell_set_checked(AotMutableCell* cell, lisp::Value* value, lisp::Value** error_out = null) {
    if (error_out != null) *error_out = null;
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (cell == null) {
        if (error_out != null) *error_out = aot_make_error_best_effort(active_interp, "aot mutable-cell: missing cell");
        return false;
    }
    lisp::Value* stored = retain_root_value(value, "aot mutable-cell: failed to promote stored value");
    if (retained_value_failed(value, stored)) {
        if (error_out != null) {
            *error_out = stored != null ? stored : aot_make_error_best_effort(active_interp, "aot mutable-cell: failed to promote stored value");
        }
        return false;
    }
    cell.value = stored;
    return true;
}

fn lisp::Value* mutable_cell_set(AotMutableCell* cell, lisp::Value* value) {
    lisp::Value* err = null;
    if (mutable_cell_set_checked(cell, value, &err)) return null;
    return err != null ? err : aot_make_error_best_effort(null, "aot mutable-cell: failed to set stored value");
}

fn bool mutable_cell_snapshot_begin(AotMutableCellSnapshot** snapshot_out, usz* count_out, lisp::Value** error_out = null) {
    if (snapshot_out == null || count_out == null) return false;
    *snapshot_out = null;
    *count_out = 0;
    if (error_out != null) *error_out = null;
    if (g_aot_mutable_cell_count == 0) return true;

    usz bytes = 0;
    if (!lisp::compiler_checked_mul_usz(AotMutableCellSnapshot.sizeof, g_aot_mutable_cell_count, &bytes)) {
        if (error_out != null) *error_out = aot_make_error_best_effort(null, "aot mutable-cell: reset snapshot size overflow");
        return false;
    }
    AotMutableCellSnapshot* snapshot = (AotMutableCellSnapshot*)mem::malloc(bytes);
    if (snapshot == null) {
        if (error_out != null) *error_out = aot_make_error_best_effort(null, "aot mutable-cell: out of memory while snapshotting reset boundary");
        return false;
    }

    usz count = 0;
    AotMutableCell* cell = g_aot_mutable_cells;
    while (cell != null && count < g_aot_mutable_cell_count) {
        snapshot[count].cell = cell;
        snapshot[count].value = cell.value;
        count++;
        cell = cell.next;
    }
    *snapshot_out = snapshot;
    *count_out = count;
    return true;
}

fn void mutable_cell_snapshot_replay(AotMutableCellSnapshot* snapshot, usz count) {
    if (snapshot == null) return;
    for (usz i = 0; i < count; i++) {
        if (snapshot[i].cell != null) snapshot[i].cell.value = snapshot[i].value;
    }
}

fn void mutable_cell_snapshot_restore(AotMutableCellSnapshot* snapshot, usz count) {
    mutable_cell_snapshot_replay(snapshot, count);
    if (snapshot == null) return;
    mem::free(snapshot);
}

fn lisp::Value* aot_closure_apply(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    TailStateSnapshot saved;
    tail_state_capture(&saved);
    g_tail_pending = false;
    g_tail_func = null;
    g_tail_arg = null;
    g_tail_argc = 0;
    g_tail_is_multi = false;

    lisp::Interp* active_interp = aot_resolve_interp(interp);
    if (active_interp == null) {
        tail_state_restore(&saved);
        (void)aot_make_error_best_effort(active_interp, "aot closure apply: runtime interpreter unavailable");
        return null;
    }

    AotClosureData* cd = (AotClosureData*)active_interp.prim_user_data;
    if (!aot_closure_payload_valid(cd)) {
        tail_state_restore(&saved);
        return aot_missing_closure_payload_error(active_interp);
    }
    lisp::Value* arg = args.len > 0 ? args[0] : lisp::make_nil(active_interp);
    lisp::Value* result = cd.invoke(cd.data, arg);
    while (g_tail_pending) {
        lisp::Value* backedge_error = tail_backedge_poll(active_interp);
        if (backedge_error != null) {
            g_tail_pending = false;
            result = backedge_error;
            break;
        }
        g_tail_pending = false;
        if (g_tail_is_multi) {
            result = apply_multi_once(g_tail_func, g_tail_arg, g_tail_argc);
        } else {
            result = invoke_once(g_tail_func, g_tail_arg);
        }
    }
    tail_state_restore(&saved);
    return result;
}

fn lisp::Value* aot_variadic_apply(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    TailStateSnapshot saved;
    tail_state_capture(&saved);
    g_tail_pending = false;
    g_tail_func = null;
    g_tail_arg = null;
    g_tail_argc = 0;
    g_tail_is_multi = false;

    lisp::Interp* active_interp = aot_resolve_interp(interp);
    if (active_interp == null) {
        tail_state_restore(&saved);
        (void)aot_make_error_best_effort(active_interp, "aot variadic apply: runtime interpreter unavailable");
        return null;
    }

    AotClosureData* cd = (AotClosureData*)active_interp.prim_user_data;
    if (!aot_variadic_payload_valid(cd)) {
        tail_state_restore(&saved);
        return aot_missing_variadic_closure_payload_error(active_interp);
    }
    lisp::Value* list = lisp::make_nil(active_interp);
    for (isz i = (isz)args.len - 1; i >= 0; i--) {
        lisp::Value* next = aot_prepend_arg_checked(active_interp, args[(usz)i], list);
        if (next == null) {
            tail_state_restore(&saved);
            return aot_make_error_best_effort(active_interp, "aot variadic apply: out of memory while building argument list");
        }
        list = next;
    }
    lisp::Value* result = cd.invoke_var(cd.data, list);
    while (g_tail_pending) {
        lisp::Value* backedge_error = tail_backedge_poll(active_interp);
        if (backedge_error != null) {
            g_tail_pending = false;
            result = backedge_error;
            break;
        }
        g_tail_pending = false;
        if (g_tail_is_multi) {
            result = apply_multi_once(g_tail_func, g_tail_arg, g_tail_argc);
        } else {
            result = invoke_once(g_tail_func, g_tail_arg);
        }
    }
    tail_state_restore(&saved);
    return result;
}

fn lisp::Value* make_closure_with_retention(void* data, AotClosureFn invoke, AotCaptureRetention* retention, bool owns_data = false) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (active_interp == null) {
        capture_retention_release(retention);
        if (owns_data && data != null) aot_free_owned_data(data);
        (void)aot_make_error_best_effort(active_interp, "aot make_closure: runtime interpreter unavailable");
        return null;
    }

    if (invoke == null) {
        capture_retention_release(retention);
        if (owns_data && data != null) aot_free_owned_data(data);
        return lisp::make_error(active_interp, "aot make_closure: missing invoke callback");
    }
    AotClosureData* cd = g_aot_force_closure_data_alloc_fail
        ? null
        : (AotClosureData*)active_interp.root_scope.alloc(AotClosureData.sizeof);
    if (cd == null) {
        capture_retention_release(retention);
        if (owns_data && data != null) aot_free_owned_data(data);
        return lisp::make_error(active_interp, "aot make_closure: out of memory while allocating closure payload");
    }
    cd.data = data; cd.invoke = invoke; cd.invoke_var = null; cd.is_variadic = false; cd.owns_data = owns_data; cd.retention = retention; cd.ref_count = 0;
    if (!main::scope_register_dtor(active_interp.root_scope, (void*)cd, &aot_closure_data_dtor)) {
        aot_closure_data_clear(cd);
        return lisp::make_error(active_interp, "aot make_closure: out of memory while registering closure payload destructor");
    }
    lisp::Value* v = g_aot_force_closure_primitive_alloc_fail
        ? null
        : lisp::make_primitive(active_interp, "aot-closure", &aot_closure_apply, 1);
    if (v == null) {
        aot_closure_data_clear(cd);
        return lisp::make_error(active_interp, "aot make_closure: out of memory while allocating primitive");
    }
    v.prim_val.user_data = (void*)cd;
    return v;
}

fn lisp::Value* make_closure(void* data, AotClosureFn invoke) {
    return make_closure_with_retention(data, invoke, null, false);
}

fn lisp::Value* make_generated_closure_with_retention(void* data, AotClosureFn invoke, AotCaptureRetention* retention) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (active_interp == null) {
        capture_retention_release(retention);
        if (data != null) aot_free_owned_data(data);
        (void)aot_make_error_best_effort(active_interp, "aot make_closure: runtime interpreter unavailable");
        return null;
    }

    if (invoke == null) {
        capture_retention_release(retention);
        if (data != null) aot_free_owned_data(data);
        return lisp::make_error(active_interp, "aot make_closure: missing invoke callback");
    }
    AotClosureData* cd = g_aot_force_closure_data_alloc_fail
        ? null
        : (AotClosureData*)mem::malloc(AotClosureData.sizeof);
    if (cd == null) {
        capture_retention_release(retention);
        if (data != null) aot_free_owned_data(data);
        return lisp::make_error(active_interp, "aot make_closure: out of memory while allocating closure payload");
    }
    cd.data = data; cd.invoke = invoke; cd.invoke_var = null; cd.is_variadic = false; cd.owns_data = true; cd.retention = retention; cd.ref_count = 1;
    if (g_aot_force_closure_primitive_alloc_fail) {
        aot_closure_data_release_user_data((void*)cd);
        return lisp::make_error(active_interp, "aot make_closure: out of memory while allocating primitive");
    }
    lisp::Value* v = lisp::make_primitive_with_user_data_in_scope(
        active_interp,
        active_interp.current_scope,
        "aot-closure",
        &aot_closure_apply,
        1,
        (void*)cd,
        &aot_closure_data_retain_user_data,
        &aot_closure_data_release_user_data);
    if (v == null) {
        return lisp::make_error(active_interp, "aot make_closure: out of memory while allocating primitive");
    }
    return v;
}

fn lisp::Value* make_variadic_closure_with_retention(void* data, AotVariadicFn invoke, AotCaptureRetention* retention, bool owns_data = false) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (active_interp == null) {
        capture_retention_release(retention);
        if (owns_data && data != null) aot_free_owned_data(data);
        (void)aot_make_error_best_effort(active_interp, "aot make_variadic_closure: runtime interpreter unavailable");
        return null;
    }

    if (invoke == null) {
        capture_retention_release(retention);
        if (owns_data && data != null) aot_free_owned_data(data);
        return lisp::make_error(active_interp, "aot make_variadic_closure: missing invoke callback");
    }
    AotClosureData* cd = g_aot_force_variadic_closure_data_alloc_fail
        ? null
        : (AotClosureData*)active_interp.root_scope.alloc(AotClosureData.sizeof);
    if (cd == null) {
        capture_retention_release(retention);
        if (owns_data && data != null) aot_free_owned_data(data);
        return lisp::make_error(active_interp, "aot make_variadic_closure: out of memory while allocating closure payload");
    }
    cd.data = data; cd.invoke = null; cd.invoke_var = invoke; cd.is_variadic = true; cd.owns_data = owns_data; cd.retention = retention; cd.ref_count = 0;
    if (!main::scope_register_dtor(active_interp.root_scope, (void*)cd, &aot_closure_data_dtor)) {
        aot_closure_data_clear(cd);
        return lisp::make_error(active_interp, "aot make_variadic_closure: out of memory while registering closure payload destructor");
    }
    lisp::Value* v = g_aot_force_variadic_closure_primitive_alloc_fail
        ? null
        : lisp::make_primitive(active_interp, "aot-closure-var", &aot_variadic_apply, -1);
    if (v == null) {
        aot_closure_data_clear(cd);
        return lisp::make_error(active_interp, "aot make_variadic_closure: out of memory while allocating primitive");
    }
    v.prim_val.user_data = (void*)cd;
    return v;
}

fn lisp::Value* make_variadic_closure(void* data, AotVariadicFn invoke) {
    return make_variadic_closure_with_retention(data, invoke, null, false);
}

fn lisp::Value* make_generated_variadic_closure_with_retention(void* data, AotVariadicFn invoke, AotCaptureRetention* retention) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (active_interp == null) {
        capture_retention_release(retention);
        if (data != null) aot_free_owned_data(data);
        (void)aot_make_error_best_effort(active_interp, "aot make_variadic_closure: runtime interpreter unavailable");
        return null;
    }

    if (invoke == null) {
        capture_retention_release(retention);
        if (data != null) aot_free_owned_data(data);
        return lisp::make_error(active_interp, "aot make_variadic_closure: missing invoke callback");
    }
    AotClosureData* cd = g_aot_force_variadic_closure_data_alloc_fail
        ? null
        : (AotClosureData*)mem::malloc(AotClosureData.sizeof);
    if (cd == null) {
        capture_retention_release(retention);
        if (data != null) aot_free_owned_data(data);
        return lisp::make_error(active_interp, "aot make_variadic_closure: out of memory while allocating closure payload");
    }
    cd.data = data; cd.invoke = null; cd.invoke_var = invoke; cd.is_variadic = true; cd.owns_data = true; cd.retention = retention; cd.ref_count = 1;
    if (g_aot_force_variadic_closure_primitive_alloc_fail) {
        aot_closure_data_release_user_data((void*)cd);
        return lisp::make_error(active_interp, "aot make_variadic_closure: out of memory while allocating primitive");
    }
    lisp::Value* v = lisp::make_primitive_with_user_data_in_scope(
        active_interp,
        active_interp.current_scope,
        "aot-closure-var",
        &aot_variadic_apply,
        -1,
        (void*)cd,
        &aot_closure_data_retain_user_data,
        &aot_closure_data_release_user_data);
    if (v == null) {
        return lisp::make_error(active_interp, "aot make_variadic_closure: out of memory while allocating primitive");
    }
    return v;
}
