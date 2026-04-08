module lisp;

import std::io;

fn void runtime_cache_clear() {
    jit_require_owner_thread("runtime_cache_clear");
    for (usz i = 0; i < JIT_CACHE_SIZE; i++) {
        runtime_backend_clear_cache_slot(i);
    }
    runtime_backend_set_cache_entry_count(0);
}

fn RuntimeCompiledExpr runtime_cache_lookup_expr(Expr* expr, Interp* interp = null) {
    jit_require_owner_thread("runtime_cache_lookup_expr");
    usz hash = (usz)expr;
    usz idx = hash % JIT_CACHE_SIZE;
    ulong interp_serial = 0;
    bool require_owner_match = interp != null;
    if (require_owner_match && !runtime_attached_interp_serial(interp, &interp_serial)) {
        return runtime_compiled_expr_null();
    }
    for (usz probe = 0; probe < 16; probe++) {
        usz slot = (idx + probe) % JIT_CACHE_SIZE;
        if (runtime_backend_cache_expr_at(slot) == expr) {
            if (!require_owner_match) return runtime_backend_wrap_compiled_expr(runtime_backend_cache_compiled_at(slot));
            if (runtime_backend_cache_owner_interp_at(slot) != interp ||
                runtime_backend_cache_owner_attach_serial_at(slot) != interp_serial) {
                runtime_cache_clear();
                return runtime_compiled_expr_null();
            }
            RuntimeCompiledExpr compiled = runtime_backend_wrap_compiled_expr(runtime_backend_cache_compiled_at(slot));
            if (runtime_compiled_expr_is_null(compiled)) return runtime_compiled_expr_null();
            if (!runtime_compiled_expr_is_live_for_interp(compiled, interp)) {
                runtime_cache_clear();
                return runtime_compiled_expr_null();
            }
            return compiled;
        }
        if (runtime_backend_cache_expr_at(slot) == null) return runtime_compiled_expr_null();
    }
    return runtime_compiled_expr_null();
}

fn bool runtime_cache_find_slot(Expr* expr, usz* slot_out, bool* is_new_entry_out) {
    usz hash = (usz)expr;
    usz idx = hash % JIT_CACHE_SIZE;
    for (usz probe = 0; probe < 16; probe++) {
        usz slot = (idx + probe) % JIT_CACHE_SIZE;
        if (runtime_backend_cache_expr_at(slot) == expr) {
            *slot_out = slot;
            *is_new_entry_out = false;
            return true;
        }
        if (runtime_backend_cache_expr_at(slot) == null) {
            *slot_out = slot;
            *is_new_entry_out = true;
            return true;
        }
    }
    return false;
}

fn void runtime_cache_commit_slot(
    usz slot,
    Expr* expr,
    RuntimeCompiledExpr compiled,
    bool is_new_entry,
    Interp* owner_interp,
    ulong owner_attach_serial
) {
    runtime_backend_store_cache_slot(slot, expr, compiled, owner_interp, owner_attach_serial);
    if (is_new_entry) runtime_backend_increment_cache_entry_count();
}

fn void runtime_cache_store_expr(Expr* expr, RuntimeCompiledExpr compiled, Interp* interp = null) {
    jit_require_owner_thread("runtime_cache_store_expr");
    Interp* owner_interp = null;
    ulong owner_serial = 0;
    if (interp != null) {
        if (!runtime_attached_interp_serial(interp, &owner_serial)) return;
        owner_interp = interp;
    }
    usz slot = 0;
    bool is_new_entry = false;
    if (runtime_cache_find_slot(expr, &slot, &is_new_entry)) {
        if (is_new_entry && runtime_backend_cache_entry_count() >= JIT_CACHE_GC_THRESHOLD) {
            $if DEBUG_BUILD:
                io::eprintfn("[debug] runtime cache full (%d/%d), clearing", runtime_backend_cache_entry_count(), JIT_CACHE_SIZE);
            $endif
            runtime_cache_clear();
            if (!runtime_cache_find_slot(expr, &slot, &is_new_entry) || !is_new_entry) {
                $if DEBUG_BUILD:
                    io::eprintfn("[warn] runtime_cache_store_expr: cache clear retry failed for expr=%p", expr);
                $endif
                return;
            }
        }
        runtime_cache_commit_slot(slot, expr, compiled, is_new_entry, owner_interp, owner_serial);
        return;
    }

    $if DEBUG_BUILD:
        io::eprintfn("[debug] runtime cache probe window saturated for expr=%p, clearing", expr);
    $endif
    runtime_cache_clear();
    if (!runtime_cache_find_slot(expr, &slot, &is_new_entry) || !is_new_entry) {
        $if DEBUG_BUILD:
            io::eprintfn("[warn] runtime_cache_store_expr: cache clear retry failed for expr=%p after saturation", expr);
        $endif
        return;
    }
    runtime_cache_commit_slot(slot, expr, compiled, true, owner_interp, owner_serial);
}
