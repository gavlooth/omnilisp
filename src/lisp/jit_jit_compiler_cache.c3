module lisp;

struct JitCacheEntry {
    Expr* expr;     // cache key (pointer identity)
    JitCompiledFn compiled;
    Interp* owner_interp;
    ulong owner_attach_serial;
}

const usz JIT_CACHE_SIZE = 1024;
const usz JIT_CACHE_GC_THRESHOLD = 768;  // clear cache at 75% capacity
JitCacheEntry[JIT_CACHE_SIZE] g_jit_cache;
usz g_jit_cache_count = 0;

fn Expr* runtime_backend_cache_expr_at(usz slot) @inline {
    return g_jit_cache[slot].expr;
}

fn JitCompiledFn runtime_backend_cache_compiled_at(usz slot) @inline {
    return g_jit_cache[slot].compiled;
}

fn Interp* runtime_backend_cache_owner_interp_at(usz slot) @inline {
    return g_jit_cache[slot].owner_interp;
}

fn ulong runtime_backend_cache_owner_attach_serial_at(usz slot) @inline {
    return g_jit_cache[slot].owner_attach_serial;
}

fn void runtime_backend_clear_cache_slot(usz slot) @inline {
    g_jit_cache[slot].expr = null;
    g_jit_cache[slot].compiled = jit_compiled_fn_null();
    g_jit_cache[slot].owner_interp = null;
    g_jit_cache[slot].owner_attach_serial = 0;
}

fn void runtime_backend_store_cache_slot(
    usz slot,
    Expr* expr,
    RuntimeCompiledExpr compiled,
    Interp* owner_interp,
    ulong owner_attach_serial
) @inline {
    g_jit_cache[slot].expr = expr;
    g_jit_cache[slot].compiled = runtime_backend_unwrap_compiled_expr(compiled);
    g_jit_cache[slot].owner_interp = owner_interp;
    g_jit_cache[slot].owner_attach_serial = owner_attach_serial;
}

fn usz runtime_backend_cache_entry_count() @inline {
    return g_jit_cache_count;
}

fn void runtime_backend_set_cache_entry_count(usz count) @inline {
    g_jit_cache_count = count;
}

fn void runtime_backend_increment_cache_entry_count() @inline {
    g_jit_cache_count++;
}

<*
@require expr != null : "jit_cache_lookup: expr must not be null"
*>
fn JitCompiledFn jit_cache_lookup(Expr* expr, Interp* interp = null) {
    return runtime_backend_unwrap_compiled_expr(runtime_cache_lookup_expr(expr, interp));
}

<*
@require expr != null : "jit_cache_find_slot: expr must not be null"
@require slot_out != null : "jit_cache_find_slot: slot_out must not be null"
@require is_new_entry_out != null : "jit_cache_find_slot: is_new_entry_out must not be null"
*>
fn bool jit_cache_find_slot(Expr* expr, usz* slot_out, bool* is_new_entry_out) {
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

<*
@require expr != null : "jit_cache_commit_slot: expr must not be null"
@require compiled.code_fn != null : "jit_cache_commit_slot: compiled fn must not be null"
@require compiled.compile_nonce != 0 : "jit_cache_commit_slot: compiled nonce must not be zero"
*>
fn void jit_cache_commit_slot(
    usz slot,
    Expr* expr,
    JitCompiledFn compiled,
    bool is_new_entry,
    Interp* owner_interp,
    ulong owner_attach_serial
) {
    runtime_backend_store_cache_slot(
        slot,
        expr,
        runtime_backend_wrap_compiled_expr(compiled),
        owner_interp,
        owner_attach_serial
    );
    if (is_new_entry) g_jit_cache_count++;
}

<*
@require expr != null : "jit_cache_store: expr must not be null"
@require compiled.code_fn != null : "jit_cache_store: compiled fn must not be null"
@require compiled.compile_nonce != 0 : "jit_cache_store: compiled nonce must not be zero"
*>
fn void jit_cache_store(Expr* expr, JitCompiledFn compiled, Interp* interp = null) {
    runtime_cache_store_expr(expr, runtime_backend_wrap_compiled_expr(compiled), interp);
}
