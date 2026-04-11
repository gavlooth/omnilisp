module lisp;

import std::core::mem;
import std::io;

usz g_signal_debug_dispatch_count = 0;

fn void signal_debug_reset_dispatch_count() {
    g_signal_debug_dispatch_count = 0;
}

fn usz signal_debug_dispatch_count() {
    return g_signal_debug_dispatch_count;
}

fn void dispatch_signal_callback(SignalHandle* sh, int signum, Interp* interp) {
    if (sh == null || !sh.open || sh.callback == null) return;
    if (!signal_callback_callable(sh.callback)) return;

    g_signal_debug_dispatch_count++;
    Value* arg = make_int(interp, (long)signum);
    Value* result = jit_apply_value(sh.callback, arg, interp);
    if (result != null && result.tag == ERROR) {
        // Keep signal delivery non-fatal: callback failures should not crash runtime loops.
        return;
    }
}

fn void drain_signal_handle_pending(SignalHandle* sh, Interp* interp) {
    if (sh == null || !sh.open || sh.uv_signal_watch == null) return;
    int pending = omni_uv_signal_poll_pending(sh.uv_signal_watch);
    if (pending <= 0) return;

    for (int i = 0; i < pending; i++) {
        dispatch_signal_callback(sh, sh.signum, interp);
    }
}

fn void dispatch_pending_signal_callbacks(Interp* interp) {
    if (interp == null) return;
    SignalHandle* sh = (SignalHandle*)interp.signal_handles_head;
    while (sh != null) {
        SignalHandle* next = sh.registry_next;
        drain_signal_handle_pending(sh, interp);
        sh = next;
    }
}

fn void close_fs_like_handle(Value* v, Interp* interp) {
    if (!ffi_handle_name_equals(v, "fs-handle")) return;
    Value*[1] close_args;
    close_args[0] = v;
    (void)prim_fs_close(close_args[..], null, interp);
}

fn char* copy_to_heap_c_string(char[] src) {
    if (src.len == usz.max) return null;
    usz cap = src.len + 1;
    char* out = (char*)mem::malloc(cap);
    if (out == null) return null;
    for (usz i = 0; i < src.len; i++) out[i] = src[i];
    out[src.len] = 0;
    return out;
}

fn char* copy_value_to_heap_c_string(Value* v, Interp* interp) {
    if (v == null) return null;
    if (is_string(v)) return copy_to_heap_c_string(v.str_chars[:v.str_len]);
    if (is_symbol(v)) return copy_to_heap_c_string(interp.symbols.get_name(v.sym_val));
    return null;
}

fn void free_heap_c_string_items(char** items, usz len) {
    if (items == null) return;
    for (usz i = 0; i < len; i++) {
        if (items[i] != null) mem::free(items[i]);
    }
    mem::free(items);
}

fn Value* value_to_process_handle_arg(Value* v, Interp* interp) {
    if (v == null) return null;
    if (ffi_handle_name_equals(v, "process-handle")) return v;
    if (v.tag == HASHMAP && v.hashmap_val != null) {
        SymbolId handle_sym = interp.symbols.intern("handle");
        if (handle_sym == INVALID_SYMBOL_ID) return null;
        Value* handle_key = make_symbol(interp, handle_sym);
        return hashmap_get(v.hashmap_val, handle_key);
    }
    return null;
}

fn Value* collect_string_sequence(
    Value* seq,
    char*** out_items,
    usz* out_len,
    Interp* interp,
    char[] code,
    char[] message
) {
    *out_items = null;
    *out_len = 0;
    if (seq == null || is_nil(seq)) return null;

    usz count = 0;
    if (seq.tag == ARRAY && seq.array_val != null) {
        count = seq.array_val.length;
    } else if (is_cons(seq)) {
        Value* cur = seq;
        while (is_cons(cur)) {
            if (count == usz.max) return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
            count++;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return io_raise(interp, code, message);
    } else {
        return io_raise(interp, code, message);
    }

    if (count == 0) return null;
    if (count > usz.max / char*.sizeof) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }

    char** items = (char**)mem::malloc(char*.sizeof * count);
    if (items == null) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    for (usz i = 0; i < count; i++) items[i] = null;

    if (seq.tag == ARRAY && seq.array_val != null) {
        for (usz i = 0; i < count; i++) {
            Value* it = seq.array_val.items[i];
            if (!(is_string(it) || is_symbol(it))) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, code, message);
            }
            char* copied = copy_value_to_heap_c_string(it, interp);
            if (copied == null) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
            }
            items[i] = copied;
        }
    } else {
        Value* cur = seq;
        usz i = 0;
        while (is_cons(cur) && i < count) {
            Value* it = car(cur);
            if (!(is_string(it) || is_symbol(it))) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, code, message);
            }
            char* copied = copy_value_to_heap_c_string(it, interp);
            if (copied == null) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
            }
            items[i] = copied;
            i++;
            cur = cdr(cur);
        }
    }

    *out_items = items;
    *out_len = count;
    return null;
}
