module lisp;

import std::collections::list;
import std::io;
import main;

// =============================================================================
// SECTION 2: VALUE CONSTRUCTORS
// =============================================================================

bool g_warned_primitive_name_truncation = false;

fn Value* make_primitive(Interp* interp, char[] name, PrimitiveFn func, int arity) {
    // Allocate in root_scope — primitives are permanent (defined in global env).
    Value* v = boundary_alloc_value_in_root(interp, true);
    v.tag = PRIMITIVE;
    v.prim_val = (Primitive*)mem::malloc(Primitive.sizeof);
    if (v.prim_val == null) return null;
    // Copy name into fixed buffer
    usz len = name.len;
    if (len > 31) {
        if (!g_warned_primitive_name_truncation) {
            io::eprintfn("WARNING: primitive names longer than 31 bytes are truncated (further warnings suppressed)");
            g_warned_primitive_name_truncation = true;
        }
        len = 31;
    }
    for (usz i = 0; i < len; i++) {
        v.prim_val.name[i] = name[i];
    }
    v.prim_val.name[len] = 0;
    v.prim_val.func = func;
    v.prim_val.arity = arity;
    v.prim_val.tag = 0;
    v.prim_val.user_data = null;

    return v;
}

fn Value* make_ffi_handle(Interp* interp, void* handle, char[] name) {
    // Compatibility helper for global library handles:
    // root lifetime, no finalizer, don't free dlopen handle.
    return make_ffi_handle_ex(interp, handle, name, null, false, true);
}

fn FfiHandle* make_ffi_box(void* handle, char[] name, FfiHandleFinalizer finalizer,
    bool free_lib_handle) {
    FfiHandle* box = (FfiHandle*)mem::malloc(FfiHandle.sizeof);
    if (box == null) return null;
    box.lib_handle = handle;
    box.refcount = 1;
    box.finalizer = finalizer;
    box.free_lib_handle = free_lib_handle;

    usz len = name.len;
    if (len > 255) {
        io::eprintfn("WARNING: FFI handle name truncated from %d to 255 bytes", name.len);
        len = 255;
    }
    for (usz i = 0; i < len; i++) {
        box.lib_name[i] = name[i];
    }
    box.lib_name[len] = 0;
    box.name_len = len;
    return box;
}

fn void ffi_handle_retain(FfiHandle* box) {
    if (box == null) return;
    box.refcount++;
}

fn void ffi_handle_release(FfiHandle* box) {
    if (box == null) return;
    if (box.refcount > 1) {
        box.refcount--;
        return;
    }

    if (box.finalizer != null && box.lib_handle != null) {
        box.finalizer(box.lib_handle);
    }
    if (box.free_lib_handle && box.lib_handle != null) {
        mem::free(box.lib_handle);
        box.lib_handle = null;
    }
    mem::free(box);
}

// Instance scope-ownership management
fn void instance_retain(Instance* inst) @inline {
    if (inst == null || inst.owner_scope == null) return;
    main::scope_retain(inst.owner_scope);
}

fn void instance_release(Instance* inst) @inline {
    if (inst == null || inst.owner_scope == null) return;
    main::scope_release(inst.owner_scope);
}

fn Value* make_ffi_handle_ex(Interp* interp, void* handle, char[] name,
    FfiHandleFinalizer finalizer, bool free_lib_handle, bool root_lifetime = false) {
    main::ScopeRegion* target_scope = root_lifetime ? interp.root_scope : interp.current_scope;
    Value* v = boundary_alloc_value_in_scope(interp, target_scope, true);
    v.tag = FFI_HANDLE;
    v.ffi_val = make_ffi_box(handle, name, finalizer, free_lib_handle);
    if (v.ffi_val == null) {
        return null;
    }
    return v;
}

fn Value* make_error(Interp* interp, char[] msg) {
    Value* v = interp.alloc_value();
    v.tag = ERROR;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    usz len = msg.len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    v.str_chars = (char*)mem::malloc(cap);
    for (usz i = 0; i < len; i++) {
        v.str_chars[i] = msg[i];
    }
    v.str_chars[len] = 0;
    v.str_len = len;

    return v;
}

fn void copy_pending_raise_message(Interp* interp, char[] msg) @inline {
    usz copy_len = msg.len;
    if (copy_len > 255) copy_len = 255;
    for (usz ci = 0; ci < copy_len; ci++) interp.raise_msg[ci] = msg[ci];
    interp.raise_msg[copy_len] = 0;
    interp.raise_msg_len = copy_len;
}

fn Value* make_raise_payload(
    Interp* interp,
    SymbolId domain_sym,
    SymbolId code_sym,
    char[] message,
    Value* data = null
) {
    Value* payload = make_hashmap(interp, 8);
    if (payload == null || payload.tag != HASHMAP) return null;

    SymbolId key_code = interp.symbols.intern("code");
    SymbolId key_message = interp.symbols.intern("message");
    SymbolId key_domain = interp.symbols.intern("domain");
    SymbolId key_data = interp.symbols.intern("data");

    Value* code_key = make_symbol(interp, key_code);
    Value* message_key = make_symbol(interp, key_message);
    Value* domain_key = make_symbol(interp, key_domain);
    Value* data_key = make_symbol(interp, key_data);

    Value* code_val = make_symbol(interp, code_sym);
    Value* message_val = make_string(interp, message);
    Value* domain_val = make_symbol(interp, domain_sym);
    Value* data_val = data == null ? make_nil(interp) : data;

    hashmap_set(payload.hashmap_val, code_key, code_val, interp);
    hashmap_set(payload.hashmap_val, message_key, message_val, interp);
    hashmap_set(payload.hashmap_val, domain_key, domain_val, interp);
    hashmap_set(payload.hashmap_val, data_key, data_val, interp);

    return payload;
}

fn Value* make_raise_payload_names(
    Interp* interp,
    char[] domain_name,
    char[] code_name,
    char[] message,
    Value* data = null
) @inline {
    SymbolId domain_sym = interp.symbols.intern(domain_name);
    SymbolId code_sym = interp.symbols.intern(code_name);
    return make_raise_payload(interp, domain_sym, code_sym, message, data);
}

fn Value* raise_error_pending_impl(Interp* interp, char[] msg, Value* payload = null) {
    // Fast path: no handlers at all → just return error
    if (interp.handler_count == 0) {
        return make_error(interp, msg);
    }

    // If a raise is already pending, don't overwrite it (error re-propagation)
    if (interp.flags.raise_pending) {
        return make_error(interp, msg);
    }

    SymbolId raise_sym = interp.sym_raise;

    // Search handler stack from top (most recent) to bottom
    for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
        EffectHandler* h = &interp.handler_stack[(usz)i];

        for (usz j = 0; j < h.clause_count; j++) {
            SymbolId ctag = h.tags != null ? h.tags[j] : h.clauses[j].effect_tag;
            if (ctag == raise_sym) {
                // Found a raise handler — set pending flag for jit_handle_impl to dispatch.
                // We can't safely call jit_eval from within a primitive call chain,
                // so we store payload/message and let the handle block dispatch.
                interp.flags.raise_pending = true;
                interp.raise_payload = payload == null ? null : boundary_promote_to_root(payload, interp);
                copy_pending_raise_message(interp, msg);
                return make_error(interp, msg);
            }
        }

        if (h.strict_mode) break;
    }

    // No raise handler found — fall back to V_ERROR
    return make_error(interp, msg);
}

fn Value* raise_error_with_payload(
    Interp* interp,
    SymbolId domain_sym,
    SymbolId code_sym,
    char[] message,
    Value* data = null
) {
    Value* payload = make_raise_payload(interp, domain_sym, code_sym, message, data);
    return raise_error_pending_impl(interp, message, payload);
}

fn Value* raise_error_with_payload_names(
    Interp* interp,
    char[] domain_name,
    char[] code_name,
    char[] message,
    Value* data = null
) @inline {
    SymbolId domain_sym = interp.symbols.intern(domain_name);
    SymbolId code_sym = interp.symbols.intern(code_name);
    return raise_error_with_payload(interp, domain_sym, code_sym, message, data);
}

/**
 * raise_error — Recoverable error via algebraic effect system.
 *
 * If a `raise` handler is on the handler stack, sets raise_pending flag and
 * returns V_ERROR for stack unwinding. If no handler, falls back to V_ERROR
 * (same as make_error).
 *
 * Use make_error() for fatal/internal errors only (stack overflow, invariants).
 */
fn Value* raise_error(Interp* interp, char[] msg) {
    return raise_error_pending_impl(interp, msg, null);
}

