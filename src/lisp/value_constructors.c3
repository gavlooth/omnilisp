module lisp;

import std::collections::list;
import std::io;
import main;

// =============================================================================
// SECTION 2: VALUE CONSTRUCTORS
// =============================================================================

bool g_warned_primitive_name_truncation = false;
int g_value_force_make_error_chars_alloc_fail_nth = 0;
int g_value_force_make_cons_or_error_alloc_fail_nth = 0;
int g_value_force_make_list1_alloc_fail_nth = 0;
int g_value_force_make_list2_alloc_fail_nth = 0;
int g_value_force_make_primitive_payload_alloc_fail_nth = 0;
int g_value_force_make_ffi_box_alloc_fail_nth = 0;

fn bool make_error_should_force_alloc_fail() @inline {
    if (g_value_force_make_error_chars_alloc_fail_nth <= 0) return false;
    g_value_force_make_error_chars_alloc_fail_nth--;
    return g_value_force_make_error_chars_alloc_fail_nth == 0;
}

fn bool make_list2_should_force_alloc_fail() @inline {
    if (g_value_force_make_list2_alloc_fail_nth <= 0) return false;
    g_value_force_make_list2_alloc_fail_nth--;
    return g_value_force_make_list2_alloc_fail_nth == 0;
}

fn bool make_list1_should_force_alloc_fail() @inline {
    if (g_value_force_make_list1_alloc_fail_nth <= 0) return false;
    g_value_force_make_list1_alloc_fail_nth--;
    return g_value_force_make_list1_alloc_fail_nth == 0;
}

fn bool make_cons_or_error_should_force_alloc_fail() @inline {
    if (g_value_force_make_cons_or_error_alloc_fail_nth <= 0) return false;
    g_value_force_make_cons_or_error_alloc_fail_nth--;
    return g_value_force_make_cons_or_error_alloc_fail_nth == 0;
}

fn bool make_primitive_payload_should_force_alloc_fail() @inline {
    if (g_value_force_make_primitive_payload_alloc_fail_nth <= 0) return false;
    g_value_force_make_primitive_payload_alloc_fail_nth--;
    return g_value_force_make_primitive_payload_alloc_fail_nth == 0;
}

fn bool make_ffi_box_should_force_alloc_fail() @inline {
    if (g_value_force_make_ffi_box_alloc_fail_nth <= 0) return false;
    g_value_force_make_ffi_box_alloc_fail_nth--;
    return g_value_force_make_ffi_box_alloc_fail_nth == 0;
}

fn Value* make_primitive(Interp* interp, char[] name, PrimitiveFn func, int arity) {
    // Allocate in root_scope — primitives are permanent (defined in global env).
    Primitive* prim = make_primitive_payload_should_force_alloc_fail()
        ? null
        : (Primitive*)mem::malloc(Primitive.sizeof);
    if (prim == null) {
        return null;
    }
    // Copy name into fixed buffer
    // Primitive.name is char[64]; keep one byte for the terminator.
    usz max_len = 63;
    usz len = name.len;
    if (len > max_len) {
        if (!g_warned_primitive_name_truncation) {
            io::eprintfn("WARNING: primitive names longer than %d bytes are truncated (further warnings suppressed)", max_len);
            g_warned_primitive_name_truncation = true;
        }
        len = max_len;
    }
    for (usz i = 0; i < len; i++) {
        prim.name[i] = name[i];
    }
    prim.name[len] = 0;
    prim.func = func;
    prim.arity = arity;
    prim.tag = 0;
    prim.user_data = null;

    Value* v = boundary_alloc_value_in_root(interp, true);
    if (v == null) {
        mem::free(prim);
        return null;
    }
    v.tag = PRIMITIVE;
    v.prim_val = prim;

    return v;
}

fn Value* make_ffi_handle(Interp* interp, void* handle, char[] name) {
    // Compatibility helper for global library handles:
    // Root lifetime, no finalizer; dlopen ownership is held by the shared registry.
    return make_ffi_handle_ex_with_descriptor(
        interp,
        handle,
        name,
        null,
        false,
        FOREIGN_RUNTIME_C_ABI,
        FOREIGN_HANDLE_LIBRARY,
        FOREIGN_CAP_LOAD | FOREIGN_CAP_RESOLVE | FOREIGN_CAP_REFLECT,
        true);
}

fn FfiHandle* make_ffi_box(void* handle, char[] name, FfiHandleFinalizer finalizer,
    bool free_lib_handle,
    ForeignRuntimeKind runtime_kind = FOREIGN_RUNTIME_NATIVE,
    ForeignHandleKind handle_kind = FOREIGN_HANDLE_OPAQUE,
    uint capabilities = 0,
    FfiHandleOwnership ownership = FFI_HANDLE_BORROWED,
    void* user_data = null,
    FfiBridgeBoundaryMode boundary_mode = FFI_BRIDGE_BOUNDARY_OPAQUE) {
    if (finalizer != null && free_lib_handle) {
        // Single release authority: finalizer path owns release policy.
        // Retain `free_lib_handle` only for plain resource pointers without
        // a finalizer callback.
        free_lib_handle = false;
    }

    FfiHandle* box = make_ffi_box_should_force_alloc_fail()
        ? null
        : (FfiHandle*)mem::malloc(FfiHandle.sizeof);
    if (box == null) return null;
    box.lib_handle = handle;
    box.user_data = user_data;
    box.refcount = 1;
    box.finalizer = finalizer;
    box.free_lib_handle = free_lib_handle;
    box.runtime_kind = runtime_kind;
    box.handle_kind = handle_kind;
    box.capabilities = capabilities;
    box.ownership = ownership;
    box.boundary_mode = boundary_mode;

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
    if (box.refcount == uint.max) return;
    box.refcount++;
}

fn bool ffi_handle_can_release_payload(FfiHandle* box) @inline {
    if (box == null) return false;
    if (box.handle_kind == FOREIGN_HANDLE_LIBRARY) return false;
    return box.finalizer != null || box.free_lib_handle;
}

fn bool ffi_handle_release_raw_allowed(void* payload, FfiHandleFinalizer finalizer,
    bool free_lib_handle, ForeignHandleKind handle_kind) @inline {
    if (payload == null) return false;
    if (handle_kind == FOREIGN_HANDLE_LIBRARY) return false;
    return finalizer != null || free_lib_handle;
}

fn void ffi_handle_release_raw_payload(void* payload, FfiHandleFinalizer finalizer,
    bool free_lib_handle, ForeignHandleKind handle_kind) {
    if (!ffi_handle_release_raw_allowed(payload, finalizer, free_lib_handle, handle_kind)) return;
    if (finalizer != null) {
        finalizer(payload);
        return;
    }
    if (free_lib_handle) {
        mem::free(payload);
    }
}

fn bool ffi_handle_release_payload(FfiHandle* box) {
    if (box == null) return false;
    if (box.lib_handle == null) return true;
    bool can_release = ffi_handle_can_release_payload(box);
    if (!can_release) return false;

    void* payload = box.user_data != null ? box.user_data : box.lib_handle;
    if (box.finalizer != null) {
        box.finalizer(payload);
    } else if (box.free_lib_handle) {
        mem::free(box.lib_handle);
    }
    box.lib_handle = null;
    box.capabilities = box.capabilities & ~FOREIGN_CAP_RELEASE;
    return true;
}

fn void ffi_handle_release(FfiHandle* box) {
    if (box == null) return;
    if (box.refcount > 1) {
        box.refcount--;
        return;
    }

    bool released = ffi_handle_release_payload(box);
    if (!released && box.handle_kind == FOREIGN_HANDLE_LIBRARY && box.lib_handle != null) {
        // Library handles are managed by the shared dlopen registry;
        // release the registry reference when the box is freed.
        ffi_dlopen_registry_release(box.lib_name[:box.name_len]);
        box.lib_handle = null;
        released = true;
    }
    if (!released && ffi_handle_can_release_payload(box)) {
        io::eprintfn("WARNING: FFI handle payload release failed for handle kind %d", (int)box.handle_kind);
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
    FfiHandleFinalizer finalizer, bool free_lib_handle, bool root_lifetime = false,
    bool* constructor_owns_handle = null, void* user_data = null) {
    FfiHandleOwnership ownership = FFI_HANDLE_BORROWED;
    if (finalizer != null) ownership = FFI_HANDLE_OWNED;
    if (free_lib_handle) ownership = FFI_HANDLE_OWNED;
    return make_ffi_handle_ex_with_descriptor(
        interp,
        handle,
        name,
        finalizer,
        free_lib_handle,
        FOREIGN_RUNTIME_NATIVE,
        FOREIGN_HANDLE_OPAQUE,
        (finalizer != null || free_lib_handle) ? FOREIGN_CAP_RELEASE : 0,
        root_lifetime,
        ownership,
        constructor_owns_handle,
        user_data);
}

fn Value* make_ffi_handle_ex_with_descriptor(
    Interp* interp,
    void* handle,
    char[] name,
    FfiHandleFinalizer finalizer,
    bool free_lib_handle,
    ForeignRuntimeKind runtime_kind,
    ForeignHandleKind handle_kind,
    uint capabilities,
    bool root_lifetime = false,
    FfiHandleOwnership ownership = FFI_HANDLE_BORROWED,
    bool* constructor_owns_handle = null,
    void* user_data = null
) {
    main::ScopeRegion* target_scope = root_lifetime ? interp.root_scope : interp.current_scope;
    if (constructor_owns_handle != null) {
        *constructor_owns_handle = ffi_handle_release_raw_allowed(handle, finalizer, free_lib_handle, handle_kind);
    }
    FfiHandle* box = make_ffi_box(handle, name, finalizer, free_lib_handle,
        runtime_kind, handle_kind, capabilities, ownership, user_data);
    if (box == null) {
        ffi_handle_release_raw_payload(handle, finalizer, free_lib_handle, handle_kind);
        return null;
    }
    Value* v = boundary_alloc_value_in_scope(interp, target_scope, true);
    if (v == null) {
        ffi_handle_release(box);
        return null;
    }
    v.tag = FFI_HANDLE;
    v.ffi_val = box;
    return v;
}

fn Value* make_error(Interp* interp, char[] msg) {
    Value* v = interp.alloc_value();
    v.tag = ERROR;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    usz len = msg.len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    char[] fallback = "out of memory while allocating error message";
    v.str_chars = (cap <= len || make_error_should_force_alloc_fail())
        ? null
        : (char*)mem::malloc(cap);
    if (v.str_chars == null) {
        v.str_len = fallback.len;
        v.str_chars = (char*)fallback.ptr;
        v.string_heap_owned = false;
        return v;
    }
    v.string_heap_owned = true;
    for (usz i = 0; i < len; i++) {
        v.str_chars[i] = msg[i];
    }
    v.str_chars[len] = 0;
    v.str_len = len;

    return v;
}

fn Value* make_list2_or_error(Interp* interp, Value* first, Value* second, char[] message) {
    if (make_list2_should_force_alloc_fail()) return make_error(interp, message);

    Value* tail = make_cons(interp, second, make_nil(interp));
    if (tail == null) return make_error(interp, message);
    if (tail.tag == ERROR) return tail;

    Value* out = make_cons(interp, first, tail);
    if (out == null) return make_error(interp, message);
    if (out.tag == ERROR) return out;
    return out;
}

fn Value* make_list1_or_error(Interp* interp, Value* first, char[] message) {
    if (make_list1_should_force_alloc_fail()) return make_error(interp, message);

    Value* out = make_cons(interp, first, make_nil(interp));
    if (out == null) return make_error(interp, message);
    if (out.tag == ERROR) return out;
    return out;
}

fn Value* make_cons_or_error(Interp* interp, Value* car, Value* cdr, char[] message) {
    if (make_cons_or_error_should_force_alloc_fail()) return make_error(interp, message);

    Value* out = make_cons(interp, car, cdr);
    if (out == null) return make_error(interp, message);
    if (out.tag == ERROR) return out;
    return out;
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
    Value* data = null,
    Value** failure_value = null
) {
    if (failure_value != null) *failure_value = null;
    if (interp == null || domain_sym == INVALID_SYMBOL_ID || code_sym == INVALID_SYMBOL_ID) return null;
    Value* payload = make_hashmap_no_raise(interp, 8);
    if (payload == null || payload.tag != HASHMAP || payload.hashmap_val == null) return null;
    bool keep_payload = false;
    HashMapSetRollback[4] payload_rollbacks = {};
    defer if (!keep_payload && payload != null && payload.hashmap_val != null) {
foreach (payload_rollback : payload_rollbacks) {
            hashmap_cleanup_rollback_record(&payload_rollback, interp);
}
        boundary_cleanup_materialized_value(null, payload, interp.root_scope);
    }

    SymbolId key_code = interp.symbols.intern("code");
    SymbolId key_message = interp.symbols.intern("message");
    SymbolId key_domain = interp.symbols.intern("domain");
    SymbolId key_data = interp.symbols.intern("data");
    if (key_code == INVALID_SYMBOL_ID ||
        key_message == INVALID_SYMBOL_ID ||
        key_domain == INVALID_SYMBOL_ID ||
        key_data == INVALID_SYMBOL_ID) {
        return null;
    }

    Value* code_key = make_symbol(interp, key_code);
    Value* message_key = make_symbol(interp, key_message);
    Value* domain_key = make_symbol(interp, key_domain);
    Value* data_key = make_symbol(interp, key_data);

    Value* code_val = make_symbol(interp, code_sym);
    Value* message_val = make_string(interp, message);
    Value* domain_val = make_symbol(interp, domain_sym);
    Value* data_val = data == null ? make_nil(interp) : data;
    if (message_val != null && message_val.tag == ERROR) {
        if (failure_value != null) *failure_value = message_val;
        return null;
    }
    if (data_val != null && data_val.tag == ERROR) {
        if (failure_value != null) *failure_value = data_val;
        return null;
    }

    if (!hashmap_set_checked_rollback_record(
        payload.hashmap_val,
        code_key,
        code_val,
        interp,
        &payload_rollbacks[0]
    )) return null;
    if (!hashmap_set_checked_rollback_record(
        payload.hashmap_val,
        message_key,
        message_val,
        interp,
        &payload_rollbacks[1]
    )) return null;
    if (!hashmap_set_checked_rollback_record(
        payload.hashmap_val,
        domain_key,
        domain_val,
        interp,
        &payload_rollbacks[2]
    )) return null;
    if (!hashmap_set_checked_rollback_record(
        payload.hashmap_val,
        data_key,
        data_val,
        interp,
        &payload_rollbacks[3]
    )) return null;

    keep_payload = true;
    return payload;
}

fn Value* make_raise_payload_rooted_no_data(
    Interp* interp,
    SymbolId domain_sym,
    SymbolId code_sym,
    char[] message,
    Value** failure_value = null
) {
    if (interp == null || interp.root_scope == null) return null;
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    defer interp.current_scope = saved_scope;
    return make_raise_payload(interp, domain_sym, code_sym, message, null, failure_value);
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
    if (domain_sym == INVALID_SYMBOL_ID || code_sym == INVALID_SYMBOL_ID) return null;
    return make_raise_payload(interp, domain_sym, code_sym, message, data);
}

fn bool raise_error_can_consume_payload(Interp* interp) @inline {
    if (interp == null || interp.handler_count == 0 || interp.flags.raise_pending) return false;
    SymbolId raise_sym = interp.sym_raise;
    for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
        EffectHandler* h = &interp.handler_stack[(usz)i];
        for (usz j = 0; j < h.clause_count; j++) {
            SymbolId ctag = h.tags != null ? h.tags[j] : h.clauses[j].effect_tag;
            if (ctag == raise_sym) return true;
        }
        if (h.strict_mode) break;
    }
    return false;
}

fn Value* raise_error_pending_impl(Interp* interp, char[] msg, Value* payload = null, bool payload_failed = false, bool payload_already_rooted = false) {
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
                if (payload_failed) {
                    return make_error(interp, "raise: failed to construct payload");
                }
                Value* promoted_payload = null;
                if (payload != null) {
                    if (payload_already_rooted) {
                        promoted_payload = payload;
                    } else {
                        promoted_payload = boundary_promote_to_root(payload, interp);
                        if (promoted_payload == null) {
                            return make_error(interp, "raise: failed to promote payload");
                        }
                        if (promoted_payload.tag == ERROR) return promoted_payload;
                    }
                }
                interp.flags.raise_pending = true;
                interp.raise_payload = promoted_payload;
                copy_pending_raise_message(interp, msg);
                return make_error(interp, msg);
            }
        }

        if (h.strict_mode) break;
    }

    // No raise handler found — fall back to V_ERROR
    return make_error(interp, msg);
}

fn Value* raise_error_with_payload_impl(
    Interp* interp,
    SymbolId domain_sym,
    SymbolId code_sym,
    char[] message,
    Value* data = null,
    bool allow_payloadless_on_payload_failure = false
) {
    if (!raise_error_can_consume_payload(interp)) {
        return raise_error_pending_impl(interp, message, null, false);
    }
    bool payload_already_rooted = data == null;
    Value* payload_failure = null;
    Value* payload = payload_already_rooted
        ? make_raise_payload_rooted_no_data(interp, domain_sym, code_sym, message, &payload_failure)
        : make_raise_payload(interp, domain_sym, code_sym, message, data, &payload_failure);
    if (payload == null && payload_failure != null && payload_failure.tag == ERROR) return payload_failure;
    bool payload_failed = payload == null && !allow_payloadless_on_payload_failure;
    return raise_error_pending_impl(interp, message, payload, payload_failed, payload_already_rooted);
}

fn Value* raise_error_with_payload(
    Interp* interp,
    SymbolId domain_sym,
    SymbolId code_sym,
    char[] message,
    Value* data = null
) {
    return raise_error_with_payload_impl(interp, domain_sym, code_sym, message, data, false);
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
    if (domain_sym == INVALID_SYMBOL_ID || code_sym == INVALID_SYMBOL_ID) {
        return raise_error_pending_impl(interp, message, null, true);
    }
    return raise_error_with_payload(interp, domain_sym, code_sym, message, data);
}

fn Value* raise_error_with_payloadless_fallback_names(
    Interp* interp,
    char[] domain_name,
    char[] code_name,
    char[] message
) @inline {
    SymbolId domain_sym = interp.symbols.intern(domain_name);
    SymbolId code_sym = interp.symbols.intern(code_name);
    if (domain_sym == INVALID_SYMBOL_ID || code_sym == INVALID_SYMBOL_ID) {
        return raise_error_pending_impl(interp, message, null, true);
    }
    return raise_error_with_payload_impl(interp, domain_sym, code_sym, message, null, true);
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
