module lisp::aot;

import std::core::mem;
import lisp;

alias AotClosureFn = fn lisp::Value*(void* data, lisp::Value* arg);
alias AotVariadicFn = fn lisp::Value*(void* data, lisp::Value* arg_list);

struct AotClosureData {
    void*         data;
    AotClosureFn  invoke;
    AotVariadicFn invoke_var;
    bool          is_variadic;
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
    return active_interp != null ? lisp::make_error(active_interp, message) : null;
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

fn lisp::Value* make_closure(void* data, AotClosureFn invoke) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (active_interp == null) return null;

    if (invoke == null) {
        return lisp::make_error(active_interp, "aot make_closure: missing invoke callback");
    }
    AotClosureData* cd = g_aot_force_closure_data_alloc_fail
        ? null
        : (AotClosureData*)active_interp.root_scope.alloc(AotClosureData.sizeof);
    if (cd == null) {
        return lisp::make_error(active_interp, "aot make_closure: out of memory while allocating closure payload");
    }
    cd.data = data; cd.invoke = invoke; cd.invoke_var = null; cd.is_variadic = false;
    lisp::Value* v = g_aot_force_closure_primitive_alloc_fail
        ? null
        : lisp::make_primitive(active_interp, "aot-closure", &aot_closure_apply, 1);
    if (v == null) {
        return lisp::make_error(active_interp, "aot make_closure: out of memory while allocating primitive");
    }
    v.prim_val.user_data = (void*)cd;
    return v;
}

fn lisp::Value* make_variadic_closure(void* data, AotVariadicFn invoke) {
    lisp::Interp* active_interp = aot_resolve_interp(null);
    if (active_interp == null) return null;

    if (invoke == null) {
        return lisp::make_error(active_interp, "aot make_variadic_closure: missing invoke callback");
    }
    AotClosureData* cd = g_aot_force_variadic_closure_data_alloc_fail
        ? null
        : (AotClosureData*)active_interp.root_scope.alloc(AotClosureData.sizeof);
    if (cd == null) {
        return lisp::make_error(active_interp, "aot make_variadic_closure: out of memory while allocating closure payload");
    }
    cd.data = data; cd.invoke = null; cd.invoke_var = invoke; cd.is_variadic = true;
    lisp::Value* v = g_aot_force_variadic_closure_primitive_alloc_fail
        ? null
        : lisp::make_primitive(active_interp, "aot-closure-var", &aot_variadic_apply, -1);
    if (v == null) {
        return lisp::make_error(active_interp, "aot make_variadic_closure: out of memory while allocating primitive");
    }
    v.prim_val.user_data = (void*)cd;
    return v;
}
