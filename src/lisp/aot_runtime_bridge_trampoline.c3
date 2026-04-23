module lisp::aot;

import lisp;

// --- Tail-call trampoline state ---
tlocal lisp::Value* g_tail_func = null;
tlocal lisp::Value* g_tail_arg = null;
tlocal long g_tail_argc = 0;
tlocal bool g_tail_is_multi = false;
tlocal bool g_tail_pending = false;

struct TailStateSnapshot {
    lisp::Value* func;
    lisp::Value* arg;
    long argc;
    bool is_multi;
    bool pending;
}

fn void tail_state_capture(TailStateSnapshot* s) @inline {
    s.func = g_tail_func;
    s.arg = g_tail_arg;
    s.argc = g_tail_argc;
    s.is_multi = g_tail_is_multi;
    s.pending = g_tail_pending;
}

fn void tail_state_restore(TailStateSnapshot* s) @inline {
    g_tail_func = s.func;
    g_tail_arg = s.arg;
    g_tail_argc = s.argc;
    g_tail_is_multi = s.is_multi;
    g_tail_pending = s.pending;
}

/**
 * invoke_once — Single call without trampoline loop.
 * Used internally by invoke's trampoline.
 */
fn lisp::Value* invoke_once(lisp::Value* func, lisp::Value* arg) @inline {
    lisp::Interp* active_interp = aot_dispatch_interp();
    if (active_interp == null) {
        return aot_make_error_best_effort(null, "aot invoke: runtime interpreter unavailable");
    }

    // Fast path: AOT closure -> call directly, skipping JIT apply indirection
    if (func != null && func.tag == lisp::ValueTag.PRIMITIVE && func.prim_val != null) {
        lisp::PrimitiveFn pfn = func.prim_val.func;
        if (pfn == &aot_closure_apply) {
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (aot_closure_payload_valid(cd)) return cd.invoke(cd.data, arg);
            return aot_missing_closure_payload_error(active_interp);
        }
        if (pfn == &aot_variadic_apply) {
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (aot_variadic_payload_valid(cd)) {
                // Variadic closures expect a proper list - wrap single arg
                lisp::Value* arg_list = aot_make_single_arg_list(active_interp, arg);
                if (arg_list == null) {
                    return aot_make_error_best_effort(active_interp, "aot invoke: out of memory while building variadic argument list");
                }
                return cd.invoke_var(cd.data, arg_list);
            }
            return aot_missing_variadic_closure_payload_error(active_interp);
        }
    }
    // Fall through to JIT path for non-AOT values (real primitives, JIT closures, continuations)
    return lisp::jit_apply_value(func, arg, active_interp);
}

/**
 * apply_multi_once — Single multi-arg call without trampoline loop.
 * Used internally by apply_multi's trampoline.
 */
fn lisp::Value* apply_multi_once(lisp::Value* func, lisp::Value* arg_list, long argc) @inline {
    lisp::Interp* active_interp = aot_dispatch_interp();
    if (active_interp == null) {
        return aot_make_error_best_effort(null, "aot apply-multi: runtime interpreter unavailable");
    }
    if (argc < 0) {
        return aot_make_error_best_effort(active_interp, "aot apply-multi: negative arg count");
    }

    // Fast path: AOT closure -> call directly, skipping JIT apply_multi indirection
    if (func != null && func.tag == lisp::ValueTag.PRIMITIVE && func.prim_val != null) {
        lisp::PrimitiveFn pfn = func.prim_val.func;
        if (pfn == &aot_variadic_apply) {
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (aot_variadic_payload_valid(cd)) return cd.invoke_var(cd.data, arg_list);
            return aot_missing_variadic_closure_payload_error(active_interp);
        }
        if (pfn == &aot_closure_apply) {
            // Stay on the AOT trampoline path for curried closure application.
            // Routing through jit_apply_value re-enters aot_closure_apply recursively
            // and defeats tail-call flattening for self-recursive AOT closures.
            if (argc == 0) {
                return invoke(func, lisp::make_nil(active_interp));
            }
            lisp::Value* result = func;
            lisp::Value* curr = arg_list;
            for (long i = 0; i < argc; i++) {
                if (curr == null || curr.tag != lisp::ValueTag.CONS) {
                    return lisp::raise_error_with_payload_names(active_interp, "type", "type/arity", "arg list too short", null);
                }
                result = invoke(result, curr.cons_val.car);
                if (result != null && result.tag == lisp::ValueTag.ERROR) return result;
                curr = curr.cons_val.cdr;
            }
            return result;
        }
    }
    // Fall through to JIT path for non-AOT values
    return lisp::jit_apply_multi_args(active_interp, func, arg_list, (usz)argc);
}

/**
 * invoke — Call a function with trampoline for tail-call optimization.
 * After the initial call, if g_tail_pending is set (by invoke_tail/apply_multi_tail),
 * re-dispatch the stored tail call instead of recursing.
 */
fn lisp::Value* invoke(lisp::Value* func, lisp::Value* arg) {
    TailStateSnapshot saved;
    tail_state_capture(&saved);
    g_tail_pending = false;
    g_tail_func = null;
    g_tail_arg = null;
    g_tail_argc = 0;
    g_tail_is_multi = false;

    lisp::Value* result = invoke_once(func, arg);
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

/**
 * apply_multi — Call a function with multiple args, with trampoline for TCO.
 */
fn lisp::Value* apply_multi(lisp::Value* func, lisp::Value* arg_list, long argc) {
    lisp::Interp* active_interp = aot_dispatch_interp();
    if (active_interp == null) {
        return aot_make_error_best_effort(null, "aot apply-multi: runtime interpreter unavailable");
    }
    if (argc < 0) {
        return aot_make_error_best_effort(active_interp, "aot apply-multi: negative arg count");
    }
    TailStateSnapshot saved;
    tail_state_capture(&saved);
    g_tail_pending = false;
    g_tail_func = null;
    g_tail_arg = null;
    g_tail_argc = 0;
    g_tail_is_multi = false;

    lisp::Value* result = apply_multi_once(func, arg_list, argc);
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

/**
 * invoke_tail — Register a tail call (single-arg) and return immediately.
 * The caller's invoke/apply_multi trampoline will dispatch this.
 */
fn lisp::Value* invoke_tail(lisp::Value* func, lisp::Value* arg) @inline {
    lisp::Interp* active_interp = aot_dispatch_interp();
    if (active_interp == null) {
        return aot_make_error_best_effort(null, "aot invoke-tail: runtime interpreter unavailable");
    }
    g_tail_func = func;
    g_tail_arg = arg;
    g_tail_is_multi = false;
    g_tail_pending = true;
    return lisp::make_nil(active_interp);
}

/**
 * apply_multi_tail — Register a tail call (multi-arg) and return immediately.
 * The caller's invoke/apply_multi trampoline will dispatch this.
 */
fn lisp::Value* apply_multi_tail(lisp::Value* func, lisp::Value* arg_list, long argc) @inline {
    lisp::Interp* active_interp = aot_dispatch_interp();
    if (active_interp == null) {
        return aot_make_error_best_effort(null, "aot apply-multi-tail: runtime interpreter unavailable");
    }
    if (argc < 0) {
        return aot_make_error_best_effort(active_interp, "aot apply-multi-tail: negative arg count");
    }
    g_tail_func = func;
    g_tail_arg = arg_list;
    g_tail_argc = argc;
    g_tail_is_multi = true;
    g_tail_pending = true;
    return lisp::make_nil(active_interp);
}
