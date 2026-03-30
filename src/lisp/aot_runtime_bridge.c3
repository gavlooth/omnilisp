module lisp::aot;

import std::io;
import std::core::mem;
import lisp;

alias AotClosureFn = fn lisp::Value*(void* data, lisp::Value* arg);
alias AotVariadicFn = fn lisp::Value*(void* data, lisp::Value* arg_list);

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
    // Fast path: AOT closure → call directly, skipping JIT apply indirection
    if (func != null && func.tag == lisp::ValueTag.PRIMITIVE && func.prim_val != null) {
        lisp::PrimitiveFn pfn = func.prim_val.func;
        if (pfn == &aot_closure_apply) {
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (cd != null) return cd.invoke(cd.data, arg);
        }
        if (pfn == &aot_variadic_apply) {
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (cd != null) {
                // Variadic closures expect a proper list — wrap single arg
                lisp::Value* arg_list = lisp::make_cons(g_aot_interp, arg, lisp::make_nil(g_aot_interp));
                return cd.invoke_var(cd.data, arg_list);
            }
        }
    }
    // Fall through to JIT path for non-AOT values (real primitives, JIT closures, continuations)
    return lisp::jit_apply_value(func, arg, g_aot_interp);
}

/**
 * apply_multi_once — Single multi-arg call without trampoline loop.
 * Used internally by apply_multi's trampoline.
 */
fn lisp::Value* apply_multi_once(lisp::Value* func, lisp::Value* arg_list, long argc) @inline {
    // Fast path: AOT closure → call directly, skipping JIT apply_multi indirection
    if (func != null && func.tag == lisp::ValueTag.PRIMITIVE && func.prim_val != null) {
        lisp::PrimitiveFn pfn = func.prim_val.func;
        if (pfn == &aot_variadic_apply) {
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (cd != null) return cd.invoke_var(cd.data, arg_list);
        }
        if (pfn == &aot_closure_apply) {
            // Non-variadic AOT closure called with multi-arg: take first arg
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (cd != null) {
                lisp::Value* first_arg = (arg_list != null && arg_list.tag == lisp::ValueTag.CONS)
                    ? arg_list.cons_val.car : lisp::make_nil(g_aot_interp);
                return cd.invoke(cd.data, first_arg);
            }
        }
    }
    // Fall through to JIT path for non-AOT values
    return lisp::jit_apply_multi_args(g_aot_interp, func, arg_list, (usz)argc);
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
    g_tail_func = func;
    g_tail_arg = arg;
    g_tail_is_multi = false;
    g_tail_pending = true;
    return lisp::make_nil(g_aot_interp);
}

/**
 * apply_multi_tail — Register a tail call (multi-arg) and return immediately.
 * The caller's invoke/apply_multi trampoline will dispatch this.
 */
fn lisp::Value* apply_multi_tail(lisp::Value* func, lisp::Value* arg_list, long argc) @inline {
    g_tail_func = func;
    g_tail_arg = arg_list;
    g_tail_argc = argc;
    g_tail_is_multi = true;
    g_tail_pending = true;
    return lisp::make_nil(g_aot_interp);
}

struct AotClosureData {
    void*         data;
    AotClosureFn  invoke;
    AotVariadicFn invoke_var;
    bool          is_variadic;
}

fn lisp::Value* aot_closure_apply(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    TailStateSnapshot saved;
    tail_state_capture(&saved);
    g_tail_pending = false;
    g_tail_func = null;
    g_tail_arg = null;
    g_tail_argc = 0;
    g_tail_is_multi = false;

    AotClosureData* cd = (AotClosureData*)interp.prim_user_data;
    lisp::Value* arg = args.len > 0 ? args[0] : lisp::make_nil(interp);
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

    AotClosureData* cd = (AotClosureData*)interp.prim_user_data;
    lisp::Value* list = lisp::make_nil(interp);
    for (isz i = (isz)args.len - 1; i >= 0; i--) list = lisp::make_cons(interp, args[(usz)i], list);
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
    AotClosureData* cd = (AotClosureData*)mem::malloc(AotClosureData.sizeof);
    assert(cd != null, "aot make_closure: closure data alloc failed");
    cd.data = data; cd.invoke = invoke; cd.invoke_var = null; cd.is_variadic = false;
    lisp::Value* v = lisp::make_primitive(g_aot_interp, "aot-closure", &aot_closure_apply, 1);
    assert(v != null, "aot make_closure: primitive alloc failed");
    v.prim_val.user_data = (void*)cd;
    return v;
}

fn lisp::Value* make_variadic_closure(void* data, AotVariadicFn invoke) {
    AotClosureData* cd = (AotClosureData*)mem::malloc(AotClosureData.sizeof);
    assert(cd != null, "aot make_variadic_closure: closure data alloc failed");
    cd.data = data; cd.invoke = null; cd.invoke_var = invoke; cd.is_variadic = true;
    lisp::Value* v = lisp::make_primitive(g_aot_interp, "aot-closure-var", &aot_variadic_apply, -1);
    assert(v != null, "aot make_variadic_closure: primitive alloc failed");
    v.prim_val.user_data = (void*)cd;
    return v;
}

fn bool is_truthy(lisp::Value* v) @inline {
    if (v == null) return false;
    if (v.tag == lisp::ValueTag.NIL) return false;
    if (v.tag == lisp::ValueTag.SYMBOL && v.sym_val == g_aot_interp.sym_false) return false;
    return true;
}

fn bool is_error(lisp::Value* v) @inline {
    return v != null && v.tag == lisp::ValueTag.ERROR;
}

fn bool guard_pred_accepts_scrutinee(lisp::Value* pred) @inline {
    if (pred == null) return false;
    switch (pred.tag) {
        case lisp::ValueTag.CLOSURE:
        case lisp::ValueTag.PRIMITIVE:
        case lisp::ValueTag.PARTIAL_PRIM:
        case lisp::ValueTag.CONTINUATION:
        case lisp::ValueTag.METHOD_TABLE:
            return true;
        default:
            return false;
    }
}

fn lisp::Value* match_guard_eval(lisp::Value* pred, lisp::Value* val) {
    if (pred == null || is_error(pred)) return pred;
    if (!guard_pred_accepts_scrutinee(pred)) return pred;
    lisp::Value* result = invoke(pred, val);
    if (result == null) return lisp::make_nil(g_aot_interp);
    return result;
}

fn bool values_equal(lisp::Value* a, lisp::Value* b) {
    return lisp::values_equal(a, b);
}

fn bool is_cons(lisp::Value* v) @inline {
    return v != null && v.tag == lisp::ValueTag.CONS;
}

fn bool is_dict(lisp::Value* v) @inline {
    return v != null && v.tag == lisp::ValueTag.HASHMAP && v.hashmap_val != null;
}

fn bool match_var_pattern(lisp::Value* v, char[] name) {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(name);
    lisp::TypeId tid = g_aot_interp.types.lookup(sid, &g_aot_interp.symbols);
    if (tid == lisp::INVALID_TYPE_ID) return true;

    lisp::TypeInfo* ti = g_aot_interp.types.get(tid);
    if (ti == null || (ti.kind != TK_CONCRETE && ti.kind != TK_UNION) || ti.field_count != 0) {
        return true;
    }
    return v != null && v.tag == lisp::ValueTag.INSTANCE && v.instance_val != null && v.instance_val.type_id == tid;
}

fn bool match_constructor(lisp::Value* v, char[] ctor_name, long field_count) {
    if (field_count < 0) return false;
    if (v == null || v.tag != lisp::ValueTag.INSTANCE || v.instance_val == null) return false;

    lisp::SymbolId sid = g_aot_interp.symbols.intern(ctor_name);
    lisp::TypeId tid = g_aot_interp.types.lookup(sid, &g_aot_interp.symbols);
    if (tid == lisp::INVALID_TYPE_ID || v.instance_val.type_id != tid) return false;
    return v.instance_val.field_count == (usz)field_count;
}

fn lisp::Value* instance_field(lisp::Value* v, long idx) {
    if (idx < 0) return lisp::make_nil(g_aot_interp);
    if (v == null || v.tag != lisp::ValueTag.INSTANCE || v.instance_val == null) return lisp::make_nil(g_aot_interp);
    usz field_index = (usz)idx;
    if (field_index >= v.instance_val.field_count) return lisp::make_nil(g_aot_interp);
    return &v.instance_val.fields[field_index];
}

fn lisp::Value* dict_get_symbol(lisp::Value* v, char[] key) {
    if (!is_dict(v)) return lisp::make_nil(g_aot_interp);
    lisp::Value* lookup_key = lisp::make_symbol(g_aot_interp, g_aot_interp.symbols.intern(key));
    lisp::Value* found = lisp::hashmap_get(v.hashmap_val, lookup_key);
    return found != null ? found : lisp::make_nil(g_aot_interp);
}

fn usz seq_length(lisp::Value* v) {
    if (v != null && v.tag == lisp::ValueTag.ARRAY && v.array_val != null) {
        return v.array_val.length;
    }
    return lisp::get_list_length(v);
}

fn lisp::Value* seq_nth(lisp::Value* v, long n) {
    if (n < 0) return lisp::make_nil(g_aot_interp);
    if (v != null && v.tag == lisp::ValueTag.ARRAY && v.array_val != null) {
        usz idx = (usz)n;
        if (idx >= v.array_val.length) return lisp::make_nil(g_aot_interp);
        return v.array_val.items[idx];
    }
    lisp::Value* out = lisp::get_list_nth(v, (usz)n);
    return out != null ? out : lisp::make_nil(g_aot_interp);
}

fn lisp::Value* seq_rest(lisp::Value* v, long n) {
    if (n < 0) return lisp::make_nil(g_aot_interp);
    return lisp::get_list_rest(v, (usz)n, g_aot_interp);
}

fn bool is_list(lisp::Value* v) {
    return lisp::is_list(v);
}

fn usz list_length(lisp::Value* v) {
    return lisp::list_length(v);
}

fn lisp::Value* list_nth(lisp::Value* v, long n) {
    return lisp::get_list_nth(v, (usz)n);
}

fn lisp::Value* list_rest(lisp::Value* v, long n) {
    return lisp::get_list_rest(v, (usz)n, g_aot_interp);
}

fn lisp::Value* lookup_prim(char[] name) {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(name);
    lisp::Value* v = g_aot_interp.global_env.lookup(sid);
    if (v != null) return v;
    return lisp::make_nil(g_aot_interp);
}

// Legacy/debug bridge only. Normal compiler lowering must not emit this path.
// Kept for explicit manual debugging of serialized forms.
// =============================================================================
// Variable Table Operations
// =============================================================================

fn void define_var(char[] name, lisp::Value* val) {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(name);
    lisp::env_define_with_barrier(g_aot_interp.global_env, sid, val, g_aot_interp);
}

fn lisp::Value* lookup_var(char[] name) {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(name);
    lisp::Value* v = g_aot_interp.global_env.lookup(sid);
    return v != null ? v : lisp::make_nil(g_aot_interp);
}

fn lisp::Value* import_module(char[] name, char[] path, bool has_path, bool import_all) {
    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_IMPORT;
    expr.import_expr.name = aot_symbol_from_text(name);
    expr.import_expr.has_path = has_path;
    expr.import_expr.path = has_path ? path.ptr : null;
    expr.import_expr.path_len = has_path ? path.len : 0;
    expr.import_expr.imports = null;
    expr.import_expr.aliases = null;
    expr.import_expr.import_count = 0;
    expr.import_expr.import_capacity = 0;
    expr.import_expr.import_all = import_all;

    lisp::EvalResult r = lisp::jit_eval_import_impl(&expr, g_aot_interp.global_env, g_aot_interp);
    if (r.error.has_error) {
        char[768] msg_buf;
        char[] err = r.error.message[:256];
        if (has_path) {
            if (err.len > 0) {
                if (try msg = io::bprintf(&msg_buf, "aot import failed for '%s' from '%s': %s", (ZString)name, (ZString)path, (ZString)err)) {
                    return lisp::make_error(g_aot_interp, msg);
                }
            } else {
                if (try msg = io::bprintf(&msg_buf, "aot import failed for '%s' from '%s'", (ZString)name, (ZString)path)) {
                    return lisp::make_error(g_aot_interp, msg);
                }
            }
        } else {
            if (err.len > 0) {
                if (try msg = io::bprintf(&msg_buf, "aot import failed for '%s': %s", (ZString)name, (ZString)err)) {
                    return lisp::make_error(g_aot_interp, msg);
                }
            } else {
                if (try msg = io::bprintf(&msg_buf, "aot import failed for '%s'", (ZString)name)) {
                    return lisp::make_error(g_aot_interp, msg);
                }
            }
        }
        return lisp::make_error(g_aot_interp, "aot import failed");
    }
    lisp::Value* bound = g_aot_interp.global_env.lookup(expr.import_expr.name);
    return bound != null ? bound : lisp::make_void(g_aot_interp);
}

fn lisp::Value* field_access(lisp::Value* current, lisp::Value* field_name) {
    if (current == null) {
        return lisp::make_error(g_aot_interp, "aot field_access: null target");
    }
    if (current.tag == lisp::ValueTag.ERROR) return current;
    if (field_name == null) {
        return lisp::make_error(g_aot_interp, "aot field_access: missing field name");
    }

    char[] field_text;
    switch (field_name.tag) {
        case lisp::ValueTag.STRING:
            field_text = field_name.str_chars[:field_name.str_len];
        case lisp::ValueTag.SYMBOL:
            field_text = g_aot_interp.symbols.get_name(field_name.sym_val);
        default:
            return lisp::make_error(g_aot_interp, "aot field_access: field name must be string or symbol");
    }

    lisp::SymbolId key = aot_symbol_from_text(field_text);
    lisp::EvalResult step = lisp::eval_path_step(current, key, g_aot_interp);
    if (step.error.has_error) {
        return lisp::make_error(g_aot_interp, step.error.message[:256]);
    }
    return step.value != null ? step.value : lisp::make_nil(g_aot_interp);
}

fn void set_var(char[] name, lisp::Value* val) {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(name);
    // AOT set_var on global env — silently ignore if unbound (define_var should be called first)
    lisp::env_set_with_barrier(g_aot_interp.global_env, sid, val, g_aot_interp)!!;
}

fn lisp::Value* explain_dispatch(char[] source, lisp::Value* callee, lisp::Value* arg_list, long argc) {
    if (argc < 0) return lisp::make_error(g_aot_interp, "aot explain-dispatch: negative arg count");
    usz arg_count = (usz)argc;
    lisp::Value** storage = arg_count > 0
        ? (lisp::Value**)mem::malloc(lisp::Value*.sizeof * arg_count)
        : null;
    defer if (storage != null) mem::free(storage);

    if (arg_count > 0 && storage == null) {
        return lisp::make_error(g_aot_interp, "aot explain-dispatch: out of memory");
    }

    lisp::Value* curr = arg_list;
    for (usz i = 0; i < arg_count; i++) {
        if (curr == null || curr.tag != lisp::ValueTag.CONS) {
            return lisp::make_error(g_aot_interp, "aot explain-dispatch: arg list too short");
        }
        storage[i] = curr.cons_val.car;
        curr = curr.cons_val.cdr;
    }

    lisp::Value*[] args = arg_count > 0 ? storage[:arg_count] : {};
    return lisp::explain_dispatch_result_from_compiled_call(g_aot_interp, source, callee, args);
}

fn lisp::Value* explain_effect_signal(char[] source, char[] effect_name, lisp::Value* arg) {
    return lisp::explain_effect_result_from_compiled_signal(
        g_aot_interp,
        source,
        aot_symbol_from_text(effect_name),
        arg
    );
}

fn lisp::Value* explain_effect_resolve(char[] source, lisp::Value* continuation) {
    return lisp::explain_effect_result_from_compiled_resolve(g_aot_interp, source, continuation);
}

// =============================================================================
// Output
// =============================================================================

fn void print_value(lisp::Value* v) {
    lisp::print_value(v, &g_aot_interp.symbols);
    io::print("\n");
}

// =============================================================================
// Dictionary/Array Operations
// =============================================================================

fn lisp::Value* dict_from_args(lisp::Value* arg_list) {
    lisp::Value* dict = lisp::make_hashmap(g_aot_interp, 16);
    lisp::HashMap* map = dict.hashmap_val;
    lisp::Value* curr = arg_list;
    while (curr != null && curr.tag == lisp::ValueTag.CONS) {
        lisp::Value* head = curr.cons_val.car;

        // Pair-list form: ((k . v) ...)
        if (head != null && head.tag == lisp::ValueTag.CONS) {
            lisp::Value* key = head.cons_val.car;
            lisp::Value* val_pair = head.cons_val.cdr;
            lisp::Value* val = (val_pair != null && val_pair.tag == lisp::ValueTag.CONS)
                ? val_pair.cons_val.car
                : lisp::make_nil(g_aot_interp);
            lisp::hashmap_set(map, key, val, g_aot_interp);
            curr = curr.cons_val.cdr;
            continue;
        }

        // Flat arg form: (k1 v1 k2 v2 ...)
        lisp::Value* key = head;
        curr = curr.cons_val.cdr;
        lisp::Value* val = lisp::make_nil(g_aot_interp);
        if (curr != null && curr.tag == lisp::ValueTag.CONS) {
            val = curr.cons_val.car;
            curr = curr.cons_val.cdr;
        }
        lisp::hashmap_set(map, key, val, g_aot_interp);
    }
    return dict;
}

fn lisp::Value* index(lisp::Value* collection, lisp::Value* idx) {
    lisp::Value* ref_fn = g_aot_interp.global_env.lookup(g_aot_interp.symbols.intern("ref"));
    if (ref_fn == null) return lisp::raise_error(g_aot_interp, "ref: primitive not found");
    lisp::Value* partial = lisp::jit_apply_value(ref_fn, collection, g_aot_interp);
    if (partial != null && partial.tag == lisp::ValueTag.ERROR) return partial;
    return lisp::jit_apply_value(partial, idx, g_aot_interp);
}

// =============================================================================
// Effects (Bridge to JIT)
// =============================================================================

/**
 * AOT handle wrapper.
 */
fn lisp::Value* compiled_handle(lisp::Value** tags, lisp::Value** handlers, usz count, lisp::Value* body) {
    return lisp::jit_handle_value(g_aot_interp, tags, handlers, count, body);
}
/**
 * AOT signal wrapper.
 */
fn lisp::Value* compiled_signal(lisp::Value* tag, lisp::Value* arg) {
    return lisp::jit_signal_value(g_aot_interp, tag, arg);
}

/**
 * AOT reset wrapper.
 */
fn lisp::Value* compiled_reset(lisp::Value* body) {
    return lisp::jit_reset_value(g_aot_interp, body);
}

/**
 * AOT shift wrapper.
 */
fn lisp::Value* compiled_shift(lisp::Value* body) {
    return lisp::jit_shift_value(g_aot_interp, body);
}

/**
 * AOT resolve wrapper.
 */
fn lisp::Value* compiled_resolve(lisp::Value* k_val, lisp::Value* val) {
    return lisp::jit_resolve_value(g_aot_interp, k_val, val);
}
