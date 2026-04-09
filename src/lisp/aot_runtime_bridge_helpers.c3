module lisp::aot;

import std::io;
import lisp;

fn lisp::Value* lookup_prim(char[] name) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot lookup-prim: runtime interpreter unavailable");
    lisp::SymbolId sid = interp.symbols.intern(name);
    lisp::Value* v = interp.global_env.lookup(sid);
    if (v != null) return v;
    return lisp::make_nil(interp);
}

fn void define_var(char[] name, lisp::Value* val) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return;
    lisp::SymbolId sid = interp.symbols.intern(name);
    lisp::env_define_with_barrier(interp.global_env, sid, val, interp);
}

fn lisp::Value* lookup_var(char[] name) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot lookup-var: runtime interpreter unavailable");
    lisp::SymbolId sid = interp.symbols.intern(name);
    lisp::Value* v = interp.global_env.lookup(sid);
    return v != null ? v : lisp::make_nil(interp);
}

fn lisp::Value* import_module(char[] name, char[] path, bool has_path, bool import_all) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot import: runtime interpreter unavailable");

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_IMPORT;
    expr.import_expr.name = interp.symbols.intern(name);
    expr.import_expr.has_path = has_path;
    expr.import_expr.path = has_path ? path.ptr : null;
    expr.import_expr.path_len = has_path ? path.len : 0;
    expr.import_expr.imports = null;
    expr.import_expr.aliases = null;
    expr.import_expr.import_count = 0;
    expr.import_expr.import_capacity = 0;
    expr.import_expr.import_all = import_all;

    lisp::EvalResult r = lisp::jit_eval_import_impl(&expr, interp.global_env, interp);
    if (r.error.has_error) {
        char[768] msg_buf;
        char[] err = r.error.message[:256];
        if (has_path) {
            if (err.len > 0) {
                if (try msg = io::bprintf(&msg_buf, "aot import failed for '%s' from '%s': %s", (ZString)name, (ZString)path, (ZString)err)) {
                    return aot_make_error_best_effort(interp, msg);
                }
            } else {
                if (try msg = io::bprintf(&msg_buf, "aot import failed for '%s' from '%s'", (ZString)name, (ZString)path)) {
                    return aot_make_error_best_effort(interp, msg);
                }
            }
        } else {
            if (err.len > 0) {
                if (try msg = io::bprintf(&msg_buf, "aot import failed for '%s': %s", (ZString)name, (ZString)err)) {
                    return aot_make_error_best_effort(interp, msg);
                }
            } else {
                if (try msg = io::bprintf(&msg_buf, "aot import failed for '%s'", (ZString)name)) {
                    return aot_make_error_best_effort(interp, msg);
                }
            }
        }
        return aot_make_error_best_effort(interp, "aot import failed");
    }
    lisp::Value* bound = interp.global_env.lookup(expr.import_expr.name);
    return bound != null ? bound : lisp::make_void(interp);
}

fn lisp::Value* field_access(lisp::Value* current, lisp::Value* field_name) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot field_access: runtime interpreter unavailable");

    if (current == null) {
        return aot_make_error_best_effort(interp, "aot field_access: null target");
    }
    if (current.tag == lisp::ValueTag.ERROR) return current;
    if (field_name == null) {
        return aot_make_error_best_effort(interp, "aot field_access: missing field name");
    }

    char[] field_text;
    switch (field_name.tag) {
        case lisp::ValueTag.STRING:
            field_text = field_name.str_chars[:field_name.str_len];
        case lisp::ValueTag.SYMBOL:
            field_text = interp.symbols.get_name(field_name.sym_val);
        default:
            return aot_make_error_best_effort(interp, "aot field_access: field name must be string or symbol");
    }

    lisp::SymbolId key = interp.symbols.intern(field_text);
    lisp::EvalResult step = lisp::eval_path_step(current, key, interp);
    if (step.error.has_error) {
        return aot_make_error_best_effort(interp, step.error.message[:256]);
    }
    return step.value != null ? step.value : lisp::make_nil(interp);
}

fn void set_var(char[] name, lisp::Value* val) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return;
    lisp::SymbolId sid = interp.symbols.intern(name);
    if (!lisp::env_set_with_barrier_checked(interp.global_env, sid, val, interp)) return;
}

fn lisp::Value* explain_dispatch(char[] source, lisp::Value* callee, lisp::Value* arg_list, long argc) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot explain-dispatch: runtime interpreter unavailable");
    if (argc < 0) return aot_make_error_best_effort(interp, "aot explain-dispatch: negative arg count");
    usz arg_count = (usz)argc;
    lisp::Value** storage = arg_count > 0
        ? (lisp::Value**)mem::malloc(lisp::Value*.sizeof * arg_count)
        : null;
    defer if (storage != null) mem::free(storage);

    if (arg_count > 0 && storage == null) {
        return aot_make_error_best_effort(interp, "aot explain-dispatch: out of memory");
    }

    lisp::Value* curr = arg_list;
    for (usz i = 0; i < arg_count; i++) {
        if (curr == null || curr.tag != lisp::ValueTag.CONS) {
            return aot_make_error_best_effort(interp, "aot explain-dispatch: arg list too short");
        }
        storage[i] = curr.cons_val.car;
        curr = curr.cons_val.cdr;
    }

    lisp::Value*[] args = arg_count > 0 ? storage[:arg_count] : {};
    return lisp::explain_dispatch_result_from_compiled_call(interp, source, callee, args);
}

fn lisp::Value* explain_effect_signal(char[] source, char[] effect_name, lisp::Value* arg) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot explain-effect-signal: runtime interpreter unavailable");
    return lisp::explain_effect_result_from_compiled_signal(
        interp,
        source,
        aot_symbol_from_text(effect_name),
        arg
    );
}

fn lisp::Value* explain_effect_resolve(char[] source, lisp::Value* continuation) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot explain-effect-resolve: runtime interpreter unavailable");
    return lisp::explain_effect_result_from_compiled_resolve(interp, source, continuation);
}
