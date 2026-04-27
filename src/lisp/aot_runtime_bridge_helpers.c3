module lisp::aot;

import std::io;
import lisp;

fn lisp::Value* lookup_prim(char[] name) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot lookup-prim: runtime interpreter unavailable");
    lisp::SymbolId sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, name, &sid)) return aot_make_error_best_effort(interp, "aot lookup-prim: invalid or uninternable symbol");
    lisp::Value* v = interp.global_env.lookup(sid);
    if (v != null) return v;
    char[256] msg_buf;
    if (try msg = io::bprintf(&msg_buf, "aot lookup-prim: primitive '%s' not found", (ZString)name)) {
        return aot_make_error_best_effort(interp, msg);
    }
    return aot_make_error_best_effort(interp, "aot lookup-prim: primitive not found");
}

fn lisp::Value* define_var(char[] name, lisp::Value* val) @nodiscard {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot define-var: runtime interpreter unavailable");
    if (val != null && val.tag == lisp::ValueTag.ERROR) return val;
    lisp::SymbolId sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, name, &sid)) return aot_make_error_best_effort(interp, "aot define-var: invalid or uninternable symbol");
    if (lisp::Value* bind_err = lisp::env_define_with_barrier_result(interp.global_env, sid, val, interp)) {
        return bind_err;
    }
    return null;
}

fn lisp::Value* lookup_var(char[] name) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot lookup-var: runtime interpreter unavailable");
    lisp::SymbolId sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, name, &sid)) return aot_make_error_best_effort(interp, "aot lookup-var: invalid or uninternable symbol");
    lisp::Value* v = interp.global_env.lookup(sid);
    if (v != null) return v;
    char[256] msg_buf;
    if (try msg = io::bprintf(&msg_buf, "aot lookup-var: unbound variable '%s'", (ZString)name)) {
        return aot_make_error_best_effort(interp, msg);
    }
    return aot_make_error_best_effort(interp, "aot lookup-var: unbound variable");
}

fn lisp::Env* push_module_env() {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return null;
    lisp::Env* saved = interp.global_env;
    if (saved == null) return null;
    lisp::Env* mod_env = lisp::boundary_make_env_in_root(interp, saved);
    if (mod_env == null) return null;
    interp.global_env = mod_env;
    return saved;
}

fn void pop_module_env(lisp::Env* saved) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null || saved == null) return;
    interp.global_env = saved;
}

fn lisp::Value* lookup_module_export(char[] module_name, char[] export_name) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot lookup-module-export: runtime interpreter unavailable");

    lisp::SymbolId module_sid = (lisp::SymbolId)0;
    lisp::SymbolId export_sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, module_name, &module_sid)) {
        return aot_make_error_best_effort(interp, "aot lookup-module-export: invalid or uninternable module name");
    }
    if (!aot_intern_checked(interp, export_name, &export_sid)) {
        return aot_make_error_best_effort(interp, "aot lookup-module-export: invalid or uninternable export name");
    }

    lisp::Module* mod = lisp::find_module(module_sid, interp);
    if (mod == null || !mod.loaded || mod.env == null) {
        return aot_make_error_best_effort(interp, "aot lookup-module-export: module not loaded");
    }

    bool exported = false;
    if (mod.export_count > 0 && mod.exports == null) {
        return aot_make_error_best_effort(interp, "aot lookup-module-export: module exports unavailable");
    }
    for (usz i = 0; i < mod.export_count; i++) {
        if ((lisp::SymbolId)mod.exports[i] == export_sid) {
            exported = true;
            break;
        }
    }
    if (!exported) {
        return aot_make_error_best_effort(interp, "aot lookup-module-export: symbol not exported from module");
    }

    lisp::Value* v = mod.env.lookup(export_sid);
    if (v != null) return v;

    char[320] msg_buf;
    if (try msg = io::bprintf(
        &msg_buf,
        "aot lookup-module-export: exported symbol '%s' is not bound in module '%s'",
        (ZString)export_name,
        (ZString)module_name
    )) {
        return aot_make_error_best_effort(interp, msg);
    }
    return aot_make_error_best_effort(interp, "aot lookup-module-export: exported symbol is not bound");
}

fn lisp::Value* import_module(char[] name, char[] path, bool has_path, bool import_all) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot import: runtime interpreter unavailable");

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_IMPORT;
    if (!aot_intern_checked(interp, name, &expr.import_expr.name)) {
        return aot_make_error_best_effort(interp, "aot import: invalid or uninternable module name");
    }
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

    lisp::SymbolId key = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, field_text, &key)) {
        return aot_make_error_best_effort(interp, "aot field_access: invalid or uninternable field name");
    }
    lisp::EvalResult step = lisp::eval_path_step(current, key, interp);
    if (step.error.has_error) {
        return aot_make_error_best_effort(interp, step.error.message[:256]);
    }
    return step.value != null ? step.value : lisp::make_nil(interp);
}

fn void set_var(char[] name, lisp::Value* val) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return;
    lisp::SymbolId sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, name, &sid)) return;
    if (!lisp::env_set_with_barrier_checked(interp.global_env, sid, val, interp)) return;
}

fn lisp::Value* set_var_result(char[] name, lisp::Value* val) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot set-var: runtime interpreter unavailable");
    lisp::SymbolId sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, name, &sid)) return aot_make_error_best_effort(interp, "aot set-var: invalid or uninternable symbol");
    bool found = false;
    lisp::Value* write_err = lisp::env_set_with_barrier_result(interp.global_env, sid, val, interp, &found);
    if (write_err != null) return write_err;
    if (!found) return aot_make_error_best_effort(interp, "set!: unbound variable");
    return null;
}

fn lisp::Value* explain_dispatch(char[] source, lisp::Value* callee, lisp::Value* arg_list, long argc) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot explain-dispatch: runtime interpreter unavailable");
    if (argc < 0) return aot_make_error_best_effort(interp, "aot explain-dispatch: negative arg count");
    usz arg_count = (usz)argc;
    if (arg_count > usz.max / lisp::Value*.sizeof) {
        return aot_make_error_best_effort(interp, "aot explain-dispatch: out of memory");
    }
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
    lisp::SymbolId effect_sym = (lisp::SymbolId)0;
    if (!aot_symbol_from_text_checked(effect_name, &effect_sym)) {
        return aot_make_error_best_effort(interp, "aot explain-effect-signal: invalid or uninternable effect name");
    }
    return lisp::explain_effect_result_from_compiled_signal(
        interp,
        source,
        effect_sym,
        arg
    );
}

fn lisp::Value* explain_effect_resolve(char[] source, lisp::Value* continuation) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot explain-effect-resolve: runtime interpreter unavailable");
    return lisp::explain_effect_result_from_compiled_resolve(interp, source, continuation);
}
