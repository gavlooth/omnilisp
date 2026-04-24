module lisp;

import std::core::mem;
import main;

// JIT expression lowering: resolve, module/export/type forms, and fallback.
fn Value* jit_resolve_lookup_continuation(Interp* interp, Env* env) {
    Value* k_val = env.lookup(interp.sym__k);
    if (k_val == null) {
        k_val = interp.global_env.lookup(interp.sym__k);
    }
    if (k_val == null || k_val.tag != CONTINUATION) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/invalid-continuation", "resolve: not inside a handler clause"
        );
    }
    return k_val;
}

fn bool jit_resolve_is_effect_continuation(Value* k_val) @inline {
    return k_val.cont_val.is_coroutine_based && k_val.cont_val.handle_state != null;
}

fn Value* jit_exec_resolve(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    // Evaluate the value to resolve with
    Value* val = jit_eval(expr.resolve.value, env, interp);
    if (val != null && val.tag == ERROR) return val;

    Value* k_val = jit_resolve_lookup_continuation(interp, env);
    if (k_val != null && k_val.tag == ERROR) return k_val;

    if (!jit_resolve_is_effect_continuation(k_val)) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/invalid-continuation", "resolve: continuation is not handler-bound"
        );
    }

    // Effect-handler resolve is single-shot by contract.
    return jit_resolve_value(interp, k_val, val);
}

fn void? jit_compile_resolve(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, locals, JIT_V2);
    emit_call_3i(s, locals, (void*)&jit_exec_resolve, JIT_V0, (long)expr, JIT_V2);
}

// --- Native compilation for previously-fallback expression types ---

fn void? jit_compile_quote(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Quote returns the literal datum directly — no eval needed
    Value* datum = expr.quote.datum;
    if (datum == null) {
        emit_call_1(s, locals, (void*)&jit_make_nil);
    } else {
        _jit_new_node_ww(s, CODE_MOVI, JIT_R0, (long)datum);
    }
}

fn void? jit_compile_index(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Compile collection sub-expression → R0, spill to stack.
    // We must not keep it only in V1 while compiling index, because helper
    // calls emitted by index compilation (e.g. int literals) can clobber V1.
    int collection_slot = _jit_allocai(s, 8);
    jit_compile_expr(s, expr.index.collection, interp, locals)!;
    _jit_new_node_www(s, CODE_STXI_L, (long)collection_slot, (long)JIT_FP, (long)JIT_R0);

    // Compile index sub-expression → R0, save to V2
    jit_compile_expr(s, expr.index.index, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);

    // Restore collection into V1, then call helper.
    _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)collection_slot);

    // Call jit_do_index(interp=V0, collection=V1, index=V2)
    emit_call_3(s, locals, (void*)&jit_do_index, JIT_V0, JIT_V1, JIT_V2);
}

fn void? jit_compile_path(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_path)!;
}

// Emit a standard 3-arg call pattern: helper(interp, expr, env)
fn void? jit_compile_3arg_helper(void* s, Expr* expr, Interp* interp, JitLocals* locals, void* helper_fn) {
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, locals, JIT_V2);
    emit_call_3i(s, locals, helper_fn, JIT_V0, (long)expr, JIT_V2);
}

fn void? jit_compile_module(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_module)!;
}

fn void? jit_compile_import(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_import)!;
}

fn void? jit_compile_with_module_open(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_with_module_open)!;
}

fn Value* jit_do_export_from(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = jit_eval_export_from_impl(expr, env, interp);
    if (r.error.has_error) return raise_error_with_payload_names(interp, "runtime", "runtime/export-from-failed", r.error.message[:256]);
    return r.value;
}

fn Module* jit_find_current_module(Interp* interp, Env* env) {
    for (usz i = 0; i < interp.module_count; i++) {
        if (interp.modules[i].env == env || interp.modules[i].env == env.parent) {
            return &interp.modules[i];
        }
    }
    for (usz i = 0; i < interp.module_count; i++) {
        if (interp.modules[i].env == interp.global_env) {
            return &interp.modules[i];
        }
    }
    return null;
}

fn bool jit_append_module_export(Module* mod, SymbolId sym) {
    return module_append_export(mod, sym);
}

fn bool jit_is_exported_symbol(Module* source_mod, SymbolId sym) {
    for (usz ei = 0; ei < source_mod.export_count; ei++) {
        if ((uint)source_mod.exports[ei] == (uint)sym) {
            return true;
        }
    }
    return false;
}

fn bool jit_bind_and_reexport(Env* env, Interp* interp, Module* current_mod, SymbolId sym, Value* val) {
    if (val == null) return true;
    if (env_define_with_barrier_result(env, sym, val, interp) != null) return false;
    return jit_append_module_export(current_mod, sym);
}

fn bool jit_reexport_all_symbols(Module* source_mod, Module* current_mod, Env* env, Interp* interp) {
    for (usz i = 0; i < source_mod.export_count; i++) {
        SymbolId sym = source_mod.exports[i];
        Value* val = source_mod.env.lookup(sym);
        if (!jit_bind_and_reexport(env, interp, current_mod, sym, val)) return false;
    }
    return true;
}

fn EvalResult jit_reexport_selected_symbols(Expr* expr, Module* source_mod, Module* current_mod, Env* env, Interp* interp) {
    for (usz ni = 0; ni < expr.export_from.name_count; ni++) {
        SymbolId sym = expr.export_from.names[ni];
        if (!jit_is_exported_symbol(source_mod, sym)) {
            return eval_error("export-from: symbol not exported from source module");
        }
        Value* val = source_mod.env.lookup(sym);
        if (!jit_bind_and_reexport(env, interp, current_mod, sym, val)) {
            return eval_error("export-from: out of memory while binding symbol");
        }
    }
    return eval_ok(make_void(interp));
}

fn EvalResult jit_eval_export_from_impl(Expr* expr, Env* env, Interp* interp) {
    SymbolId source_name = expr.export_from.source_module;
    Module* source_mod = find_module(source_name, interp);
    if (source_mod == null) {
        return eval_error("export-from: source module not found");
    }

    Module* current_mod = jit_find_current_module(interp, env);
    if (current_mod == null) {
        return eval_error("export-from: must be used inside a module");
    }

    if (expr.export_from.all) {
        if (!jit_reexport_all_symbols(source_mod, current_mod, env, interp)) {
            return eval_error("export-from: out of memory while binding symbol");
        }
        return eval_ok(make_void(interp));
    }
    return jit_reexport_selected_symbols(expr, source_mod, current_mod, env, interp);
}

fn void? jit_compile_export_from(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_export_from)!;
}

fn void? jit_compile_deftype(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_deftype)!;
}

fn void? jit_compile_defabstract(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_defabstract)!;
}

fn void? jit_compile_defunion(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_defunion)!;
}

fn void? jit_compile_defalias(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_defalias)!;
}

fn void? jit_compile_defeffect(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_defeffect)!;
}

fn void? jit_compile_fallback(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    (void)s;
    (void)expr;
    (void)interp;
    (void)locals;
    return JIT_COMPILE_FAILED~;
}
