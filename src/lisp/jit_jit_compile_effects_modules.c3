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

fn Value* jit_resolve_resume_effect_continuation(Interp* interp, Value* k_val, Value* val) {
    main::StackCtx* ctx = k_val.cont_val.ctx;
    if (ctx == null || ctx.status != main::StackCtxStatus.CTX_SUSPENDED) {
        return raise_error_with_payload_names(
            interp, "runtime", "runtime/continuation-not-suspended", "resolve: continuation not suspended"
        );
    }

    HandleEffectState* hstate = (HandleEffectState*)k_val.cont_val.handle_state;

    // Reinstall handler so the body can signal again
    if (interp.handler_count >= interp.handler_capacity) interp.grow_handler_stack();
    interp.handler_stack[hstate.handler_idx] = hstate.handler_copy;
    interp.handler_count = hstate.handler_idx + 1;

    // Set resume value
    interp.resume_value = val;

    SavedInterpState saved_resolve;
    save_interp_state(interp, &saved_resolve);

    main::StackContext parent_ctx;
    main::stack_ctx_resume(ctx, &parent_ctx);

    restore_interp_state(interp, &saved_resolve);

    // Pop the reinstalled handler
    interp.handler_count = hstate.handler_idx;

    // Stack overflow — context hit guard page after resolve
    if (ctx.status == main::StackCtxStatus.CTX_DEAD) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/stack-overflow", "stack overflow after resolve");
    }

    // Return value: body result if completed, or val as placeholder
    if (ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
        Value* body_result = (Value*)ctx.result;
        return body_result != null ? body_result : val;
    }
    // Body signaled again (hstate.signaled set by signal) — return val
    // The handle loop will dispatch the next signal
    return val;
}

fn Value* jit_exec_resolve(Interp* interp, Expr* expr, Env* env) {
    // Evaluate the value to resolve with
    Value* val = jit_eval(expr.resolve.value, env, interp);
    if (val != null && val.tag == ERROR) return val;

    Value* k_val = jit_resolve_lookup_continuation(interp, env);
    if (k_val != null && k_val.tag == ERROR) return k_val;

    // Effect handler path: single-shot resume (no clone)
    if (jit_resolve_is_effect_continuation(k_val)) {
        return jit_resolve_resume_effect_continuation(interp, k_val, val);
    }

    // All continuations are context-based — apply directly
    EvalResult r = jit_apply_continuation(k_val, val, interp);
    if (r.error.has_error) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/resolve-apply-failed", r.error.message[:256]);
    }
    return r.value;
}

fn void? jit_compile_resolve(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);
    emit_call_3i(s, (void*)&jit_exec_resolve, JIT_V0, (long)expr, JIT_V2);
}

// --- Native compilation for previously-fallback expression types ---

fn void? jit_compile_quote(void* s, Expr* expr, Interp* interp) {
    // Quote returns the literal datum directly — no eval needed
    Value* datum = expr.quote.datum;
    if (datum == null) {
        emit_call_1(s, (void*)&jit_make_nil);
    } else {
        _jit_new_node_ww(s, CODE_MOVI, JIT_R0, (long)datum);
    }
}

fn void? jit_compile_index(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Compile collection sub-expression → R0, save to V1
    jit_compile_expr(s, expr.index.collection, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);

    // Compile index sub-expression → R0, save to V2
    jit_compile_expr(s, expr.index.index, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);

    // Call jit_do_index(interp=V0, collection=V1, index=V2)
    emit_call_3(s, (void*)&jit_do_index, JIT_V0, JIT_V1, JIT_V2);
}

fn void? jit_compile_path(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_path)!;
}

// Emit a standard 3-arg call pattern: helper(interp, expr, env)
fn void? jit_compile_3arg_helper(void* s, Expr* expr, Interp* interp, JitLocals* locals, void* helper_fn) {
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);
    emit_call_3i(s, helper_fn, JIT_V0, (long)expr, JIT_V2);
}

fn void? jit_compile_module(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_module)!;
}

fn void? jit_compile_import(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_do_import)!;
}

fn Value* jit_do_export_from(Interp* interp, Expr* expr, Env* env) {
    EvalResult r = jit_eval_export_from_impl(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
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

fn void jit_append_module_export(Module* mod, SymbolId sym) {
    if (mod.export_count >= mod.export_capacity) {
        usz new_cap = mod.export_capacity * 2;
        SymbolId* new_exports = (SymbolId*)mem::malloc(SymbolId.sizeof * new_cap);
        for (usz j = 0; j < mod.export_count; j++) {
            new_exports[j] = mod.exports[j];
        }
        mem::free(mod.exports);
        mod.exports = new_exports;
        mod.export_capacity = new_cap;
    }
    mod.exports[mod.export_count] = sym;
    mod.export_count++;
}

fn bool jit_is_exported_symbol(Module* source_mod, SymbolId sym) {
    for (usz ei = 0; ei < source_mod.export_count; ei++) {
        if ((uint)source_mod.exports[ei] == (uint)sym) {
            return true;
        }
    }
    return false;
}

fn void jit_bind_and_reexport(Env* env, Interp* interp, Module* current_mod, SymbolId sym, Value* val) {
    if (val == null) return;
    env_define_with_barrier(env, sym, val, interp);
    jit_append_module_export(current_mod, sym);
}

fn void jit_reexport_all_symbols(Module* source_mod, Module* current_mod, Env* env, Interp* interp) {
    for (usz i = 0; i < source_mod.export_count; i++) {
        SymbolId sym = source_mod.exports[i];
        Value* val = source_mod.env.lookup(sym);
        jit_bind_and_reexport(env, interp, current_mod, sym, val);
    }
}

fn EvalResult jit_reexport_selected_symbols(Expr* expr, Module* source_mod, Module* current_mod, Env* env, Interp* interp) {
    for (usz ni = 0; ni < expr.export_from.name_count; ni++) {
        SymbolId sym = expr.export_from.names[ni];
        if (!jit_is_exported_symbol(source_mod, sym)) {
            return eval_error("export-from: symbol not exported from source module");
        }
        Value* val = source_mod.env.lookup(sym);
        jit_bind_and_reexport(env, interp, current_mod, sym, val);
    }
    return eval_ok(make_nil(interp));
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
        jit_reexport_all_symbols(source_mod, current_mod, env, interp);
        return eval_ok(make_nil(interp));
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
    // Build env from JIT locals into V2 (callee-saved), or load runtime env
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);

    // Fall back to interpreter: jit_eval_fallback(expr, env, interp)
    _jit_prepare(s);
    _jit_pushargi(s, (long)expr, CODE_PUSHARGI_L);                   // arg0 = expr ptr
    _jit_pushargr(s, JIT_V2, CODE_PUSHARGR_L);                       // arg1 = env (V2)
    _jit_pushargr(s, JIT_V0, CODE_PUSHARGR_L);                       // arg2 = interp
    _jit_finishi(s, (void*)&jit_eval_fallback);
    _jit_retval_l(s, JIT_R0);
}
