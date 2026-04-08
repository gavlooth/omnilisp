module lisp;

import std::io;
import main;

fn Value* jit_tco_sentinel() {
    return &g_jit_tco_sentinel;
}

fn RuntimeCompiledExpr runtime_backend_compile_expr(Expr* expr, Interp* interp) @inline {
    return runtime_backend_wrap_compiled_expr(jit_compile(expr, interp));
}

fn Value* runtime_backend_exec_compiled_checked_expr(RuntimeCompiledExpr compiled, Interp* interp) {
    JitCompiledFn jit_compiled = runtime_backend_unwrap_compiled_expr(compiled);
    if (jit_compiled_fn_is_null(jit_compiled)) return raise_error(interp, "jit: null compiled function");
    if (!jit_compiled_fn_is_live_for_interp(jit_compiled, interp)) {
        return raise_error(interp, "jit: stale or foreign compiled function pointer");
    }
    runtime_exec_enter(interp);
    Value* result = jit_compiled.code_fn(interp);
    runtime_exec_leave(interp);
    return result;
}

fn Value* runtime_backend_exec_compiled_expr(RuntimeCompiledExpr compiled, Interp* interp) @inline {
    return jit_exec(runtime_backend_unwrap_compiled_expr(compiled), interp);
}

fn Value* jit_exec_compiled_checked(JitCompiledFn compiled, Interp* interp) {
    return runtime_backend_exec_compiled_checked_expr(runtime_backend_wrap_compiled_expr(compiled), interp);
}

// Execute a compiled JIT handle with trampoline TCO handling.
// Must be used whenever calling compiled JIT code from outside jit_eval.
fn Value* jit_exec(JitCompiledFn compiled, Interp* interp) {
    Value* result = jit_exec_compiled_checked(compiled, interp);
    if (is_error(result)) return result;
    while (interp.flags.jit_tco_bounce) {
        interp.flags.jit_tco_bounce = false;
        result = jit_eval(interp.jit_tco_expr, interp.jit_tco_env, interp);
    }
    return result;
}

// Returns jit_env if set (sub-expression context), else global_env
fn Env* jit_get_env(Interp* interp) {
    if (interp.jit_env != null) return interp.jit_env;
    return interp.global_env;
}

fn bool jit_is_falsy(Value* v, Interp* interp) {
    return is_falsy(v, interp);
}

fn Value* jit_make_true(Interp* interp) {
    return make_symbol(interp, interp.sym_true);
}

fn Value* jit_make_false(Interp* interp) {
    return make_symbol(interp, interp.sym_false);
}

fn char[] jit_env_copy_fault_message(BoundaryEnvCopyFault fault_code) @inline {
    switch (fault_code) {
        case BOUNDARY_ENV_COPY_FAULT_NONE:
            return "jit: closure env-copy failed";
        case BOUNDARY_ENV_COPY_FAULT_PROMOTION_CTX_ALLOC:
            return "jit: closure env-copy failed (promotion context allocation)";
        case BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT:
            return "jit: closure env-copy failed (env depth limit)";
        case BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC:
            return "jit: closure env-copy failed (env frame allocation)";
        case BOUNDARY_ENV_COPY_FAULT_BINDING_BUFFER_ALLOC:
            return "jit: closure env-copy failed (binding buffer allocation)";
        case BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY:
            return "jit: closure env-copy failed (binding value copy)";
        case BOUNDARY_ENV_COPY_FAULT_PARENT_COPY:
            return "jit: closure env-copy failed (parent chain copy)";
    }
}

fn bool delay_force_construct_trace_enabled() @inline {
    return boundary_c_getenv("OMNI_TRACE_DELAY_FORCE_CONSTRUCT") != null;
}

fn bool jit_delay_force_promise_env_matches(Interp* interp, Env* env) {
    if (interp == null || env == null) return false;
    SymbolId thunk_sym = interp.symbols.intern("thunk");
    SymbolId result_sym = interp.symbols.intern("result");
    SymbolId forced_sym = interp.symbols.intern("forced");
    return env.lookup(thunk_sym) != null &&
        env.lookup(result_sym) != null &&
        env.lookup(forced_sym) != null;
}

fn void jit_trace_delay_force_promise_closure(
    Interp* interp,
    Value* closure,
    Env* source_env,
    Env* copied_env,
    main::ScopeRegion* env_scope,
    bool need_env_copy
) {
    if (!delay_force_construct_trace_enabled()) return;
    if (closure == null || closure.tag != CLOSURE || closure.closure_val == null) return;
    bool promise_env = jit_delay_force_promise_env_matches(interp, source_env);
    if (closure.closure_val.param_count != 0 && closure.closure_val.has_param) return;
    io::printfn(
        "OMNI_DELAY_TRACE construct promise_env=%d need_env_copy=%d source_env_in_current=%d source_env_scope_gen=%d copied_same=%d copied_in_env_scope=%d copied_env_scope_gen=%d env_scope_gen=%d current_gen=%d body_tag=%d",
        promise_env ? 1 : 0,
        need_env_copy ? 1 : 0,
        source_env != null && boundary_ptr_in_scope((void*)source_env, interp.current_scope) ? 1 : 0,
        source_env != null ? (int)source_env.scope_gen : -1,
        copied_env == source_env ? 1 : 0,
        copied_env != null && env_scope != null && boundary_ptr_in_scope((void*)copied_env, env_scope) ? 1 : 0,
        copied_env != null ? (int)copied_env.scope_gen : -1,
        env_scope != null ? (int)env_scope.generation : -1,
        interp.current_scope != null ? (int)interp.current_scope.generation : -1,
        (int)closure.closure_val.body.tag
    );
}

fn bool jit_copy_closure_env_if_needed(
    Interp* interp,
    Value* closure,
    Env* env,
    bool need_env_copy,
    BoundaryEnvCopyFault* fault_out = null
) {
    if (fault_out != null) *fault_out = BOUNDARY_ENV_COPY_FAULT_NONE;
    if (!need_env_copy) {
        jit_trace_delay_force_promise_closure(interp, closure, env, env, closure.closure_val.env_scope, need_env_copy);
        return true;
    }
    main::ScopeRegion* env_scope = main::scope_create(null);
    if (env_scope == null) return false;
    BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(env, interp, env_scope);
    if (!boundary_env_copy_result_ok(copied)) {
        if (fault_out != null) *fault_out = copied.fault_code;
        main::scope_release(env_scope);
        return false;
    }
    Env* copied_env = copied.env;
    if (copied_env == null) {
        if (fault_out != null) *fault_out = BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC;
        main::scope_release(env_scope);
        return false;
    }
    closure.closure_val.env = copied_env;
    if (boundary_ptr_in_scope((void*)copied_env, env_scope)) {
        closure.closure_val.env_scope = env_scope;
    } else {
        // Env-copy may legally return an existing root-owned chain by identity.
        // That path does not need a detached ownership envelope: the root already
        // owns the reused chain, and retaining root_scope here creates a teardown
        // cycle for root-owned closures.
        main::scope_release(env_scope);
        closure.closure_val.env_scope = null;
    }
    jit_trace_delay_force_promise_closure(interp, closure, env, copied_env, closure.closure_val.env_scope, need_env_copy);
    return true;
}

fn void jit_copy_lambda_variadic_info(Interp* interp, Value* closure, Expr* expr) {
    closure.closure_val.param_count = expr.lambda.param_count;
    if (expr.lambda.param_count > 0) {
        closure.closure_val.params = (SymbolId*)interp.current_scope.alloc(SymbolId.sizeof * expr.lambda.param_count);
        for (usz i = 0; i < expr.lambda.param_count; i++) {
            closure.closure_val.params[i] = expr.lambda.params[i];
        }
    }
    closure.closure_val.has_rest = expr.lambda.has_rest;
    closure.closure_val.rest_param = expr.lambda.rest_param;

    if (expr.lambda.has_rest && expr.lambda.param_count == 0) {
        closure.closure_val.has_param = false;
        closure.closure_val.param = 0;
    }
}
