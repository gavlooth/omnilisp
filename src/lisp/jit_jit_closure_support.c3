module lisp;

import std::io;
import main;

// =============================================================================
// SECTION 3A: JIT CLOSURE HELPERS
// =============================================================================
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

fn void method_signature_init_empty(MethodSignature* sig) {
    sig.param_types = null;
    sig.param_type_vars = null;
    sig.val_literals = null;
    sig.has_val_literal = null;
    sig.param_count = 0;
    sig.constraints = null;
    sig.constraint_count = 0;
}

fn bool method_signature_alloc_arrays_in_scope(MethodSignature* sig, main::ScopeRegion* scope, usz param_count, usz constraint_count) {
    method_signature_init_empty(sig);
    sig.param_count = param_count;
    sig.constraint_count = constraint_count;
    if (param_count > 0) {
        sig.param_types = (TypeId*)scope.alloc(TypeId.sizeof * param_count);
        sig.param_type_vars = (SymbolId*)scope.alloc(SymbolId.sizeof * param_count);
        sig.val_literals = (ValueLiteralKey*)scope.alloc(ValueLiteralKey.sizeof * param_count);
        sig.has_val_literal = (bool*)scope.alloc(bool.sizeof * param_count);
        if (sig.param_types == null || sig.param_type_vars == null || sig.val_literals == null || sig.has_val_literal == null) return false;
        for (usz i = 0; i < param_count; i++) {
            sig.param_types[i] = INVALID_TYPE_ID;
            sig.param_type_vars[i] = (SymbolId)0;
            sig.val_literals[i].tag = NIL;
            sig.val_literals[i].int_value = 0;
            sig.val_literals[i].sym_value = 0;
            sig.has_val_literal[i] = false;
        }
    }
    if (constraint_count > 0) {
        sig.constraints = (MethodConstraint*)scope.alloc(MethodConstraint.sizeof * constraint_count);
        if (sig.constraints == null) return false;
    }
    return true;
}

fn bool method_signature_copy_to_scope(MethodSignature* dst, MethodSignature* src, main::ScopeRegion* scope) {
    if (!method_signature_alloc_arrays_in_scope(dst, scope, src.param_count, src.constraint_count)) return false;
    for (usz i = 0; i < src.param_count; i++) {
        dst.param_types[i] = src.param_types[i];
        dst.param_type_vars[i] = src.param_type_vars[i];
        dst.val_literals[i] = src.val_literals[i];
        dst.has_val_literal[i] = src.has_val_literal[i];
    }
    for (usz i = 0; i < src.constraint_count; i++) dst.constraints[i] = src.constraints[i];
    return true;
}

fn void method_signature_destroy_heap(MethodSignature* sig) {
    if (sig == null) return;
    if (sig.param_types != null) mem::free(sig.param_types);
    if (sig.param_type_vars != null) mem::free(sig.param_type_vars);
    if (sig.val_literals != null) mem::free(sig.val_literals);
    if (sig.has_val_literal != null) mem::free(sig.has_val_literal);
    if (sig.constraints != null) mem::free(sig.constraints);
    method_signature_init_empty(sig);
}

fn bool method_signature_copy_to_heap(MethodSignature* dst, MethodSignature* src) {
    method_signature_init_empty(dst);
    dst.param_count = src.param_count;
    dst.constraint_count = src.constraint_count;
    if (src.param_count > 0) {
        dst.param_types = (TypeId*)mem::malloc(TypeId.sizeof * src.param_count);
        dst.param_type_vars = (SymbolId*)mem::malloc(SymbolId.sizeof * src.param_count);
        dst.val_literals = (ValueLiteralKey*)mem::malloc(ValueLiteralKey.sizeof * src.param_count);
        dst.has_val_literal = (bool*)mem::malloc(bool.sizeof * src.param_count);
        if (dst.param_types == null || dst.param_type_vars == null || dst.val_literals == null || dst.has_val_literal == null) {
            method_signature_destroy_heap(dst);
            return false;
        }
        for (usz i = 0; i < src.param_count; i++) {
            dst.param_types[i] = src.param_types[i];
            dst.param_type_vars[i] = src.param_type_vars[i];
            dst.val_literals[i] = src.val_literals[i];
            dst.has_val_literal[i] = src.has_val_literal[i];
        }
    }
    if (src.constraint_count > 0) {
        dst.constraints = (MethodConstraint*)mem::malloc(MethodConstraint.sizeof * src.constraint_count);
        if (dst.constraints == null) {
            method_signature_destroy_heap(dst);
            return false;
        }
        for (usz i = 0; i < src.constraint_count; i++) dst.constraints[i] = src.constraints[i];
    }
    return true;
}

fn MethodSignature* jit_build_lambda_type_sig(Interp* interp, Expr* expr) {
    MethodSignature* sig = (MethodSignature*)interp.current_scope.alloc(MethodSignature.sizeof);
    if (sig == null) return null;
    SymbolId sym_lambda_type = interp.symbols.intern("Lambda");

    usz constraint_count = 0;
    for (usz i = 0; i < expr.lambda.param_count; i++) {
        TypeAnnotation* ann = &expr.lambda.param_annotations[i];
        if (ann.has_annotation && ann.is_dict) {
            for (usz m = 0; m < ann.meta_count; m++) {
                if (!ann.meta[m].is_int) constraint_count++;
            }
        }
    }
    if (!method_signature_alloc_arrays_in_scope(sig, interp.current_scope, expr.lambda.param_count, constraint_count)) {
        return null;
    }
    sig.constraint_count = 0;

    for (usz i = 0; i < sig.param_count; i++) {
        TypeAnnotation* ann = &expr.lambda.param_annotations[i];
        if (ann.has_annotation) {
            if (ann.has_val_literal) {
                sig.param_types[i] = INVALID_TYPE_ID;
                sig.param_type_vars[i] = (SymbolId)0;
                sig.val_literals[i] = ann.val_literal;
                sig.has_val_literal[i] = true;
            } else if (ann.is_dict) {
                sig.param_types[i] = INVALID_TYPE_ID;
                sig.param_type_vars[i] = (SymbolId)0;
                sig.val_literals[i].tag = NIL;
                sig.val_literals[i].int_value = 0;
                sig.val_literals[i].sym_value = 0;
                sig.has_val_literal[i] = false;
                bool saw_type_var = false;
                for (usz m = 0; m < ann.meta_count; m++) {
                    if (!ann.meta[m].is_int) {
                        if (!saw_type_var) {
                            sig.param_type_vars[i] = ann.meta[m].key;
                            saw_type_var = true;
                        } else if ((uint)sig.param_type_vars[i] != (uint)ann.meta[m].key) {
                            // Multi-key metadata does not have an unambiguous single type-var binding for this param.
                            sig.param_type_vars[i] = (SymbolId)0;
                        }
                        sig.constraints[sig.constraint_count].param_sym = ann.meta[m].key;
                        sig.constraints[sig.constraint_count].bound_type =
                            interp.types.lookup(ann.meta[m].sym_value, &interp.symbols);
                        sig.constraint_count++;
                    }
                }
            } else {
                TypeId resolved = interp.types.lookup(ann.base_type, &interp.symbols);
                if ((uint)ann.base_type == (uint)sym_lambda_type) {
                    // `^Lambda` / `^(Lambda ...)` gates function-valued args at call boundaries.
                    sig.param_types[i] = interp.tid_Closure;
                    sig.param_type_vars[i] = (SymbolId)0;
                } else {
                    sig.param_types[i] = resolved;
                    sig.param_type_vars[i] = resolved == INVALID_TYPE_ID ? ann.base_type : (SymbolId)0;
                }
                sig.val_literals[i].tag = NIL;
                sig.val_literals[i].int_value = 0;
                sig.val_literals[i].sym_value = 0;
                sig.has_val_literal[i] = false;
            }
        } else {
            sig.param_types[i] = INVALID_TYPE_ID;
            sig.param_type_vars[i] = (SymbolId)0;
            sig.val_literals[i].tag = NIL;
            sig.val_literals[i].int_value = 0;
            sig.val_literals[i].sym_value = 0;
            sig.has_val_literal[i] = false;
        }
    }
    return sig;
}

// Lambda/closure creation helper — called from JIT-compiled code.
// Mirrors eval_lambda from eval.c3: creates a closure value from an E_LAMBDA Expr*.
// Phase 4b: Closures allocated in current_scope (not root_scope). Env copied to a
// standalone env_scope (refcounted). Temporary closures freed when scope releases.
fn Value* jit_make_closure_from_expr(Interp* interp, Expr* expr, Env* env) {
    bool need_env_copy = (interp.current_scope != interp.root_scope);
    BoundaryEnvCopyFault copy_fault = BOUNDARY_ENV_COPY_FAULT_NONE;

    // Check for zero-arg lambda (sentinel param)
    if ((uint)expr.lambda.param == 0xFFFFFFFF && !expr.lambda.has_rest) {
        Value* closure = make_closure_no_param(interp, expr.lambda.body, env);
        if (!jit_copy_closure_env_if_needed(interp, closure, env, need_env_copy, &copy_fault)) {
            return raise_error(interp, jit_env_copy_fault_message(copy_fault));
        }
        return closure;
    }

    // Create closure in current_scope
    Value* closure = make_closure(interp, expr.lambda.param, expr.lambda.body, env);

    jit_copy_lambda_variadic_info(interp, closure, expr);

    closure.closure_val.has_typed_params = expr.lambda.has_typed_params;
    closure.closure_val.type_sig = null;
    if (expr.lambda.has_typed_params) {
        closure.closure_val.type_sig = jit_build_lambda_type_sig(interp, expr);
        if (closure.closure_val.type_sig == null) {
            return raise_error(interp, "jit: failed to allocate method signature");
        }
    }

    if (!jit_copy_closure_env_if_needed(interp, closure, env, need_env_copy, &copy_fault)) {
        return raise_error(interp, jit_env_copy_fault_message(copy_fault));
    }

    return closure;
}
