module lisp;

import main;

usz g_method_signature_heap_copy_call_count = 0;
usz g_method_signature_force_heap_copy_fail_on_call = 0;
usz g_method_signature_scope_copy_call_count = 0;
usz g_method_signature_force_scope_copy_fail_on_call = 0;

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
    if (param_count > 0) {
        if (param_count > usz.max / TypeId.sizeof ||
            param_count > usz.max / SymbolId.sizeof ||
            param_count > usz.max / ValueLiteralKey.sizeof ||
            param_count > usz.max / bool.sizeof) {
            return false;
        }
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
            sig.val_literals[i].sym_value = (SymbolId)0;
            sig.has_val_literal[i] = false;
        }
    }
    if (constraint_count > 0) {
        if (constraint_count > usz.max / MethodConstraint.sizeof) return false;
        sig.constraints = (MethodConstraint*)scope.alloc(MethodConstraint.sizeof * constraint_count);
        if (sig.constraints == null) return false;
    }
    sig.param_count = param_count;
    sig.constraint_count = constraint_count;
    return true;
}

fn bool method_signature_copy_to_scope(MethodSignature* dst, MethodSignature* src, main::ScopeRegion* scope) {
    g_method_signature_scope_copy_call_count++;
    if (g_method_signature_force_scope_copy_fail_on_call != 0 &&
        g_method_signature_scope_copy_call_count == g_method_signature_force_scope_copy_fail_on_call) {
        method_signature_init_empty(dst);
        return false;
    }
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
    g_method_signature_heap_copy_call_count++;
    if (g_method_signature_force_heap_copy_fail_on_call != 0 &&
        g_method_signature_heap_copy_call_count == g_method_signature_force_heap_copy_fail_on_call) {
        method_signature_init_empty(dst);
        return false;
    }
    method_signature_init_empty(dst);
    if (src.param_count > 0) {
        if (src.param_count > usz.max / TypeId.sizeof ||
            src.param_count > usz.max / SymbolId.sizeof ||
            src.param_count > usz.max / ValueLiteralKey.sizeof ||
            src.param_count > usz.max / bool.sizeof) {
            return false;
        }
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
        if (src.constraint_count > usz.max / MethodConstraint.sizeof) {
            method_signature_destroy_heap(dst);
            return false;
        }
        dst.constraints = (MethodConstraint*)mem::malloc(MethodConstraint.sizeof * src.constraint_count);
        if (dst.constraints == null) {
            method_signature_destroy_heap(dst);
            return false;
        }
        for (usz i = 0; i < src.constraint_count; i++) dst.constraints[i] = src.constraints[i];
    }
    dst.param_count = src.param_count;
    dst.constraint_count = src.constraint_count;
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
                if (ann.meta[m].is_int) continue;
                if (constraint_count == usz.max) return null;
                constraint_count++;
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
                sig.val_literals[i].sym_value = (SymbolId)0;
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
                        if (sig.constraint_count >= constraint_count) return null;
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
                sig.val_literals[i].sym_value = (SymbolId)0;
                sig.has_val_literal[i] = false;
            }
        } else {
            sig.param_types[i] = INVALID_TYPE_ID;
            sig.param_type_vars[i] = (SymbolId)0;
            sig.val_literals[i].tag = NIL;
            sig.val_literals[i].int_value = 0;
            sig.val_literals[i].sym_value = (SymbolId)0;
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
