module lisp;

import main;

// -----------------------------------------------------------------------------
// Basic expression compilers
// -----------------------------------------------------------------------------

fn void? jit_compile_lit(void* s, Expr* expr, Interp* interp) {
    Value* lit = expr.lit.value;
    if (lit == null || lit.tag == NIL) {
        emit_call_1(s, (void*)&jit_make_nil);
        return;
    }
    if (lit.tag == INT) {
        emit_call_2i(s, (void*)&jit_make_int, JIT_V0, lit.int_val);
        return;
    }
    // For other literal types, use the literal value directly
    // (it's a pointer to a pre-existing Value in the expr pool)
    _jit_new_node_ww(s, CODE_MOVI, JIT_R0, (long)lit);
}

fn void? jit_compile_var(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    uint sym_id = (uint)expr.var_expr.name;

    // Check JIT locals first (reverse order for proper scoping of shadowed names)
    for (usz i = locals.locals.len(); i > 0; i--) {
        if ((uint)locals.locals[i - 1].name == sym_id) {
            if (locals.locals[i - 1].is_mutable) {
                // Mutable local: stack slot holds Env* (the box). Lookup value in it.
                _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V1, (long)JIT_FP, (long)locals.locals[i - 1].stack_offset);
                emit_call_3i(s, (void*)&jit_env_lookup_local, JIT_V0, (long)sym_id, JIT_V1);
                return;
            }
            // Non-mutable: load value directly from stack slot
            _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_R0, (long)JIT_FP, (long)locals.locals[i - 1].stack_offset);
            return;
        }
    }

    // Fall back to jit_lookup_var: checks jit_env (for sub-expression local bindings), then global
    emit_call_2i(s, (void*)&jit_lookup_var, JIT_V0, (long)sym_id);
}

fn void? jit_compile_if(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Compile test expression -> R0 (test is never in tail position)
    jit_compile_expr(s, expr.if_expr.test, interp, locals)!;

    // Save test result to V1, call is_falsy(V1, interp)
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = test result
    emit_call_2(s, (void*)&jit_is_falsy, JIT_V1, JIT_V0);

    // If is_falsy returned true (non-zero), jump to else branch
    void* jump_else = _jit_new_node_pww(s, CODE_BNEI, null, (long)JIT_R0, 0);

    // Then branch — inherits tail position
    jit_compile_expr(s, expr.if_expr.then_branch, interp, locals, is_tail)!;
    void* jump_end = _jit_new_node_p(s, CODE_JMPI, null);

    // Else branch — inherits tail position
    _jit_patch(s, jump_else);
    jit_compile_expr(s, expr.if_expr.else_branch, interp, locals, is_tail)!;

    // End
    _jit_patch(s, jump_end);
}

fn void? jit_compile_and(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Evaluate left -> R0 (left is never in tail position)
    jit_compile_expr(s, expr.and_expr.left, interp, locals)!;

    // Check if left is falsy
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = left result
    emit_call_2(s, (void*)&jit_is_falsy, JIT_V1, JIT_V0);

    // If falsy, short-circuit: result = left (V1)
    void* jump_done = _jit_new_node_pww(s, CODE_BNEI, null, (long)JIT_R0, 0);

    // Not falsy: evaluate right — inherits tail position
    jit_compile_expr(s, expr.and_expr.right, interp, locals, is_tail)!;
    void* jump_end = _jit_new_node_p(s, CODE_JMPI, null);

    // Short-circuit: restore V1 to R0
    _jit_patch(s, jump_done);
    _jit_new_node_ww(s, CODE_MOVR, JIT_R0, JIT_V1);

    _jit_patch(s, jump_end);
}

fn void? jit_compile_or(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Evaluate left -> R0 (left is never in tail position)
    jit_compile_expr(s, expr.or_expr.left, interp, locals)!;

    // Check if left is falsy
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = left result
    emit_call_2(s, (void*)&jit_is_falsy, JIT_V1, JIT_V0);

    // If NOT falsy, short-circuit: result = left (V1)
    void* jump_done = _jit_new_node_pww(s, CODE_BEQI, null, (long)JIT_R0, 0);

    // Falsy: evaluate right — inherits tail position
    jit_compile_expr(s, expr.or_expr.right, interp, locals, is_tail)!;
    void* jump_end = _jit_new_node_p(s, CODE_JMPI, null);

    // Short-circuit: restore V1 to R0
    _jit_patch(s, jump_done);
    _jit_new_node_ww(s, CODE_MOVR, JIT_R0, JIT_V1);

    _jit_patch(s, jump_end);
}

fn void? jit_compile_block(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    usz count = expr.block.expr_count;
    if (count == 0) {
        emit_call_1(s, (void*)&jit_make_nil);
        return;
    }
    // Evaluate all expressions; last inherits tail position
    for (usz i = 0; i < count; i++) {
        bool tail = is_tail && (i == count - 1);
        jit_compile_expr(s, expr.block.exprs[i], interp, locals, tail)!;
    }
}

fn void? jit_compile_app(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Compile function -> R0, save to V1 (not in tail position)
    jit_compile_expr(s, expr.app.func, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = func

    // Compile argument -> R0, save to V2 (not in tail position)
    jit_compile_expr(s, expr.app.arg, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V2, JIT_R0);  // V2 = arg

    // Call apply — use _tail variant when in tail position
    void* apply_fn = is_tail ? (void*)&jit_apply_value_tail : (void*)&jit_apply_value;
    emit_call_3(s, apply_fn, JIT_V1, JIT_V2, JIT_V0);
}
