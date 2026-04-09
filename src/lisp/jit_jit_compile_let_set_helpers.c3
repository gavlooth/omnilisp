module lisp;

import main;

fn bool call_mutates_named_local(Expr* expr, SymbolId name, Interp* interp) {
    if (expr == null || expr.tag != E_CALL || expr.call.func == null || expr.call.arg_count == 0) return false;
    if (expr.call.func.tag != E_VAR) return false;
    if (expr.call.args[0] == null || expr.call.args[0].tag != E_VAR) return false;
    if (expr.call.args[0].var_expr.name != name) return false;

    SymbolId func_name = expr.call.func.var_expr.name;
    return (uint)func_name == (uint)interp.symbols.intern("push!") ||
        (uint)func_name == (uint)interp.symbols.intern("remove!") ||
        (uint)func_name == (uint)interp.symbols.intern("set!");
}

fn bool has_mutating_call_on_name(Expr* expr, SymbolId name, Interp* interp) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_LET:
            if (expr.let_expr.name == name) return false;
            return has_mutating_call_on_name(expr.let_expr.init, name, interp) ||
                has_mutating_call_on_name(expr.let_expr.body, name, interp);
        case E_IF:
            return has_mutating_call_on_name(expr.if_expr.test, name, interp) ||
                has_mutating_call_on_name(expr.if_expr.then_branch, name, interp) ||
                has_mutating_call_on_name(expr.if_expr.else_branch, name, interp);
        case E_LAMBDA:
            return has_mutating_call_on_name(expr.lambda.body, name, interp);
        case E_BLOCK:
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (has_mutating_call_on_name(expr.block.exprs[i], name, interp)) return true;
            }
            return false;
        case E_APP:
            return has_mutating_call_on_name(expr.app.func, name, interp) ||
                has_mutating_call_on_name(expr.app.arg, name, interp);
        case E_CALL:
            if (call_mutates_named_local(expr, name, interp)) return true;
            if (has_mutating_call_on_name(expr.call.func, name, interp)) return true;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                if (has_mutating_call_on_name(expr.call.args[i], name, interp)) return true;
            }
            return false;
        case E_AND:
            return has_mutating_call_on_name(expr.and_expr.left, name, interp) ||
                has_mutating_call_on_name(expr.and_expr.right, name, interp);
        case E_OR:
            return has_mutating_call_on_name(expr.or_expr.left, name, interp) ||
                has_mutating_call_on_name(expr.or_expr.right, name, interp);
        default:
            return false;
    }
}

fn bool has_set_on_name(Expr* expr, SymbolId name) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_SET:
            if (expr.set_expr.name == name) return true;
            return has_set_on_name(expr.set_expr.value, name);
        case E_LET:
            if (expr.let_expr.name == name) return false;  // shadowed
            return has_set_on_name(expr.let_expr.init, name) ||
                   has_set_on_name(expr.let_expr.body, name);
        case E_IF:
            return has_set_on_name(expr.if_expr.test, name) ||
                   has_set_on_name(expr.if_expr.then_branch, name) ||
                   has_set_on_name(expr.if_expr.else_branch, name);
        case E_LAMBDA:
            return has_set_on_name(expr.lambda.body, name);
        case E_BLOCK:
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (has_set_on_name(expr.block.exprs[i], name)) return true;
            }
            return false;
        case E_APP:
            return has_set_on_name(expr.app.func, name) ||
                   has_set_on_name(expr.app.arg, name);
        case E_CALL:
            if (has_set_on_name(expr.call.func, name)) return true;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                if (has_set_on_name(expr.call.args[i], name)) return true;
            }
            return false;
        case E_AND:
            return has_set_on_name(expr.and_expr.left, name) ||
                   has_set_on_name(expr.and_expr.right, name);
        case E_OR:
            return has_set_on_name(expr.or_expr.left, name) ||
                   has_set_on_name(expr.or_expr.right, name);
        default: return false;
    }
}

// Check if a lambda in the expression captures and mutates the named variable
fn bool has_closure_set_on_local(Expr* body, SymbolId name, Interp* interp) {
    if (body == null) return false;
    switch (body.tag) {
        case E_LAMBDA:
            return has_set_on_name(body.lambda.body, name) ||
                has_mutating_call_on_name(body.lambda.body, name, interp);
        case E_LET:
            if (body.let_expr.name == name) return false;  // shadowed
            return has_closure_set_on_local(body.let_expr.init, name, interp) ||
                   has_closure_set_on_local(body.let_expr.body, name, interp);
        case E_IF:
            return has_closure_set_on_local(body.if_expr.test, name, interp) ||
                   has_closure_set_on_local(body.if_expr.then_branch, name, interp) ||
                   has_closure_set_on_local(body.if_expr.else_branch, name, interp);
        case E_BLOCK:
            for (usz i = 0; i < body.block.expr_count; i++) {
                if (has_closure_set_on_local(body.block.exprs[i], name, interp)) return true;
            }
            return false;
        case E_APP:
            return has_closure_set_on_local(body.app.func, name, interp) ||
                   has_closure_set_on_local(body.app.arg, name, interp);
        case E_CALL:
            if (call_mutates_named_local(body, name, interp)) return true;
            if (has_closure_set_on_local(body.call.func, name, interp)) return true;
            for (usz i = 0; i < body.call.arg_count; i++) {
                if (has_closure_set_on_local(body.call.args[i], name, interp)) return true;
            }
            return false;
        case E_AND:
            return has_closure_set_on_local(body.and_expr.left, name, interp) ||
                   has_closure_set_on_local(body.and_expr.right, name, interp);
        case E_OR:
            return has_closure_set_on_local(body.or_expr.left, name, interp) ||
                   has_closure_set_on_local(body.or_expr.right, name, interp);
        default: return false;
    }
}

fn void jit_load_locals_env_or_runtime(void* s, Interp* interp, JitLocals* locals) @inline {
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);
}

fn void? jit_compile_destructuring_let(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_load_locals_env_or_runtime(s, interp, locals);
    emit_call_3i(s, (void*)&jit_eval_let_destruct, JIT_V0, (long)expr, JIT_V2);
}

fn void jit_store_mutable_let_local(void* s, Expr* expr, Interp* interp, JitLocals* locals, int offset) {
    // Save init value to V1 (callee-saved, survives env building)
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);
    jit_load_locals_env_or_runtime(s, interp, locals);

    // Call jit_env_extend_root(interp, parent_env, name, init_value) -> Env* (the box)
    // Uses root-scope lifetime so the box is persistent and shared with closures.
    emit_call_4_rrir(s, (void*)&jit_env_extend_root, JIT_V0, JIT_V2, (long)(uint)expr.let_expr.name, JIT_V1);

    // Store Env* to stack slot
    _jit_new_node_www(s, CODE_STXI_L, (long)offset, (long)JIT_FP, (long)JIT_R0);
}

fn void jit_store_immutable_let_local(void* s, int offset) {
    // Non-mutable local: store value directly in stack slot (fast path)
    _jit_new_node_www(s, CODE_STXI_L, (long)offset, (long)JIT_FP, (long)JIT_R0);
}

fn void? jit_push_let_local_and_compile_body(void* s, Expr* expr, Interp* interp, JitLocals* locals, int offset, bool is_mutable, bool is_tail) {
    JitLocal local = { .name = expr.let_expr.name, .stack_offset = offset, .is_mutable = is_mutable };
    locals.locals.push(local);
    defer (void)locals.locals.pop();
    jit_compile_expr(s, expr.let_expr.body, interp, locals, is_tail)!;
}

fn bool jit_set_is_path_assign(Expr* expr) @inline {
    return expr.set_expr.is_path && expr.set_expr.path_segment_count >= 2;
}

fn void? jit_compile_set_path_assign(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_expr(s, expr.set_expr.value, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);

    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);

    emit_call_4_rirr(s, (void*)&jit_eval_set_path, JIT_V0, (long)expr, JIT_V1, JIT_V2);
}

fn isz jit_find_local_binding(JitLocals* locals, SymbolId name) {
    for (usz i = locals.locals.len(); i > 0; i--) {
        if (locals.locals[i - 1].name == name) return (isz)(i - 1);
    }
    return -1;
}

fn void? jit_compile_set_mutable_local(void* s, Expr* expr, Interp* interp, JitLocals* locals, usz local_idx) {
    jit_compile_expr(s, expr.set_expr.value, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);

    _jit_new_node_www(s, CODE_LDXI_L, (long)JIT_V2, (long)JIT_FP,
                     (long)locals.locals[local_idx].stack_offset);

    emit_call_4_rirr(s, (void*)&jit_eval_set, JIT_V0, (long)(uint)expr.set_expr.name, JIT_V1, JIT_V2);
}

fn void? jit_compile_set_immutable_local(void* s, Expr* expr, Interp* interp, JitLocals* locals, usz local_idx) {
    jit_compile_expr(s, expr.set_expr.value, interp, locals)!;
    _jit_new_node_www(s, CODE_STXI_L, (long)locals.locals[local_idx].stack_offset,
                     (long)JIT_FP, (long)JIT_R0);
    emit_call_1(s, (void*)&jit_make_void);
}

fn void? jit_compile_set_env_assign(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_expr(s, expr.set_expr.value, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);

    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);

    emit_call_4_rirr(s, (void*)&jit_eval_set, JIT_V0, (long)(uint)expr.set_expr.name, JIT_V1, JIT_V2);
}
