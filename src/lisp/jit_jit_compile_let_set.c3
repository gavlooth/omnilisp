module lisp;

import std::core::mem;
import main;

// JIT expression lowering: let/lambda/set/define and effect entry points.
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
fn bool has_closure_set_on_local(Expr* body, SymbolId name) {
    if (body == null) return false;
    switch (body.tag) {
        case E_LAMBDA:
            return has_set_on_name(body.lambda.body, name);
        case E_LET:
            if (body.let_expr.name == name) return false;  // shadowed
            return has_closure_set_on_local(body.let_expr.init, name) ||
                   has_closure_set_on_local(body.let_expr.body, name);
        case E_IF:
            return has_closure_set_on_local(body.if_expr.test, name) ||
                   has_closure_set_on_local(body.if_expr.then_branch, name) ||
                   has_closure_set_on_local(body.if_expr.else_branch, name);
        case E_BLOCK:
            for (usz i = 0; i < body.block.expr_count; i++) {
                if (has_closure_set_on_local(body.block.exprs[i], name)) return true;
            }
            return false;
        case E_APP:
            return has_closure_set_on_local(body.app.func, name) ||
                   has_closure_set_on_local(body.app.arg, name);
        case E_CALL:
            if (has_closure_set_on_local(body.call.func, name)) return true;
            for (usz i = 0; i < body.call.arg_count; i++) {
                if (has_closure_set_on_local(body.call.args[i], name)) return true;
            }
            return false;
        case E_AND:
            return has_closure_set_on_local(body.and_expr.left, name) ||
                   has_closure_set_on_local(body.and_expr.right, name);
        case E_OR:
            return has_closure_set_on_local(body.or_expr.left, name) ||
                   has_closure_set_on_local(body.or_expr.right, name);
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
    defer locals.locals.pop()!!;
    jit_compile_expr(s, expr.let_expr.body, interp, locals, is_tail)!;
}

fn void? jit_compile_let(void* s, Expr* expr, Interp* interp, JitLocals* locals, bool is_tail) {
    // Destructuring let: fall back to interpreter helper
    if (expr.let_expr.pattern != null) {
        jit_compile_destructuring_let(s, expr, interp, locals)!;
        return;
    }

    // Recursive let (let ^rec): use dedicated helper that does full env setup + patching
    if (expr.let_expr.is_recursive) {
        jit_compile_let_rec(s, expr, interp, locals)!;
        return;
    }

    bool is_mutable = has_closure_set_on_local(expr.let_expr.body, expr.let_expr.name);

    // Compile init expression -> result in R0 (init is not in tail position)
    jit_compile_expr(s, expr.let_expr.init, interp, locals)!;

    // Allocate a stack slot for this local variable
    int offset = _jit_allocai(s, 8);

    if (is_mutable) {
        jit_store_mutable_let_local(s, expr, interp, locals, offset);
    } else {
        jit_store_immutable_let_local(s, offset);
    }

    jit_push_let_local_and_compile_body(s, expr, interp, locals, offset, is_mutable, is_tail)!;
}

fn void? jit_compile_lambda(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Build env from JIT locals into V2 (callee-saved), or load runtime env
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);

    // Call jit_make_closure_from_expr(interp, expr, env)
    emit_call_3i(s, (void*)&jit_make_closure_from_expr, JIT_V0, (long)expr, JIT_V2);
}

fn void? jit_compile_let_rec(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Build env from JIT locals into V2 (callee-saved), or load runtime env
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);

    // Call jit_eval_let_rec(interp, expr, env)
    emit_call_3i(s, (void*)&jit_eval_let_rec, JIT_V0, (long)expr, JIT_V2);
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
}

fn void? jit_compile_set_env_assign(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_expr(s, expr.set_expr.value, interp, locals)!;
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);

    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);

    emit_call_4_rirr(s, (void*)&jit_eval_set, JIT_V0, (long)(uint)expr.set_expr.name, JIT_V1, JIT_V2);
}

fn void? jit_compile_set(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Dot-path set!: (set! obj.field value) — use runtime helper
    if (jit_set_is_path_assign(expr)) {
        jit_compile_set_path_assign(s, expr, interp, locals)!;
        return;
    }

    isz local_idx = jit_find_local_binding(locals, expr.set_expr.name);
    if (local_idx >= 0) {
        if (locals.locals[(usz)local_idx].is_mutable) {
            jit_compile_set_mutable_local(s, expr, interp, locals, (usz)local_idx)!;
            return;
        }
        jit_compile_set_immutable_local(s, expr, interp, locals, (usz)local_idx)!;
        return;
    }

    // Not a local: use env-based set!
    jit_compile_set_env_assign(s, expr, interp, locals)!;
}

fn void? jit_compile_define(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Compile the value expression -> R0
    jit_compile_expr(s, expr.define.value, interp, locals)!;

    // Call jit_eval_define(interp, name, value)
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = value
    emit_call_3i(s, (void*)&jit_eval_define, JIT_V0, (long)(uint)expr.define.name, JIT_V1);
}

fn void? jit_compile_quasiquote(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_eval_quasiquote)!;
}

fn void? jit_compile_match(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Compile scrutinee natively → R0
    jit_compile_expr(s, expr.match.scrutinee, interp, locals)!;
    // Save scrutinee to V1
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);

    // Build env from locals for pattern bindings, or load runtime env
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);

    // Call jit_do_match(interp, scrutinee, expr, env)
    emit_call_4_rrir(s, (void*)&jit_do_match, JIT_V0, JIT_V1, (long)expr, JIT_V2);
}

fn void? jit_compile_define_macro(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_eval_define_macro)!;
}

fn void? jit_compile_reset(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_exec_reset)!;
}

fn void? jit_compile_shift(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_exec_shift)!;
}

fn void? jit_compile_handle(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_exec_handle)!;
}

fn void? jit_compile_perform(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, JIT_V2);
    emit_call_3i(s, (void*)&jit_exec_perform, JIT_V0, (long)expr, JIT_V2);
}
