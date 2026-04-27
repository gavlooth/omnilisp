module lisp;

import main;

// JIT expression lowering: let/lambda/set/define and effect entry points.
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

    bool is_mutable = has_closure_set_on_local(expr.let_expr.body, expr.let_expr.name, interp);

    // Compile init expression -> result in R0 (init is not in tail position)
    jit_compile_expr(s, expr.let_expr.init, interp, locals)!;
    jit_compile_return_if_sequence_should_stop(s, locals);

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
    if (!has_env) emit_load_env(s, locals, JIT_V2);

    // Call jit_make_closure_from_expr(interp, expr, env)
    emit_call_3i(s, locals, (void*)&jit_make_closure_from_expr, JIT_V0, (long)expr, JIT_V2);
}

fn void? jit_compile_let_rec(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Build env from JIT locals into V2 (callee-saved), or load runtime env
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, locals, JIT_V2);

    // Call jit_eval_let_rec(interp, expr, env)
    emit_call_3i(s, locals, (void*)&jit_eval_let_rec, JIT_V0, (long)expr, JIT_V2);
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
    jit_compile_return_if_sequence_should_stop(s, locals);

    // Call jit_eval_define(interp, name, value)
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);  // V1 = value
    emit_call_3i(s, locals, (void*)&jit_eval_define, JIT_V0, (long)(uint)expr.define.name, JIT_V1);
}

fn void? jit_compile_quasiquote(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    jit_compile_3arg_helper(s, expr, interp, locals, (void*)&jit_eval_quasiquote)!;
}

fn void? jit_compile_match(void* s, Expr* expr, Interp* interp, JitLocals* locals) {
    // Compile scrutinee natively → R0
    jit_compile_expr(s, expr.match.scrutinee, interp, locals)!;
    jit_compile_return_if_sequence_should_stop(s, locals);
    // Save scrutinee to V1
    _jit_new_node_ww(s, CODE_MOVR, JIT_V1, JIT_R0);

    // Build env from locals for pattern bindings, or load runtime env
    bool has_env = emit_build_locals_env(s, interp, locals);
    if (!has_env) emit_load_env(s, locals, JIT_V2);

    // Call jit_do_match(interp, scrutinee, expr, env)
    emit_call_4_rrir(s, locals, (void*)&jit_do_match, JIT_V0, JIT_V1, (long)expr, JIT_V2);
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
    if (!has_env) emit_load_env(s, locals, JIT_V2);
    emit_call_3i(s, locals, (void*)&jit_exec_perform, JIT_V0, (long)expr, JIT_V2);
}
