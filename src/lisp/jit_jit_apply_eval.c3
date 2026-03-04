module lisp;

/// Recursively check if an Expr tree contains E_SHIFT.
/// Used to detect named-let bodies that capture continuations,
/// which need normal env chain copying (can't use escape-scope env).
fn bool expr_contains_shift(Expr* e) {
    if (e == null) return false;
    if (e.tag == E_SHIFT) return true;
    switch (e.tag) {
        case E_IF:
            return expr_contains_shift(e.if_expr.test) ||
                   expr_contains_shift(e.if_expr.then_branch) ||
                   expr_contains_shift(e.if_expr.else_branch);
        case E_LET:
            return expr_contains_shift(e.let_expr.init) ||
                   expr_contains_shift(e.let_expr.body);
        case E_LAMBDA:
            // Lambda creates a closure boundary — shift inside captures
            // the lambda's context, not the named-let's. Safe to skip.
            return false;
        case E_BEGIN:
            for (usz i = 0; i < e.begin.expr_count; i++) {
                if (expr_contains_shift(e.begin.exprs[i])) return true;
            }
            return false;
        case E_CALL:
            if (expr_contains_shift(e.call.func)) return true;
            for (usz i = 0; i < e.call.arg_count; i++) {
                if (expr_contains_shift(e.call.args[i])) return true;
            }
            return false;
        case E_APP:
            return expr_contains_shift(e.app.func) ||
                   expr_contains_shift(e.app.arg);
        case E_MATCH:
            if (expr_contains_shift(e.match.scrutinee)) return true;
            for (usz i = 0; i < e.match.clause_count; i++) {
                if (expr_contains_shift(e.match.clauses[i].result)) return true;
            }
            return false;
        case E_AND:
            return expr_contains_shift(e.and_expr.left) ||
                   expr_contains_shift(e.and_expr.right);
        case E_OR:
            return expr_contains_shift(e.or_expr.left) ||
                   expr_contains_shift(e.or_expr.right);
        case E_SET:
            return expr_contains_shift(e.set_expr.value);
        case E_DEFINE:
            return expr_contains_shift(e.define.value);
        case E_PERFORM:
            return expr_contains_shift(e.perform.arg);
        case E_RESOLVE:
            return expr_contains_shift(e.resolve.value);
        case E_RESET:
            // shift inside reset is delimited — doesn't escape to named-let scope
            return false;
        case E_HANDLE:
            // Handle body runs on its own StackCtx — shift inside is delimited
            return false;
        default:
            return false;
    }
}

fn void jit_warm_pattern_cache(Pattern* pat, Interp* interp) {
    if (pat == null) return;
    switch (pat.tag) {
        case PAT_CONS:
            jit_warm_pattern_cache(pat.car_pat, interp);
            jit_warm_pattern_cache(pat.cdr_pat, interp);
        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                jit_warm_pattern_cache(pat.elements[i], interp);
            }
        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                jit_warm_pattern_cache(pat.ctor_sub_patterns[i], interp);
            }
        case PAT_GUARD:
            jit_warm_expr_cache(pat.guard_pred, interp);
            jit_warm_pattern_cache(pat.guard_sub, interp);
        default:
            break;
    }
}

fn void jit_warm_expr_cache(Expr* expr, Interp* interp) {
    if (expr == null || interp == null) return;

    JitFn cached = jit_cache_lookup(expr);
    if (cached == null) {
        JitFn compiled = jit_compile(expr, interp);
        if (compiled != null) {
            jit_cache_store(expr, compiled);
        }
    }

    switch (expr.tag) {
        case E_LAMBDA:
            jit_warm_expr_cache(expr.lambda.body, interp);
        case E_APP:
            jit_warm_expr_cache(expr.app.func, interp);
            jit_warm_expr_cache(expr.app.arg, interp);
        case E_IF:
            jit_warm_expr_cache(expr.if_expr.test, interp);
            jit_warm_expr_cache(expr.if_expr.then_branch, interp);
            jit_warm_expr_cache(expr.if_expr.else_branch, interp);
        case E_LET:
            jit_warm_expr_cache(expr.let_expr.init, interp);
            jit_warm_expr_cache(expr.let_expr.body, interp);
        case E_DEFINE:
            jit_warm_expr_cache(expr.define.value, interp);
        case E_RESET:
            jit_warm_expr_cache(expr.reset.body, interp);
        case E_SHIFT:
            jit_warm_expr_cache(expr.shift.body, interp);
        case E_PERFORM:
            jit_warm_expr_cache(expr.perform.arg, interp);
        case E_HANDLE:
            jit_warm_expr_cache(expr.handle.body, interp);
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                jit_warm_expr_cache(expr.handle.clauses[i].handler_body, interp);
            }
        case E_RESOLVE:
            jit_warm_expr_cache(expr.resolve.value, interp);
        case E_INDEX:
            jit_warm_expr_cache(expr.index.collection, interp);
            jit_warm_expr_cache(expr.index.index, interp);
        case E_MATCH:
            jit_warm_expr_cache(expr.match.scrutinee, interp);
            for (usz i = 0; i < expr.match.clause_count; i++) {
                jit_warm_pattern_cache(expr.match.clauses[i].pattern, interp);
                jit_warm_expr_cache(expr.match.clauses[i].result, interp);
            }
        case E_AND:
            jit_warm_expr_cache(expr.and_expr.left, interp);
            jit_warm_expr_cache(expr.and_expr.right, interp);
        case E_OR:
            jit_warm_expr_cache(expr.or_expr.left, interp);
            jit_warm_expr_cache(expr.or_expr.right, interp);
        case E_CALL:
            jit_warm_expr_cache(expr.call.func, interp);
            for (usz i = 0; i < expr.call.arg_count; i++) {
                jit_warm_expr_cache(expr.call.args[i], interp);
            }
        case E_BEGIN:
            for (usz i = 0; i < expr.begin.expr_count; i++) {
                jit_warm_expr_cache(expr.begin.exprs[i], interp);
            }
        case E_SET:
            jit_warm_expr_cache(expr.set_expr.value, interp);
        case E_QUASIQUOTE:
            jit_warm_expr_cache(expr.quasiquote.body, interp);
        case E_UNQUOTE:
            jit_warm_expr_cache(expr.unquote.body, interp);
        case E_UNQUOTE_SPLICING:
            jit_warm_expr_cache(expr.unquote_splicing.body, interp);
        case E_MODULE:
            for (usz i = 0; i < expr.module_expr.body_count; i++) {
                jit_warm_expr_cache(expr.module_expr.body[i], interp);
            }
        case E_FFI_LIB:
            jit_warm_expr_cache(expr.ffi_lib.path_expr, interp);
        default:
            break;
    }
}
