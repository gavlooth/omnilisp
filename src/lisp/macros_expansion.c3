module lisp;

// =============================================================================
// SECTION 2.6/2.7: macro expansion + module helpers
// =============================================================================

fn MacroDef* lookup_macro(SymbolId name, Interp* interp) {
    usz hash_slot = (usz)name % interp.macro_hash_capacity;
    for (usz probe = 0; probe < interp.macro_hash_capacity; probe++) {
        usz idx = interp.macro_hash_index[hash_slot];
        if (idx == usz.max) return null;
        if ((uint)interp.macro_table[idx].name == (uint)name) {
            return &interp.macro_table[idx];
        }
        hash_slot = (hash_slot + 1) % interp.macro_hash_capacity;
    }
    return null;
}

fn usz lookup_macro_index(SymbolId name, Interp* interp) {
    usz hash_slot = (usz)name % interp.macro_hash_capacity;
    for (usz probe = 0; probe < interp.macro_hash_capacity; probe++) {
        usz idx = interp.macro_hash_index[hash_slot];
        if (idx == usz.max) return usz.max;
        if ((uint)interp.macro_table[idx].name == (uint)name) return idx;
        hash_slot = (hash_slot + 1) % interp.macro_hash_capacity;
    }
    return usz.max;
}

fn void macro_def_destroy(MacroDef* mdef) {
    if (mdef == null) return;
    if (mdef.clauses != null) mem::free(mdef.clauses);
    if (mdef.captured_bindings != null) mem::free(mdef.captured_bindings);
    mdef.clauses = null;
    mdef.captured_bindings = null;
    mdef.clause_count = 0;
    mdef.captured_count = 0;
}

fn void expand_macros_in_sequence(Expr** exprs, usz expr_count, Interp* interp) {
    for (usz i = 0; i < expr_count; i++) {
        Expr* child = exprs[i];
        if (child != null && child.tag == E_DEFMACRO) {
            eval_define_macro(child, interp.global_env, interp);
            continue;
        }
        exprs[i] = expand_macros_in_expr(child, interp);
    }
}

fn Expr* expand_macro_call_site(Expr* expr, Interp* interp) {
    if (expr.call.func == null || expr.call.func.tag != E_VAR) return null;
    MacroDef* mdef = lookup_macro(expr.call.func.var_expr.name, interp);
    if (mdef == null) return null;

    EvalResult r = expand_pattern_macro(mdef, expr, interp);
    if (r.error.has_error) return null;

    Expr* expanded = value_to_expr(r.value, interp);
    return expand_macros_in_expr(expanded, interp);
}

fn void expand_macros_call_children(Expr* expr, Interp* interp) {
    expr.call.func = expand_macros_in_expr(expr.call.func, interp);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        expr.call.args[i] = expand_macros_in_expr(expr.call.args[i], interp);
    }
}

fn bool expand_macros_unary(Expr* expr, Interp* interp) {
    switch (expr.tag) {
        case E_DEFINE:
            expr.define.value = expand_macros_in_expr(expr.define.value, interp);
            return true;
        case E_LAMBDA:
            expr.lambda.body = expand_macros_in_expr(expr.lambda.body, interp);
            return true;
        case E_SET:
            expr.set_expr.value = expand_macros_in_expr(expr.set_expr.value, interp);
            return true;
        case E_RESET:
            expr.reset.body = expand_macros_in_expr(expr.reset.body, interp);
            return true;
        case E_SHIFT:
            expr.shift.body = expand_macros_in_expr(expr.shift.body, interp);
            return true;
        case E_PERFORM:
            expr.perform.arg = expand_macros_in_expr(expr.perform.arg, interp);
            return true;
        case E_RESOLVE:
            expr.resolve.value = expand_macros_in_expr(expr.resolve.value, interp);
            return true;
        default:
            return false;
    }
}

fn bool expand_macros_binary(Expr* expr, Interp* interp) {
    switch (expr.tag) {
        case E_IF:
            expr.if_expr.test = expand_macros_in_expr(expr.if_expr.test, interp);
            expr.if_expr.then_branch = expand_macros_in_expr(expr.if_expr.then_branch, interp);
            expr.if_expr.else_branch = expand_macros_in_expr(expr.if_expr.else_branch, interp);
            return true;
        case E_LET:
            expr.let_expr.init = expand_macros_in_expr(expr.let_expr.init, interp);
            expr.let_expr.body = expand_macros_in_expr(expr.let_expr.body, interp);
            return true;
        case E_APP:
            expr.app.func = expand_macros_in_expr(expr.app.func, interp);
            expr.app.arg = expand_macros_in_expr(expr.app.arg, interp);
            return true;
        case E_AND:
            expr.and_expr.left = expand_macros_in_expr(expr.and_expr.left, interp);
            expr.and_expr.right = expand_macros_in_expr(expr.and_expr.right, interp);
            return true;
        case E_OR:
            expr.or_expr.left = expand_macros_in_expr(expr.or_expr.left, interp);
            expr.or_expr.right = expand_macros_in_expr(expr.or_expr.right, interp);
            return true;
        case E_INDEX:
            expr.index.collection = expand_macros_in_expr(expr.index.collection, interp);
            expr.index.index = expand_macros_in_expr(expr.index.index, interp);
            return true;
        default:
            return false;
    }
}

fn bool expand_macros_complex(Expr* expr, Interp* interp) {
    switch (expr.tag) {
        case E_HANDLE:
            expr.handle.body = expand_macros_in_expr(expr.handle.body, interp);
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                expr.handle.clauses[i].handler_body = expand_macros_in_expr(expr.handle.clauses[i].handler_body, interp);
            }
            return true;
        case E_MATCH:
            expr.match.scrutinee = expand_macros_in_expr(expr.match.scrutinee, interp);
            for (usz i = 0; i < expr.match.clause_count; i++) {
                expr.match.clauses[i].result = expand_macros_in_expr(expr.match.clauses[i].result, interp);
            }
            return true;
        case E_MODULE:
            expand_macros_in_sequence(expr.module_expr.body, expr.module_expr.body_count, interp);
            return true;
        default:
            return false;
    }
}

/**
 * expand_macros_in_expr — Pre-JIT macro expansion pass.
 *
 * Walks an AST and expands macros before JIT compilation. Within E_BEGIN
 * blocks, processes sequentially: E_DEFMACRO children are evaluated to register
 * the macro, then subsequent E_CALL children are expanded if they invoke a
 * known macro.
 *
 * Returns the (possibly transformed) expression. Mutates the tree in-place
 * where possible (e.g. replacing E_BEGIN children).
 */
fn Expr* expand_macros_in_expr(Expr* expr, Interp* interp) {
    if (expr == null) return expr;

    switch (expr.tag) {
        case E_BEGIN:
            expand_macros_in_sequence(expr.begin.exprs, expr.begin.expr_count, interp);
            return expr;
        case E_CALL: {
            if (Expr* expanded = expand_macro_call_site(expr, interp)) return expanded;
            expand_macros_call_children(expr, interp);
            return expr;
        }
        default:
            if (expand_macros_binary(expr, interp)) return expr;
            if (expand_macros_unary(expr, interp)) return expr;
            if (expand_macros_complex(expr, interp)) return expr;
            return expr;
    }
}

// =============================================================================
// SECTION 2.7: MODULE SYSTEM
// =============================================================================

/**
 * Find a module by name in the module table.
 */
fn Module* find_module(SymbolId name, Interp* interp) {
    usz hash_slot = (usz)name % interp.module_hash_capacity;
    for (usz probe = 0; probe < interp.module_hash_capacity; probe++) {
        usz idx = interp.module_hash_index[hash_slot];
        if (idx == usz.max) return null;
        if ((uint)interp.modules[idx].name == (uint)name) {
            return &interp.modules[idx];
        }
        hash_slot = (hash_slot + 1) % interp.module_hash_capacity;
    }
    return null;
}

// Module eval/import — handled by JIT (jit.c3)
