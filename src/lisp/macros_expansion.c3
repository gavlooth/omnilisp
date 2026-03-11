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

fn Expr* macro_error_expr(Interp* interp, char[] msg) {
    Expr* e = interp.alloc_expr();
    e.tag = E_LIT;
    e.lit.value = make_error(interp, msg);
    return e;
}

fn Expr* macro_error_expr_at(Interp* interp, char[] msg, Expr* source_expr) {
    Expr* e = macro_error_expr(interp, msg);
    if (source_expr != null) {
        e.loc_line = source_expr.loc_line;
        e.loc_column = source_expr.loc_column;
    }
    return e;
}

fn void macro_stamp_expr_location(Expr* expr, usz line, usz column) {
    if (expr == null) return;
    if (line == 0 && column == 0) return;

    if (expr.loc_line == 0) expr.loc_line = line;
    if (expr.loc_column == 0) expr.loc_column = column;

    switch (expr.tag) {
        case E_LAMBDA:
            if (expr.lambda != null) macro_stamp_expr_location(expr.lambda.body, line, column);
            return;
        case E_APP:
            macro_stamp_expr_location(expr.app.func, line, column);
            macro_stamp_expr_location(expr.app.arg, line, column);
            return;
        case E_IF:
            macro_stamp_expr_location(expr.if_expr.test, line, column);
            macro_stamp_expr_location(expr.if_expr.then_branch, line, column);
            macro_stamp_expr_location(expr.if_expr.else_branch, line, column);
            return;
        case E_LET:
            macro_stamp_expr_location(expr.let_expr.init, line, column);
            macro_stamp_expr_location(expr.let_expr.body, line, column);
            return;
        case E_DEFINE:
            macro_stamp_expr_location(expr.define.value, line, column);
            return;
        case E_RESET:
            macro_stamp_expr_location(expr.reset.body, line, column);
            return;
        case E_SHIFT:
            macro_stamp_expr_location(expr.shift.body, line, column);
            return;
        case E_PERFORM:
            macro_stamp_expr_location(expr.perform.arg, line, column);
            return;
        case E_RESOLVE:
            macro_stamp_expr_location(expr.resolve.value, line, column);
            return;
        case E_INDEX:
            macro_stamp_expr_location(expr.index.collection, line, column);
            macro_stamp_expr_location(expr.index.index, line, column);
            return;
        case E_MATCH:
            if (expr.match == null) return;
            macro_stamp_expr_location(expr.match.scrutinee, line, column);
            for (usz i = 0; i < expr.match.clause_count; i++) {
                macro_stamp_expr_location(expr.match.clauses[i].result, line, column);
            }
            return;
        case E_AND:
            macro_stamp_expr_location(expr.and_expr.left, line, column);
            macro_stamp_expr_location(expr.and_expr.right, line, column);
            return;
        case E_OR:
            macro_stamp_expr_location(expr.or_expr.left, line, column);
            macro_stamp_expr_location(expr.or_expr.right, line, column);
            return;
        case E_CALL:
            if (expr.call == null) return;
            macro_stamp_expr_location(expr.call.func, line, column);
            for (usz i = 0; i < expr.call.arg_count; i++) {
                macro_stamp_expr_location(expr.call.args[i], line, column);
            }
            return;
        case E_BLOCK:
            if (expr.block == null) return;
            for (usz i = 0; i < expr.block.expr_count; i++) {
                macro_stamp_expr_location(expr.block.exprs[i], line, column);
            }
            return;
        case E_SET:
            macro_stamp_expr_location(expr.set_expr.value, line, column);
            return;
        case E_QUASIQUOTE:
            macro_stamp_expr_location(expr.quasiquote.body, line, column);
            return;
        case E_UNQUOTE:
            macro_stamp_expr_location(expr.unquote.body, line, column);
            return;
        case E_UNQUOTE_SPLICING:
            macro_stamp_expr_location(expr.unquote_splicing.body, line, column);
            return;
        case E_HANDLE:
            if (expr.handle == null) return;
            macro_stamp_expr_location(expr.handle.body, line, column);
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                macro_stamp_expr_location(expr.handle.clauses[i].handler_body, line, column);
            }
            return;
        case E_MODULE:
            if (expr.module_expr == null) return;
            for (usz i = 0; i < expr.module_expr.body_count; i++) {
                macro_stamp_expr_location(expr.module_expr.body[i], line, column);
            }
            return;
        default:
            return;
    }
}

fn Expr* macro_result_to_expr(EvalResult* r, Expr* call_expr, Interp* interp) {
    if (r.error.has_error) return macro_error_expr_at(interp, r.error.message[:256], call_expr);

    Expr* expanded = value_to_expr(r.value, interp);
    if (expanded == null) return macro_error_expr_at(interp, "macro expansion produced null expression", call_expr);

    usz line = call_expr != null ? call_expr.loc_line : 0;
    usz column = call_expr != null ? call_expr.loc_column : 0;
    macro_stamp_expr_location(expanded, line, column);
    return expanded;
}

fn bool try_expand_macro_call_expr(Expr* call_expr, Interp* interp, EvalResult* out_result) {
    if (call_expr == null || call_expr.tag != E_CALL) return false;
    if (call_expr.call.func == null || call_expr.call.func.tag != E_VAR) return false;
    MacroDef* mdef = lookup_macro(call_expr.call.func.var_expr.name, interp);
    if (mdef == null) return false;
    *out_result = expand_pattern_macro(mdef, call_expr, interp);
    return true;
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
    EvalResult r;
    if (!try_expand_macro_call_expr(expr, interp, &r)) return null;
    Expr* expanded = macro_result_to_expr(&r, expr, interp);
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
 * Walks an AST and expands macros before JIT compilation. Within E_BLOCK
 * blocks, processes sequentially: E_DEFMACRO children are evaluated to register
 * the macro, then subsequent E_CALL children are expanded if they invoke a
 * known macro.
 *
 * Returns the (possibly transformed) expression. Mutates the tree in-place
 * where possible (e.g. replacing E_BLOCK children).
 */
fn Expr* expand_macros_in_expr(Expr* expr, Interp* interp) {
    if (expr == null) return expr;

    switch (expr.tag) {
        case E_BLOCK:
            expand_macros_in_sequence(expr.block.exprs, expr.block.expr_count, interp);
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
