module lisp;

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
        case E_WITH_MODULE:
            if (expr.with_module == null) return;
            for (usz i = 0; i < expr.with_module.body_count; i++) {
                macro_stamp_expr_location(expr.with_module.body[i], line, column);
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
