module lisp;

import main;
// =============================================================================
// SECTION 2.6: DEFMACRO (expr/value conversion + lookup)
// =============================================================================
fn Value* make_list1(Interp* interp, Value* a) @inline {
    return make_cons(interp, a, make_nil(interp));
}

fn Value* make_list2(Interp* interp, Value* a, Value* b) @inline {
    return make_cons(interp, a, make_list1(interp, b));
}

fn Value* make_list3(Interp* interp, Value* a, Value* b, Value* c) @inline {
    return make_cons(interp, a, make_list2(interp, b, c));
}

fn Value* make_tagged_form1(Interp* interp, SymbolId tag, Value* a) {
    return make_cons(interp, make_symbol(interp, tag), make_list1(interp, a));
}

fn Value* make_tagged_form2(Interp* interp, SymbolId tag, Value* a, Value* b) {
    return make_cons(interp, make_symbol(interp, tag), make_list2(interp, a, b));
}

fn Value* make_tagged_form3(Interp* interp, SymbolId tag, Value* a, Value* b, Value* c) {
    return make_cons(interp, make_symbol(interp, tag), make_list3(interp, a, b, c));
}

fn Value* expr_call_to_value(Expr* expr, Interp* interp) {
    Value* result = make_nil(interp);
    for (usz i = expr.call.arg_count; i > 0; i--) {
        result = make_cons(interp, expr_to_value(expr.call.args[i - 1], interp), result);
    }
    return make_cons(interp, expr_to_value(expr.call.func, interp), result);
}

fn Value* expr_lambda_to_value(Expr* expr, Interp* interp) {
    Value* params = make_nil(interp);
    for (usz i = expr.lambda.param_count; i > 0; i--) {
        params = make_cons(interp, make_symbol(interp, expr.lambda.params[i - 1]), params);
    }
    Value* body = expr_to_value(expr.lambda.body, interp);
    return make_tagged_form2(interp, interp.sym_lambda, params, body);
}

fn Value* expr_begin_to_value(Expr* expr, Interp* interp) {
    Value* items = make_nil(interp);
    for (usz i = expr.block.expr_count; i > 0; i--) {
        items = make_cons(interp, expr_to_value(expr.block.exprs[i - 1], interp), items);
    }
    return make_cons(interp, make_symbol(interp, interp.sym_block), items);
}

fn Value* expr_to_value(Expr* expr, Interp* interp) {
    if (expr == null) return make_nil(interp);

    switch (expr.tag) {
        case E_LIT:
            return expr.lit.value;
        case E_VAR:
            return make_symbol(interp, expr.var_expr.name);
        case E_QUOTE: {
            return make_tagged_form1(interp, interp.sym_quote, expr.quote.datum);
        }
        case E_CALL:
            return expr_call_to_value(expr, interp);
        case E_APP: {
            Value* f = expr_to_value(expr.app.func, interp);
            Value* a = expr_to_value(expr.app.arg, interp);
            return make_list2(interp, f, a);
        }
        case E_IF: {
            Value* t = expr_to_value(expr.if_expr.test, interp);
            Value* th = expr_to_value(expr.if_expr.then_branch, interp);
            Value* el = expr_to_value(expr.if_expr.else_branch, interp);
            return make_tagged_form3(interp, interp.sym_if, t, th, el);
        }
        case E_LET: {
            Value* name_sym = make_symbol(interp, expr.let_expr.name);
            Value* init_val = expr_to_value(expr.let_expr.init, interp);
            // Preserve parser flat-pair let binding form: (let (name init) body).
            // Using nested pair form here loses bindings on value_to_expr round-trip.
            Value* bindings = make_list2(interp, name_sym, init_val);
            Value* body = expr_to_value(expr.let_expr.body, interp);
            return make_tagged_form2(interp, interp.sym_let, bindings, body);
        }
        case E_LAMBDA:
            return expr_lambda_to_value(expr, interp);
        case E_BLOCK:
            return expr_begin_to_value(expr, interp);
        case E_SET: {
            Value* name_sym = make_symbol(interp, expr.set_expr.name);
            Value* val = expr_to_value(expr.set_expr.value, interp);
            return make_tagged_form2(interp, interp.sym_set, name_sym, val);
        }
        case E_QUASIQUOTE: {
            Value* body = expr_to_value(expr.quasiquote.body, interp);
            return make_tagged_form1(interp, interp.sym_quasiquote, body);
        }
        case E_UNQUOTE: {
            Value* body = expr_to_value(expr.unquote.body, interp);
            return make_tagged_form1(interp, interp.sym_unquote, body);
        }
        case E_UNQUOTE_SPLICING: {
            Value* body = expr_to_value(expr.unquote_splicing.body, interp);
            return make_tagged_form1(interp, interp.sym_unquote_splicing, body);
        }
        case E_DEFINE: {
            Value* name_sym = make_symbol(interp, expr.define.name);
            Value* val = expr_to_value(expr.define.value, interp);
            return make_tagged_form2(interp, interp.sym_define, name_sym, val);
        }
        default:
            return make_nil(interp);
    }
}
