module lisp;

import std::io;
import std::collections::list;
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

// --- value_to_expr helpers ---

// Create an E_LIT nil expression.
fn Expr* make_nil_expr(Interp* interp) @inline {
    Expr* e = interp.alloc_expr();
    e.tag = E_LIT;
    e.lit.value = make_nil(interp);
    return e;
}

// Create an E_LIT error expression.
fn Expr* make_error_expr(Interp* interp, char[] msg) @inline {
    Expr* e = interp.alloc_expr();
    e.tag = E_LIT;
    e.lit.value = make_error(interp, msg);
    return e;
}

// Convert car of a cons rest-list to Expr, or nil if rest is not a cons.
fn Expr* rest_car_expr(Value* rest, Interp* interp) @inline {
    if (is_cons(rest)) return value_to_expr(rest.cons_val.car, interp);
    return make_nil_expr(interp);
}

// Extract SymbolId from a value, or 0 if not a symbol.
fn SymbolId val_to_sym(Value* v) @inline {
    return (v != null && v.tag == SYMBOL) ? v.sym_val : 0;
}

// Construct a single-param lambda ExprLambda node.
fn Expr* make_single_lambda_expr(Interp* interp, SymbolId param, Expr* body) {
    Expr* e = interp.alloc_expr();
    e.tag = E_LAMBDA;
    e.lambda = (ExprLambda*)ast_arena_alloc(&interp.ast_arena, ExprLambda.sizeof);
    e.lambda.param = param;
    e.lambda.param_count = 1;
    e.lambda.params = (SymbolId*)ast_arena_alloc(&interp.ast_arena, SymbolId.sizeof);
    e.lambda.params[0] = param;
    e.lambda.has_rest = false;
    e.lambda.rest_param = 0;
    e.lambda.has_typed_params = false;
    e.lambda.param_annotations = null;
    e.lambda.body = body;
    e.lambda.is_accessor_shorthand = false;
    return e;
}

// Construct a zero-param lambda ExprLambda node.
fn Expr* make_nullary_lambda_expr(Interp* interp, Expr* body) {
    Expr* e = interp.alloc_expr();
    e.tag = E_LAMBDA;
    e.lambda = (ExprLambda*)ast_arena_alloc(&interp.ast_arena, ExprLambda.sizeof);
    e.lambda.param = (SymbolId)0xFFFFFFFF;
    e.lambda.param_count = 0;
    e.lambda.params = null;
    e.lambda.has_rest = false;
    e.lambda.rest_param = 0;
    e.lambda.has_typed_params = false;
    e.lambda.param_annotations = null;
    e.lambda.body = body;
    e.lambda.is_accessor_shorthand = false;
    return e;
}

// Convert (lambda (params...) body) value-list to Expr.
// Desugars multi-param lambdas into nested single-param lambdas.
fn Expr* vte_lambda(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_nil_expr(interp);
    Value* params_list = rest.cons_val.car;
    rest = rest.cons_val.cdr;
    Expr* body = rest_car_expr(rest, interp);

    // Collect param symbols from list
    List{SymbolId} params;
    Value* p = params_list;
    while (is_cons(p)) {
        if (p.cons_val.car != null && p.cons_val.car.tag == SYMBOL) {
            params.push(p.cons_val.car.sym_val);
        }
        p = p.cons_val.cdr;
    }
    usz param_count = params.len();

    if (param_count == 0) {
        params.free();
        return make_nullary_lambda_expr(interp, body);
    }
    if (param_count == 1) {
        SymbolId p0 = params[0];
        params.free();
        return make_single_lambda_expr(interp, p0, body);
    }
    // Multi-param: desugar to nested single-param lambdas (right to left)
    Expr* inner = body;
    for (usz i = param_count; i > 1; i--) {
        inner = make_single_lambda_expr(interp, params[i - 1], inner);
    }
    Expr* result = make_single_lambda_expr(interp, params[0], inner);
    params.free();
    return result;
}

// Convert (let (name1 val1 name2 val2 ...) body) value-list to nested E_LET exprs.
fn Expr* vte_let(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_error_expr(interp, "macro expansion emitted malformed let: expected bindings list and body");
    Value* bindings_list = rest.cons_val.car;
    rest = rest.cons_val.cdr;
    Expr* body = rest_car_expr(rest, interp);

    char[] invalid_bindings_msg = "macro expansion emitted invalid let bindings; expected flat pairs: (let (x 1 y 2) ...)";
    if (!is_cons(bindings_list)) return make_error_expr(interp, invalid_bindings_msg);

    // Collect flat-pair bindings: name val name val ...
    List{SymbolId} names;
    List{Value*} inits;
    Value* bp = bindings_list;
    while (is_cons(bp)) {
        Value* name_v = bp.cons_val.car;
        if (name_v == null || name_v.tag != SYMBOL) {
            names.free();
            inits.free();
            return make_error_expr(interp, invalid_bindings_msg);
        }

        names.push(name_v.sym_val);
        bp = bp.cons_val.cdr;
        if (!is_cons(bp)) {
            names.free();
            inits.free();
            return make_error_expr(interp, invalid_bindings_msg);
        }
        inits.push(bp.cons_val.car);
        bp = bp.cons_val.cdr;
    }
    if (bp != null && bp.tag != NIL) {
        names.free();
        inits.free();
        return make_error_expr(interp, invalid_bindings_msg);
    }

    if (names.len() == 0) {
        names.free();
        inits.free();
        return make_error_expr(interp, "macro expansion emitted malformed let: expected at least one binding");
    }

    // Build nested lets from inside out
    Expr* inner = body;
    for (usz i = names.len(); i > 0; i--) {
        Expr* e = interp.alloc_expr();
        e.tag = E_LET;
        e.let_expr.name = names[i - 1];
        e.let_expr.init = value_to_expr(inits[i - 1], interp);
        e.let_expr.body = inner;
        e.let_expr.is_recursive = false;
        inner = e;
    }
    names.free(); inits.free();
    return inner;
}

// Convert (block e1 e2 ...) value-list to E_BLOCK expr.
fn Expr* vte_begin(Value* rest, Interp* interp) {
    List{Expr*} items;
    while (is_cons(rest)) {
        items.push(value_to_expr(rest.cons_val.car, interp));
        rest = rest.cons_val.cdr;
    }
    Expr* e = interp.alloc_expr();
    e.tag = E_BLOCK;
    e.block = (ExprBlock*)ast_arena_alloc(&interp.ast_arena, ExprBlock.sizeof);
    e.block.expr_count = items.len();
    e.block.exprs = (Expr**)ast_arena_alloc(&interp.ast_arena, Expr*.sizeof * items.len());
    for (usz i = 0; i < items.len(); i++) { e.block.exprs[i] = items[i]; }
    items.free();
    return e;
}

// Convert generic function call (func arg1 arg2 ...) cons-list to E_CALL expr.
fn Expr* vte_call(Value* val, Interp* interp) {
    usz len = list_length(val);
    if (len == 0) return make_nil_expr(interp);

    Expr* func_e = value_to_expr(val.cons_val.car, interp);
    List{Expr*} args;
    Value* rest = val.cons_val.cdr;
    while (is_cons(rest)) {
        args.push(value_to_expr(rest.cons_val.car, interp));
        rest = rest.cons_val.cdr;
    }
    Expr* e = interp.alloc_expr();
    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&interp.ast_arena, ExprCall.sizeof);
    e.call.func = func_e;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)ast_arena_alloc(&interp.ast_arena, Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();
    return e;
}

fn Expr* vte_if_form(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_nil_expr(interp);
    Expr* e = interp.alloc_expr();
    e.tag = E_IF;
    e.if_expr.test = value_to_expr(rest.cons_val.car, interp);
    rest = rest.cons_val.cdr;
    e.if_expr.then_branch = rest_car_expr(rest, interp);
    rest = is_cons(rest) ? rest.cons_val.cdr : rest;
    e.if_expr.else_branch = rest_car_expr(rest, interp);
    return e;
}

fn Expr* vte_define_or_set_form(Value* rest, Interp* interp, bool is_set) {
    if (!is_cons(rest)) return make_nil_expr(interp);
    Expr* e = interp.alloc_expr();
    if (is_set) {
        e.tag = E_SET;
        e.set_expr.is_path = false;
        e.set_expr.path_segments = null;
        e.set_expr.path_segment_count = 0;
        e.set_expr.name = val_to_sym(rest.cons_val.car);
        rest = rest.cons_val.cdr;
        e.set_expr.value = rest_car_expr(rest, interp);
        return e;
    }
    e.tag = E_DEFINE;
    e.define.name = val_to_sym(rest.cons_val.car);
    rest = rest.cons_val.cdr;
    e.define.value = rest_car_expr(rest, interp);
    return e;
}

fn Expr* vte_and_or_form(Value* rest, Interp* interp, bool is_and) {
    if (!is_cons(rest)) return make_nil_expr(interp);
    Expr* e = interp.alloc_expr();
    if (is_and) {
        e.tag = E_AND;
        e.and_expr.left = value_to_expr(rest.cons_val.car, interp);
        rest = rest.cons_val.cdr;
        e.and_expr.right = rest_car_expr(rest, interp);
        return e;
    }
    e.tag = E_OR;
    e.or_expr.left = value_to_expr(rest.cons_val.car, interp);
    rest = rest.cons_val.cdr;
    e.or_expr.right = rest_car_expr(rest, interp);
    return e;
}

fn Expr* vte_special_form(SymbolId sym, Value* rest, Interp* interp) {
    if ((uint)sym == (uint)interp.sym_if)               return vte_if_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_quote) {
        Expr* e = interp.alloc_expr();
        e.tag = E_QUOTE;
        e.quote.datum = is_cons(rest) ? rest.cons_val.car : make_nil(interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_block)            return vte_begin(rest, interp);
    if ((uint)sym == (uint)interp.sym_lambda)           return vte_lambda(rest, interp);
    if ((uint)sym == (uint)interp.sym_let)              return vte_let(rest, interp);
    if ((uint)sym == (uint)interp.sym_define)           return vte_define_or_set_form(rest, interp, false);
    if ((uint)sym == (uint)interp.sym_set)              return vte_define_or_set_form(rest, interp, true);
    if ((uint)sym == (uint)interp.sym_and)              return vte_and_or_form(rest, interp, true);
    if ((uint)sym == (uint)interp.sym_or)               return vte_and_or_form(rest, interp, false);
    if ((uint)sym == (uint)interp.sym_checkpoint) {
        Expr* e = interp.alloc_expr();
        e.tag = E_RESET;
        e.reset.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_capture) {
        if (!is_cons(rest)) return make_nil_expr(interp);
        Value* k_name = rest.cons_val.car;
        rest = rest.cons_val.cdr;
        if (k_name == null || k_name.tag != SYMBOL) return make_nil_expr(interp);
        Expr* e = interp.alloc_expr();
        e.tag = E_SHIFT;
        e.shift.k_name = k_name.sym_val;
        e.shift.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_quasiquote) {
        Expr* e = interp.alloc_expr();
        e.tag = E_QUASIQUOTE;
        e.quasiquote.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_unquote) {
        Expr* e = interp.alloc_expr();
        e.tag = E_UNQUOTE;
        e.unquote.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_unquote_splicing) {
        Expr* e = interp.alloc_expr();
        e.tag = E_UNQUOTE_SPLICING;
        e.unquote_splicing.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_signal) {
        if (!is_cons(rest)) return make_nil_expr(interp);
        Expr* e = interp.alloc_expr();
        e.tag = E_PERFORM;
        e.perform.tag = val_to_sym(rest.cons_val.car);
        rest = rest.cons_val.cdr;
        e.perform.arg = rest_car_expr(rest, interp);
        return e;
    }
    return null;
}

/**
 * Convert a Value (list representation) back to an Expr AST.
 * Used for macro expansion: transforms the macro output back to executable AST.
 */
fn Expr* value_to_expr(Value* val, Interp* interp) {
    if (val == null || val.tag == NIL) return make_nil_expr(interp);

    if (val.tag == INT || val.tag == STRING || val.tag == DOUBLE) {
        Expr* e = interp.alloc_expr();
        e.tag = E_LIT;
        e.lit.value = val;
        return e;
    }

    if (val.tag == SYMBOL) {
        Expr* e = interp.alloc_expr();
        e.tag = E_VAR;
        e.var_expr.name = val.sym_val;
        return e;
    }

    if (val.tag != CONS) {
        // Fallback: return as literal
        Expr* e = interp.alloc_expr();
        e.tag = E_LIT;
        e.lit.value = val;
        return e;
    }

    // CONS -- check if head is a special form symbol
    Value* head = val.cons_val.car;
    if (head == null || head.tag != SYMBOL) return vte_call(val, interp);

    SymbolId sym = head.sym_val;
    Value* rest = val.cons_val.cdr;
    if (Expr* special = vte_special_form(sym, rest, interp)) return special;

    // No special form matched -- generic function call
    return vte_call(val, interp);
}

/**
 * Check if a symbol is a registered macro.
 * Returns the macro definition, or null if not a macro.
 */
