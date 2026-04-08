module lisp;

import std::collections::list;
import main;

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
    return e;
}

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
    return e;
}

fn Expr* vte_lambda(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_nil_expr(interp);
    Value* params_list = rest.cons_val.car;
    rest = rest.cons_val.cdr;
    Expr* body = rest_car_expr(rest, interp);

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
    Expr* inner = body;
    for (usz i = param_count; i > 1; i--) {
        inner = make_single_lambda_expr(interp, params[i - 1], inner);
    }
    Expr* result = make_single_lambda_expr(interp, params[0], inner);
    params.free();
    return result;
}

fn Expr* vte_let(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_error_expr(interp, "macro expansion emitted malformed let: expected bindings list and body");
    Value* bindings_list = rest.cons_val.car;
    rest = rest.cons_val.cdr;
    Expr* body = rest_car_expr(rest, interp);

    char[] invalid_bindings_msg = "macro expansion emitted invalid let bindings; expected flat pairs: (let (x 1 y 2) ...)";
    if (!is_cons(bindings_list)) return make_error_expr(interp, invalid_bindings_msg);

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
