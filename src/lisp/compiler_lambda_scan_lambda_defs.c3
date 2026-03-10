module lisp;

import std::io;
import std::collections::list;
import main;

fn void Compiler.collect_lambda_own_bound(Compiler* self, ExprLambda* lambda, List{SymbolId}* own_bound) {
    if (lambda.param_count > 1) {
        for (usz pi = 0; pi < lambda.param_count; pi++) {
            own_bound.push(lambda.params[pi]);
        }
    } else if ((uint)lambda.param != 0xFFFFFFFF) {
        own_bound.push(lambda.param);
    }
    // Keep existing behavior: variadic lambdas append fixed params and rest param.
    if (lambda.has_rest) {
        for (usz pi = 0; pi < lambda.param_count; pi++) {
            own_bound.push(lambda.params[pi]);
        }
        if ((uint)lambda.rest_param != 0) {
            own_bound.push(lambda.rest_param);
        }
    }
}

fn void Compiler.collect_lambda_nested_bound(Compiler* self, ExprLambda* lambda, List{SymbolId}* enclosing_bound, List{SymbolId}* out) {
    foreach (b : *enclosing_bound) {
        out.push(b);
    }
    if (lambda.param_count > 1) {
        for (usz pi = 0; pi < lambda.param_count; pi++) {
            out.push(lambda.params[pi]);
        }
    } else if ((uint)lambda.param != 0xFFFFFFFF) {
        out.push(lambda.param);
    }
    if (lambda.has_rest && (uint)lambda.rest_param != 0) {
        out.push(lambda.rest_param);
    }
}

fn void Compiler.init_lambda_def_from_expr(Compiler* self, LambdaDef* def, Expr* expr) {
    def.id = self.lambda_counter++;
    def.param = expr.lambda.param;
    def.body = expr.lambda.body;
    def.capture_count = 0;
    def.capture_capacity = LAMBDA_CAPTURE_INITIAL;
    def.captures = (SymbolId*)mem::malloc(SymbolId.sizeof * def.capture_capacity);
    def.has_rest = expr.lambda.has_rest;
    def.rest_param = expr.lambda.rest_param;
    def.param_count = expr.lambda.param_count;
    def.param_capacity = expr.lambda.param_count < LAMBDA_CAPTURE_INITIAL ? LAMBDA_CAPTURE_INITIAL : expr.lambda.param_count;
    def.params = (SymbolId*)mem::malloc(SymbolId.sizeof * def.param_capacity);
    for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
        def.params[pi] = expr.lambda.params[pi];
    }
}

fn void Compiler.lambda_def_push_capture(Compiler* self, LambdaDef* def, SymbolId capture) {
    if (def.capture_count >= def.capture_capacity) {
        usz new_cap = def.capture_capacity * 2;
        SymbolId* new_caps = (SymbolId*)mem::malloc(SymbolId.sizeof * new_cap);
        for (usz ci = 0; ci < def.capture_count; ci++) new_caps[ci] = def.captures[ci];
        mem::free(def.captures);
        def.captures = new_caps;
        def.capture_capacity = new_cap;
    }
    def.captures[def.capture_count++] = capture;
}

fn bool Compiler.scan_lambdas_lambda(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    List{SymbolId} own_bound;
    self.collect_lambda_own_bound(expr.lambda, &own_bound);
    List{SymbolId} free;
    self.find_free_vars(expr.lambda.body, &own_bound, &free, enclosing_bound);

    LambdaDef def;
    self.init_lambda_def_from_expr(&def, expr);

    foreach (f : free) {
        self.lambda_def_push_capture(&def, f);
    }

    List{SymbolId} new_enclosing;
    self.collect_lambda_nested_bound(expr.lambda, enclosing_bound, &new_enclosing);
    bool body_has_lambda = self.scan_lambdas_with_scope(expr.lambda.body, &new_enclosing);

    def.creates_closure = body_has_lambda;
    if (def.id < 256) {
        self.lambda_creates_closure[def.id] = body_has_lambda;
    }

    self.lambda_defs.push(def);

    new_enclosing.free();
    own_bound.free();
    free.free();
    return true;
}
