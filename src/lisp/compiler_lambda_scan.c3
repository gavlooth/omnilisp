module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 5: MAIN COMPILATION (lambda scan + wrapping)
// =============================================================================
/**
 * Scan expression tree for lambdas and register them.
 */
fn void Compiler.scan_lambdas(Compiler* self, Expr* expr) {
    List{SymbolId} empty;
    self.scan_lambdas_with_scope(expr, &empty);
    empty.free();
}

// Returns true if any E_LAMBDA was found in the subtree (replaces body_creates_closure)
// --- scan_lambdas_with_scope helpers ---

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

fn bool Compiler.scan_lambdas_let(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    List{SymbolId} let_scope;
    foreach (b : *enclosing_bound) {
        let_scope.push(b);
    }
    let_scope.push(expr.let_expr.name);
    bool let_found;
    if (expr.let_expr.is_recursive) {
        let_found = self.scan_lambdas_with_scope(expr.let_expr.init, &let_scope);
    } else {
        let_found = self.scan_lambdas_with_scope(expr.let_expr.init, enclosing_bound);
    }
    bool let_body = self.scan_lambdas_with_scope(expr.let_expr.body, &let_scope);
    let_scope.free();
    return let_found || let_body;
}

fn bool Compiler.scan_lambdas_call(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    bool c_found = self.scan_lambdas_with_scope(expr.call.func, enclosing_bound);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        c_found |= self.scan_lambdas_with_scope(expr.call.args[i], enclosing_bound);
    }
    return c_found;
}

fn Expr* Compiler.make_synthetic_lambda(Compiler* self, Expr* source_expr, SymbolId param, Expr* body) {
    Expr* wrapper = self.interp.alloc_expr();
    wrapper.tag = E_LAMBDA;
    wrapper.lambda = (ExprLambda*)mem::malloc(ExprLambda.sizeof);
    wrapper.loc_line = source_expr.loc_line;
    wrapper.loc_column = source_expr.loc_column;
    wrapper.lambda.param = param;
    wrapper.lambda.param_count = 1;
    wrapper.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof);
    wrapper.lambda.params[0] = param;
    wrapper.lambda.param_annotations = null;
    wrapper.lambda.has_rest = false;
    wrapper.lambda.rest_param = 0;
    wrapper.lambda.has_typed_params = false;
    wrapper.lambda.body = body;
    return wrapper;
}

fn Expr* Compiler.make_pair_projection_app(Compiler* self, SymbolId fn_name, SymbolId pair_param) {
    Expr* pair_ref = self.interp.alloc_expr();
    pair_ref.tag = E_VAR;
    pair_ref.var_expr.name = pair_param;
    Expr* proj_fn = self.interp.alloc_expr();
    proj_fn.tag = E_VAR;
    proj_fn.var_expr.name = fn_name;
    Expr* proj_call = self.interp.alloc_expr();
    proj_call.tag = E_APP;
    proj_call.app.func = proj_fn;
    proj_call.app.arg = pair_ref;
    return proj_call;
}

fn Expr* Compiler.wrap_handle_clause_body(Compiler* self, Expr* source_expr, Expr* orig_handler, SymbolId pair_param, SymbolId k_name, SymbolId arg_name, SymbolId sym_car, SymbolId sym_cdr) {
    Expr* cdr_call = self.make_pair_projection_app(sym_cdr, pair_param);
    Expr* let_arg = self.interp.alloc_expr();
    let_arg.tag = E_LET;
    let_arg.let_expr.name = arg_name;
    let_arg.let_expr.init = cdr_call;
    let_arg.let_expr.body = orig_handler;
    let_arg.let_expr.is_recursive = false;

    Expr* car_call = self.make_pair_projection_app(sym_car, pair_param);
    Expr* let_k = self.interp.alloc_expr();
    let_k.tag = E_LET;
    let_k.let_expr.name = k_name;
    let_k.let_expr.init = car_call;
    let_k.let_expr.body = let_arg;
    let_k.let_expr.is_recursive = false;

    return self.make_synthetic_lambda(source_expr, pair_param, let_k);
}

fn bool Compiler.scan_lambdas_reset(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
    Expr* orig_body = expr.reset.body;
    Expr* wrapper = self.make_synthetic_lambda(expr, dummy_param, orig_body);
    expr.reset.body = wrapper;
    return self.scan_lambdas_with_scope(wrapper, enclosing_bound);
}

fn bool Compiler.scan_lambdas_shift(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    Expr* orig_body = expr.shift.body;
    Expr* wrapper = self.make_synthetic_lambda(expr, expr.shift.k_name, orig_body);
    expr.shift.body = wrapper;
    return self.scan_lambdas_with_scope(wrapper, enclosing_bound);
}

fn bool Compiler.scan_lambdas_handle(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
    Expr* orig_body = expr.handle.body;
    Expr* body_wrapper = self.make_synthetic_lambda(expr, dummy_param, orig_body);
    expr.handle.body = body_wrapper;
    bool h_found = self.scan_lambdas_with_scope(body_wrapper, enclosing_bound);

    SymbolId pair_param = self.interp.symbols.intern("_eff_pair");
    SymbolId sym_car = self.interp.symbols.intern("car");
    SymbolId sym_cdr = self.interp.symbols.intern("cdr");
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        Expr* orig_handler = expr.handle.clauses[i].handler_body;
        SymbolId k_name = expr.handle.clauses[i].k_name;
        SymbolId arg_name = expr.handle.clauses[i].arg_name;
        Expr* handler_wrapper = self.wrap_handle_clause_body(expr, orig_handler, pair_param, k_name, arg_name, sym_car, sym_cdr);
        expr.handle.clauses[i].handler_body = handler_wrapper;
        h_found |= self.scan_lambdas_with_scope(handler_wrapper, enclosing_bound);
    }
    return h_found;
}

fn bool Compiler.scan_lambdas_binary_expr(Compiler* self, Expr* left, Expr* right, List{SymbolId}* enclosing_bound) {
    bool left_found = self.scan_lambdas_with_scope(left, enclosing_bound);
    return self.scan_lambdas_with_scope(right, enclosing_bound) || left_found;
}

fn bool Compiler.scan_lambdas_if_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    bool test_found = self.scan_lambdas_with_scope(expr.if_expr.test, enclosing_bound);
    bool then_found = self.scan_lambdas_with_scope(expr.if_expr.then_branch, enclosing_bound);
    return self.scan_lambdas_with_scope(expr.if_expr.else_branch, enclosing_bound) || test_found || then_found;
}

fn bool Compiler.scan_lambdas_match_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    bool found = self.scan_lambdas_with_scope(expr.match.scrutinee, enclosing_bound);
    for (usz i = 0; i < expr.match.clause_count; i++) {
        found |= self.scan_lambdas_with_scope(expr.match.clauses[i].result, enclosing_bound);
    }
    return found;
}

fn bool Compiler.scan_lambdas_begin_expr(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    bool found = false;
    for (usz i = 0; i < expr.begin.expr_count; i++) {
        found |= self.scan_lambdas_with_scope(expr.begin.exprs[i], enclosing_bound);
    }
    return found;
}

fn bool Compiler.scan_lambdas_module_expr(Compiler* self, Expr* expr) {
    bool found = false;
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        List{SymbolId} mod_empty;
        found |= self.scan_lambdas_with_scope(expr.module_expr.body[i], &mod_empty);
        mod_empty.free();
    }
    return found;
}

fn bool Compiler.scan_lambdas_with_scope(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_LAMBDA: return self.scan_lambdas_lambda(expr, enclosing_bound);
        case E_APP: return self.scan_lambdas_binary_expr(expr.app.func, expr.app.arg, enclosing_bound);
        case E_IF: return self.scan_lambdas_if_expr(expr, enclosing_bound);
        case E_LET: return self.scan_lambdas_let(expr, enclosing_bound);
        case E_DEFINE: return self.scan_lambdas_with_scope(expr.define.value, enclosing_bound);
        case E_AND: return self.scan_lambdas_binary_expr(expr.and_expr.left, expr.and_expr.right, enclosing_bound);
        case E_OR: return self.scan_lambdas_binary_expr(expr.or_expr.left, expr.or_expr.right, enclosing_bound);
        case E_MATCH: return self.scan_lambdas_match_expr(expr, enclosing_bound);
        case E_CALL: return self.scan_lambdas_call(expr, enclosing_bound);
        case E_INDEX: return self.scan_lambdas_binary_expr(expr.index.collection, expr.index.index, enclosing_bound);
        case E_PATH: return false;
        case E_RESET: return self.scan_lambdas_reset(expr, enclosing_bound);
        case E_SHIFT: return self.scan_lambdas_shift(expr, enclosing_bound);
        case E_PERFORM: return self.scan_lambdas_with_scope(expr.perform.arg, enclosing_bound);
        case E_RESOLVE: return self.scan_lambdas_with_scope(expr.resolve.value, enclosing_bound);
        case E_HANDLE: return self.scan_lambdas_handle(expr, enclosing_bound);
        case E_BEGIN: return self.scan_lambdas_begin_expr(expr, enclosing_bound);
        case E_SET: return self.scan_lambdas_with_scope(expr.set_expr.value, enclosing_bound);
        case E_QUASIQUOTE: return self.scan_lambdas_with_scope(expr.quasiquote.body, enclosing_bound);
        case E_UNQUOTE: return self.scan_lambdas_with_scope(expr.unquote.body, enclosing_bound);
        case E_UNQUOTE_SPLICING: return self.scan_lambdas_with_scope(expr.unquote_splicing.body, enclosing_bound);
        case E_DEFMACRO: return false;
        case E_MODULE: return self.scan_lambdas_module_expr(expr);
        case E_IMPORT:
        case E_EXPORT_FROM: return false;
        default: return false;
    }
}
