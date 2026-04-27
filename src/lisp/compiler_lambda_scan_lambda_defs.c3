module lisp;

import std::io;
import std::collections::list;
import main;

fn void Compiler.collect_lambda_own_bound(Compiler* self, ExprLambda* lambda, List{SymbolId}* own_bound) {
    if (lambda.param_count > 1) {
        for (usz pi = 0; pi < lambda.param_count; pi++) {
            own_bound.push(lambda.params[pi]);
        }
    } else if (lambda.param != INVALID_SYMBOL_ID) {
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

fn void Compiler.collect_lambda_nested_bound(Compiler* self, LambdaDef* def, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable, List{SymbolId}* out, List{bool}* out_mutable) {
    foreach (b : *enclosing_bound) {
        out.push(b);
    }
    foreach (m : *enclosing_mutable) {
        out_mutable.push(m);
    }
    if (def.param_count > 0) {
        for (usz pi = 0; pi < def.param_count; pi++) {
            out.push(def.params[pi]);
            out_mutable.push(false);
        }
    } else if (def.param != INVALID_SYMBOL_ID) {
        out.push(def.param);
        out_mutable.push(false);
    }
    if (def.has_rest && (uint)def.rest_param != 0) {
        out.push(def.rest_param);
        out_mutable.push(false);
    }
}

fn bool Compiler.find_enclosing_capture_mutability(Compiler* self, SymbolId capture, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    usz count = enclosing_bound.len();
    if (count != enclosing_mutable.len()) {
        self.set_compile_error("compiler: lambda scan scope mutability invariant failed");
        return false;
    }
    for (usz i = count; i > 0; i--) {
        if ((uint)enclosing_bound.entries[i - 1] == (uint)capture) {
            return enclosing_mutable.entries[i - 1];
        }
    }
    return false;
}

fn bool Compiler.init_lambda_def_from_expr(Compiler* self, LambdaDef* def, Expr* expr) {
    def.captures = null;
    def.capture_is_mutable = null;
    def.capture_count = 0;
    def.capture_capacity = 0;
    def.params = null;
    def.param_count = 0;
    def.param_capacity = 0;

    def.id = self.lambda_counter++;
    def.expr = expr;
    def.param = expr.lambda.param;
    def.body = expr.lambda.body;
    def.capture_capacity = LAMBDA_CAPTURE_INITIAL;

    usz capture_bytes = 0;
    bool capture_size_ok = !g_compiler_force_lambda_capture_size_overflow
        && compiler_checked_mul_usz(SymbolId.sizeof, def.capture_capacity, &capture_bytes);
    if (!capture_size_ok) {
        self.set_compile_error("compiler: lambda capture table size overflow");
        return false;
    }
    def.captures = g_compiler_force_lambda_capture_alloc_fail
        ? null
        : (SymbolId*)mem::malloc(capture_bytes);
    if (def.captures == null) {
        self.set_compile_error("compiler: out of memory while allocating lambda capture table");
        return false;
    }

    usz capture_mutable_bytes = 0;
    bool capture_mutable_size_ok = !g_compiler_force_lambda_capture_size_overflow
        && compiler_checked_mul_usz(bool.sizeof, def.capture_capacity, &capture_mutable_bytes);
    if (!capture_mutable_size_ok) {
        self.set_compile_error("compiler: lambda capture mutability table size overflow");
        self.free_lambda_def(def);
        return false;
    }
    def.capture_is_mutable = g_compiler_force_lambda_capture_alloc_fail
        ? null
        : (bool*)mem::malloc(capture_mutable_bytes);
    if (def.capture_is_mutable == null) {
        self.set_compile_error("compiler: out of memory while allocating lambda capture mutability table");
        self.free_lambda_def(def);
        return false;
    }

    def.has_rest = expr.lambda.has_rest;
    def.rest_param = expr.lambda.rest_param;
    def.param_count = expr.lambda.param_count;
    def.param_capacity = expr.lambda.param_count < LAMBDA_CAPTURE_INITIAL ? LAMBDA_CAPTURE_INITIAL : expr.lambda.param_count;

    usz param_bytes = 0;
    bool param_size_ok = !g_compiler_force_lambda_param_size_overflow
        && compiler_checked_mul_usz(SymbolId.sizeof, def.param_capacity, &param_bytes);
    if (!param_size_ok) {
        self.set_compile_error("compiler: lambda parameter table size overflow");
        self.free_lambda_def(def);
        return false;
    }
    def.params = g_compiler_force_lambda_param_alloc_fail
        ? null
        : (SymbolId*)mem::malloc(param_bytes);
    if (def.params == null) {
        self.set_compile_error("compiler: out of memory while allocating lambda parameter table");
        self.free_lambda_def(def);
        return false;
    }

    for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
        def.params[pi] = expr.lambda.params[pi];
    }
    return true;
}

fn void Compiler.free_lambda_def(Compiler* self, LambdaDef* def) {
    if (def.captures != null) {
        mem::free(def.captures);
        def.captures = null;
    }
    if (def.capture_is_mutable != null) {
        mem::free(def.capture_is_mutable);
        def.capture_is_mutable = null;
    }
    def.capture_count = 0;
    def.capture_capacity = 0;

    if (def.params != null) {
        mem::free(def.params);
        def.params = null;
    }
    def.param_count = 0;
    def.param_capacity = 0;
}

fn bool Compiler.lambda_def_push_capture(Compiler* self, LambdaDef* def, SymbolId capture, bool is_mutable_capture) {
    if (def.captures == null || def.capture_is_mutable == null) {
        self.set_compile_error("compiler: invalid lambda capture table state");
        return false;
    }

    if (def.capture_count >= def.capture_capacity) {
        if (g_compiler_force_lambda_capture_size_overflow
            || def.capture_capacity > usz.max / 2) {
            self.set_compile_error("compiler: lambda capture table capacity overflow");
            return false;
        }

        usz new_cap = def.capture_capacity * 2;
        usz grow_bytes = 0;
        if (!compiler_checked_mul_usz(SymbolId.sizeof, new_cap, &grow_bytes)) {
            self.set_compile_error("compiler: lambda capture table size overflow");
            return false;
        }
        usz mutable_grow_bytes = 0;
        if (!compiler_checked_mul_usz(bool.sizeof, new_cap, &mutable_grow_bytes)) {
            self.set_compile_error("compiler: lambda capture mutability table size overflow");
            return false;
        }

        SymbolId* new_caps = g_compiler_force_lambda_capture_growth_alloc_fail
            ? null
            : (SymbolId*)mem::malloc(grow_bytes);
        if (new_caps == null) {
            self.set_compile_error("compiler: out of memory while growing lambda capture table");
            return false;
        }
        bool* new_mutable = g_compiler_force_lambda_capture_growth_alloc_fail
            ? null
            : (bool*)mem::malloc(mutable_grow_bytes);
        if (new_mutable == null) {
            mem::free(new_caps);
            self.set_compile_error("compiler: out of memory while growing lambda capture mutability table");
            return false;
        }
        for (usz ci = 0; ci < def.capture_count; ci++) new_caps[ci] = def.captures[ci];
        for (usz ci = 0; ci < def.capture_count; ci++) new_mutable[ci] = def.capture_is_mutable[ci];
        mem::free(def.captures);
        mem::free(def.capture_is_mutable);
        def.captures = new_caps;
        def.capture_is_mutable = new_mutable;
        def.capture_capacity = new_cap;
    }
    def.captures[def.capture_count] = capture;
    def.capture_is_mutable[def.capture_count] = is_mutable_capture;
    def.capture_count++;
    return true;
}

fn bool Compiler.scan_lambdas_lambda(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound, List{bool}* enclosing_mutable) {
    if (self.has_error) return false;

    List{SymbolId} own_bound;
    self.collect_lambda_own_bound(expr.lambda, &own_bound);
    List{SymbolId} free;
    self.find_free_vars(expr.lambda.body, &own_bound, &free, enclosing_bound);

    LambdaDef def;
    if (!self.init_lambda_def_from_expr(&def, expr)) {
        own_bound.free();
        free.free();
        return false;
    }

    foreach (f : free) {
        bool capture_is_mutable = self.find_enclosing_capture_mutability(f, enclosing_bound, enclosing_mutable);
        if (self.has_error) {
            self.free_lambda_def(&def);
            own_bound.free();
            free.free();
            return false;
        }
        if (!self.lambda_def_push_capture(&def, f, capture_is_mutable)) {
            self.free_lambda_def(&def);
            own_bound.free();
            free.free();
            return false;
        }
    }

    List{SymbolId} new_enclosing;
    List{bool} new_enclosing_mutable;
    self.collect_lambda_nested_bound(&def, enclosing_bound, enclosing_mutable, &new_enclosing, &new_enclosing_mutable);
    bool body_has_lambda = self.scan_lambdas_with_scope(expr.lambda.body, &new_enclosing, &new_enclosing_mutable);
    if (self.has_error) {
        self.free_lambda_def(&def);
        new_enclosing.free();
        new_enclosing_mutable.free();
        own_bound.free();
        free.free();
        return false;
    }

    def.creates_closure = body_has_lambda;
    if (def.id < 256) {
        self.lambda_creates_closure[def.id] = body_has_lambda;
    }

    self.lambda_defs.push(def);

    new_enclosing.free();
    new_enclosing_mutable.free();
    own_bound.free();
    free.free();
    return true;
}
