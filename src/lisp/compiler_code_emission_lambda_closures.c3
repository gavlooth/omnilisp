module lisp;

fn bool Compiler.is_variadic_lambda(Compiler* self, usz lambda_id) {
    foreach (def : self.lambda_defs) {
        if (def.id == lambda_id) return def.has_rest;
    }
    return false;
}

// Multi-param lambdas also receive args as a cons list (like variadic)
fn bool Compiler.needs_arg_list(Compiler* self, usz lambda_id) {
    foreach (def : self.lambda_defs) {
        if (def.id == lambda_id) return def.has_rest || def.param_count > 1;
    }
    return false;
}

fn void Compiler.emit_make_closure_call(Compiler* self, usz lambda_id) {
    if (self.needs_arg_list(lambda_id)) {
        self.emit("make_variadic_closure");
    } else {
        self.emit("make_closure");
    }
}

fn LambdaDef* Compiler.find_lambda_def_by_expr(Compiler* self, Expr* expr, usz* lambda_id) {
    foreach (&def : self.lambda_defs) {
        if (def.expr == expr) {
            *lambda_id = def.id;
            return def;
        }
    }
    *lambda_id = 0;
    return null;
}

fn void Compiler.emit_closure_capture_data(Compiler* self, LambdaDef* def, usz lambda_id) {
    self.emit_indent();
    self.emit("Lambda_");
    self.emit_usz(lambda_id);
    self.emit("* _closure_data = (Lambda_");
    self.emit_usz(lambda_id);
    self.emit("*)mem::malloc(Lambda_");
    self.emit_usz(lambda_id);
    self.emit(".sizeof);\n");
    self.emit_indent();
    self.emit("if (_closure_data == null) return aot::make_error(\"compiler: out of memory while allocating closure capture data\");\n");
    for (usz i = 0; i < def.capture_count; i++) {
        self.emit_indent();
        self.emit("_closure_data.captured_");
        self.emit_symbol_name(def.captures[i]);
        self.emit(" = ");
        self.emit_symbol_name(def.captures[i]);
        self.emit(";\n");
    }
}

fn void Compiler.emit_closure_expr(Compiler* self, usz lambda_id, bool with_capture_data) {
    self.emit("aot::");
    self.emit_make_closure_call(lambda_id);
    if (with_capture_data) {
        self.emit("((void*)_closure_data, &invoke_lambda_");
    } else {
        self.emit("(null, &invoke_lambda_");
    }
    self.emit_usz(lambda_id);
    self.emit(")");
}

fn void Compiler.emit_lambda_return_common(Compiler* self, Expr* expr, bool bind_result) {
    usz lambda_id = 0;
    LambdaDef* found_def = g_compiler_force_lambda_emit_lookup_miss
        ? null
        : self.find_lambda_def_by_expr(expr, &lambda_id);
    if (found_def == null) {
        self.set_compile_error("compiler: missing lambda definition during closure emission");
        self.emit_indent();
        self.emit("return aot::make_nil();\n");
        return;
    }
    bool has_captures = found_def != null && found_def.capture_count > 0;
    if (has_captures) {
        self.emit_closure_capture_data(found_def, lambda_id);
    }
    self.emit_indent();
    self.emit("lisp::Value* _result = ");
    self.emit_closure_expr(lambda_id, has_captures);
    self.emit(";\n");
    if (has_captures) {
        self.emit_indent();
        self.emit("if (_result != null && _result.tag == lisp::ValueTag.ERROR) mem::free(_closure_data);\n");
    }
    self.emit_indent();
    self.emit("return _result;\n");
}

fn void Compiler.emit_lambda_return(Compiler* self, Expr* expr) {
    self.emit_lambda_return_common(expr, true);
}
