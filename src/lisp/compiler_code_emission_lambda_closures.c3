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
        self.emit("make_generated_variadic_closure_with_retention");
    } else {
        self.emit("make_generated_closure_with_retention");
    }
}

fn void Compiler.emit_make_closure_with_retention_call(Compiler* self, usz lambda_id) {
    if (self.needs_arg_list(lambda_id)) {
        self.emit("make_generated_variadic_closure_with_retention");
    } else {
        self.emit("make_generated_closure_with_retention");
    }
}

fn bool Compiler.lambda_needs_capture_retention(Compiler* self, LambdaDef* def) {
    if (def == null) return false;
    for (usz i = 0; i < def.capture_count; i++) {
        if (!def.capture_is_mutable[i]) return true;
    }
    return false;
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

fn void Compiler.emit_lambda_capture_source(Compiler* self, LambdaDef* def, usz capture_index) {
    SymbolId capture = def.captures[capture_index];
    if (def.capture_is_mutable[capture_index]) {
        SymbolId cell_name = INVALID_SYMBOL_ID;
        if (!self.find_active_mutable_cell(capture, &cell_name)) {
            self.set_compile_error("compiler: mutable lambda capture missing active cell");
            self.emit("null");
            return;
        }
        self.emit_symbol_name(cell_name);
        return;
    }
    self.emit_symbol_value_reference(capture);
}

fn void Compiler.emit_closure_capture_data(Compiler* self, LambdaDef* def, usz lambda_id, bool needs_retention) {
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
    if (needs_retention) {
        self.emit_indent();
        self.emit("aot::AotCaptureRetention* _closure_retention = aot::capture_retention_begin();\n");
        self.emit_indent();
        self.emit("if (_closure_retention == null) { mem::free(_closure_data); return aot::make_error(\"compiler: out of memory while allocating closure capture retention\"); }\n");
    } else {
        self.emit_indent();
        self.emit("aot::AotCaptureRetention* _closure_retention = null;\n");
    }
    for (usz i = 0; i < def.capture_count; i++) {
        if (def.capture_is_mutable[i]) {
            self.emit_indent();
            self.emit("_closure_data.captured_");
            self.emit_symbol_name(def.captures[i]);
            self.emit(" = ");
            self.emit_lambda_capture_source(def, i);
            self.emit(";\n");
            continue;
        }
        self.emit_indent();
        self.emit("lisp::Value* _capture_source_");
        self.emit_usz(lambda_id);
        self.emit("_");
        self.emit_usz(i);
        self.emit(" = ");
        self.emit_lambda_capture_source(def, i);
        self.emit(";\n");
        self.emit_indent();
        self.emit("lisp::Value* _capture_retain_error_");
        self.emit_usz(lambda_id);
        self.emit("_");
        self.emit_usz(i);
        self.emit(" = aot::capture_retention_retain_value(_closure_retention, _capture_source_");
        self.emit_usz(lambda_id);
        self.emit("_");
        self.emit_usz(i);
        self.emit(");\n");
        self.emit_indent();
        self.emit("if (_capture_retain_error_");
        self.emit_usz(lambda_id);
        self.emit("_");
        self.emit_usz(i);
        self.emit(" != null) { aot::capture_retention_release(_closure_retention); mem::free(_closure_data); return _capture_retain_error_");
        self.emit_usz(lambda_id);
        self.emit("_");
        self.emit_usz(i);
        self.emit("; }\n");
        self.emit_indent();
        self.emit("_closure_data.captured_");
        self.emit_symbol_name(def.captures[i]);
        self.emit(" = _capture_source_");
        self.emit_usz(lambda_id);
        self.emit("_");
        self.emit_usz(i);
        self.emit(";\n");
    }
}

fn void Compiler.emit_closure_expr(Compiler* self, usz lambda_id, bool with_capture_data) {
    self.emit("aot::");
    if (with_capture_data) {
        self.emit_make_closure_with_retention_call(lambda_id);
    } else {
        self.emit_make_closure_call(lambda_id);
    }
    if (with_capture_data) {
        self.emit("((void*)_closure_data, &invoke_lambda_");
    } else {
        self.emit("(null, &invoke_lambda_");
    }
    self.emit_usz(lambda_id);
    if (with_capture_data) {
        self.emit(", _closure_retention");
    } else {
        self.emit(", null");
    }
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
    bool needs_retention = self.lambda_needs_capture_retention(found_def);
    if (has_captures) {
        self.emit_closure_capture_data(found_def, lambda_id, needs_retention);
    }
    self.emit_indent();
    self.emit("lisp::Value* _result = ");
    self.emit_closure_expr(lambda_id, has_captures);
    self.emit(";\n");
    self.emit_indent();
    self.emit("return _result;\n");
}

fn void Compiler.emit_lambda_return(Compiler* self, Expr* expr) {
    self.emit_lambda_return_common(expr, true);
}
