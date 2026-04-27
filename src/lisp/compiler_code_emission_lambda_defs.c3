module lisp;

fn SymbolId Compiler.lambda_mutable_cell_symbol(Compiler* self, LambdaDef* def, usz capture_index) {
    char[96] cell_buf;
    char[] cell_text = self.generated_name_pair(cell_buf[..], "_aot_cell_lambda_", (long)def.id, (long)capture_index);
    return self.interp.symbols.intern(cell_text);
}

fn void Compiler.emit_lambda_signature(Compiler* self, LambdaDef* def, bool is_zero_arg) {
    self.emit("fn lisp::Value* invoke_lambda_");
    self.emit_usz(def.id);
    if (def.has_rest || def.param_count > 1) {
        self.emit("(void* _self, lisp::Value* _arg_list) {\n");
        return;
    }
    if (is_zero_arg) {
        self.emit("(void* _self, lisp::Value* _unused) {\n");
        return;
    }
    self.emit("(void* _self, lisp::Value* ");
    self.emit_symbol_name(def.param);
    self.emit(") {\n");
}

fn void Compiler.emit_lambda_param_unpack(Compiler* self, LambdaDef* def, bool is_zero_arg) {
    if (def.has_rest || def.param_count > 1) return self.emit_lambda_param_unpack_list(def);
    if (!is_zero_arg) {
        self.mark_declared(def.param);
    }
}

fn void Compiler.emit_lambda_param_unpack_list(Compiler* self, LambdaDef* def) {
    self.emit_indent();
    self.emit("usz _arg_count = 0;\n");
    self.emit_indent();
    self.emit("lisp::Value* _arg_count_error = aot::count_arg_list(_arg_list, &_arg_count);\n");
    self.emit_indent();
    self.emit("if (_arg_count_error != null) return _arg_count_error;\n");
    if (def.has_rest) {
        if (def.param_count > 0) {
            self.emit_indent();
            self.emit("if (_arg_count < ");
            self.emit_usz(def.param_count);
            self.emit(") return aot::make_variadic_min_arity_error(");
            self.emit_usz(def.param_count);
            self.emit(", _arg_count);\n");
        }
    } else {
        self.emit_indent();
        self.emit("if (_arg_count < ");
        self.emit_usz(def.param_count);
        self.emit(") return aot::make_arity_error(");
        self.emit_usz(def.param_count);
        self.emit(", _arg_count);\n");
    }
    self.emit_indent();
    self.emit("lisp::Value* _curr = _arg_list;\n");
    for (usz pi = 0; pi < def.param_count; pi++) {
        self.mark_declared(def.params[pi]);
        self.emit_indent();
        self.emit("lisp::Value* ");
        self.emit_symbol_name(def.params[pi]);
        self.emit(" = aot::car(_curr);\n");
        self.emit_indent();
        self.emit("_curr = aot::cdr(_curr);\n");
    }
    if (!def.has_rest) return;

    self.mark_declared(def.rest_param);
    self.emit_indent();
    self.emit("lisp::Value* ");
    self.emit_symbol_name(def.rest_param);
    self.emit(" = _curr;\n");
}

fn void Compiler.emit_lambda_capture_bindings(Compiler* self, LambdaDef* def) {
    if (def.capture_count == 0) return;
    self.emit_indent();
    self.emit("Lambda_");
    self.emit_usz(def.id);
    self.emit("* self = (Lambda_");
    self.emit_usz(def.id);
    self.emit("*)_self;\n");
    for (usz i = 0; i < def.capture_count; i++) {
        if (def.capture_is_mutable[i]) {
            SymbolId cell_name = self.lambda_mutable_cell_symbol(def, i);
            if (cell_name == INVALID_SYMBOL_ID) {
                self.set_compile_error("compiler: invalid mutable lambda capture cell symbol");
                return;
            }
            self.mark_declared(cell_name);
            self.emit_indent();
            self.emit("aot::AotMutableCell* ");
            self.emit_symbol_name(cell_name);
            self.emit(" = self.captured_");
            self.emit_symbol_name(def.captures[i]);
            self.emit(";\n");
        } else {
            self.mark_declared(def.captures[i]);
            self.emit_indent();
            self.emit("lisp::Value* ");
            self.emit_symbol_name(def.captures[i]);
            self.emit(" = self.captured_");
            self.emit_symbol_name(def.captures[i]);
            self.emit(";\n");
        }
    }
}

fn void Compiler.emit_lambda_body_return(Compiler* self, LambdaDef* def) {
    usz pushed_count = 0;
    for (usz pi = 0; pi < def.param_count; pi++) {
        if (self.push_primitive_shadow_if_needed(def.params[pi])) pushed_count++;
    }
    if (def.param_count <= 1 && def.param != INVALID_SYMBOL_ID) {
        if (self.push_primitive_shadow_if_needed(def.param)) pushed_count++;
    }
    if (def.has_rest && def.rest_param != INVALID_SYMBOL_ID) {
        if (self.push_primitive_shadow_if_needed(def.rest_param)) pushed_count++;
    }
    for (usz ci = 0; ci < def.capture_count; ci++) {
        if (self.push_primitive_shadow_if_needed(def.captures[ci])) pushed_count++;
        if (def.capture_is_mutable[ci]) {
            SymbolId cell_name = self.lambda_mutable_cell_symbol(def, ci);
            if (cell_name == INVALID_SYMBOL_ID) {
                self.set_compile_error("compiler: invalid mutable lambda capture cell alias");
                continue;
            }
            self.push_mutable_cell_alias(def.captures[ci], cell_name);
        }
    }
    if (def.body.tag == E_LAMBDA) {
        self.emit_lambda_return(def.body);
        for (usz ci = def.capture_count; ci > 0; ci--) {
            if (def.capture_is_mutable[ci - 1]) self.pop_mutable_cell_alias();
        }
        for (usz i = 0; i < pushed_count; i++) {
            self.pop_primitive_shadow(true);
        }
        return;
    }
    usz body_r = self.compile_to_temp_tail(def.body);
    for (usz ci = def.capture_count; ci > 0; ci--) {
        if (def.capture_is_mutable[ci - 1]) self.pop_mutable_cell_alias();
    }
    for (usz i = 0; i < pushed_count; i++) {
        self.pop_primitive_shadow(true);
    }
    if (!def.has_rest && def.param_count > 1) {
        self.emit_indent();
        self.emit("if (_arg_count > ");
        self.emit_usz(def.param_count);
        self.emit(") return aot::apply_multi(");
        self.emit_temp_ref(body_r);
        self.emit(", _curr, (long)(_arg_count - ");
        self.emit_usz(def.param_count);
        self.emit("));\n");
    }
    self.emit_indent();
    self.emit("return ");
    self.emit_temp_ref(body_r);
    self.emit(";\n");
}
