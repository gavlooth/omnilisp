module lisp;

import main;
// =============================================================================
// SECTION: NATIVE EFFECT COMPILATION (flat style)
// =============================================================================

fn usz Compiler.compile_reset_flat(Compiler* self, Expr* expr) {
    // Body has been wrapped as a lambda during prescan
    usz body_r = self.compile_to_temp(expr.reset.body);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_reset(");
    self.emit_temp_ref(body_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_shift_flat(Compiler* self, Expr* expr) {
    // Body has been wrapped as lambda (lambda (k) ...) during prescan
    usz body_r = self.compile_to_temp(expr.shift.body);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_shift(");
    self.emit_temp_ref(body_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_perform_flat(Compiler* self, Expr* expr) {
    // Compile the argument
    usz arg_r = self.compile_to_temp(expr.perform.arg);
    // Create tag symbol
    usz tag_r = self.next_result();
    self.emit_temp_decl(tag_r);
    self.emit(" = aot::make_symbol(\"");
    char[] tag_name = self.interp.symbols.get_name(expr.perform.tag);
    self.emit_escaped(tag_name);
    self.emit("\");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_signal(");
    self.emit_temp_ref(tag_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_handle_flat(Compiler* self, Expr* expr) {
    usz clause_count = expr.handle.clause_count;

    // Compile all handler closures and tags
    usz[8] handler_rs;
    usz[8] tag_rs;
    for (usz i = 0; i < clause_count && i < 8; i++) {
        tag_rs[i] = self.compile_handle_effect_tag(expr.handle.clauses[i].effect_tag);
        // Handler closure (synthetic lambda from prescan)
        handler_rs[i] = self.compile_to_temp(expr.handle.clauses[i].handler_body);
    }

    // Compile body closure (synthetic lambda from prescan)
    usz body_r = self.compile_to_temp(expr.handle.body);

    // Build tags and handlers arrays (C3 array syntax: Type[N] name).
    usz tags_arr = 0;
    usz hdlrs_arr = 0;
    self.emit_handle_arrays(clause_count, &tags_arr, &hdlrs_arr);

    for (usz i = 0; i < clause_count && i < 8; i++) {
        self.emit_handle_array_slot("_eff_tags_", tags_arr, i, tag_rs[i]);
        self.emit_handle_array_slot("_eff_hdlrs_", hdlrs_arr, i, handler_rs[i]);
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_handle(&_eff_tags_");
    self.emit_usz(tags_arr);
    self.emit(", &_eff_hdlrs_");
    self.emit_usz(hdlrs_arr);
    self.emit(", ");
    self.emit_usz(clause_count);
    self.emit(", ");
    self.emit_temp_ref(body_r);
    self.emit(");\n");
    return id;
}
