module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (let/set and primitive checks)
// =============================================================================
fn usz Compiler.compile_if_flat(Compiler* self, Expr* expr) {
    return self.compile_if_common(expr, false);
}

fn usz Compiler.compile_and_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.and_expr.left);
    return self.compile_short_circuit(left, expr.and_expr.right, false, false);
}

fn usz Compiler.compile_or_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.or_expr.left);
    return self.compile_short_circuit(left, expr.or_expr.right, true, false);
}

fn usz Compiler.compile_block_flat(Compiler* self, Expr* expr) {
    return self.compile_block_common(expr, false);
}

fn usz Compiler.compile_mutable_captured_set_flat(Compiler* self, SymbolId name, Expr* value) {
    usz val_r = self.compile_to_temp(value);
    self.emit_return_if_sequence_should_stop(val_r);
    SymbolId cell = INVALID_SYMBOL_ID;
    if (!self.find_active_mutable_cell(name, &cell)) {
        self.set_compile_error("compiler: mutable captured set! missing active lexical cell");
        return self.emit_nil_temp();
    }
    usz set_err_r = self.next_result();
    self.emit_indent();
    self.emit_temp_decl(set_err_r);
    self.emit(" = null;\n");
    usz set_ok_r = self.next_result();
    self.emit_indent();
    self.emit("bool _aot_cell_set_ok_");
    self.emit_usz(set_ok_r);
    self.emit(" = aot::mutable_cell_set_checked(");
    self.emit_symbol_name(cell);
    self.emit(", ");
    self.emit_temp_ref(val_r);
    self.emit(", &");
    self.emit_temp_ref(set_err_r);
    self.emit(");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (!_aot_cell_set_ok_");
    self.emit_usz(set_ok_r);
    self.emit(") {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(set_err_r);
    self.emit(" != null ? ");
    self.emit_temp_ref(set_err_r);
    self.emit(" : aot::make_error(\"compiler: failed to update mutable capture cell\");\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = aot::make_void();\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn usz Compiler.compile_local_set_flat(Compiler* self, SymbolId name, Expr* value) {
    usz val_r = self.compile_to_temp(value);
    self.emit_return_if_sequence_should_stop(val_r);
    self.emit_indent();
    (void)self.emit_symbol_assignment_target(name);
    self.emit(" = ");
    self.emit_temp_ref(val_r);
    self.emit(";\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_void();\n");
    return id;
}

fn usz Compiler.compile_env_set_flat(Compiler* self, SymbolId name, Expr* value) {
    usz val_r = self.compile_to_temp(value);
    self.emit_return_if_sequence_should_stop(val_r);

    usz set_err_r = self.next_result();
    self.emit_indent();
    self.emit_temp_decl(set_err_r);
    self.emit(" = aot::set_var_result(\"");
    char[] set_name = self.interp.symbols.get_name(name);
    self.emit_escaped(set_name);
    self.emit("\", ");
    self.emit_temp_ref(val_r);
    self.emit(");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (");
    self.emit_temp_ref(set_err_r);
    self.emit(" != null) {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(set_err_r);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = aot::make_void();\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn bool Compiler.has_local_assignment_target(Compiler* self, SymbolId name) {
    SymbolId backing = INVALID_SYMBOL_ID;
    if (self.find_active_lexical_alias(name, &backing)) return true;
    if (self.find_active_module_private_backing(name, &backing)) return true;
    for (usz i = self.module_open_exports.len(); i > 0; i--) {
        if ((uint)self.module_open_exports.entries[i - 1] == (uint)name) return true;
    }
    if (self.module_private_symbols.len() > 0) return true;
    if (self.is_global(name)) return true;
    if (self.is_declared(name)) return true;
    return false;
}

fn usz Compiler.compile_set_flat(Compiler* self, Expr* expr) {
    SymbolId cell = INVALID_SYMBOL_ID;
    SymbolId backing = INVALID_SYMBOL_ID;
    if (self.find_active_lexical_alias(expr.set_expr.name, &backing)) {
        return self.compile_local_set_flat(expr.set_expr.name, expr.set_expr.value);
    }
    if (self.find_active_mutable_cell_from(expr.set_expr.name, self.module_binding_mutable_cell_mark, &cell)) {
        return self.compile_mutable_captured_set_flat(expr.set_expr.name, expr.set_expr.value);
    }
    if (self.has_local_assignment_target(expr.set_expr.name)) {
        return self.compile_local_set_flat(expr.set_expr.name, expr.set_expr.value);
    }
    if (self.find_active_mutable_cell(expr.set_expr.name, &cell)) {
        return self.compile_mutable_captured_set_flat(expr.set_expr.name, expr.set_expr.value);
    }
    return self.compile_env_set_flat(expr.set_expr.name, expr.set_expr.value);
}

fn bool Compiler.is_builtin_primitive(Compiler* self, SymbolId sym) {
    // Check ONLY compiler builtins, NOT defined_globals. Delegation injection
    // still needs defined globals injected into the interpreter environment
    // because they are C3 locals, not interpreter variables.
    return self.is_compiler_builtin_symbol(sym);
}
