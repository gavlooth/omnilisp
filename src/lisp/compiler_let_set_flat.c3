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
    self.emit_indent();
    self.emit("aot::set_var(\"");
    char[] set_name = self.interp.symbols.get_name(name);
    self.emit_escaped(set_name);
    self.emit("\", ");
    self.emit_temp_ref(val_r);
    self.emit(");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_void();\n");
    return id;
}

fn usz Compiler.compile_local_set_flat(Compiler* self, SymbolId name, Expr* value) {
    usz val_r = self.compile_to_temp(value);
    self.emit_indent();
    self.emit_symbol_name(name);
    self.emit(" = ");
    self.emit_temp_ref(val_r);
    self.emit(";\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_void();\n");
    return id;
}

fn usz Compiler.compile_set_flat(Compiler* self, Expr* expr) {
    if (self.is_mutable_captured_var(expr.set_expr.name)) {
        return self.compile_mutable_captured_set_flat(expr.set_expr.name, expr.set_expr.value);
    }
    return self.compile_local_set_flat(expr.set_expr.name, expr.set_expr.value);
}

fn bool Compiler.is_builtin_primitive(Compiler* self, SymbolId sym) {
    // Check ONLY compiler builtins, NOT defined_globals. Delegation injection
    // still needs defined globals injected into the interpreter environment
    // because they are C3 locals, not interpreter variables.
    return self.is_compiler_builtin_symbol(sym);
}
