module lisp;

fn void Compiler.emit_global_declarations(Compiler* self) {
    if (self.defined_globals.len() == 0 && self.referenced_prims.len() == 0) return;
    self.emit_global_value_declarations();
    self.emit_cached_primitive_declarations();
}

fn void Compiler.emit_global_value_declarations(Compiler* self) {
    if (self.defined_globals.len() == 0) return;
    self.emit("// Global variables\n");
    foreach (name : self.defined_globals) {
        self.emit("lisp::Value* ");
        self.emit_symbol_name(name);
        self.emit(";\n");
    }
    self.emit_newline();
}

fn void Compiler.emit_cached_primitive_declarations(Compiler* self) {
    if (self.referenced_prims.len() == 0) return;
    // D2: Cached primitive globals (initialized once in main after aot_init)
    self.emit("// Cached primitives\n");
    foreach (sym : self.referenced_prims) {
        self.emit("lisp::Value* ");
        self.emit_prim_global_name(sym);
        self.emit(";\n");
    }
    self.emit_newline();
}

fn void Compiler.emit_main_start(Compiler* self) {
    self.emit("fn int main() {\n");
    self.indent++;

    // Initialize AOT interpreter
    self.emit_line("aot::aot_init();");
    self.emit_newline();

    // Initialize all global variables to nil
    if (self.defined_globals.len() > 0) {
        self.emit_line("// Initialize global variables to nil");
        foreach (name : self.defined_globals) {
            self.emit_indent();
            self.emit_symbol_name(name);
            self.emit(" = aot::make_nil();\n");
        }
        self.emit_newline();
    }

    // Macros (when, unless, cond) are expanded at compile time — no runtime registration needed
    self.emit_newline();
}

fn void Compiler.emit_main_end(Compiler* self) {
    self.emit_newline();
    // Shutdown AOT interpreter
    self.emit_line("aot::aot_shutdown();");
    self.emit_line("return 0;");
    self.indent--;
    self.emit("}\n");
}
