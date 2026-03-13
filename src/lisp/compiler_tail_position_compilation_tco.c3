module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 8b2: TAIL-POSITION COMPILATION (TCO)
// =============================================================================

/**
 * compile_module_flat — Flat-style module compilation.
 * Compiles module body expressions inline, registers module in table.
 */
fn usz Compiler.compile_module_flat(Compiler* self, Expr* expr) {
    // Register the module in compiled_modules table
    if (self.compiled_module_count >= self.compiled_module_capacity) {
        usz new_cap = self.compiled_module_capacity * 2;
        CompiledModule* new_mods = (CompiledModule*)mem::malloc(CompiledModule.sizeof * new_cap);
        for (usz i = 0; i < self.compiled_module_count; i++) new_mods[i] = self.compiled_modules[i];
        mem::free(self.compiled_modules);
        self.compiled_modules = new_mods;
        self.compiled_module_capacity = new_cap;
    }
    {
        CompiledModule* mod = &self.compiled_modules[self.compiled_module_count];
        mod.name = expr.module_expr.name;
        usz cap = expr.module_expr.export_count < 16 ? 16 : expr.module_expr.export_count;
        mod.exports = (SymbolId*)mem::malloc(SymbolId.sizeof * cap);
        mod.export_capacity = cap;
        mod.export_count = expr.module_expr.export_count;
        for (usz i = 0; i < expr.module_expr.export_count; i++) {
            mod.exports[i] = expr.module_expr.exports[i];
        }
        mod.loaded = true;
        self.compiled_module_count++;
    }

    // Compile body expressions inline
    usz last_id = 0;
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        Expr* body_expr = expr.module_expr.body[i];
        if (body_expr.tag == E_DEFINE) {
            // Define: compile init, assign to global
            usz init_r = self.compile_to_temp(body_expr.define.value);
            self.emit_indent();
            self.emit_symbol_name(body_expr.define.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
            last_id = init_r;
        } else {
            // Non-define: compile inline
            last_id = self.compile_to_temp(body_expr);
        }
    }

    // Module is command-style and evaluates to Void.
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_void();\n");
    return id;
}

/**
 * compile_import_flat — Flat-style import compilation.
 * For inline-compiled modules, all definitions are already globals — no-op.
 */
fn usz Compiler.compile_import_flat(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_void(); // import: inline module\n");
    return id;
}

/**
 * compile_export_from_flat — export-from is a no-op in compiled mode
 * (re-exported symbols are already inlined as globals).
 */
fn usz Compiler.compile_export_from_flat(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_void(); // export-from: no-op in compiled mode\n");
    return id;
}
