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
    if (self.has_error) return self.emit_nil_temp();

    if (self.compiled_modules == null || self.compiled_module_capacity == 0) {
        usz init_cap = 16;
        usz init_bytes = 0;
        bool init_size_ok = compiler_checked_mul_usz(CompiledModule.sizeof, init_cap, &init_bytes);
        if (!init_size_ok) {
            self.set_compile_error("compiler: compiled module registry allocation size overflow");
            return self.emit_nil_temp();
        }
        CompiledModule* init_mods = g_compiler_force_module_registry_alloc_fail
            ? null
            : (CompiledModule*)mem::malloc(init_bytes);
        if (init_mods == null) {
            self.set_compile_error("compiler: out of memory while allocating compiled module registry");
            return self.emit_nil_temp();
        }
        self.compiled_modules = init_mods;
        self.compiled_module_capacity = init_cap;
        self.compiled_module_count = 0;
    }

    // Register the module in compiled_modules table
    if (self.compiled_module_count >= self.compiled_module_capacity) {
        if (g_compiler_force_module_registry_growth_overflow
            || self.compiled_module_capacity > usz.max / 2) {
            self.set_compile_error("compiler: compiled module registry capacity overflow");
            return self.emit_nil_temp();
        }
        usz new_cap = self.compiled_module_capacity * 2;
        if (new_cap < 16) new_cap = 16;
        usz grow_bytes = 0;
        if (!compiler_checked_mul_usz(CompiledModule.sizeof, new_cap, &grow_bytes)) {
            self.set_compile_error("compiler: compiled module registry allocation size overflow");
            return self.emit_nil_temp();
        }
        CompiledModule* new_mods = g_compiler_force_module_registry_alloc_fail
            ? null
            : (CompiledModule*)mem::malloc(grow_bytes);
        if (new_mods == null) {
            self.set_compile_error("compiler: out of memory while growing compiled module registry");
            return self.emit_nil_temp();
        }
        for (usz i = 0; i < self.compiled_module_count; i++) new_mods[i] = self.compiled_modules[i];
        mem::free(self.compiled_modules);
        self.compiled_modules = new_mods;
        self.compiled_module_capacity = new_cap;
    }

    {
        CompiledModule* mod = &self.compiled_modules[self.compiled_module_count];
        mod.name = expr.module_expr.name;
        usz cap = expr.module_expr.export_count < 16 ? 16 : expr.module_expr.export_count;
        usz export_bytes = 0;
        bool export_size_ok = !g_compiler_force_module_exports_size_overflow
            && compiler_checked_mul_usz(SymbolId.sizeof, cap, &export_bytes);
        if (!export_size_ok) {
            self.set_compile_error("compiler: module export table size overflow");
            return self.emit_nil_temp();
        }
        mod.exports = g_compiler_force_module_exports_alloc_fail
            ? null
            : (SymbolId*)mem::malloc(export_bytes);
        if (mod.exports == null) {
            self.set_compile_error("compiler: out of memory while allocating module export table");
            return self.emit_nil_temp();
        }
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
 * Executes runtime import side effects for non-top-level import forms.
 * For inline-compiled modules with no specifiers, import remains a no-op.
 */
fn usz Compiler.compile_import_flat(Compiler* self, Expr* expr) {
    ExprImport imp = expr.import_expr;
    bool inline_noop = !imp.has_path
        && imp.import_count == 0
        && compiler_has_inline_compiled_module(self, imp.name);

    if (!inline_noop) {
        self.emit_indent();
        self.emit("(void)aot::import_module(\"");
        self.emit_escaped(self.interp.symbols.get_name(imp.name));
        self.emit("\", ");
        if (imp.has_path) {
            self.emit("\"");
            self.emit_escaped(imp.path[:imp.path_len]);
            self.emit("\", true, ");
        } else {
            self.emit("\"\", false, ");
        }
        self.emit(imp.import_all ? "true" : "false");
        self.emit(");\n");

        for (usz ii = 0; ii < imp.import_count; ii++) {
            SymbolId bind_name = (uint)imp.aliases[ii] != 0 ? imp.aliases[ii] : imp.imports[ii];
            self.emit_indent();
            self.emit("aot::define_var(\"");
            self.emit_escaped(self.interp.symbols.get_name(bind_name));
            self.emit("\", aot::lookup_var(\"");
            self.emit_escaped(self.interp.symbols.get_name(imp.imports[ii]));
            self.emit("\"));\n");
        }
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    if (inline_noop) {
        self.emit(" = aot::make_void(); // import: inline module\n");
    } else {
        self.emit(" = aot::make_void();\n");
    }
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
