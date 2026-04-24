module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 8b2: TAIL-POSITION COMPILATION (TCO)
// =============================================================================

fn CompiledModule* Compiler.find_inline_compiled_module(Compiler* self, SymbolId module_name) {
    for (usz i = 0; i < self.compiled_module_count; i++) {
        if (self.compiled_modules[i].name == module_name) {
            return &self.compiled_modules[i];
        }
    }
    return null;
}

fn bool Compiler.resolve_with_module_exports(
    Compiler* self,
    Expr* expr,
    SymbolId** exports_out,
    usz* export_count_out,
    bool* inline_compiled_out
) {
    if (expr == null || expr.with_module == null || exports_out == null || export_count_out == null || inline_compiled_out == null) {
        self.set_compile_error("compiler: scoped module open is malformed");
        return false;
    }

    SymbolId module_name = expr.with_module.import_expr.name;
    CompiledModule* inline_mod = self.find_inline_compiled_module(module_name);
    if (inline_mod != null) {
        *exports_out = inline_mod.exports;
        *export_count_out = inline_mod.export_count;
        *inline_compiled_out = true;
        return true;
    }

    Module* loaded_mod = null;
    if (expr.with_module.import_expr.has_path && expr.with_module.import_expr.path != null) {
        loaded_mod = find_module_by_path_any(expr.with_module.import_expr.path[:expr.with_module.import_expr.path_len], self.interp);
    } else {
        loaded_mod = find_module(module_name, self.interp);
    }
    if (loaded_mod != null && loaded_mod.loaded && loaded_mod.exports != null) {
        *exports_out = loaded_mod.exports;
        *export_count_out = loaded_mod.export_count;
        *inline_compiled_out = false;
        return true;
    }

    self.set_compile_error("compiler: scoped module open requires a known loaded module or inline-compiled module");
    return false;
}

fn void Compiler.emit_with_module_open_binding_source(Compiler* self, SymbolId module_name, SymbolId export_name, bool inline_compiled) {
    if (inline_compiled) {
        self.emit_symbol_name(export_name);
        return;
    }

    self.emit("aot::lookup_module_export(\"");
    self.emit_escaped(self.interp.symbols.get_name(module_name));
    self.emit("\", \"");
    self.emit_escaped(self.interp.symbols.get_name(export_name));
    self.emit("\")");
}

fn void Compiler.restore_module_open_bindings(
    Compiler* self,
    List{SymbolId}* saved_exports,
    List{SymbolId}* saved_aliases
) {
    self.module_open_exports.free();
    self.module_open_aliases.free();
    for (usz i = 0; i < saved_exports.len(); i++) {
        self.module_open_exports.push(saved_exports.entries[i]);
    }
    for (usz i = 0; i < saved_aliases.len(); i++) {
        self.module_open_aliases.push(saved_aliases.entries[i]);
    }
    saved_exports.free();
    saved_aliases.free();
}

fn usz Compiler.compile_with_module_open_flat_common(Compiler* self, Expr* expr, bool tail) {
    if (self.has_error) return self.emit_nil_temp();

    SymbolId* exports = null;
    usz export_count = 0;
    bool inline_compiled = false;
    if (!self.resolve_with_module_exports(expr, &exports, &export_count, &inline_compiled)) {
        return self.emit_nil_temp();
    }

    List{SymbolId} saved_exports;
    List{SymbolId} saved_aliases;
    for (usz i = 0; i < self.module_open_exports.len(); i++) {
        saved_exports.push(self.module_open_exports.entries[i]);
    }
    for (usz i = 0; i < self.module_open_aliases.len(); i++) {
        saved_aliases.push(self.module_open_aliases.entries[i]);
    }
    defer self.restore_module_open_bindings(&saved_exports, &saved_aliases);

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("{\n");
    self.indent++;

    for (usz i = 0; i < export_count; i++) {
        SymbolId export_name = exports[i];
        char[64] alias_buf;
        long alias_id = (long)self.temp_counter + (long)i;
        char[] alias_text = compiler_format_generated_name_id(alias_buf[..], "_aot_with_", alias_id);
        SymbolId alias_sym = self.interp.symbols.intern(alias_text);
        if (alias_sym == INVALID_SYMBOL_ID) {
            self.set_compile_error("compiler: invalid scoped module alias");
            break;
        }

        self.module_open_exports.push(export_name);
        self.module_open_aliases.push(alias_sym);

        self.emit_indent();
        self.emit("lisp::Value* ");
        self.emit_symbol_name(alias_sym);
        self.emit(" = ");
        self.emit_with_module_open_binding_source(expr.with_module.import_expr.name, export_name, inline_compiled);
        self.emit(";\n");
    }

    if (!self.has_error) {
        usz last_r = 0;
        if (expr.with_module.body_count > 0) {
            for (usz i = 0; i + 1 < expr.with_module.body_count; i++) {
                self.compile_to_temp(expr.with_module.body[i]);
            }
            if (tail) {
                last_r = self.compile_to_temp_tail(expr.with_module.body[expr.with_module.body_count - 1]);
            } else {
                last_r = self.compile_to_temp(expr.with_module.body[expr.with_module.body_count - 1]);
            }
        } else {
            last_r = self.emit_nil_temp();
        }
        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = ");
        self.emit_temp_ref(last_r);
        self.emit(";\n");
    }

    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

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
        } else if (body_expr.tag == E_FFI_LIB) {
            usz init_r = self.compile_to_temp(body_expr);
            self.emit_indent();
            self.emit_symbol_name(body_expr.ffi_lib.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
            last_id = init_r;
        } else if (body_expr.tag == E_FFI_FN && body_expr.ffi_fn != null) {
            usz init_r = self.compile_to_temp(body_expr);
            self.emit_indent();
            self.emit_symbol_name(body_expr.ffi_fn.fn_name);
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

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");

    if (!inline_noop) {
        usz import_err_r = self.next_result();
        self.emit_indent();
        self.emit_temp_decl(import_err_r);
        self.emit(" = aot::import_module(\"");
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
        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = (");
        self.emit_temp_ref(import_err_r);
        self.emit(" != null && ");
        self.emit_temp_ref(import_err_r);
        self.emit(".tag == lisp::ValueTag.ERROR) ? ");
        self.emit_temp_ref(import_err_r);
        self.emit(" : aot::make_void();\n");

        for (usz ii = 0; ii < imp.import_count; ii++) {
            SymbolId bind_name = (uint)imp.aliases[ii] != 0 ? imp.aliases[ii] : imp.imports[ii];
            usz define_err_r = self.next_result();
            self.emit_indent();
            self.emit_temp_decl(define_err_r);
            self.emit(" = aot::define_var(\"");
            self.emit_escaped(self.interp.symbols.get_name(bind_name));
            self.emit("\", aot::lookup_var(\"");
            self.emit_escaped(self.interp.symbols.get_name(imp.imports[ii]));
            self.emit("\"));\n");
            self.emit_indent();
            self.emit("if (");
            self.emit_temp_ref(id);
            self.emit(" == null || ");
            self.emit_temp_ref(id);
            self.emit(".tag != lisp::ValueTag.ERROR) ");
            self.emit_temp_ref(id);
            self.emit(" = ");
            self.emit_temp_ref(define_err_r);
            self.emit(" != null ? ");
            self.emit_temp_ref(define_err_r);
            self.emit(" : aot::make_void();\n");
        }
    } else {
        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = aot::make_void(); // import: inline module\n");
    }

    return id;
}

fn usz Compiler.compile_with_module_open_flat(Compiler* self, Expr* expr) {
    return self.compile_with_module_open_flat_common(expr, false);
}

fn usz Compiler.compile_with_module_open_flat_tail(Compiler* self, Expr* expr) {
    return self.compile_with_module_open_flat_common(expr, true);
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
