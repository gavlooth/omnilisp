module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 8b2: TAIL-POSITION COMPILATION (TCO)
// =============================================================================


fn bool Compiler.ensure_inline_compiled_module_metadata(Compiler* self, Expr* expr) {
    if (expr == null || expr.tag != E_MODULE || expr.module_expr == null) return true;
    CompiledModule* existing = self.find_inline_compiled_module(expr.module_expr.name);
    if (existing != null) {
        if (existing.source_expr == expr) return true;
        self.set_compile_error("compiler: duplicate inline module name");
        return false;
    }

    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        Expr* body_expr = expr.module_expr.body[i];
        if (body_expr != null && body_expr.tag == E_MODULE) {
            if (!self.ensure_inline_compiled_module_metadata(body_expr)) return false;
        }
    }

    if (!self.ensure_compiled_module_capacity()) return false;

    usz export_cap = expr.module_expr.export_count < 16 ? 16 : expr.module_expr.export_count;
    usz local_cap = expr.module_expr.body_count + expr.module_expr.export_count;
    if (local_cap < 16) local_cap = 16;
    usz export_bytes = 0;
    usz local_bytes = 0;
    bool size_ok = !g_compiler_force_module_exports_size_overflow
        && compiler_checked_mul_usz(SymbolId.sizeof, export_cap, &export_bytes)
        && compiler_checked_mul_usz(SymbolId.sizeof, local_cap, &local_bytes);
    if (!size_ok) {
        self.set_compile_error("compiler: module export table size overflow");
        return false;
    }

    SymbolId* exports = g_compiler_force_module_exports_alloc_fail
        ? null
        : (SymbolId*)mem::malloc(export_bytes);
    SymbolId* locals = g_compiler_force_module_exports_alloc_fail
        ? null
        : (SymbolId*)mem::malloc(local_bytes);
    SymbolId* backings = g_compiler_force_module_exports_alloc_fail
        ? null
        : (SymbolId*)mem::malloc(local_bytes);
    if (exports == null || locals == null || backings == null) {
        if (exports != null) mem::free(exports);
        if (locals != null) mem::free(locals);
        if (backings != null) mem::free(backings);
        self.set_compile_error("compiler: out of memory while allocating module export table");
        return false;
    }

    CompiledModule* mod = &self.compiled_modules[self.compiled_module_count];
    mod.name = expr.module_expr.name;
    mod.source_expr = expr;
    mod.exports = exports;
    mod.export_capacity = export_cap;
    mod.export_count = expr.module_expr.export_count;
    mod.locals = locals;
    mod.local_backings = backings;
    mod.local_capacity = local_cap;
    mod.local_count = 0;
    mod.loaded = true;

    for (usz i = 0; i < expr.module_expr.export_count; i++) {
        mod.exports[i] = expr.module_expr.exports[i];
    }

    usz module_index = self.compiled_module_count;
    self.compiled_module_count++;
    bool saved_lexical_effect_rejection = self.reject_lexical_defeffect_globals;
    self.reject_lexical_defeffect_globals = false;
    bool locals_ok = true;
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.module_expr.body[i])) {
            locals_ok = false;
            break;
        }
    }
    self.reject_lexical_defeffect_globals = saved_lexical_effect_rejection;
    if (!locals_ok) return false;

    for (usz i = 0; i < mod.export_count; i++) {
        if (compiled_module_find_local_index(mod, mod.exports[i]) < 0) {
            self.set_compile_error("compiler: inline module export has no compiled private backing");
            return false;
        }
    }

    return true;
}

fn bool Compiler.find_inline_module_export_backing(
    Compiler* self,
    SymbolId module_name,
    SymbolId export_name,
    SymbolId* backing_out
) {
    CompiledModule* mod = self.find_inline_compiled_module(module_name);
    if (mod == null || backing_out == null) return false;
    for (usz i = 0; i < mod.export_count; i++) {
        if ((uint)mod.exports[i] == (uint)export_name) {
            isz local_index = compiled_module_find_local_index(mod, export_name);
            if (local_index < 0) return false;
            *backing_out = mod.local_backings[(usz)local_index];
            return true;
        }
    }
    return false;
}

fn bool Compiler.find_inline_module_local_backing(
    Compiler* self,
    SymbolId module_name,
    SymbolId local_name,
    SymbolId* backing_out
) {
    CompiledModule* mod = self.find_inline_compiled_module(module_name);
    if (mod == null || backing_out == null) return false;
    isz local_index = compiled_module_find_local_index(mod, local_name);
    if (local_index < 0) return false;
    *backing_out = mod.local_backings[(usz)local_index];
    return true;
}

fn bool Compiler.find_active_module_private_backing(Compiler* self, SymbolId local_name, SymbolId* backing_out) {
    if (backing_out == null) return false;
    for (usz i = self.module_private_symbols.len(); i > 0; i--) {
        if ((uint)self.module_private_symbols.entries[i - 1] == (uint)local_name) {
            *backing_out = self.module_private_backings.entries[i - 1];
            return true;
        }
    }
    return false;
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
        SymbolId backing = INVALID_SYMBOL_ID;
        if (!self.find_inline_module_export_backing(module_name, export_name, &backing)) {
            self.set_compile_error("compiler: scoped module open could not resolve inline module export backing");
            self.emit("aot::make_nil()");
            return;
        }
        self.emit_symbol_name(backing);
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
    List{SymbolId}* saved_aliases,
    usz saved_lexical_alias_mark,
    usz saved_mutable_cell_mark
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
    self.module_binding_lexical_alias_mark = saved_lexical_alias_mark;
    self.module_binding_mutable_cell_mark = saved_mutable_cell_mark;
}

fn void Compiler.restore_module_private_bindings(
    Compiler* self,
    List{SymbolId}* saved_symbols,
    List{SymbolId}* saved_backings,
    usz saved_lexical_alias_mark,
    usz saved_mutable_cell_mark
) {
    self.module_private_symbols.free();
    self.module_private_backings.free();
    for (usz i = 0; i < saved_symbols.len(); i++) {
        self.module_private_symbols.push(saved_symbols.entries[i]);
    }
    for (usz i = 0; i < saved_backings.len(); i++) {
        self.module_private_backings.push(saved_backings.entries[i]);
    }
    saved_symbols.free();
    saved_backings.free();
    self.module_binding_lexical_alias_mark = saved_lexical_alias_mark;
    self.module_binding_mutable_cell_mark = saved_mutable_cell_mark;
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
    usz saved_lexical_alias_mark = self.module_binding_lexical_alias_mark;
    usz saved_mutable_cell_mark = self.module_binding_mutable_cell_mark;
    for (usz i = 0; i < self.module_open_exports.len(); i++) {
        saved_exports.push(self.module_open_exports.entries[i]);
    }
    for (usz i = 0; i < self.module_open_aliases.len(); i++) {
        saved_aliases.push(self.module_open_aliases.entries[i]);
    }
    defer self.restore_module_open_bindings(&saved_exports, &saved_aliases, saved_lexical_alias_mark, saved_mutable_cell_mark);
    self.module_binding_lexical_alias_mark = self.lexical_alias_symbols.len();
    self.module_binding_mutable_cell_mark = self.mutable_cell_symbols.len();

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
        char[] alias_text = self.generated_name_id(alias_buf[..], "_aot_with_", alias_id);
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
        usz last_r = self.compile_sequence_common(expr.with_module.body, expr.with_module.body_count, tail);
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
    if (!self.ensure_inline_compiled_module_metadata(expr)) return self.emit_nil_temp();
    CompiledModule* mod = self.find_inline_compiled_module(expr.module_expr.name);
    if (mod == null) {
        self.set_compile_error("compiler: inline module metadata missing during lowering");
        return self.emit_nil_temp();
    }

    List{SymbolId} saved_private_symbols;
    List{SymbolId} saved_private_backings;
    usz saved_lexical_alias_mark = self.module_binding_lexical_alias_mark;
    usz saved_mutable_cell_mark = self.module_binding_mutable_cell_mark;
    for (usz i = 0; i < self.module_private_symbols.len(); i++) {
        saved_private_symbols.push(self.module_private_symbols.entries[i]);
    }
    for (usz i = 0; i < self.module_private_backings.len(); i++) {
        saved_private_backings.push(self.module_private_backings.entries[i]);
    }
    defer self.restore_module_private_bindings(&saved_private_symbols, &saved_private_backings, saved_lexical_alias_mark, saved_mutable_cell_mark);
    self.module_binding_lexical_alias_mark = self.lexical_alias_symbols.len();
    self.module_binding_mutable_cell_mark = self.mutable_cell_symbols.len();

    for (usz i = 0; i < mod.local_count; i++) {
        self.module_private_symbols.push(mod.locals[i]);
        self.module_private_backings.push(mod.local_backings[i]);
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("{\n");
    self.indent++;

    usz module_env_id = self.next_result();
    char[64] module_env_buf;
    char[] module_env_name = self.generated_name_id(module_env_buf[..], "_aot_module_env_saved_", (long)module_env_id);
    self.emit_indent();
    self.emit("lisp::Env* ");
    self.emit(module_env_name);
    self.emit(" = aot::push_module_env();\n");
    self.emit_indent();
    self.emit("if (");
    self.emit(module_env_name);
    self.emit(" == null) {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = aot::make_error(\"compiler: failed to allocate inline module environment\");\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    self.emit_indent();
    self.emit("defer aot::pop_module_env(");
    self.emit(module_env_name);
    self.emit(");\n");

    // Compile body expressions inline
    usz last_id = 0;
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        Expr* body_expr = expr.module_expr.body[i];
        if (body_expr.tag == E_DEFINE) {
            // Define: compile init, assign to global
            usz init_r = self.compile_define_rhs_with_typed_bridge(body_expr);
            self.emit_indent();
            SymbolId target = body_expr.define.name;
            if (!self.find_inline_module_local_backing(expr.module_expr.name, body_expr.define.name, &target)) {
                self.set_compile_error("compiler: inline module define missing private backing");
                break;
            }
            self.emit_return_if_sequence_should_stop(init_r);
            self.emit_symbol_name(target);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
            last_id = init_r;
        } else {
            // Non-define: compile inline
            last_id = self.compile_to_temp(body_expr);
            self.emit_return_if_sequence_should_stop(last_id);
        }
    }

    // Module is command-style and evaluates to Void.
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = aot::make_void();\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

/**
 * compile_import_flat — Flat-style import compilation.
 * Executes runtime import side effects for non-top-level import forms.
 * For inline-compiled modules with no specifiers, import remains a no-op.
 */
fn usz Compiler.compile_import_flat(Compiler* self, Expr* expr) {
    ExprImport imp = expr.import_expr;
    CompiledModule* inline_mod = imp.has_path ? null : self.find_inline_compiled_module(imp.name);

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");

    if (inline_mod != null) {
        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = aot::make_void(); // import: inline module\n");

        if (imp.import_all) {
            for (usz ii = 0; ii < inline_mod.export_count; ii++) {
                SymbolId backing = INVALID_SYMBOL_ID;
                if (!self.find_inline_module_export_backing(imp.name, inline_mod.exports[ii], &backing)) {
                    self.set_compile_error("compiler: inline import all could not resolve export backing");
                    break;
                }
                usz define_err_r = self.next_result();
                self.emit_indent();
                self.emit_temp_decl(define_err_r);
                self.emit(" = aot::define_var(\"");
                self.emit_escaped(self.interp.symbols.get_name(inline_mod.exports[ii]));
                self.emit("\", ");
                self.emit_symbol_name(backing);
                self.emit(");\n");
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
                SymbolId private_backing = INVALID_SYMBOL_ID;
                SymbolId bind_name = inline_mod.exports[ii];
                if (self.find_active_module_private_backing(bind_name, &private_backing)) {
                    self.emit_indent();
                    self.emit_symbol_name(private_backing);
                    self.emit(" = aot::lookup_var(\"");
                    self.emit_escaped(self.interp.symbols.get_name(bind_name));
                    self.emit("\");\n");
                }
            }
        }

        for (usz ii = 0; ii < imp.import_count; ii++) {
            SymbolId bind_name = (uint)imp.aliases[ii] != 0 ? imp.aliases[ii] : imp.imports[ii];
            SymbolId backing = INVALID_SYMBOL_ID;
            if (!self.find_inline_module_export_backing(imp.name, imp.imports[ii], &backing)) {
                self.set_compile_error("compiler: inline selective import could not resolve export backing");
                break;
            }
            usz define_err_r = self.next_result();
            self.emit_indent();
            self.emit_temp_decl(define_err_r);
            self.emit(" = aot::define_var(\"");
            self.emit_escaped(self.interp.symbols.get_name(bind_name));
            self.emit("\", ");
            self.emit_symbol_name(backing);
            self.emit(");\n");
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
            SymbolId private_backing = INVALID_SYMBOL_ID;
            if (self.find_active_module_private_backing(bind_name, &private_backing)) {
                self.emit_indent();
                self.emit_symbol_name(private_backing);
                self.emit(" = aot::lookup_var(\"");
                self.emit_escaped(self.interp.symbols.get_name(bind_name));
                self.emit("\");\n");
            }
        }
    } else {
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
            SymbolId private_backing = INVALID_SYMBOL_ID;
            if (self.find_active_module_private_backing(bind_name, &private_backing)) {
                self.emit_indent();
                self.emit_symbol_name(private_backing);
                self.emit(" = aot::lookup_var(\"");
                self.emit_escaped(self.interp.symbols.get_name(bind_name));
                self.emit("\");\n");
            }
        }
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
 * compile_export_from_flat — re-export from inline modules via private backing
 * symbols. Runtime-loaded module re-export is rejected until AOT can preserve
 * interpreter module-export semantics for loaded modules.
 */
fn usz Compiler.compile_export_from_flat(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_void(); // export-from: compiled inline bridge\n");

    CompiledModule* source_mod = self.find_inline_compiled_module(expr.export_from.source_module);
    if (source_mod == null) {
        self.set_compile_error("compiler: export-from runtime modules is not supported in AOT");
        return id;
    }

    if (expr.export_from.all) {
        for (usz i = 0; i < source_mod.export_count; i++) {
            SymbolId target = INVALID_SYMBOL_ID;
            SymbolId source = INVALID_SYMBOL_ID;
            if (!self.find_active_module_private_backing(source_mod.exports[i], &target)) continue;
            if (!self.find_inline_module_export_backing(expr.export_from.source_module, source_mod.exports[i], &source)) {
                self.set_compile_error("compiler: export-from could not resolve inline source backing");
                break;
            }
            self.emit_indent();
            self.emit_symbol_name(target);
            self.emit(" = ");
            self.emit_symbol_name(source);
            self.emit(";\n");
        }
        return id;
    }

    for (usz i = 0; i < expr.export_from.name_count; i++) {
        SymbolId name = expr.export_from.names[i];
        SymbolId target = INVALID_SYMBOL_ID;
        SymbolId source = INVALID_SYMBOL_ID;
        if (!self.find_active_module_private_backing(name, &target)) continue;
        if (!self.find_inline_module_export_backing(expr.export_from.source_module, name, &source)) {
            self.set_compile_error("compiler: export-from could not resolve inline source backing");
            break;
        }
        self.emit_indent();
        self.emit_symbol_name(target);
        self.emit(" = ");
        self.emit_symbol_name(source);
        self.emit(";\n");
    }
    return id;
}
