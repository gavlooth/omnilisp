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

fn SymbolId compiler_module_direct_binding_name(Expr* expr) {
    if (expr == null) return INVALID_SYMBOL_ID;
    switch (expr.tag) {
        case E_DEFINE:
            return expr.define.name;
        case E_FFI_LIB:
            return expr.ffi_lib.name;
        case E_FFI_FN:
            if (expr.ffi_fn != null) return expr.ffi_fn.fn_name;
            return INVALID_SYMBOL_ID;
        case E_DEFEFFECT:
            return expr.defeffect.name;
        default:
            return INVALID_SYMBOL_ID;
    }
}

fn isz compiled_module_find_local_index(CompiledModule* mod, SymbolId sym) {
    if (mod == null) return -1;
    for (usz i = 0; i < mod.local_count; i++) {
        if ((uint)mod.locals[i] == (uint)sym) return (isz)i;
    }
    return -1;
}

fn bool Compiler.add_compiled_module_local(
    Compiler* self,
    CompiledModule* mod,
    usz module_index,
    SymbolId local_name
) {
    if (local_name == INVALID_SYMBOL_ID) return true;
    if (compiled_module_find_local_index(mod, local_name) >= 0) return true;
    if (mod.local_count >= mod.local_capacity) {
        if (mod.local_capacity > usz.max / 2) {
            self.set_compile_error("compiler: module private binding table size overflow");
            return false;
        }
        usz new_cap = mod.local_capacity * 2;
        if (new_cap < 16) new_cap = 16;
        usz new_bytes = 0;
        if (!compiler_checked_mul_usz(SymbolId.sizeof, new_cap, &new_bytes)) {
            self.set_compile_error("compiler: module private binding table size overflow");
            return false;
        }
        SymbolId* new_locals = g_compiler_force_module_exports_alloc_fail
            ? null
            : (SymbolId*)mem::malloc(new_bytes);
        SymbolId* new_backings = g_compiler_force_module_exports_alloc_fail
            ? null
            : (SymbolId*)mem::malloc(new_bytes);
        if (new_locals == null || new_backings == null) {
            if (new_locals != null) mem::free(new_locals);
            if (new_backings != null) mem::free(new_backings);
            self.set_compile_error("compiler: out of memory while growing module private binding table");
            return false;
        }
        for (usz i = 0; i < mod.local_count; i++) {
            new_locals[i] = mod.locals[i];
            new_backings[i] = mod.local_backings[i];
        }
        mem::free(mod.locals);
        mem::free(mod.local_backings);
        mod.locals = new_locals;
        mod.local_backings = new_backings;
        mod.local_capacity = new_cap;
    }

    char[96] backing_buf;
    char[] backing_text = compiler_format_generated_name_pair(backing_buf[..], "_aot_mod_", (long)module_index, (long)mod.local_count);
    SymbolId backing_sym = self.interp.symbols.intern(backing_text);
    if (backing_sym == INVALID_SYMBOL_ID) {
        self.set_compile_error("compiler: invalid module private backing symbol");
        return false;
    }

    mod.locals[mod.local_count] = local_name;
    mod.local_backings[mod.local_count] = backing_sym;
    mod.local_count++;
    return true;
}

fn bool Compiler.add_compiled_module_locals_from_expr(
    Compiler* self,
    CompiledModule* mod,
    usz module_index,
    Expr* expr
) {
    if (expr == null) return true;
    SymbolId local_name = compiler_module_direct_binding_name(expr);
    if (local_name != INVALID_SYMBOL_ID && !self.add_compiled_module_local(mod, module_index, local_name)) {
        return false;
    }

    switch (expr.tag) {
        case E_DEFINE:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.define.value);
        case E_LAMBDA:
            if (expr.lambda != null) return self.add_compiled_module_locals_from_expr(mod, module_index, expr.lambda.body);
        case E_APP:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.app.func)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.app.arg);
        case E_IF:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.if_expr.test)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.if_expr.then_branch)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.if_expr.else_branch);
        case E_LET:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.let_expr.init)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.let_expr.body);
        case E_RESET:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.reset.body);
        case E_SHIFT:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.shift.body);
        case E_PERFORM:
            if (expr.perform.has_tag_expr &&
                !self.add_compiled_module_locals_from_expr(mod, module_index, expr.perform.tag_expr)) {
                return false;
            }
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.perform.arg);
        case E_RESOLVE:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.resolve.value);
        case E_HANDLE:
            if (expr.handle != null) {
                if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.handle.body)) return false;
                for (usz hi = 0; hi < expr.handle.clause_count; hi++) {
                    if (expr.handle.clauses[hi].has_effect_tag_expr &&
                        !self.add_compiled_module_locals_from_expr(mod, module_index, expr.handle.clauses[hi].effect_tag_expr)) {
                        return false;
                    }
                    if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.handle.clauses[hi].handler_body)) return false;
                }
            }
        case E_INDEX:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.index.collection)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.index.index);
        case E_MATCH:
            if (expr.match != null) {
                if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.match.scrutinee)) return false;
                for (usz mi = 0; mi < expr.match.clause_count; mi++) {
                    if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.match.clauses[mi].result)) return false;
                }
            }
        case E_AND:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.and_expr.left)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.and_expr.right);
        case E_OR:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.or_expr.left)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.or_expr.right);
        case E_CALL:
            if (expr.call != null) {
                if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.call.func)) return false;
                for (usz ai = 0; ai < expr.call.arg_count; ai++) {
                    if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.call.args[ai])) return false;
                }
            }
        case E_BLOCK:
            if (expr.block != null) {
                for (usz bi = 0; bi < expr.block.expr_count; bi++) {
                    if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.block.exprs[bi])) return false;
                }
            }
        case E_SET:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.set_expr.value);
        case E_QUASIQUOTE:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.quasiquote.body);
        case E_UNQUOTE:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.unquote.body);
        case E_UNQUOTE_SPLICING:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.unquote_splicing.body);
        case E_EXPORT_FROM:
            if (expr.export_from.all) {
                CompiledModule* source_mod = self.find_inline_compiled_module(expr.export_from.source_module);
                if (source_mod == null) return true;
                for (usz si = 0; si < source_mod.export_count; si++) {
                    if (!self.add_compiled_module_local(mod, module_index, source_mod.exports[si])) return false;
                }
            } else {
                for (usz ni = 0; ni < expr.export_from.name_count; ni++) {
                    if (!self.add_compiled_module_local(mod, module_index, expr.export_from.names[ni])) return false;
                }
            }
        default:
    }
    return !self.has_error;
}

fn bool Compiler.ensure_compiled_module_capacity(Compiler* self) {
    if (self.compiled_modules == null || self.compiled_module_capacity == 0) {
        usz init_cap = 16;
        usz init_bytes = 0;
        bool init_size_ok = compiler_checked_mul_usz(CompiledModule.sizeof, init_cap, &init_bytes);
        if (!init_size_ok) {
            self.set_compile_error("compiler: compiled module registry allocation size overflow");
            return false;
        }
        CompiledModule* init_mods = g_compiler_force_module_registry_alloc_fail
            ? null
            : (CompiledModule*)mem::malloc(init_bytes);
        if (init_mods == null) {
            self.set_compile_error("compiler: out of memory while allocating compiled module registry");
            return false;
        }
        self.compiled_modules = init_mods;
        self.compiled_module_capacity = init_cap;
        self.compiled_module_count = 0;
    }

    if (self.compiled_module_count < self.compiled_module_capacity) return true;

    if (g_compiler_force_module_registry_growth_overflow
        || self.compiled_module_capacity > usz.max / 2) {
        self.set_compile_error("compiler: compiled module registry capacity overflow");
        return false;
    }
    usz new_cap = self.compiled_module_capacity * 2;
    if (new_cap < 16) new_cap = 16;
    usz grow_bytes = 0;
    if (!compiler_checked_mul_usz(CompiledModule.sizeof, new_cap, &grow_bytes)) {
        self.set_compile_error("compiler: compiled module registry allocation size overflow");
        return false;
    }
    CompiledModule* new_mods = g_compiler_force_module_registry_alloc_fail
        ? null
        : (CompiledModule*)mem::malloc(grow_bytes);
    if (new_mods == null) {
        self.set_compile_error("compiler: out of memory while growing compiled module registry");
        return false;
    }
    for (usz i = 0; i < self.compiled_module_count; i++) new_mods[i] = self.compiled_modules[i];
    mem::free(self.compiled_modules);
    self.compiled_modules = new_mods;
    self.compiled_module_capacity = new_cap;
    return true;
}

fn bool Compiler.ensure_inline_compiled_module_metadata(Compiler* self, Expr* expr) {
    if (expr == null || expr.tag != E_MODULE || expr.module_expr == null) return true;
    if (self.find_inline_compiled_module(expr.module_expr.name) != null) return true;

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
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.module_expr.body[i])) return false;
    }

    for (usz i = 0; i < mod.export_count; i++) {
        if (compiled_module_find_local_index(mod, mod.exports[i]) < 0) {
            self.set_compile_error("compiler: inline module export has no compiled private backing");
            return false;
        }
    }

    self.compiled_module_count++;
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

fn void Compiler.restore_module_private_bindings(
    Compiler* self,
    List{SymbolId}* saved_symbols,
    List{SymbolId}* saved_backings
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
    for (usz i = 0; i < self.module_private_symbols.len(); i++) {
        saved_private_symbols.push(self.module_private_symbols.entries[i]);
    }
    for (usz i = 0; i < self.module_private_backings.len(); i++) {
        saved_private_backings.push(self.module_private_backings.entries[i]);
    }
    defer self.restore_module_private_bindings(&saved_private_symbols, &saved_private_backings);

    for (usz i = 0; i < mod.local_count; i++) {
        self.module_private_symbols.push(mod.locals[i]);
        self.module_private_backings.push(mod.local_backings[i]);
    }

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
                return self.emit_nil_temp();
            }
            self.emit_symbol_name(target);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
            last_id = init_r;
        } else if (body_expr.tag == E_FFI_LIB) {
            usz init_r = self.compile_to_temp(body_expr);
            self.emit_indent();
            SymbolId target = body_expr.ffi_lib.name;
            if (!self.find_inline_module_local_backing(expr.module_expr.name, body_expr.ffi_lib.name, &target)) {
                self.set_compile_error("compiler: inline module ffi lib missing private backing");
                return self.emit_nil_temp();
            }
            self.emit_symbol_name(target);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
            last_id = init_r;
        } else if (body_expr.tag == E_FFI_FN && body_expr.ffi_fn != null) {
            usz init_r = self.compile_to_temp(body_expr);
            self.emit_indent();
            SymbolId target = body_expr.ffi_fn.fn_name;
            if (!self.find_inline_module_local_backing(expr.module_expr.name, body_expr.ffi_fn.fn_name, &target)) {
                self.set_compile_error("compiler: inline module ffi function missing private backing");
                return self.emit_nil_temp();
            }
            self.emit_symbol_name(target);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
            last_id = init_r;
        } else if (body_expr.tag == E_DEFEFFECT) {
            usz init_r = self.compile_to_temp(body_expr);
            self.emit_indent();
            SymbolId target = body_expr.defeffect.name;
            if (!self.find_inline_module_local_backing(expr.module_expr.name, body_expr.defeffect.name, &target)) {
                self.set_compile_error("compiler: inline module effect missing private backing");
                return self.emit_nil_temp();
            }
            self.emit_symbol_name(target);
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
 * symbols. Runtime-loaded modules still use the existing bridge/no-op behavior.
 */
fn usz Compiler.compile_export_from_flat(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_void(); // export-from: compiled inline bridge\n");

    CompiledModule* source_mod = self.find_inline_compiled_module(expr.export_from.source_module);
    if (source_mod == null) return id;

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
