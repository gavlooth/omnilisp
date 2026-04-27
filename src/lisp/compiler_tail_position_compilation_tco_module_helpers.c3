module lisp;

import std::io;
import std::collections::list;
import main;

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
        case E_DEFTYPE:
            if (expr.deftype != null) return expr.deftype.name;
            return INVALID_SYMBOL_ID;
        case E_DEFABSTRACT:
            return expr.defabstract.name;
        case E_DEFUNION:
            if (expr.defunion != null) return expr.defunion.name;
            return INVALID_SYMBOL_ID;
        case E_DEFALIAS:
            return expr.defalias.name;
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

fn isz compiled_module_find_export_index(CompiledModule* mod, SymbolId sym) {
    if (mod == null) return -1;
    for (usz i = 0; i < mod.export_count; i++) {
        if ((uint)mod.exports[i] == (uint)sym) return (isz)i;
    }
    return -1;
}

fn bool Compiler.add_compiled_module_export(Compiler* self, CompiledModule* mod, SymbolId export_name) {
    if (export_name == INVALID_SYMBOL_ID) return true;
    if (compiled_module_find_export_index(mod, export_name) >= 0) return true;
    if (mod.export_count >= mod.export_capacity) {
        if (mod.export_capacity > usz.max / 2) {
            self.set_compile_error("compiler: module export table size overflow");
            return false;
        }
        usz new_cap = mod.export_capacity * 2;
        if (new_cap < 16) new_cap = 16;
        usz new_bytes = 0;
        if (!compiler_checked_mul_usz(SymbolId.sizeof, new_cap, &new_bytes)) {
            self.set_compile_error("compiler: module export table size overflow");
            return false;
        }
        SymbolId* new_exports = g_compiler_force_module_exports_alloc_fail
            ? null
            : (SymbolId*)mem::malloc(new_bytes);
        if (new_exports == null) {
            self.set_compile_error("compiler: out of memory while growing module export table");
            return false;
        }
        for (usz i = 0; i < mod.export_count; i++) {
            new_exports[i] = mod.exports[i];
        }
        mem::free(mod.exports);
        mod.exports = new_exports;
        mod.export_capacity = new_cap;
    }
    mod.exports[mod.export_count] = export_name;
    mod.export_count++;
    return true;
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
    char[] backing_text = self.generated_name_pair(backing_buf[..], "_aot_mod_", (long)module_index, (long)mod.local_count);
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

fn bool compiled_module_symbol_is_export(CompiledModule* mod, SymbolId sym) {
    return compiled_module_find_export_index(mod, sym) >= 0;
}

fn bool Compiler.compiled_module_pattern_binds_export(Compiler* self, CompiledModule* mod, Pattern* pat) {
    if (mod == null || pat == null) return false;
    switch (pat.tag) {
        case PAT_WILDCARD:
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_DICT:
            return false;
        case PAT_CONS:
            return self.compiled_module_pattern_binds_export(mod, pat.car_pat)
                || self.compiled_module_pattern_binds_export(mod, pat.cdr_pat);
        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                if (self.compiled_module_pattern_binds_export(mod, pat.elements[i])) return true;
            }
        case PAT_AS:
            return self.compiled_module_pattern_binds_export(mod, pat.as_sub);
        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                if (self.compiled_module_pattern_binds_export(mod, pat.ctor_sub_patterns[i])) return true;
            }
        case PAT_GUARD:
            return self.compiled_module_pattern_binds_export(mod, pat.guard_sub)
                || self.compiled_module_expr_binds_export(mod, pat.guard_pred);
        default:
            self.set_compile_error("compiler: unknown pattern tag in inline module export classification");
            return false;
    }
    return false;
}

fn bool Compiler.compiled_module_expr_binds_export(Compiler* self, CompiledModule* mod, Expr* expr) {
    if (mod == null || expr == null) return false;
    SymbolId local_name = compiler_module_direct_binding_name(expr);
    if (local_name != INVALID_SYMBOL_ID && compiled_module_symbol_is_export(mod, local_name)) return true;
    switch (expr.tag) {
        case E_LIT:
        case E_VAR:
        case E_LAMBDA:
        case E_QUOTE:
        case E_PATH:
        case E_DEFMACRO:
        case E_DEFTYPE:
        case E_DEFABSTRACT:
        case E_DEFALIAS:
        case E_DEFEFFECT:
        case E_FFI_LIB:
        case E_FFI_FN:
            return false;
        case E_DEFINE:
            return self.compiled_module_expr_binds_export(mod, expr.define.value);
        case E_APP:
            return self.compiled_module_expr_binds_export(mod, expr.app.func)
                || self.compiled_module_expr_binds_export(mod, expr.app.arg);
        case E_IF:
            return self.compiled_module_expr_binds_export(mod, expr.if_expr.test)
                || self.compiled_module_expr_binds_export(mod, expr.if_expr.then_branch)
                || self.compiled_module_expr_binds_export(mod, expr.if_expr.else_branch);
        case E_LET:
            return self.compiled_module_expr_binds_export(mod, expr.let_expr.init)
                || self.compiled_module_expr_binds_export(mod, expr.let_expr.body);
        case E_RESET:
            return self.compiled_module_expr_binds_export(mod, expr.reset.body);
        case E_SHIFT:
            return self.compiled_module_expr_binds_export(mod, expr.shift.body);
        case E_PERFORM:
            return (expr.perform.has_tag_expr && self.compiled_module_expr_binds_export(mod, expr.perform.tag_expr))
                || self.compiled_module_expr_binds_export(mod, expr.perform.arg);
        case E_RESOLVE:
            return self.compiled_module_expr_binds_export(mod, expr.resolve.value);
        case E_HANDLE:
            if (expr.handle != null) {
                if (self.compiled_module_expr_binds_export(mod, expr.handle.body)) return true;
                for (usz hi = 0; hi < expr.handle.clause_count; hi++) {
                    if (expr.handle.clauses[hi].has_effect_tag_expr &&
                        self.compiled_module_expr_binds_export(mod, expr.handle.clauses[hi].effect_tag_expr)) return true;
                    if (self.compiled_module_expr_binds_export(mod, expr.handle.clauses[hi].handler_body)) return true;
                }
            }
        case E_INDEX:
            return self.compiled_module_expr_binds_export(mod, expr.index.collection)
                || self.compiled_module_expr_binds_export(mod, expr.index.index);
        case E_MATCH:
            if (expr.match != null) {
                if (self.compiled_module_expr_binds_export(mod, expr.match.scrutinee)) return true;
                for (usz mi = 0; mi < expr.match.clause_count; mi++) {
                    if (self.compiled_module_pattern_binds_export(mod, expr.match.clauses[mi].pattern)) return true;
                    if (self.compiled_module_expr_binds_export(mod, expr.match.clauses[mi].result)) return true;
                }
            }
        case E_AND:
            return self.compiled_module_expr_binds_export(mod, expr.and_expr.left)
                || self.compiled_module_expr_binds_export(mod, expr.and_expr.right);
        case E_OR:
            return self.compiled_module_expr_binds_export(mod, expr.or_expr.left)
                || self.compiled_module_expr_binds_export(mod, expr.or_expr.right);
        case E_CALL:
            if (expr.call != null) {
                if (self.compiled_module_expr_binds_export(mod, expr.call.func)) return true;
                for (usz ai = 0; ai < expr.call.arg_count; ai++) {
                    if (self.compiled_module_expr_binds_export(mod, expr.call.args[ai])) return true;
                }
            }
        case E_BLOCK:
            if (expr.block != null) {
                for (usz bi = 0; bi < expr.block.expr_count; bi++) {
                    if (self.compiled_module_expr_binds_export(mod, expr.block.exprs[bi])) return true;
                }
            }
        case E_SET:
            return self.compiled_module_expr_binds_export(mod, expr.set_expr.value);
        case E_QUASIQUOTE:
            return self.compiled_module_expr_binds_export(mod, expr.quasiquote.body);
        case E_UNQUOTE:
            return self.compiled_module_expr_binds_export(mod, expr.unquote.body);
        case E_UNQUOTE_SPLICING:
            return self.compiled_module_expr_binds_export(mod, expr.unquote_splicing.body);
        case E_DEFUNION:
            if (expr.defunion != null) {
                for (usz vi = 0; vi < expr.defunion.variant_count; vi++) {
                    if (compiled_module_symbol_is_export(mod, expr.defunion.variants[vi].name)) return true;
                }
            }
        case E_EXPORT_FROM:
            if (expr.export_from.all) return true;
            for (usz ni = 0; ni < expr.export_from.name_count; ni++) {
                if (compiled_module_symbol_is_export(mod, expr.export_from.names[ni])) return true;
            }
        case E_IMPORT:
            if (expr.import_expr.import_all) return true;
            for (usz ii = 0; ii < expr.import_expr.import_count; ii++) {
                SymbolId bind_name = (uint)expr.import_expr.aliases[ii] != 0
                    ? expr.import_expr.aliases[ii]
                    : expr.import_expr.imports[ii];
                if (compiled_module_symbol_is_export(mod, bind_name)) return true;
            }
        case E_WITH_MODULE:
            if (expr.with_module != null) {
                for (usz wi = 0; wi < expr.with_module.body_count; wi++) {
                    if (self.compiled_module_expr_binds_export(mod, expr.with_module.body[wi])) return true;
                }
            }
        default:
            self.set_compile_error("compiler: unknown expression tag in inline module export classification");
            return false;
    }
    return false;
}

fn bool Compiler.add_compiled_module_locals_from_expr(
    Compiler* self,
    CompiledModule* mod,
    usz module_index,
    Expr* expr
) {
    if (expr == null) return true;
    if (expr.tag == E_DEFEFFECT && self.reject_lexical_defeffect_globals) {
        self.set_compile_error("compiler: lexical effect declarations are not supported in AOT");
        return false;
    }
    SymbolId local_name = compiler_module_direct_binding_name(expr);
    if (local_name != INVALID_SYMBOL_ID && !self.add_compiled_module_local(mod, module_index, local_name)) {
        return false;
    }

    switch (expr.tag) {
        case E_LIT:
        case E_VAR:
        case E_QUOTE:
        case E_PATH:
        case E_DEFMACRO:
        case E_DEFTYPE:
        case E_DEFABSTRACT:
        case E_DEFALIAS:
        case E_DEFEFFECT:
        case E_FFI_LIB:
        case E_FFI_FN:
            return true;
        case E_DEFINE:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.define.value);
        case E_LAMBDA:
            return true;
        case E_APP:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.app.func)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.app.arg);
        case E_IF:
            if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.if_expr.test)) return false;
            if (compiler_expr_is_literal_false(self, expr.if_expr.test)) {
                return self.add_compiled_module_locals_from_expr(mod, module_index, expr.if_expr.else_branch);
            }
            if (compiler_expr_is_literal_truthy(self, expr.if_expr.test)) {
                return self.add_compiled_module_locals_from_expr(mod, module_index, expr.if_expr.then_branch);
            }
            if (self.compiled_module_expr_binds_export(mod, expr.if_expr.then_branch) ||
                self.compiled_module_expr_binds_export(mod, expr.if_expr.else_branch)) {
                self.set_compile_error("compiler: conditional inline module exports are not definitely assigned in AOT");
                return false;
            }
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.if_expr.then_branch)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.if_expr.else_branch);
        case E_LET:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.let_expr.init)
                && self.add_compiled_module_locals_from_expr_lexical(mod, module_index, expr.let_expr.body);
        case E_RESET:
            if (self.compiled_module_expr_binds_export(mod, expr.reset.body)) {
                self.set_compile_error("compiler: checkpoint inline module exports are not supported in AOT");
                return false;
            }
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.reset.body);
        case E_SHIFT:
            return self.add_compiled_module_locals_from_expr_lexical(mod, module_index, expr.shift.body);
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
                if (self.compiled_module_expr_binds_export(mod, expr.handle.body)) {
                    self.set_compile_error("compiler: handle inline module exports are not supported in AOT");
                    return false;
                }
                if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.handle.body)) return false;
                for (usz hi = 0; hi < expr.handle.clause_count; hi++) {
                    if (expr.handle.clauses[hi].has_effect_tag_expr &&
                        self.compiled_module_expr_binds_export(mod, expr.handle.clauses[hi].effect_tag_expr)) {
                        self.set_compile_error("compiler: handle inline module exports are not supported in AOT");
                        return false;
                    }
                    if (self.compiled_module_expr_binds_export(mod, expr.handle.clauses[hi].handler_body)) {
                        self.set_compile_error("compiler: handle inline module exports are not supported in AOT");
                        return false;
                    }
                    if (expr.handle.clauses[hi].has_effect_tag_expr &&
                        !self.add_compiled_module_locals_from_expr(mod, module_index, expr.handle.clauses[hi].effect_tag_expr)) {
                        return false;
                    }
                    if (!self.add_compiled_module_locals_from_expr_lexical(mod, module_index, expr.handle.clauses[hi].handler_body)) return false;
                }
            }
        case E_INDEX:
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.index.collection)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.index.index);
        case E_MATCH:
            if (expr.match != null) {
                if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.match.scrutinee)) return false;
                for (usz mi = 0; mi < expr.match.clause_count; mi++) {
                    if (self.compiled_module_pattern_binds_export(mod, expr.match.clauses[mi].pattern) ||
                        self.compiled_module_expr_binds_export(mod, expr.match.clauses[mi].result)) {
                        self.set_compile_error("compiler: match inline module exports are not definitely assigned in AOT");
                        return false;
                    }
                    if (!self.add_compiled_module_locals_from_pattern(mod, module_index, expr.match.clauses[mi].pattern)) return false;
                    if (!self.add_compiled_module_locals_from_expr_lexical(mod, module_index, expr.match.clauses[mi].result)) return false;
                }
            }
        case E_AND:
            if (self.compiled_module_expr_binds_export(mod, expr.and_expr.right) &&
                !compiler_expr_is_literal_truthy(self, expr.and_expr.left)) {
                self.set_compile_error("compiler: short-circuit inline module exports are not definitely assigned in AOT");
                return false;
            }
            return self.add_compiled_module_locals_from_expr(mod, module_index, expr.and_expr.left)
                && self.add_compiled_module_locals_from_expr(mod, module_index, expr.and_expr.right);
        case E_OR:
            if (self.compiled_module_expr_binds_export(mod, expr.or_expr.right) &&
                !compiler_expr_is_literal_false(self, expr.or_expr.left)) {
                self.set_compile_error("compiler: short-circuit inline module exports are not definitely assigned in AOT");
                return false;
            }
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
        case E_DEFUNION:
            if (expr.defunion != null) {
                for (usz vi = 0; vi < expr.defunion.variant_count; vi++) {
                    if (!self.add_compiled_module_local(mod, module_index, expr.defunion.variants[vi].name)) return false;
                }
            }
        case E_MODULE:
            return self.ensure_inline_compiled_module_metadata(expr);
        case E_EXPORT_FROM:
            if (expr.export_from.all) {
                CompiledModule* source_mod = self.find_inline_compiled_module(expr.export_from.source_module);
                if (source_mod == null) {
                    self.set_compile_error("compiler: export-from runtime modules is not supported in AOT");
                    return false;
                }
                for (usz si = 0; si < source_mod.export_count; si++) {
                    if (!self.add_compiled_module_local(mod, module_index, source_mod.exports[si])) return false;
                    if (!self.add_compiled_module_export(mod, source_mod.exports[si])) return false;
                }
            } else {
                CompiledModule* source_mod = self.find_inline_compiled_module(expr.export_from.source_module);
                if (source_mod == null) {
                    self.set_compile_error("compiler: export-from runtime modules is not supported in AOT");
                    return false;
                }
                for (usz ni = 0; ni < expr.export_from.name_count; ni++) {
                    if (compiled_module_find_export_index(source_mod, expr.export_from.names[ni]) < 0) {
                        self.set_compile_error("compiler: export-from could not resolve inline source export");
                        return false;
                    }
                    if (!self.add_compiled_module_local(mod, module_index, expr.export_from.names[ni])) return false;
                    if (!self.add_compiled_module_export(mod, expr.export_from.names[ni])) return false;
                }
            }
        case E_WITH_MODULE:
            if (expr.with_module != null) {
                for (usz wi = 0; wi < expr.with_module.body_count; wi++) {
                    if (!self.add_compiled_module_locals_from_expr(mod, module_index, expr.with_module.body[wi])) return false;
                }
            }
        case E_IMPORT:
            if (expr.import_expr.import_all) {
                if (expr.import_expr.has_path) {
                    self.set_compile_error("compiler: module-body import-all from runtime modules is not supported in AOT");
                    return false;
                }
                CompiledModule* source_mod = self.find_inline_compiled_module(expr.import_expr.name);
                if (source_mod == null) {
                    self.set_compile_error("compiler: module-body import-all from runtime modules is not supported in AOT");
                    return false;
                }
                for (usz si = 0; si < source_mod.export_count; si++) {
                    if (!self.add_compiled_module_local(mod, module_index, source_mod.exports[si])) return false;
                }
                return true;
            }
            for (usz ii = 0; ii < expr.import_expr.import_count; ii++) {
                SymbolId bind_name = (uint)expr.import_expr.aliases[ii] != 0
                    ? expr.import_expr.aliases[ii]
                    : expr.import_expr.imports[ii];
                if (!self.add_compiled_module_local(mod, module_index, bind_name)) return false;
            }
        default:
            self.set_compile_error("compiler: unknown expression tag in inline module local collection");
            return false;
    }
    return !self.has_error;
}

fn bool Compiler.add_compiled_module_locals_from_pattern(
    Compiler* self,
    CompiledModule* mod,
    usz module_index,
    Pattern* pat
) {
    if (pat == null) return true;
    switch (pat.tag) {
        case PAT_WILDCARD:
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_DICT:
            return true;
        case PAT_CONS:
            return self.add_compiled_module_locals_from_pattern(mod, module_index, pat.car_pat)
                && self.add_compiled_module_locals_from_pattern(mod, module_index, pat.cdr_pat);
        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                if (!self.add_compiled_module_locals_from_pattern(mod, module_index, pat.elements[i])) return false;
            }
        case PAT_AS:
            return self.add_compiled_module_locals_from_pattern(mod, module_index, pat.as_sub);
        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                if (!self.add_compiled_module_locals_from_pattern(mod, module_index, pat.ctor_sub_patterns[i])) return false;
            }
        case PAT_GUARD:
            if (!self.add_compiled_module_locals_from_pattern(mod, module_index, pat.guard_sub)) return false;
            return self.add_compiled_module_locals_from_expr_lexical(mod, module_index, pat.guard_pred);
        default:
            self.set_compile_error("compiler: unknown pattern tag in inline module local collection");
            return false;
    }
    return true;
}

fn bool Compiler.add_compiled_module_locals_from_expr_lexical(
    Compiler* self,
    CompiledModule* mod,
    usz module_index,
    Expr* expr
) {
    bool saved_effect_rejection = self.reject_lexical_defeffect_globals;
    bool saved_symbol_folding = self.allow_symbol_literal_constant_folding;
    self.reject_lexical_defeffect_globals = true;
    self.allow_symbol_literal_constant_folding = false;
    bool ok = self.add_compiled_module_locals_from_expr(mod, module_index, expr);
    self.allow_symbol_literal_constant_folding = saved_symbol_folding;
    self.reject_lexical_defeffect_globals = saved_effect_rejection;
    return ok;
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
