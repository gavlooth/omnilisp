module lisp;

import std::collections::list;
import std::io;
import main;

fn bool compiler_find_global_value_symbol(Interp* interp, Value* target, SymbolId* out_sym) {
    if (interp == null || target == null || out_sym == null) return false;

    Env* env = interp.global_env;
    while (env != null) {
        for (usz i = 0; i < env.binding_count; i++) {
            if (env.bindings[i].value == target) {
                *out_sym = env.bindings[i].name;
                return true;
            }
        }
        env = env.parent;
    }
    return false;
}

fn void Compiler.add_global_if_missing(Compiler* self, SymbolId sym) {
    if (!self.is_global(sym)) {
        self.defined_globals.push(sym);
    }
}

fn bool compiler_has_inline_compiled_module(Compiler* self, SymbolId module_name) {
    for (usz i = 0; i < self.compiled_module_count; i++) {
        if (self.compiled_modules[i].name == module_name) return true;
    }
    return false;
}

fn bool compiler_can_fold_symbol_literal(Compiler* self, SymbolId name) {
    if (self == null || !self.allow_symbol_literal_constant_folding) return false;
    if (self.symbol_is_active_primitive_shadow(name) || self.is_global(name)) return false;
    return true;
}

fn bool compiler_expr_is_literal_false(Compiler* self, Expr* expr) {
    if (self == null || self.interp == null || expr == null) return false;
    if (expr.tag == E_VAR) {
        return compiler_can_fold_symbol_literal(self, expr.var_expr.name)
            && expr.var_expr.name == self.interp.sym_false;
    }
    if (expr.tag != E_LIT) return false;
    return expr.lit.value.tag == NIL ||
        (expr.lit.value.tag == SYMBOL && expr.lit.value.sym_val == self.interp.sym_false);
}

fn bool compiler_expr_is_literal_truthy(Compiler* self, Expr* expr) {
    if (self == null || self.interp == null || expr == null) return false;
    if (expr.tag == E_VAR) {
        return compiler_can_fold_symbol_literal(self, expr.var_expr.name)
            && expr.var_expr.name == self.interp.sym_true;
    }
    if (expr.tag != E_LIT) return false;
    return !compiler_expr_is_literal_false(self, expr);
}

fn void Compiler.add_inline_module_backing_globals(Compiler* self, Expr* expr) {
    if (expr == null || expr.tag != E_MODULE) return;
    if (!self.ensure_inline_compiled_module_metadata(expr)) return;
    CompiledModule* mod = self.find_inline_compiled_module(expr.module_expr.name);
    if (mod == null) return;
    for (usz i = 0; i < mod.local_count; i++) {
        self.add_global_if_missing(mod.local_backings[i]);
    }
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        self.collect_inline_module_backing_globals_in_expr(expr.module_expr.body[i]);
    }
}

fn void Compiler.collect_inline_module_backing_globals_in_expr(Compiler* self, Expr* expr) {
    if (expr == null) return;
    switch (expr.tag) {
        case E_LIT:
        case E_VAR:
        case E_QUOTE:
        case E_PATH:
        case E_DEFMACRO:
        case E_IMPORT:
        case E_EXPORT_FROM:
        case E_FFI_LIB:
        case E_FFI_FN:
        case E_DEFTYPE:
        case E_DEFABSTRACT:
        case E_DEFUNION:
        case E_DEFALIAS:
        case E_DEFEFFECT:
            return;
        case E_MODULE:
            self.add_inline_module_backing_globals(expr);
        case E_WITH_MODULE:
            if (expr.with_module != null) {
                for (usz wi = 0; wi < expr.with_module.body_count; wi++) {
                    self.collect_inline_module_backing_globals_in_expr(expr.with_module.body[wi]);
                }
            }
        case E_DEFINE:
            self.collect_inline_module_backing_globals_in_expr(expr.define.value);
        case E_LAMBDA:
            if (expr.lambda != null) self.collect_inline_module_backing_globals_in_expr(expr.lambda.body);
        case E_APP:
            self.collect_inline_module_backing_globals_in_expr(expr.app.func);
            self.collect_inline_module_backing_globals_in_expr(expr.app.arg);
        case E_IF:
            self.collect_inline_module_backing_globals_in_expr(expr.if_expr.test);
            if (compiler_expr_is_literal_false(self, expr.if_expr.test)) {
                self.collect_inline_module_backing_globals_in_expr(expr.if_expr.else_branch);
            } else if (compiler_expr_is_literal_truthy(self, expr.if_expr.test)) {
                self.collect_inline_module_backing_globals_in_expr(expr.if_expr.then_branch);
            } else {
                self.collect_inline_module_backing_globals_in_expr(expr.if_expr.then_branch);
                self.collect_inline_module_backing_globals_in_expr(expr.if_expr.else_branch);
            }
        case E_LET:
            self.collect_inline_module_backing_globals_in_expr(expr.let_expr.init);
            self.collect_inline_module_backing_globals_in_expr(expr.let_expr.body);
        case E_RESET:
            self.collect_inline_module_backing_globals_in_expr(expr.reset.body);
        case E_SHIFT:
            self.collect_inline_module_backing_globals_in_expr(expr.shift.body);
        case E_PERFORM:
            if (expr.perform.has_tag_expr) self.collect_inline_module_backing_globals_in_expr(expr.perform.tag_expr);
            self.collect_inline_module_backing_globals_in_expr(expr.perform.arg);
        case E_RESOLVE:
            self.collect_inline_module_backing_globals_in_expr(expr.resolve.value);
        case E_HANDLE:
            if (expr.handle != null) {
                self.collect_inline_module_backing_globals_in_expr(expr.handle.body);
                for (usz hi = 0; hi < expr.handle.clause_count; hi++) {
                    if (expr.handle.clauses[hi].has_effect_tag_expr) {
                        self.collect_inline_module_backing_globals_in_expr(expr.handle.clauses[hi].effect_tag_expr);
                    }
                    self.collect_inline_module_backing_globals_in_expr(expr.handle.clauses[hi].handler_body);
                }
            }
        case E_INDEX:
            self.collect_inline_module_backing_globals_in_expr(expr.index.collection);
            self.collect_inline_module_backing_globals_in_expr(expr.index.index);
        case E_MATCH:
            if (expr.match != null) {
                self.collect_inline_module_backing_globals_in_expr(expr.match.scrutinee);
                for (usz mi = 0; mi < expr.match.clause_count; mi++) {
                    self.collect_inline_module_backing_globals_in_expr(expr.match.clauses[mi].result);
                }
            }
        case E_AND:
            self.collect_inline_module_backing_globals_in_expr(expr.and_expr.left);
            self.collect_inline_module_backing_globals_in_expr(expr.and_expr.right);
        case E_OR:
            self.collect_inline_module_backing_globals_in_expr(expr.or_expr.left);
            self.collect_inline_module_backing_globals_in_expr(expr.or_expr.right);
        case E_CALL:
            if (expr.call != null) {
                self.collect_inline_module_backing_globals_in_expr(expr.call.func);
                for (usz ai = 0; ai < expr.call.arg_count; ai++) {
                    self.collect_inline_module_backing_globals_in_expr(expr.call.args[ai]);
                }
            }
        case E_BLOCK:
            if (expr.block != null) {
                for (usz bi = 0; bi < expr.block.expr_count; bi++) {
                    self.collect_inline_module_backing_globals_in_expr(expr.block.exprs[bi]);
                }
            }
        case E_SET:
            self.collect_inline_module_backing_globals_in_expr(expr.set_expr.value);
        case E_QUASIQUOTE:
            self.collect_inline_module_backing_globals_in_expr(expr.quasiquote.body);
        case E_UNQUOTE:
            self.collect_inline_module_backing_globals_in_expr(expr.unquote.body);
        case E_UNQUOTE_SPLICING:
            self.collect_inline_module_backing_globals_in_expr(expr.unquote_splicing.body);
        default:
            if (try msg = io::bprintf(&self.error_storage, "compiler: unknown expression tag in inline module backing global collection: %d", (int)expr.tag)) {
                self.set_compile_error(msg);
            } else {
                self.set_compile_error("compiler: unknown expression tag in inline module backing global collection");
            }
    }
}

fn void Compiler.add_inline_import_globals(Compiler* self, ExprImport* imp) {
    if (imp == null || imp.has_path) return;
    CompiledModule* mod = self.find_inline_compiled_module(imp.name);
    if (mod == null) return;

    if (imp.import_all) {
        for (usz i = 0; i < mod.export_count; i++) {
            self.add_global_if_missing(mod.exports[i]);
        }
        return;
    }

    for (usz ii = 0; ii < imp.import_count; ii++) {
        SymbolId bind_name = (uint)imp.aliases[ii] != 0 ? imp.aliases[ii] : imp.imports[ii];
        self.add_global_if_missing(bind_name);
    }
}

fn void Compiler.collect_expr_generated_globals(Compiler* self, Expr* expr) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_LIT:
            self.collect_literal_generated_globals(expr.lit.value);
        case E_VAR:
        case E_QUOTE:
        case E_PATH:
        case E_DEFMACRO:
        case E_IMPORT:
        case E_EXPORT_FROM:
            return;
        case E_DEFINE:
            self.add_global_if_missing(expr.define.name);
            self.collect_expr_generated_globals(expr.define.value);
        case E_FFI_LIB:
            self.add_global_if_missing(expr.ffi_lib.name);
        case E_FFI_FN:
            if (expr.ffi_fn != null) {
                self.add_global_if_missing(expr.ffi_fn.fn_name);
            }
        case E_LAMBDA:
            if (expr.lambda != null) self.collect_expr_generated_globals_lexical(expr.lambda.body);
        case E_APP:
            self.collect_expr_generated_globals(expr.app.func);
            self.collect_expr_generated_globals(expr.app.arg);
        case E_IF:
            self.collect_expr_generated_globals(expr.if_expr.test);
            if (compiler_expr_is_literal_false(self, expr.if_expr.test)) {
                self.collect_expr_generated_globals(expr.if_expr.else_branch);
            } else if (compiler_expr_is_literal_truthy(self, expr.if_expr.test)) {
                self.collect_expr_generated_globals(expr.if_expr.then_branch);
            } else {
                self.collect_expr_generated_globals(expr.if_expr.then_branch);
                self.collect_expr_generated_globals(expr.if_expr.else_branch);
            }
        case E_LET:
            self.collect_expr_generated_globals(expr.let_expr.init);
            self.collect_expr_generated_globals_lexical(expr.let_expr.body);
            self.collect_pattern_generated_globals(expr.let_expr.pattern);
        case E_RESET:
            self.collect_expr_generated_globals(expr.reset.body);
        case E_SHIFT:
            self.collect_expr_generated_globals_lexical(expr.shift.body);
        case E_PERFORM:
            if (expr.perform.has_tag_expr) self.collect_expr_generated_globals(expr.perform.tag_expr);
            self.collect_expr_generated_globals(expr.perform.arg);
        case E_RESOLVE:
            self.collect_expr_generated_globals(expr.resolve.value);
        case E_HANDLE:
            if (expr.handle != null) {
                self.collect_expr_generated_globals(expr.handle.body);
                for (usz hi = 0; hi < expr.handle.clause_count; hi++) {
                    if (expr.handle.clauses[hi].has_effect_tag_expr) {
                        self.collect_expr_generated_globals(expr.handle.clauses[hi].effect_tag_expr);
                    }
                    self.collect_expr_generated_globals_lexical(expr.handle.clauses[hi].handler_body);
                }
            }
        case E_INDEX:
            self.collect_expr_generated_globals(expr.index.collection);
            self.collect_expr_generated_globals(expr.index.index);
        case E_MATCH:
            if (expr.match != null) {
                self.collect_expr_generated_globals(expr.match.scrutinee);
                for (usz mi = 0; mi < expr.match.clause_count; mi++) {
                    self.collect_pattern_generated_globals(expr.match.clauses[mi].pattern);
                    self.collect_expr_generated_globals_lexical(expr.match.clauses[mi].result);
                }
            }
        case E_AND:
            self.collect_expr_generated_globals(expr.and_expr.left);
            self.collect_expr_generated_globals(expr.and_expr.right);
        case E_OR:
            self.collect_expr_generated_globals(expr.or_expr.left);
            self.collect_expr_generated_globals(expr.or_expr.right);
        case E_CALL:
            if (expr.call != null) {
                self.collect_expr_generated_globals(expr.call.func);
                for (usz ai = 0; ai < expr.call.arg_count; ai++) {
                    self.collect_expr_generated_globals(expr.call.args[ai]);
                }
            }
        case E_BLOCK:
            if (expr.block != null) {
                for (usz bi = 0; bi < expr.block.expr_count; bi++) {
                    self.collect_expr_generated_globals(expr.block.exprs[bi]);
                }
            }
        case E_SET:
            self.collect_expr_generated_globals(expr.set_expr.value);
        case E_QUASIQUOTE:
            self.collect_expr_generated_globals(expr.quasiquote.body);
        case E_UNQUOTE:
            self.collect_expr_generated_globals(expr.unquote.body);
        case E_UNQUOTE_SPLICING:
            self.collect_expr_generated_globals(expr.unquote_splicing.body);
        case E_MODULE:
            self.add_inline_module_backing_globals(expr);
        case E_WITH_MODULE:
            if (expr.with_module != null) {
                for (usz wi = 0; wi < expr.with_module.body_count; wi++) {
                    self.collect_expr_generated_globals(expr.with_module.body[wi]);
                }
            }
        case E_DEFTYPE:
            if (expr.deftype != null) {
                self.add_global_if_missing(expr.deftype.name);
            }
        case E_DEFABSTRACT:
            self.add_global_if_missing(expr.defabstract.name);
        case E_DEFUNION:
            if (expr.defunion != null) {
                self.add_global_if_missing(expr.defunion.name);
                for (usz vi = 0; vi < expr.defunion.variant_count; vi++) {
                    self.add_global_if_missing(expr.defunion.variants[vi].name);
                }
            }
        case E_DEFALIAS:
            self.add_global_if_missing(expr.defalias.name);
        case E_DEFEFFECT:
            if (self.reject_lexical_defeffect_globals) {
                self.set_compile_error("compiler: lexical effect declarations are not supported in AOT");
            } else {
                self.add_global_if_missing(expr.defeffect.name);
            }
        default:
            if (try msg = io::bprintf(&self.error_storage, "compiler: unknown expression tag in generated global collection: %d", (int)expr.tag)) {
                self.set_compile_error(msg);
            } else {
                self.set_compile_error("compiler: unknown expression tag in generated global collection");
            }
    }
}

fn void Compiler.collect_literal_generated_globals(Compiler* self, Value* value) {
    if (self == null || value == null) return;

    SymbolId global_name = INVALID_SYMBOL_ID;
    if (value.tag == CLOSURE && compiler_find_global_value_symbol(self.interp, value, &global_name)) {
        self.add_global_if_missing(global_name);
        return;
    }

    switch (value.tag) {
        case CONS:
            self.collect_literal_generated_globals(value.cons_val.car);
            self.collect_literal_generated_globals(value.cons_val.cdr);
        case ARRAY:
            if (value.array_val != null) {
                for (usz i = 0; i < value.array_val.length; i++) {
                    self.collect_literal_generated_globals(value.array_val.items[i]);
                }
            }
        case HASHMAP:
        case SET:
            if (value.hashmap_val != null && hashmap_storage_valid(value.hashmap_val)) {
                for (uint i = 0; i < value.hashmap_val.capacity; i++) {
                    if (value.hashmap_val.entries[i].key != null) {
                        self.collect_literal_generated_globals(value.hashmap_val.entries[i].key);
                        self.collect_literal_generated_globals(value.hashmap_val.entries[i].value);
                    }
                }
            }
        case NIL:
        case INT:
        case STRING:
        case SYMBOL:
        case CLOSURE:
        case CONTINUATION:
        case PRIMITIVE:
        case PARTIAL_PRIM:
        case ERROR:
        case DOUBLE:
        case FLOAT32:
        case BIG_INTEGER:
        case BIG_FLOAT:
        case BIG_COMPLEX:
        case COMPLEX128:
        case COMPLEX64:
        case FFI_HANDLE:
        case TYPE_INFO:
        case INSTANCE:
        case METHOD_TABLE:
        case MODULE:
        case ITERATOR:
        case COROUTINE:
        case TIME_POINT:
        case TENSOR:
        case VOID:
            return;
        default:
            if (try msg = io::bprintf(&self.error_storage, "compiler: unknown value tag in generated global collection: %d", (int)value.tag)) {
                self.set_compile_error(msg);
            } else {
                self.set_compile_error("compiler: unknown value tag in generated global collection");
            }
    }
}

fn void Compiler.collect_expr_generated_globals_lexical(Compiler* self, Expr* expr) {
    bool saved_effect_rejection = self.reject_lexical_defeffect_globals;
    bool saved_symbol_folding = self.allow_symbol_literal_constant_folding;
    self.reject_lexical_defeffect_globals = true;
    self.allow_symbol_literal_constant_folding = false;
    self.collect_expr_generated_globals(expr);
    self.allow_symbol_literal_constant_folding = saved_symbol_folding;
    self.reject_lexical_defeffect_globals = saved_effect_rejection;
}

fn void Compiler.collect_pattern_generated_globals(Compiler* self, Pattern* pat) {
    if (pat == null) return;

    switch (pat.tag) {
        case PAT_WILDCARD:
        case PAT_VAR:
        case PAT_LIT:
        case PAT_QUOTE:
        case PAT_DICT:
            return;
        case PAT_CONS:
            self.collect_pattern_generated_globals(pat.car_pat);
            self.collect_pattern_generated_globals(pat.cdr_pat);
        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                self.collect_pattern_generated_globals(pat.elements[i]);
            }
        case PAT_AS:
            self.collect_pattern_generated_globals(pat.as_sub);
        case PAT_CONSTRUCTOR:
            for (usz i = 0; i < pat.ctor_sub_count; i++) {
                self.collect_pattern_generated_globals(pat.ctor_sub_patterns[i]);
            }
        case PAT_GUARD:
            self.collect_expr_generated_globals_lexical(pat.guard_pred);
            self.collect_pattern_generated_globals(pat.guard_sub);
        default:
            if (try msg = io::bprintf(&self.error_storage, "compiler: unknown pattern tag in generated global collection: %d", (int)pat.tag)) {
                self.set_compile_error(msg);
            } else {
                self.set_compile_error("compiler: unknown pattern tag in generated global collection");
            }
    }
}

fn void Compiler.collect_top_level_defines(Compiler* self, List{Expr*}* exprs) {
    foreach (expr : *exprs) {
        if (expr.tag == E_MODULE) {
            self.add_inline_module_backing_globals(expr);
        } else if (expr.tag == E_WITH_MODULE) {
            if (expr.with_module != null) {
                for (usz wi = 0; wi < expr.with_module.body_count; wi++) {
                    self.collect_expr_generated_globals(expr.with_module.body[wi]);
                }
            }
        } else if (expr.tag == E_IMPORT) {
            self.add_inline_import_globals(&expr.import_expr);
            bool inline_import = !expr.import_expr.has_path
                && compiler_has_inline_compiled_module(self, expr.import_expr.name);
            if (!expr.import_expr.has_path) {
                self.add_global_if_missing(expr.import_expr.name);
            }
            if (inline_import) continue;
            for (usz ii = 0; ii < expr.import_expr.import_count; ii++) {
                SymbolId imp_name = expr.import_expr.imports[ii];
                SymbolId alias_name = expr.import_expr.aliases[ii];
                self.add_global_if_missing(imp_name);
                if ((uint)alias_name != 0) {
                    self.add_global_if_missing(alias_name);
                }
            }
        } else {
            self.collect_expr_generated_globals(expr);
        }
    }
}

fn void Compiler.emit_global_lookup_sync(Compiler* self, SymbolId sym) {
    self.emit_indent();
    self.emit_symbol_name(sym);
    self.emit(" = aot::lookup_var(\"");
    self.emit_escaped(self.interp.symbols.get_name(sym));
    self.emit("\");\n");
}
