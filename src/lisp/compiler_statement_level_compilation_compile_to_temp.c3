module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (compile_to_temp)
//
// compile_to_temp emits statements that compute an expression's value
// and store it in a temp variable _rN. Returns N.
// This avoids GCC-style statement expressions ({ ... }) which C3 doesn't support.
// =============================================================================

fn usz Compiler.next_result(Compiler* self) {
    usz id = self.temp_counter;
    self.temp_counter++;
    return id;
}

fn void Compiler.emit_temp_decl(Compiler* self, usz id) {
    self.emit_indent();
    self.emit("lisp::Value* _r");
    self.emit_usz(id);
}

fn void Compiler.emit_temp_ref(Compiler* self, usz id) {
    self.emit("_r");
    self.emit_usz(id);
}

/**
 * Compile an expression as statements, storing result in _rN.
 * Returns N. All temp declarations are emitted as proper C3 statements.
 */
fn usz Compiler.compile_to_temp(Compiler* self, Expr* expr) {
    if (expr == null) {
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::make_nil();\n");
        return id;
    }

    switch (expr.tag) {
        case E_CALL:
            return self.compile_call_flat(expr);

        case E_LET:
            return self.compile_let_flat(expr);

        case E_AND:
            return self.compile_and_flat(expr);

        case E_OR:
            return self.compile_or_flat(expr);

        case E_BEGIN:
            return self.compile_begin_flat(expr);

        case E_SET:
            return self.compile_set_flat(expr);

        case E_RESET:
            return self.compile_reset_flat(expr);

        case E_SHIFT:
            return self.compile_shift_flat(expr);

        case E_PERFORM:
            return self.compile_perform_flat(expr);

        case E_RESOLVE: {
            // Compile value, then call aot::compiled_resolve(__k, val)
            usz val_r = self.compile_to_temp(expr.resolve.value);
            // Emit __k resolution (since it is a free var, we can emit a var read)
            // Wait! The symbol is literally __k.
            // I'll emit it as a C3 identifier `__k` or as captured.
            // But how do I emit a variable read without an AST node?
            // Actually, I can just create a dummy E_VAR node!
            Expr dummy_k;
            dummy_k.tag = E_VAR;
            dummy_k.var_expr.name = self.interp.sym__k;
            usz k_r = self.compile_to_temp(&dummy_k);

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::compiled_resolve(");
            self.emit_temp_ref(k_r);
            self.emit(", ");
            self.emit_temp_ref(val_r);
            self.emit(");\n");
            return id;
        }

        case E_HANDLE:
            return self.compile_handle_flat(expr);

        case E_QUASIQUOTE:
            return self.compile_qq_flat(expr.quasiquote.body, 0);

        case E_DEFMACRO:
            // Macros expanded at compile time — no-op at runtime
            usz defmacro_id = self.next_result();
            self.emit_temp_decl(defmacro_id);
            self.emit(" = aot::make_nil(); // macro: expanded at compile time\n");
            return defmacro_id;

        case E_MODULE:
            return self.compile_module_flat(expr);

        case E_IMPORT:
            return self.compile_import_flat(expr);

        case E_EXPORT_FROM:
            return self.compile_export_from_flat(expr);

        case E_IF:
            return self.compile_if_flat(expr);

        case E_APP:
            return self.compile_app_flat(expr);

        case E_LAMBDA:
            return self.compile_lambda_flat(expr);

        case E_MATCH:
            return self.compile_match_flat(expr);

        case E_INDEX:
            usz idx_coll_r = self.compile_to_temp(expr.index.collection);
            usz idx_idx_r = self.compile_to_temp(expr.index.index);
            usz idx_id = self.next_result();
            self.emit_temp_decl(idx_id);
            self.emit(" = aot::index(");
            self.emit_temp_ref(idx_coll_r);
            self.emit(", ");
            self.emit_temp_ref(idx_idx_r);
            self.emit(");\n");
            return idx_id;

        case E_DEFINE:
            usz def_val_r = self.compile_to_temp(expr.define.value);
            self.emit_indent();
            self.emit_symbol_name(expr.define.name);
            self.emit(" = ");
            self.emit_temp_ref(def_val_r);
            self.emit(";\n");
            return def_val_r;

        default:
            // Leaf expressions (E_LIT, E_VAR, E_QUOTE, E_PATH):
            // compile_expr produces inline code without ({ }) or recursion
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = ");
            self.compile_expr(expr);
            self.emit(";\n");
            return id;
    }
}

/**
 * compile_to_temp for tail position. For calls, emits make_thunk instead of rt_invoke.
 */
fn usz Compiler.compile_to_temp_tail(Compiler* self, Expr* expr) {
    if (expr == null) {
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::make_nil();\n");
        return id;
    }

    switch (expr.tag) {
        case E_APP:
            return self.compile_app_tail_flat(expr);

        case E_CALL:
            return self.compile_call_tail_flat(expr);

        case E_IF:
            // Propagate tail to branches
            usz cond = self.compile_to_temp(expr.if_expr.test);
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(";\n");
            self.emit_indent();
            self.emit("if (aot::is_truthy(");
            self.emit_temp_ref(cond);
            self.emit(")) {\n");
            self.indent++;
            usz then_r = self.compile_to_temp_tail(expr.if_expr.then_branch);
            self.emit_indent();
            self.emit_temp_ref(id);
            self.emit(" = ");
            self.emit_temp_ref(then_r);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("} else {\n");
            self.indent++;
            usz else_r = self.compile_to_temp_tail(expr.if_expr.else_branch);
            self.emit_indent();
            self.emit_temp_ref(id);
            self.emit(" = ");
            self.emit_temp_ref(else_r);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("}\n");
            return id;

        case E_BEGIN:
            usz bcount = expr.begin.expr_count;
            if (bcount == 0) {
                usz bid = self.next_result();
                self.emit_temp_decl(bid);
                self.emit(" = aot::make_nil();\n");
                return bid;
            }
            for (usz i = 0; i < bcount - 1; i++) {
                self.compile_to_temp(expr.begin.exprs[i]);
            }
            return self.compile_to_temp_tail(expr.begin.exprs[bcount - 1]);

        case E_LET:
            return self.compile_let_flat_tail(expr);

        case E_AND:
            usz and_left = self.compile_to_temp(expr.and_expr.left);
            usz and_id = self.next_result();
            self.emit_temp_decl(and_id);
            self.emit(";\n");
            self.emit_indent();
            self.emit("if (aot::is_truthy(");
            self.emit_temp_ref(and_left);
            self.emit(")) {\n");
            self.indent++;
            usz and_right = self.compile_to_temp_tail(expr.and_expr.right);
            self.emit_indent();
            self.emit_temp_ref(and_id);
            self.emit(" = ");
            self.emit_temp_ref(and_right);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("} else {\n");
            self.indent++;
            self.emit_indent();
            self.emit_temp_ref(and_id);
            self.emit(" = ");
            self.emit_temp_ref(and_left);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("}\n");
            return and_id;

        case E_OR:
            usz or_left = self.compile_to_temp(expr.or_expr.left);
            usz or_id = self.next_result();
            self.emit_temp_decl(or_id);
            self.emit(";\n");
            self.emit_indent();
            self.emit("if (aot::is_truthy(");
            self.emit_temp_ref(or_left);
            self.emit(")) {\n");
            self.indent++;
            self.emit_indent();
            self.emit_temp_ref(or_id);
            self.emit(" = ");
            self.emit_temp_ref(or_left);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("} else {\n");
            self.indent++;
            usz or_right = self.compile_to_temp_tail(expr.or_expr.right);
            self.emit_indent();
            self.emit_temp_ref(or_id);
            self.emit(" = ");
            self.emit_temp_ref(or_right);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("}\n");
            return or_id;

        default:
            // Non-tail forms: just use compile_to_temp
            return self.compile_to_temp(expr);
    }
}

// --- Flat (statement-level) compilation of individual forms ---

fn usz Compiler.compile_call_flat(Compiler* self, Expr* expr) {
    usz arg_count = expr.call.arg_count;

    if (expr.call.func.tag == E_VAR) {
        char[] fname = self.interp.symbols.get_name(expr.call.func.var_expr.name);
        if (self.str_eq(fname, "dict")) {
            if (arg_count == 0) {
                usz id = self.next_result();
                self.emit_temp_decl(id);
                self.emit(" = aot::dict_from_args(aot::make_nil());\n");
                return id;
            }
            usz[32] arg_temps;
            for (usz i = 0; i < arg_count && i < 32; i++) {
                arg_temps[i] = self.compile_to_temp(expr.call.args[i]);
            }
            usz args_list = self.next_result();
            self.emit_temp_decl(args_list);
            self.emit(" = aot::make_nil();\n");
            for (isz i = (isz)arg_count - 1; i >= 0; i--) {
                self.emit_indent();
                self.emit_temp_ref(args_list);
                self.emit(" = aot::cons(");
                self.emit_temp_ref(arg_temps[(usz)i]);
                self.emit(", ");
                self.emit_temp_ref(args_list);
                self.emit(");\n");
            }
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::dict_from_args(");
            self.emit_temp_ref(args_list);
            self.emit(");\n");
            return id;
        }
        if (self.str_eq(fname, "list")) {
            if (arg_count == 0) {
                usz id = self.next_result();
                self.emit_temp_decl(id);
                self.emit(" = aot::make_nil();\n");
                return id;
            }
            usz[32] arg_temps;
            for (usz i = 0; i < arg_count && i < 32; i++) {
                arg_temps[i] = self.compile_to_temp(expr.call.args[i]);
            }
            usz result_r = self.next_result();
            self.emit_temp_decl(result_r);
            self.emit(" = aot::make_nil();\n");
            for (isz i = (isz)arg_count - 1; i >= 0; i--) {
                self.emit_indent();
                self.emit_temp_ref(result_r);
                self.emit(" = aot::cons(");
                self.emit_temp_ref(arg_temps[(usz)i]);
                self.emit(", ");
                self.emit_temp_ref(result_r);
                self.emit(");\n");
            }
            return result_r;
        }
    }

    if (arg_count == 0) {
        usz func_r = self.compile_to_temp(expr.call.func);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::invoke(");
        self.emit_temp_ref(func_r);
        self.emit(", aot::make_nil());\n");
        return id;
    }

    usz[32] arg_temps;
    for (usz i = 0; i < arg_count && i < 32; i++) {
        arg_temps[i] = self.compile_to_temp(expr.call.args[i]);
    }

    usz func_r = self.compile_to_temp(expr.call.func);

    usz args_list = self.next_result();
    self.emit_temp_decl(args_list);
    self.emit(" = aot::make_nil();\n");
    for (isz i = (isz)arg_count - 1; i >= 0; i--) {
        self.emit_indent();
        self.emit_temp_ref(args_list);
        self.emit(" = aot::cons(");
        self.emit_temp_ref(arg_temps[(usz)i]);
        self.emit(", ");
        self.emit_temp_ref(args_list);
        self.emit(");\n");
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::apply_multi(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(args_list);
    self.emit(", ");
    self.emit_usz(arg_count);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_call_tail_flat(Compiler* self, Expr* expr) {
    usz arg_count = expr.call.arg_count;

    // Special cases: list/dict build data structures, delegate to non-tail version
    if (expr.call.func.tag == E_VAR) {
        char[] fname = self.interp.symbols.get_name(expr.call.func.var_expr.name);
        if (self.str_eq(fname, "list") || self.str_eq(fname, "dict")) {
            return self.compile_call_flat(expr);
        }
    }

    if (arg_count == 0) {
        usz func_r = self.compile_to_temp(expr.call.func);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::invoke_tail(");
        self.emit_temp_ref(func_r);
        self.emit(", aot::make_nil());\n");
        return id;
    }

    // Compile each arg to a temp
    usz[32] arg_temps;
    for (usz i = 0; i < arg_count && i < 32; i++) {
        arg_temps[i] = self.compile_to_temp(expr.call.args[i]);
    }

    // Compile func
    usz func_r = self.compile_to_temp(expr.call.func);

    // Build cons list of args
    usz args_list = self.next_result();
    self.emit_temp_decl(args_list);
    self.emit(" = aot::make_nil();\n");
    for (isz i = (isz)arg_count - 1; i >= 0; i--) {
        self.emit_indent();
        self.emit_temp_ref(args_list);
        self.emit(" = aot::cons(");
        self.emit_temp_ref(arg_temps[(usz)i]);
        self.emit(", ");
        self.emit_temp_ref(args_list);
        self.emit(");\n");
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::apply_multi_tail(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(args_list);
    self.emit(", ");
    self.emit_usz(arg_count);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_app_flat(Compiler* self, Expr* expr) {
    usz func_r = self.compile_to_temp(expr.app.func);
    usz arg_r = self.compile_to_temp(expr.app.arg);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::invoke(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_app_tail_flat(Compiler* self, Expr* expr) {
    usz func_r = self.compile_to_temp(expr.app.func);
    usz arg_r = self.compile_to_temp(expr.app.arg);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::invoke_tail(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_if_flat(Compiler* self, Expr* expr) {
    usz cond = self.compile_to_temp(expr.if_expr.test);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_truthy(");
    self.emit_temp_ref(cond);
    self.emit(")) {\n");
    self.indent++;
    usz then_r = self.compile_to_temp(expr.if_expr.then_branch);
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(then_r);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    usz else_r = self.compile_to_temp(expr.if_expr.else_branch);
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(else_r);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn usz Compiler.compile_let_flat(Compiler* self, Expr* expr) {
    bool is_mc = self.is_mutable_captured_var(expr.let_expr.name);

    if (is_mc) {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        self.emit("aot::define_var(\"");
        char[] mc_name = self.interp.symbols.get_name(expr.let_expr.name);
        self.emit_escaped(mc_name);
        self.emit("\", ");
        self.emit_temp_ref(init_r);
        self.emit(");\n");
        return self.compile_to_temp(expr.let_expr.body);
    }

    bool already = self.is_declared(expr.let_expr.name);

    if (expr.let_expr.is_recursive) {
        if (!already) {
            self.emit_indent();
            self.emit("lisp::Value* ");
            self.emit_symbol_name(expr.let_expr.name);
            self.emit(";\n");
            self.mark_declared(expr.let_expr.name);
        }
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        self.emit_symbol_name(expr.let_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(init_r);
        self.emit(";\n");

        Expr* rec_init = expr.let_expr.init;
        if (rec_init.tag == E_LAMBDA) {
            foreach (&def : self.lambda_defs) {
                if (def.body == rec_init.lambda.body && (uint)def.param == (uint)rec_init.lambda.param) {
                    for (usz ci = 0; ci < def.capture_count; ci++) {
                        if ((uint)def.captures[ci] == (uint)expr.let_expr.name) {
                            self.emit_indent();
                            self.emit("lisp::aot::AotClosureData* _acd_");
                            self.emit_usz(def.id);
                            self.emit(" = (lisp::aot::AotClosureData*)");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(".prim_val.user_data;\n");
                            self.emit_indent();
                            self.emit("Lambda_");
                            self.emit_usz(def.id);
                            self.emit("* _sp_");
                            self.emit_usz(def.id);
                            self.emit(" = (Lambda_");
                            self.emit_usz(def.id);
                            self.emit("*)_acd_");
                            self.emit_usz(def.id);
                            self.emit(".data;\n");
                            self.emit_indent();
                            self.emit("_sp_");
                            self.emit_usz(def.id);
                            self.emit(".captured_");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(" = ");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(";\n");
                            break;
                        }
                    }
                    break;
                }
            }
        }
    } else {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        if (!already) {
            self.emit("lisp::Value* ");
            self.mark_declared(expr.let_expr.name);
        }
        self.emit_symbol_name(expr.let_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(init_r);
        self.emit(";\n");
    }

    return self.compile_to_temp(expr.let_expr.body);
}
fn usz Compiler.compile_let_flat_tail(Compiler* self, Expr* expr) {
    bool is_mc = self.is_mutable_captured_var(expr.let_expr.name);

    if (is_mc) {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        self.emit("aot::define_var(\"");
        char[] mc_name = self.interp.symbols.get_name(expr.let_expr.name);
        self.emit_escaped(mc_name);
        self.emit("\", ");
        self.emit_temp_ref(init_r);
        self.emit(");\n");
        return self.compile_to_temp_tail(expr.let_expr.body);
    }

    bool already = self.is_declared(expr.let_expr.name);

    if (expr.let_expr.is_recursive) {
        if (!already) {
            self.emit_indent();
            self.emit("lisp::Value* ");
            self.emit_symbol_name(expr.let_expr.name);
            self.emit(";\n");
            self.mark_declared(expr.let_expr.name);
        }
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        self.emit_symbol_name(expr.let_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(init_r);
        self.emit(";\n");

        // Patch self-reference for recursive closure
        Expr* rec_init = expr.let_expr.init;
        if (rec_init.tag == E_LAMBDA) {
            foreach (&def : self.lambda_defs) {
                if (def.body == rec_init.lambda.body && (uint)def.param == (uint)rec_init.lambda.param) {
                    for (usz ci = 0; ci < def.capture_count; ci++) {
                        if ((uint)def.captures[ci] == (uint)expr.let_expr.name) {
                            // Patch: AOT closures are wrapped as Primitives
                            // Access via prim_val.user_data -> AotClosureData -> data
                            self.emit_indent();
                            self.emit("lisp::aot::AotClosureData* _acd_");
                            self.emit_usz(def.id);
                            self.emit(" = (lisp::aot::AotClosureData*)");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(".prim_val.user_data;\n");
                            self.emit_indent();
                            self.emit("Lambda_");
                            self.emit_usz(def.id);
                            self.emit("* _sp_");
                            self.emit_usz(def.id);
                            self.emit(" = (Lambda_");
                            self.emit_usz(def.id);
                            self.emit("*)_acd_");
                            self.emit_usz(def.id);
                            self.emit(".data;\n");
                            self.emit_indent();
                            self.emit("_sp_");
                            self.emit_usz(def.id);
                            self.emit(".captured_");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(" = ");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(";\n");
                            break;
                        }
                    }
                    break;
                }
            }
        }
    } else {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        if (!already) {
            self.emit("lisp::Value* ");
            self.mark_declared(expr.let_expr.name);
        }
        self.emit_symbol_name(expr.let_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(init_r);
        self.emit(";\n");
    }

    return self.compile_to_temp_tail(expr.let_expr.body);
}

fn usz Compiler.compile_and_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.and_expr.left);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_truthy(");
    self.emit_temp_ref(left);
    self.emit(")) {\n");
    self.indent++;
    usz right = self.compile_to_temp(expr.and_expr.right);
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(right);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(left);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn usz Compiler.compile_or_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.or_expr.left);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_truthy(");
    self.emit_temp_ref(left);
    self.emit(")) {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(left);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    usz right = self.compile_to_temp(expr.or_expr.right);
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(right);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn usz Compiler.compile_begin_flat(Compiler* self, Expr* expr) {
    usz count = expr.begin.expr_count;
    if (count == 0) {
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::make_nil();\n");
        return id;
    }
    // Compile all but last as discarded statements
    for (usz i = 0; i < count - 1; i++) {
        self.compile_to_temp(expr.begin.exprs[i]);
    }
    // Last expression is the result
    return self.compile_to_temp(expr.begin.exprs[count - 1]);
}

fn usz Compiler.compile_set_flat(Compiler* self, Expr* expr) {
    if (self.is_mutable_captured_var(expr.set_expr.name)) {
        usz val_r = self.compile_to_temp(expr.set_expr.value);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::set_var(\"");
        char[] set_name = self.interp.symbols.get_name(expr.set_expr.name);
        self.emit_escaped(set_name);
        self.emit("\", ");
        self.emit_temp_ref(val_r);
        self.emit(");\n");
        return id;
    } else {
        usz val_r = self.compile_to_temp(expr.set_expr.value);
        self.emit_indent();
        self.emit_symbol_name(expr.set_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(val_r);
        self.emit(";\n");
        // Result is the variable itself
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = ");
        self.emit_symbol_name(expr.set_expr.name);
        self.emit(";\n");
        return id;
    }
}

fn bool Compiler.is_builtin_primitive(Compiler* self, SymbolId sym) {
    // Check ONLY the hardcoded primitives list, NOT defined_globals.
    // Used for delegation injection — defined globals still need injection
    // into the interpreter env since they're C3 locals, not interpreter vars.
    char[] name = self.interp.symbols.get_name(sym);
    char[][*] primitives = {
        "+", "-", "*", "/", "%",
        "=", "<", ">", "<=", ">=",
        "cons", "car", "cdr", "null?", "pair?",
        "list", "length", "not",
        "print", "println", "newline", "display",
        "string-append", "string-join", "substring",
        "string-split", "string-length", "string->list",
        "list->string", "string-upcase", "string-downcase",
        "string-trim", "string?", "int?", "symbol?",
        "closure?", "continuation?", "double?", "list?",
        "boolean?", "number?",
        "string->number", "number->string",
        "gensym", "load", "apply", "equal?",
        "dict", "dict-set!", "dict?",
        "ref", "push!", "keys", "values", "has?", "remove!",
        "read-file", "write-file", "file-exists?", "read-lines",
        "type-of",
        "abs", "min", "max", "floor", "ceiling", "round",
        "truncate", "sqrt", "even?", "odd?", "zero?",
        "positive?", "negative?", "gcd",
        "bitwise-and", "bitwise-or", "bitwise-xor",
        "bitwise-not", "lshift", "rshift",
        "true", "false", "nil"
    };
    foreach (prim : primitives) {
        if (self.str_eq(name, prim)) return true;
    }
    return false;
}

// compile_delegate_flat — REMOVED: All expression types now compiled natively.
// Delegation via rt_eval_source() is no longer used.

/**
 * compile_qq_flat — Flat-style quasiquote compilation.
 * Emits statements that build the quoted structure, storing result in _rN.
 * At depth 0, unquote sub-expressions are compiled as normal expressions.
 */
fn usz Compiler.compile_qq_flat(Compiler* self, Expr* tmpl, usz depth) {
    if (tmpl == null) {
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::make_nil();\n");
        return id;
    }

    switch (tmpl.tag) {
        case E_UNQUOTE: {
            if (depth == 0) {
                // Evaluate the unquoted expression
                return self.compile_to_temp(tmpl.unquote.body);
            }
            // Nested: build (unquote <inner>)
            usz inner_r = self.compile_qq_flat(tmpl.unquote.body, depth - 1);
            usz nil_r = self.next_result();
            self.emit_temp_decl(nil_r);
            self.emit(" = aot::make_nil();\n");

            usz rest_r = self.next_result();
            self.emit_temp_decl(rest_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(inner_r);
            self.emit(", ");
            self.emit_temp_ref(nil_r);
            self.emit(");\n");

            usz sym_r = self.next_result();
            self.emit_temp_decl(sym_r);
            self.emit(" = aot::make_symbol(\"unquote\");\n");

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(sym_r);
            self.emit(", ");
            self.emit_temp_ref(rest_r);
            self.emit(");\n");
            return id;
        }
        case E_UNQUOTE_SPLICING: {
            if (depth == 0) {
                // Splicing at top level is an error
                usz id = self.next_result();
                self.emit_temp_decl(id);
                self.emit(" = aot::make_nil(); /* ERROR: ,@ in non-list context */\n");
                return id;
            }
            // Nested: build (unquote-splicing <inner>)
            usz inner_r = self.compile_qq_flat(tmpl.unquote_splicing.body, depth - 1);
            usz nil_r = self.next_result();
            self.emit_temp_decl(nil_r);
            self.emit(" = aot::make_nil();\n");

            usz rest_r = self.next_result();
            self.emit_temp_decl(rest_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(inner_r);
            self.emit(", ");
            self.emit_temp_ref(nil_r);
            self.emit(");\n");

            usz sym_r = self.next_result();
            self.emit_temp_decl(sym_r);
            self.emit(" = aot::make_symbol(\"unquote-splicing\");\n");

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(sym_r);
            self.emit(", ");
            self.emit_temp_ref(rest_r);
            self.emit(");\n");
            return id;
        }
        case E_QUASIQUOTE: {
            // Nested quasiquote: build (quasiquote <inner>)
            usz inner_r = self.compile_qq_flat(tmpl.quasiquote.body, depth + 1);
            usz nil_r = self.next_result();
            self.emit_temp_decl(nil_r);
            self.emit(" = aot::make_nil();\n");

            usz rest_r = self.next_result();
            self.emit_temp_decl(rest_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(inner_r);
            self.emit(", ");
            self.emit_temp_ref(nil_r);
            self.emit(");\n");

            usz sym_r = self.next_result();
            self.emit_temp_decl(sym_r);
            self.emit(" = aot::make_symbol(\"quasiquote\");\n");

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(sym_r);
            self.emit(", ");
            self.emit_temp_ref(rest_r);
            self.emit(");\n");
            return id;
        }
        case E_LIT: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = ");
            self.compile_literal(tmpl.lit.value);
            self.emit(";\n");
            return id;
        }
        case E_VAR: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::make_symbol(\"");
            char[] name = self.interp.symbols.get_name(tmpl.var_expr.name);
            self.emit_escaped(name);
            self.emit("\");\n");
            return id;
        }
        case E_QUOTE: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = ");
            self.compile_literal(tmpl.quote.datum);
            self.emit(";\n");
            return id;
        }
        case E_APP: {
            // Two-element list: (func arg)
            usz func_r = self.compile_qq_flat(tmpl.app.func, depth);
            usz arg_r = self.compile_qq_flat(tmpl.app.arg, depth);

            usz nil_r = self.next_result();
            self.emit_temp_decl(nil_r);
            self.emit(" = aot::make_nil();\n");

            usz inner_r = self.next_result();
            self.emit_temp_decl(inner_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(arg_r);
            self.emit(", ");
            self.emit_temp_ref(nil_r);
            self.emit(");\n");

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(func_r);
            self.emit(", ");
            self.emit_temp_ref(inner_r);
            self.emit(");\n");
            return id;
        }
        case E_CALL:
            return self.compile_qq_call_flat(tmpl, depth);
        default: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::make_nil();\n");
            return id;
        }
    }
}

/**
 * compile_qq_call_flat — Flat-style compilation of E_CALL in QQ context.
 * Builds cons chain from elements, handling ,@ splicing via rt_list_append.
 */
fn usz Compiler.compile_qq_call_flat(Compiler* self, Expr* tmpl, usz depth) {
    usz total = tmpl.call.arg_count + 1;
    Expr*[17] elements;
    elements[0] = tmpl.call.func;
    for (usz i = 0; i < tmpl.call.arg_count && i < 16; i++) {
        elements[i + 1] = tmpl.call.args[i];
    }

    // Check for splicing
    bool has_splice = false;
    if (depth == 0) {
        for (usz i = 0; i < total; i++) {
            if (elements[i].tag == E_UNQUOTE_SPLICING) {
                has_splice = true;
                break;
            }
        }
    }

    if (!has_splice) {
        // No splicing: build cons chain right to left
        // Start with nil, then cons each element from the end
        usz result_r = self.next_result();
        self.emit_temp_decl(result_r);
        self.emit(" = aot::make_nil();\n");

        for (usz i = total; i > 0; i--) {
            usz elem_r = self.compile_qq_flat(elements[i - 1], depth);
            usz new_r = self.next_result();
            self.emit_temp_decl(new_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(elem_r);
            self.emit(", ");
            self.emit_temp_ref(result_r);
            self.emit(");\n");
            result_r = new_r;
        }
        return result_r;
    }

    // Has splicing: process elements right to left
    // For each element:
    //   - if ,@ at depth 0: compile inner, append to accumulated result
    //   - otherwise: compile qq, cons to accumulated result
    usz result_r = self.next_result();
    self.emit_temp_decl(result_r);
    self.emit(" = aot::make_nil();\n");

    for (usz i = total; i > 0; i--) {
        Expr* elem = elements[i - 1];
        if (elem.tag == E_UNQUOTE_SPLICING && depth == 0) {
            // Splice: compile the inner expression and append it before current result
            usz splice_r = self.compile_to_temp(elem.unquote_splicing.body);
            usz new_r = self.next_result();
            self.emit_temp_decl(new_r);
            self.emit(" = aot::list_append(");
            self.emit_temp_ref(splice_r);
            self.emit(", ");
            self.emit_temp_ref(result_r);
            self.emit(");\n");
            result_r = new_r;
        } else {
            // Normal element: compile qq and cons
            usz elem_r = self.compile_qq_flat(elem, depth);
            usz new_r = self.next_result();
            self.emit_temp_decl(new_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(elem_r);
            self.emit(", ");
            self.emit_temp_ref(result_r);
            self.emit(");\n");
            result_r = new_r;
        }
    }
    return result_r;
}

