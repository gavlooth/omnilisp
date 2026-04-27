module lisp;

import std::io;
import std::collections::list;
import main;

fn void Compiler.emit_let_forward_decl(Compiler* self, SymbolId name) {
    self.emit_indent();
    self.emit("lisp::Value* ");
    self.emit_symbol_name(name);
    self.emit(";\n");
    self.mark_declared(name);
}

fn void Compiler.emit_let_assignment(Compiler* self, SymbolId name, usz init_r, bool with_decl) {
    self.emit_indent();
    if (with_decl) {
        self.emit("lisp::Value* ");
        self.mark_declared(name);
    }
    self.emit_symbol_name(name);
    self.emit(" = ");
    self.emit_temp_ref(init_r);
    self.emit(";\n");
}

fn void Compiler.emit_recursive_closure_self_patch(Compiler* self, Expr* rec_init, SymbolId let_name, SymbolId backing_name) {
    if (rec_init.tag != E_LAMBDA) return;
    foreach (&def : self.lambda_defs) {
        if (def.body == rec_init.lambda.body && (uint)def.param == (uint)rec_init.lambda.param) {
            for (usz ci = 0; ci < def.capture_count; ci++) {
                if ((uint)def.captures[ci] == (uint)let_name) {
                    if (def.capture_is_mutable[ci]) {
                        break;
                    }
                    self.emit_indent();
                    self.emit("lisp::aot::AotClosureData* _acd_");
                    self.emit_usz(def.id);
                    self.emit(" = (lisp::aot::AotClosureData*)");
                    self.emit_symbol_name(backing_name);
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
                    self.emit_symbol_name(let_name);
                    self.emit(" = ");
                    self.emit_symbol_name(backing_name);
                    self.emit(";\n");
                    break;
                }
            }
            break;
        }
    }
}

fn bool Compiler.compile_mutable_captured_let(Compiler* self, Expr* expr, bool tail, usz* result) {
    if (!self.is_let_binding_mutable_capture(expr)) return false;
    usz result_r = self.next_result();
    char[96] cell_buf;
    char[] cell_text = self.generated_name_id(cell_buf[..], "_aot_cell_", (long)result_r);
    SymbolId cell_name = self.interp.symbols.intern(cell_text);
    if (cell_name == INVALID_SYMBOL_ID) {
        self.set_compile_error("compiler: invalid mutable let cell symbol");
        *result = self.emit_nil_temp();
        return true;
    }

    self.emit_temp_decl(result_r);
    self.emit(";\n");
    self.emit_indent();
    self.emit("{\n");
    self.indent++;
    usz declared_mark = self.mark_declared_scope();

    usz init_r = 0;
    if (!expr.let_expr.is_recursive) {
        init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_return_if_sequence_should_stop(init_r);
    }
    self.emit_indent();
    self.emit("lisp::Value* _aot_cell_error_");
    self.emit_usz(result_r);
    self.emit(" = null;\n");
    self.emit_indent();
    self.emit("aot::AotMutableCell* ");
    self.emit_symbol_name(cell_name);
    self.emit(" = aot::make_mutable_cell_checked(");
    if (expr.let_expr.is_recursive) {
        self.emit("aot::make_nil()");
    } else {
        self.emit_temp_ref(init_r);
    }
    self.emit(", &_aot_cell_error_");
    self.emit_usz(result_r);
    self.emit(");\n");
    self.mark_declared(cell_name);
    self.emit_indent();
    self.emit("if (");
    self.emit_symbol_name(cell_name);
    self.emit(" == null) {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(result_r);
    self.emit(" = _aot_cell_error_");
    self.emit_usz(result_r);
    self.emit(" != null ? _aot_cell_error_");
    self.emit_usz(result_r);
    self.emit(" : aot::make_error(\"compiler: out of memory while allocating mutable capture cell\");\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    self.push_mutable_cell_alias(expr.let_expr.name, cell_name);
    bool primitive_shadow = self.push_primitive_shadow_if_needed(expr.let_expr.name);
    if (expr.let_expr.is_recursive) {
        init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_return_if_sequence_should_stop(init_r);
        usz init_err_r = self.next_result();
        self.emit_indent();
        self.emit_temp_decl(init_err_r);
        self.emit(" = null;\n");
        usz init_set_ok_r = self.next_result();
        self.emit_indent();
        self.emit("bool _aot_cell_init_set_ok_");
        self.emit_usz(init_set_ok_r);
        self.emit(" = aot::mutable_cell_set_checked(");
        self.emit_symbol_name(cell_name);
        self.emit(", ");
        self.emit_temp_ref(init_r);
        self.emit(", &");
        self.emit_temp_ref(init_err_r);
        self.emit(");\n");
        self.emit_indent();
        self.emit("if (!_aot_cell_init_set_ok_");
        self.emit_usz(init_set_ok_r);
        self.emit(") {\n");
        self.indent++;
        self.emit_indent();
        self.emit_temp_ref(result_r);
        self.emit(" = ");
        self.emit_temp_ref(init_err_r);
        self.emit(" != null ? ");
        self.emit_temp_ref(init_err_r);
        self.emit(" : aot::make_error(\"compiler: failed to initialize mutable capture cell\");\n");
        self.indent--;
        self.emit_indent();
        self.emit("} else {\n");
        self.indent++;
    }

    if (tail) {
        usz body_r = self.compile_to_temp_tail(expr.let_expr.body);
        self.emit_indent();
        self.emit_temp_ref(result_r);
        self.emit(" = ");
        self.emit_temp_ref(body_r);
        self.emit(";\n");
    } else {
        usz body_r = self.compile_to_temp(expr.let_expr.body);
        self.emit_indent();
        self.emit_temp_ref(result_r);
        self.emit(" = ");
        self.emit_temp_ref(body_r);
        self.emit(";\n");
    }
    if (expr.let_expr.is_recursive) {
        self.indent--;
        self.emit_indent();
        self.emit("}\n");
    }
    self.pop_primitive_shadow(primitive_shadow);
    self.pop_mutable_cell_alias();
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    self.restore_declared_scope(declared_mark);
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    *result = result_r;
    return true;
}

fn usz Compiler.compile_let_flat_common(Compiler* self, Expr* expr, bool tail) {
    usz result;
    if (self.compile_mutable_captured_let(expr, tail, &result)) {
        return result;
    }

    usz result_r = self.next_result();
    char[96] backing_buf;
    char[] backing_text = self.generated_name_id(backing_buf[..], "_aot_let_", (long)result_r);
    SymbolId backing_name = self.interp.symbols.intern(backing_text);
    if (backing_name == INVALID_SYMBOL_ID) {
        self.set_compile_error("compiler: invalid let backing symbol");
        return self.emit_nil_temp();
    }
    self.emit_temp_decl(result_r);
    self.emit(";\n");
    self.emit_indent();
    self.emit("{\n");
    self.indent++;
    usz declared_mark = self.mark_declared_scope();

    if (expr.let_expr.is_recursive) {
        self.emit_let_forward_decl(backing_name);
        self.push_lexical_alias(expr.let_expr.name, backing_name);
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_return_if_error_or_sequence_should_stop(init_r);
        self.emit_let_assignment(backing_name, init_r, false);
        self.emit_recursive_closure_self_patch(expr.let_expr.init, expr.let_expr.name, backing_name);
    } else {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_return_if_sequence_should_stop(init_r);
        self.emit_let_assignment(backing_name, init_r, true);
        self.push_lexical_alias(expr.let_expr.name, backing_name);
    }

    bool primitive_shadow = self.push_primitive_shadow_if_needed(expr.let_expr.name);
    usz body_r = tail
        ? self.compile_to_temp_tail(expr.let_expr.body)
        : self.compile_to_temp(expr.let_expr.body);
    self.pop_primitive_shadow(primitive_shadow);
    self.emit_temp_assign(result_r, body_r);
    self.pop_lexical_alias();
    self.restore_declared_scope(declared_mark);
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return result_r;
}

fn usz Compiler.compile_let_flat(Compiler* self, Expr* expr) {
    return self.compile_let_flat_common(expr, false);
}

fn usz Compiler.compile_let_flat_tail(Compiler* self, Expr* expr) {
    return self.compile_let_flat_common(expr, true);
}
