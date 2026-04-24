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

fn void Compiler.emit_recursive_closure_self_patch(Compiler* self, Expr* rec_init, SymbolId let_name) {
    if (rec_init.tag != E_LAMBDA) return;
    foreach (&def : self.lambda_defs) {
        if (def.body == rec_init.lambda.body && (uint)def.param == (uint)rec_init.lambda.param) {
            for (usz ci = 0; ci < def.capture_count; ci++) {
                if ((uint)def.captures[ci] == (uint)let_name) {
                    self.emit_indent();
                    self.emit("lisp::aot::AotClosureData* _acd_");
                    self.emit_usz(def.id);
                    self.emit(" = (lisp::aot::AotClosureData*)");
                    self.emit_symbol_name(let_name);
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
                    self.emit_symbol_name(let_name);
                    self.emit(";\n");
                    break;
                }
            }
            break;
        }
    }
}

fn bool Compiler.compile_mutable_captured_let(Compiler* self, Expr* expr, bool tail, usz* result) {
    if (!self.is_mutable_captured_var(expr.let_expr.name)) return false;
    usz init_r = self.compile_to_temp(expr.let_expr.init);
    usz result_r = self.next_result();
    self.emit_temp_decl(result_r);
    self.emit(";\n");
    usz define_err_r = self.next_result();
    self.emit_indent();
    self.emit_temp_decl(define_err_r);
    self.emit(" = aot::define_var(\"");
    char[] mc_name = self.interp.symbols.get_name(expr.let_expr.name);
    self.emit_escaped(mc_name);
    self.emit("\", ");
    self.emit_temp_ref(init_r);
    self.emit(");\n");
    self.emit_indent();
    self.emit("if (");
    self.emit_temp_ref(define_err_r);
    self.emit(" != null) {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(result_r);
    self.emit(" = ");
    self.emit_temp_ref(define_err_r);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
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

    bool already = self.is_declared(expr.let_expr.name);
    if (expr.let_expr.is_recursive) {
        if (!already) self.emit_let_forward_decl(expr.let_expr.name);
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_let_assignment(expr.let_expr.name, init_r, false);
        self.emit_recursive_closure_self_patch(expr.let_expr.init, expr.let_expr.name);
    } else {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_let_assignment(expr.let_expr.name, init_r, !already);
    }

    if (tail) {
        return self.compile_to_temp_tail(expr.let_expr.body);
    }
    return self.compile_to_temp(expr.let_expr.body);
}

fn usz Compiler.compile_let_flat(Compiler* self, Expr* expr) {
    return self.compile_let_flat_common(expr, false);
}

fn usz Compiler.compile_let_flat_tail(Compiler* self, Expr* expr) {
    return self.compile_let_flat_common(expr, true);
}
