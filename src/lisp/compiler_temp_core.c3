module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (core temp dispatch)
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

fn usz Compiler.emit_nil_temp(Compiler* self) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_nil();\n");
    return id;
}

fn usz Compiler.compile_leaf_expr_to_temp(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = ");
    self.compile_expr(expr);
    self.emit(";\n");
    return id;
}

fn usz Compiler.compile_resolve_flat(Compiler* self, Expr* expr) {
    usz val_r = self.compile_to_temp(expr.resolve.value);
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

fn usz Compiler.compile_index_flat(Compiler* self, Expr* expr) {
    usz coll_r = self.compile_to_temp(expr.index.collection);
    usz idx_r = self.compile_to_temp(expr.index.index);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::index(");
    self.emit_temp_ref(coll_r);
    self.emit(", ");
    self.emit_temp_ref(idx_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_define_flat(Compiler* self, Expr* expr) {
    usz def_val_r = self.compile_to_temp(expr.define.value);
    self.emit_indent();
    self.emit_symbol_name(expr.define.name);
    self.emit(" = ");
    self.emit_temp_ref(def_val_r);
    self.emit(";\n");
    return def_val_r;
}

fn void Compiler.emit_temp_assign(Compiler* self, usz dst_id, usz src_id) {
    self.emit_indent();
    self.emit_temp_ref(dst_id);
    self.emit(" = ");
    self.emit_temp_ref(src_id);
    self.emit(";\n");
}

fn void Compiler.compile_branch_assign(Compiler* self, usz dst_id, Expr* branch, bool tail) {
    usz branch_r;
    if (tail) {
        branch_r = self.compile_to_temp_tail(branch);
    } else {
        branch_r = self.compile_to_temp(branch);
    }
    self.emit_temp_assign(dst_id, branch_r);
}

fn void Compiler.compile_tail_branch_assign(Compiler* self, usz dst_id, Expr* branch) {
    self.compile_branch_assign(dst_id, branch, true);
}

fn void Compiler.compile_begin_prefix(Compiler* self, Expr** exprs, usz count) {
    for (usz i = 0; i < count; i++) {
        self.compile_to_temp(exprs[i]);
    }
}

fn usz Compiler.compile_short_circuit(Compiler* self, usz left_r, Expr* right_expr, bool left_when_truthy, bool tail) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_truthy(");
    self.emit_temp_ref(left_r);
    self.emit(")) {\n");
    self.indent++;
    if (left_when_truthy) {
        self.emit_temp_assign(id, left_r);
    } else {
        self.compile_branch_assign(id, right_expr, tail);
    }
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    if (left_when_truthy) {
        self.compile_branch_assign(id, right_expr, tail);
    } else {
        self.emit_temp_assign(id, left_r);
    }
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn usz Compiler.compile_tail_short_circuit(Compiler* self, usz left_r, Expr* right_expr, bool left_when_truthy) {
    return self.compile_short_circuit(left_r, right_expr, left_when_truthy, true);
}

fn usz Compiler.compile_if_common(Compiler* self, Expr* expr, bool tail) {
    usz cond = self.compile_to_temp(expr.if_expr.test);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_truthy(");
    self.emit_temp_ref(cond);
    self.emit(")) {\n");
    self.indent++;
    self.compile_branch_assign(id, expr.if_expr.then_branch, tail);
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    self.compile_branch_assign(id, expr.if_expr.else_branch, tail);
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn usz Compiler.compile_begin_common(Compiler* self, Expr* expr, bool tail) {
    usz count = expr.begin.expr_count;
    if (count == 0) return self.emit_nil_temp();
    self.compile_begin_prefix(expr.begin.exprs, count - 1);
    if (tail) return self.compile_to_temp_tail(expr.begin.exprs[count - 1]);
    return self.compile_to_temp(expr.begin.exprs[count - 1]);
}

fn usz Compiler.compile_tail_if_flat(Compiler* self, Expr* expr) {
    return self.compile_if_common(expr, true);
}

fn usz Compiler.compile_tail_begin_flat(Compiler* self, Expr* expr) {
    return self.compile_begin_common(expr, true);
}

fn usz Compiler.compile_tail_and_flat(Compiler* self, Expr* expr) {
    usz and_left = self.compile_to_temp(expr.and_expr.left);
    return self.compile_tail_short_circuit(and_left, expr.and_expr.right, false);
}

fn usz Compiler.compile_tail_or_flat(Compiler* self, Expr* expr) {
    usz or_left = self.compile_to_temp(expr.or_expr.left);
    return self.compile_tail_short_circuit(or_left, expr.or_expr.right, true);
}

/**
 * Compile an expression as statements, storing result in _rN.
 * Returns N. All temp declarations are emitted as proper C3 statements.
 */
fn usz Compiler.compile_to_temp_non_null(Compiler* self, Expr* expr) {
    switch (expr.tag) {
        case E_CALL: return self.compile_call_flat(expr);
        case E_LET: return self.compile_let_flat(expr);
        case E_AND: return self.compile_and_flat(expr);
        case E_OR: return self.compile_or_flat(expr);
        case E_BEGIN: return self.compile_begin_flat(expr);
        case E_SET: return self.compile_set_flat(expr);
        case E_RESET: return self.compile_reset_flat(expr);
        case E_SHIFT: return self.compile_shift_flat(expr);
        case E_PERFORM: return self.compile_perform_flat(expr);
        case E_RESOLVE: return self.compile_resolve_flat(expr);
        case E_HANDLE: return self.compile_handle_flat(expr);
        case E_QUASIQUOTE: return self.compile_qq_flat(expr.quasiquote.body, 0);
        case E_DEFMACRO:
            usz defmacro_id = self.next_result();
            self.emit_temp_decl(defmacro_id);
            self.emit(" = aot::make_nil(); // macro: expanded at compile time\n");
            return defmacro_id;
        case E_MODULE: return self.compile_module_flat(expr);
        case E_IMPORT: return self.compile_import_flat(expr);
        case E_EXPORT_FROM: return self.compile_export_from_flat(expr);
        case E_IF: return self.compile_if_flat(expr);
        case E_APP: return self.compile_app_flat(expr);
        case E_LAMBDA: return self.compile_lambda_flat(expr);
        case E_MATCH: return self.compile_match_flat(expr);
        case E_INDEX: return self.compile_index_flat(expr);
        case E_DEFINE: return self.compile_define_flat(expr);
        default: return self.compile_leaf_expr_to_temp(expr);
    }
}

fn usz Compiler.compile_to_temp(Compiler* self, Expr* expr) {
    if (expr == null) {
        return self.emit_nil_temp();
    }
    return self.compile_to_temp_non_null(expr);
}

/**
 * compile_to_temp for tail position. For calls, emits make_thunk instead of rt_invoke.
 */
fn usz Compiler.compile_to_temp_tail(Compiler* self, Expr* expr) {
    if (expr == null) {
        return self.emit_nil_temp();
    }

    switch (expr.tag) {
        case E_APP:
            return self.compile_app_tail_flat(expr);

        case E_CALL:
            return self.compile_call_tail_flat(expr);

        case E_IF:
            return self.compile_tail_if_flat(expr);

        case E_BEGIN:
            return self.compile_tail_begin_flat(expr);

        case E_LET:
            return self.compile_let_flat_tail(expr);

        case E_AND:
            return self.compile_tail_and_flat(expr);

        case E_OR:
            return self.compile_tail_or_flat(expr);

        default:
            // Non-tail forms: just use compile_to_temp
            return self.compile_to_temp(expr);
    }
}
