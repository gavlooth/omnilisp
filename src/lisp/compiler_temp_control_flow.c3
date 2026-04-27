module lisp;

fn void Compiler.emit_temp_assign(Compiler* self, usz dst_id, usz src_id) {
    self.emit_indent();
    self.emit_temp_ref(dst_id);
    self.emit(" = ");
    self.emit_temp_ref(src_id);
    self.emit(";\n");
}

fn void Compiler.emit_return_if_sequence_should_stop(Compiler* self, usz value_id) {
    if (!self.emits_value_return) return;
    self.emit_indent();
    self.emit("if (aot::sequence_should_stop(");
    self.emit_temp_ref(value_id);
    self.emit(")) return ");
    self.emit_temp_ref(value_id);
    self.emit(";\n");
}

fn void Compiler.emit_return_if_error_or_sequence_should_stop(Compiler* self, usz value_id) {
    if (!self.emits_value_return) return;
    self.emit_indent();
    self.emit("if (aot::is_error(");
    self.emit_temp_ref(value_id);
    self.emit(") || aot::sequence_should_stop(");
    self.emit_temp_ref(value_id);
    self.emit(")) return ");
    self.emit_temp_ref(value_id);
    self.emit(";\n");
}

fn void Compiler.compile_branch_assign(Compiler* self, usz dst_id, Expr* branch, bool tail) {
    usz declared_mark = self.mark_declared_scope();
    usz branch_r;
    if (tail) {
        branch_r = self.compile_to_temp_tail(branch);
    } else {
        branch_r = self.compile_to_temp(branch);
    }
    self.emit_temp_assign(dst_id, branch_r);
    self.restore_declared_scope(declared_mark);
}

fn void Compiler.compile_tail_branch_assign(Compiler* self, usz dst_id, Expr* branch) {
    self.compile_branch_assign(dst_id, branch, true);
}

fn usz Compiler.compile_block_child_to_temp(Compiler* self, Expr* expr) {
    usz result = self.compile_to_temp(expr);
    return result;
}

fn void Compiler.compile_block_prefix(Compiler* self, Expr** exprs, usz count) {
    for (usz i = 0; i < count; i++) {
        self.compile_block_child_to_temp(exprs[i]);
    }
}

fn usz Compiler.compile_sequence_common(Compiler* self, Expr** exprs, usz count, bool tail) {
    if (count == 0) return self.emit_nil_temp();

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");

    usz open_guards = 0;
    for (usz i = 0; i + 1 < count; i++) {
        usz child_r = self.compile_block_child_to_temp(exprs[i]);
        self.emit_indent();
        self.emit("if (aot::is_error(");
        self.emit_temp_ref(child_r);
        self.emit(") || aot::sequence_should_stop(");
        self.emit_temp_ref(child_r);
        self.emit(")) {\n");
        self.indent++;
        self.emit_temp_assign(id, child_r);
        self.indent--;
        self.emit_indent();
        self.emit("} else {\n");
        self.indent++;
        open_guards++;
    }

    usz last_r = tail
        ? self.compile_to_temp_tail(exprs[count - 1])
        : self.compile_block_child_to_temp(exprs[count - 1]);
    self.emit_temp_assign(id, last_r);

    for (usz i = 0; i < open_guards; i++) {
        self.indent--;
        self.emit_indent();
        self.emit("}\n");
    }

    return id;
}

fn usz Compiler.compile_short_circuit(Compiler* self, usz left_r, Expr* right_expr, bool left_when_truthy, bool tail) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_error(");
    self.emit_temp_ref(left_r);
    self.emit(") || aot::sequence_should_stop(");
    self.emit_temp_ref(left_r);
    self.emit(")) {\n");
    self.indent++;
    self.emit_temp_assign(id, left_r);
    self.indent--;
    self.emit_indent();
    self.emit("} else if (aot::is_truthy(");
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
    usz condition_tmp = self.compile_to_temp(expr.if_expr.test);
    self.emit_return_if_error_or_sequence_should_stop(condition_tmp);
    if (compiler_expr_is_literal_false(self, expr.if_expr.test)) {
        if (tail) return self.compile_to_temp_tail(expr.if_expr.else_branch);
        return self.compile_to_temp(expr.if_expr.else_branch);
    }
    if (compiler_expr_is_literal_truthy(self, expr.if_expr.test)) {
        if (tail) return self.compile_to_temp_tail(expr.if_expr.then_branch);
        return self.compile_to_temp(expr.if_expr.then_branch);
    }
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_error(");
    self.emit_temp_ref(condition_tmp);
    self.emit(") || aot::sequence_should_stop(");
    self.emit_temp_ref(condition_tmp);
    self.emit(")) {\n");
    self.indent++;
    self.emit_temp_assign(id, condition_tmp);
    self.indent--;
    self.emit_indent();
    self.emit("} else if (aot::is_truthy(");
    self.emit_temp_ref(condition_tmp);
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

fn usz Compiler.compile_block_common(Compiler* self, Expr* expr, bool tail) {
    usz count = expr.block.expr_count;
    return self.compile_sequence_common(expr.block.exprs, count, tail);
}

fn usz Compiler.compile_tail_if_flat(Compiler* self, Expr* expr) {
    return self.compile_if_common(expr, true);
}

fn usz Compiler.compile_tail_block_flat(Compiler* self, Expr* expr) {
    return self.compile_block_common(expr, true);
}

fn usz Compiler.compile_tail_and_flat(Compiler* self, Expr* expr) {
    usz and_left = self.compile_to_temp(expr.and_expr.left);
    return self.compile_tail_short_circuit(and_left, expr.and_expr.right, false);
}

fn usz Compiler.compile_tail_or_flat(Compiler* self, Expr* expr) {
    usz or_left = self.compile_to_temp(expr.or_expr.left);
    return self.compile_tail_short_circuit(or_left, expr.or_expr.right, true);
}
