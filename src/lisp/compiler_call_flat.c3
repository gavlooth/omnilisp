module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (call/app forms)
// =============================================================================
// --- Flat (statement-level) compilation of individual forms ---

fn usz Compiler.compile_when_unless_if(Compiler* self, Expr* expr, bool when_form, bool tail) {
    usz arg_count = expr.call.arg_count;
    if (arg_count < 1) return self.emit_nil_temp();

    Expr* branch = null;
    usz branch_count = arg_count - 1;
    if (branch_count == 0) {
        branch = make_nil_expr(self.interp);
    } else if (branch_count == 1) {
        branch = expr.call.args[1];
    } else {
        branch = (Expr*)ast_arena_alloc(&self.interp.ast_arena, Expr.sizeof);
        if (branch == null) {
            self.set_compile_error("compiler: out of memory while allocating when/unless synthetic block expression");
            return usz.max;
        }
        branch.tag = E_BLOCK;
        branch.block = (ExprBlock*)ast_arena_alloc(&self.interp.ast_arena, ExprBlock.sizeof);
        if (branch.block == null) {
            self.set_compile_error("compiler: out of memory while allocating when/unless synthetic block payload");
            return usz.max;
        }
        branch.block.expr_count = branch_count;
        usz exprs_size = 0;
        if (!compiler_checked_mul_usz(Expr*.sizeof, branch_count, &exprs_size)) {
            self.set_compile_error("compiler: when/unless synthetic block expression array size overflow");
            return usz.max;
        }
        branch.block.exprs = (Expr**)ast_arena_alloc(&self.interp.ast_arena, exprs_size);
        if (branch.block.exprs == null) {
            self.set_compile_error("compiler: out of memory while allocating when/unless synthetic block expressions");
            return usz.max;
        }
        for (usz i = 0; i < branch_count; i++) {
            branch.block.exprs[i] = expr.call.args[i + 1];
        }
    }

    Expr if_expr;
    if_expr.tag = E_IF;
    if_expr.if_expr.test = expr.call.args[0];
    if_expr.if_expr.then_branch = when_form ? branch : make_nil_expr(self.interp);
    if_expr.if_expr.else_branch = when_form ? make_nil_expr(self.interp) : branch;

    if (tail) return self.compile_if_common(&if_expr, true);
    return self.compile_if_common(&if_expr, false);
}

fn usz Compiler.compile_call_flat(Compiler* self, Expr* expr) {
    usz arg_count = expr.call.arg_count;

    if (self.call_is_explain_thunked(expr)) {
        usz explain_r = self.compile_explain_flat(expr);
        if (explain_r != usz.max) return explain_r;
        if (self.has_error) return usz.max;
    }

    if (arg_count >= 1) {
        if (self.call_head_is(expr, "when")) return self.compile_when_unless_if(expr, true, false);
        if (self.call_head_is(expr, "unless")) return self.compile_when_unless_if(expr, false, false);
    }

    if (arg_count == 0) {
        usz func_r = self.compile_to_temp(expr.call.func);
        if (self.has_error || func_r == usz.max) return usz.max;
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::apply_multi(");
        self.emit_temp_ref(func_r);
        self.emit(", aot::make_nil(), 0);\n");
        return id;
    }

    usz[] arg_temps = {};
    if (!self.compile_call_arg_temps(expr.call.args, arg_count, &arg_temps)) return usz.max;
    defer mem::free(arg_temps.ptr);

    usz func_r = self.compile_to_temp(expr.call.func);
    if (self.has_error || func_r == usz.max) return usz.max;
    usz args_list = self.build_arg_list_from_temps(arg_temps, arg_count);
    if (args_list == usz.max) return usz.max;

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

    if (self.call_is_explain_thunked(expr)) {
        usz explain_r = self.compile_explain_flat(expr);
        if (explain_r != usz.max) return explain_r;
        if (self.has_error) return usz.max;
    }

    // Special cases: list/dict build data structures, delegate to non-tail version
    if (self.tail_call_requires_non_tail_path(expr)) return self.compile_call_flat(expr);

    if (self.call_head_is(expr, "when")) return self.compile_when_unless_if(expr, true, true);
    if (self.call_head_is(expr, "unless")) return self.compile_when_unless_if(expr, false, true);

    if (arg_count == 0) return self.compile_zero_arg_tail_call(expr);

    usz[] arg_temps = {};
    if (!self.compile_call_arg_temps(expr.call.args, arg_count, &arg_temps)) return usz.max;
    defer mem::free(arg_temps.ptr);

    usz func_r = self.compile_to_temp(expr.call.func);
    if (self.has_error || func_r == usz.max) return usz.max;
    usz args_list = self.build_arg_list_from_temps(arg_temps, arg_count);
    if (args_list == usz.max) return usz.max;

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
    if (self.has_error || func_r == usz.max) return usz.max;
    usz arg_r = self.compile_to_temp(expr.app.arg);
    if (self.has_error || arg_r == usz.max) return usz.max;
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
    if (self.has_error || func_r == usz.max) return usz.max;
    usz arg_r = self.compile_to_temp(expr.app.arg);
    if (self.has_error || arg_r == usz.max) return usz.max;
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::invoke_tail(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}
