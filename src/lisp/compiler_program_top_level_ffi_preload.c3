module lisp;

import std::collections::list;
import main;

fn bool Compiler.has_ffi_preload_expr(Compiler* self, Expr* expr) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_FFI_LIB:
        case E_FFI_FN:
            return true;
        case E_LAMBDA:
            return expr.lambda != null && self.has_ffi_preload_expr(expr.lambda.body);
        case E_APP:
            return self.has_ffi_preload_expr(expr.app.func) || self.has_ffi_preload_expr(expr.app.arg);
        case E_IF:
            return self.has_ffi_preload_expr(expr.if_expr.test)
                || self.has_ffi_preload_expr(expr.if_expr.then_branch)
                || self.has_ffi_preload_expr(expr.if_expr.else_branch);
        case E_LET:
            return self.has_ffi_preload_expr(expr.let_expr.init)
                || self.has_ffi_preload_expr(expr.let_expr.body);
        case E_RESET:
            return self.has_ffi_preload_expr(expr.reset.body);
        case E_SHIFT:
            return self.has_ffi_preload_expr(expr.shift.body);
        case E_PERFORM:
            return (expr.perform.has_tag_expr && self.has_ffi_preload_expr(expr.perform.tag_expr))
                || self.has_ffi_preload_expr(expr.perform.arg);
        case E_RESOLVE:
            return self.has_ffi_preload_expr(expr.resolve.value);
        case E_HANDLE:
            if (expr.handle == null) return false;
            if (self.has_ffi_preload_expr(expr.handle.body)) return true;
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                if (expr.handle.clauses[i].has_effect_tag_expr &&
                    self.has_ffi_preload_expr(expr.handle.clauses[i].effect_tag_expr)) return true;
                if (self.has_ffi_preload_expr(expr.handle.clauses[i].handler_body)) return true;
            }
            return false;
        case E_INDEX:
            return self.has_ffi_preload_expr(expr.index.collection)
                || self.has_ffi_preload_expr(expr.index.index);
        case E_MATCH:
            if (expr.match == null) return false;
            if (self.has_ffi_preload_expr(expr.match.scrutinee)) return true;
            for (usz i = 0; i < expr.match.clause_count; i++) {
                if (self.has_ffi_preload_expr(expr.match.clauses[i].result)) return true;
            }
            return false;
        case E_AND:
            return self.has_ffi_preload_expr(expr.and_expr.left)
                || self.has_ffi_preload_expr(expr.and_expr.right);
        case E_OR:
            return self.has_ffi_preload_expr(expr.or_expr.left)
                || self.has_ffi_preload_expr(expr.or_expr.right);
        case E_CALL:
            if (expr.call == null) return false;
            if (self.has_ffi_preload_expr(expr.call.func)) return true;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                if (self.has_ffi_preload_expr(expr.call.args[i])) return true;
            }
            return false;
        case E_MODULE:
            if (expr.module_expr == null) return false;
            for (usz i = 0; i < expr.module_expr.body_count; i++) {
                if (self.has_ffi_preload_expr(expr.module_expr.body[i])) return true;
            }
            return false;
        case E_WITH_MODULE:
            if (expr.with_module == null) return false;
            for (usz i = 0; i < expr.with_module.body_count; i++) {
                if (self.has_ffi_preload_expr(expr.with_module.body[i])) return true;
            }
            return false;
        case E_BLOCK:
            if (expr.block == null) return false;
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (self.has_ffi_preload_expr(expr.block.exprs[i])) return true;
            }
            return false;
        case E_SET:
            return self.has_ffi_preload_expr(expr.set_expr.value);
        case E_DEFINE:
            return self.has_ffi_preload_expr(expr.define.value);
        default:
            return false;
    }
}

fn void Compiler.emit_ffi_preload_result_guard(Compiler* self, usz result_id) {
    self.emit_indent();
    self.emit("if (_r");
    self.emit_usz(result_id);
    self.emit(" != null && _r");
    self.emit_usz(result_id);
    self.emit(".tag == lisp::ValueTag.ERROR) { aot::aot_process_shutdown(); return 1; }\n");
}

fn void Compiler.emit_ffi_preload_expr_phase(Compiler* self, Expr* expr, bool libs_phase) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_LAMBDA:
            if (expr.lambda != null) self.emit_ffi_preload_expr_phase(expr.lambda.body, libs_phase);
            return;
        case E_APP:
            self.emit_ffi_preload_expr_phase(expr.app.func, libs_phase);
            self.emit_ffi_preload_expr_phase(expr.app.arg, libs_phase);
            return;
        case E_IF:
            self.emit_ffi_preload_expr_phase(expr.if_expr.test, libs_phase);
            self.emit_ffi_preload_expr_phase(expr.if_expr.then_branch, libs_phase);
            self.emit_ffi_preload_expr_phase(expr.if_expr.else_branch, libs_phase);
            return;
        case E_LET:
            self.emit_ffi_preload_expr_phase(expr.let_expr.init, libs_phase);
            self.emit_ffi_preload_expr_phase(expr.let_expr.body, libs_phase);
            return;
        case E_RESET:
            self.emit_ffi_preload_expr_phase(expr.reset.body, libs_phase);
            return;
        case E_SHIFT:
            self.emit_ffi_preload_expr_phase(expr.shift.body, libs_phase);
            return;
        case E_PERFORM:
            if (expr.perform.has_tag_expr) self.emit_ffi_preload_expr_phase(expr.perform.tag_expr, libs_phase);
            self.emit_ffi_preload_expr_phase(expr.perform.arg, libs_phase);
            return;
        case E_RESOLVE:
            self.emit_ffi_preload_expr_phase(expr.resolve.value, libs_phase);
            return;
        case E_HANDLE:
            if (expr.handle != null) {
                self.emit_ffi_preload_expr_phase(expr.handle.body, libs_phase);
                for (usz i = 0; i < expr.handle.clause_count; i++) {
                    if (expr.handle.clauses[i].has_effect_tag_expr) {
                        self.emit_ffi_preload_expr_phase(expr.handle.clauses[i].effect_tag_expr, libs_phase);
                    }
                    self.emit_ffi_preload_expr_phase(expr.handle.clauses[i].handler_body, libs_phase);
                }
            }
            return;
        case E_INDEX:
            self.emit_ffi_preload_expr_phase(expr.index.collection, libs_phase);
            self.emit_ffi_preload_expr_phase(expr.index.index, libs_phase);
            return;
        case E_MATCH:
            if (expr.match != null) {
                self.emit_ffi_preload_expr_phase(expr.match.scrutinee, libs_phase);
                for (usz i = 0; i < expr.match.clause_count; i++) {
                    self.emit_ffi_preload_expr_phase(expr.match.clauses[i].result, libs_phase);
                }
            }
            return;
        case E_AND:
            self.emit_ffi_preload_expr_phase(expr.and_expr.left, libs_phase);
            self.emit_ffi_preload_expr_phase(expr.and_expr.right, libs_phase);
            return;
        case E_OR:
            self.emit_ffi_preload_expr_phase(expr.or_expr.left, libs_phase);
            self.emit_ffi_preload_expr_phase(expr.or_expr.right, libs_phase);
            return;
        case E_CALL:
            if (expr.call != null) {
                self.emit_ffi_preload_expr_phase(expr.call.func, libs_phase);
                for (usz i = 0; i < expr.call.arg_count; i++) {
                    self.emit_ffi_preload_expr_phase(expr.call.args[i], libs_phase);
                }
            }
            return;
        case E_BLOCK:
            if (expr.block != null) {
                for (usz i = 0; i < expr.block.expr_count; i++) {
                    self.emit_ffi_preload_expr_phase(expr.block.exprs[i], libs_phase);
                }
            }
            return;
        case E_SET:
            self.emit_ffi_preload_expr_phase(expr.set_expr.value, libs_phase);
            return;
        case E_DEFINE:
            self.emit_ffi_preload_expr_phase(expr.define.value, libs_phase);
            break;
        case E_MODULE:
            if (expr.module_expr != null) {
                for (usz i = 0; i < expr.module_expr.body_count; i++) {
                    self.emit_ffi_preload_expr_phase(expr.module_expr.body[i], libs_phase);
                }
            }
            return;
        case E_WITH_MODULE:
            if (expr.with_module != null) {
                for (usz i = 0; i < expr.with_module.body_count; i++) {
                    self.emit_ffi_preload_expr_phase(expr.with_module.body[i], libs_phase);
                }
            }
            return;
        default:
    }

    if (libs_phase) {
        if (expr.tag != E_FFI_LIB || expr.ffi_lib.path_expr == null
            || expr.ffi_lib.path_expr.tag != E_LIT
            || expr.ffi_lib.path_expr.lit.value.tag != STRING) {
            return;
        }
        usz result_id = self.next_result();
        self.emit_temp_decl(result_id);
        self.emit(" = ");
        self.emit_ffi_lib_call(expr, true);
        self.emit(";\n");
        self.emit_ffi_preload_result_guard(result_id);
        return;
    }

    if (expr.tag != E_FFI_FN || expr.ffi_fn == null || expr.ffi_fn.is_variadic
        || expr.ffi_fn.c_name == null || expr.ffi_fn.c_name_len == 0) {
        return;
    }

    int* param_tags = null;
    FfiHandlePolicy* param_policies = null;
    int return_tag = 0;
    FfiHandlePolicy return_policy = ffi_handle_policy_default();
    char[] lib_name = "";
    char[] fn_name = "";
    char[] c_symbol = "";
    usz param_count = 0;
    if (!self.prepare_ffi_fn_call(expr, &param_tags, &param_policies, &return_tag, &return_policy,
        &lib_name, &fn_name, &c_symbol, &param_count)) {
        if (param_tags != null) mem::free(param_tags);
        if (param_policies != null) mem::free(param_policies);
        return;
    }
    usz tags_id = self.next_result();
    char[64] tags_name_buf;
    char[] tags_name = compiler_format_generated_name_id(tags_name_buf[..], "_ffi_param_tags_", (long)tags_id);
    usz policies_id = self.next_result();
    char[64] param_policies_name_buf;
    char[] param_policies_name = compiler_format_generated_name_id(param_policies_name_buf[..], "_ffi_param_policies_", (long)policies_id);
    char[64] return_policy_name_buf;
    char[] return_policy_name = compiler_format_generated_name_id(return_policy_name_buf[..], "_ffi_return_policy_", (long)policies_id);
    bool emit_policy_specs = compiler_ffi_prepared_needs_aot_policy_specs(param_policies, param_count, &return_policy);

    self.emit_ffi_fn_support_decls(param_tags, param_policies, param_count, &return_policy,
        tags_name, param_policies_name, return_policy_name, emit_policy_specs);
    usz result_id = self.next_result();
    self.emit_temp_decl(result_id);
    self.emit(" = ");
    self.emit_ffi_fn_call_expr_prepared(lib_name, fn_name, c_symbol, tags_name, param_count,
        expr.ffi_fn.has_return_type, return_tag, true, param_policies_name, return_policy_name, emit_policy_specs);
    self.emit(";\n");
    self.emit_ffi_preload_result_guard(result_id);
    if (param_tags != null) mem::free(param_tags);
    if (param_policies != null) mem::free(param_policies);
}

fn void Compiler.emit_ffi_startup_preload(Compiler* self, List{Expr*}* exprs) {
    if (exprs == null || exprs.len() == 0) return;
    bool has_ffi = false;
    foreach (expr : *exprs) {
        if (self.has_ffi_preload_expr(expr)) {
            has_ffi = true;
            break;
        }
    }
    if (!has_ffi) return;

    self.emit_line("// Startup FFI preload");
    for (usz i = 0; i < exprs.len(); i++) {
        self.emit_ffi_preload_expr_phase((*exprs)[i], true);
    }
    for (usz i = 0; i < exprs.len(); i++) {
        self.emit_ffi_preload_expr_phase((*exprs)[i], false);
    }
    self.emit_newline();
}
