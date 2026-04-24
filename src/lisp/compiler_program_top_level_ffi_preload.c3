module lisp;

import std::collections::list;
import main;

fn bool Compiler.has_ffi_preload_expr(Compiler* self, Expr* expr) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_FFI_LIB:
        case E_FFI_FN:
            return true;
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
        default:
            return false;
    }
}

fn void Compiler.emit_ffi_preload_expr_phase(Compiler* self, Expr* expr, bool libs_phase) {
    if (expr == null) return;
    if (expr.tag == E_BLOCK && expr.block != null) {
        for (usz i = 0; i < expr.block.expr_count; i++) {
            self.emit_ffi_preload_expr_phase(expr.block.exprs[i], libs_phase);
        }
        return;
    }
    if (expr.tag == E_MODULE && expr.module_expr != null) {
        for (usz i = 0; i < expr.module_expr.body_count; i++) {
            self.emit_ffi_preload_expr_phase(expr.module_expr.body[i], libs_phase);
        }
        return;
    }

    if (libs_phase) {
        if (expr.tag != E_FFI_LIB || expr.ffi_lib.path_expr == null
            || expr.ffi_lib.path_expr.tag != E_LIT
            || expr.ffi_lib.path_expr.lit.value.tag != STRING) {
            return;
        }
        self.emit_indent();
        self.emit_ffi_lib_call(expr, true);
        self.emit(";\n");
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
    self.emit_indent();
    self.emit_ffi_fn_call_expr_prepared(lib_name, fn_name, c_symbol, tags_name, param_count,
        expr.ffi_fn.has_return_type, return_tag, true, param_policies_name, return_policy_name, emit_policy_specs);
    self.emit(";\n");
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
