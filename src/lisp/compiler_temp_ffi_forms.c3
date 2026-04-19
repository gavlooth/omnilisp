module lisp;

import std::io;
import main;

fn bool compiler_ffi_type_to_tag_int(Compiler* self, TypeAnnotation* ann, Expr* expr,
    char[] role, int* out_tag, FfiHandlePolicy* out_policy) {
    FfiTypeTag ffi_tag;
    FfiHandlePolicy policy = ffi_handle_policy_default();
    if (ffi_type_annotation_to_tag(ann, self.interp, &ffi_tag, &policy)) {
        *out_tag = (int)ffi_tag;
        *out_policy = policy;
        return true;
    }

    char[256] detail_buf;
    if (ann != null && ann.is_dict) {
        if (try detail_text = io::bprintf(&detail_buf,
            "[ffi λ] unsupported %s type annotation metadata dictionary in AOT lowering",
            (ZString)role)) {
            self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column, detail_text);
        } else {
            self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
                "[ffi λ] unsupported type annotation in AOT lowering");
        }
    } else {
        char[] type_name = ann != null ? self.interp.symbols.get_name(ann.base_type) : "unknown";
        if (try detail_text = io::bprintf(&detail_buf,
            "[ffi λ] unsupported %s type annotation '^%s' in AOT lowering",
            (ZString)role, (ZString)type_name)) {
            self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column, detail_text);
        } else {
            self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
                "[ffi λ] unsupported type annotation in AOT lowering");
        }
    }
    return false;
}

fn bool compiler_ffi_policy_needs_aot_spec(FfiHandlePolicy* policy) @inline {
    if (policy == null || !policy.is_foreign_handle) return false;
    return policy.name != INVALID_SYMBOL_ID
        || policy.ownership != FFI_HANDLE_BORROWED
        || policy.nullability != FFI_HANDLE_NULLABLE
        || policy.finalizer_symbol != INVALID_SYMBOL_ID;
}

fn void Compiler.emit_ffi_policy_spec_init(Compiler* self, char[] target, FfiHandlePolicy* policy) {
    FfiHandlePolicy default_policy = ffi_handle_policy_default();
    if (policy == null) policy = &default_policy;

    self.emit_target_assignment_prefix(target, "is_foreign_handle");
    self.emit_bool_literal(policy.is_foreign_handle);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "name");
    if (policy.name != INVALID_SYMBOL_ID) {
        self.emit_string_literal(self.interp.symbols.get_name(policy.name));
    } else {
        self.emit_string_literal("");
    }
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "ownership");
    self.emit_int((long)policy.ownership);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "nullability");
    self.emit_int((long)policy.nullability);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "finalizer");
    if (policy.finalizer_symbol != INVALID_SYMBOL_ID) {
        self.emit_string_literal(self.interp.symbols.get_name(policy.finalizer_symbol));
    } else {
        self.emit_string_literal("");
    }
    self.emit(";\n");
}

fn void Compiler.emit_ffi_lib_call(Compiler* self, Expr* expr, bool explicit_policy) {
    ExprFfiLib* fl = &expr.ffi_lib;
    char[] lib_name = self.interp.symbols.get_name(fl.name);
    char[] soname = fl.path_expr.lit.value.str_chars[:fl.path_expr.lit.value.str_len];

    self.emit("aot::ffi_declare_lib(");
    self.emit_string_literal(lib_name);
    self.emit(", ");
    self.emit_string_literal(soname);
    if (explicit_policy) {
        self.emit(", true, true");
    }
    self.emit(")");
}

fn bool Compiler.prepare_ffi_fn_call(
    Compiler* self,
    Expr* expr,
    int** out_param_tags,
    FfiHandlePolicy** out_param_policies,
    int* out_return_tag,
    FfiHandlePolicy* out_return_policy,
    char[]* out_lib_name,
    char[]* out_fn_name,
    char[]* out_c_symbol,
    usz* out_param_count
) {
    ExprFfiFn* ff = expr.ffi_fn;
    *out_param_tags = null;
    *out_param_policies = null;
    *out_return_tag = 0;
    *out_return_policy = ffi_handle_policy_default();
    *out_lib_name = self.interp.symbols.get_name(ff.lib_name);
    *out_fn_name = self.interp.symbols.get_name(ff.fn_name);
    *out_c_symbol = ff.c_name[:ff.c_name_len];
    *out_param_count = ff.param_count;

    int* param_tags = null;
    FfiHandlePolicy* param_policies = null;
    if (ff.param_count > 0) {
        if (ff.param_count > usz.max / int.sizeof) {
            self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
                "[ffi λ] parameter ABI table size overflow in AOT lowering");
            return false;
        }
        param_tags = (int*)mem::malloc(int.sizeof * ff.param_count);
        if (param_tags == null) {
            self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
                "[ffi λ] out of memory while preparing parameter ABI tags in AOT lowering");
            return false;
        }
        if (ff.param_count > usz.max / FfiHandlePolicy.sizeof) {
            mem::free(param_tags);
            self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
                "[ffi λ] parameter handle policy table size overflow in AOT lowering");
            return false;
        }
        param_policies = (FfiHandlePolicy*)mem::malloc(FfiHandlePolicy.sizeof * ff.param_count);
        if (param_policies == null) {
            mem::free(param_tags);
            self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
                "[ffi λ] out of memory while preparing parameter handle policies in AOT lowering");
            return false;
        }
        for (usz i = 0; i < ff.param_count; i++) {
            int param_tag = 0;
            if (!compiler_ffi_type_to_tag_int(self, &ff.param_types[i], expr, "parameter", &param_tag, &param_policies[i])) {
                mem::free(param_tags);
                mem::free(param_policies);
                return false;
            }
            if (param_policies[i].ownership == FFI_HANDLE_OWNED) {
                mem::free(param_tags);
                mem::free(param_policies);
                self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
                    "[ffi λ] owned ForeignHandle parameter policy is not supported in AOT lowering");
                return false;
            }
            param_tags[i] = param_tag;
        }
    }

    int return_tag = 0; // FFI_TYPE_VOID
    FfiHandlePolicy return_policy = ffi_handle_policy_default();
    if (ff.has_return_type
        && !compiler_ffi_type_to_tag_int(self, &ff.return_type, expr, "return", &return_tag, &return_policy)) {
        if (param_tags != null) mem::free(param_tags);
        if (param_policies != null) mem::free(param_policies);
        return false;
    }

    *out_param_tags = param_tags;
    *out_param_policies = param_policies;
    *out_return_tag = return_tag;
    *out_return_policy = return_policy;
    return true;
}

fn bool compiler_ffi_prepared_needs_aot_policy_specs(
    FfiHandlePolicy* param_policies,
    usz param_count,
    FfiHandlePolicy* return_policy
) {
    for (usz i = 0; i < param_count; i++) {
        if (compiler_ffi_policy_needs_aot_spec(&param_policies[i])) return true;
    }
    return compiler_ffi_policy_needs_aot_spec(return_policy);
}

fn void Compiler.emit_ffi_fn_support_decls(
    Compiler* self,
    int* param_tags,
    FfiHandlePolicy* param_policies,
    usz param_count,
    FfiHandlePolicy* return_policy,
    char[] tags_name,
    char[] param_policies_name,
    char[] return_policy_name,
    bool emit_policy_specs
) {
    if (param_count > 0) {
        self.emit_indent();
        self.emit("int[");
        self.emit_usz(param_count);
        self.emit("] ");
        self.emit(tags_name);
        self.emit(";\n");
        for (usz i = 0; i < param_count; i++) {
            self.emit_indent();
            self.emit(tags_name);
            self.emit("[");
            self.emit_usz(i);
            self.emit("] = ");
            self.emit_int((long)param_tags[i]);
            self.emit(";\n");
        }
    }

    if (emit_policy_specs && param_count > 0) {
        self.emit_indent();
        self.emit("lisp::aot::AotFfiHandlePolicySpec[");
        self.emit_usz(param_count);
        self.emit("] ");
        self.emit(param_policies_name);
        self.emit(";\n");
        for (usz i = 0; i < param_count; i++) {
            char[96] policy_buf;
            char[] entry = compiler_format_generated_name_index(policy_buf[..], param_policies_name, (long)i);
            self.emit_ffi_policy_spec_init(entry, &param_policies[i]);
        }
    }

    if (emit_policy_specs) {
        self.emit_indent();
        self.emit("lisp::aot::AotFfiHandlePolicySpec ");
        self.emit(return_policy_name);
        self.emit(";\n");
        self.emit_ffi_policy_spec_init(return_policy_name, return_policy);
    }
}

fn void Compiler.emit_ffi_fn_call_expr_prepared(
    Compiler* self,
    char[] lib_name,
    char[] fn_name,
    char[] c_symbol,
    char[] tags_name,
    usz param_count,
    bool has_return_type,
    int return_tag,
    bool explicit_policy,
    char[] param_policies_name,
    char[] return_policy_name,
    bool emit_policy_specs
) {
    self.emit("aot::ffi_declare_fn(");
    self.emit_string_literal(lib_name);
    self.emit(", ");
    self.emit_string_literal(fn_name);
    self.emit(", ");
    self.emit_string_literal(c_symbol);
    self.emit(", ");
    if (param_count > 0) {
        self.emit("&");
        self.emit(tags_name);
        self.emit("[0]");
    } else {
        self.emit("null");
    }
    self.emit(", ");
    self.emit_usz(param_count);
    self.emit(", ");
    self.emit(has_return_type ? "true" : "false");
    self.emit(", ");
    self.emit_int((long)return_tag);
    if (explicit_policy) {
        self.emit(", true, true");
    }
    if (emit_policy_specs) {
        if (!explicit_policy) self.emit(", true, true");
        self.emit(", ");
        if (param_count > 0) {
            self.emit("&");
            self.emit(param_policies_name);
            self.emit("[0]");
        } else {
            self.emit("null");
        }
        self.emit(", &");
        self.emit(return_policy_name);
    }
    self.emit(")");
}

fn usz Compiler.compile_ffi_lib_direct(Compiler* self, Expr* expr) {
    ExprFfiLib* fl = &expr.ffi_lib;
    if (fl.path_expr == null
        || fl.path_expr.tag != E_LIT
        || fl.path_expr.lit.value.tag != STRING) {
        self.set_compile_error_formatted(
            "compiler",
            expr.loc_line,
            expr.loc_column,
            "[ffi lib] currently requires a string literal soname in AOT lowering"
        );
        return self.emit_nil_temp();
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = ");
    self.emit_ffi_lib_call(expr, false);
    self.emit(";\n");
    return id;
}

fn usz Compiler.compile_ffi_fn_direct(Compiler* self, Expr* expr) {
    ExprFfiFn* ff = expr.ffi_fn;
    if (ff == null) {
        self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
            "[ffi λ] missing function descriptor in AOT lowering");
        return self.emit_nil_temp();
    }
    if (ff.is_variadic) {
        self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
            "[ffi λ variadic] is not supported by AOT lowering");
        return self.emit_nil_temp();
    }
    if (ff.c_name == null || ff.c_name_len == 0) {
        self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
            "[ffi λ] missing C symbol name in AOT lowering");
        return self.emit_nil_temp();
    }
    if (ff.param_count > (usz)int.max) {
        self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column,
            "[ffi λ] parameter count exceeds AOT arity limit");
        return self.emit_nil_temp();
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
        return self.emit_nil_temp();
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
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = ");
    self.emit_ffi_fn_call_expr_prepared(lib_name, fn_name, c_symbol, tags_name, param_count,
        ff.has_return_type, return_tag, false, param_policies_name, return_policy_name, emit_policy_specs);
    if (param_tags != null) mem::free(param_tags);
    if (param_policies != null) mem::free(param_policies);
    self.emit(";\n");
    return id;
}
