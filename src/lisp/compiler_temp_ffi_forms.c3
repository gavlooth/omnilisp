module lisp;

import std::io;
import main;

fn bool compiler_ffi_type_to_tag_int(Compiler* self, SymbolId base_type, Expr* expr, char[] role, int* out_tag) {
    FfiTypeTag ffi_tag;
    if (type_ann_to_ffi_tag(base_type, self.interp, &ffi_tag)) {
        *out_tag = (int)ffi_tag;
        return true;
    }

    char[256] detail_buf;
    char[] type_name = self.interp.symbols.get_name(base_type);
    char[] detail = io::bprintf(&detail_buf,
        "[ffi λ] unsupported %s type annotation '^%s' in AOT lowering",
        (ZString)role, (ZString)type_name)!!;
    self.set_compile_error_formatted("compiler", expr.loc_line, expr.loc_column, detail);
    return false;
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
    int* out_return_tag,
    char[]* out_lib_name,
    char[]* out_fn_name,
    char[]* out_c_symbol,
    usz* out_param_count
) {
    ExprFfiFn* ff = expr.ffi_fn;
    *out_param_tags = null;
    *out_return_tag = 0;
    *out_lib_name = self.interp.symbols.get_name(ff.lib_name);
    *out_fn_name = self.interp.symbols.get_name(ff.fn_name);
    *out_c_symbol = ff.c_name[:ff.c_name_len];
    *out_param_count = ff.param_count;

    int* param_tags = null;
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
        for (usz i = 0; i < ff.param_count; i++) {
            int param_tag = 0;
            if (!compiler_ffi_type_to_tag_int(self, ff.param_types[i].base_type, expr, "parameter", &param_tag)) {
                mem::free(param_tags);
                return false;
            }
            param_tags[i] = param_tag;
        }
    }

    int return_tag = 0; // FFI_TYPE_VOID
    if (ff.has_return_type
        && !compiler_ffi_type_to_tag_int(self, ff.return_type.base_type, expr, "return", &return_tag)) {
        if (param_tags != null) mem::free(param_tags);
        return false;
    }

    *out_param_tags = param_tags;
    *out_return_tag = return_tag;
    return true;
}

fn void Compiler.emit_ffi_fn_call_prepared(
    Compiler* self,
    char[] lib_name,
    char[] fn_name,
    char[] c_symbol,
    int* param_tags,
    usz param_count,
    bool has_return_type,
    int return_tag,
    bool explicit_policy
) {
    usz tags_id = self.next_result();
    char[64] tags_name_buf;
    char[] tags_name = io::bprintf(&tags_name_buf, "_ffi_param_tags_%d", (int)tags_id)!!;

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
    int return_tag = 0;
    char[] lib_name = "";
    char[] fn_name = "";
    char[] c_symbol = "";
    usz param_count = 0;
    if (!self.prepare_ffi_fn_call(expr, &param_tags, &return_tag, &lib_name, &fn_name, &c_symbol, &param_count)) {
        if (param_tags != null) mem::free(param_tags);
        return self.emit_nil_temp();
    }
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = ");
    self.emit_ffi_fn_call_prepared(lib_name, fn_name, c_symbol, param_tags, param_count, ff.has_return_type, return_tag, false);
    if (param_tags != null) mem::free(param_tags);
    self.emit(";\n");
    return id;
}
