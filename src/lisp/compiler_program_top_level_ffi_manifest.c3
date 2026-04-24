module lisp;

import std::collections::list;
import main;

fn char[] ffi_manifest_type_name(FfiTypeTag tag) {
    switch (tag) {
        case FFI_TYPE_VOID:
            return "Void";
        case FFI_TYPE_INT:
            return "Integer";
        case FFI_TYPE_DOUBLE:
            return "Float64";
        case FFI_TYPE_FLOAT32:
            return "Float32";
        case FFI_TYPE_INT32:
            return "Int32";
        case FFI_TYPE_INT64:
            return "Int64";
        case FFI_TYPE_UINT32:
            return "UInt32";
        case FFI_TYPE_UINT64:
            return "UInt64";
        case FFI_TYPE_PTR:
            return "ForeignHandle";
        case FFI_TYPE_BUFFER:
            return "Buffer";
        case FFI_TYPE_STRUCT:
            return "Struct";
        case FFI_TYPE_BOOL:
            return "Boolean";
        case FFI_TYPE_STRING:
            return "String";
        default:
            return "Void";
    }
}

fn void Compiler.emit_json_string(Compiler* self, char[] s) {
    self.emit_char('"');
    foreach (c : s) {
        switch (c) {
            case '"':
                self.emit("\\\"");
            case '\\':
                self.emit("\\\\");
            case '\n':
                self.emit("\\n");
            case '\r':
                self.emit("\\r");
            case '\t':
                self.emit("\\t");
            case (char)8:
                self.emit("\\b");
            case (char)12:
                self.emit("\\f");
            case '`':
                self.emit("\\u0060");
            default:
                uint byte = (uint)c & 0xFF;
                if (byte < 0x20) {
                    char[] hex = "0123456789abcdef";
                    self.emit("\\u00");
                    self.emit_char(hex[(usz)((byte >> 4) & 0xF)]);
                    self.emit_char(hex[(usz)(byte & 0xF)]);
                } else {
                    self.emit_char(c);
                }
        }
    }
    self.emit_char('"');
}

fn void Compiler.emit_ffi_manifest_signature_json(
    Compiler* self,
    int* param_tags,
    usz param_count,
    bool has_return_type,
    int return_tag
) {
    self.emit("{\n");
    self.emit("        \"params\": [");
    for (usz i = 0; i < param_count; i++) {
        if (i > 0) self.emit(", ");
        self.emit_json_string(ffi_manifest_type_name((FfiTypeTag)param_tags[i]));
    }
    self.emit("],\n");
    self.emit("        \"return\": ");
    self.emit_json_string(ffi_manifest_type_name((FfiTypeTag)(has_return_type ? return_tag : (int)FfiTypeTag.FFI_TYPE_VOID)));
    self.emit("\n      }");
}

fn void Compiler.emit_ffi_manifest_lib_entry(Compiler* self, Expr* expr, bool* first) {
    if (expr == null || expr.tag != E_FFI_LIB) return;
    if (expr.ffi_lib.path_expr == null
        || expr.ffi_lib.path_expr.tag != E_LIT
        || expr.ffi_lib.path_expr.lit.value.tag != STRING) {
        return;
    }

    if (!*first) self.emit(",\n");
    *first = false;

    ExprFfiLib* fl = &expr.ffi_lib;
    char[] lib_name = self.interp.symbols.get_name(fl.name);
    char[] soname = fl.path_expr.lit.value.str_chars[:fl.path_expr.lit.value.str_len];

    self.emit("      {\n");
    self.emit("        \"binding\": ");
    self.emit_json_string(lib_name);
    self.emit(",\n");
    self.emit("        \"soname\": ");
    self.emit_json_string(soname);
    self.emit(",\n");
    self.emit("        \"eager_load\": true,\n");
    self.emit("        \"required_library\": true\n");
    self.emit("      }");
}

fn void Compiler.emit_ffi_manifest_fn_entry(Compiler* self, Expr* expr, bool* first) {
    if (expr == null || expr.tag != E_FFI_FN || expr.ffi_fn == null || expr.ffi_fn.is_variadic
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

    if (!*first) self.emit(",\n");
    *first = false;

    self.emit("      {\n");
    self.emit("        \"binding\": ");
    self.emit_json_string(fn_name);
    self.emit(",\n");
    self.emit("        \"library\": ");
    self.emit_json_string(lib_name);
    self.emit(",\n");
    self.emit("        \"symbol\": ");
    self.emit_json_string(c_symbol);
    self.emit(",\n");
    self.emit("        \"eager_resolve\": true,\n");
    self.emit("        \"required_library\": true,\n");
    self.emit("        \"required_symbol\": true,\n");
    self.emit("        \"signature\": ");
    self.emit_ffi_manifest_signature_json(param_tags, param_count, expr.ffi_fn.has_return_type, return_tag);
    self.emit("\n      }");

    if (param_tags != null) mem::free(param_tags);
    if (param_policies != null) mem::free(param_policies);
}

fn void Compiler.emit_ffi_manifest_lib_entries(Compiler* self, Expr* expr, bool* first) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_LAMBDA:
            if (expr.lambda != null) self.emit_ffi_manifest_lib_entries(expr.lambda.body, first);
            return;
        case E_APP:
            self.emit_ffi_manifest_lib_entries(expr.app.func, first);
            self.emit_ffi_manifest_lib_entries(expr.app.arg, first);
            return;
        case E_IF:
            self.emit_ffi_manifest_lib_entries(expr.if_expr.test, first);
            self.emit_ffi_manifest_lib_entries(expr.if_expr.then_branch, first);
            self.emit_ffi_manifest_lib_entries(expr.if_expr.else_branch, first);
            return;
        case E_LET:
            self.emit_ffi_manifest_lib_entries(expr.let_expr.init, first);
            self.emit_ffi_manifest_lib_entries(expr.let_expr.body, first);
            return;
        case E_RESET:
            self.emit_ffi_manifest_lib_entries(expr.reset.body, first);
            return;
        case E_SHIFT:
            self.emit_ffi_manifest_lib_entries(expr.shift.body, first);
            return;
        case E_PERFORM:
            if (expr.perform.has_tag_expr) self.emit_ffi_manifest_lib_entries(expr.perform.tag_expr, first);
            self.emit_ffi_manifest_lib_entries(expr.perform.arg, first);
            return;
        case E_RESOLVE:
            self.emit_ffi_manifest_lib_entries(expr.resolve.value, first);
            return;
        case E_HANDLE:
            if (expr.handle != null) {
                self.emit_ffi_manifest_lib_entries(expr.handle.body, first);
                for (usz hi = 0; hi < expr.handle.clause_count; hi++) {
                    if (expr.handle.clauses[hi].has_effect_tag_expr) {
                        self.emit_ffi_manifest_lib_entries(expr.handle.clauses[hi].effect_tag_expr, first);
                    }
                    self.emit_ffi_manifest_lib_entries(expr.handle.clauses[hi].handler_body, first);
                }
            }
            return;
        case E_INDEX:
            self.emit_ffi_manifest_lib_entries(expr.index.collection, first);
            self.emit_ffi_manifest_lib_entries(expr.index.index, first);
            return;
        case E_MATCH:
            if (expr.match != null) {
                self.emit_ffi_manifest_lib_entries(expr.match.scrutinee, first);
                for (usz mi = 0; mi < expr.match.clause_count; mi++) {
                    self.emit_ffi_manifest_lib_entries(expr.match.clauses[mi].result, first);
                }
            }
            return;
        case E_AND:
            self.emit_ffi_manifest_lib_entries(expr.and_expr.left, first);
            self.emit_ffi_manifest_lib_entries(expr.and_expr.right, first);
            return;
        case E_OR:
            self.emit_ffi_manifest_lib_entries(expr.or_expr.left, first);
            self.emit_ffi_manifest_lib_entries(expr.or_expr.right, first);
            return;
        case E_CALL:
            if (expr.call != null) {
                self.emit_ffi_manifest_lib_entries(expr.call.func, first);
                for (usz ai = 0; ai < expr.call.arg_count; ai++) {
                    self.emit_ffi_manifest_lib_entries(expr.call.args[ai], first);
                }
            }
            return;
        case E_MODULE:
            if (expr.module_expr != null) {
                for (usz mi = 0; mi < expr.module_expr.body_count; mi++) {
                    self.emit_ffi_manifest_lib_entries(expr.module_expr.body[mi], first);
                }
            }
            return;
        case E_WITH_MODULE:
            if (expr.with_module != null) {
                for (usz wi = 0; wi < expr.with_module.body_count; wi++) {
                    self.emit_ffi_manifest_lib_entries(expr.with_module.body[wi], first);
                }
            }
            return;
        case E_BLOCK:
            if (expr.block != null) {
                for (usz bi = 0; bi < expr.block.expr_count; bi++) {
                    self.emit_ffi_manifest_lib_entries(expr.block.exprs[bi], first);
                }
            }
            return;
        case E_SET:
            self.emit_ffi_manifest_lib_entries(expr.set_expr.value, first);
            return;
        case E_DEFINE:
            self.emit_ffi_manifest_lib_entries(expr.define.value, first);
            return;
        default:
    }
    self.emit_ffi_manifest_lib_entry(expr, first);
}

fn void Compiler.emit_ffi_manifest_fn_entries(Compiler* self, Expr* expr, bool* first) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_LAMBDA:
            if (expr.lambda != null) self.emit_ffi_manifest_fn_entries(expr.lambda.body, first);
            return;
        case E_APP:
            self.emit_ffi_manifest_fn_entries(expr.app.func, first);
            self.emit_ffi_manifest_fn_entries(expr.app.arg, first);
            return;
        case E_IF:
            self.emit_ffi_manifest_fn_entries(expr.if_expr.test, first);
            self.emit_ffi_manifest_fn_entries(expr.if_expr.then_branch, first);
            self.emit_ffi_manifest_fn_entries(expr.if_expr.else_branch, first);
            return;
        case E_LET:
            self.emit_ffi_manifest_fn_entries(expr.let_expr.init, first);
            self.emit_ffi_manifest_fn_entries(expr.let_expr.body, first);
            return;
        case E_RESET:
            self.emit_ffi_manifest_fn_entries(expr.reset.body, first);
            return;
        case E_SHIFT:
            self.emit_ffi_manifest_fn_entries(expr.shift.body, first);
            return;
        case E_PERFORM:
            if (expr.perform.has_tag_expr) self.emit_ffi_manifest_fn_entries(expr.perform.tag_expr, first);
            self.emit_ffi_manifest_fn_entries(expr.perform.arg, first);
            return;
        case E_RESOLVE:
            self.emit_ffi_manifest_fn_entries(expr.resolve.value, first);
            return;
        case E_HANDLE:
            if (expr.handle != null) {
                self.emit_ffi_manifest_fn_entries(expr.handle.body, first);
                for (usz hi = 0; hi < expr.handle.clause_count; hi++) {
                    if (expr.handle.clauses[hi].has_effect_tag_expr) {
                        self.emit_ffi_manifest_fn_entries(expr.handle.clauses[hi].effect_tag_expr, first);
                    }
                    self.emit_ffi_manifest_fn_entries(expr.handle.clauses[hi].handler_body, first);
                }
            }
            return;
        case E_INDEX:
            self.emit_ffi_manifest_fn_entries(expr.index.collection, first);
            self.emit_ffi_manifest_fn_entries(expr.index.index, first);
            return;
        case E_MATCH:
            if (expr.match != null) {
                self.emit_ffi_manifest_fn_entries(expr.match.scrutinee, first);
                for (usz mi = 0; mi < expr.match.clause_count; mi++) {
                    self.emit_ffi_manifest_fn_entries(expr.match.clauses[mi].result, first);
                }
            }
            return;
        case E_AND:
            self.emit_ffi_manifest_fn_entries(expr.and_expr.left, first);
            self.emit_ffi_manifest_fn_entries(expr.and_expr.right, first);
            return;
        case E_OR:
            self.emit_ffi_manifest_fn_entries(expr.or_expr.left, first);
            self.emit_ffi_manifest_fn_entries(expr.or_expr.right, first);
            return;
        case E_CALL:
            if (expr.call != null) {
                self.emit_ffi_manifest_fn_entries(expr.call.func, first);
                for (usz ai = 0; ai < expr.call.arg_count; ai++) {
                    self.emit_ffi_manifest_fn_entries(expr.call.args[ai], first);
                }
            }
            return;
        case E_MODULE:
            if (expr.module_expr != null) {
                for (usz mi = 0; mi < expr.module_expr.body_count; mi++) {
                    self.emit_ffi_manifest_fn_entries(expr.module_expr.body[mi], first);
                }
            }
            return;
        case E_WITH_MODULE:
            if (expr.with_module != null) {
                for (usz wi = 0; wi < expr.with_module.body_count; wi++) {
                    self.emit_ffi_manifest_fn_entries(expr.with_module.body[wi], first);
                }
            }
            return;
        case E_BLOCK:
            if (expr.block != null) {
                for (usz bi = 0; bi < expr.block.expr_count; bi++) {
                    self.emit_ffi_manifest_fn_entries(expr.block.exprs[bi], first);
                }
            }
            return;
        case E_SET:
            self.emit_ffi_manifest_fn_entries(expr.set_expr.value, first);
            return;
        case E_DEFINE:
            self.emit_ffi_manifest_fn_entries(expr.define.value, first);
            return;
        default:
    }
    self.emit_ffi_manifest_fn_entry(expr, first);
}

fn void Compiler.emit_ffi_contract_manifest(Compiler* self, List{Expr*}* exprs) {
    if (exprs == null || exprs.len() == 0) return;
    bool has_ffi = false;
    foreach (expr : *exprs) {
        if (self.has_ffi_preload_expr(expr)) {
            has_ffi = true;
            break;
        }
    }
    if (!has_ffi) return;

    self.emit("const char[] __omni_ffi_contract_json = `{\n");
    self.emit("  \"format\": \"omni.ffi-contract/v1\",\n");
    self.emit("  \"libraries\": [\n");
    bool first_lib = true;
    for (usz i = 0; i < exprs.len(); i++) {
        self.emit_ffi_manifest_lib_entries((*exprs)[i], &first_lib);
    }
    self.emit("\n  ],\n");
    self.emit("  \"functions\": [\n");
    bool first_fn = true;
    for (usz i = 0; i < exprs.len(); i++) {
        self.emit_ffi_manifest_fn_entries((*exprs)[i], &first_fn);
    }
    self.emit("\n  ]\n");
    self.emit("}`;\n\n");
}
