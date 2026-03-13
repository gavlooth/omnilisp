module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (resolve/index/define forms)
// =============================================================================
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

fn void Compiler.emit_bool_literal(Compiler* self, bool value) @inline {
    self.emit(value ? "true" : "false");
}

fn void Compiler.emit_value_tag_literal(Compiler* self, ValueTag tag) {
    switch (tag) {
        case INT:
            self.emit("lisp::ValueTag.INT");
        case SYMBOL:
            self.emit("lisp::ValueTag.SYMBOL");
        case STRING:
            self.emit("lisp::ValueTag.STRING");
        default:
            self.emit("lisp::ValueTag.NIL");
    }
}

fn void Compiler.emit_target_assignment_prefix(Compiler* self, char[] target, char[] field) {
    self.emit_indent();
    self.emit(target);
    self.emit(".");
    self.emit(field);
    self.emit(" = ");
}

fn void Compiler.emit_string_array_init(Compiler* self, char[] name, char[]* values, usz count) {
    if (count == 0) return;
    self.emit_indent();
    self.emit("char[][");
    self.emit_usz(count);
    self.emit("] ");
    self.emit(name);
    self.emit(";\n");
    for (usz i = 0; i < count; i++) {
        self.emit_indent();
        self.emit(name);
        self.emit("[");
        self.emit_usz(i);
        self.emit("] = ");
        self.emit_string_literal(values[i]);
        self.emit(";\n");
    }
}

fn void Compiler.emit_aot_type_annotation_spec_init(Compiler* self, char[] target, TypeAnnotation* ann) {
    self.emit_target_assignment_prefix(target, "has_annotation");
    self.emit_bool_literal(ann.has_annotation);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "is_compound");
    self.emit_bool_literal(ann.is_compound);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "is_dict");
    self.emit_bool_literal(ann.is_dict);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "base_type");
    self.emit_string_literal((uint)ann.base_type != 0 ? self.interp.symbols.get_name(ann.base_type) : "");
    self.emit(";\n");

    if (ann.param_count > 0 && ann.params != null) {
        usz params_id = self.next_result();
        char[64] params_name_buf;
        char[] params_name = io::bprintf(&params_name_buf, "_aot_ann_params_%d", (int)params_id)!!;
        self.emit_indent();
        self.emit("char[][");
        self.emit_usz(ann.param_count);
        self.emit("] ");
        self.emit(params_name);
        self.emit(";\n");
        for (usz i = 0; i < ann.param_count; i++) {
            self.emit_indent();
            self.emit(params_name);
            self.emit("[");
            self.emit_usz(i);
            self.emit("] = ");
            self.emit_string_literal(self.interp.symbols.get_name(ann.params[i]));
            self.emit(";\n");
        }

        self.emit_target_assignment_prefix(target, "params");
        self.emit("&");
        self.emit(params_name);
        self.emit("[0];\n");
    } else {
        self.emit_target_assignment_prefix(target, "params");
        self.emit("null;\n");
    }

    self.emit_target_assignment_prefix(target, "param_count");
    self.emit_usz(ann.param_count);
    self.emit(";\n");

    if (ann.meta_count > 0 && ann.meta != null) {
        usz meta_id = self.next_result();
        char[64] meta_name_buf;
        char[] meta_name = io::bprintf(&meta_name_buf, "_aot_ann_meta_%d", (int)meta_id)!!;
        self.emit_indent();
        self.emit("lisp::aot::AotMetaSpec[");
        self.emit_usz(ann.meta_count);
        self.emit("] ");
        self.emit(meta_name);
        self.emit(";\n");
        for (usz i = 0; i < ann.meta_count; i++) {
            char[96] entry_buf;
            char[] entry = io::bprintf(&entry_buf, "%s[%d]", (ZString)meta_name, (int)i)!!;
            self.emit_target_assignment_prefix(entry, "key");
            self.emit_string_literal(self.interp.symbols.get_name(ann.meta[i].key));
            self.emit(";\n");
            self.emit_target_assignment_prefix(entry, "sym_value");
            self.emit_string_literal(!ann.meta[i].is_int && (uint)ann.meta[i].sym_value != 0 ? self.interp.symbols.get_name(ann.meta[i].sym_value) : "");
            self.emit(";\n");
            self.emit_target_assignment_prefix(entry, "int_value");
            self.emit_int(ann.meta[i].int_value);
            self.emit(";\n");
            self.emit_target_assignment_prefix(entry, "is_int");
            self.emit_bool_literal(ann.meta[i].is_int);
            self.emit(";\n");
        }

        self.emit_target_assignment_prefix(target, "meta");
        self.emit("&");
        self.emit(meta_name);
        self.emit("[0];\n");
    } else {
        self.emit_target_assignment_prefix(target, "meta");
        self.emit("null;\n");
    }

    self.emit_target_assignment_prefix(target, "meta_count");
    self.emit_usz(ann.meta_count);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "has_val_literal");
    self.emit_bool_literal(ann.has_val_literal);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "val_tag");
    self.emit_value_tag_literal(ann.val_literal.tag);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "val_int");
    self.emit_int(ann.val_literal.int_value);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "val_sym");
    if ((uint)ann.val_literal.sym_value != 0) {
        self.emit_string_literal(self.interp.symbols.get_name(ann.val_literal.sym_value));
    } else {
        self.emit_string_literal("");
    }
    self.emit(";\n");
}

fn usz Compiler.compile_typed_define_direct(Compiler* self, Expr* define_expr) {
    usz value_r = self.compile_to_temp(define_expr.define.value);
    ExprLambda* lambda = define_expr.define.value.lambda;
    usz params_id = self.next_result();
    char[64] params_name_buf;
    char[] params_name = io::bprintf(&params_name_buf, "_aot_typed_params_%d", (int)params_id)!!;

    if (lambda.param_count > 0) {
        self.emit_indent();
        self.emit("lisp::aot::AotTypeAnnotationSpec[");
        self.emit_usz(lambda.param_count);
        self.emit("] ");
        self.emit(params_name);
        self.emit(";\n");
        for (usz i = 0; i < lambda.param_count; i++) {
            char[96] target_buf;
            char[] target = io::bprintf(&target_buf, "%s[%d]", (ZString)params_name, (int)i)!!;
            self.emit_aot_type_annotation_spec_init(target, &lambda.param_annotations[i]);
        }
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_typed_method(");
    self.emit_string_literal(self.interp.symbols.get_name(define_expr.define.name));
    self.emit(", ");
    self.emit_temp_ref(value_r);
    self.emit(", ");
    if (lambda.param_count > 0) {
        self.emit("&");
        self.emit(params_name);
        self.emit("[0]");
    } else {
        self.emit("null");
    }
    self.emit(", ");
    self.emit_usz(lambda.param_count);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_deftype_direct(Compiler* self, Expr* expr) {
    ExprDefType* dt = expr.deftype;
    usz fields_id = self.next_result();
    char[64] fields_name_buf;
    char[] fields_name = io::bprintf(&fields_name_buf, "_aot_type_fields_%d", (int)fields_id)!!;

    if (dt.type_param_count > 0) {
        usz params_id = self.next_result();
        char[64] params_name_buf;
        char[] params_name = io::bprintf(&params_name_buf, "_aot_type_params_%d", (int)params_id)!!;
        self.emit_indent();
        self.emit("char[][");
        self.emit_usz(dt.type_param_count);
        self.emit("] ");
        self.emit(params_name);
        self.emit(";\n");
        for (usz i = 0; i < dt.type_param_count; i++) {
            self.emit_indent();
            self.emit(params_name);
            self.emit("[");
            self.emit_usz(i);
            self.emit("] = ");
            self.emit_string_literal(self.interp.symbols.get_name(dt.type_params[i]));
            self.emit(";\n");
        }
        if (dt.field_count > 0) {
            self.emit_indent();
            self.emit("lisp::aot::AotTypeFieldSpec[");
            self.emit_usz(dt.field_count);
            self.emit("] ");
            self.emit(fields_name);
            self.emit(";\n");
            for (usz i = 0; i < dt.field_count; i++) {
                char[96] field_buf;
                char[] field_target = io::bprintf(&field_buf, "%s[%d]", (ZString)fields_name, (int)i)!!;
                self.emit_target_assignment_prefix(field_target, "name");
                self.emit_string_literal(self.interp.symbols.get_name(dt.fields[i].name));
                self.emit(";\n");
                char[128] ann_buf;
                char[] ann_target = io::bprintf(&ann_buf, "%s.annotation", (ZString)field_target)!!;
                self.emit_aot_type_annotation_spec_init(ann_target, &dt.fields[i].type_ann);
            }
        }

        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::define_type(");
        self.emit_string_literal(self.interp.symbols.get_name(dt.name));
        self.emit(", ");
        self.emit_bool_literal(dt.has_parent);
        self.emit(", ");
        self.emit_string_literal(dt.has_parent ? self.interp.symbols.get_name(dt.parent) : "");
        self.emit(", &");
        self.emit(params_name);
        self.emit("[0], ");
        self.emit_usz(dt.type_param_count);
        self.emit(", ");
        if (dt.field_count > 0) {
            self.emit("&");
            self.emit(fields_name);
            self.emit("[0]");
        } else {
            self.emit("null");
        }
        self.emit(", ");
        self.emit_usz(dt.field_count);
        self.emit(");\n");
        return id;
    }

    if (dt.field_count > 0) {
        self.emit_indent();
        self.emit("lisp::aot::AotTypeFieldSpec[");
        self.emit_usz(dt.field_count);
        self.emit("] ");
        self.emit(fields_name);
        self.emit(";\n");
        for (usz i = 0; i < dt.field_count; i++) {
            char[96] field_buf;
            char[] field_target = io::bprintf(&field_buf, "%s[%d]", (ZString)fields_name, (int)i)!!;
            self.emit_target_assignment_prefix(field_target, "name");
            self.emit_string_literal(self.interp.symbols.get_name(dt.fields[i].name));
            self.emit(";\n");
            char[128] ann_buf;
            char[] ann_target = io::bprintf(&ann_buf, "%s.annotation", (ZString)field_target)!!;
            self.emit_aot_type_annotation_spec_init(ann_target, &dt.fields[i].type_ann);
        }
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_type(");
    self.emit_string_literal(self.interp.symbols.get_name(dt.name));
    self.emit(", ");
    self.emit_bool_literal(dt.has_parent);
    self.emit(", ");
    self.emit_string_literal(dt.has_parent ? self.interp.symbols.get_name(dt.parent) : "");
    self.emit(", null, 0, ");
    if (dt.field_count > 0) {
        self.emit("&");
        self.emit(fields_name);
        self.emit("[0]");
    } else {
        self.emit("null");
    }
    self.emit(", ");
    self.emit_usz(dt.field_count);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_defabstract_direct(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_abstract(");
    self.emit_string_literal(self.interp.symbols.get_name(expr.defabstract.name));
    self.emit(", ");
    self.emit_bool_literal(expr.defabstract.has_parent);
    self.emit(", ");
    self.emit_string_literal(expr.defabstract.has_parent ? self.interp.symbols.get_name(expr.defabstract.parent) : "");
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_defunion_direct(Compiler* self, Expr* expr) {
    ExprDefUnion* du = expr.defunion;
    usz variants_id = self.next_result();
    char[64] variants_name_buf;
    char[] variants_name = io::bprintf(&variants_name_buf, "_aot_union_variants_%d", (int)variants_id)!!;

    if (du.type_param_count > 0) {
        usz params_id = self.next_result();
        char[64] params_name_buf;
        char[] params_name = io::bprintf(&params_name_buf, "_aot_union_params_%d", (int)params_id)!!;
        self.emit_indent();
        self.emit("char[][");
        self.emit_usz(du.type_param_count);
        self.emit("] ");
        self.emit(params_name);
        self.emit(";\n");
        for (usz i = 0; i < du.type_param_count; i++) {
            self.emit_indent();
            self.emit(params_name);
            self.emit("[");
            self.emit_usz(i);
            self.emit("] = ");
            self.emit_string_literal(self.interp.symbols.get_name(du.type_params[i]));
            self.emit(";\n");
        }

        if (du.variant_count > 0) {
            self.emit_indent();
            self.emit("lisp::aot::AotUnionVariantSpec[");
            self.emit_usz(du.variant_count);
            self.emit("] ");
            self.emit(variants_name);
            self.emit(";\n");

            for (usz i = 0; i < du.variant_count; i++) {
                char[96] variant_buf;
                char[] variant_target = io::bprintf(&variant_buf, "%s[%d]", (ZString)variants_name, (int)i)!!;
                self.emit_target_assignment_prefix(variant_target, "name");
                self.emit_string_literal(self.interp.symbols.get_name(du.variants[i].name));
                self.emit(";\n");
                if (du.variants[i].field_count > 0) {
                    usz fields_id = self.next_result();
                    char[64] fields_name_buf;
                    char[] fields_name = io::bprintf(&fields_name_buf, "_aot_union_variant_fields_%d", (int)fields_id)!!;
                    self.emit_indent();
                    self.emit("char[][");
                    self.emit_usz(du.variants[i].field_count);
                    self.emit("] ");
                    self.emit(fields_name);
                    self.emit(";\n");
                    for (usz fi = 0; fi < du.variants[i].field_count; fi++) {
                        self.emit_indent();
                        self.emit(fields_name);
                        self.emit("[");
                        self.emit_usz(fi);
                        self.emit("] = ");
                        self.emit_string_literal(self.interp.symbols.get_name(du.variants[i].fields[fi]));
                        self.emit(";\n");
                    }
                    self.emit_target_assignment_prefix(variant_target, "fields");
                    self.emit("&");
                    self.emit(fields_name);
                    self.emit("[0];\n");
                } else {
                    self.emit_target_assignment_prefix(variant_target, "fields");
                    self.emit("null;\n");
                }
                self.emit_target_assignment_prefix(variant_target, "field_count");
                self.emit_usz(du.variants[i].field_count);
                self.emit(";\n");
            }
        }

        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::define_union(");
        self.emit_string_literal(self.interp.symbols.get_name(du.name));
        self.emit(", &");
        self.emit(params_name);
        self.emit("[0], ");
        self.emit_usz(du.type_param_count);
        self.emit(", ");
        if (du.variant_count > 0) {
            self.emit("&");
            self.emit(variants_name);
            self.emit("[0]");
        } else {
            self.emit("null");
        }
        self.emit(", ");
        self.emit_usz(du.variant_count);
        self.emit(");\n");
        return id;
    }

    if (du.variant_count > 0) {
        self.emit_indent();
        self.emit("lisp::aot::AotUnionVariantSpec[");
        self.emit_usz(du.variant_count);
        self.emit("] ");
        self.emit(variants_name);
        self.emit(";\n");
        for (usz i = 0; i < du.variant_count; i++) {
            char[96] variant_buf;
            char[] variant_target = io::bprintf(&variant_buf, "%s[%d]", (ZString)variants_name, (int)i)!!;
            self.emit_target_assignment_prefix(variant_target, "name");
            self.emit_string_literal(self.interp.symbols.get_name(du.variants[i].name));
            self.emit(";\n");
            if (du.variants[i].field_count > 0) {
                usz fields_id = self.next_result();
                char[64] fields_name_buf;
                char[] fields_name = io::bprintf(&fields_name_buf, "_aot_union_variant_fields_%d", (int)fields_id)!!;
                self.emit_indent();
                self.emit("char[][");
                self.emit_usz(du.variants[i].field_count);
                self.emit("] ");
                self.emit(fields_name);
                self.emit(";\n");
                for (usz fi = 0; fi < du.variants[i].field_count; fi++) {
                    self.emit_indent();
                    self.emit(fields_name);
                    self.emit("[");
                    self.emit_usz(fi);
                    self.emit("] = ");
                    self.emit_string_literal(self.interp.symbols.get_name(du.variants[i].fields[fi]));
                    self.emit(";\n");
                }
                self.emit_target_assignment_prefix(variant_target, "fields");
                self.emit("&");
                self.emit(fields_name);
                self.emit("[0];\n");
            } else {
                self.emit_target_assignment_prefix(variant_target, "fields");
                self.emit("null;\n");
            }
            self.emit_target_assignment_prefix(variant_target, "field_count");
            self.emit_usz(du.variants[i].field_count);
            self.emit(";\n");
        }
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_union(");
    self.emit_string_literal(self.interp.symbols.get_name(du.name));
    self.emit(", null, 0, ");
    if (du.variant_count > 0) {
        self.emit("&");
        self.emit(variants_name);
        self.emit("[0]");
    } else {
        self.emit("null");
    }
    self.emit(", ");
    self.emit_usz(du.variant_count);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_defalias_direct(Compiler* self, Expr* expr) {
    usz ann_id = self.next_result();
    char[64] ann_name_buf;
    char[] ann_name = io::bprintf(&ann_name_buf, "_aot_alias_target_%d", (int)ann_id)!!;
    self.emit_indent();
    self.emit("lisp::aot::AotTypeAnnotationSpec ");
    self.emit(ann_name);
    self.emit(";\n");
    self.emit_aot_type_annotation_spec_init(ann_name, &expr.defalias.target);

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_alias(");
    self.emit_string_literal(self.interp.symbols.get_name(expr.defalias.name));
    self.emit(", ");
    self.emit("&");
    self.emit(ann_name);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_defeffect_direct(Compiler* self, Expr* expr) {
    char[64] arg_name_buf;
    char[] arg_name = "";
    if (expr.defeffect.has_arg_type) {
        usz ann_id = self.next_result();
        arg_name = io::bprintf(&arg_name_buf, "_aot_effect_arg_%d", (int)ann_id)!!;
        self.emit_indent();
        self.emit("lisp::aot::AotTypeAnnotationSpec ");
        self.emit(arg_name);
        self.emit(";\n");
        self.emit_aot_type_annotation_spec_init(arg_name, &expr.defeffect.arg_type);
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_effect(");
    self.emit_string_literal(self.interp.symbols.get_name(expr.defeffect.name));
    self.emit(", ");
    self.emit_bool_literal(expr.defeffect.has_arg_type);
    self.emit(", ");
    if (expr.defeffect.has_arg_type) {
        self.emit("&");
        self.emit(arg_name);
    } else {
        self.emit("null");
    }
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_type_form_direct(Compiler* self, Expr* expr) {
    switch (expr.tag) {
        case E_DEFTYPE:
            return self.compile_deftype_direct(expr);
        case E_DEFABSTRACT:
            return self.compile_defabstract_direct(expr);
        case E_DEFUNION:
            return self.compile_defunion_direct(expr);
        case E_DEFALIAS:
            return self.compile_defalias_direct(expr);
        case E_DEFEFFECT:
            return self.compile_defeffect_direct(expr);
        default:
            // Type-form lowering is explicit only; no serialized-eval fallback bridge.
            return self.compile_leaf_expr_to_temp(expr);
    }
}

fn bool Compiler.is_typed_lambda_define(Compiler* self, Expr* expr) @inline {
    if (expr == null || expr.tag != E_DEFINE) return false;
    Expr* value = expr.define.value;
    if (value == null || value.tag != E_LAMBDA || value.lambda == null) return false;
    return value.lambda.has_typed_params;
}

fn usz Compiler.compile_define_rhs_with_typed_bridge(Compiler* self, Expr* define_expr) {
    if (self.is_typed_lambda_define(define_expr)) {
        return self.compile_typed_define_direct(define_expr);
    }
    return self.compile_to_temp(define_expr.define.value);
}

fn usz Compiler.compile_define_flat(Compiler* self, Expr* expr) {
    usz def_val_r = self.compile_define_rhs_with_typed_bridge(expr);
    self.emit_indent();
    self.emit_symbol_name(expr.define.name);
    self.emit(" = ");
    self.emit_temp_ref(def_val_r);
    self.emit(";\n");
    return def_val_r;
}
