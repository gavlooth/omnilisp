module lisp::aot;

import lisp;

struct AotMetaSpec {
    char[] key;
    char[] sym_value;
    long int_value;
    bool is_int;
}

struct AotTypeAnnotationSpec {
    bool has_annotation;
    bool is_compound;
    bool is_dict;
    char[] base_type;
    char[]* params;
    usz param_count;
    AotMetaSpec* meta;
    usz meta_count;
    bool has_val_literal;
    lisp::ValueTag val_tag;
    long val_int;
    char[] val_sym;
}

struct AotTypeFieldSpec {
    char[] name;
    AotTypeAnnotationSpec annotation;
}

struct AotUnionVariantSpec {
    char[] name;
    char[]* fields;
    usz field_count;
}

fn void aot_type_annotation_init_empty(lisp::TypeAnnotation* ann) {
    ann.has_annotation = false;
    ann.is_compound = false;
    ann.is_dict = false;
    ann.base_type = (lisp::SymbolId)0;
    ann.params = null;
    ann.param_count = 0;
    ann.meta = null;
    ann.meta_count = 0;
    ann.val_literal.tag = lisp::ValueTag.NIL;
    ann.val_literal.int_value = 0;
    ann.val_literal.sym_value = (lisp::SymbolId)0;
    ann.has_val_literal = false;
}

fn void aot_type_annotation_destroy(lisp::TypeAnnotation* ann) {
    if (ann == null) return;
    if (ann.params != null) mem::free(ann.params);
    if (ann.meta != null) mem::free(ann.meta);
    aot_type_annotation_init_empty(ann);
}

fn bool aot_type_annotation_init_from_spec(lisp::TypeAnnotation* ann, AotTypeAnnotationSpec* spec) {
    aot_type_annotation_init_empty(ann);
    if (spec == null || !spec.has_annotation) return true;

    ann.has_annotation = true;
    ann.is_compound = spec.is_compound;
    ann.is_dict = spec.is_dict;
    ann.base_type = aot_symbol_from_text(spec.base_type);
    ann.param_count = spec.param_count;
    if (spec.param_count > 0) {
        ann.params = (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * spec.param_count);
        if (ann.params == null) return false;
        for (usz i = 0; i < spec.param_count; i++) {
            ann.params[i] = aot_symbol_from_text(spec.params[i]);
        }
    }

    ann.meta_count = spec.meta_count;
    if (spec.meta_count > 0) {
        ann.meta = (lisp::MetaEntry*)mem::malloc(lisp::MetaEntry.sizeof * spec.meta_count);
        if (ann.meta == null) {
            aot_type_annotation_destroy(ann);
            return false;
        }
        for (usz i = 0; i < spec.meta_count; i++) {
            ann.meta[i].key = aot_symbol_from_text(spec.meta[i].key);
            ann.meta[i].is_int = spec.meta[i].is_int;
            ann.meta[i].int_value = spec.meta[i].int_value;
            ann.meta[i].sym_value = spec.meta[i].is_int ? (lisp::SymbolId)0 : aot_symbol_from_text(spec.meta[i].sym_value);
        }
    }

    ann.has_val_literal = spec.has_val_literal;
    if (spec.has_val_literal) {
        ann.val_literal.tag = spec.val_tag;
        ann.val_literal.int_value = spec.val_int;
        ann.val_literal.sym_value = aot_symbol_from_text(spec.val_sym);
    }
    return true;
}

fn void aot_method_signature_init_empty(lisp::MethodSignature* sig) {
    sig.param_types = null;
    sig.param_type_vars = null;
    sig.val_literals = null;
    sig.has_val_literal = null;
    sig.param_count = 0;
    sig.constraints = null;
    sig.constraint_count = 0;
}

fn void aot_method_signature_destroy(lisp::MethodSignature* sig) {
    if (sig == null) return;
    if (sig.param_types != null) mem::free(sig.param_types);
    if (sig.param_type_vars != null) mem::free(sig.param_type_vars);
    if (sig.val_literals != null) mem::free(sig.val_literals);
    if (sig.has_val_literal != null) mem::free(sig.has_val_literal);
    if (sig.constraints != null) mem::free(sig.constraints);
    aot_method_signature_init_empty(sig);
}

fn bool aot_method_signature_alloc(lisp::MethodSignature* sig, usz param_count, usz constraint_count) {
    aot_method_signature_init_empty(sig);
    sig.param_count = param_count;
    sig.constraint_count = constraint_count;
    if (param_count > 0) {
        sig.param_types = (lisp::TypeId*)mem::malloc(lisp::TypeId.sizeof * param_count);
        sig.param_type_vars = (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * param_count);
        sig.val_literals = (lisp::ValueLiteralKey*)mem::malloc(lisp::ValueLiteralKey.sizeof * param_count);
        sig.has_val_literal = (bool*)mem::malloc(bool.sizeof * param_count);
        if (sig.param_types == null || sig.param_type_vars == null || sig.val_literals == null || sig.has_val_literal == null) {
            aot_method_signature_destroy(sig);
            return false;
        }
        for (usz i = 0; i < param_count; i++) {
            sig.param_types[i] = lisp::INVALID_TYPE_ID;
            sig.param_type_vars[i] = (lisp::SymbolId)0;
            sig.val_literals[i].tag = lisp::ValueTag.NIL;
            sig.val_literals[i].int_value = 0;
            sig.val_literals[i].sym_value = (lisp::SymbolId)0;
            sig.has_val_literal[i] = false;
        }
    }
    if (constraint_count > 0) {
        sig.constraints = (lisp::MethodConstraint*)mem::malloc(lisp::MethodConstraint.sizeof * constraint_count);
        if (sig.constraints == null) {
            aot_method_signature_destroy(sig);
            return false;
        }
    }
    return true;
}

fn bool aot_build_method_signature_from_specs(lisp::MethodSignature* sig, AotTypeAnnotationSpec* specs, usz param_count) {
    usz constraint_count = 0;
    for (usz i = 0; i < param_count; i++) {
        if (!specs[i].has_annotation || !specs[i].is_dict) continue;
        for (usz m = 0; m < specs[i].meta_count; m++) {
            if (!specs[i].meta[m].is_int) constraint_count++;
        }
    }
    if (!aot_method_signature_alloc(sig, param_count, constraint_count)) return false;

    lisp::SymbolId sym_lambda_type = g_aot_interp.symbols.intern("Lambda");
    usz next_constraint = 0;
    for (usz i = 0; i < param_count; i++) {
        AotTypeAnnotationSpec* ann = &specs[i];
        if (!ann.has_annotation) continue;

        if (ann.has_val_literal) {
            sig.has_val_literal[i] = true;
            sig.val_literals[i].tag = ann.val_tag;
            sig.val_literals[i].int_value = ann.val_int;
            sig.val_literals[i].sym_value = aot_symbol_from_text(ann.val_sym);
            continue;
        }

        if (ann.is_dict) {
            bool saw_type_var = false;
            for (usz m = 0; m < ann.meta_count; m++) {
                if (ann.meta[m].is_int) continue;
                lisp::SymbolId param_sym = aot_symbol_from_text(ann.meta[m].key);
                if (!saw_type_var) {
                    sig.param_type_vars[i] = param_sym;
                    saw_type_var = true;
                } else if ((uint)sig.param_type_vars[i] != (uint)param_sym) {
                    sig.param_type_vars[i] = (lisp::SymbolId)0;
                }
                sig.constraints[next_constraint].param_sym = param_sym;
                sig.constraints[next_constraint].bound_type = g_aot_interp.types.lookup(aot_symbol_from_text(ann.meta[m].sym_value), &g_aot_interp.symbols);
                next_constraint++;
            }
            continue;
        }

        lisp::SymbolId base_type = aot_symbol_from_text(ann.base_type);
        lisp::TypeId resolved = g_aot_interp.types.lookup(base_type, &g_aot_interp.symbols);
        if ((uint)base_type == (uint)sym_lambda_type) {
            sig.param_types[i] = g_aot_interp.tid_Closure;
            sig.param_type_vars[i] = (lisp::SymbolId)0;
        } else {
            sig.param_types[i] = resolved;
            sig.param_type_vars[i] = resolved == lisp::INVALID_TYPE_ID ? base_type : (lisp::SymbolId)0;
        }
    }
    return true;
}

fn lisp::Value* define_type(
    char[] name,
    bool has_parent,
    char[] parent_name,
    char[]* type_params,
    usz type_param_count,
    AotTypeFieldSpec* fields,
    usz field_count
) {
    lisp::TypeField* rt_fields = field_count > 0 ? (lisp::TypeField*)mem::malloc(lisp::TypeField.sizeof * field_count) : null;
    defer if (rt_fields != null) {
        for (usz i = 0; i < field_count; i++) aot_type_annotation_destroy(&rt_fields[i].type_ann);
        mem::free(rt_fields);
    };

    lisp::SymbolId* rt_type_params = type_param_count > 0 ? (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * type_param_count) : null;
    defer if (rt_type_params != null) mem::free(rt_type_params);

    if ((field_count > 0 && rt_fields == null) || (type_param_count > 0 && rt_type_params == null)) {
        return lisp::make_error(g_aot_interp, "aot define-type: out of memory");
    }

    for (usz i = 0; i < field_count; i++) {
        aot_type_annotation_init_empty(&rt_fields[i].type_ann);
    }
    for (usz i = 0; i < type_param_count; i++) {
        rt_type_params[i] = aot_symbol_from_text(type_params[i]);
    }
    for (usz i = 0; i < field_count; i++) {
        rt_fields[i].name = aot_symbol_from_text(fields[i].name);
        if (!aot_type_annotation_init_from_spec(&rt_fields[i].type_ann, &fields[i].annotation)) {
            return lisp::make_error(g_aot_interp, "aot define-type: out of memory");
        }
    }

    lisp::ExprDefType dt;
    dt.name = aot_symbol_from_text(name);
    dt.parent = has_parent ? aot_symbol_from_text(parent_name) : (lisp::SymbolId)0;
    dt.has_parent = has_parent;
    dt.fields = rt_fields;
    dt.field_count = field_count;
    dt.type_params = rt_type_params;
    dt.type_param_count = type_param_count;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFTYPE;
    expr.deftype = &dt;
    return aot_eval_result_value(lisp::eval_deftype(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_abstract(char[] name, bool has_parent, char[] parent_name) {
    lisp::ExprDefAbstract da;
    da.name = aot_symbol_from_text(name);
    da.parent = has_parent ? aot_symbol_from_text(parent_name) : (lisp::SymbolId)0;
    da.has_parent = has_parent;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFABSTRACT;
    expr.defabstract = da;
    return aot_eval_result_value(lisp::eval_defabstract(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_union(
    char[] name,
    char[]* type_params,
    usz type_param_count,
    AotUnionVariantSpec* variants,
    usz variant_count
) {
    lisp::SymbolId* rt_type_params = type_param_count > 0 ? (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * type_param_count) : null;
    defer if (rt_type_params != null) mem::free(rt_type_params);

    lisp::UnionVariant* rt_variants = variant_count > 0 ? (lisp::UnionVariant*)mem::malloc(lisp::UnionVariant.sizeof * variant_count) : null;
    defer if (rt_variants != null) {
        for (usz i = 0; i < variant_count; i++) {
            if (rt_variants[i].fields != null) mem::free(rt_variants[i].fields);
        }
        mem::free(rt_variants);
    };

    if ((type_param_count > 0 && rt_type_params == null) || (variant_count > 0 && rt_variants == null)) {
        return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
    }

    for (usz i = 0; i < variant_count; i++) {
        rt_variants[i].fields = null;
        rt_variants[i].field_count = 0;
    }
    for (usz i = 0; i < type_param_count; i++) {
        rt_type_params[i] = aot_symbol_from_text(type_params[i]);
    }
    for (usz i = 0; i < variant_count; i++) {
        rt_variants[i].name = aot_symbol_from_text(variants[i].name);
        rt_variants[i].field_count = variants[i].field_count;
        rt_variants[i].fields = variants[i].field_count > 0 ? (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * variants[i].field_count) : null;
        if (variants[i].field_count > 0 && rt_variants[i].fields == null) {
            return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
        }
        for (usz fi = 0; fi < variants[i].field_count; fi++) {
            rt_variants[i].fields[fi] = aot_symbol_from_text(variants[i].fields[fi]);
        }
    }

    lisp::ExprDefUnion du;
    du.name = aot_symbol_from_text(name);
    du.type_params = rt_type_params;
    du.type_param_count = type_param_count;
    du.variants = rt_variants;
    du.variant_count = variant_count;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFUNION;
    expr.defunion = &du;
    return aot_eval_result_value(lisp::eval_defunion(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_alias(char[] name, AotTypeAnnotationSpec* target_spec) {
    lisp::TypeAnnotation target;
    if (!aot_type_annotation_init_from_spec(&target, target_spec)) {
        return lisp::make_error(g_aot_interp, "aot define-alias: out of memory");
    }
    defer aot_type_annotation_destroy(&target);

    lisp::ExprDefAlias da;
    da.name = aot_symbol_from_text(name);
    da.target = target;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFALIAS;
    expr.defalias = da;
    return aot_eval_result_value(lisp::eval_defalias(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_effect(char[] name, bool has_arg_type, AotTypeAnnotationSpec* arg_type_spec) {
    lisp::TypeAnnotation arg_type;
    if (!aot_type_annotation_init_from_spec(&arg_type, arg_type_spec)) {
        return lisp::make_error(g_aot_interp, "aot define-effect: out of memory");
    }
    defer aot_type_annotation_destroy(&arg_type);

    lisp::ExprDefEffect de;
    de.name = aot_symbol_from_text(name);
    de.arg_type = arg_type;
    de.has_arg_type = has_arg_type;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFEFFECT;
    expr.defeffect = de;
    return aot_eval_result_value(lisp::eval_defeffect(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_typed_method(char[] name, lisp::Value* value, AotTypeAnnotationSpec* param_specs, usz param_count) {
    lisp::MethodSignature sig;
    if (!aot_build_method_signature_from_specs(&sig, param_specs, param_count)) {
        return lisp::make_error(g_aot_interp, "aot define-typed-method: out of memory");
    }
    defer aot_method_signature_destroy(&sig);

    lisp::Value* stored_val = lisp::boundary_promote_to_root_site(
        value,
        g_aot_interp,
        lisp::COPY_SITE_JIT_EVAL_DEFINE
    );
    if (stored_val != null && stored_val.tag == lisp::ValueTag.ERROR) return stored_val;

    lisp::SymbolId name_sym = aot_symbol_from_text(name);
    return lisp::jit_eval_define_typed_callable(g_aot_interp, name_sym, stored_val, &sig);
}
