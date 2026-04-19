module lisp::aot;

import lisp;

fn lisp::Value* define_type(
    char[] name,
    bool has_parent,
    char[] parent_name,
    char[]* type_params,
    usz type_param_count,
    AotTypeFieldSpec* fields,
    usz field_count
) {
    if (field_count > 0 && field_count > usz.max / lisp::TypeField.sizeof) {
        return lisp::make_error(g_aot_interp, "aot define-type: out of memory");
    }
    if (type_param_count > 0 && type_param_count > usz.max / lisp::SymbolId.sizeof) {
        return lisp::make_error(g_aot_interp, "aot define-type: out of memory");
    }

    lisp::TypeField* rt_fields = field_count > 0 ? (lisp::TypeField*)mem::malloc(lisp::TypeField.sizeof * field_count) : null;
    if (field_count > 0 && rt_fields == null) {
        return lisp::make_error(g_aot_interp, "aot define-type: out of memory");
    }
    for (usz i = 0; i < field_count; i++) {
        aot_type_annotation_init_empty(&rt_fields[i].type_ann);
    }
    defer if (rt_fields != null) {
        for (usz i = 0; i < field_count; i++) aot_type_annotation_destroy(&rt_fields[i].type_ann);
        mem::free(rt_fields);
    };

    lisp::SymbolId* rt_type_params = type_param_count > 0 ? (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * type_param_count) : null;
    defer if (rt_type_params != null) mem::free(rt_type_params);

    if (type_param_count > 0 && rt_type_params == null) {
        return lisp::make_error(g_aot_interp, "aot define-type: out of memory");
    }

    for (usz i = 0; i < type_param_count; i++) {
        if (!aot_symbol_from_text_checked(type_params[i], &rt_type_params[i])) {
            return lisp::make_error(g_aot_interp, "aot define-type: invalid or uninternable type parameter");
        }
    }
    for (usz i = 0; i < field_count; i++) {
        if (!aot_symbol_from_text_checked(fields[i].name, &rt_fields[i].name)) {
            return lisp::make_error(g_aot_interp, "aot define-type: invalid or uninternable field name");
        }
        if (!aot_type_annotation_init_from_spec(&rt_fields[i].type_ann, &fields[i].annotation)) {
            return lisp::make_error(g_aot_interp, "aot define-type: out of memory");
        }
    }

    lisp::ExprDefType dt;
    if (!aot_symbol_from_text_checked(name, &dt.name)) {
        return lisp::make_error(g_aot_interp, "aot define-type: invalid or uninternable type name");
    }
    dt.parent = (lisp::SymbolId)0;
    if (has_parent && !aot_symbol_from_text_checked(parent_name, &dt.parent)) {
        return lisp::make_error(g_aot_interp, "aot define-type: invalid or uninternable parent type name");
    }
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
    if (!aot_symbol_from_text_checked(name, &da.name)) {
        return lisp::make_error(g_aot_interp, "aot define-abstract: invalid or uninternable type name");
    }
    da.parent = (lisp::SymbolId)0;
    if (has_parent && !aot_symbol_from_text_checked(parent_name, &da.parent)) {
        return lisp::make_error(g_aot_interp, "aot define-abstract: invalid or uninternable parent type name");
    }
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
    if (type_param_count > 0 && type_param_count > usz.max / lisp::SymbolId.sizeof) {
        return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
    }
    if (variant_count > 0 && variant_count > usz.max / lisp::UnionVariant.sizeof) {
        return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
    }

    lisp::SymbolId* rt_type_params = type_param_count > 0 ? (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * type_param_count) : null;
    defer if (rt_type_params != null) mem::free(rt_type_params);

    if (type_param_count > 0 && rt_type_params == null) {
        return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
    }

    lisp::UnionVariant* rt_variants = variant_count > 0 ? (lisp::UnionVariant*)mem::malloc(lisp::UnionVariant.sizeof * variant_count) : null;
    if (variant_count > 0 && rt_variants == null) {
        return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
    }
    for (usz i = 0; i < variant_count; i++) {
        rt_variants[i].fields = null;
        rt_variants[i].field_count = 0;
    }
    defer if (rt_variants != null) {
        for (usz i = 0; i < variant_count; i++) {
            if (rt_variants[i].fields != null) mem::free(rt_variants[i].fields);
        }
        mem::free(rt_variants);
    };

    for (usz i = 0; i < type_param_count; i++) {
        if (!aot_symbol_from_text_checked(type_params[i], &rt_type_params[i])) {
            return lisp::make_error(g_aot_interp, "aot define-union: invalid or uninternable type parameter");
        }
    }
    for (usz i = 0; i < variant_count; i++) {
        if (!aot_symbol_from_text_checked(variants[i].name, &rt_variants[i].name)) {
            return lisp::make_error(g_aot_interp, "aot define-union: invalid or uninternable variant name");
        }
        rt_variants[i].field_count = variants[i].field_count;
        if (variants[i].field_count > 0 && variants[i].field_count > usz.max / lisp::SymbolId.sizeof) {
            return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
        }
        rt_variants[i].fields = variants[i].field_count > 0 ? (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * variants[i].field_count) : null;
        if (variants[i].field_count > 0 && rt_variants[i].fields == null) {
            return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
        }
        for (usz fi = 0; fi < variants[i].field_count; fi++) {
            if (!aot_symbol_from_text_checked(variants[i].fields[fi], &rt_variants[i].fields[fi])) {
                return lisp::make_error(g_aot_interp, "aot define-union: invalid or uninternable variant field");
            }
        }
    }

    lisp::ExprDefUnion du;
    if (!aot_symbol_from_text_checked(name, &du.name)) {
        return lisp::make_error(g_aot_interp, "aot define-union: invalid or uninternable union name");
    }
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
    if (!aot_symbol_from_text_checked(name, &da.name)) {
        return lisp::make_error(g_aot_interp, "aot define-alias: invalid or uninternable alias name");
    }
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
    if (!aot_symbol_from_text_checked(name, &de.name)) {
        return lisp::make_error(g_aot_interp, "aot define-effect: invalid or uninternable effect name");
    }
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
    if (stored_val == null) return lisp::make_error(g_aot_interp, "aot define-typed-method: failed to promote method implementation");
    if (stored_val != null && stored_val.tag == lisp::ValueTag.ERROR) return stored_val;

    lisp::SymbolId name_sym = (lisp::SymbolId)0;
    if (!aot_symbol_from_text_checked(name, &name_sym)) {
        return lisp::make_error(g_aot_interp, "aot define-typed-method: invalid or uninternable method name");
    }
    return lisp::jit_eval_define_typed_callable(g_aot_interp, name_sym, stored_val, &sig);
}
