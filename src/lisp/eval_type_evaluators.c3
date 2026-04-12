module lisp;

import std::io;
import main;

fn EvalResult eval_defabstract(Expr* expr, Env* env, Interp* interp) {
    ExprDefAbstract* da = &expr.defabstract;

    TypeInfo info;
    info.name = da.name;
    info.kind = TK_ABSTRACT;
    info.fields = null;
    info.field_count = 0;
    info.type_params = null;
    info.type_param_count = 0;
    info.constraints = null;
    info.constraint_count = 0;
    info.variants = null;
    info.variant_count = 0;
    info.alias_target = INVALID_TYPE_ID;

    if (da.has_parent) {
        info.parent = interp.types.lookup(da.parent, &interp.symbols);
        if (info.parent == INVALID_TYPE_ID) {
            char[256] ebuf;
            char[] msg = "unknown parent type";
            if (try char[] formatted = io::bprintf(&ebuf, "unknown parent type '%s'",
                (ZString)interp.symbols.get_name(da.parent))) {
                msg = formatted;
            }
            return eval_error(msg);
        }
    } else {
        info.parent = INVALID_TYPE_ID;
    }

    TypeId type_id = interp.types.register_type(info, &interp.symbols);
    if (type_id == INVALID_TYPE_ID) {
        return eval_error("out of memory while registering abstract type");
    }

    // Return type name as symbol
    Value* result = make_symbol(interp, da.name);
    return eval_ok(result);
}

fn void eval_defunion_init_union_info(TypeInfo* union_info, ExprDefUnion* du) {
    union_info.name = du.name;
    union_info.kind = TK_UNION;
    union_info.parent = INVALID_TYPE_ID;
    union_info.fields = null;
    union_info.field_count = 0;
    union_info.type_params = null;
    union_info.type_param_count = du.type_param_count;
    union_info.constraints = null;
    union_info.constraint_count = 0;
    union_info.variants = null;
    union_info.variant_count = du.variant_count;
    union_info.alias_target = INVALID_TYPE_ID;
}

fn TypeId eval_defunion_register_variant_type(UnionVariant* variant, TypeId union_id, Interp* interp) {
    TypeInfo var_info;
    var_info.name = variant.name;
    var_info.kind = TK_CONCRETE;
    var_info.parent = union_id;  // variant is subtype of union
    var_info.fields = null;
    var_info.field_count = variant.field_count;
    var_info.type_params = null;
    var_info.type_param_count = 0;
    var_info.constraints = null;
    var_info.constraint_count = 0;
    var_info.variants = null;
    var_info.variant_count = 0;
    var_info.alias_target = INVALID_TYPE_ID;
    if (!eval_type_info_alloc_fields(&var_info, variant.field_count)) {
        return INVALID_TYPE_ID;
    }
    for (usz j = 0; j < variant.field_count; j++) {
        var_info.fields[j].name = variant.fields[j];
        var_info.fields[j].field_type = INVALID_TYPE_ID;
        var_info.fields[j].annotation_sym = 0;
    }
    return interp.types.register_type(var_info, &interp.symbols);
}

fn EvalResult eval_defunion_bind_variant(UnionVariant* variant, TypeId var_id, Interp* interp) {
    if (variant.field_count == 0) {
        // Nullary variant: define as a constant instance in root_scope
        Value* inst = make_instance_root(interp, var_id, null, 0);
        if (inst == null) {
            char[256] ebuf;
            char[] msg = "failed to allocate union variant";
            if (try char[] formatted = io::bprintf(&ebuf, "failed to allocate union variant '%s'",
                (ZString)interp.symbols.get_name(variant.name))) {
                msg = formatted;
            }
            return eval_error(msg);
        }
        if (!env_define_with_barrier_checked(interp.global_env, variant.name, inst, interp)) {
            return eval_error("out of memory while registering union variant");
        }
        return eval_ok(make_nil(interp));
    }

    // Variant with fields: define as constructor primitive (make_primitive uses root_scope)
    char[] vname = interp.symbols.get_name(variant.name);
    Value* ctor = make_primitive(interp, vname, &prim_type_constructor, (int)variant.field_count);
    if (ctor == null) {
        return eval_error("out of memory while registering union variant");
    }
    ctor.prim_val.tag = (int)var_id;
    if (!env_define_with_barrier_checked(interp.global_env, variant.name, ctor, interp)) {
        return eval_error("out of memory while registering union variant");
    }
    return eval_ok(make_nil(interp));
}

fn EvalResult eval_defunion(Expr* expr, Env* env, Interp* interp) {
    ExprDefUnion* du = expr.defunion;

    // Register the union type itself
    TypeInfo union_info;
    eval_defunion_init_union_info(&union_info, du);
    if (!eval_type_info_copy_symbol_array(&union_info.type_params, du.type_params, du.type_param_count) ||
        !eval_type_info_copy_union_variants(&union_info, du.variants, du.variant_count)) {
        if (union_info.type_params != null) mem::free(union_info.type_params);
        return eval_error("out of memory while registering union type");
    }

    TypeId union_id = interp.types.register_type(union_info, &interp.symbols);
    if (union_id == INVALID_TYPE_ID) {
        return eval_error("out of memory while registering union type");
    }

    // Register each variant as its own type (with union as parent)
    for (usz i = 0; i < du.variant_count; i++) {
        UnionVariant* variant = &du.variants[i];
        TypeId var_id = eval_defunion_register_variant_type(variant, union_id, interp);
        if (var_id == INVALID_TYPE_ID) {
            return eval_error("out of memory while registering union variant");
        }
        EvalResult bind_result = eval_defunion_bind_variant(variant, var_id, interp);
        if (bind_result.error.has_error) {
            return bind_result;
        }
    }

    Value* result = make_symbol(interp, du.name);
    return eval_ok(result);
}

fn EvalResult eval_defalias(Expr* expr, Env* env, Interp* interp) {
    ExprDefAlias* da = &expr.defalias;

    TypeInfo info;
    info.name = da.name;
    info.kind = TK_ALIAS;
    info.parent = INVALID_TYPE_ID;
    info.fields = null;
    info.field_count = 0;
    info.type_params = null;
    info.type_param_count = 0;
    info.constraints = null;
    info.constraint_count = 0;
    info.variants = null;
    info.variant_count = 0;

    // Resolve target
    if (da.target.has_annotation) {
        info.alias_target = interp.types.lookup(da.target.base_type, &interp.symbols);
    } else {
        info.alias_target = INVALID_TYPE_ID;
    }

    TypeId type_id = interp.types.register_type(info, &interp.symbols);
    if (type_id == INVALID_TYPE_ID) {
        return eval_error("out of memory while registering type alias");
    }

    Value* result = make_symbol(interp, da.name);
    return eval_ok(result);
}

fn EvalResult eval_defeffect(Expr* expr, Env* env, Interp* interp) {
    ExprDefEffect* de = &expr.defeffect;

    TypeInfo info;
    info.name = de.name;
    info.kind = TK_EFFECT;
    info.parent = INVALID_TYPE_ID;
    info.fields = null;
    info.type_params = null;
    info.constraints = null;
    info.variants = null;
    info.type_param_count = 0;
    info.constraint_count = 0;
    info.variant_count = 0;
    info.alias_target = INVALID_TYPE_ID;

    if (de.has_arg_type) {
        if (!eval_type_info_alloc_fields(&info, 1)) {
            return eval_error("out of memory while registering effect type");
        }
        info.field_count = 1;
        info.fields[0].name = interp.symbols.intern("arg");
        if (de.arg_type.has_annotation) {
            TypeId resolved = interp.types.lookup(de.arg_type.base_type, &interp.symbols);
            info.fields[0].field_type = resolved;
            info.fields[0].annotation_sym = de.arg_type.base_type;
        } else {
            info.fields[0].field_type = INVALID_TYPE_ID;
            info.fields[0].annotation_sym = (SymbolId)0;
        }
    } else {
        info.fields = null;
        info.field_count = 0;
    }

    TypeId type_id = interp.types.register_type(info, &interp.symbols);
    if (type_id == INVALID_TYPE_ID) {
        return eval_error("out of memory while registering effect type");
    }

    // Bind the declared effect name in the current environment so real module
    // members like ui.open resolve to the canonical symbol tag value.
    if (!env_define_with_barrier_checked(env, de.name, make_symbol(interp, de.name), interp)) {
        return eval_error("out of memory while registering effect type");
    }

    return eval_ok(make_nil(interp));
}

// =============================================================================
