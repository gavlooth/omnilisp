module lisp;

import std::io;
import main;

// SECTION 2.35B: TYPE CONSTRUCTOR + OTHER TYPE EVALUATORS
// =============================================================================

/**
 * Generic type constructor primitive.
 * Uses interp.constructor_type_id (set before each call) to know which type to construct.
 */
fn Value* constructor_arity_error(TypeInfo* ti, usz got, Interp* interp) {
    char[256] ebuf;
    char[] type_n = interp.symbols.get_name(ti.name);
    char[] msg = io::bprintf(&ebuf, "%s constructor expects %d argument(s), got %d",
        (ZString)type_n, (int)ti.field_count, (int)got)!!;
    return raise_error(interp, msg);
}

fn bool is_type_param_annotation(TypeInfo* ti, SymbolId annotation_sym) {
    if (annotation_sym == (SymbolId)0) return false;
    for (usz p = 0; p < ti.type_param_count; p++) {
        if (ti.type_params[p] == annotation_sym) return true;
    }
    return false;
}

fn Value* validate_constructor_fields(Value*[] args, TypeInfo* ti, Interp* interp) {
    for (usz i = 0; i < args.len && i < ti.field_count; i++) {
        TypeId expected = ti.fields[i].field_type;
        if (expected == INVALID_TYPE_ID) continue;
        if (is_type_param_annotation(ti, ti.fields[i].annotation_sym)) continue;

        TypeId actual = infer_value_type(args[i], interp);
        if (actual == INVALID_TYPE_ID) continue;
        if (actual != expected && !interp.types.is_subtype(actual, expected)) {
            char[128] ebuf;
            char[] expected_name = interp.symbols.get_name(ti.fields[i].annotation_sym);
            char[] field_name = interp.symbols.get_name(ti.fields[i].name);
            char[] actual_name = interp.symbols.get_name(value_type_name(args[i], interp));
            char[] emsg = io::bprintf(&ebuf, "type error: field '%s' expects %s, got %s",
                (ZString)field_name, (ZString)expected_name, (ZString)actual_name)!!;
            return raise_error(interp, emsg);
        }
    }
    return null;
}

fn void infer_instance_type_args(Instance* inst, TypeInfo* ti, Value*[] args, Interp* interp) {
    inst.type_arg_count = ti.type_param_count;
    inst.type_args = ti.type_param_count > 0
        ? (TypeId*)inst.owner_scope.alloc(TypeId.sizeof * ti.type_param_count)
        : null;
    for (usz p = 0; p < ti.type_param_count; p++) {
        SymbolId param_sym = ti.type_params[p];
        inst.type_args[p] = INVALID_TYPE_ID;
        for (usz f = 0; f < ti.field_count; f++) {
            if (ti.fields[f].annotation_sym == param_sym && f < args.len) {
                inst.type_args[p] = infer_value_type(args[f], interp);
                break;
            }
        }
    }
}

fn Value* validate_instance_constraints(Instance* inst, TypeInfo* ti, Interp* interp) {
    for (usz c = 0; c < ti.constraint_count; c++) {
        SymbolId param = ti.constraints[c].param_sym;
        TypeId bound = ti.constraints[c].bound_type;
        if (bound == INVALID_TYPE_ID) continue;
        for (usz p = 0; p < ti.type_param_count; p++) {
            if (ti.type_params[p] == param) {
                TypeId inferred = inst.type_args[p];
                if (inferred != INVALID_TYPE_ID) {
                    if (inferred != bound && !interp.types.is_subtype(inferred, bound)) {
                        char[128] ebuf;
                        char[] param_name = interp.symbols.get_name(param);
                        char[] bound_name = interp.symbols.get_name(interp.types.types[(usz)bound].name);
                        char[] actual_name = interp.symbols.get_name(interp.types.types[(usz)inferred].name);
                        char[] emsg = io::bprintf(&ebuf, "type constraint violated: %s must be subtype of %s, got %s",
                            (ZString)param_name, (ZString)bound_name, (ZString)actual_name)!!;
                        return raise_error(interp, emsg);
                    }
                }
                break;
            }
        }
    }
    return null;
}

fn Value* prim_type_constructor(Value*[] args, Env* env, Interp* interp) {
    TypeId type_id = (TypeId)interp.constructor_type_id;
    TypeInfo* ti = interp.types.get(type_id);
    if (ti == null) return make_error(interp, "internal error: type not found in constructor");

    if (args.len != ti.field_count) {
        return constructor_arity_error(ti, args.len, interp);
    }

    Value* field_error = validate_constructor_fields(args, ti, interp);
    if (field_error != null) return field_error;

    Value*[] fields = args.len > 0 ? mem::new_array(Value*, args.len) : {};
    if (args.len > 0 && fields.ptr == null) {
        return make_error(interp, "type constructor: failed to allocate field buffer");
    }
    defer if (fields.ptr != null) mem::free(fields.ptr);
    for (usz i = 0; i < args.len; i++) {
        fields[i] = args[i];
    }
    Value* result = make_instance(interp, type_id, fields, args.len);
    if (result == null) {
        return make_error(interp, "type constructor: failed to allocate instance");
    }

    if (ti.type_param_count > 0 && result.instance_val != null) {
        Instance* inst = result.instance_val;
        infer_instance_type_args(inst, ti, args, interp);
        Value* constraint_error = validate_instance_constraints(inst, ti, interp);
        if (constraint_error != null) return constraint_error;
    }

    return result;
}

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
            char[] msg = io::bprintf(&ebuf, "unknown parent type '%s'",
                (ZString)interp.symbols.get_name(da.parent))!!;
            return eval_error(msg);
        }
    } else {
        info.parent = INVALID_TYPE_ID;
    }

    TypeId type_id = interp.types.register_type(info, &interp.symbols);

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
            char[] msg = io::bprintf(&ebuf, "failed to allocate union variant '%s'",
                (ZString)interp.symbols.get_name(variant.name))!!;
            return eval_error(msg);
        }
        env_define_with_barrier(interp.global_env, variant.name, inst, interp);
        return eval_ok(make_nil(interp));
    }

    // Variant with fields: define as constructor primitive (make_primitive uses root_scope)
    char[] vname = interp.symbols.get_name(variant.name);
    Value* ctor = make_primitive(interp, vname, &prim_type_constructor, (int)variant.field_count);
    ctor.prim_val.tag = (int)var_id;
    env_define_with_barrier(interp.global_env, variant.name, ctor, interp);
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

    return eval_ok(make_nil(interp));
}

// =============================================================================
