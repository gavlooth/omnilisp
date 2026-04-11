module lisp;

import main;

// SECTION 2.35A: TYPE DECLARATION HELPERS
// =============================================================================

fn bool eval_type_info_copy_symbol_array(SymbolId** out, SymbolId* src, usz count) {
    if (count == 0) {
        *out = null;
        return true;
    }
    if (count > usz.max / SymbolId.sizeof) return false;
    SymbolId* buf = (SymbolId*)mem::malloc(SymbolId.sizeof * count);
    if (buf == null) return false;
    for (usz i = 0; i < count; i++) buf[i] = src[i];
    *out = buf;
    return true;
}

fn bool eval_type_info_alloc_fields(TypeInfo* info, usz count) {
    if (count == 0) {
        info.fields = null;
        return true;
    }
    if (count > usz.max / TypeFieldInfo.sizeof) return false;
    info.fields = (TypeFieldInfo*)mem::malloc(TypeFieldInfo.sizeof * count);
    return info.fields != null;
}

fn bool eval_type_info_alloc_constraints(TypeInfo* info, usz count) {
    if (count == 0) {
        info.constraints = null;
        return true;
    }
    if (count > usz.max / MethodConstraint.sizeof) return false;
    info.constraints = (MethodConstraint*)mem::malloc(MethodConstraint.sizeof * count);
    return info.constraints != null;
}

fn bool eval_deftype_constraint_capacity(ExprDefType* dt, usz* out) {
    if (out == null) return false;
    *out = 0;
    usz count = 0;
    for (usz i = 0; i < dt.field_count; i++) {
        usz meta_count = dt.fields[i].type_ann.meta_count;
        if (meta_count > usz.max - count) return false;
        count += meta_count;
    }
    *out = count;
    return true;
}

fn bool eval_type_info_copy_union_variants(TypeInfo* info, UnionVariant* src, usz count) {
    if (count == 0) {
        info.variants = null;
        return true;
    }
    if (count > usz.max / UnionVariant.sizeof) return false;
    info.variants = (UnionVariant*)mem::malloc(UnionVariant.sizeof * count);
    if (info.variants == null) return false;
    for (usz i = 0; i < count; i++) {
        info.variants[i].name = src[i].name;
        info.variants[i].field_count = src[i].field_count;
        if (!eval_type_info_copy_symbol_array(&info.variants[i].fields, src[i].fields, src[i].field_count)) {
            for (usz j = 0; j < i; j++) if (info.variants[j].fields != null) mem::free(info.variants[j].fields);
            mem::free(info.variants);
            info.variants = null;
            return false;
        }
    }
    return true;
}

fn void eval_deftype_init_type_info(TypeInfo* info, ExprDefType* dt) {
    info.name = dt.name;
    info.kind = TK_CONCRETE;
    info.fields = null;
    info.field_count = dt.field_count;
    info.type_params = null;
    info.type_param_count = dt.type_param_count;
    info.constraints = null;
    info.variants = null;
    info.variant_count = 0;
    info.alias_target = INVALID_TYPE_ID;
    info.parent = INVALID_TYPE_ID;
    info.constraint_count = 0;
}

fn bool eval_deftype_resolve_parent_and_params(TypeInfo* info, ExprDefType* dt, Interp* interp) {
    info.type_param_count = 0;
    if (dt.type_param_count == 0) return true;

    TypeId maybe_parent = interp.types.lookup(dt.type_params[0], &interp.symbols);
    if (maybe_parent != INVALID_TYPE_ID) {
        // First symbol is a known type — it's the parent.
        info.parent = maybe_parent;
        // Remaining symbols are actual type params.
        info.type_param_count = dt.type_param_count - 1;
        return eval_type_info_copy_symbol_array(&info.type_params, &dt.type_params[1], info.type_param_count);
    }

    // Not a known type — all are type params, no parent.
    info.type_param_count = dt.type_param_count;
    return eval_type_info_copy_symbol_array(&info.type_params, dt.type_params, info.type_param_count);
}

fn void eval_deftype_apply_dict_annotation(TypeInfo* info, TypeField* field, TypeFieldInfo* out_field, Interp* interp) {
    // Dictionary annotation like ^{'T Number} — extract constraints.
    out_field.field_type = INVALID_TYPE_ID;
    out_field.annotation_sym = 0;
    for (usz m = 0; m < field.type_ann.meta_count; m++) {
        if (field.type_ann.meta[m].is_int) continue;
        info.constraints[info.constraint_count].param_sym = field.type_ann.meta[m].key;
        info.constraints[info.constraint_count].bound_type =
            interp.types.lookup(field.type_ann.meta[m].sym_value, &interp.symbols);
        // Also store the param_sym as annotation_sym for type-arg inference.
        out_field.annotation_sym = field.type_ann.meta[m].key;
        info.constraint_count++;
    }
}

fn void eval_deftype_copy_fields(TypeInfo* info, ExprDefType* dt, Interp* interp) {
    for (usz i = 0; i < dt.field_count; i++) {
        TypeField* src_field = &dt.fields[i];
        TypeFieldInfo* dst_field = &info.fields[i];
        dst_field.name = src_field.name;

        if (!src_field.type_ann.has_annotation) {
            dst_field.field_type = INVALID_TYPE_ID;  // Any
            dst_field.annotation_sym = 0;
            continue;
        }

        if (src_field.type_ann.is_dict) {
            eval_deftype_apply_dict_annotation(info, src_field, dst_field, interp);
            continue;
        }

        dst_field.field_type = interp.types.lookup(src_field.type_ann.base_type, &interp.symbols);
        dst_field.annotation_sym = src_field.type_ann.base_type;
    }
}

fn EvalResult eval_deftype(Expr* expr, Env* env, Interp* interp) {
    ExprDefType* dt = expr.deftype;

    // Build TypeInfo
    TypeInfo info;
    eval_deftype_init_type_info(&info, dt);

    // Disambiguate parent vs type-params from collected symbols.
    // If the first symbol is a registered type, it's the parent; rest are type params.
    // Otherwise, all symbols are type params and there is no parent.
    if (!eval_deftype_resolve_parent_and_params(&info, dt, interp)) {
        return eval_error("out of memory while registering type");
    }

    if (!eval_type_info_alloc_fields(&info, dt.field_count)) {
        if (info.type_params != null) mem::free(info.type_params);
        return eval_error("out of memory while registering type");
    }
    usz constraint_capacity = 0;
    if (!eval_deftype_constraint_capacity(dt, &constraint_capacity)) {
        if (info.fields != null) mem::free(info.fields);
        if (info.type_params != null) mem::free(info.type_params);
        return eval_error("out of memory while registering type");
    }
    if (!eval_type_info_alloc_constraints(&info, constraint_capacity)) {
        if (info.fields != null) mem::free(info.fields);
        if (info.type_params != null) mem::free(info.type_params);
        return eval_error("out of memory while registering type");
    }

    // Copy fields and store original annotation symbol for type-param mapping
    eval_deftype_copy_fields(&info, dt, interp);

    // Register in type registry
    TypeId existing_type_id = interp.types.lookup(dt.name, &interp.symbols);
    TypeId type_id = interp.types.register_type(info, &interp.symbols);
    if (type_id == INVALID_TYPE_ID) {
        type_registry_destroy_type_info(&info);
        return eval_error("out of memory while registering type");
    }

    // Create constructor primitive in root_scope (lives permanently in global env)
    if (dt.field_count > (usz)int.max) {
        if (existing_type_id == INVALID_TYPE_ID) interp.types.rollback_recent_new_type(type_id, &interp.symbols);
        return eval_error("type constructor arity exceeds supported limit");
    }
    char[] name_str = interp.symbols.get_name(dt.name);
    Value* constructor = make_primitive(interp, name_str, &prim_type_constructor, (int)dt.field_count);
    if (constructor == null) {
        if (existing_type_id == INVALID_TYPE_ID) interp.types.rollback_recent_new_type(type_id, &interp.symbols);
        return eval_error("out of memory while registering type");
    }
    constructor.prim_val.tag = (int)type_id;

    // Also make type info available as a value
    Value* type_val = interp.alloc_value();
    if (type_val == null) {
        if (existing_type_id == INVALID_TYPE_ID) interp.types.rollback_recent_new_type(type_id, &interp.symbols);
        return eval_error("out of memory while registering type");
    }
    type_val.tag = TYPE_INFO;
    type_val.type_info_val = &interp.types.types[(usz)type_id];

    // Define constructor in global env
    if (!env_define_with_barrier_checked(interp.global_env, dt.name, constructor, interp)) {
        if (existing_type_id == INVALID_TYPE_ID) interp.types.rollback_recent_new_type(type_id, &interp.symbols);
        return eval_error("out of memory while registering type");
    }

    return eval_ok(constructor);
}

// =============================================================================
