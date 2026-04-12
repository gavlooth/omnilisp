module lisp;

import std::io;
import main;

fn Value* constructor_arity_error(TypeInfo* ti, usz got, Interp* interp) {
    char[256] ebuf;
    char[] type_n = interp.symbols.get_name(ti.name);
    char[] msg;
    if (try formatted = io::bprintf(&ebuf, "%s constructor expects %d argument(s), got %d",
        (ZString)type_n, (int)ti.field_count, (int)got)) {
        msg = formatted;
    } else {
        msg = "type constructor arity mismatch";
    }
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
            char[] emsg;
            if (try formatted = io::bprintf(&ebuf, "type error: field '%s' expects %s, got %s",
                (ZString)field_name, (ZString)expected_name, (ZString)actual_name)) {
                emsg = formatted;
            } else {
                emsg = "type error: constructor field type mismatch";
            }
            return raise_error(interp, emsg);
        }
    }
    return null;
}

fn bool infer_instance_type_args(Instance* inst, TypeInfo* ti, Value*[] args, Interp* interp) {
    inst.type_arg_count = ti.type_param_count;

    if (ti.type_param_count > 0 && ti.type_param_count > usz.max / TypeId.sizeof) {
        return false;
    }

    inst.type_args = ti.type_param_count > 0
        ? (TypeId*)inst.owner_scope.alloc(TypeId.sizeof * ti.type_param_count)
        : null;
    if (inst.type_args == null && ti.type_param_count > 0) {
        return false;
    }

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

    return true;
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
                        TypeInfo* bound_info = interp.types.get(bound);
                        TypeInfo* inferred_info = interp.types.get(inferred);
                        char[] bound_name = bound_info != null ? interp.symbols.get_name(bound_info.name) : "<invalid-type-id>";
                        char[] actual_name = inferred_info != null ? interp.symbols.get_name(inferred_info.name) : "<invalid-type-id>";
                        char[] emsg;
                        if (try formatted = io::bprintf(&ebuf, "type constraint violated: %s must be subtype of %s, got %s",
                            (ZString)param_name, (ZString)bound_name, (ZString)actual_name)) {
                            emsg = formatted;
                        } else {
                            emsg = "type constraint violated";
                        }
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
        if (!infer_instance_type_args(inst, ti, args, interp)) {
            return make_error(interp, "type constructor: failed to infer constructor type args");
        }
        Value* constraint_error = validate_instance_constraints(inst, ti, interp);
        if (constraint_error != null) return constraint_error;
    }

    return result;
}
