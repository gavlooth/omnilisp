module lisp;

import std::core::mem;
import std::io;
import main;

struct CtorTypeForm {
    SymbolId ctor;
    Value** args;
    usz arg_count;
}

struct CtorArgMismatchPath {
    usz len;
    usz[16] items;
}

fn bool ctor_form_parse(Value* form, CtorTypeForm* out) {
    out.ctor = (SymbolId)0;
    out.args = null;
    out.arg_count = 0;

    if (form == null) return false;
    if (form.tag == SYMBOL) {
        out.ctor = form.sym_val;
        return true;
    }
    if (form.tag != CONS) return false;
    if (form.cons_val.car == null || form.cons_val.car.tag != SYMBOL) return false;

    out.ctor = form.cons_val.car.sym_val;

    usz count = 0;
    Value* cursor = form.cons_val.cdr;
    while (cursor != null && cursor.tag == CONS) {
        count++;
        cursor = cursor.cons_val.cdr;
    }
    if (cursor == null || cursor.tag != NIL) return false;

    out.arg_count = count;
    if (count == 0) return true;

    out.args = (Value**)mem::malloc(Value*.sizeof * count);
    if (out.args == null) return false;

    cursor = form.cons_val.cdr;
    for (usz i = 0; i < count; i++) {
        out.args[i] = cursor.cons_val.car;
        cursor = cursor.cons_val.cdr;
    }
    return true;
}

fn void ctor_form_destroy(CtorTypeForm* form) @inline {
    if (form == null) return;
    if (form.args != null) mem::free(form.args);
    form.args = null;
    form.arg_count = 0;
}

fn Value* ctor_type_args_to_symbol_list(TypeId* args, usz count, Interp* interp) {
    Value* out = make_nil(interp);
    for (usz i = count; i > 0; i--) {
        TypeId tid = args[i - 1];
        Value* cell = make_nil(interp);
        if (tid != INVALID_TYPE_ID) {
            TypeInfo* ti = interp.types.get(tid);
            if (ti != null) {
                cell = make_symbol(interp, ti.name);
            }
        }
        out = make_cons(interp, cell, out);
    }
    return out;
}

fn Value* ctor_expected_args_to_list(Value** args, usz count, Interp* interp) {
    Value* out = make_nil(interp);
    for (usz i = count; i > 0; i--) {
        out = make_cons(interp, args[i - 1], out);
    }
    return out;
}

fn Value* ctor_arg_path_to_list(CtorArgMismatchPath* path, Interp* interp) {
    Value* out = make_nil(interp);
    if (path == null) return out;
    for (usz i = path.len; i > 0; i--) {
        out = make_cons(interp, make_int(interp, (long)path.items[i - 1]), out);
    }
    return out;
}

fn void ctor_path_prepend(CtorArgMismatchPath* path, usz index) @inline {
    if (path == null) return;
    if (path.len >= path.items.len) return;
    for (usz i = path.len; i > 0; i--) {
        path.items[i] = path.items[i - 1];
    }
    path.items[0] = index;
    path.len++;
}

fn Value* ctor_mismatch_data(
    Interp* interp,
    SymbolId ctor_sym,
    Value* expected_args,
    Value* actual_args,
    bool include_index,
    usz arg_index,
    CtorArgMismatchPath* path
) {
    Value* data = make_hashmap(interp, 8);
    if (data == null || data.tag != HASHMAP) return null;

    SymbolId key_ctor = interp.symbols.intern("ctor");
    SymbolId key_expected = interp.symbols.intern("expected-args");
    SymbolId key_actual = interp.symbols.intern("actual-args");
    hashmap_set(data.hashmap_val, make_symbol(interp, key_ctor), make_symbol(interp, ctor_sym), interp);
    hashmap_set(data.hashmap_val, make_symbol(interp, key_expected), expected_args, interp);
    hashmap_set(data.hashmap_val, make_symbol(interp, key_actual), actual_args, interp);

    if (include_index) {
        SymbolId key_arg_index = interp.symbols.intern("arg-index");
        SymbolId key_arg_path = interp.symbols.intern("arg-path");
        hashmap_set(data.hashmap_val, make_symbol(interp, key_arg_index), make_int(interp, (long)arg_index), interp);
        hashmap_set(data.hashmap_val, make_symbol(interp, key_arg_path), ctor_arg_path_to_list(path, interp), interp);
    }

    return data;
}

fn Value* ctor_type_raise(Interp* interp, char[] code, char[] message, Value* data = null) @inline {
    return raise_error_with_payload_names(interp, "type", code, message, data);
}

fn Value* ctor_type_param_runtime_value(Instance* inst, TypeInfo* ti, usz type_param_index) {
    if (inst == null || ti == null) return null;
    if (type_param_index >= ti.type_param_count) return null;
    if (inst.field_count == 0 || inst.fields == null) return null;

    SymbolId param_sym = ti.type_params[type_param_index];
    for (usz f = 0; f < ti.field_count && f < inst.field_count; f++) {
        if ((uint)ti.fields[f].annotation_sym == (uint)param_sym) {
            return &inst.fields[f];
        }
    }
    return null;
}

fn bool ctor_match_type_form(
    Value* expected_form,
    TypeId actual_tid,
    Value* actual_value,
    Interp* interp,
    CtorArgMismatchPath* path,
    usz depth
) {
    if (expected_form == null) return false;
    if (depth > 8) return false;

    if (expected_form.tag == SYMBOL) {
        SymbolId expected_sym = expected_form.sym_val;
        if ((uint)expected_sym == (uint)interp.sym_Any) return true;
        if (actual_tid == INVALID_TYPE_ID) return false;
        TypeId expected_tid = interp.types.lookup(expected_sym, &interp.symbols);
        if (expected_tid != INVALID_TYPE_ID) {
            TypeId expected_canonical = interp.types.resolve_alias(expected_tid);
            TypeId actual_canonical = interp.types.resolve_alias(actual_tid);
            if (expected_canonical == INVALID_TYPE_ID || actual_canonical == INVALID_TYPE_ID) return false;
            return actual_canonical == expected_canonical;
        }
        TypeInfo* actual_ti = interp.types.get(actual_tid);
        return actual_ti != null && (uint)actual_ti.name == (uint)expected_sym;
    }

    CtorTypeForm nested = {};
    if (!ctor_form_parse(expected_form, &nested)) return false;
    defer ctor_form_destroy(&nested);

    if (actual_tid == INVALID_TYPE_ID) return false;
    TypeInfo* actual_ti = interp.types.get(actual_tid);
    if (actual_ti == null) return false;
    if ((uint)actual_ti.name != (uint)nested.ctor) return false;

    if (nested.arg_count == 0) return true;

    // Runtime-inference fallback: if nested runtime value doesn't carry
    // parameterized instance info, treat constructor-head match as sufficient.
    if (actual_value == null || actual_value.tag != INSTANCE || actual_value.instance_val == null) return true;

    Instance* nested_inst = actual_value.instance_val;
    if (nested_inst.type_arg_count != nested.arg_count) return false;

    TypeInfo* nested_ti = interp.types.get(nested_inst.type_id);
    if (nested_ti == null) return false;

    for (usz i = 0; i < nested.arg_count; i++) {
        Value* child_runtime = ctor_type_param_runtime_value(nested_inst, nested_ti, i);
        if (!ctor_match_type_form(
            nested.args[i],
            nested_inst.type_args[i],
            child_runtime,
            interp,
            path,
            depth + 1
        )) {
            ctor_path_prepend(path, i);
            return false;
        }
    }

    return true;
}
