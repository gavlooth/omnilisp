module lisp;

import std::core::mem;
import std::io;
import main;

// Macro/eval/type/meta primitives and memory free primitive.
fn Value* prim_is_boolean(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "boolean?: expected 1 argument");
    if (is_nil(args[0])) return make_symbol(interp, interp.sym_true); // false is nil
    if (is_symbol(args[0]) && args[0].sym_val == interp.sym_true) return make_symbol(interp, interp.sym_true);
    return make_nil(interp);
}

fn Value* prim_is_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "list?: expected 1 argument");
    Value* v = args[0];
    if (is_nil(v)) return make_symbol(interp, interp.sym_true); // empty list is a list
    // Check if proper list (nil-terminated cons chain)
    while (is_cons(v)) { v = v.cons_val.cdr; }
    return is_nil(v) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_procedure(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "procedure?: expected 1 argument");
    ValueTag t = args[0].tag;
    return (t == CLOSURE || t == PRIMITIVE || t == PARTIAL_PRIM || t == METHOD_TABLE || t == CONTINUATION) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_zero(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "zero?: expected number");
    return to_double(args[0]) == 0.0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_positive(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "positive?: expected number");
    return to_double(args[0]) > 0.0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_negative(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "negative?: expected number");
    return to_double(args[0]) < 0.0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_even(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_int(args[0])) return raise_error(interp, "even?: expected integer");
    return (args[0].int_val % 2 == 0) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_is_odd(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_int(args[0])) return raise_error(interp, "odd?: expected integer");
    return (args[0].int_val % 2 != 0) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_time_point_p(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "time-point?: expected 1 argument");
    return is_time_point(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

// --- Phase 7: Macroexpand, eval, apply, bound? ---

fn Value* prim_macroexpand(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "macroexpand: expected quoted expression");
    Value* form = args[0];
    // Form should be a list (macro-name arg...)
    if (!is_cons(form)) return form; // not a macro call, return as-is
    Value* head = form.cons_val.car;
    if (head == null || !is_symbol(head)) return form;
    SymbolId sym = head.sym_val;
    // Check macro table using hash index
    usz mh = (usz)((uint)sym * 2654435761);
    usz mmask = interp.macro_hash_capacity - 1;
    for (usz probe = 0; probe < interp.macro_hash_capacity; probe++) {
        usz slot = (mh + probe) & mmask;
        usz midx = interp.macro_hash_index[slot];
        if (midx == usz.max) break; // empty slot
        if ((uint)interp.macro_table[midx].name == (uint)sym) {
            Expr* call_expr = value_to_expr(form, interp);
            if (call_expr == null) return form;
            EvalResult r = expand_pattern_macro(&interp.macro_table[midx], call_expr, interp);
            if (!r.error.has_error && r.value != null) return r.value;
            return form;
        }
    }
    return form;
}

fn Value* prim_eval(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "eval: expected expression");
    Expr* expr = value_to_expr(args[0], interp);
    if (expr == null) return raise_error(interp, "eval: could not convert to expression");
    Value* result = jit_eval(expr, interp.global_env, interp);
    if (result != null && result.tag == ERROR) return raise_error(interp, "eval: error during evaluation");
    return result;
}

fn Value* prim_apply(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "apply: expected function and argument list");
    Value* func = args[0];
    Value* arg_list = args[1];
    // Count args
    usz arg_count = 0;
    Value* cur = arg_list;
    while (is_cons(cur)) {
        arg_count++;
        cur = cur.cons_val.cdr;
    }
    // Use multi-arg apply (handles multi-param closures, variadics, etc.)
    return jit_apply_multi_args(interp, func, arg_list, arg_count);
}

fn Value* prim_bound(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_symbol(args[0])) return raise_error(interp, "bound?: expected symbol");
    SymbolId sym = args[0].sym_val;
    Env* lookup_env = env != null ? env : interp.global_env;
    Value* v = lookup_env.lookup(sym);
    return (v != null) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_error(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return raise_error(interp, "error: expected string message");
    return raise_error(interp, args[0].str_chars[:args[0].str_len]);
}

fn Value* prim_error_message(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != ERROR) return raise_error(interp, "error-message: expected error value");
    return make_string(interp, args[0].str_chars[:args[0].str_len]);
}

/// =============================================================================
// SECTION 7.5: TYPE SYSTEM PRIMITIVES
// =============================================================================

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
        if (expected_tid != INVALID_TYPE_ID) return actual_tid == expected_tid;
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

/**
 * (^ type-form value) -> value
 * Explicit constructor annotation checker for forms like:
 *   (^(Box Int) (Box 42))
 */
fn Value* prim_ctor_type_apply(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return ctor_type_raise(interp, "type/ctor-arity-mismatch", "^: expected type form and value", null);
    }

    CtorTypeForm expected = {};
    if (!ctor_form_parse(args[0], &expected)) {
        return ctor_type_raise(interp, "type/ctor-type-arg-mismatch",
            "^: expected constructor type form (symbol or list)", null);
    }
    defer ctor_form_destroy(&expected);

    Value* value = args[1];
    if (value == null || value.tag != INSTANCE || value.instance_val == null) {
        Value* expected_args = ctor_expected_args_to_list(expected.args, expected.arg_count, interp);
        Value* actual_args = make_nil(interp);
        Value* data = ctor_mismatch_data(interp, expected.ctor, expected_args, actual_args, false, 0, null);
        char[256] buf;
        char[] msg = io::bprintf(&buf, "^: expected instance of %s",
            (ZString)interp.symbols.get_name(expected.ctor))!!;
        return ctor_type_raise(interp, "type/ctor-type-arg-mismatch", msg, data);
    }

    Instance* inst = value.instance_val;
    TypeInfo* actual_ti = interp.types.get(inst.type_id);
    if (actual_ti == null) {
        return ctor_type_raise(interp,
            "type/ctor-missing-instance-type-info",
            "internal error: missing instance type info",
            null);
    }

    Value* expected_args = ctor_expected_args_to_list(expected.args, expected.arg_count, interp);
    Value* actual_args = ctor_type_args_to_symbol_list(inst.type_args, inst.type_arg_count, interp);

    if ((uint)actual_ti.name != (uint)expected.ctor) {
        Value* data = ctor_mismatch_data(interp, expected.ctor, expected_args, actual_args, false, 0, null);
        char[256] buf;
        char[] msg = io::bprintf(&buf, "^: constructor mismatch, expected %s got %s",
            (ZString)interp.symbols.get_name(expected.ctor),
            (ZString)interp.symbols.get_name(actual_ti.name))!!;
        return ctor_type_raise(interp, "type/ctor-type-arg-mismatch", msg, data);
    }

    if (expected.arg_count != inst.type_arg_count) {
        Value* data = ctor_mismatch_data(interp, expected.ctor, expected_args, actual_args, false, 0, null);
        char[256] buf;
        char[] msg = io::bprintf(&buf, "^: %s expected %d type arg(s), got %d",
            (ZString)interp.symbols.get_name(expected.ctor),
            (int)expected.arg_count,
            (int)inst.type_arg_count)!!;
        return ctor_type_raise(interp, "type/ctor-arity-mismatch", msg, data);
    }

    CtorArgMismatchPath mismatch = {};
    for (usz i = 0; i < expected.arg_count; i++) {
        Value* runtime_field = ctor_type_param_runtime_value(inst, actual_ti, i);
        if (!ctor_match_type_form(expected.args[i], inst.type_args[i], runtime_field, interp, &mismatch, 0)) {
            ctor_path_prepend(&mismatch, i);
            usz arg_index = mismatch.len > 0 ? mismatch.items[0] : i;
            Value* data = ctor_mismatch_data(interp, expected.ctor, expected_args, actual_args, true, arg_index, &mismatch);
            char[256] buf;
            char[] msg = io::bprintf(&buf, "^: %s type arg mismatch at index %d",
                (ZString)interp.symbols.get_name(expected.ctor), (int)arg_index)!!;
            return ctor_type_raise(interp, "type/ctor-type-arg-mismatch", msg, data);
        }
    }

    return value;
}

/**
 * (type-of v) -> symbol representing the type name
 */
fn Value* prim_type_of(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "type-of: expected 1 argument");
    SymbolId type_name = value_type_name(args[0], interp);
    return make_symbol(interp, type_name);
}

/**
 * (is? v 'TypeName) -> true if v's type is or is subtype of TypeName
 */
fn Value* prim_is_type(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "is?: expected 2 arguments");
    if (args[1] == null || args[1].tag != SYMBOL) {
        return raise_error(interp, "is?: second argument must be a symbol");
    }

    SymbolId target_name = args[1].sym_val;
    TypeId target_type = interp.types.lookup(target_name, &interp.symbols);
    if (target_type == INVALID_TYPE_ID) {
        return make_nil(interp);  // Unknown type
    }

    TypeId value_type = infer_value_type(args[0], interp);
    if (value_type == INVALID_TYPE_ID) {
        return make_nil(interp);
    }

    // Check for Any type (everything matches)
    TypeId any_type = interp.types.lookup(interp.sym_Any, &interp.symbols);
    if (target_type == any_type) {
        return make_symbol(interp, interp.sym_true);
    }

    // Check exact match or subtype
    if (interp.types.is_subtype(value_type, target_type)) {
        return make_symbol(interp, interp.sym_true);
    }

    return make_nil(interp);
}

/**
 * (instance? v) -> true if v is a user-defined type instance
 */
fn Value* prim_is_instance(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "instance?: expected 1 argument");
    bool result = args[0].tag == INSTANCE;
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

/**
 * (type-args v) -> list of inferred type argument symbols, or nil if not parametric.
 * Example: (type-args (Box 42)) => '(Int)
 */
fn Value* prim_type_args(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "type-args: expected 1 argument");
    if (args[0] == null || args[0].tag != INSTANCE) return make_nil(interp);
    Instance* inst = args[0].instance_val;
    if (inst == null || inst.type_arg_count == 0) return make_nil(interp);

    // Build a list of type name symbols from the inferred type args
    Value* result = make_nil(interp);
    // Build in reverse so first type arg is at head
    for (usz i = inst.type_arg_count; i > 0; i--) {
        TypeId tid = inst.type_args[i - 1];
        if (tid != INVALID_TYPE_ID) {
            TypeInfo* ti = interp.types.get(tid);
            if (ti != null) {
                result = make_cons(interp, make_symbol(interp, ti.name), result);
            } else {
                result = make_cons(interp, make_nil(interp), result);
            }
        } else {
            result = make_cons(interp, make_nil(interp), result);
        }
    }
    return result;
}

// --- Memory reclamation ---

fn Value* prim_free_bang(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "unsafe-free!: expected 1 argument");
    Value* v = args[0];
    if (v == null) return make_nil(interp);

    // Free heap-allocated backing storage and mark as error.
    // Note: Array struct is malloc'd, but HashMap/Instance structs are region-allocated.
    // Only free what was actually malloc'd.
    switch (v.tag) {
        case ARRAY:
            if (v.array_val != null) {
                if (v.array_val.items != null) mem::free(v.array_val.items);
                mem::free(v.array_val);  // Array struct is malloc'd in make_array
                v.array_val = null;
            }
        case HASHMAP:
            // HashMap struct is region-allocated, only entries buffer is malloc'd
            if (v.hashmap_val != null) {
                if (v.hashmap_val.entries != null) {
                    mem::free(v.hashmap_val.entries);
                    v.hashmap_val.entries = null;
                }
                v.hashmap_val.count = 0;
                v.hashmap_val.capacity = 0;
            }
        case INSTANCE:
            // Instance struct is malloc'd in make_instance
            if (v.instance_val != null) {
                mem::free(v.instance_val);
                v.instance_val = null;
            }
        case STRING:
        case ERROR:
            if (v.str_chars != null) {
                mem::free(v.str_chars);
                v.str_chars = null;
            }
        default: return make_nil(interp);  // No backing to free
    }
    // Mark as error so use-after-free is caught instead of silently becoming nil
    v.tag = ERROR;
    char[] msg = "use after unsafe-free!";
    StringVal* err_builder = strval_new(msg.len + 1);
    for (usz i = 0; i < msg.len; i++) {
        err_builder.chars[i] = msg[i];
    }
    err_builder.chars[msg.len] = 0;
    err_builder.len = msg.len;
    strval_into_value(err_builder, v);
    return make_nil(interp);
}

// =============================================================================
// ITERATOR PRIMITIVES
// =============================================================================
