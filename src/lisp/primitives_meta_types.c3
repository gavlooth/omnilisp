module lisp;

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
    return (t == CLOSURE || t == PRIMITIVE || t == PARTIAL_PRIM || t == METHOD_TABLE) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
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
    StringVal* err_builder = strval_new(msg.len);
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
