module lisp;

import std::core::mem;
import main;

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
 * Example: (type-args (Box 42)) => '(Integer)
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
    if (v == null) return make_void(interp);

    // Free backing storage and mark as error.
    // Array and HashMap payloads are malloc-backed; Instance payloads are
    // scope-owned and must release their owner scope instead of raw-freeing.
    switch (v.tag) {
        case ARRAY:
            if (v.array_val != null) {
                if (v.array_val.items != null) mem::free(v.array_val.items);
                mem::free(v.array_val);  // Array struct is malloc'd in make_array
                v.array_val = null;
            }
        case TENSOR:
            if (v.tensor_val != null) {
                tensor_free_payload(v.tensor_val);
                v.tensor_val = null;
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
            // Instance payloads are owned by a dedicated scope.
            if (v.instance_val != null) {
                instance_release(v.instance_val);
                v.instance_val = null;
            }
        case STRING:
        case ERROR:
            if (v.string_heap_owned && v.str_chars != null) {
                mem::free(v.str_chars);
                v.str_chars = null;
            }
            v.string_heap_owned = false;
        default: return make_void(interp);  // No backing to free
    }
    // Mark as error so use-after-free is caught instead of silently becoming nil
    v.tag = ERROR;
    char[] msg = "use after unsafe-free!";
    StringVal* err_builder = strval_new_from_slice(msg);
    if (err_builder == null) {
        v.str_chars = null;
        v.str_len = 0;
        v.string_heap_owned = false;
        return make_void(interp);
    }
    strval_into_value(err_builder, v);
    return make_void(interp);
}

// =============================================================================
// ITERATOR PRIMITIVES
// =============================================================================
