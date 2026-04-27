module lisp;

import std::core::mem;
import std::io;
import main;

/// =============================================================================
// SECTION 7.5: TYPE SYSTEM PRIMITIVES
// =============================================================================

/**
 * (type-of v) -> symbol representing the type name
 */
<*
@require interp != null
*>
fn Value* prim_type_of(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arity", "type-of: expected 1 argument");
    SymbolId type_name = value_type_name(args[0], interp);
    return make_symbol(interp, type_name);
}

/**
 * (is? v 'TypeName) -> true if v's type is or is subtype of TypeName
 */
<*
@require interp != null
*>
fn Value* prim_is_type(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return meta_raise(interp, "type/arity", "is?: expected 2 arguments");
    if (args[1] == null || args[1].tag != SYMBOL) {
        return meta_raise(interp, "type/arg-mismatch", "is?: second argument must be a symbol");
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
<*
@require interp != null
*>
fn Value* prim_is_instance(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arity", "instance?: expected 1 argument");
    bool result = args[0].tag == INSTANCE;
    return result ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

/**
 * (type-args v) -> list of inferred type argument symbols, or nil if not parametric.
 * Example: (type-args (Box 42)) => '(Integer)
 */
<*
@require interp != null
*>
fn Value* prim_type_args(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return meta_raise(interp, "type/arity", "type-args: expected 1 argument");
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

fn bool literal_type_name(Value* value, Interp* interp, DString* out_name) {
    if (value == null || out_name == null) return false;

    switch (value.tag) {
        case NIL:
            out_name.append_string("(Literal nil)");
            return true;
        case INT:
            char[64] buf;
            if (try rendered = io::bprintf(&buf, "(Literal %d)", value.int_val)) {
                out_name.append_string((String)rendered);
                return true;
            }
            return false;
        case STRING:
            out_name.append_string("(Literal \"");
            out_name.append_string((String)value.str_chars[:value.str_len]);
            out_name.append_string("\")");
            return true;
        case SYMBOL:
            if ((uint)value.sym_val == (uint)interp.sym_true ||
                (uint)value.sym_val == (uint)interp.sym_false) {
                out_name.append_string("(Literal ");
                out_name.append_string((String)interp.symbols.get_name(value.sym_val));
                out_name.append_char(')');
                return true;
            }
            out_name.append_string("(Literal '");
            out_name.append_string((String)interp.symbols.get_name(value.sym_val));
            out_name.append_char(')');
            return true;
        default:
            return false;
    }
}

fn Value* literal_type_descriptor_for(Value* value, Interp* interp) {
    DString type_name;
    type_name.init(mem);
    defer type_name.free();

    if (!literal_type_name(value, interp, &type_name)) {
        return meta_raise(interp, "type/arg-mismatch",
            "Literal: expected int/string/symbol/bool/nil literal");
    }

    char[] type_name_slice = type_name.str_view();
    SymbolId name = interp.symbols.intern(type_name_slice);
    if (name == INVALID_SYMBOL_ID) {
        return meta_raise(interp, "type/allocation-failed",
            "Literal: failed to intern literal type name");
    }

    TypeId existing = interp.types.lookup(name, &interp.symbols);
    TypeId tid = existing;
    if (tid == INVALID_TYPE_ID) {
        TypeInfo info;
        info.name = name;
        info.kind = TK_BUILTIN;
        TypeId parent = interp.types.lookup(value_type_name(value, interp), &interp.symbols);
        info.parent = parent != INVALID_TYPE_ID ? parent : interp.tid_Literal;
        info.fields = null;
        info.field_count = 0;
        info.type_params = null;
        info.type_param_count = 0;
        info.constraints = null;
        info.constraint_count = 0;
        info.variants = null;
        info.variant_count = 0;
        info.alias_target = INVALID_TYPE_ID;
        tid = interp.types.register_type(info, &interp.symbols);
        if (tid == INVALID_TYPE_ID) {
            return meta_raise(interp, "type/allocation-failed",
                "Literal: failed to register literal singleton type");
        }
    }

    Value* out = interp.alloc_value();
    if (out == null) {
        return meta_raise(interp, "type/allocation-failed",
            "Literal: failed to allocate type descriptor");
    }
    out.tag = TYPE_INFO;
    out.type_info_val = &interp.types.types[(usz)tid];
    return out;
}

/**
 * (Literal datum) -> singleton literal type descriptor.
 *
 * This is a type-level constructor, not a boxed runtime value. For example,
 * (Literal 3) returns #<type (Literal 3)> and ordinary value 3 remains an
 * Integer value.
 */
<*
@require interp != null
*>
fn Value* prim_literal_type(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) {
        return meta_raise(interp, "type/arity", "Literal: expected 1 literal argument");
    }
    return literal_type_descriptor_for(args[0], interp);
}

// --- Memory reclamation ---

<*
@require interp != null
*>
fn Value* prim_free_bang(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return runtime_raise(interp, "type/arity", "unsafe-free!: expected 1 argument");
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
        case BIG_INTEGER:
            if (v.big_integer_val != null) {
                omni_big_integer_free(v.big_integer_val);
                v.big_integer_val = null;
            }
        case BIG_FLOAT:
            if (v.big_float_val != null) {
                omni_big_float_free(v.big_float_val);
                v.big_float_val = null;
            }
        case BIG_COMPLEX:
            if (v.big_complex_val != null) {
                omni_big_complex_free(v.big_complex_val);
                v.big_complex_val = null;
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
