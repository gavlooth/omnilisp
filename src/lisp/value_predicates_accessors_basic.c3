module lisp;

import std::collections::list;
import std::core::mem;
import std::io;
import main;

bool g_value_force_make_iterator_alloc_fail = false;
bool g_value_force_make_array_struct_alloc_fail = false;
bool g_value_force_make_array_items_alloc_fail = false;

// =============================================================================
// SECTION 3/4: VALUE PREDICATES + ACCESSORS + formatting helpers
// =============================================================================
// =============================================================================
// SECTION 3: VALUE PREDICATES
// =============================================================================

fn bool is_nil(Value* v) @inline {
    return v == null || v.tag == NIL;
}

fn bool is_int(Value* v) @inline {
    return v != null && v.tag == INT;
}

fn bool is_string(Value* v) @inline {
    return v != null && v.tag == STRING;
}

fn bool is_symbol(Value* v) @inline {
    return v != null && v.tag == SYMBOL;
}

fn bool is_time_point(Value* v) @inline {
    return v != null && v.tag == TIME_POINT;
}

fn bool is_cons(Value* v) @inline {
    return v != null && v.tag == CONS;
}

fn bool primitive_name_equals(Value* v, char[] expected) @inline {
    if (v == null || v.tag != PRIMITIVE || v.prim_val == null) return false;
    if (expected.len >= 64) return false;
    for (usz i = 0; i < expected.len; i++) {
        char actual = v.prim_val.name[i];
        if (actual == 0 || actual != expected[i]) return false;
    }
    return v.prim_val.name[expected.len] == 0;
}

fn bool is_aot_closure_wrapper(Value* v) @inline {
    return primitive_name_equals(v, "aot-closure")
        || primitive_name_equals(v, "aot-closure-var");
}

fn bool is_closure(Value* v) @inline {
    return v != null && (v.tag == CLOSURE || is_aot_closure_wrapper(v));
}

fn bool is_continuation(Value* v) @inline {
    return v != null && v.tag == CONTINUATION;
}

fn bool is_primitive(Value* v) @inline {
    return v != null && v.tag == PRIMITIVE;
}

fn bool is_error(Value* v) @inline {
    return v != null && v.tag == ERROR;
}

fn bool is_double(Value* v) @inline {
    return v != null && v.tag == DOUBLE;
}

fn bool is_big_integer(Value* v) @inline {
    return v != null && v.tag == BIG_INTEGER;
}

fn bool is_number(Value* v) @inline {
    return v != null && (v.tag == INT || v.tag == DOUBLE);
}

fn bool is_numeric_value(Value* v) @inline {
    return v != null && (v.tag == INT || v.tag == DOUBLE || v.tag == BIG_INTEGER);
}

fn double to_double(Value* v) @inline {
    if (v.tag == DOUBLE) return v.double_val;
    return (double)v.int_val;
}

fn bool try_numeric_to_double(Value* v, double* out) @inline {
    if (v == null || out == null) return false;
    if (v.tag == DOUBLE) {
        *out = v.double_val;
        return true;
    }
    if (v.tag == INT) {
        *out = (double)v.int_val;
        return true;
    }
    if (v.tag == BIG_INTEGER) {
        return big_integer_try_to_double(v, out);
    }
    return false;
}

fn bool is_ffi_handle(Value* v) @inline {
    return v != null && v.tag == FFI_HANDLE;
}

// Convert a ^Type annotation base_type symbol to FfiTypeTag.
fn bool type_ann_to_ffi_tag(SymbolId base_type, Interp* interp, FfiTypeTag* out_tag) {
    if (base_type == interp.sym_Integer) {
        *out_tag = FFI_TYPE_INT;
        return true;
    }
    if (base_type == interp.sym_Double) {
        *out_tag = FFI_TYPE_DOUBLE;
        return true;
    }
    if (base_type == interp.sym_String) {
        *out_tag = FFI_TYPE_STRING;
        return true;
    }
    if (base_type == interp.sym_ForeignHandle) {
        *out_tag = FFI_TYPE_PTR;
        return true;
    }
    if (base_type == interp.sym_Boolean) {
        *out_tag = FFI_TYPE_BOOL;
        return true;
    }
    if (base_type == interp.sym_Void) {
        *out_tag = FFI_TYPE_VOID;
        return true;
    }
    return false;
}

fn bool ffi_char_slice_equals(char[] left, char[] right) @inline {
    if (left.len != right.len) return false;
    for (usz i = 0; i < left.len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn bool ffi_symbol_name_equals(SymbolId sym, Interp* interp, char[] expected) @inline {
    return ffi_char_slice_equals(interp.symbols.get_name(sym), expected);
}

fn FfiHandlePolicy ffi_handle_policy_default() @inline {
    FfiHandlePolicy policy = {};
    policy.is_foreign_handle = false;
    policy.name = INVALID_SYMBOL_ID;
    policy.ownership = FFI_HANDLE_BORROWED;
    policy.nullability = FFI_HANDLE_NULLABLE;
    policy.finalizer_symbol = INVALID_SYMBOL_ID;
    policy.finalizer = null;
    return policy;
}

fn bool ffi_meta_key_is_handle_policy(SymbolId key, Interp* interp) @inline {
    return ffi_symbol_name_equals(key, interp, "type")
        || ffi_symbol_name_equals(key, interp, "name")
        || ffi_symbol_name_equals(key, interp, "ownership")
        || ffi_symbol_name_equals(key, interp, "finalizer")
        || ffi_symbol_name_equals(key, interp, "nullability");
}

fn bool ffi_metadata_type_annotation_to_tag(TypeAnnotation* ann, Interp* interp,
    FfiTypeTag* out_tag, FfiHandlePolicy* out_policy) {
    if (ann == null || !ann.is_dict) return false;

    FfiHandlePolicy policy = ffi_handle_policy_default();
    bool saw_handle_policy_key = false;
    bool saw_type = false;
    bool saw_ownership = false;
    bool saw_finalizer = false;

    for (usz i = 0; i < ann.meta_count; i++) {
        MetaEntry* entry = &ann.meta[i];
        for (usz j = 0; j < i; j++) {
            if (ann.meta[j].key == entry.key) return false;
        }

        if (!ffi_meta_key_is_handle_policy(entry.key, interp)) return false;
        saw_handle_policy_key = true;

        if (ffi_symbol_name_equals(entry.key, interp, "type")) {
            if (entry.is_int || entry.sym_value != interp.sym_ForeignHandle) return false;
            saw_type = true;
        } else if (ffi_symbol_name_equals(entry.key, interp, "name")) {
            if (entry.is_int) return false;
            policy.name = entry.sym_value;
        } else if (ffi_symbol_name_equals(entry.key, interp, "ownership")) {
            if (entry.is_int) return false;
            if (ffi_symbol_name_equals(entry.sym_value, interp, "owned")) {
                policy.ownership = FFI_HANDLE_OWNED;
            } else if (ffi_symbol_name_equals(entry.sym_value, interp, "borrowed")) {
                policy.ownership = FFI_HANDLE_BORROWED;
            } else if (ffi_symbol_name_equals(entry.sym_value, interp, "manual")) {
                policy.ownership = FFI_HANDLE_MANUAL;
            } else {
                return false;
            }
            saw_ownership = true;
        } else if (ffi_symbol_name_equals(entry.key, interp, "finalizer")) {
            if (entry.is_int) return false;
            policy.finalizer_symbol = entry.sym_value;
            saw_finalizer = true;
        } else if (ffi_symbol_name_equals(entry.key, interp, "nullability")) {
            if (entry.is_int) return false;
            if (ffi_symbol_name_equals(entry.sym_value, interp, "nullable")) {
                policy.nullability = FFI_HANDLE_NULLABLE;
            } else if (ffi_symbol_name_equals(entry.sym_value, interp, "non-null")) {
                policy.nullability = FFI_HANDLE_NON_NULL;
            } else {
                return false;
            }
        }
    }

    if (!saw_handle_policy_key && !saw_type) return false;
    if (saw_finalizer && saw_ownership && policy.ownership != FFI_HANDLE_OWNED) return false;
    if (saw_finalizer && !saw_ownership) policy.ownership = FFI_HANDLE_OWNED;
    if (policy.ownership == FFI_HANDLE_OWNED && policy.finalizer_symbol == INVALID_SYMBOL_ID) return false;

    policy.is_foreign_handle = true;
    *out_tag = FFI_TYPE_PTR;
    *out_policy = policy;
    return true;
}

fn bool ffi_type_annotation_to_tag(TypeAnnotation* ann, Interp* interp,
    FfiTypeTag* out_tag, FfiHandlePolicy* out_policy) {
    if (ann == null || !ann.has_annotation) return false;

    FfiHandlePolicy policy = ffi_handle_policy_default();
    if (ann.is_dict) {
        return ffi_metadata_type_annotation_to_tag(ann, interp, out_tag, out_policy);
    }

    if (!type_ann_to_ffi_tag(ann.base_type, interp, out_tag)) return false;
    if (*out_tag == FFI_TYPE_PTR) policy.is_foreign_handle = true;
    *out_policy = policy;
    return true;
}

fn bool is_array(Value* v) @inline {
    return v != null && v.tag == ARRAY;
}

fn bool is_dict(Value* v) @inline {
    return v != null && v.tag == HASHMAP;
}

fn bool is_set(Value* v) @inline {
    return v != null && v.tag == SET;
}

fn bool is_hashmap_backed(Value* v) @inline {
    return v != null && (v.tag == HASHMAP || v.tag == SET);
}

fn Value* make_array_checked(Interp* interp, usz capacity, char[] oom_message) {
    Array* arr = g_value_force_make_array_struct_alloc_fail
        ? null
        : (Array*)mem::malloc(Array.sizeof);
    if (arr == null) {
        return raise_error(interp, oom_message);
    }

    arr.capacity = capacity < 4 ? 4 : capacity;
    arr.length = 0;
    if (arr.capacity > usz.max / Value*.sizeof) {
        mem::free(arr);
        return raise_error(interp, oom_message);
    }
    arr.items = g_value_force_make_array_items_alloc_fail
        ? null
        : (Value**)mem::malloc(Value*.sizeof * arr.capacity);
    if (arr.items == null) {
        mem::free(arr);
        return raise_error(interp, oom_message);
    }

    Value* v = boundary_alloc_value_in_root(interp, true);
    if (v == null) {
        mem::free(arr.items);
        mem::free(arr);
        return raise_error(interp, oom_message);
    }

    v.tag = ARRAY;
    v.array_val = arr;
    return v;
}

fn Value* make_array(Interp* interp, usz capacity) {
    return make_array_checked(interp, capacity, "array: out of memory");
}

fn Value* make_module(Interp* interp, Module* mod) {
    if (interp == null || interp.root_scope == null || mod == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-module", "module: invalid module value");
    }
    Module* stable_mod = (Module*)interp.root_scope.alloc(Module.sizeof);
    if (stable_mod == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "module: failed to allocate module descriptor");
    }
    *stable_mod = *mod;

    // Allocate Value in root_scope so it persists across scope releases.
    Value* v = boundary_alloc_value_in_root(interp);
    if (v == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "module: failed to allocate module value");
    }
    v.tag = MODULE;
    v.module_val = stable_mod;
    return v;
}

fn Value* make_set(Interp* interp, uint capacity) {
    return make_set_checked(interp, capacity, "Set: out of memory");
}

fn Value* make_iterator(Interp* interp, Value* thunk) {
    if (thunk == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "iterator: failed to allocate thunk");
    }
    if (is_error(thunk)) return thunk;

    Value* v = g_value_force_make_iterator_alloc_fail ? null : interp.alloc_value();
    if (v == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "iterator: failed to allocate iterator value");
    }
    v.tag = ITERATOR;
    v.iterator_val = thunk;
    return v;
}

fn Value* make_coroutine(Interp* interp, main::StackCtx* ctx) {
    if (ctx == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "coroutine: missing context");
    }

    // Allocate coroutine wrapper in root_scope so abandonment still runs a dtor.
    Value* v = boundary_alloc_value_in_root(interp, true);
    if (v == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "coroutine: failed to allocate coroutine value");
    }

    v.tag = COROUTINE;
    v.coroutine_val = ctx;
    return v;
}

fn bool is_list(Value* v) {
    while (v != null && v.tag == CONS) {
        v = v.cons_val.cdr;
    }
    return is_nil(v);
}

// =============================================================================
// SECTION 4: VALUE ACCESSORS
// =============================================================================

<* @require v != null && v.tag == INT : "get_int called on non-INT value" *>
fn long get_int(Value* v) @inline {
    return v.int_val;
}

<* @require v != null && v.tag == SYMBOL : "get_symbol called on non-SYMBOL value" *>
fn SymbolId get_symbol(Value* v) @inline {
    return v.sym_val;
}

<* @require v != null && v.tag == CONS : "car called on non-CONS value" *>
fn Value* car(Value* v) @inline {
    return v.cons_val.car;
}

<* @require v != null && v.tag == CONS : "cdr called on non-CONS value" *>
fn Value* cdr(Value* v) @inline {
    return v.cons_val.cdr;
}

// List helpers
fn Value* cadr(Value* v) @inline { return car(cdr(v)); }
fn Value* caddr(Value* v) @inline { return car(cdr(cdr(v))); }
fn Value* cadddr(Value* v) @inline { return car(cdr(cdr(cdr(v)))); }

fn usz list_length(Value* v) {
    usz len = 0;
    while (is_cons(v)) {
        len++;
        v = cdr(v);
    }
    return len;
}
