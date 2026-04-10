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
    if (v == null || v.tag != PRIMITIVE) return false;
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

fn bool is_number(Value* v) @inline {
    return v != null && (v.tag == INT || v.tag == DOUBLE);
}

fn double to_double(Value* v) @inline {
    if (v.tag == DOUBLE) return v.double_val;
    return (double)v.int_val;
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
    if (base_type == interp.sym_Pointer) {
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
    Value* v = boundary_alloc_value_in_root(interp, true);
    v.tag = ARRAY;

    Array* arr = g_value_force_make_array_struct_alloc_fail
        ? null
        : (Array*)mem::malloc(Array.sizeof);
    if (arr == null) {
        v.tag = NIL;
        v.array_val = null;
        return raise_error(interp, oom_message);
    }

    arr.capacity = capacity < 4 ? 4 : capacity;
    arr.length = 0;
    arr.items = g_value_force_make_array_items_alloc_fail
        ? null
        : (Value**)mem::malloc(Value*.sizeof * arr.capacity);
    if (arr.items == null) {
        mem::free(arr);
        v.tag = NIL;
        v.array_val = null;
        return raise_error(interp, oom_message);
    }

    v.array_val = arr;
    return v;
}

fn Value* make_array(Interp* interp, usz capacity) {
    // Allocate Value in root_scope so it persists across scope releases.
    Value* v = boundary_alloc_value_in_root(interp, true);
    v.tag = ARRAY;
    Array* arr = (Array*)mem::malloc(Array.sizeof);
    arr.capacity = capacity < 4 ? 4 : capacity;
    arr.length = 0;
    arr.items = (Value**)mem::malloc(Value*.sizeof * arr.capacity);
    v.array_val = arr;

    return v;
}

fn Value* make_module(Interp* interp, Module* mod) {
    // Allocate Value in root_scope so it persists across scope releases.
    Value* v = boundary_alloc_value_in_root(interp);
    v.tag = MODULE;
    v.module_val = mod;
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
    // Allocate coroutine wrapper in root_scope so abandonment still runs a dtor.
    Value* v = boundary_alloc_value_in_root(interp, true);

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
