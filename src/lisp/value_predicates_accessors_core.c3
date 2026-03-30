module lisp;

import std::collections::list;
import std::io;
import main;

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

// Convert a ^Type annotation base_type symbol to FfiTypeTag
fn FfiTypeTag type_ann_to_ffi_tag(SymbolId base_type, Interp* interp) {
    if (base_type == interp.sym_Integer) return FFI_TYPE_INT;
    if (base_type == interp.sym_Double) return FFI_TYPE_DOUBLE;
    if (base_type == interp.sym_String) return FFI_TYPE_PTR;
    if (base_type == interp.sym_Pointer) return FFI_TYPE_PTR;
    if (base_type == interp.sym_Boolean) return FFI_TYPE_BOOL;
    if (base_type == interp.sym_Void) return FFI_TYPE_VOID;
    // Unknown type — default to pointer (handles opaque types)
    return FFI_TYPE_PTR;
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
    Value* v = boundary_alloc_value_in_root(interp, true);
    v.tag = SET;
    v.hashmap_val = hashmap_new(capacity, interp);
    return v;
}

fn Value* make_iterator(Interp* interp, Value* thunk) {
    Value* v = interp.alloc_value();
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

// =============================================================================
// String conversion helpers (shared across primitives, eval, value printing)
// =============================================================================

/**
 * Convert integer to string in caller-provided buffer. Returns slice of written chars.
 */
fn char[] int_to_string(long val, char[] buffer) {
    if (val == 0) { buffer[0] = '0'; return buffer[:1]; }
    usz len = 0;
    bool negative = val < 0;
    if (negative) val = -val;
    while (val > 0 && len < buffer.len - 1) {
        buffer[len++] = (char)('0' + (val % 10));
        val /= 10;
    }
    if (negative) buffer[len++] = '-';
    // Reverse in-place
    for (usz j = 0; j < len / 2; j++) {
        char tmp = buffer[j];
        buffer[j] = buffer[len - 1 - j];
        buffer[len - 1 - j] = tmp;
    }
    return buffer[:len];
}

/**
 * Format a TimePoint into caller-provided buffer. Returns slice of written chars.
 */
fn char[] time_point_to_string(TimePoint tp, char[] buffer) {
    if (buffer.len == 0) return buffer[:0];

    if (tp.kind == TIME_POINT_DATE) {
        if (tp.microsecond != 0) {
            return io::bprintf(buffer, "%04d-%02d-%02d", tp.year, tp.month, tp.day)!!;
        }
        return io::bprintf(buffer, "%04d-%02d-%02d", tp.year, tp.month, tp.day)!!;
    }

    if (tp.kind == TIME_POINT_TIME) {
        if (tp.microsecond > 0) {
            return io::bprintf(
                buffer,
                "%02d:%02d:%02d.%06d",
                tp.hour,
                tp.minute,
                tp.second,
                tp.microsecond
            )!!;
        }
        return io::bprintf(buffer, "%02d:%02d:%02d", tp.hour, tp.minute, tp.second)!!;
    }

    if (tp.kind == TIME_POINT_DATETIMETZ && tp.tz_offset != 0) {
        int tza = tp.tz_offset < 0 ? -tp.tz_offset : tp.tz_offset;
        int tzh = tza / 60;
        int tzm = tza % 60;
        char sign = tp.tz_offset < 0 ? '-' : '+';
        if (tp.microsecond > 0) {
            return io::bprintf(
                buffer,
                "%04d-%02d-%02dT%02d:%02d:%02d.%06d%c%02d:%02d",
                tp.year, tp.month, tp.day,
                tp.hour, tp.minute, tp.second,
                tp.microsecond,
                sign, tzh, tzm
            )!!;
        }
        return io::bprintf(
            buffer,
            "%04d-%02d-%02dT%02d:%02d:%02d%c%02d:%02d",
            tp.year, tp.month, tp.day,
            tp.hour, tp.minute, tp.second,
            sign, tzh, tzm
        )!!;
    }

    if (tp.microsecond > 0) {
        return io::bprintf(
            buffer,
            "%04d-%02d-%02dT%02d:%02d:%02d.%06d",
            tp.year, tp.month, tp.day,
            tp.hour, tp.minute, tp.second,
            tp.microsecond
        )!!;
    }
    return io::bprintf(buffer, "%04d-%02d-%02dT%02d:%02d:%02d", tp.year, tp.month, tp.day, tp.hour, tp.minute, tp.second)!!;
}

/**
 * Format double to string in caller-provided buffer, ensuring decimal point.
 * Handles NaN, +inf, -inf. Buffer must be at least 64 bytes.
 */
fn char[] double_to_string(double val, char[] buffer) {
    if (val != val) {
        char[] nan = "+nan.0";
        for (usz i = 0; i < nan.len; i++) buffer[i] = nan[i];
        return buffer[:nan.len];
    }
    if (val == 1.0/0.0) {
        char[] inf = "+inf.0";
        for (usz i = 0; i < inf.len; i++) buffer[i] = inf[i];
        return buffer[:inf.len];
    }
    if (val == -1.0/0.0) {
        char[] ninf = "-inf.0";
        for (usz i = 0; i < ninf.len; i++) buffer[i] = ninf[i];
        return buffer[:ninf.len];
    }
    char[64] fmt_buf;
    char[] s = io::bprintf(&fmt_buf, "%.15g", val)!!;
    for (usz i = 0; i < s.len; i++) buffer[i] = s[i];
    usz len = s.len;
    bool has_dot = false;
    for (usz i = 0; i < len; i++) {
        if (buffer[i] == '.' || buffer[i] == 'e' || buffer[i] == 'E') { has_dot = true; break; }
    }
    if (!has_dot && len + 2 <= buffer.len) {
        buffer[len] = '.';
        buffer[len + 1] = '0';
        len += 2;
    }
    return buffer[:len];
}

