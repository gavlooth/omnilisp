module lisp;

import std::io;
import std::math;
import main;

int g_string_force_builder_alloc_fail_nth = 0;
int g_string_force_builder_grow_fail_nth = 0;
bool g_string_force_result_wrapper_alloc_fail = false;

fn bool strval_should_force_fail(int* counter) @inline {
    if (*counter <= 0) return false;
    (*counter)--;
    return *counter == 0;
}

fn bool strval_needs_growth(StringVal* sv, usz needed) @inline {
    if (sv == null || sv.failed) return true;
    usz target = 0;
    if (strval_target_capacity_overflows(sv.len, needed, &target)) return true;
    return target > sv.capacity;
}

fn bool strval_target_capacity_overflows(usz len, usz needed, usz* out) @inline {
    usz target = 0;
    if (math::overflow_add(len, needed, &target)) return true;
    if (math::overflow_add(target, (usz)1, &target)) return true;
    *out = target;
    return false;
}

fn StringVal* strval_new(usz initial_cap) {
    if (initial_cap < 32) initial_cap = 32;
    if (initial_cap >= usz.max) return null;
    // Reserve one extra byte for trailing NUL; many callers materialize
    // exact-length content and then write chars[len] = 0.
    usz alloc_cap = initial_cap + 1;
    if (strval_should_force_fail(&g_string_force_builder_alloc_fail_nth)) return null;
    StringVal* sv = (StringVal*)mem::malloc(StringVal.sizeof);
    if (sv == null) return null;
    sv.chars = (char*)mem::malloc(alloc_cap);
    if (sv.chars == null) {
        mem::free(sv);
        return null;
    }
    sv.len = 0;
    sv.capacity = alloc_cap;
    sv.failed = false;
    sv.chars[0] = 0;
    return sv;
}

fn StringVal* strval_new_from_slice(char[] s) {
    StringVal* sv = strval_new(s.len);
    if (sv == null) return null;
    for (usz i = 0; i < s.len; i++) {
        sv.chars[i] = s[i];
    }
    sv.chars[s.len] = 0;
    sv.len = s.len;
    return sv;
}

/**
 * Ensure StringVal has room for at least `needed` more bytes.
 */
fn bool strval_ensure(StringVal* sv, usz needed) {
    if (sv == null || sv.failed) return false;
    if (!strval_needs_growth(sv, needed)) return true;
    usz target = 0;
    if (strval_target_capacity_overflows(sv.len, needed, &target)) {
        sv.failed = true;
        return false;
    }

    usz new_cap = sv.capacity > 0 ? sv.capacity : 32;
    while (new_cap < target) {
        if (new_cap > usz.max / 2) {
            new_cap = target;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap < target || strval_should_force_fail(&g_string_force_builder_grow_fail_nth)) {
        sv.failed = true;
        return false;
    }

    char* new_chars = (char*)mem::malloc(new_cap);
    if (new_chars == null) {
        sv.failed = true;
        return false;
    }
    for (usz i = 0; i < sv.len; i++) new_chars[i] = sv.chars[i];
    mem::free(sv.chars);
    sv.chars = new_chars;
    sv.capacity = new_cap;
    sv.chars[sv.len] = 0;
    return true;
}

/**
 * Append a single char to a StringVal.
 */
fn bool strval_push(StringVal* sv, char c) {
    if (!strval_ensure(sv, 1)) return false;
    sv.chars[sv.len++] = c;
    sv.chars[sv.len] = 0;
    return true;
}

// Transfer builder contents into a Value. Frees the StringVal struct
// but keeps the chars buffer (Value takes ownership).
fn void strval_into_value(StringVal* sv, Value* v) {
    v.str_chars = sv.chars;
    v.str_len = sv.len;
    v.string_heap_owned = true;
    mem::free(sv);  // free struct only, chars owned by Value
}

fn void strval_dispose(StringVal* sv) {
    if (sv == null) return;
    if (sv.chars != null) mem::free(sv.chars);
    mem::free(sv);
}

fn bool strval_failed(StringVal* sv) @inline {
    return sv == null || sv.failed;
}

fn Value* strval_raise_oom(StringVal* sv, Interp* interp, char[] message) {
    strval_dispose(sv);
    return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", message);
}

fn Value* strval_materialize_result(StringVal* sv, Interp* interp, char[] message) {
    if (strval_failed(sv)) {
        return strval_raise_oom(sv, interp, message);
    }
    Value* result = g_string_force_result_wrapper_alloc_fail ? null : interp.alloc_value();
    if (result == null) {
        return strval_raise_oom(sv, interp, message);
    }
    result.tag = STRING;
    strval_into_value(sv, result);
    return result;
}

// Helper: append a char[] slice to a StringVal
fn bool strval_append(StringVal* sv, char[] s) {
    if (!strval_ensure(sv, s.len)) return false;
foreach (item : s) {
        sv.chars[sv.len++] = item;
}
    sv.chars[sv.len] = 0;
    return true;
}

// Helper: append with width/precision padding
fn bool strval_append_padded(StringVal* sv, char[] s, int width, int precision, bool left_align, bool has_precision) {
    // Apply precision (truncate) — only for string-like content
    char[] content = s;
    if (has_precision && precision >= 0 && (usz)precision < content.len) {
        content = content[:(usz)precision];
    }
    if (content.len > (usz)int.max) return false;
    int content_len = (int)content.len;
    int pad = 0;
    if (width > content_len) pad = width - content_len;

    if (!left_align && pad > 0) {
        if (!strval_ensure(sv, (usz)pad)) return false;
        for (int k = 0; k < pad; k++) sv.chars[sv.len++] = ' ';
        sv.chars[sv.len] = 0;
    }
    if (!strval_append(sv, content)) return false;
    if (left_align && pad > 0) {
        if (!strval_ensure(sv, (usz)pad)) return false;
        for (int k = 0; k < pad; k++) sv.chars[sv.len++] = ' ';
        sv.chars[sv.len] = 0;
    }
    return true;
}

// Helper: append numeric string with width padding (no precision truncation)
fn bool strval_append_num_padded(StringVal* sv, char[] s, int width, bool left_align) {
    if (s.len > (usz)int.max) return false;
    int content_len = (int)s.len;
    int pad = 0;
    if (width > content_len) pad = width - content_len;

    if (!left_align && pad > 0) {
        if (!strval_ensure(sv, (usz)pad)) return false;
        for (int k = 0; k < pad; k++) sv.chars[sv.len++] = ' ';
        sv.chars[sv.len] = 0;
    }
    if (!strval_append(sv, s)) return false;
    if (left_align && pad > 0) {
        if (!strval_ensure(sv, (usz)pad)) return false;
        for (int k = 0; k < pad; k++) sv.chars[sv.len++] = ' ';
        sv.chars[sv.len] = 0;
    }
    return true;
}

// Helper: append a value in display form to a StringVal (like %s: no quotes on strings)
fn bool format_append_display(StringVal* sv, Value* a, Interp* interp) {
    if (a == null || is_nil(a)) {
        return strval_append(sv, "nil");
    } else if (is_int(a)) {
        char[32] nbuf;
        if (try nslice = io::bprintf(&nbuf, "%d", a.int_val)) {
            return strval_append(sv, nslice);
        } else {
            char[64] rendered;
            usz rendered_len = print_value_to_buf(a, &interp.symbols, &rendered, rendered.len);
            return strval_append(sv, rendered[:rendered_len]);
        }
    } else if (is_double(a)) {
        char[64] nbuf;
        if (try nslice = io::bprintf(&nbuf, "%.15g", a.double_val)) {
            return strval_append(sv, nslice);
        } else {
            char[64] rendered;
            usz rendered_len = print_value_to_buf(a, &interp.symbols, &rendered, rendered.len);
            return strval_append(sv, rendered[:rendered_len]);
        }
    } else if (is_float32(a)) {
        char[64] nbuf;
        if (try nslice = io::bprintf(&nbuf, "%.15g", (double)a.float32_val)) {
            return strval_append(sv, nslice);
        } else {
            char[64] rendered;
            usz rendered_len = print_value_to_buf(a, &interp.symbols, &rendered, rendered.len);
            return strval_append(sv, rendered[:rendered_len]);
        }
    } else if (a.tag == STRING) {
        return strval_append(sv, a.str_chars[:a.str_len]);
    } else if (is_symbol(a)) {
        return strval_append(sv, interp.symbols.get_name(a.sym_val));
    } else {
        // Fallback to canonical value renderer so type descriptors and
        // structured values have the same `%s` surface as REPL printing.
        char[512] rendered;
        usz rendered_len = print_value_to_buf(a, &interp.symbols, &rendered, rendered.len);
        return strval_append(sv, rendered[:rendered_len]);
    }
}

fn bool value_as_double(Value* a, double* out) {
    if (is_int(a)) {
        *out = (double)a.int_val;
        return true;
    }
    if (is_double(a)) {
        *out = a.double_val;
        return true;
    }
    if (is_float32(a)) {
        *out = (double)a.float32_val;
        return true;
    }
    return false;
}

fn char[] truncate_after_decimal(char[] val, int precision) {
    usz dot_pos = 0;
    bool found_dot = false;
    for (usz k = 0; k < val.len; k++) {
        if (val[k] == '.') {
            dot_pos = k;
            found_dot = true;
            break;
        }
    }
    if (!found_dot) return val;
    if (precision == 0) return val[:dot_pos];
    usz end = dot_pos + 1 + (usz)precision;
    if (end > val.len) end = val.len;
    return val[:end];
}

fn char[] truncate_scientific(char[] val, int precision, char[] tmp) {
    usz dot_pos = 0;
    bool found_dot = false;
    for (usz k = 0; k < val.len; k++) {
        if (val[k] == '.') {
            dot_pos = k;
            found_dot = true;
            break;
        }
    }
    if (!found_dot) return val;
    usz exp_pos = val.len;
    for (usz k = 0; k < val.len; k++) {
        if (val[k] == 'e' || val[k] == 'E') {
            exp_pos = k;
            break;
        }
    }
    usz trunc_end = dot_pos + 1 + (usz)precision;
    if (trunc_end > exp_pos) trunc_end = exp_pos;
    usz idx = 0;
    for (usz k = 0; k < trunc_end; k++) tmp[idx++] = val[k];
    for (usz k = exp_pos; k < val.len; k++) tmp[idx++] = val[k];
    return tmp[:idx];
}
