module lisp;

import std::io;
import main;

fn StringVal* strval_new(usz initial_cap) {
    if (initial_cap < 32) initial_cap = 32;
    // Reserve one extra byte for trailing NUL; many callers materialize
    // exact-length content and then write chars[len] = 0.
    usz alloc_cap = initial_cap + 1;
    StringVal* sv = (StringVal*)mem::malloc(StringVal.sizeof);
    sv.chars = (char*)mem::malloc(alloc_cap);
    sv.len = 0;
    sv.capacity = alloc_cap;
    sv.chars[0] = 0;
    return sv;
}

/**
 * Ensure StringVal has room for at least `needed` more bytes.
 */
fn void strval_ensure(StringVal* sv, usz needed) {
    if (sv.len + needed + 1 > sv.capacity) {
        usz new_cap = sv.capacity * 2;
        while (new_cap < sv.len + needed + 1) new_cap *= 2;
        char* new_chars = (char*)mem::malloc(new_cap);
        for (usz i = 0; i < sv.len; i++) new_chars[i] = sv.chars[i];
        mem::free(sv.chars);
        sv.chars = new_chars;
        sv.capacity = new_cap;
    }
}

/**
 * Append a single char to a StringVal.
 */
fn void strval_push(StringVal* sv, char c) {
    strval_ensure(sv, 1);
    sv.chars[sv.len++] = c;
}

// Transfer builder contents into a Value. Frees the StringVal struct
// but keeps the chars buffer (Value takes ownership).
fn void strval_into_value(StringVal* sv, Value* v) {
    v.str_chars = sv.chars;
    v.str_len = sv.len;
    mem::free(sv);  // free struct only, chars owned by Value
}

// Helper: append a char[] slice to a StringVal
fn void strval_append(StringVal* sv, char[] s) {
    strval_ensure(sv, s.len);
    for (usz k = 0; k < s.len; k++) {
        sv.chars[sv.len++] = s[k];
    }
}

// Helper: append with width/precision padding
fn void strval_append_padded(StringVal* sv, char[] s, int width, int precision, bool left_align, bool has_precision) {
    // Apply precision (truncate) — only for string-like content
    char[] content = s;
    if (has_precision && precision >= 0 && (usz)precision < content.len) {
        content = content[:(usz)precision];
    }
    int content_len = (int)content.len;
    int pad = 0;
    if (width > content_len) pad = width - content_len;

    if (!left_align && pad > 0) {
        strval_ensure(sv, (usz)pad);
        for (int k = 0; k < pad; k++) sv.chars[sv.len++] = ' ';
    }
    strval_append(sv, content);
    if (left_align && pad > 0) {
        strval_ensure(sv, (usz)pad);
        for (int k = 0; k < pad; k++) sv.chars[sv.len++] = ' ';
    }
}

// Helper: append numeric string with width padding (no precision truncation)
fn void strval_append_num_padded(StringVal* sv, char[] s, int width, bool left_align) {
    int content_len = (int)s.len;
    int pad = 0;
    if (width > content_len) pad = width - content_len;

    if (!left_align && pad > 0) {
        strval_ensure(sv, (usz)pad);
        for (int k = 0; k < pad; k++) sv.chars[sv.len++] = ' ';
    }
    strval_append(sv, s);
    if (left_align && pad > 0) {
        strval_ensure(sv, (usz)pad);
        for (int k = 0; k < pad; k++) sv.chars[sv.len++] = ' ';
    }
}

// Helper: append a value in display form to a StringVal (like %s: no quotes on strings)
fn void format_append_display(StringVal* sv, Value* a, Interp* interp) {
    if (a == null || is_nil(a)) {
        strval_append(sv, "nil");
    } else if (is_int(a)) {
        char[32] nbuf;
        if (try nslice = io::bprintf(&nbuf, "%d", a.int_val)) {
            strval_append(sv, nslice);
        } else {
            char[64] rendered;
            usz rendered_len = print_value_to_buf(a, &interp.symbols, &rendered, rendered.len);
            strval_append(sv, rendered[:rendered_len]);
        }
    } else if (is_double(a)) {
        char[64] nbuf;
        if (try nslice = io::bprintf(&nbuf, "%.15g", a.double_val)) {
            strval_append(sv, nslice);
        } else {
            char[64] rendered;
            usz rendered_len = print_value_to_buf(a, &interp.symbols, &rendered, rendered.len);
            strval_append(sv, rendered[:rendered_len]);
        }
    } else if (a.tag == STRING) {
        strval_append(sv, a.str_chars[:a.str_len]);
    } else if (is_symbol(a)) {
        strval_append(sv, interp.symbols.get_name(a.sym_val));
    } else {
        // Fallback to canonical value renderer so type descriptors and
        // structured values have the same `%s` surface as REPL printing.
        char[512] rendered;
        usz rendered_len = print_value_to_buf(a, &interp.symbols, &rendered, rendered.len);
        strval_append(sv, rendered[:rendered_len]);
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
