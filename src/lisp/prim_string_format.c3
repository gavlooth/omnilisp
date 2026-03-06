module lisp;

import std::io;
import main;

// =============================================================================
// SECTION: STRING FORMATTING + CL-FORMAT
// =============================================================================
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
        char[] nslice = io::bprintf(&nbuf, "%d", a.int_val)!!;
        strval_append(sv, nslice);
    } else if (is_double(a)) {
        char[64] nbuf;
        char[] nslice = io::bprintf(&nbuf, "%.15g", a.double_val)!!;
        strval_append(sv, nslice);
    } else if (a.tag == STRING) {
        strval_append(sv, a.str_chars[:a.str_len]);
    } else if (is_symbol(a)) {
        strval_append(sv, interp.symbols.get_name(a.sym_val));
    } else {
        strval_append(sv, "#<obj>");
    }
}

// Handle one % directive in format string. Called with i pointing past the '%'.
// Parses flags, width, precision, then dispatches on directive char.
// Returns null on success, error Value* on failure.
// Advances *i_ptr past the directive, *arg_idx past consumed argument.
// Returns SPECIAL_BREAK sentinel to signal caller should break (trailing %).
// Returns SPECIAL_CONTINUE sentinel to signal caller should continue (%%).
const Value* SPECIAL_BREAK = (Value*)1;
const Value* SPECIAL_CONTINUE = (Value*)2;

struct FormatSpec {
    bool left_align;
    int width;
    int precision;
    bool has_precision;
}

fn FormatSpec parse_format_spec(char[] fmt, usz* i_ptr) {
    usz i = *i_ptr;
    FormatSpec spec;
    spec.left_align = false;
    spec.width = 0;
    spec.precision = -1;
    spec.has_precision = false;

    if (i < fmt.len && fmt[i] == '-') {
        spec.left_align = true;
        i++;
    }
    while (i < fmt.len && fmt[i] >= '0' && fmt[i] <= '9') {
        spec.width = spec.width * 10 + (int)(fmt[i] - '0');
        i++;
    }
    if (i < fmt.len && fmt[i] == '.') {
        i++;
        spec.precision = 0;
        spec.has_precision = true;
        while (i < fmt.len && fmt[i] >= '0' && fmt[i] <= '9') {
            spec.precision = spec.precision * 10 + (int)(fmt[i] - '0');
            i++;
        }
    }
    *i_ptr = i;
    return spec;
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

fn Value* format_display_directive(StringVal* builder, Value* a, FormatSpec spec, Interp* interp) {
    StringVal* stmp = strval_new(32);
    defer {
        mem::free(stmp.chars);
        mem::free(stmp);
    }
    format_append_display(stmp, a, interp);
    char[] sval = stmp.chars[:stmp.len];
    strval_append_padded(builder, sval, spec.width, spec.precision, spec.left_align, spec.has_precision);
    return null;
}

fn Value* format_int_directive(StringVal* builder, Value* a, FormatSpec spec, char directive, Interp* interp) {
    if (!is_int(a)) {
        switch (directive) {
            case 'd': return raise_error(interp, "format: %d expects integer");
            case 'x': return raise_error(interp, "format: %x expects integer");
            case 'X': return raise_error(interp, "format: %X expects integer");
            case 'o': return raise_error(interp, "format: %o expects integer");
            default:  return raise_error(interp, "format: integer directive expects integer");
        }
    }
    char[32] buf;
    char[] val;
    switch (directive) {
        case 'd': val = io::bprintf(&buf, "%d", a.int_val)!!;
        case 'x': val = io::bprintf(&buf, "%x", a.int_val)!!;
        case 'X': val = io::bprintf(&buf, "%X", a.int_val)!!;
        case 'o': val = io::bprintf(&buf, "%o", a.int_val)!!;
        default: return raise_error(interp, "format: unknown integer directive");
    }
    strval_append_num_padded(builder, val, spec.width, spec.left_align);
    return null;
}

fn Value* format_float_directive(StringVal* builder, Value* a, FormatSpec spec, Interp* interp) {
    double num;
    if (!value_as_double(a, &num)) return raise_error(interp, "format: %f expects number");
    char[128] buf;
    char[] val = spec.has_precision
        ? truncate_after_decimal(io::bprintf(&buf, "%.20f", num)!!, spec.precision)
        : io::bprintf(&buf, "%.6f", num)!!;
    strval_append_num_padded(builder, val, spec.width, spec.left_align);
    return null;
}

fn Value* format_scientific_directive(StringVal* builder, Value* a, FormatSpec spec, Interp* interp) {
    double num;
    if (!value_as_double(a, &num)) return raise_error(interp, "format: %e expects number");
    char[128] buf;
    char[] val;
    if (spec.has_precision) {
        char[128] tmp;
        val = truncate_scientific(io::bprintf(&buf, "%.20e", num)!!, spec.precision, tmp[0:128]);
    } else {
        val = io::bprintf(&buf, "%e", num)!!;
    }
    strval_append_num_padded(builder, val, spec.width, spec.left_align);
    return null;
}

fn Value* format_binary_directive(StringVal* builder, Value* a, FormatSpec spec, Interp* interp) {
    if (!is_int(a)) return raise_error(interp, "format: %b expects integer");
    char[68] out;
    usz out_len = 0;
    long val = a.int_val;
    if (val == 0) {
        out[0] = '0';
        out_len = 1;
    } else {
        if (val < 0) {
            out[out_len++] = '-';
            val = -val;
        }
        char[64] rev;
        usz rev_len = 0;
        while (val > 0) {
            rev[rev_len++] = (char)('0' + (char)(val % 2));
            val = val / 2;
        }
        for (usz k = 0; k < rev_len; k++) {
            out[out_len++] = rev[rev_len - 1 - k];
        }
    }
    strval_append_num_padded(builder, out[:out_len], spec.width, spec.left_align);
    return null;
}

fn Value* format_directive(StringVal* builder, char[] fmt, usz* i_ptr, Value*[] args, usz* arg_idx, Interp* interp) {
    usz i = *i_ptr;
    FormatSpec spec = parse_format_spec(fmt, &i);

    if (i >= fmt.len) {
        // Trailing '%' with no directive — just emit '%'
        strval_push(builder, '%');
        *i_ptr = i;
        return SPECIAL_BREAK;
    }

    char directive = fmt[i];
    *i_ptr = i;

    if (directive == '%') {
        // %% → literal %
        strval_push(builder, '%');
        return SPECIAL_CONTINUE;
    }

    // All other directives consume an argument
    if (*arg_idx >= args.len) {
        return raise_error(interp, "format: not enough arguments");
    }
    Value* a = args[(*arg_idx)++];

    switch (directive) {
        case 's': return format_display_directive(builder, a, spec, interp);
        case 'd': return format_int_directive(builder, a, spec, directive, interp);
        case 'f': return format_float_directive(builder, a, spec, interp);
        case 'e': return format_scientific_directive(builder, a, spec, interp);
        case 'x': return format_int_directive(builder, a, spec, directive, interp);
        case 'X': return format_int_directive(builder, a, spec, directive, interp);
        case 'o': return format_int_directive(builder, a, spec, directive, interp);
        case 'b': return format_binary_directive(builder, a, spec, interp);

        default:
            // Unknown directive — emit as literal
            strval_push(builder, '%');
            strval_push(builder, directive);
            return null;
    }
}

fn Value* prim_format(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return raise_error(interp, "format: expected format string");
    char[] fmt = args[0].str_chars[:args[0].str_len];
    StringVal* builder = strval_new(fmt.len);
    usz arg_idx = 1;

    for (usz i = 0; i < fmt.len; i++) {
        if (fmt[i] == '%' && i + 1 < fmt.len) {
            i++; // skip '%'
            Value* err = format_directive(builder, fmt, &i, args, &arg_idx, interp);
            if (err == SPECIAL_BREAK) break;
            if (err == SPECIAL_CONTINUE) continue;
            if (err != null) return err;
        } else {
            strval_push(builder, fmt[i]);
        }
    }
    builder.chars[builder.len] = 0;
    Value* result = interp.alloc_value();
    result.tag = STRING;
    strval_into_value(builder, result);
    return result;
}

// Handle one ~ directive in cl-format string.
// Dispatches on directive char, appends to builder, advances *arg_idx.
fn void cl_format_directive(StringVal* builder, char directive, Value*[] args, usz* arg_idx, Interp* interp) {
    if (directive == 'a' || directive == 's') {
        if (*arg_idx < args.len) {
            Value* a = args[(*arg_idx)++];
            if (a == null || is_nil(a)) {
                strval_append(builder, "nil");
            } else if (is_int(a)) {
                char[32] nbuf;
                char[] nslice = io::bprintf(&nbuf, "%d", a.int_val)!!;
                strval_append(builder, nslice);
            } else if (is_double(a)) {
                char[64] nbuf;
                char[] nslice = io::bprintf(&nbuf, "%.15g", a.double_val)!!;
                strval_append(builder, nslice);
            } else if (a.tag == STRING) {
                if (directive == 's') {
                    strval_push(builder, '"');
                }
                strval_append(builder, a.str_chars[:a.str_len]);
                if (directive == 's') {
                    strval_push(builder, '"');
                }
            } else if (is_symbol(a)) {
                strval_append(builder, interp.symbols.get_name(a.sym_val));
            } else {
                strval_append(builder, "#<obj>");
            }
        }
    } else if (directive == '~') {
        strval_push(builder, '~');
    } else {
        strval_push(builder, '~');
        strval_push(builder, directive);
    }
}

fn Value* prim_cl_format(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return raise_error(interp, "cl-format: expected format string");
    char[] fmt = args[0].str_chars[:args[0].str_len];
    StringVal* builder = strval_new(fmt.len);
    usz arg_idx = 1;
    for (usz i = 0; i < fmt.len; i++) {
        if (fmt[i] == '~' && i + 1 < fmt.len) {
            i++;
            cl_format_directive(builder, fmt[i], args, &arg_idx, interp);
        } else {
            strval_push(builder, fmt[i]);
        }
    }
    builder.chars[builder.len] = 0;
    Value* result = interp.alloc_value();
    result.tag = STRING;
    strval_into_value(builder, result);
    return result;
}

