module lisp;

import std::io;
import main;

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
