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
    bool invalid;
}

fn bool format_spec_accumulate_digit(int current, char ch, int* out) @inline {
    int digit = (int)(ch - '0');
    if (current > (int.max - digit) / 10) return false;
    *out = current * 10 + digit;
    return true;
}

fn FormatSpec parse_format_spec(char[] fmt, usz* i_ptr) {
    usz i = *i_ptr;
    FormatSpec spec;
    spec.left_align = false;
    spec.width = 0;
    spec.precision = -1;
    spec.has_precision = false;
    spec.invalid = false;

    if (i < fmt.len && fmt[i] == '-') {
        spec.left_align = true;
        i++;
    }
    while (i < fmt.len && fmt[i] >= '0' && fmt[i] <= '9') {
        int next_width = 0;
        if (!format_spec_accumulate_digit(spec.width, fmt[i], &next_width)) {
            spec.invalid = true;
        } else {
            spec.width = next_width;
        }
        i++;
    }
    if (i < fmt.len && fmt[i] == '.') {
        i++;
        spec.precision = 0;
        spec.has_precision = true;
        while (i < fmt.len && fmt[i] >= '0' && fmt[i] <= '9') {
            int next_precision = 0;
            if (!format_spec_accumulate_digit(spec.precision, fmt[i], &next_precision)) {
                spec.invalid = true;
            } else {
                spec.precision = next_precision;
            }
            i++;
        }
    }
    *i_ptr = i;
    return spec;
}

fn Value* format_display_directive(StringVal* builder, Value* a, FormatSpec spec, Interp* interp) {
    StringVal* stmp = strval_new(32);
    if (stmp == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to allocate temporary builder");
    }
    defer {
        strval_dispose(stmp);
    }
    if (!format_append_display(stmp, a, interp) || strval_failed(stmp)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to grow temporary builder");
    }
    char[] sval = stmp.chars[:stmp.len];
    if (!strval_append_padded(builder, sval, spec.width, spec.precision, spec.left_align, spec.has_precision)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to grow result buffer");
    }
    return null;
}

fn Value* format_int_directive(StringVal* builder, Value* a, FormatSpec spec, char directive, Interp* interp) {
    if (!is_int(a)) {
        switch (directive) {
            case 'd': return string_raise(interp, "type/arg-mismatch", "format: %d expects integer");
            case 'x': return string_raise(interp, "type/arg-mismatch", "format: %x expects integer");
            case 'X': return string_raise(interp, "type/arg-mismatch", "format: %X expects integer");
            case 'o': return string_raise(interp, "type/arg-mismatch", "format: %o expects integer");
            default:  return string_raise(interp, "type/arg-mismatch", "format: integer directive expects integer");
        }
    }
    char[32] buf;
    char[] val = "";
    switch (directive) {
        case 'd':
            if (try char[] rendered = io::bprintf(&buf, "%d", a.int_val)) {
                val = rendered;
            } else {
                return string_raise(interp, "type/arg-mismatch", "format: internal integer formatting failure");
            }
        case 'x':
            if (try char[] rendered = io::bprintf(&buf, "%x", a.int_val)) {
                val = rendered;
            } else {
                return string_raise(interp, "type/arg-mismatch", "format: internal integer formatting failure");
            }
        case 'X':
            if (try char[] rendered = io::bprintf(&buf, "%X", a.int_val)) {
                val = rendered;
            } else {
                return string_raise(interp, "type/arg-mismatch", "format: internal integer formatting failure");
            }
        case 'o':
            if (try char[] rendered = io::bprintf(&buf, "%o", a.int_val)) {
                val = rendered;
            } else {
                return string_raise(interp, "type/arg-mismatch", "format: internal integer formatting failure");
            }
        default: return string_raise(interp, "type/arg-mismatch", "format: unknown integer directive");
    }
    if (!strval_append_num_padded(builder, val, spec.width, spec.left_align)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to grow result buffer");
    }
    return null;
}

fn Value* format_float_directive(StringVal* builder, Value* a, FormatSpec spec, Interp* interp) {
    double num;
    if (!value_as_double(a, &num)) return string_raise(interp, "type/arg-mismatch", "format: %f expects number");
    char[128] buf;
    char[] val;
    if (spec.has_precision) {
        if (try char[] rendered = io::bprintf(&buf, "%.20f", num)) {
            val = truncate_after_decimal(rendered, spec.precision);
        } else {
            return string_raise(interp, "type/arg-mismatch", "format: internal float formatting failure");
        }
    } else {
        if (try char[] rendered = io::bprintf(&buf, "%.6f", num)) {
            val = rendered;
        } else {
            return string_raise(interp, "type/arg-mismatch", "format: internal float formatting failure");
        }
    }
    if (!strval_append_num_padded(builder, val, spec.width, spec.left_align)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to grow result buffer");
    }
    return null;
}

fn Value* format_scientific_directive(StringVal* builder, Value* a, FormatSpec spec, Interp* interp) {
    double num;
    if (!value_as_double(a, &num)) return string_raise(interp, "type/arg-mismatch", "format: %e expects number");
    char[128] buf;
    char[] val;
    if (spec.has_precision) {
        char[128] tmp;
        if (try char[] rendered = io::bprintf(&buf, "%.20e", num)) {
            val = truncate_scientific(rendered, spec.precision, tmp[0:128]);
        } else {
            return string_raise(interp, "type/arg-mismatch", "format: internal scientific formatting failure");
        }
    } else {
        if (try char[] rendered = io::bprintf(&buf, "%e", num)) {
            val = rendered;
        } else {
            return string_raise(interp, "type/arg-mismatch", "format: internal scientific formatting failure");
        }
    }
    if (!strval_append_num_padded(builder, val, spec.width, spec.left_align)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to grow result buffer");
    }
    return null;
}

fn Value* format_binary_directive(StringVal* builder, Value* a, FormatSpec spec, Interp* interp) {
    if (!is_int(a)) return string_raise(interp, "type/arg-mismatch", "format: %b expects integer");
    char[68] out;
    usz out_len = 0;
    long val = a.int_val;
    if (val == 0) {
        out[0] = '0';
        out_len = 1;
    } else {
        if (val < 0) {
            out[out_len++] = '-';
            if (val == long.min) return string_raise(interp, "type/arg-mismatch", "format: %b integer overflow");
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
    if (!strval_append_num_padded(builder, out[:out_len], spec.width, spec.left_align)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to grow result buffer");
    }
    return null;
}

fn Value* format_directive(StringVal* builder, char[] fmt, usz* i_ptr, Value*[] args, usz* arg_idx, Interp* interp) {
    usz i = *i_ptr;
    FormatSpec spec = parse_format_spec(fmt, &i);
    if (spec.invalid) {
        return string_raise(interp, "type/range-error", "format: width or precision too large");
    }

    if (i >= fmt.len) {
        // Trailing '%' with no directive — just emit '%'
        if (!strval_push(builder, '%')) {
            return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to grow result buffer");
        }
        *i_ptr = i;
        return SPECIAL_BREAK;
    }

    char directive = fmt[i];
    *i_ptr = i;

    if (directive == '%') {
        // %% → literal %
        if (!strval_push(builder, '%')) {
            return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to grow result buffer");
        }
        return SPECIAL_CONTINUE;
    }

    // All other directives consume an argument
    if (*arg_idx >= args.len) {
        return string_raise(interp, "type/arg-mismatch", "format: not enough arguments");
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
            if (!strval_push(builder, '%') || !strval_push(builder, directive)) {
                return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to grow result buffer");
            }
            return null;
    }
}

// Handle one ~ directive in cl-format string.
// Dispatches on directive char, appends to builder, advances *arg_idx.
fn Value* cl_format_directive(StringVal* builder, char directive, Value*[] args, usz* arg_idx, Interp* interp) {
    if (directive == 'a' || directive == 's') {
        if (*arg_idx < args.len) {
            Value* a = args[(*arg_idx)++];
            if (a == null || is_nil(a)) {
                if (!strval_append(builder, "nil")) {
                    return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                }
            } else if (is_int(a)) {
                char[32] nbuf;
                if (try char[] nslice = io::bprintf(&nbuf, "%d", a.int_val)) {
                    if (!strval_append(builder, nslice)) {
                        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                    }
                } else {
                    if (!strval_append(builder, "#<fmt-error>")) {
                        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                    }
                }
            } else if (is_double(a)) {
                char[64] nbuf;
                if (try char[] nslice = io::bprintf(&nbuf, "%.15g", a.double_val)) {
                    if (!strval_append(builder, nslice)) {
                        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                    }
                } else {
                    if (!strval_append(builder, "#<fmt-error>")) {
                        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                    }
                }
            } else if (is_float32(a)) {
                char[64] nbuf;
                if (try char[] nslice = io::bprintf(&nbuf, "%.15g", (double)a.float32_val)) {
                    if (!strval_append(builder, nslice)) {
                        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                    }
                } else {
                    if (!strval_append(builder, "#<fmt-error>")) {
                        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                    }
                }
            } else if (a.tag == STRING) {
                if (directive == 's') {
                    if (!strval_push(builder, '"')) {
                        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                    }
                }
                if (!strval_append(builder, a.str_chars[:a.str_len])) {
                    return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                }
                if (directive == 's') {
                    if (!strval_push(builder, '"')) {
                        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                    }
                }
            } else if (is_symbol(a)) {
                if (!strval_append(builder, interp.symbols.get_name(a.sym_val))) {
                    return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                }
            } else {
                if (!strval_append(builder, "#<obj>")) {
                    return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
                }
            }
        }
    } else if (directive == '~') {
        if (!strval_push(builder, '~')) {
            return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
        }
    } else {
        if (!strval_push(builder, '~') || !strval_push(builder, directive)) {
            return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to grow result buffer");
        }
    }
    return null;
}
