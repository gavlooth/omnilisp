module lisp;

import std::io;
import std::core::mem;
import main;

// =============================================================================
// STRING PRIMITIVES
// =============================================================================

/**
 * Create a new empty StringVal with given initial capacity.
 */
fn StringVal* strval_new(usz initial_cap) {
    if (initial_cap < 32) initial_cap = 32;
    StringVal* sv = (StringVal*)mem::malloc(StringVal.sizeof);
    sv.chars = (char*)mem::malloc(initial_cap);
    sv.len = 0;
    sv.capacity = initial_cap;
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

fn Value* format_directive(StringVal* builder, char[] fmt, usz* i_ptr, Value*[] args, usz* arg_idx, Interp* interp) {
    usz i = *i_ptr;

    // Parse flags
    bool left_align = false;
    if (i < fmt.len && fmt[i] == '-') {
        left_align = true;
        i++;
    }

    // Parse width
    int width = 0;
    bool has_width = false;
    while (i < fmt.len && fmt[i] >= '0' && fmt[i] <= '9') {
        width = width * 10 + (int)(fmt[i] - '0');
        has_width = true;
        i++;
    }

    // Parse precision
    int precision = -1;
    bool has_precision = false;
    if (i < fmt.len && fmt[i] == '.') {
        i++;
        precision = 0;
        has_precision = true;
        while (i < fmt.len && fmt[i] >= '0' && fmt[i] <= '9') {
            precision = precision * 10 + (int)(fmt[i] - '0');
            i++;
        }
    }

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
        case 's':
            // Display value (strings without quotes)
            // Use a temp StringVal to capture the display output
            StringVal* stmp = strval_new(32);
            format_append_display(stmp, a, interp);
            char[] sval = stmp.chars[:stmp.len];
            strval_append_padded(builder, sval, width, precision, left_align, has_precision);
            mem::free(stmp.chars);
            mem::free(stmp);

        case 'd':
            // Decimal integer
            if (!is_int(a)) return raise_error(interp, "format: %d expects integer");
            char[32] dbuf;
            char[] dval = io::bprintf(&dbuf, "%d", a.int_val)!!;
            strval_append_num_padded(builder, dval, width, left_align);

        case 'f':
            // Fixed-point float
            double fnum;
            if (is_int(a)) {
                fnum = (double)a.int_val;
            } else if (is_double(a)) {
                fnum = a.double_val;
            } else {
                return raise_error(interp, "format: %f expects number");
            }
            char[128] fbuf;
            char[] fval;
            if (has_precision) {
                // Build format string dynamically for precision
                char[16] ffmt;
                char[] ffmt_s = io::bprintf(&ffmt, "%%.%df", precision)!!;
                // We need to use the precision value; use bprintf with a fixed set
                // Since C3 bprintf doesn't support dynamic format strings easily,
                // we format with max precision and truncate
                fval = io::bprintf(&fbuf, "%.20f", fnum)!!;
                // Find decimal point and truncate to precision digits after it
                usz dot_pos = 0;
                bool found_dot = false;
                for (usz k = 0; k < fval.len; k++) {
                    if (fval[k] == '.') { dot_pos = k; found_dot = true; break; }
                }
                if (found_dot && precision == 0) {
                    fval = fval[:dot_pos];
                } else if (found_dot) {
                    usz end = dot_pos + 1 + (usz)precision;
                    if (end > fval.len) end = fval.len;
                    fval = fval[:end];
                }
            } else {
                // Default: 6 decimal places
                fval = io::bprintf(&fbuf, "%.6f", fnum)!!;
            }
            strval_append_num_padded(builder, fval, width, left_align);

        case 'e':
            // Scientific notation
            double eval_num;
            if (is_int(a)) {
                eval_num = (double)a.int_val;
            } else if (is_double(a)) {
                eval_num = a.double_val;
            } else {
                return raise_error(interp, "format: %e expects number");
            }
            char[128] ebuf;
            char[] eval_s;
            if (has_precision) {
                eval_s = io::bprintf(&ebuf, "%.20e", eval_num)!!;
                // Find decimal point and truncate to precision digits after it,
                // preserving the exponent part
                usz edot = 0;
                bool efound_dot = false;
                for (usz k = 0; k < eval_s.len; k++) {
                    if (eval_s[k] == '.') { edot = k; efound_dot = true; break; }
                }
                // Find 'e' or 'E'
                usz epos = eval_s.len;
                for (usz k = 0; k < eval_s.len; k++) {
                    if (eval_s[k] == 'e' || eval_s[k] == 'E') { epos = k; break; }
                }
                if (efound_dot) {
                    usz trunc_end = edot + 1 + (usz)precision;
                    if (trunc_end > epos) trunc_end = epos;
                    // Build: digits before trunc_end + exponent part
                    char[128] etmp;
                    usz eidx = 0;
                    for (usz k = 0; k < trunc_end; k++) { etmp[eidx++] = eval_s[k]; }
                    for (usz k = epos; k < eval_s.len; k++) { etmp[eidx++] = eval_s[k]; }
                    eval_s = etmp[:eidx];
                }
            } else {
                eval_s = io::bprintf(&ebuf, "%e", eval_num)!!;
            }
            strval_append_num_padded(builder, eval_s, width, left_align);

        case 'x':
            // Hexadecimal lowercase
            if (!is_int(a)) return raise_error(interp, "format: %x expects integer");
            char[32] xbuf;
            char[] xval = io::bprintf(&xbuf, "%x", a.int_val)!!;
            strval_append_num_padded(builder, xval, width, left_align);

        case 'X':
            // Hexadecimal uppercase
            if (!is_int(a)) return raise_error(interp, "format: %X expects integer");
            char[32] xubuf;
            // C3 bprintf %X should give uppercase hex
            char[] xuval = io::bprintf(&xubuf, "%X", a.int_val)!!;
            strval_append_num_padded(builder, xuval, width, left_align);

        case 'o':
            // Octal
            if (!is_int(a)) return raise_error(interp, "format: %o expects integer");
            char[32] obuf;
            char[] oval = io::bprintf(&obuf, "%o", a.int_val)!!;
            strval_append_num_padded(builder, oval, width, left_align);

        case 'b':
            // Binary — manual conversion
            if (!is_int(a)) return raise_error(interp, "format: %b expects integer");
            char[68] bbuf;
            usz bidx = 0;
            long bval = a.int_val;
            if (bval == 0) {
                bbuf[0] = '0';
                bidx = 1;
            } else {
                bool bneg = bval < 0;
                // For negative, show minus sign then binary of absolute value
                if (bneg) {
                    bbuf[bidx++] = '-';
                    bval = -bval;
                }
                // Write binary digits in reverse
                char[64] brev;
                usz brev_len = 0;
                while (bval > 0) {
                    brev[brev_len++] = (char)('0' + (char)(bval % 2));
                    bval = bval / 2;
                }
                // Reverse into bbuf
                for (usz k = 0; k < brev_len; k++) {
                    bbuf[bidx++] = brev[brev_len - 1 - k];
                }
            }
            strval_append_num_padded(builder, bbuf[:bidx], width, left_align);

        default:
            // Unknown directive — emit as literal
            strval_push(builder, '%');
            strval_push(builder, directive);
    }
    return null;
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

// --- String operations ---

/**
 * (string-append s1 s2 ...) -> concatenated string
 * Variadic: concatenates all string arguments
 */
fn Value* prim_string_append(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return make_string(interp, "");

    // Validate all args are strings
    for (usz i = 0; i < args.len; i++) {
        if (!is_string(args[i])) {
            return raise_error(interp, "string-append: expected string arguments");
        }
    }

    DString ds;
    ds.init(mem);
    for (usz i = 0; i < args.len; i++) {
        ds.append_string((String)args[i].str_chars[:args[i].str_len]);
    }
    Value* result = make_string(interp, ds.str_view());
    ds.free();
    return result;
}

/**
 * (string-upcase s) -> uppercase string
 */
fn Value* prim_string_upcase(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-upcase: expected 1 argument");
    if (!is_string(args[0])) {
        return raise_error(interp, "string-upcase: expected string argument");
    }

    // Allocate result, then transform in place
    Value* result = make_string(interp, args[0].str_chars[:args[0].str_len]);
    for (usz i = 0; i < result.str_len; i++) {
        char c = result.str_chars[i];
        if (c >= 'a' && c <= 'z') {
            result.str_chars[i] = c - 32;
        }
    }
    return result;
}

/**
 * (string-downcase s) -> lowercase string
 */
fn Value* prim_string_downcase(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-downcase: expected 1 argument");
    if (!is_string(args[0])) {
        return raise_error(interp, "string-downcase: expected string argument");
    }

    // Allocate result, then transform in place
    Value* result = make_string(interp, args[0].str_chars[:args[0].str_len]);
    for (usz i = 0; i < result.str_len; i++) {
        char c = result.str_chars[i];
        if (c >= 'A' && c <= 'Z') {
            result.str_chars[i] = c + 32;
        }
    }
    return result;
}

/**
 * (string-trim s) -> string with leading/trailing whitespace removed
 */
fn Value* prim_string_trim(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string-trim: expected 1 argument");
    if (!is_string(args[0])) {
        return raise_error(interp, "string-trim: expected string argument");
    }

    char* str = args[0].str_chars;
    usz len = args[0].str_len;

    // Find start (skip leading whitespace)
    usz start = 0;
    while (start < len && (str[start] == ' ' || str[start] == '\t' ||
                           str[start] == '\n' || str[start] == '\r')) {
        start++;
    }

    // Find end (skip trailing whitespace)
    usz end = len;
    while (end > start && (str[end - 1] == ' ' || str[end - 1] == '\t' ||
                           str[end - 1] == '\n' || str[end - 1] == '\r')) {
        end--;
    }

    // Empty result
    if (start >= end) {
        return make_string(interp, "");
    }

    // Direct slice — no temp buffer needed
    return make_string(interp, args[0].str_chars[start:end - start]);
}

/**
 * (string-join sep list) -> joined string
 * Join list of strings with separator
 */
fn Value* prim_string_join(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "string-join: expected 2 arguments");
    if (!is_string(args[0])) return raise_error(interp, "string-join: expected string separator");

    // Allow nil list to return empty string
    if (is_nil(args[1])) {
        return make_string(interp, "");
    }

    if (!is_cons(args[1])) {
        return raise_error(interp, "string-join: expected list as second argument");
    }

    char[] sep = args[0].str_chars[:args[0].str_len];

    DString ds;
    ds.init(mem);
    defer ds.free();
    bool first = true;

    Value* list = args[1];
    while (is_cons(list)) {
        Value* elem = list.cons_val.car;
        list = list.cons_val.cdr;

        if (!first) {
            ds.append_string((String)sep);
        }
        first = false;

        if (is_string(elem)) {
            ds.append_string((String)elem.str_chars[:elem.str_len]);
        }
    }

    return make_string(interp, ds.str_view());
}

/**
 * (substring s start end) -> substring
 */
fn Value* prim_substring(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) return raise_error(interp, "substring: expected 3 arguments");
    if (!is_string(args[0])) return raise_error(interp, "substring: expected string as first argument");
    if (!is_int(args[1]) || !is_int(args[2])) {
        return raise_error(interp, "substring: expected integer indices");
    }

    char[] bytes = args[0].str_chars[:args[0].str_len];
    usz cp_len = utf8_strlen(bytes);
    long start = args[1].int_val;
    long end = args[2].int_val;

    // Handle negative indices (Python-style) — based on codepoint count
    if (start < 0) start += (long)cp_len;
    if (end < 0) end += (long)cp_len;

    // Clamp to bounds
    if (start < 0) start = 0;
    if (end > (long)cp_len) end = (long)cp_len;
    if (start >= end) return make_string(interp, "");

    // Convert codepoint indices to byte offsets
    usz byte_start = utf8_byte_offset(bytes, (usz)start);
    usz byte_end = utf8_byte_offset(bytes, (usz)end);
    usz sub_len = byte_end - byte_start;
    return make_string(interp, bytes[byte_start:sub_len]);
}

/**
 * (string-split s sep) -> list of strings
 */
fn Value* prim_string_split(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "string-split: expected 2 arguments");
    if (!is_string(args[0]) || !is_string(args[1])) {
        return raise_error(interp, "string-split: expected string arguments");
    }
    if (args[1].str_len == 0) {
        return raise_error(interp, "string-split: empty separator");
    }

    char* str = args[0].str_chars;
    usz str_len = args[0].str_len;
    // NOTE: Currently only supports single-character delimiters.
    // The first character of the separator string is used.
    char delim = args[1].str_chars[0];

    // Collect parts in reverse order, then reverse
    Value* parts = make_nil(interp);
    usz start = 0;

    for (usz i = 0; i <= str_len; i++) {
        if (i == str_len || str[i] == delim) {
            // Extract substring [start..i) — direct slice, no temp buffer
            usz part_len = i - start;
            Value* part;
            if (part_len == 0) {
                part = make_string(interp, "");
            } else {
                part = make_string(interp, args[0].str_chars[start:part_len]);
            }
            parts = make_cons(interp, part, parts);
            start = i + 1;
        }
    }

    // Reverse the list
    Value* result = make_nil(interp);
    while (is_cons(parts)) {
        result = make_cons(interp, parts.cons_val.car, result);
        parts = parts.cons_val.cdr;
    }
    return result;
}

fn Value* prim_string_contains(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || args[1].tag != STRING) return raise_error(interp, "string-contains?: expected 2 strings");
    char[] haystack = args[0].str_chars[:args[0].str_len];
    char[] needle = args[1].str_chars[:args[1].str_len];
    if (needle.len == 0) return make_symbol(interp, interp.sym_true);
    if (needle.len > haystack.len) return make_nil(interp);
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) { match = false; break; }
        }
        if (match) return make_symbol(interp, interp.sym_true);
    }
    return make_nil(interp);
}

fn Value* prim_string_index_of(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || args[1].tag != STRING) return raise_error(interp, "string-index-of: expected 2 strings");
    char[] haystack = args[0].str_chars[:args[0].str_len];
    char[] needle = args[1].str_chars[:args[1].str_len];
    if (needle.len == 0) return make_int(interp, 0);
    if (needle.len > haystack.len) return make_int(interp, -1);
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) { match = false; break; }
        }
        if (match) return make_int(interp, (long)i);
    }
    return make_int(interp, -1);
}

fn Value* prim_string_replace(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3 || args[0].tag != STRING || args[1].tag != STRING || args[2].tag != STRING) {
        return raise_error(interp, "string-replace: expected 3 strings");
    }
    char[] str = args[0].str_chars[:args[0].str_len];
    char[] old = args[1].str_chars[:args[1].str_len];
    char[] new_str = args[2].str_chars[:args[2].str_len];
    if (old.len == 0) return args[0]; // nothing to replace
    StringVal* builder = strval_new(str.len);
    usz i = 0;
    while (i < str.len) {
        if (i + old.len <= str.len) {
            bool match = true;
            for (usz j = 0; j < old.len; j++) {
                if (str[i + j] != old[j]) { match = false; break; }
            }
            if (match) {
                strval_ensure(builder, new_str.len);
                for (usz j = 0; j < new_str.len; j++) {
                    builder.chars[builder.len++] = new_str[j];
                }
                i += old.len;
                continue;
            }
        }
        strval_push(builder, str[i]);
        i++;
    }
    builder.chars[builder.len] = 0;
    Value* result = interp.alloc_value();
    result.tag = STRING;
    strval_into_value(builder, result);
    return result;
}

fn Value* prim_char_at(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || !is_int(args[1])) return raise_error(interp, "char-at: expected string and int");
    char[] bytes = args[0].str_chars[:args[0].str_len];
    usz cp_count = utf8_strlen(bytes);
    long idx = args[1].int_val;
    if (idx < 0) idx += (long)cp_count;
    if (idx < 0 || idx >= (long)cp_count) return raise_error(interp, "char-at: index out of range");
    // Find byte offset of the idx-th codepoint
    usz byte_off = utf8_byte_offset(bytes, (usz)idx);
    usz cplen = utf8_codepoint_len(bytes[byte_off]);
    if (byte_off + cplen > bytes.len) cplen = bytes.len - byte_off;
    return make_string(interp, bytes[byte_off:cplen]);
}

fn Value* prim_string_repeat(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args[0].tag != STRING || !is_int(args[1])) return raise_error(interp, "string-repeat: expected string and int");
    long n = args[1].int_val;
    if (n <= 0) return make_string(interp, "");
    StringVal* builder = strval_new(args[0].str_len * (usz)n);
    for (long i = 0; i < n; i++) {
        strval_ensure(builder, args[0].str_len);
        for (usz j = 0; j < args[0].str_len; j++) {
            builder.chars[builder.len++] = args[0].str_chars[j];
        }
    }
    builder.chars[builder.len] = 0;
    Value* result = interp.alloc_value();
    result.tag = STRING;
    strval_into_value(builder, result);
    return result;
}

/**
 * (string-length s) -> int
 */
fn Value* prim_string_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_int(interp, 0);
    }
    // Count UTF-8 codepoints, not bytes
    usz cp_len = utf8_strlen(args[0].str_chars[:args[0].str_len]);
    return make_int(interp, (long)cp_len);
}

fn Value* prim_string_byte_length(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) {
        return make_int(interp, 0);
    }
    return make_int(interp, (long)args[0].str_len);
}

/**
 * (string->list s) -> list of single-char strings
 */
fn Value* prim_string_to_list(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "string->list: expected 1 argument");
    if (!is_string(args[0])) {
        return raise_error(interp, "string->list: expected string argument");
    }

    char[] bytes = args[0].str_chars[:args[0].str_len];

    // First pass: count codepoints
    usz cp_count = 0;
    usz pos = 0;
    while (pos < bytes.len) {
        usz cplen = utf8_codepoint_len(bytes[pos]);
        if (pos + cplen > bytes.len) cplen = bytes.len - pos;
        pos += cplen;
        cp_count++;
    }

    if (cp_count == 0) return make_nil(interp);

    // Allocate exact-size arrays via temp allocator
    usz[] offsets = mem::temp_array(usz, cp_count);
    usz[] cp_lens = mem::temp_array(usz, cp_count);

    // Second pass: fill offsets and lens
    pos = 0;
    for (usz i = 0; i < cp_count; i++) {
        offsets[i] = pos;
        usz cplen = utf8_codepoint_len(bytes[pos]);
        if (pos + cplen > bytes.len) cplen = bytes.len - pos;
        cp_lens[i] = cplen;
        pos += cplen;
    }

    // Build list in reverse order
    Value* result = make_nil(interp);
    for (isz i = (isz)cp_count - 1; i >= 0; i--) {
        usz off = offsets[(usz)i];
        usz clen = cp_lens[(usz)i];
        Value* char_str = make_string(interp, bytes[off:clen]);
        result = make_cons(interp, char_str, result);
    }
    return result;
}

/**
 * (list->string chars) -> string
 * Convert a list of single-character strings to a string
 */
fn Value* prim_list_to_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) {
        return raise_error(interp, "list->string: expected 1 argument");
    }

    // Allow nil to return empty string
    if (is_nil(args[0])) {
        return make_string(interp, "");
    }

    if (!is_cons(args[0])) {
        return raise_error(interp, "list->string: expected a list");
    }

    DString ds;
    ds.init(mem);
    defer ds.free();

    Value* list = args[0];
    while (is_cons(list)) {
        Value* elem = list.cons_val.car;
        list = list.cons_val.cdr;

        if (is_string(elem) && elem.str_len > 0) {
            ds.append_char(elem.str_chars[0]);
        }
    }

    return make_string(interp, ds.str_view());
}

// --- Conversion ---

fn Value* prim_string_to_number(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_string(args[0])) return make_nil(interp);
    char[] s = args[0].str_chars[:args[0].str_len];
    if (s.len == 0) return make_nil(interp);
    // Check if it contains a dot or exponent → parse as float
    bool is_float = false;
    for (usz j = 0; j < s.len; j++) {
        if (s[j] == '.' || s[j] == 'e' || s[j] == 'E') { is_float = true; break; }
    }
    if (is_float) {
        // Parse as double: [+-]digits[.digits][e[+-]digits]
        usz i = 0;
        bool negative = false;
        if (i < s.len && (s[i] == '-' || s[i] == '+')) { negative = s[i] == '-'; i++; }
        if (i >= s.len) return make_nil(interp);
        double val = 0.0;
        bool has_digits = false;
        while (i < s.len && s[i] >= '0' && s[i] <= '9') { val = val * 10.0 + (double)(s[i] - '0'); i++; has_digits = true; }
        if (i < s.len && s[i] == '.') {
            i++;
            double frac = 0.1;
            while (i < s.len && s[i] >= '0' && s[i] <= '9') { val = val + (double)(s[i] - '0') * frac; frac = frac * 0.1; i++; has_digits = true; }
        }
        if (!has_digits) return make_nil(interp);
        if (i < s.len && (s[i] == 'e' || s[i] == 'E')) {
            i++;
            bool exp_neg = false;
            if (i < s.len && (s[i] == '-' || s[i] == '+')) { exp_neg = s[i] == '-'; i++; }
            long exp_val = 0;
            while (i < s.len && s[i] >= '0' && s[i] <= '9') { exp_val = exp_val * 10 + (long)(s[i] - '0'); i++; }
            double mult = 1.0;
            for (long e = 0; e < exp_val; e++) mult = mult * 10.0;
            if (exp_neg) { val = val / mult; } else { val = val * mult; }
        }
        if (i != s.len) return make_nil(interp);
        if (negative) val = -val;
        return make_double(interp, val);
    }
    // Integer parse
    usz i = 0;
    bool negative = false;
    if (s[0] == '-') { negative = true; i = 1; }
    if (i >= s.len) return make_nil(interp);
    long val = 0;
    while (i < s.len) {
        if (s[i] < '0' || s[i] > '9') return make_nil(interp);
        val = val * 10 + (long)(s[i] - '0');
        i++;
    }
    if (negative) val = -val;
    return make_int(interp, val);
}

fn Value* prim_number_to_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "number->string: expected number");
    char[64] buf;
    if (is_double(args[0])) {
        return make_string(interp, double_to_string(args[0].double_val, &buf));
    }
    return make_string(interp, int_to_string(args[0].int_val, &buf));
}

fn Value* prim_exact_to_inexact(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "exact->inexact: expected number");
    return make_double(interp, to_double(args[0]));
}

fn Value* prim_inexact_to_exact(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_number(args[0])) return raise_error(interp, "inexact->exact: expected number");
    return make_int(interp, (long)to_double(args[0]));
}

fn Value* prim_read_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return raise_error(interp, "read-string: expected string");
    char[] src = args[0].str_chars[:args[0].str_len];
    EvalResult r = run(src, interp);
    if (r.error.has_error) return raise_error(interp, "read-string: parse/eval error");
    return r.value;
}

fn Value* prim_string_to_symbol(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return raise_error(interp, "string->symbol: expected string");
    SymbolId sym = interp.symbols.intern(args[0].str_chars[:args[0].str_len]);
    return make_symbol(interp, sym);
}

fn Value* prim_symbol_to_string(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_symbol(args[0])) return raise_error(interp, "symbol->string: expected symbol");
    char[] name = interp.symbols.get_name(args[0].sym_val);
    return make_string(interp, name);
}
