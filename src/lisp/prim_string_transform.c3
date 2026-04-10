module lisp;

import main;

// =============================================================================
// SECTION: STRING TRANSFORMATIONS
// =============================================================================
fn Value* prim_string_append(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return make_string(interp, "");

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

    Value* result = make_string(interp, args[0].str_chars[:args[0].str_len]);
    if (result != null && result.tag == ERROR) return result;
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

    Value* result = make_string(interp, args[0].str_chars[:args[0].str_len]);
    if (result != null && result.tag == ERROR) return result;
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

    usz start = 0;
    while (start < len && (str[start] == ' ' || str[start] == '\t' ||
                           str[start] == '\n' || str[start] == '\r')) {
        start++;
    }

    usz end = len;
    while (end > start && (str[end - 1] == ' ' || str[end - 1] == '\t' ||
                           str[end - 1] == '\n' || str[end - 1] == '\r')) {
        end--;
    }

    if (start >= end) {
        return make_string(interp, "");
    }

    return make_string(interp, args[0].str_chars[start:end - start]);
}

/**
 * (string-join sep list) -> joined string
 * Join list of strings with separator
 */
fn Value* prim_string_join(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "string-join: expected 2 arguments");
    if (!is_string(args[0])) return raise_error(interp, "string-join: expected string separator");

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

    if (start < 0) start += (long)cp_len;
    if (end < 0) end += (long)cp_len;

    if (start < 0) start = 0;
    if (end > (long)cp_len) end = (long)cp_len;
    if (start >= end) return make_string(interp, "");

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
    char delim = args[1].str_chars[0];

    Value* parts = make_nil(interp);
    usz start = 0;

    for (usz i = 0; i <= str_len; i++) {
        if (i == str_len || str[i] == delim) {
            usz part_len = i - start;
            Value* part;
            if (part_len == 0) {
                part = make_string(interp, "");
            } else {
                part = make_string(interp, args[0].str_chars[start:part_len]);
            }
            if (part != null && part.tag == ERROR) return part;
            parts = make_cons(interp, part, parts);
            start = i + 1;
        }
    }

    Value* result = make_nil(interp);
    while (is_cons(parts)) {
        result = make_cons(interp, parts.cons_val.car, result);
        parts = parts.cons_val.cdr;
    }
    return result;
}
